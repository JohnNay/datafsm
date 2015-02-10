# Being in state/row $k$ (e.g. 2) corresponds to taking action $j$ (e.g. D). 
# All rows of any column that corresponds to taking action $j$ last period 
# (e.g. columns 2 and 4 for D) \textit{are} identifiable; however, columns 
# that correspond to not taking action $j$ last period (e.g. columns 1 and 3 for D) 
# for the row $k$ that corresponds to taking action $j$ \textit{are not} identifiable. 
# For all elements of the matrix that are not identifiable, the value of the element 
# can be any integer in the inclusive range of the number of rows of the matrix (e.g. 1 or 2).

find_wildcards <- function(state_mat, action_vec, cols){
        # cols is a vector that says what action each col of state_mat corresponds to
        if (ncol(state_mat)!=length(cols)){
                stop("Number of cols of state matrix
                     does not equal length of cols argument.")
        }
        if (nrow(state_mat)!=length(action_vec)){
                stop("Number of rows of state matrix does not equal
                     the length of the action vector.")
        }
        
        counter <- 1
        indices <- as.list(rep(NA, length(as.vector(state_mat))))
        for (i in seq(length(action_vec))){ 
                for (j in seq(length(cols))){
                        if (action_vec[i] != cols[j]){
                                indices[[counter]] <- c(i, j)
                                counter <- counter + 1
                        } else indices[[counter]] <- NULL
                }
        }
        indices              
}

# # test -----
# tft_state <- matrix(c(1, 1, 1, 1, 2, 2, 2, 2), 2, 4)
# tft_action <- matrix(c(1, 2))
# find_wildcards(tft_state, tft_action,  c(1, 2, 1, 2))

# cols <- c(1, 2, 1, 2)

degeneracy_check <- function(state_mat, action_vec, cols, fitness_func, data, outcome){
        indices <- find_wildcards(state_mat, action_vec, cols)
        
        results1 <- fitness_func(action_vec, state_mat, data)
        if (anyNA(results1) | length(results1)==0){
                stop("Results from first fitness evaluation have missing values.")
        }
        results1 <- sum(ifelse( results1 == outcome , 1 , 0)) / length(results1)
        
        sparse_state_mat <- state_mat
        corrected_state_mat <- state_mat
        # sparse will be corrected and have zeros added for non-identifiable
        # corrected will only be corrected, this way it can be directyly used for varImp()
        
        dif <- rep(NA, length(indices)) # how many are not identifiable
        
        for(i in seq(length(dif))){
                
                state_mat_flipped <- state_mat
                state_mat_flipped[indices[[i]][1], 
                                  indices[[i]][2]] <- ifelse(state_mat[indices[[i]][1], 
                                                                       indices[[i]][2]]==1, 2, 1)
                
                results2 <- fitness_func(action_vec, state_mat_flipped, data)
                if (anyNA(results2) | length(results2)==0){
                        stop("Results from subsequent fitness evaluation have missing values.")
                }
                results2 <- sum(ifelse( results2 == outcome , 1 , 0)) / length(results2)
                
                dif[i] <- (results2 - results1) / results1
                
                # for any equal zero, use 0 as the identifier of non-identifiable
                sparse_state_mat[indices[[i]][1], 
                                 indices[[i]][2]] <- ifelse(dif[i]==0, 0, state_mat[indices[[i]][1], 
                                                                                    indices[[i]][2]])
                # for any greater than zero, give it opposite value that improved it
                sparse_state_mat[indices[[i]][1], 
                                 indices[[i]][2]] <- ifelse(dif[i] > 0, state_mat_flipped[indices[[i]][1], 
                                                                                          indices[[i]][2]], state_mat[indices[[i]][1], 
                                                                                                                      indices[[i]][2]])
                
                # for any greater than zero, give it opposite value that improved it
                corrected_state_mat[indices[[i]][1], 
                                    indices[[i]][2]] <- ifelse(dif[i] > 0, state_mat_flipped[indices[[i]][1], 
                                                                                             indices[[i]][2]], state_mat[indices[[i]][1], 
                                                                                                                         indices[[i]][2]])
        }
         # if the model is fine, then sparse_state_mat and corrected_state_mat should be equal to state_mat
        list(dif = dif, sparse_state_mat = sparse_state_mat, corrected_state_mat = corrected_state_mat)
}

# if(any(dif > 0)) cat("You have not found an optimal strategy.\n")
# if(any(dif < 0)) cat("You have randomness in your strategy, which may or may not be optimal.\n")
