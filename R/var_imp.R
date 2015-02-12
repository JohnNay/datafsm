#' Variable Importance Measure for A FSM Model
#'
#' \code{var_imp} calculates the importance of the covariates of the model.
#'
#' Takes the state matrix and action vector from an already evolved model and
#' the fitness function and data used to evolve the model (or this could be test
#' data), flips the values of each of the elements in the state matrix and
#' measures the change in fitness (prediction of data) relative to the original
#' model. Then these changes are summed across columns to provide the importance
#' of each of the columns of the state matrix.
#'
#' @param state_mat Numeric matrix with rows as states and columns as
#'   predictors.
#' @param action_vec Numeric vector indicating what action to take for each
#'   state.
#' @param fitness_func Function that takes \code{data}, \code{action_vec}, and
#'   \code{state_mat} as input and returns numeric vector of same length as the
#'   \code{outcome}. This is then used inside \code{var_imp} to compute a
#'   fitness score by comparing it to the provided \code{outcome}.
#' @param data Numeric matrix that has first col period and rest of cols are
#'   predictors.
#' @param outcome Numeric vector same length as the number of rows as data.
#'
#' @return Numeric vector the same length as the number of columns of the
#'   provided state matrix (the number of predictors in the model) with relative
#'   importance scores for each predictor.
#'
#' @export

var_imp <- function(state_mat, action_vec, fitness_func, data, outcome, cols=NULL){

        counter <- 1
        indices <- as.list(rep(NA, length(as.vector(state_mat))))
        for (i in seq(nrow(state_mat))){
                for (j in seq(ncol(state_mat))){
                        indices[[counter]] <- c(i, j)
                        counter <- counter + 1
                }
        }

        if (nrow(state_mat)*ncol(state_mat)!=length(indices)){
                stop("Numbers of elements of state matrix does not equal
                     length of list to hold indices for each of those elements.")
        }

        fitness_mat <- state_mat

        results1 <- fitness_func(action_vec, state_mat, data)

        if (anyNA(results1) | length(results1)==0){
                stop("Results from initial fitness evaluation have missing values.")
        }

        results1 <- sum(ifelse( results1 == outcome , 1 , 0)) / length(results1)

        for (i in seq(length(indices))) {
                state_mat_flipped <- state_mat
                state_mat_flipped[indices[[i]][1],
                                  indices[[i]][2]] <- ifelse(state_mat[indices[[i]][1],
                                                                       indices[[i]][2]]==1, 2, 1)
                results2 <- fitness_func(action_vec, state_mat_flipped, data)

                if (anyNA(results2) | length(results2)==0){
                        stop("Results from subsequent fitness evaluation have missing values.")
                }

                results2 <- sum(ifelse( results2 == outcome , 1 , 0)) / length(results2)

                dif <- results1 - results2

                fitness_mat[indices[[i]][1],
                            indices[[i]][2]] <- dif
        }

        varImp <- as.vector(apply(fitness_mat, MARGIN=2, sum)) # sum each col
        varImp <- (varImp/ifelse(max(varImp)==0, 0.001, max(varImp)))*100 # make the best be 100
        varImp <- ifelse(varImp < 0, 0, varImp)

        if (missing(cols)){
                return(varImp)
        } else {
                names(varImp) <- cols
                return(varImp)
        }
}
