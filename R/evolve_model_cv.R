
################################################################################
add_interact_num <- function(d){
        game <- rep(NA, nrow(d))
        game[1] <- 1
        for (i in 2:nrow(d)){
                game[i] <- ifelse(d$period[i]==1, game[i-1] + 1, game[i-1])
        }
        game
}


#' Estimate Optimal Number of States of a Finite-state Machine Model
#'
#' \code{evolve_model_cv} calls \code{evolve_model} with varied numbers of
#' states and compares their performance with cross-validation.
#'
#' @param data data.frame that has "period" and "outcome" columns and rest of
#'   columns are predictors, ranging from one to three predictors. All of the
#'   (3-5 columns) should be named.
#' @param k Numeric vector with the number of folds for k-fold
#'   cross-validation.
#' @param max_states Numeric vector with the maximum number of states
#'   to test.
#' @param actions Numeric vector with the number of actions. If not
#'   provided, then actions will be set as the number of unique values in the
#'   outcome vector.
#' @param seed Numeric vector length one.
#' @param popSize Numeric vector length one specifying the size of the
#'   GA population. A larger number will increase the probability of finding a
#'   very good solution but will also increase the computation time. This is
#'   passed to the GA::ga() function of the \strong{GA} package.
#' @param pcrossover Numeric vector length one specifying probability
#'   of crossover for GA. This is passed to the GA::ga() function of the
#'   \strong{GA} package.
#' @param pmutation Numeric vector length one specifying probability of
#'   mutation for GA. This is passed to the GA::ga() function of the \strong{GA}
#'   package.
#' @param maxiter Numeric vector length one specifying max number of
#'   iterations for stopping the GA evolution. A larger number will increase the
#'   probability of finding a very good solution but will also increase the
#'   computation time. This is passed to the GA::ga() function of the
#'   \strong{GA} package.
#' @param run Numeric vector length one specifying max number of
#'   consecutive iterations without improvement in best fitness score for
#'   stopping the GA evolution. A larger number will increase the probability of
#'   finding a very good solution but will also increase the computation time.
#'   This is passed to the GA::ga() function of the \strong{GA} package.
#' @param parallel Logical vector length one. For running the GA
#'   evolution in parallel. Depending on the number of cores registered and the
#'   memory on your machine, this can make the process much faster, but only
#'   works for Unix-based machines that can fork the processes.
#' @param boltzmann Logical vector length one.
#' @param alpha Numeric vector length one. This is an additional
#'   parameter to tune/set if \code{boltzmann} is set to TRUE.
#'
#' @return Returns the number of states that maximizes accuracy.
#'
#' @export

################################################################################
evolve_model_cv <- function(data, k,
                            actions,
                            max_states,
                            seed,
                            popSize, pcrossover, pmutation, maxiter, run,
                            parallel,
                            boltzmann, alpha) {

        interacts <- add_interact_num(data)

        mat <- matrix(NA, nrow = max_states, ncol = k)

        for(s in 2:max_states){
                # divide interacts into k folds
                group_folds <- caret::createFolds(y = unique(interacts), k = k, list = FALSE)

                if(length(group_folds) != length(unique(interacts))) stop("Assignment of groups to folds didnt work.")
                if(length(unique(group_folds)) != k) stop("Assignment of groups to folds didnt work.")
                #cat("Group folds:", group_folds ,"\n")

                # create a vector same length as data with assignments of each row to a fold:
                fold_ass <- rep(NA, nrow(data))
                for (i in seq(nrow(data))) fold_ass[i] <- group_folds[interacts[i]]
                if(length(fold_ass) != nrow(data))
                        stop("Creating a vector same length as data with assignments of each row to a fold didnt work.")
                if(length(unique(fold_ass)) != length(unique(group_folds)))
                        stop("Creating a vector same length as data with assignments of each row to a fold didnt work.")

                # In the fth fold, the elements of folds that equal f are in the test set, and the remainder are in the training set.
                for(f in seq(k)){
                        training <- fold_ass == f
                        if(class(training) != "logical") stop("Training index not logical vector.")
                        mat[s, f] <- evolve_model(data[training, ], data[!training, ],
                                                  states = s, cv = FALSE, seed = seed,
                                                  popSize = popSize, pcrossover =pcrossover,
                                                  pmutation = pmutation, maxiter = maxiter, run = run,
                                                  parallel = parallel,
                                                  boltzmann =  boltzmann, alpha=alpha)@predictive
                }
        }
        results <- apply(na.omit(mat), 1, mean) # mean predictive accuracy for each number of states across all k folds (columns)
        which(results==max(results))+1 # na.omit dropped the first row of mat bc we started at states==2
        # the number of states that maximizes accuracy obtained from index with highest value, but add one because
        # first position in vector corresponds to states==2
}

# data = ipd_data; k=2; actions=2; max_states=4; seed=1; popSize = 75; pcrossover = 0.8; pmutation = 0.1;
# maxiter = 55; run = 25; parallel = FALSE; boltzmann = FALSE; alpha=0.4
