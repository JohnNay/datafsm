#' Estimate Optimal Number of States of a Finite-state Machine Model
#'
#' \code{evolve_model_cv} calls \code{evolve_model} with varied numbers of
#' states and compares the performance with cross-validation.
#'
#' @param data data.frame that has "period" and "outcome" columns and rest of
#'   columns are predictors, ranging from one to three predictors. All of the
#'   (3-5 columns) should be named.
#' @param k Optional numeric vector with the number of folds for k-fold cross-validation.
#' @param max_states Optional numeric vector with the maximum number of states to test.
#' @param actions Optional numeric vector with the number of actions. If not
#'   provided, then actions will be set as the number of unique values in the
#'   outcome vector.
#' @param seed Optional numeric vector length one.
#' @param popSize Optional numeric vector length one specifying the size of the
#'   GA population. A larger number will increase the probability of finding a
#'   very good solution but will also increase the computation time. This is
#'   passed to the GA::ga() function of the \strong{GA} package.
#' @param pcrossover Optional numeric vector length one specifying probability
#'   of crossover for GA. This is passed to the GA::ga() function of the
#'   \strong{GA} package.
#' @param pmutation Optional numeric vector length one specifying probability of
#'   mutation for GA. This is passed to the GA::ga() function of the \strong{GA}
#'   package.
#' @param maxiter Optional numeric vector length one specifying max number of
#'   iterations for stopping the GA evolution. A larger number will increase the
#'   probability of finding a very good solution but will also increase the
#'   computation time. This is passed to the GA::ga() function of the
#'   \strong{GA} package.
#' @param run Optional numeric vector length one specifying max number of
#'   consecutive iterations without improvement in best fitness score for
#'   stopping the GA evolution. A larger number will increase the probability of
#'   finding a very good solution but will also increase the computation time.
#'   This is passed to the GA::ga() function of the \strong{GA} package.
#' @param parallel Optional logical vector length one. For running the GA
#'   evolution in parallel. Depending on the number of cores registered and the
#'   memory on your machine, this can make the process much faster, but only
#'   works for Unix-based machines that can fork the processes.
#' @param priors Optional numeric matrix of solutions strings to be included in
#'   the initialization, where the number of columns is equal to the number of
#'   bits. User needs to use a decoder function to translate prior decision
#'   models into bits and then provide them. If this is not specified, then
#'   random priors are automatically created.
#' @param boltzmann Optional logical vector length one.
#' @param alpha Optional numeric vector length one. This is an additional
#'   parameter to tune/set if \code{boltzmann} is set to TRUE.
#'
#' @return Returns numeric vector of cross-validated errors for each number of states.
#'
#' @export

################################################################################
evolve_model_cv <- function(data, k = 10,
                            actions = NULL,
                            max_states = 4,
                            seed = NULL,
                            popSize = 75, pcrossover = 0.8, pmutation = 0.1, maxiter = 55, run = 25,
                            parallel = FALSE,
                            priors = NULL,
                            boltzmann = FALSE, alpha=0.4) {
        # add identifier to data for individual groups
        # use that to divide groups into k folds with caret
        vec <- rep(NA, max_states)
        for(s in seq(max_states)){

                for(f in seq(k)){
                        evolve_model(data=data,
                                     states = s,
                                     actions = actions,
                                     seed = seed,
                                     popSize = popSize, pcrossover =pcrossover,
                                     pmutation = pmutation, maxiter = maxiter, run = run,
                                     parallel = parallel,
                                     priors = priors,
                                     boltzmann =  boltzmann, alpha=alpha)
                }
        }
        vec
}
