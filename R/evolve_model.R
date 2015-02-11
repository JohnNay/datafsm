#' Uses a Genetic Algorithm to Estimate a Finite-state Machine
#'
#' \code{evolve_model} uses a genetic algorithm to estimate a finite-state machine model.
#'
#' This is the main function of the \strong{fsm} package. It takes data on predictors
#' and data on the outcome and then uses a stochastic meta-heuristic optimization
#' routine to estimate the parameter. Generalized simulated annealing could work.
#' The current version uses the \strong{GA} package's genetic algorithm
#' because GAs perform well in rugged search spaces to solve integer
#' optimization problems and are a natural complement to our binary string
#' representation of FSMs.
#'
#' @param data Numeric matrix that has first col period and rest of cols are
#' predictors.
#' @param outcome Numeric vector same length as the number of rows as data.
#' @param actions Numeric vector with the number of actions.
#' @param states Numeric vector with the number of states.
#' @param seed Numeric vector length one.
#' @param fitness_func Function that takes data, action vector, and state matrix
#' as input and returns numeric vector of same length as the \code{outcome}. This
#' is then used inside \code{evolve_model} to compute a fitness score by comparing
#' it to the provided \code{outcome}.
#'
#' @return Returns an S4 object of class ga_fsm. See \linkS4class{ga_fsm} for
#' the details of the slots (objects) that this type of object will have and for
#' information on the methods that can be used to summarize the calling and
#' execution of \code{evolve_model()}, including \code{\link{summary.ga_fsm}}.
#'
#' @examples
#' \dontrun{
#' # create "fitnessC"
#' Rcpp::sourceCpp("/Users/johnnaymacbook/Documents/Rprojects/ga_fsm/genetic_abm/fitness.cpp")
#' # load data from fsm package
#' data(data); data (outcome)
#' # 80% of the data for training
#' train_data <- data[1:108306, ]
#' train_outcome <- outcome[1:108306]
#' # 20% of the data for testing
#' test_data <- data[108307:nrow(data), ]
#' test_outcome <- outcome[108307:nrow(data)]
#' evolved_models_empirical_data <- evolve_model(data = train_data, outcome = train_outcome,
#'                                               actions = 2, states = 2, seed = 13,
#'                                               fitness_func = fitnessC, cols = c(1, 2, 1, 2),
#'                                               test_data =  test_data, test_outcome = test_outcome,
#'                                               popSize = 175, maxiter = 55, run = 25, parallel = TRUE)
#' print(evolved_models_empirical_data)
#' show(evolved_models_empirical_data)
#' summary(evolved_models_empirical_data)
#'
#' data <- data.frame(y = evolved_models_empirical_data@@varImp, x= c("cc", "dc", "cd", "dd"))
#' p <- ggplot(data, aes(x = x, y=y)) + geom_bar(stat="identity") + coord_flip() +
#'         ylab("Relative Importance") +
#'         xlab("Variables")
#' p
#'
#' plot(evolved_models_empirical_data@@GA,
#'      cex.points = 1.2, cex.lab = 1.1, cex.axis = 1.1,
#'      col = c("green3", "dodgerblue3", adjustcolor("green3", alpha.f = 0.1)),
#'     pch = c(16, 1), lty = c(1,2), grid = graphics:::grid)
#' }
#'
#'   @export


################################################################################
evolve_model <- function(data, outcome, actions, states, seed, fitness_func,
                         cols = NULL,
                         test_data = NULL, test_outcome = NULL,
                         popSize = 39, pcrossover = 0.8, pmutation = 0.1, maxiter = 20, run = 8,
                         parallel = FALSE,
                         priors = NULL,
                         boltzmann = FALSE, alpha=0.4) {
        # data has first col period and rest of cols are covariates
        # This function evolves the models on training data and then, if test set provided,
        # uses the best solution to make predictions on test data, and then returns the GA object
        # and the decoded version of the best string in the population.
        # fitness_func takes in the decoded string and the data and loops through all the data
        # this should be in cpp so it is very fast.

        # priors is a matrix of solutions strings to be included in the initialization,
        # where the number of columns == the number of bits. so you need to use the decoder funcs
        # to translate prior decision models into bits and then provide them.

        call <- match.call()

        ## GA-related errors:
        if (missing(fitness_func)) stop("A fitness function must be provided --
                                        either an R function or a C++ function call-able in R.")
        if (popSize < 10) warning("The population size is less than 10. Consider using a larger size.")
        if (maxiter < 1) stop("The maximum number of iterations must be at least 1.")
        if (pcrossover < 0 || pcrossover > 1) stop("Probability of crossover must be between 0 and 1.")
        if (pmutation < 0 || pmutation > 1) stop("Probability of mutation must be between 0 and 1.")
        ## Data-related errors:
        if (nrow(data)!=length(outcome)) stop("The data (covariates) and the
                                              outcome variable are not the same length.")
        if (anyNA(outcome)) stop("There are missing values in the data.")
        if (length(outcome)==0) stop("The outcome is zero length.")
        if (missing(seed)) stop("Set a seed to make this reproducible.")


        inputs <- 2^(ncol(data)-1) # bc 1 of the columns is for period, and the rest are covariates

        fitnessR <- function(s){ # functions defined in globalenv() above: decode_action_vec, decode_state_mat
                action_vec <- decode_action_vec(s, states, inputs, actions)
                state_mat <- decode_state_mat(s, states, inputs, actions)
                results <- fitness_func(action_vec, state_mat, data)

                if (anyNA(results) | length(results)==0){
                        stop("Results from fitness evaluation have missing values.")
                }

                sum(ifelse( results == outcome , 1 , 0)) / length(results)
        }

        poss.state.values <- 1:states
        b1 <- decimal2binary(max(poss.state.values))
        l1 <- length(b1) #how many binary elements to represent one element of state matrix

        poss.action.values <- 1:actions
        b2 <- decimal2binary(max(poss.action.values))
        l2 <- length(b2) #how many binary elements to represent one element of action matrix

        nBits <- (states*inputs*l1 + states*l2)

        if (missing(priors)) {
                priors <- matrix(nrow = 0, ncol = nBits)
        } else {
                if (is.vector(priors)) {
                        if (nBits > 1){
                                priors <- matrix(priors, nrow = 1)
                        } else  {
                                priors <- matrix(priors, ncol = 1)
                        }
                } else {
                        priors <- as.matrix(priors)
                }
                if (nBits != ncol(priors)) stop("Priors do not match number of variables.
                                                Remember that you need to provide a decoded bitstring for the priors.")
        }

        if (!boltzmann) {
                GA <- GA::ga(type = "binary",
                         fitness = fitnessR,
                         nBits = nBits,
                         popSize = popSize,
                         pcrossover = pcrossover,
                         pmutation = pmutation,
                         maxiter = maxiter,
                         run = run,
                         maxfitness = 1,
                         parallel = parallel,
                         suggestions = priors,
                         seed=seed)
        } else {
                source("boltzmann.R")
                GA <- GA::ga(type = "binary",
                         fitness =  fitnessR,
                         nBits = nBits,
                         popSize = popSize,
                         pcrossover = pcrossover,
                         pmutation = pmutation,
                         maxiter = maxiter,
                         run = run,
                         maxfitness = 1,
                         parallel = parallel,
                         suggestions = priors,
                         seed = seed,
                         selection = function(...) BoltzmannSelection(..., alpha = alpha))
        }

        state_mat <- decode_state_mat(GA@solution[1, ],  states, inputs, actions)
        action_vec <- decode_action_vec(GA@solution[1, ],  states, inputs, actions)

        if (missing(test_data)){
                predictive <- "No test data provided. Provide some to get more accurate estimation of generalization power."
        } else {
                results <- fitness_func(action_vec, state_mat, test_data)
                stopifnot(!anyNA(results), length(results)!=0)
                predictive <-  sum(ifelse( results == test_outcome , 1 , 0)) / length(results)
        }

        if (missing(cols)){
                message <- "No column actions provided. Provide some to get a degeneracy check."
                dif <- NA
                sparse_state_mat <- NA
                corrected_state_mat <- state_mat # need this to be non-NA for varImp below
        } else {
                d_check <- degeneracy_check(state_mat, action_vec, cols, fitness_func, data, outcome)
                dif <- d_check$dif
                sparse_state_mat <- d_check$sparse_state_mat
                corrected_state_mat <- d_check$corrected_state_mat

                if (any(dif >= 0)) {
                        message <- ifelse(any(dif == 0), "See the sparse matrix returned. The elements in that matrix with a 0 are unidentifiable. Their value makes no difference to the fit of the strategy to the provided data.",
                                          "You have not found an optimal strategy, by randomly flipping values of some components, we improved it.")
                } else {
                        message <- ifelse(all(dif < 0), "Your strategy is a deterministic approximation of a stochastic process. All of the elements of the state matrix can be identified.",
                                          "You could improve your strategy by changing at least one component to its opposite value.")
                }
        }

        # if the model is fine, then corrected_state_mat should be equal to state_mat
        varImp <- var_imp(corrected_state_mat, action_vec, fitness_func, data, outcome, cols)

        object <- new("ga_fsm",
                      call = call,
                      actions = actions, states = states,
                      GA = GA,
                      state_mat = state_mat, action_vec = action_vec,
                      predictive = predictive,
                      degeneracy = list(message=message, dif=dif, sparse_state_mat = sparse_state_mat),
                      varImp = varImp)
        # return an object of class ga_fsm
        object
}
