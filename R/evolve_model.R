#' Uses a Genetic Algorithm to Estimate a Finite-state Machine
#'
#' \code{evolve_model} uses a genetic algorithm to estimate a finite-state
#' machine model, primarily for understanding and predicting decision-making.
#'
#' This is the main function of the \strong{fsm} package. It takes data on
#' predictors and data on the outcome and then uses a stochastic meta-heuristic
#' optimization routine to estimate the parameters. Generalized simulated
#' annealing could work. The current version uses the \strong{GA} package's
#' genetic algorithm because GAs perform well in rugged search spaces to solve
#' integer optimization problems and are a natural complement to our binary
#' string representation of FSMs.
#'
#' This function evolves the models on training data and then, if test set
#' provided, uses the best solution to make predictions on test data, and then
#' returns the GA object and the decoded version of the best string in the
#' population. See \linkS4class{ga_fsm} for the details of the slots (objects)
#' that this type of object will have.
#'
#' @param data Numeric matrix that has first col period and rest of cols are
#'   predictors.
#' @param outcome Numeric vector same length as the number of rows as data.
#' @param actions Numeric vector with the number of actions.
#' @param states Numeric vector with the number of states.
#' @param seed Numeric vector length one.
#' @param fitness_func Function that takes data, action vector, and state matrix
#'   as input and returns numeric vector of same length as the \code{outcome}.
#'   This is then used inside \code{evolve_model()} to compute a fitness score
#'   by comparing it to the provided \code{outcome}. Because this function must
#'   loop through all the data, it makes sense to implement in C++ so it is
#'   fast.
#' @param cols Optional numeric vector same length as number of columns of the
#'   state matrix (\code{state_mat}) with the action that each column of the
#'   state matrix corresponds to the decision model taking in the previous
#'   period. This is only relevant when the predictor variables of the FSM are
#'   lagged outcomes that include the previous actions taken by that decision
#'   model.
#' @param test_data Numeric matrix that has first column the period of
#'   interaction between decision-makers and the rest of the columns are
#'   predictors for that decision. These predictors are often lagged decisions
#'   from previous time periods.
#' @param test_outcome Numeric vector same length as the number of rows as
#'   test_data with the decision the decision-maker took for that period.
#' @param popSize Numeric vector length one specifying the size of the GA
#'   population. A larger number will increase the probability of finding a very
#'   good solution but will also increase the computation time. This is passed
#'   to the ga() function of the GA package.
#' @param pcrossover Numeric vector length one specifying probability of
#'   crossover for GA. This is passed to the ga() function of the GA package.
#' @param pmutation Numeric vector length one specifying probability of mutation
#'   for GA. This is passed to the ga() function of the GA package.
#' @param maxiter Numeric vector length one specifying max number of iterations
#'   for stopping the GA evolution. A larger number will increase the
#'   probability of finding a very good solution but will also increase the
#'   computation time. This is passed to the ga() function of the GA package.
#' @param run Numeric vector length one specifying max number of consecutive
#'   iterations without improvement in best fitness score for stopping the GA
#'   evolution. A larger number will increase the probability of finding a very
#'   good solution but will also increase the computation time. This is passed
#'   to the ga() function of the GA package.
#' @param parallel Logical vector length one. For running the GA evolution in
#'   parallel. Depending on the number of cores registered and the memory on
#'   your machine, this can make the process much faster.
#' @param priors Numeric matrix of solutions strings to be included in the
#'   initialization, where the number of columns is equal to the number of bits.
#'   User needs to use a decoder function to translate prior decision models
#'   into bits and then provide them. If this is not specified, then random
#'   priors are automatically created.
#' @param boltzmann Logical vector length one.
#' @param alpha Numeric vector length one. This is an additional parameter to
#'   tune/set if \code{boltzmann} is set to TRUE.
#'
#' @return Returns an S4 object of class ga_fsm. See \linkS4class{ga_fsm} for
#'   the details of the slots (objects) that this type of object will have and
#'   for information on the methods that can be used to summarize the calling
#'   and execution of \code{evolve_model()}, including
#'   \code{\link{summary.ga_fsm}}.
#'
#' @examples
#' \dontrun{
#' # create "fitnessC"
#' Rcpp::sourceCpp("/Users/johnnaymacbook/Documents/Rprojects/ga_fsm/genetic_abm/fitness.cpp")
#' # load data from fsm package
#' data(data); data(outcome)
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
#'                                               popSize = 75, maxiter = 55, run = 25, parallel = TRUE)
#' print(evolved_models_empirical_data)
#' show(evolved_models_empirical_data)
#' summary(evolved_models_empirical_data)
#'
#' data <- data.frame(y = evolved_models_empirical_data@@varImp, x= c("cc", "dc", "cd", "dd"))
#' p <- ggplot2::ggplot(data, ggplot2::aes(x = x, y=y)) + ggplot2::geom_bar(stat="identity") +
#'                      ggplot2::coord_flip() +
#'                      ggplot2::ylab("Relative Importance") + ggplot2::xlab("Variables")
#' p
#'
#' plot(evolved_models_empirical_data@@GA,
#'      cex.points = 1.2, cex.lab = 1.1, cex.axis = 1.1,
#'      col = c("green3", "dodgerblue3", adjustcolor("green3", alpha.f = 0.1)),
#'     pch = c(16, 1), lty = c(1,2), grid = graphics:::grid)
#'
#' }
#'
#' @export

################################################################################
evolve_model <- function(data, outcome, actions, states, seed, fitness_func,
                         cols = NULL,
                         test_data = NULL, test_outcome = NULL,
                         popSize = 39, pcrossover = 0.8, pmutation = 0.1, maxiter = 20, run = 8,
                         parallel = FALSE,
                         priors = NULL,
                         boltzmann = FALSE, alpha=0.4) {
  # data has first col period and rest of cols are covariates
  # priors is a matrix of solutions strings to be included in the initialization,
  # where the number of columns == the number of bits. so you need to use the decoder funcs
  # to translate prior decision models into bits and then provide them.

  call <- match.call()

  ## GA-related errors:
  if (missing(fitness_func)) stop("Error: A fitness function must be provided --
                                        either an R function or a C++ function call-able in R.")
  if (popSize < 10) warning("The population size is less than 10. Consider using a larger size.")
  if (maxiter < 1) stop("Error: The maximum number of iterations must be at least 1.")
  if (pcrossover < 0 || pcrossover > 1) stop("Error: Probability of crossover must be between 0 and 1.")
  if (pmutation < 0 || pmutation > 1) stop("Error: Probability of mutation must be between 0 and 1.")
  ## Data-related errors:
  if (nrow(data)!=length(outcome)) stop("Error: The data (covariates) and the
                                              outcome variable are not the same length.")
  if (anyNA(outcome)) stop("Error: There are missing values in the data.")
  if (length(outcome)==0) stop("Error: The outcome is zero length.")
  if (missing(seed)) stop("Error: Set a seed to make this reproducible.")

  inputs <- 2^(ncol(data)-1) # bc 1 of the columns is for period, and the rest are covariates

  fitnessR <- function(s){ # functions defined elsewhere: decode_action_vec, decode_state_mat
    action_vec <- decode_action_vec(s, states, inputs, actions)
    state_mat <- decode_state_mat(s, states, inputs, actions)
    results <- fitness_func(action_vec, state_mat, data)

    if (anyNA(results) | length(results)==0){
      stop("Error: Results from fitness evaluation have missing values.")
    }

    sum(ifelse( results == outcome , 1 , 0)) / length(results)
  }

  warning_threshold <- 100

  valid_bs <- function(bs) {
    a <- decode_action_vec(bs, states, inputs, actions )
    sm <- decode_state_mat(bs, states, inputs, actions )
    all(a <= actions) && all(sm <= states)
  }

  valid_bsl <- function(x) {
    vbs <- function(i) valid_bs(x[i,])
    as.logical(lapply(1:nrow(x), vbs))
  }

  spCrossover <- function(object, parents, ...) {
    iter <- 0
    output <- NULL
    while(is.null(output)) {
      iter <- iter + 1
      output <- GA::gabin_spCrossover(object, parents, ...)
      children <- output$children
      if (! all(valid_bsl(children))) {
        if (iter > warning_threshold) {
          cat("Invalid crossover #", iter, '\n')
          print(children)
        }
        output <- NULL
      }
    }
    output
  }

  raMutation <- function(object, parent, ...) {
    iter <- 0
    output <- NULL
    while(is.null(output)) {
      iter <- iter + 1
      output <- GA::gabin_raMutation(object, parent, ...)
      if (! valid_bs(output)) {
        if (iter > warning_threshold) {
          cat("Invalid mutation #", iter, '\n')
          print(output)
        }
        output <- NULL
      }
    }
    output
  }

  poss.state.values <- (1:states) - 1
  b1 <- GA::decimal2binary(max(poss.state.values))
  l1 <- length(b1) #how many binary elements to represent one element of state matrix

  poss.action.values <- (1:actions) - 1
  b2 <- GA::decimal2binary(max(poss.action.values))
  l2 <- length(b2) #how many binary elements to represent one element of action matrix

  nBits <- (states*inputs*l1 + states*l2)

  build_priors <- function(popSize, nBits, states, inputs, actions) {
    priors <- matrix(nrow = popSize, ncol = nBits)
    for(i in 1:popSize) {
      av <- sample(actions,states,TRUE)
      sm <- matrix(sample(states,states * inputs,TRUE), nrow=states)
      priors[i,] <- build_bitstring(av,sm, actions)
    }
    #    prior_fitness <- unlist(lapply(1:popSize, function(i) fitnessR(priors[i,])))
    priors
  }

  if (missing(priors)) {
    priors <- build_priors(popSize, nBits, states, inputs, actions)
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
    if (nBits != ncol(priors)) stop("Error: Priors do not match number of variables.
                                                Remember that you need to provide a decoded bitstring for the priors.")
  }

  if (!boltzmann) {
    GA <- GA::ga(type = "binary",
             fitness = fitnessR,
             nBits = nBits,
             crossover = spCrossover,
             mutation = raMutation,
             popSize = popSize,
             pcrossover = pcrossover,
             pmutation = pmutation,
             maxiter = maxiter,
             run = run,
             maxfitness = 1,
             parallel = parallel,
             suggestions = priors,
             seed = seed)
  } else {
    GA <- GA::ga(type = "binary",
             fitness =  fitnessR,
             nBits = nBits,
             crossover = spCrossover,
             mutation = raMutation,
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
    if (anyNA(results) | length(results)==0){
      stop("Error: Results from fitness evaluation have missing values.")
    }
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
                actions = actions,
                states = states,
                GA = GA,
                state_mat = state_mat,
                action_vec = action_vec,
                predictive = predictive,
                degeneracy = list(message=message, dif=dif, sparse_state_mat = sparse_state_mat),
                varImp = varImp)

  object
}
