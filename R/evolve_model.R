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
        
        ## Packages
        require(GA)
        if (parallel == TRUE) require(parallel)
        
        inputs <- 2^(ncol(data)-1) # bc 1 of the columns is for period, and the rest are covariates
        
        fitnessR <- function(s){ # functions defined in globalenv() above: decode_action_vec, decode_state_mat
                action_vec <- decode_action_vec(s, states, inputs, actions)
                state_mat <- decode_state_mat(s, states, inputs, actions)
                results <- fitness_func(action_vec, state_mat, data)
                
                stopifnot(!anyNA(results), length(results)!=0)
                
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
                GA <- ga(type = "binary", 
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
                GA <- ga(type = "binary", 
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

################################################################################
check_ga_fsm <- function(object) {
        errors <- character()
        #         timeSteps <- times(object)
        #         if (timeSteps[1] > timeSteps[2]) {
        #                 msg <- paste("Initial time is ", timeSteps[1],
        #                              ".  Ending time is ", timeSteps[2], ".", sep = "")
        #                 errors <- c(errors, msg)
        #         }
        
        # TODO: add checks here.

        if (length(errors) == 0) TRUE else errors
}

################################################################################
setClassUnion("numericOrchar", members = c("numeric", "character"))

setClass("ga_fsm",
         slots = c(call = "language", 
                   actions = "numeric",
                   states = "numeric",
                   GA = "ga", # from package "GA"
                   state_mat = "matrix", 
                   action_vec = "numeric", 
                   predictive = "numericOrchar",
                   degeneracy = "list",
                   varImp = "numeric"),
         validity = check_ga_fsm
)

################################################################################
setMethod("print", "ga_fsm", function(x, ...) str(x))

################################################################################
setMethod("show", "ga_fsm",
          function(object) { 
                  cat("An object of class \"ga_fsm\"\n")
                  cat("\nCall:\n", deparse(object@call), "\n\n",sep="")
                  cat("Available slots:\n")
                  print(slotNames(object))
          }
) 

################################################################################
summary.ga_fsm <- function(object, ...) {
        out <- list(
                # ga-related
                popSize = object@GA@popSize,
                maxiter = object@GA@maxiter,
                elitism = object@GA@elitism,
                pcrossover = object@GA@pcrossover,
                pmutation = object@GA@pmutation,
                iter = object@GA@iter,
                fitness = object@GA@fitnessValue,
                bit_string_solution = object@GA@solution,
                # fsm-related
                actions = object@actions, 
                states = object@states,
                state_mat = object@state_mat,
                action_vec = object@action_vec,
                predictive = object@predictive,
                degeneracy = object@degeneracy,
                varImp = object@varImp
        )  
        class(out) <- "summary.ga_fsm"
        return(out)
}

################################################################################
setMethod("summary", "ga_fsm", summary.ga_fsm)

################################################################################
print.summary.ga_fsm <- function(x, digits = 3, ...) {
        cat("                                    \n")
        cat("           GA-FSM Results:          \n")
        cat("                                    \n")
        
        cat("Gentic Algorithm Settings: \n")
        cat(paste("Population size       = ", x$popSize, "\n"))
        cat(paste("Number of generations = ", x$maxiter, "\n"))
        cat(paste("Elitism               = ", x$elitism, "\n"))
        cat(paste("Crossover probability = ", format(x$pcrossover, digits = digits), "\n"))
        cat(paste("Mutation probability  = ", format(x$pmutation, digits = digits), "\n"))
        
        cat("\nFinite State Machine Settings: \n")
        cat(paste("Actions = ", x$actions, "\n"))
        cat(paste("States  = ", x$states, "\n"))
        
        cat("\nResults: \n\n")
        cat(paste("Iterations For This Run              =", format(x$iter, digits = digits), "\n"))
        cat(paste("Training Data Fitness Function Value =", format(x$fitness, digits = digits), "\n"))
        cat(paste("Test Data Fitness Function Value     =", format(x$predictive, digits = digits), "\n"))
        
        cat(paste("\n(Bit String Form) of Solution: \n")) 
        print(x$bit_string_solution[1, ], digits = digits)
        
        cat("\nState Matrix of Solution: \n")
        print(x$state_mat, digits = digits)
        
        cat("\nAction Vector of Solution: \n")
        print(x$action_vec, digits = digits)
        
        cat(paste("\nFriendly Degeneracy Message:", x$degeneracy$message, "\n"))
        #         TODO: degeneracy$diff TODO: degeneracy$sparse_state_mat
        
        cat("\nVariable Importance: \n")
        print(x$varImp, digits = digits)
        
        invisible()
}