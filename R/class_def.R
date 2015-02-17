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

################################################################################
#' An S4 class to return the results of using a GA to estimate a FSM with 
#' \code{\link{evolve_model}}.
#' 
#' @slot call Language from the call of the function \code{\link{evolve_model}}.
#' @slot actions Numeric vector with the number of actions.
#' @slot states Numeric vector with the number of states.
#' @slot GA S4 object created by ga() from the GA package.
#' @slot state_mat Numeric matrix with rows as states and columns as predictors.
#' @slot action_vec Numeric vector indicating what action to take for each 
#'   state.
#' @slot predictive Numeric vector of length one with test data accuracy if test
#'   data was supplied; otherwise, a character vector with a message that the 
#'   user should provide test data for better estimate of performance.
#' @slot degeneracy List with message and sparse matrix.
#' @slot varImp Numeric vector same length as number of columns of state matrix 
#'   with relative importance scores for each predictor.

setClass("ga_fsm",
         slots = c(call = "language",
                   actions = "numeric",
                   states = "numeric",
                   GA = "ANY", # from package "GA"
                   state_mat = "matrix",
                   action_vec = "numeric",
                   predictive = "numericOrchar",
                   degeneracy = "list",
                   varImp = "numeric"),
         validity = check_ga_fsm
)

################################################################################
#' @describeIn ga_fsm An S4 method for printing a ga_fsm S4 object
#'  @export

setMethod("print",
          "ga_fsm",
          function(x, ...) str(x)
)

################################################################################
#' @describeIn ga_fsm An S4 method for showing a ga_fsm S4 object
#' @param object S4 ga_fsm object
#'  @export

setMethod("show", "ga_fsm",
          function(object) {
            cat("An object of class \"ga_fsm\"\n")
            cat("\nCall:\n", deparse(object@call), "\n\n",sep="")
            cat("Available slots:\n")
            print(slotNames(object))
          }
)

################################################################################
#' Turns ga_fsm S4 object into list for printing
#' @param object S4 ga_fsm object
#'  @export

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
  out
}

################################################################################
#' @describeIn ga_fsm An S4 method for summarizing a ga_fsm S4 object
#'  @export
setMethod("summary", "ga_fsm", summary.ga_fsm)

################################################################################
#' Prints a summary of a ga_fsm S4 object
#' @param x ga_fsm object
#' @param digits numeric vector length one for how many significant digits to
#' print
#'  @export

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
