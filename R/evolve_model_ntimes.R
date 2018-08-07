#'Use a Genetic Algorithm to Estimate a Finite-state Machine Model n-times
#'
#'\code{evolve_model} uses a genetic algorithm to estimate a finite-state 
#'machine model, primarily for understanding and predicting decision-making.
#'
#'This function of the \strong{datafsm} package applies the \code{evolve_model} 
#'function multiple times and then returns a list with either all the models or 
#'the best one.
#'
#'\code{evolve_model} uses a stochastic meta-heuristic optimization routine to 
#'estimate the parameters that define a FSM model. Because this is not 
#'guaranteed to return the best result, we run it many times.
#'
#'@usage evolve_model_ntimes(data, test_data = NULL, drop_nzv = FALSE,
#'         measure = c("accuracy", "sens", "spec", "ppv"), 
#'         states = NULL, cv = FALSE, max_states = NULL, k = 2, 
#'         actions = NULL, seed = NULL, popSize = 75, 
#'         pcrossover = 0.8, pmutation = 0.1, maxiter = 50, 
#'         run = 25, parallel = FALSE, priors = NULL, 
#'         verbose = TRUE, return_best = TRUE, ntimes = 10, 
#'         cores = NULL)
#'
#'@param cores integer vector length one specifying number of cores to use if
#'  parallel is TRUE.
#'  
#'@inheritParams evolve_model
#'  
#'@return Returns a list where each element is an S4 object of class ga_fsm. See
#'  \linkS4class{ga_fsm} for the details of the slots (objects) that this type 
#'  of object will have and for information on the methods that can be used to 
#'  summarize the calling and execution of \code{evolve_model()}, including 
#'  \code{summary}, \code{print}, and \code{plot}.
#'  
#' @examples
#' # Create data:
#'cdata <- data.frame(period = rep(1:10, 1000),
#'                    outcome = rep(1:2, 5000),
#'                    my.decision1 = sample(1:0, 10000, TRUE),
#'                    other.decision1 = sample(1:0, 10000, TRUE))
#' (res <- evolve_model_ntimes(cdata, ntimes=2))
#' (res <- evolve_model_ntimes(cdata, return_best = FALSE, ntimes=2))
#'
#'@export

################################################################################
evolve_model_ntimes <- function(data, test_data = NULL, drop_nzv = FALSE,
                                measure = c("accuracy", "sens", "spec", "ppv"),
                                states = NULL, cv = FALSE, max_states = NULL, 
                                k = 2, actions = NULL,
                                seed = NULL,
                                popSize = 75, pcrossover = 0.8, pmutation = 0.1, 
                                maxiter = 50, run = 25,
                                parallel = FALSE,
                                priors = NULL,
                                verbose = TRUE,
                                return_best = TRUE, ntimes = 10, cores = NULL) {
  if (parallel) {
    if (is.null(cores)) cores <- parallel::detectCores() - 1
    doParallel::registerDoParallel(cores = cores)
  } else{
    cores <- 1
  }
  
  out <- parallel::mclapply(seq(ntimes), 
                            function(i) {
                              tryCatch(evolve_model(data = data, test_data = test_data, drop_nzv = drop_nzv,
                                                    measure = measure ,
                                                    states = states, cv = cv, max_states = max_states, k = k,
                                                    actions = actions,
                                                    seed = seed,
                                                    popSize = popSize, pcrossover = pcrossover, 
                                                    pmutation = pmutation, maxiter = maxiter, run = run,
                                                    parallel = FALSE, # This operation runs single threaded.
                                                    priors = priors,
                                                    verbose = verbose, 
                                                    ntimes = 1), 
                                       error = function(e) NA)},
                            mc.cores = cores)
  
  if(return_best){
    return(out[[which.min(vapply(out, best_performance, FUN.VALUE=numeric(1)))]])
  } else{
    return(out)
  }
}
