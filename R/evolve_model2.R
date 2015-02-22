#' Uses a Genetic Algorithm to Estimate a Finite-state Machine
#'
#' \code{evolve_model} uses a genetic algorithm to estimate a finite-state
#' machine model, primarily for understanding and predicting decision-making.
#'
#' This is the main function of the \strong{fsm} package. It takes data on
#' predictors and data on the outcome. It automatically creates a fitness
#' function that takes data, action vector, and state matrix as input and
#' returns numeric vector of same length as the \code{outcome}. This is then
#' used to compute a fitness score by comparing it to the provided
#' \code{outcome}. Because this function must loop through all the data, it is
#' implemented in C++ so it is fast. Then \code{evolve_model} uses a stochastic
#' meta-heuristic optimization routine to estimate the parameters. Generalized
#' simulated annealing could work. The current version uses the \strong{GA}
#' package's genetic algorithm because GAs perform well in rugged search spaces
#' to solve integer optimization problems and are a natural complement to our
#' binary string representation of FSMs.
#'
#' This function evolves the models on training data and then, if test set
#' provided, uses the best solution to make predictions on test data, and then
#' returns the GA object and the decoded version of the best string in the
#' population. See \linkS4class{ga_fsm} for the details of the slots (objects)
#' that this type of object will have.
#'
#' @param data Data frame that has "period" and "outcome" columns and rest of
#'   cols are predictors, ranging from one to three predictors. All of the (3-5
#'   columns) should be named.
#' @param states Optional numeric vector with the number of states.
#' @param actions Optional numeric vector with the number of actions. If not
#'   provided, then actions will be set as the number of unique values in the
#'   outcome vector.
#' @param seed Optional numeric vector length one.
#' @param cols Optional numeric vector same length as number of columns of the
#'   state matrix (\code{state_mat}) with the action that each column of the
#'   state matrix corresponds to the decision model taking in the previous
#'   period. This is only relevant when the predictor variables of the FSM are
#'   lagged outcomes that include the previous actions taken by that decision
#'   model.
#' @param test_data Optional numeric matrix that has first column the period of
#'   interaction between decision-makers and the rest of the columns are
#'   predictors for that decision. These predictors are often lagged decisions
#'   from previous time periods.
#' @param test_outcome Optional numeric vector same length as the number of rows
#'   as test_data with the decision the decision-maker took for that period.
#' @param popSize Optional numeric vector length one specifying the size of the
#'   GA population. A larger number will increase the probability of finding a
#'   very good solution but will also increase the computation time. This is
#'   passed to the ga() function of the GA package.
#' @param pcrossover Optional numeric vector length one specifying probability
#'   of crossover for GA. This is passed to the ga() function of the GA package.
#' @param pmutation Optional numeric vector length one specifying probability of
#'   mutation for GA. This is passed to the ga() function of the GA package.
#' @param maxiter Optional numeric vector length one specifying max number of
#'   iterations for stopping the GA evolution. A larger number will increase the
#'   probability of finding a very good solution but will also increase the
#'   computation time. This is passed to the ga() function of the GA package.
#' @param run Optional numeric vector length one specifying max number of
#'   consecutive iterations without improvement in best fitness score for
#'   stopping the GA evolution. A larger number will increase the probability of
#'   finding a very good solution but will also increase the computation time.
#'   This is passed to the ga() function of the GA package.
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
#' @return Returns an S4 object of class ga_fsm. See \linkS4class{ga_fsm} for
#'   the details of the slots (objects) that this type of object will have and
#'   for information on the methods that can be used to summarize the calling
#'   and execution of \code{evolve_model()}, including \code{\link{summary}}.
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
#'                                               fitness_func = fitnessC, cols = c(1, 2, 1, 2),
#'                                               test_data =  test_data, test_outcome = test_outcome,
#'                                               parallel = TRUE)
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
#' }
#'
#' @export

data <- data.frame(period = 1:5, outcome = c(1,0,1,1,1),
                   my.decision1 = as.logical(c(1,0,1,1,1)),
                   other.decision1 = as.logical(c(0,0,0,1,1)))

################################################################################
evolve_model <- function(data,
                         states = 2,
                         actions = NULL,
                         seed = NULL,
                         cols = NULL,
                         test_data = NULL, test_outcome = NULL,
                         popSize = 75, pcrossover = 0.8, pmutation = 0.1, maxiter = 55, run = 25,
                         parallel = FALSE,
                         priors = NULL,
                         boltzmann = FALSE, alpha=0.4) {
        # priors is a matrix of solutions strings to be included in the initialization,
        # where the number of columns == the number of bits. so you need to use the decoder funcs
        # to translate prior decision models into bits and then provide them.

        #TODO: add automatic run CV across states==2:4 on training data to find optimal value
        # in terms of generalization performance.

        # made it so fitnessR() can be built in here
        # without needing to pass in any fitness_func arg. you will just need to pass in a
        # matrix with predictor columns that are only binary for whether that predictor
        # variable is true or false for that period. And then a period column that is
        # integer valued denoting what period that row corresponds to. Then in here we take the
        # predictor columns and use model.matrix() to create new cols for all the combinations of
        # values of predictors.

        call <- match.call()

        ## GA-related errors:
        if (popSize < 10) warning("The population size is less than 10. Consider using a larger size.")
        if (maxiter < 1) stop("Error: The maximum number of iterations must be at least 1.")
        if (pcrossover < 0 || pcrossover > 1) stop("Error: Probability of crossover must be between 0 and 1.")
        if (pmutation < 0 || pmutation > 1) stop("Error: Probability of mutation must be between 0 and 1.")
        ## Data-related errors:
        period <- data$period
        outcome <- data$outcome

        if (nrow(data)!=length(outcome)) stop("Error: The data (covariates) and the
                                              outcome variable are not the same length.")
        if (anyNA(outcome)) stop("Error: There are missing values in the data.")
        if (length(outcome) == 0) stop("Error: The outcome is zero length.")
        if (missing(seed)) {
                seed <- floor(runif(1, 1,101))
                warning(paste("We set a seed for you to make this reproducible. It is ", seed, ".", sep=""))
        }
        if (missing(actions)) {
                if(length(unique(outcome))==1){
                        Error("Error: There is only one unique values in the
                                                  outcome vector you supplied.")
                } else {
                        actions <- length(unique(outcome))
                }
        } else {
                if (length(unique(outcome)) != actions) {warning("The number of unique values in the
                                                  outcome vector you supplied does not
                                                  equal the value of actions you supplied.
                                                  The outcome vector should be a vector of
                                                  observed actions. We are going to use the
                                                  number of unique values in the outcome
                                                  vector you supplied as the value of actions.")
                                                         actions <- length(unique(outcome))
                }
        }

        inputs <- 2^(ncol(data[ , -which(names(data) %in% c("period", "outcome"))]))
        #  data <- data[ , -which(names(data) %in% c("period", "outcome"))]

        names <- colnames(data[ , -which(names(data) %in% c("period", "outcome"))])
        if (length(names)==1){
                names[1] <- parse(text=(names[1]))
                data <- model.matrix(outcome ~ 0 + eval(names[1]):eval(names[2]), data)
        } else {
                if (length(names)==2){
                        names[1] <- parse(text=(names[1]))
                        names[2] <- parse(text=(names[2]))
                        data <- model.matrix(outcome ~ 0 + eval(names[1]):eval(names[2]), data)
                } else {
                        if (length(names)==3){
                                names[1] <- parse(text=(names[1]))
                                names[2] <- parse(text=(names[2]))
                                names[3] <- parse(text=(names[3]))
                                data <- model.matrix(outcome ~ 0 + eval(names[1]):eval(names[2]):eval(names[3]), data)
                        } else {
                                stop("Error: You have more than 3 predictors.
                                             Your model will be too complicated. Do some type of feature selection
                                             to choose less than 4 predictors and then use the data.frame
                                             with just those features next time.")
                        }
                }
        }

        if (ncol(data) != inputs)
                stop("Error: At least one of your predictor variables does not have exactly 2 levels.")

        #   for (i in seq(from=1, to=ncol(data)+1, by=2)){
        #           name <- colnames(data[i])
        #           newcol <- data.frame(!data[ , i, drop = FALSE])
        #           colnames(newcol) <- paste(name, "F", sep="")
        #           data1 <- data.frame(data[ , 1:i, drop = FALSE])
        #           if (i==ncol(data)){
        #                   data <- cbind(data1, newcol)
        #           } else {
        #                   data2 <- data.frame(data[ , (i+1):ncol(data), drop = FALSE])
        #                   data <- cbind(data1, newcol, data2)
        #           }
        #   }
        fitnessR <- function(s){ # functions defined elsewhere: decode_action_vec, decode_state_mat
                action_vec <- decode_action_vec(s, states, inputs, actions)
                state_mat <- decode_state_mat(s, states, inputs, actions)
                results <- fitnessCPP(action_vec, state_mat, data, period)

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
                test_period <- test_data$period
                names <- colnames(test_data[ , -which(names(test_data) %in% c("period", "outcome"))])
                if (length(names)==1){
                        names[1] <- parse(text=(names[1]))
                        test_data <- model.matrix(outcome ~ 0 + eval(names[1]):eval(names[2]), test_data)
                } else {
                        if (length(names)==2){
                                names[1] <- parse(text=(names[1]))
                                names[2] <- parse(text=(names[2]))
                                test_data <- model.matrix(outcome ~ 0 + eval(names[1]):eval(names[2]), test_data)
                        } else {
                                if (length(names)==3){
                                        names[1] <- parse(text=(names[1]))
                                        names[2] <- parse(text=(names[2]))
                                        names[3] <- parse(text=(names[3]))
                                        test_data <- model.matrix(outcome ~ 0 + eval(names[1]):eval(names[2]):eval(names[3]), test_data)
                                } else {
                                        stop("Error: You have more than 3 predictors.
                                             Your model will be too complicated. Do some type of feature selection
                                             to choose less than 4 predictors and then use the data.frame
                                             with just those features next time.")
                                }
                        }
                }
                results <- fitnessCPP(action_vec, state_mat, test_data, test_period)
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
                #     d_check <- degeneracy_check(state_mat, action_vec, cols, fitnessCPP, data, outcome, period)
                #     dif <- d_check$dif
                #     sparse_state_mat <- d_check$sparse_state_mat
                #     corrected_state_mat <- d_check$corrected_state_mat
                #
                #     if (any(dif >= 0)) {
                #       message <- ifelse(any(dif == 0), "See the sparse matrix returned. The elements in that matrix with a 0 are unidentifiable. Their value makes no difference to the fit of the strategy to the provided data.",
                #                         "You have not found an optimal strategy, by randomly flipping values of some components, we improved it.")
                #     } else {
                #       message <- ifelse(all(dif < 0), "Your strategy is a deterministic approximation of a stochastic process. All of the elements of the state matrix can be identified.",
                #                         "You could improve your strategy by changing at least one component to its opposite value.")
                #     }
        }

        # if the model is fine, then corrected_state_mat should be equal to state_mat
        #varImp <- var_imp(corrected_state_mat, action_vec, fitness_func, data, outcome, cols)

        object <- new("ga_fsm",
                      call = call,
                      actions = actions,
                      states = states,
                      GA = GA,
                      state_mat = state_mat,
                      action_vec = action_vec,
                      predictive = predictive,
                      degeneracy = list(message=message, dif=dif, sparse_state_mat = sparse_state_mat)) #,
        #varImp = varImp)

        object
}
