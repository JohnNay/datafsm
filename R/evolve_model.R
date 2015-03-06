#' Use a Genetic Algorithm to Estimate a Finite-state Machine Model
#'
#' \code{evolve_model} uses a genetic algorithm to estimate a finite-state
#' machine model, primarily for understanding and predicting decision-making.
#'
#' This is the main function of the \strong{fsm} package. It takes data on
#' predictors and data on the outcome. It automatically creates a fitness
#' function that takes the data, an action vector \code{evolve_model} generates,
#' and a state matrix \code{evolve_model} generates as input and returns numeric
#' vector of same length as the \code{outcome}. \code{evolve_model} then
#' computes a fitness score for that potential solution FSM by comparing it to
#' the provided \code{outcome}. This is repeated for every FSM in the
#' population. If parallel is set to TRUE, then these evaluations are
#' distributed across the available processors of the computer, otherwise, the
#' evalulations of fitness are conducted sequentially. Because this fitness
#' function that \code{evolve_model} creates must loop through all the data
#' everytime it is evaluated and we need to evaluate many possible solution
#' FSMs, the fitness function is implemented in C++ so it is very fast.
#'
#' \code{evolve_model} uses a stochastic meta-heuristic optimization routine to
#' estimate the parameters that define a FSM model. Generalized simulated
#' annealing, or tabu search could work, but they are more difficult to
#' parallelize. The current version uses the \strong{GA} package's genetic
#' algorithm because GAs perform well in rugged search spaces to solve integer
#' optimization problems, are a natural complement to our binary string
#' representation of FSMs, and are easily parallelized.
#'
#' This function evolves the models on training data and then, if a test set is
#' provided, uses the best solution to make predictions on test data. Finally,
#' the function returns the GA object and the decoded version of the best string
#' in the population. See \linkS4class{ga_fsm} for the details of the slots
#' (objects) that this type of object will have.
#'
#' @param data Data frame that has "period" and "outcome" columns and rest of
#'   cols are predictors, ranging from one to three predictors. All of the (3-5
#'   columns) should be named.
#' @param test_data Optional Data frame that has "period" and "outcome" columns
#'   and rest of cols are predictors, ranging from one to three predictors. All
#'   of the (3-5 columns) should be named. Outcome variable is the decision the
#'   decision-maker took for that period.
#' @param states Optional numeric vector with the number of states.
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
#' @return Returns an S4 object of class ga_fsm. See \linkS4class{ga_fsm} for
#'   the details of the slots (objects) that this type of object will have and
#'   for information on the methods that can be used to summarize the calling
#'   and execution of \code{evolve_model()}, including \code{\link{summary}} and
#'   print.
#'
#' @examples
#' # Create data:
#' cdata <- data.frame(period = 1:5, outcome = c(1,2,1,1,1),
#' my.decision1 = c(1,0,1,1,1), other.decision1 = c(0,0,0,1,1))
#' (result <- evolve_model(cdata))
#'
#' # Use data from this package:
#' data(ipd_data)
#' percent_train <- 0.80
#' ipd_samples <- nrow(ipd_data)
#' ntrain <- as.integer(percent_train * ipd_samples)
#' # Back up to just before a new interaction starts:
#' ntrain <- ntrain - ipd_data[ntrain,"period"]
#' train_indices <- 1:ntrain
#' test_indices <- 1:ipd_samples
#' test_indices <- test_indices[! test_indices %in% train_indices]
#' train_data <- ipd_data[train_indices, ]
#' test_data <- ipd_data[test_indices,]
#' summary(evolve_model(train_data, test_data, run = 5))
#' @export

################################################################################
evolve_model <- function(data, test_data = NULL,
                         states = 2,
                         actions = NULL,
                         seed = NULL,
                         popSize = 75, pcrossover = 0.8, pmutation = 0.1, maxiter = 55, run = 25,
                         parallel = FALSE,
                         priors = NULL,
                         boltzmann = FALSE, alpha=0.4) {
        # priors is a matrix of solutions strings to be included in the initialization,
        # where the number of columns == the number of bits. so you need to use the decoder funcs
        # to translate prior decision models into bits and then provide them.

        # made it so fitnessR() can be built in here
        # without needing to pass in any fitness_func arg. you will just need to pass in a
        # matrix with predictor columns that are only binary for whether that predictor
        # variable is true or false for that period. And then a period column that is
        # integer valued denoting what period that row corresponds to. Then in here we take the
        # predictor columns and use model.matrix() to create new cols for all the combinations of
        # values of predictors.
        #TODO: varImp to generalize to more than 2-state FSMs
        #TODO: add automatic run CV across states==2:4 on training data to find optimal value
        # in terms of generalization performance.

        call <- match.call()

        ## GA-related errors:
        if (popSize < 10) warning("The population size is less than 10. Consider using a larger size.")
        if (maxiter < 1) stop("Error: The maximum number of iterations must be at least 1.")
        if (pcrossover < 0 || pcrossover > 1) stop("Error: Probability of crossover must be between 0 and 1.")
        if (pmutation < 0 || pmutation > 1) stop("Error: Probability of mutation must be between 0 and 1.")
        if(!requireNamespace("doParallel", quietly = TRUE) & parallel == TRUE)
                stop("You asked to run this in parallel, but you dont have package doParallel installed.
                     run install.package() for this package, library() it and then try this again.")
        ## Data-related errors:
        period <- data$period
        outcome <- data$outcome
        test_period <- test_data$period
        test_outcome <- test_data$outcome

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
                        stop("Error: There is only one unique value in the
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

        # so we are assured that the action vec will just need to be comprised of the possible
        # number of actions in the data:
        if (!all.equal(unique(outcome), as.numeric(unique(seq(length(unique(outcome))))),
                       ignore.environment = TRUE)){
                stop("Error: The actions in the outcome column of the data are not the right values.
                     There should be actions sequenced from 1 to however many actions that are feasible.
                     E.g., if there are two feasible actions, then the outcome column should be comprised
                     of only 1s and 2s, with at least one 1 and and at least one 2. If there are three feasible
                     actions, the outcome column should be comprised of only 1s, 2s, and 3s, with at least one
                     1 and, at least one 2, and at least one 3.")
        }

        inputs <- 2^(ncol(data[ , -which(names(data) %in% c("period", "outcome"))]))

        # change any non-logical predictor variable vectors to logical
        data[ , -which(names(data) %in% c("period", "outcome"))] <- data.frame(lapply(data[ , -which(names(data) %in% c("period", "outcome"))],
                                                                                      function(x) {
                                                                                              if (class(x)!="logical") {
                                                                                                      as.logical(x)
                                                                                              } else {
                                                                                                      x
                                                                                              }}))
        #replace all NA's with 0 or 1 so these rows are not dropped
        # TODO: this works fine if the NAs are only for the first period play bc
        # then the predictor columns dont make a difference bc the FSM will initialize
        # with the same action regardless of the predictors at that time
        # but this would bias the results if NA's are occuring in predictors in other periods
        data[is.na(data)] <- TRUE

        names <- colnames(data[ , -which(names(data) %in% c("period", "outcome"))])

        if (length(names)==1){
                form <- paste("outcome ~ 0 +", names, sep=" ")
                data <- model.matrix(eval(parse(text=form)), data)
        } else {
                predictors <- paste(names, collapse=":")
                form <- paste("outcome ~ 0 +", predictors, sep=" ")
                data <- model.matrix(eval(parse(text=form)), data)
        }

        if (length(names) > 3) stop("Error: You have more than 3 predictors.
                                  Your model will be too complicated.
                                  Do some type of feature selection to choose less
                                  than 4 predictors and then use the data.frame
                                  with just those features next time.")

        if (ncol(data) != inputs)
                stop("Error: At least one of your predictor variables does not have exactly 2 levels.")

        cols <- colnames(data)
        # numeric vector same length as number of columns of the
        # state matrix (\code{state_mat}) with the action that each column of the
        # state matrix corresponds to the decision model taking in the previous
        # period. This is only relevant when the predictor variables of the FSM are
        # lagged outcomes that include the previous actions taken by that decision model.

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
        colnames(state_mat) <- cols

        action_vec <- decode_action_vec(GA@solution[1, ],  states, inputs, actions)

        if (missing(test_data)){
                predictive <- "No test data provided. Provide some to get more accurate estimation of generalization power."
        } else {
                if (!all.equal(unique(test_outcome), as.numeric(unique(seq(length(unique(test_outcome))))),
                               ignore.environment = TRUE)){
                        stop("Error: The actions in the outcome column of the test data
                              are not the right values. There should be actions sequenced from
                              1 to however many actions that are feasible. E.g., if there are
                              two feasible actions, then the outcome columns should be comprised
                              of only 1s and 2s, with at least one 1 and and at least one 2.")
                }

                test_inputs <- 2^(ncol(test_data[ , -which(names(test_data) %in% c("period", "outcome"))]))

                # change any non-logical predictor variable vectors to logical
                test_data[ , -which(names(test_data) %in% c("period", "outcome"))] <- data.frame(lapply(test_data[ , -which(names(test_data) %in% c("period", "outcome"))],
                                                                                                        function(x) {
                                                                                                                if (class(x)!="logical") {
                                                                                                                        as.logical(x)
                                                                                                                } else {
                                                                                                                        x
                                                                                                                }}))

                #replace all NA's with 0 or 1 so these rows are not dropped
                # TODO: this works fine if the NAs are only for the first period play bc
                # then the predictor columns dont make a difference bc the FSM will initialize
                # with the same action regardless of the predictors at that time
                # but this would bias the results if NA's are occuring in predictors in other periods
                test_data[is.na(test_data)] <- TRUE

                names <- colnames(test_data[ , -which(names(test_data) %in% c("period", "outcome"))])

                if (length(names)==1){
                        form <- paste("outcome ~ 0 +", names, sep=" ")
                        test_data <- model.matrix(eval(parse(text=form)), test_data)
                } else {
                        predictors <- paste(names, collapse=":")
                        form <- paste("outcome ~ 0 +", predictors, sep=" ")
                        test_data <- model.matrix(eval(parse(text=form)), test_data)
                }

                if (length(names) > 3) stop("Error: You have more than 3 predictors.
                                  Your model will be too complicated.
                                  Do some type of feature selection to choose less
                                  than 4 predictors and then use the data.frame
                                  with just those predictors next time.")

                if (ncol(test_data) != test_inputs)
                        stop("Error: At least one of your predictor variables in your test data
                             does not have exactly 2 levels.")

                test_cols <- colnames(test_data)

                results <- fitnessCPP(action_vec, state_mat, test_data, test_period)
                if (anyNA(results) | length(results)==0){
                        stop("Error: Results from fitness evaluation have missing values.")
                }
                predictive <-  sum(ifelse( results == test_outcome , 1 , 0)) / length(results)
        }

        # degeneracy_check
        #             d_check <- degeneracy_check(state_mat, action_vec, cols, data, outcome, period)
        #             dif <- d_check$dif
        #             sparse_state_mat <- d_check$sparse_state_mat
        #             corrected_state_mat <- d_check$corrected_state_mat
        #
        #             if (any(dif >= 0)) {
        #               message <- ifelse(any(dif == 0), "See the sparse matrix returned. The elements in that matrix with a 0 are unidentifiable. Their value makes no difference to the fit of the strategy to the provided data.",
        #                                 "You have not found an optimal strategy, by randomly flipping values of some components, we improved it.")
        #             } else {
        #               message <- ifelse(all(dif < 0), "Your strategy is a deterministic approximation of a stochastic process. All of the elements of the state matrix can be identified.",
        #                                 "You could improve your strategy by changing at least one component to its opposite value.")
        #             }
        # if the model is fine, then corrected_state_mat should be equal to state_mat

        # Variable Importance:
        #varImp <- var_imp(corrected_state_mat, action_vec, fitness_func, data, outcome, cols)
        varImp <- var_imp(state_mat, action_vec, data, outcome, period)

        object <- new("ga_fsm",
                      call = call,
                      actions = actions,
                      states = states,
                      GA = GA,
                      state_mat = state_mat,
                      action_vec = action_vec,
                      predictive = predictive,
                      varImp = varImp) #,
        #degeneracy = list(message=message, dif=dif, sparse_state_mat = sparse_state_mat)))

        object
}
