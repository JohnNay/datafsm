% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/evolve_model.R
\name{evolve_model}
\alias{evolve_model}
\title{Use a Genetic Algorithm to Estimate a Finite-state Machine Model}
\usage{
evolve_model(data, test_data = NULL, drop_nzv = FALSE,
        measure = c("accuracy", "sens", "spec", "ppv"),
        states = NULL, cv = FALSE, max_states = NULL, k = 2,
        actions = NULL, seed = NULL, popSize = 75,
        pcrossover = 0.8, pmutation = 0.1, maxiter = 50,
        run = 25, parallel = FALSE, priors = NULL,
        verbose = TRUE, return_best = TRUE, ntimes = 1)
}
\arguments{
\item{data}{A \code{data.frame} that has columns named "period" and "outcome"
(period is the time period that the outcome action was taken), and one to
three additional columns, containing predictors.
All of the 3-5 columns should be named.
The period and outcome columns should be integer vectors and the columns
with the predictor variable data should be logical vectors
(\code{TRUE, FALSE}).
If the predictor variable data is not  logical, it will coerced to logical
with \cr
\code{base::as.logical()}.}

\item{test_data}{Optional \code{data.frame} that has "period" and "outcome"
columns, with one to three additional columns containing predictors.
All of the (3-5 columns) should be named. The outcome variable is the
decision the decision-maker took for that period.
This \code{data.frame} should be in the same format and have the same order
of columns as the data.frame passed to the required \code{data} argument.}

\item{drop_nzv}{Optional logical vector length one specifying whether
predictors variables with variance in provided data near zero should be
dropped before model building. Default is \code{FALSE}. See
\code{caret::nearZeroVar()}, which calls:\cr
\code{caret::nzv()}.}

\item{measure}{Optional length one character vector that is either:
"accuracy", "sens", "spec", or "ppv". This specifies what measure of
predictive performance to use for training and evaluating the model. The
default measure is \code{"accuracy"}. However, accuracy can be a problematic
measure when the classes are imbalanced in the samples, i.e. if a class the
model is trying to predict is very rare. Alternatives to accuracy are
available that illuminate different aspects of predictive power. Sensitivity
answers the question, `` given that a result is truly an event, what is the
probability that the model will predict an event?'' Specificity answers the
question, ``given that a result is truly not an event, what is the
probability that the model will predict a negative?'' Positive predictive
value answers, ``what is the percent of predicted positives that are
actually positive?''}

\item{states}{Optional numeric vector with the number of states.
If not provided, will be set to \code{max(data$outcome)}.}

\item{cv}{Optional logical vector length one for whether cross-validation
should be conducted on training data to select optimal number of states.
This can drastically increase computation time because if \code{TRUE}, it
will run \code{evolve_model} \code{k*max_states} times to estimate optimal
value for states. Ties are broken by choosing the smaller number of states.
Default is \code{FALSE}.}

\item{max_states}{Optional numeric vector length one only relevant if
\code{cv==TRUE}. It specifies how up to how many states that
cross-validation should search through.
If not provided, will be set to \code{states + 1}.}

\item{k}{Optional numeric vector length one only relevant if cv==TRUE,
specifying number of folds for cross-validation.}

\item{actions}{Optional numeric vector with the number of actions. If not
provided, then actions will be set as the number of unique values in the
outcome vector.}

\item{seed}{Optional numeric vector length one.}

\item{popSize}{Optional numeric vector length one specifying the size of the
GA population. A larger number will increase the probability of finding a
very good solution but will also increase the computation time. This is
passed to the GA::ga() function of the \strong{GA} package.}

\item{pcrossover}{Optional numeric vector length one specifying probability of
crossover for GA. This is passed to the GA::ga() function of the \strong{GA}
package.}

\item{pmutation}{Optional numeric vector length one specifying probability of
mutation for GA. This is passed to the GA::ga() function of the \strong{GA}
package.}

\item{maxiter}{Optional numeric vector length one specifying max number of
iterations for stopping the GA evolution. A larger number will increase the
probability of finding a very good solution but will also increase the
computation time. This is passed to the GA::ga() function of the \strong{GA}
package. \code{maxiter} is scaled by how many parameters are in the model:\cr
\code{maxiter <- maxiter + ((maxiter*(nBits^2)) / maxiter)}.}

\item{run}{Optional numeric vector length one specifying max number of
consecutive iterations without improvement in best fitness score for
stopping the GA evolution. A larger number will increase the probability of
finding a very good solution but will also increase the computation time.
This is passed to the GA::ga() function of the \strong{GA} package.}

\item{parallel}{Optional logical vector length one. For running the GA
 evolution in parallel. Depending on the number of cores registered and the
memory on your machine, this can make the process much faster, but only works
for Unix-based machines that can fork the processes.}

\item{priors}{Optional numeric matrix of solutions strings to be included in
the initialization. User needs to use a decoder function to translate prior
decision models into bits and then provide them. If this is not specified,
then random priors are automatically created.}

\item{verbose}{Optional logical vector length one specifying whether helpful
messages should be displayed on the user's console or not.}

\item{return_best}{Optional logical vector length one specifying whether to
return just the best model or all models. Only relevant if ntimes > 1.
Default is TRUE.}

\item{ntimes}{Optional integer vector length one specifying the number of
times to estimate model. Default is 1 time.}
}
\value{
Returns an S4 object of class ga_fsm. See \linkS4class{ga_fsm} for the
 details of the slots (objects) that this type of object will have and for
 information on the methods that can be used to summarize the calling and
 execution of \code{evolve_model()}, including \code{summary}, \code{print},
 and \code{plot}. Timing measurement is in seconds.
}
\description{
\code{evolve_model} uses a genetic algorithm to estimate a finite-state
machine model, primarily for understanding and predicting decision-making.
}
\details{
This is the main function of the \strong{datafsm} package. It relies on the
\strong{GA} package for genetic algorithm optimization. \code{evolve_model}
takes data on predictors and data on the outcome. It automatically creates a
fitness function that takes the data, an action vector \code{evolve_model}
generates, and a state matrix \code{evolve_model} generates as input and
returns numeric vector of the same length as the \code{outcome}.
\code{evolve_model} then computes a fitness score for that potential solution
FSM by comparing it to the provided \code{outcome}. This is repeated for every
FSM in the population and then the probability of selection for the next
generation is proportional to the fitness scores. The default is also for the
function to call itself recursively while varying the number of states inside
a cross-validation loop in order to estimate the optimal number of states.

If parallel is set to TRUE, then these evaluations are distributed across the
available processors of the computer using the \strong{doParallel} package,
otherwise, the evaluations of fitness are conducted sequentially. Because
this fitness function that \code{evolve_model} creates must loop through all
the data every time it is evaluated and we need to evaluate many possible
solution FSMs, the fitness function is implemented in C++ so it is very fast.

\code{evolve_model} uses a stochastic meta-heuristic optimization routine to
estimate the parameters that define a FSM model. Generalized simulated
annealing, or tabu search could work, but they are more difficult to
parallelize. The current version uses the \strong{GA} package's genetic
algorithm because GAs perform well in rugged search spaces to solve integer
optimization problems, are a natural complement to our binary string
representation of FSMs, and are easily parallelized.

This function evolves the models on training data and then, if a test set is
provided, uses the best solution to make predictions on test data. Finally,
the function returns the GA object and the decoded version of the best string
in the population. See \linkS4class{ga_fsm} for the details of the slots
(objects) that this type of object will have.
}
\examples{
\dontrun{
# Create data:
cdata <- data.frame(period = rep(1:10, 1000),
                   outcome = rep(1:2, 5000),
                   my.decision1 = sample(1:0, 10000, TRUE),
                   other.decision1 = sample(1:0, 10000, TRUE))
(res <- evolve_model(cdata, cv=FALSE))
summary(res)
plot(res, action_label = c("C", "D"))
library(GA)
plot(estimation_details(res))
}

# In scripts, it can makes sense to set parallel to
# 'as.logical(Sys.info()['sysname'] != 'Windows')'.

}
\references{
Luca Scrucca (2013). GA: A Package for Genetic Algorithms in R.
 Journal of Statistical Software, 53(4), 1-37. URL
 \url{https://www.jstatsoft.org/v53/i04/}.
}
