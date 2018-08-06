#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
int predictor_lookup(const IntegerVector x){
  int result = -1;
  int num_ones = 0;

  { // limit scope of loop variables
    IntegerVector::const_iterator it;
    int i;

    for(it = x.begin(), i = 0; it != x.end(); ++it, ++i){
      if (*it > 1 || *it < 0){
        Rprintf("Illegal value for: %d.\n", *it);
      }
      if (*it == 1){
        result = i; // Not using R indexing convention here
        ++num_ones;
      }
    }
    if (i != x.length())
      Rcpp::stop("predictor_lookup took %d iterations for a vector of length %d", i, x.length());
  }
  if (num_ones > 1)
    Rcpp::stop("There are %d 1s in this row, but there can only be one 1 in each row.\n", num_ones);
  if (num_ones == 0)
    Rcpp::stop("There are no 1s in this row, but there must be one 1 in each row.\n");

  return result;
}

//' Fitness Function in C++
//'
//' A generated action vector and state matrix are input and this function
//' returns a numeric vector of the same length as the \code{outcome}.
//' \code{evolve_model} then computes a fitness score for that potential
//' solution FSM by comparing it to the provided \code{outcome}. This is
//' repeated for every FSM in the population and then the probability of
//' selection for the next generation is set to be proportional to the fitness
//' scores. This function is also used in the predict method for the resulting
//' final model that is returned. The function aborts if the user aborts in R,
//' checking every 1000 iterations.
//'
//' @param action_vec Integer Vector.
//' @param state_mat Integer Matrix.
//' @param covariates Integer Matrix.
//' @param period Integer Vector.
//'
//' @export
//' @useDynLib datafsm, .registration = TRUE
//' @importFrom Rcpp sourceCpp
// [[Rcpp::export]]

IntegerVector fitnessCPP(const IntegerVector action_vec, const IntegerMatrix state_mat,
                         const IntegerMatrix covariates, const IntegerVector period){

  int n = covariates.nrow();
  if (period.length() != n)
    Rcpp::stop("Mismatch: nrow(covariates) = %d, length(period) = %d", n, period.length());

  int state = 1;
  int max_state = state_mat.nrow();
  if (action_vec.length() < max_state)
    Rcpp::stop("Action vector has length %d, but there are %d states\n",
               action_vec.length(), max_state);
  IntegerVector decision(n);

  for(int i = 0; i < n; ++i){
    int old_state = state;
    if (period[i] > 1){
      const IntegerVector these_covariates = covariates(i,_);

      LogicalVector legal_covariates;
      legal_covariates = (these_covariates <= 1) & (these_covariates >= 0);

      if (is_false(all(legal_covariates))) {
        Rprintf("Looking up history[%d], period %d: (", i, period[i]);
        for (IntegerVector::const_iterator it = these_covariates.begin();
             it != these_covariates.end(); ++it) {
          if (it > these_covariates.begin()) {
            Rprintf(", ");
          }
          Rprintf("%d", *it);
        }
        Rprintf(")\n");
      }

      int history = predictor_lookup(these_covariates);

      if (history >= covariates.ncol() || history < 0)
        Rcpp::stop("Illegal value for history: %d (max legal value is %d).\n", history, covariates.ncol());
      // Not using R indexing convention for history anymore, just for state

      state = state_mat((state-1), history); // action_vec and state_mat were made for R where indexing starts at 1,
      if (state > max_state || state < 1) {
        Rprintf("Illegal state[%d](%d,%d) = %d in period %d. max_state = %d\n", i, old_state, history,
                state, period[i], max_state);
        Rcpp::stop("Illegal state matrix");
      }
      // not 0 like in Cpp

      decision[i] = action_vec[state-1];

      // if (i >= 6 && i <= 9)
      // {
      //   Rprintf("i = %2d, period = %2d, state = %2d, old_state = %2d, covariates = (",
      //           i, period[i], state, old_state);
      //   for(IntegerVector::const_iterator it = these_covariates.begin();
      //       it != these_covariates.end(); ++it) {
      //     if (it != these_covariates.begin())
      //       Rprintf(", ");
      //     Rprintf("%d", *it);
      //   }
      //   Rprintf("), history = %2d, decision = %2d\n", history, decision[i]);
      // }

    } else {
      decision[i] = action_vec[0];
      state = 1;
    }

    // abort the cpp func if the user aborts in R, checking every 1000 iterations
    if (i % 1000 == 0)
      Rcpp::checkUserInterrupt();
  }

  return decision;
}
