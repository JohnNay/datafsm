#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
int predictor_lookup(IntegerVector x){
  int result;
  int num_ones = 0;
  int n = x.length();

  for(int i = 0; i < n; i++){
          if (x[i] > 1 || x[i] < 0){
                  Rprintf("Illegal value for: %d.\n", x[i]);
          }
          if (x[i]==1){
                  result = i; // Not using R indexing convention here
                  num_ones = num_ones + 1;
          }
  }
  if (num_ones > 1)
    Rcpp::stop("There are %d 1s in this row, but there can only be one 1 in each row.\n", num_ones);

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
//' final model that is returned. The function aborts the user aborts in R,
//' checking every 1000 iterations.
//' 
//' @param action_vec Integer Vector.
//' @param state_mat Integer Matrix.
//' @param covariates Integer Matrix.
//' @param period Integer Vector.
//'   
//' @export
//' @useDynLib datafsm
//' @importFrom Rcpp sourceCpp
// [[Rcpp::export]]

IntegerVector fitnessCPP(IntegerVector action_vec, IntegerMatrix state_mat, IntegerMatrix covariates, IntegerVector period){
  
  int n = covariates.nrow();
  
  int state = 1;
  int max_state = state_mat.nrow();
  if (action_vec.length() < max_state) Rprintf("Action vector has length %d, but there are %d states\n", action_vec.length(), max_state);
  IntegerVector decision(n);
  
  for(int i = 0; i < n; i++){
    int old_state;
    if (period[i] > 1){
      IntegerVector these_covariates = covariates(i,_);
      
      if (these_covariates[0] > 1 || these_covariates[0] < 0 ||
          these_covariates[1] > 1 || these_covariates[1] < 0 ||
          these_covariates[2] > 1 || these_covariates[2] < 0) {
        Rprintf("Looking up history[%d], period %d: (%d, %d, %d)\n", i,
                these_covariates[0], these_covariates[1], these_covariates[2], these_covariates[3]);
      }
      
      int history = predictor_lookup(these_covariates);
      
      if (history > covariates.ncol() || history < 0) 
        Rcpp::stop("Illegal value for history: %d.\n", history);
      // Not using R indexing convention for history anymore, just for state
      
      old_state = state;
      state = state_mat((state-1), history); // action_vec and state_mat were made for R where indexing starts at 1,
      if (state > max_state) {
        Rprintf("Illegal state[%d](%d,%d) = %d in period %d. max_state = %d\n", i, old_state, history,
                state, period[i], max_state);
        Rcpp::stop("Illegal state matrix");
      }
      // not 0 like in Cpp
      
      decision[i] = action_vec[(state-1)];
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
