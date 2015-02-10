#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector fitnessC(IntegerVector action_vec, IntegerMatrix state_mat, IntegerMatrix x){
                                       
        int n = x.nrow(); 
        
        IntegerVector period = x(_, 0);
        IntegerMatrix covariates = x(_, Range(1, x.ncol()-1));
        
        IntegerMatrix decision_mat(2,2);
        decision_mat(0,0) = 0; // c,c
        decision_mat(1,0) = 1; // d,c
        decision_mat(0,1) = 2; // c,d
        decision_mat(1,1) = 3; // d,d
        
        int state = 1;
        IntegerVector decision(n);
        
        for(int i = 0; i < n; i++){              
                if (period[i] > 1){
                        int my_decision1 = covariates(i,0); // 1 coop; 2 defect
                        int other_decision1 = covariates(i,1) ; // 1 coop; 2 defect
                        int history = decision_mat((my_decision1-1), (other_decision1-1)); // row/col indices into decision_mat
                        state = state_mat((state-1), history); // action_vec and state_mat were made for R where indexing starts at 1, not 0 like in Cpp
                        
                        decision[i] = action_vec[(state-1)];
                } else { 
                        decision[i] = action_vec[0];
                        state = 1;
                }
                // TODO: abort the cpp func if the user aborts in R: Rcpp::checkUserInterrupt();
        } // or if saving fitness results, could do: fitness_count[i] = decision == outcome[i] ? 1 : 0; 
        
        return decision;
}