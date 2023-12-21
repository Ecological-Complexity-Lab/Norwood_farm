#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;


// calculate species' marginal persistence probabilities
// Input:
// - A: adjacency matrix of the food web
// - Pb: vector of baseline extinction probabilities
// - alpha: first parameter of the Beta distribution
// - beta: second parameter of the Beta distribution
// - nreps: number of iterations for Monte Carlo sim of Bayesian network
// Output:
// - vector of marginal persistence probabilities
// [[Rcpp::export]]
NumericVector marginal_persistence_probs(NumericMatrix A, NumericVector Pb,
                                         double alpha, double beta, int nreps) {
  int S = Pb.size(), ind_basal = 0, i, j, rep;
  double A_times_extant, frac, p_ext;
  NumericVector rowsums(S), marginal(S), extant(S), rand(S);
  for (i = 0; i < S; i++) {
    rowsums(i) = sum(A(i,_));
    if (rowsums(i) == 0.0) ind_basal++;
  }
  for (rep = 0; rep < nreps; rep++) {
    rand = Rcpp::runif(S);
    for (i = 0; i < ind_basal; i++) extant(i) = (rand(i) < Pb(i) ? 0.0 : 1.0);
    for (i = ind_basal; i < S; i++) {
      A_times_extant = 0.0;
      for (j = 0; j < S; j++) A_times_extant += A(i,j) * extant(j);
      frac = 1.0 - A_times_extant / rowsums(i);
      p_ext = Pb(i) + (1 - Pb(i)) * R::pbeta(frac, alpha, beta, true, false);
      extant(i) = (rand(i) < p_ext ? 0.0 : 1.0);
    }
    marginal += extant;
  }
  return(marginal/nreps);
}
