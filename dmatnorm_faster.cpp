#define ARMA_DONT_PRINT_ERRORS
#include <RcppArmadillo.h>
#include <ctime>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
#include <omp.h>
#include <vector>

// [[Rcpp::export]]
arma::colvec dmatnorm_calc(arma::cube& x, arma::mat& mean, arma::mat& U_inv,
                           arma::mat& V_inv, int p, int r, int N, double logdetU, double logdetV) {
  arma::colvec logresult(N);
  #pragma omp parallel for
  for (int i = 0; i < N; i++) {
    arma::mat XM = x.slice(i) - mean;
    logresult(i) = -0.5 * p * r * log(2 * M_PI) - 0.5 * r * logdetU -
                   0.5 * p * logdetV -
                   0.5 * trace(V_inv * trans(XM) * U_inv * XM);
  }
  return logresult;
}
