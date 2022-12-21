#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector rcpp_rowProd(NumericMatrix X, double prior) {
  int n = X.nrow();
  int p = X.ncol();
  NumericVector result(n);

  for (int i = 0; i < n; i++) {
    result[i] = prior;
    for (int j = 0; j < p; j++) {
      result[i] *= X(i, j);
    }
  }

  return result;
}

