#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix rcpp_divide_by_rowsum(NumericMatrix X) {
  int n = X.nrow(), p = X.ncol();
  NumericMatrix result(n, p);

  for (int i = 0; i < n; i++) {
    double row_sum = 0;
    for (int j = 0; j < p; j++) {
      row_sum += X(i, j);
    }
    for (int j = 0; j < p; j++) {
      result(i, j) = X(i, j) / row_sum;
    }
  }
  return result;
}

