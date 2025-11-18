#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector fooCpp(NumericVector x) {
  return x * 2;
}



