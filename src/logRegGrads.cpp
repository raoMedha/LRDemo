#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List logRegBinGradCpp(const NumericMatrix X,
                      const NumericVector y,
                      const NumericVector beta) {
  int n = X.nrow();
  int p = X.ncol();

  NumericVector eta(n);
  NumericVector pHat(n);

  // eta = X * beta, pHat = logistic(eta)
  for (int i = 0; i < n; i++) {
    double sum = 0.0;
    for (int j = 0; j < p; j++) {
      sum += X(i, j) * beta[j];
    }
    eta[i] = sum;
    pHat[i] = 1.0 / (1.0 + std::exp(-sum));
  }

  // grad = X' * (pHat - y) / n
  NumericVector grad(p);
  for (int j = 0; j < p; j++) {
    double g = 0.0;
    for (int i = 0; i < n; i++) {
      g += X(i, j) * (pHat[i] - y[i]);
    }
    grad[j] = g / n;
  }

  return List::create(
    _["eta"] = eta,
    _["pHat"] = pHat,
    _["grad"] = grad
  );
}

// [[Rcpp::export]]
List logRegOvrCpp(const NumericMatrix X,
                  const IntegerVector y,
                  const NumericMatrix betaMat) {
  // One v rest gradients for K classes, trained in parallel
  // y's class labels coded 0,...,K-1
  int n = X.nrow();
  int p = X.ncol();
  int K = betaMat.ncol();

  // Probabilities per class
  NumericMatrix P(n, K);
  NumericMatrix gradMat(p, K);

  for (int k = 0; k < K; k++) {
    // compute eta and pHat for class k (vs rest)
    NumericVector etaK(n);
    NumericVector pHatK(n);

    for (int i = 0; i < n; i++) {
      double sum = 0.0;
      for (int j = 0; j < p; j++) {
        sum += X(i, j) * betaMat(j, k);
      }
      etaK[i] = sum;
      pHatK[i] = 1.0 / (1.0 + std::exp(-sum));
      P(i, k) = pHatK[i];
    }

    // yBin = 1 if y == k else 0
    // gradK = X' * (pHatK - yBin) / n
    NumericVector gradK(p);
    for (int j = 0; j < p; j++) {
      double g = 0.0;
      for (int i = 0; i < n; i++) {
        double yBin = (y[i] == k) ? 1.0 : 0.0;
        g += X(i, j) * (pHatK[i] - yBin);
      }
      gradK[j] = g / n;
      gradMat(j, k) = gradK[j];
    }
  }

  return List::create(
    _["P"] = P,
    _["grad"] = gradMat
  );
}
