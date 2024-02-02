#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List SimpLinCpp(NumericVector X, NumericVector Y) {
  int n = X.size();
  double mX = mean(X);
  double mY = mean(Y);
  double sXY = 0;
  double sXX = 0;
  for (int i = 0; i < n; i++) {
    sXY += (X[i] - mX) * (Y[i] - mY);
    sXX += (X[i] - mX) * (X[i] - mX);
  }

  double beta1 = sXY / sXX;
  double beta0 = mY - beta1 * mX;
  NumericVector residual(n);
  NumericVector prediction(n);

  double sse = 0;
  for (int i = 0; i < n; i++) {
    prediction[i] = beta1 * X[i] + beta0;
    residual[i] = (Y[i] - prediction[i])*(Y[i] - prediction[i]);
    sse += residual[i];
  }
  double mse = sse / (n-2);
  double se1 = sqrt(mse/sXX);
  double se0 = sqrt(mse*(1/n + (mX*mX)/sXX));
  double beta1low = beta1 - 1.96*se1;
  double beta1high = beta1 + 1.96*se1;
  double beta0low = beta0 - 1.96*se0;
  double beta0high = beta0 + 1.96*se0;

  NumericVector beta = NumericVector::create(_["beta0"] = beta0, _["beta1"] = beta1);
  NumericVector se = NumericVector::create(_["se1"] = se1, _["se0"] = se0);
  NumericVector CIbeta1 = NumericVector::create(_["beta1low"] = beta1low, _["beta1high"] = beta1high);
  NumericVector CIbeta0 = NumericVector::create(_["beta0low"] = beta0low, _["beta0high"] = beta0high);
  List result = List::create(Named("beta") = beta , _["se"] = se,
                             _["CIbeta1"] = CIbeta1,_["CIbeta0"] = CIbeta0,_["MSE"] = mse,
                             _["residual"] = residual, _["prediction"] = prediction);
  return result;
}



