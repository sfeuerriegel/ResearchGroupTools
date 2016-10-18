#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//


NumericVector moment(NumericVector x, int m) {
  int n = x.length();
  NumericVector out = no_init(n);

  double var = 0.0;
  double mnt = 0.0;
  double sum = x[0];
  double ave = 0.0;
  double sdev = 0.0;
  double s = 0.0;
  double p = 0.0;
  double ep = 0.0;

  out[0] = NA_REAL;

  for(int j = 1; j < n; j++) {
    var = 0.0;
    mnt = 0.0;
    ep = 0.0;
    sum += x[j];
    ave = sum / (j + 1.0);
    for (int i = 0; i <= j; i++) {
      s = x[i] - ave;
      ep += s;
      p = pow(s, 2);
      var += p;
      p = pow(s, m);
      mnt += p;
    }
    var = (var - ((ep*ep)/(j+1.0)))/j;
    if (var) {
      if (2 == m) {
        out[j] = sqrt(var);
      } else if (3 == m) {
        sdev = sqrt(var);
        out[j] = mnt/((j+1.0)*var*sdev);
      } else if (4 == m) {
        out[j] = mnt/((j+1.0)*var*var)-3.0;
      }
    } else {
      out[j] = NA_REAL;
    }
  }

  return out;

}

//' Cumulativate versions of skewness, kurtosis, sd, adv
//'
//' Functions include \code{cumskewness}, \code{cumkurtosis},
//' \code{cumadev}, and \code{cumsd} to complete
//' R's set of cumulate functions to match the aggregation functions available
//' in most databases.
//' @param x For \code{cumskewness}, \code{cumkurtosis}, \code{cumsd},
//' \code{cumadv} an integer or numeric vector.
//' @return Returns a vector of the same length with new elements.
//' @details The most common variants \code{\link{cummin}}, \code{\link{cummax}}
//' and \code{\link{cumsum}} are included in the \code{"base"} package. In
//' addition, \code{"dplyr"} ships a \code{\link[dplyr]{cummean}}.
//' @examples
//' library(dplyr)
//'
//' df <- data.frame(x = 1:10, y = rnorm(10))
//' cumsd(df$x)
//' df %>%
//'   mutate_all(funs("mean" = cummean, "sd" = cumsd))
//' @export
// [[Rcpp::export]]
NumericVector cumsd(NumericVector x) {
  return moment(x, 2);
}


//' @export
//' @rdname cumsd
// [[Rcpp::export]]
NumericVector cumskewness(NumericVector x) {
  return moment(x, 3);
}


//' @export
//' @rdname cumsd
// [[Rcpp::export]]
NumericVector cumkurtosis(NumericVector x) {
  return moment(x, 4);
}


//' @export
//' @rdname cumsd
// [[Rcpp::export]]
NumericVector cumadev(NumericVector x) {
  int n = x.length();
  NumericVector out = no_init(n);

  double sum = x[0];
  double ave = 0.0;
  double adev = 0.0;

  out[0] = 0.0;

  for(int j = 1; j < n; j++) {
    adev = 0.0;
    sum += x[j];
    ave = sum / (j + 1.0);
    for (int i = 0; i <= j; i++) {
      adev += abs(x[j] - ave);
    }
    out[j] = adev / (j + 1.0);
  }

  return out;
}



