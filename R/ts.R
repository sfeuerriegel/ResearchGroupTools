# TODO: monthly differences

#' Lagged returns
#'
#' Calculates returns between values with specified lag.
#' @param x A numeric vector containing the values for which returns are calculated.
#' @param lag Integer indicating which lag to use (default: 1).
#' @param na_padding Flag whether to pad truncated values with NA's (default: TRUE).
#' @return Numeric vector with returns of specified lag.
#' @note Parameter \code{na_padding} behaves slightly different from \code{\link{diff}}
#' in order to achieve consistency with \code{\link[dplyr]{lead}} and \code{\link[dplyr]{lag}}.
#' @examples
#' returns(1:10)
#' returns(c(1, 2, 4, 8, 16, 32))
#'
#' returns(c(1, 2, 4, 8, 16, 32), lag = 2)
#' @seealso \code{\link{diff}}, \code{\link{logReturns}}
#' @export
returns <- function(x, lag = 1, na_padding = TRUE) {
  r <- (x[-(1:lag)] - x[-seq(length(x), by = -1, length.out = lag)] ) / x[-seq(length(x), by = -1, length.out = lag)]

  if (na_padding) {
    r <- c(rep(NA, lag), r)
  }

  return(r)
}

#' Lagged log-returns
#'
#' Calculates log-returns between values with specified lag.
#' @param x A numeric vector containing the values for which returns are calculated.
#' @param lag Integer indicating which lag to use (default: 1).
#' @param na_padding Flag whether to pad truncated values with NA's (default: TRUE).
#' @param base A positive number giving the base with respect to which the logarithm is
#' computed. Default is e, i.e. \code{exp(1)}.
#' @return Numeric vector with log-returns of specified lag.
#' @note Parameter \code{na_padding} behaves slightly different from \code{\link{diff}}
#' in order to achieve consistency with \code{\link[dplyr]{lead}} and \code{\link[dplyr]{lag}}.
#' @examples
#' logReturns(1:10)
#' logReturns(c(1, 2, 4, 8, 16, 32), base = 2)
#' logReturns(c(1, 2, 4, 8, 16, 32), base = 2, na_padding = FALSE)
#' @seealso \code{\link{diff}}, \code{\link{returns}}
#' @importFrom stats na.omit
#' @export
logReturns <- function(x, lag = 1, na_padding = TRUE, base = exp(1)) {
  r <- returns(x, lag = lag, na_padding = na_padding)

  if (any(na.omit(1 + r) <= 0)) {
    stop("Log-returns not defined if any value in (1 + returns) is <= 0.")
  }

  log(1 + r, base = base)
}

#' Lagged differences
#'
#' Calculates differences between values with specified lag.
#' @param x A numeric vector containing the values for which returns are calculated.
#' @param lag Integer indicating which lag to use (default: 1).
#' @param order Order of differences (default: 1).
#' @param na_padding Flag whether to pad truncated values with NA's (default: TRUE).
#' @return Numeric vector with differences of specified lag and order.
#' @note Parameter \code{na_padding} behaves slightly different from \code{\link{diff}}
#' in order to achieve consistency with \code{\link[dplyr]{lead}} and \code{\link[dplyr]{lag}}.
#' @examples
#' differences(1:10)
#' differences(c(1, 2, 4, 8, 16, 32))
#' differences(c(1, 2, 4, 8, 16, 32), order = 2)
#' differences(c(1, 2, 4, 8, 16, 32), na_padding = FALSE)
#' @seealso \code{\link{diff}}, \code{\link{returns}}
#' @export
differences <- function(x, lag = 1, order = 1, na_padding = TRUE) {
  d <- diff(x, lag = lag, differences = order)

  if (na_padding) {
    d <- c(rep(NA, length(x) - length(d)), d)
  }

  return(d)
}

#' Augmented Dickey-Fuller (ADF) test
#'
#' Performs the Augmented Dickey-Fuller (ADF) test to check if a time
#' series is stationary. The result comes in a pretty format.
#' @param d Data frame containing the time series in column-wise format.
#' @param vars Column names to check for stationarity. If not specified,
#' then all columns are tested.
#' @param type Test type, either \code{"none"}, \code{"drift"} or
#' \code{"trend"}. See \code{\link[urca]{ur.df}} for details.
#' @param lags Number of lags for endogenous variable to be included.
#' Default is 1. See \code{\link[urca]{ur.df}} for details.
#' @param filename Optional filename to export the table as LaTeX.
#' Default is \code{NULL}, i.e. no export.
#' @param digits Number of digits to be printed (default: 3).
#' @param verbose Flag if the result of each ADF test is printed.
#' Default is yes (\code{TRUE}).
#' @param ... Further parameters passed on to \code{\link[urca]{ur.df}}.
#' @return P-values from ADF test in a simple table. If P-values are
#' < 0.05, this is an indication of stationarity.
#' @examples
#' data(USArrests)
#' adf(USArrests)
#' adf(USArrests, vars = c("Murder", "Rape"), type = "drift",
#'     filename = "adf.tex", verbose = FALSE)
#' unlink("adf.tex")
#' @importFrom stats pnorm
#' @export
adf <- function(d, vars = colnames(d),
                type = c("none", "drift", "trend"), lags = 1,
                filename = NULL, digits = 3, verbose = TRUE, ...) {
  result <- data.frame(matrix(0, nrow = length(vars), ncol = 8))
  result[, 1] <- vars
  result[, 2] <- type[1]
  colnames(result) <- c("Variable", "Type", "Lags",
                        "TestStat", "CriticalValue1", "CriticalValue5", "CriticalValue10",
                        "Pvalue")

  for (i in 1:length(vars)) {
    cat(paste0(i, "\n"))

    if (inherits(d, "tbl_df")) {
      ts <- as.numeric(pull_string(d, vars[i]))
    } else {
      ts <- as.numeric(d[, i])
    }

    tmp <- urca::ur.df(ts, type = type[1], ...)
    result[i, 3:8] <- c(tmp@lags, tmp@teststat[1], tmp@cval[1, ], pnorm(tmp@teststat[1])*2)
    if (verbose) {
      print(summary(tmp))
    }
  }

  if (!is.null(filename)) {
    cat("Column names: ", paste0(colnames(result), collapse = " & "))
    print(xtable::xtable(result[, 1:7], digits = digits),
          only.contents = TRUE, include.colnames = FALSE, booktabs = TRUE,
          file = filename, type = "latex")
  }

  # P-value close to 0: stationarity
  if (all(result$Pvalue < 0.05)) {
    cat("All time series appear stationary, since all P-values < 0.05.\n")
  } else {
    cat("The following time series appear stationary, as P-values > 0.05: ",
        paste(vars[result$Pvalue >= 0.05], collapse = ", "), "\n")
  }

  return(result)
}

#' Cointegration test
#'
#' Conducts a cointegration test based on the Johansen procedure for
#' multivariate time series.
#' @param d Data frame containing the time series in column-wise format.
#' @param vars Column names to check for stationarity. If not specified,
#' then all columns are tested.
#' @param type The test to be conducted, either \code{"eigen"} or
#'  \code{"trace"}. See \code{\link[urca]{ur.df}} for details.
#' @param K The lag order of the series (levels) in the VAR.
#' @param filename Filename to export the table as LaTeX.
#' @param digits Number of digits to be printed (default: 3).
#' @param ... Further parameters passed on to \code{\link[urca]{ca.jo}}.
#' @return Table with result from cointegration test.
#' @examples
#' data(USArrests)
#' cointegrationTable(USArrests, vars = c("Murder", "Rape"), K = 2)
#' unlink("cointegration_eigen.tex")
#' @export
cointegrationTable <- function(d, vars = colnames(d),
                               type = c("eigen", "trace"), K = NULL,
                               filename = paste0("cointegration", type[1], ".tex"),
                               digits = 3, ...) {
  if (is.null(K)) {
    stop("Lag number K needs to be estimated via information criterion and passed on to this function.")
  }

  coint <- urca::summary(urca::ca.jo(d[, vars], type = type[1], K = K, ...))

  result <- cbind(rownames(coint@cval),
                  round(coint@teststat, digits),
                  coint@cval)
  colnames(result) <- c("H0", "TestStatistic", "CriticalValue10", "CriticalValue5", "CriticalValue1")

  # TODO: simple ca.jo interpretation

  cat("Column names: ", paste(colnames(result), collapse = " & "))
  print(xtable::xtable(result, digits = digits),
        only.contents = TRUE, include.colnames = FALSE, booktabs = TRUE,
        file = filename, type = "latex")

  return(as.data.frame(result))
}

#' Pretty plot of impulse response function
#'
#' Plots the impulse response function in black/white manner for
#' suitability with common journals.
#' @param irf Object of type \code{\link[vars]{varirf}} with impulse response function.
#' @param ylab ylab
#' @export
plotImpulseResponseFunction <- function(irf, ylab) {

}

