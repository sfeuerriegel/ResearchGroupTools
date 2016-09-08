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
  result[, 2] <- plyr::mapvalues(type[1], c("drift", "trend"), c("Constant", "Trend"))
  colnames(result) <- c("Variable", "Type", "Lags",
                        "TestStat", "CriticalValue10", "CriticalValue5", "CriticalValue1",
                        "Pvalue")

  for (i in 1:length(vars)) {
    if (inherits(d, "tbl_df")) {
      ts <- as.numeric(pull_string(d, vars[i]))
    } else {
      ts <- as.numeric(d[, i])
    }

    tmp <- urca::ur.df(ts, type = type[1], ...)
    result[i, 3:8] <- c(tmp@lags, tmp@teststat[1], tmp@cval[1, 3:1], pnorm(tmp@teststat[1])*2)
    if (verbose) {
      cat(paste0(i, "\n"))
      print(summary(tmp))
    }
  }

  if (!is.null(filename)) {
    cat("\n\n")
    cat("\\begin{tabular}{ll SSSSS} \n")
    cat("\\toprule \n")
    cat("\\multicolumn{1}{l}{Variable} & \\multicolumn{1}{l}{Deterministic trend} & \\multicolumn{1}{c}{Lags}& \\multicolumn{1}{c}{Test value} & \\multicolumn{3}{c}{\\textbf{Critical values}}\\\\ \n")
    cat("\\cline{5-7} \n")
    cat("&&&& $10\\,\\%$ & $5\\,\\%$ & $1\\,\\%$ \\\\ \n")
    cat("\n\n")

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
#' @param filename Filename to export the table as LaTeX. Default is
#' \code{NULL}, i.e. no export.
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
                               filename = NULL,
                               digits = 3, ...) {
  if (is.null(K)) {
    stop("Lag number K needs to be estimated via information criterion and passed on to this function.")
  }

  coint <- urca::summary(urca::ca.jo(d[, vars], type = type[1], K = K, ...))

  result <- data.frame(cbind(c(paste0("r<=", (coint@P-1):1), "r=0"),
                             round(coint@teststat, digits),
                             coint@cval),
                       stringsAsFactors = FALSE)
  colnames(result) <- c("H0", "TestStatistic", "CriticalValue10", "CriticalValue5", "CriticalValue1")

  result <- result[nrow(result):1, ]
  rownames(result) <- NULL

  if (all(result$TestStatistic < result$CriticalValue1)) {
    cat("All test statistics are smaller than the 1% critical values: time series are not cointegrated.", "\n")
  } else {
    # any(result$TestStatistic > result$CriticalValue1 ))

    if (result$TestStatistic[1] > result$CriticalValue1[1]) {
      cat("Test statistic in the top row is larger than the 1% values:  All time-series variables are stationary, i.e. I(0), to start with. Cointegration is not relevant here.", "\n")
    } else {
      i <- min(which(result$TestStatistic > result$CriticalValue1)) - 1
      cat("First order r where test statistic is larger than the 1% critical value is", rownames(result)[i], ": Integrated of order", i, "\n")
    }
  }

  if (!is.null(filename))
  {
    cat("\n\n")
    cat("\\begin{tabular}{l SSSS} \n")
    cat("\\toprule \n")
    cat("\\textbf{$H_{0}$} \n")
    cat("& \\textbf{Test statistic} & \\multicolumn{3}{c}{\\textbf{Critical Values}}\\\\ \n")
    cat("\\crule{3-5} \n")
    cat("& {$n = ", coint@P, "$}& {$10\\,\\%$} & {$5\\,\\%$} & {$1\\,\\%$} \\\\ \n", sep = "")
    cat("\n\n")

    result_file <- cbind(c("$r = 0$", paste0("$r \\leq ", 1:(coint@P-1), "$")),
                         result[, -1])

    print(xtable::xtable(result_file, digits = digits),
          only.contents = TRUE, include.colnames = FALSE, booktabs = TRUE,
          file = filename, type = "latex",
          sanitize.text.function = identity,
          include.rownames = FALSE,)
  }

  return(result)
}

#' Pretty plot of impulse response function
#'
#' Plots the impulse response function in black/white manner for
#' suitability with common journals.
#' @param irf Object package with impulse response function, i.e. type
#' \code{varirf} from the \code{vars}
#' @param name Name of variable of interest.
#' @param ylab Text on y-axis. By default, this argument is \code{NULL} and the
#' variable name is taken automatically instead.
#' @param alpha Opacity for confidence interval. Default is 0.3
#' @param n.ahead Optional parameter to later choose a smaller x-range for
#' plotting. Argument expects a numeric value with the maximum step.
#' @return Object of \code{\link[ggplot2]{ggplot}}.
#' @examples
#' library(vars)
#' data(Canada)
#' # For VAR
#' var.2c <- VAR(Canada, p = 2, type = "const")
#' \dontrun{
#' irf <- irf(var.2c, impulse = "e", response = "prod", boot = TRUE)
#' plotIrf(irf, ylab = "Production")
#' }
#' @export
plotIrf <- function(irf, name = NULL, ylab = NULL, alpha = 0.3, n.ahead = NULL) {
  if (!irf$boot) {
    stop("Plot requires confidence intervals (call irf with argument boot).")
  }

  if (is.null(name)) {
    if (length(irf$impulse) == 1) {
      name <- irf$impulse
    } else {
      stop("More than one impulse stored, but no selection made via argument 'name'.")
    }
  } else if (!(name %in% irf$impulse)) {
    stop("Argument 'name' is not a valid impulse as it is not stored in 'irf' object.")
  }

  # to surpress warnings
  x <- NULL
  impulse <- NULL
  upper <- NULL
  lower <- NULL

  df <- data.frame(x = 1:length(irf$irf[[name]]),
                   impulse = irf$irf[[name]],
                   upper = irf$Upper[[name]],
                   lower = irf$Lower[[name]])
  colnames(df) <- c("x", "impulse", "upper", "lower")

  if (!is.null(n.ahead)) {
    df <- df[1:n.ahead, ]
  }

  p <- ggplot2::ggplot(df, ggplot2::aes(x = x)) +
    ggplot2::geom_line(ggplot2::aes(y = impulse)) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = 0)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), alpha = alpha) +
    ggplot2::theme_bw() +
    ggplot2::xlab("")

  if (is.null(ylab)) {
    p <- p + ggplot2::ylab(name)
  } else {
    p <- p + ggplot2::ylab(ylab)
  }

  return(p)
}

#' Calculation and pretty plot of impulse response function
#'
#' Computes and plots the impulse response function in black/white manner for
#' suitability with common journals.
#' @param var Object package with impulse response function, i.e. type
#' \code{varest} from the \code{vars}
#' @param impulse String identifier which variable experiences a shock.
#' @param response String identfier which variable is the response.
#' @param n.ahead Optional parameter to later choose a smaller x-range for the
#' impulse response function. Argument expects a numeric value with the maximum
#' step. Default is 10.
#' @param ... Further arguments passed on to \code{\link{plotIrf}}
#' @return Object of \code{\link[ggplot2]{ggplot}}.
#' @examples
#' library(vars)
#' data(Canada)
#' # For VAR
#' var.2c <- VAR(Canada, p = 2, type = "const")
#' \dontrun{
#' impulseResponsePlot(var.2c, impulse = "e", response = "prod", ylab = "Production")
#' }
#' @export
impulseResponsePlot <- function(var, impulse, response, n.ahead = 10, ...) {
  if (class(var) != "varest") {
    stop("Argument 'var' is not of type varest.")
  }

  if (!is.character(impulse) || length(impulse) > 1) {
    stop("Argument 'impulse' must be a single identifier as a string.")
  }
  if (!is.character(response) || length(response) > 1) {
    stop("Argument 'response' must be a single identifier as a string.")
  }

  irf <- vars::irf(var, impulse = impulse, response = response, boot = TRUE)
  plotIrf(irf, ...)
}

