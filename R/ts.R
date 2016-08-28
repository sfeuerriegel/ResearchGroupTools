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
