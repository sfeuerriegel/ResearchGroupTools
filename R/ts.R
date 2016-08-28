# TODO: monthly differences

#' Lagged returns
#'
#' Calculates returns between values with specified lag.
#' @param x A numeric vector containing the values for which returns are calculated.
#' @param lag Integer indicating which lag to use (default: 1).
#' @param na_padding Flag whether to pad truncated values with NA's (default: TRUE).
#' @return Numeric vector with returns of specified lag.
#' @note Parameter \code{na_padding} behaves slightly different from \code{\link{diff}} in order to achieve consistency with \code{\link[dplyr]{lead}} and \code{\link[dplyr]{lag}}.
#' @examples
#' returns(1:10)
#' returns(c(1, 2, 4, 8, 16, 32))
#'
#' returns(c(1, 2, 4, 8, 16, 32), lag = 2)
#' @seealso \code{\link{diff}}
#' @export
returns <- function(x, lag = 1, na_padding = TRUE) {
  r <- (x[-seq(length(x), by = -1, length.out = lag)] - x[-(1:lag)]) / x[-seq(length(x), by = -1, length.out = lag)]

  if (na_padding) {
    r <- c(rep(NA, lag), r)
  }

  return(r)
}
