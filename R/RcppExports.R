# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#' Cumulativate versions of skewness, kurtosis, sd, adv
#'
#' Functions include \code{cumskewness}, \code{cumkurtosis},
#' \code{cumadev} (average deviation), and \code{cumsd} to complete
#' R's set of cumulate functions to match the aggregation functions available
#' in most databases.
#' @param x For \code{cumskewness}, \code{cumkurtosis}, \code{cumsd},
#' \code{cumadv} an integer or numeric vector.
#' @return Returns a vector of the same length with new elements.
#' @details The most common variants \code{\link{cummin}}, \code{\link{cummax}}
#' and \code{\link{cumsum}} are included in the \code{"base"} package. In
#' addition, \code{"dplyr"} ships a \code{\link[dplyr]{cummean}}.
#' @examples
#' library(dplyr)
#'
#' df <- data.frame(x = 1:10, y = rnorm(10))
#' cumsd(df$x)
#' df %>%
#'   mutate_all(funs("mean" = cummean, "sd" = cumsd))
#' @export
cumsd <- function(x) {
    .Call('ResearchGroupTools_cumsd', PACKAGE = 'ResearchGroupTools', x)
}

#' @export
#' @rdname cumsd
cumskewness <- function(x) {
    .Call('ResearchGroupTools_cumskewness', PACKAGE = 'ResearchGroupTools', x)
}

#' @export
#' @rdname cumsd
cumkurtosis <- function(x) {
    .Call('ResearchGroupTools_cumkurtosis', PACKAGE = 'ResearchGroupTools', x)
}

#' @export
#' @rdname cumsd
cumadev <- function(x) {
    .Call('ResearchGroupTools_cumadev', PACKAGE = 'ResearchGroupTools', x)
}

