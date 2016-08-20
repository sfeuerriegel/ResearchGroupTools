#' Downward rounding of numbers
#'
#' Takes a single numeric value or a vector and returns the smallest integers not less
#' than the corresponding elements.
#' @param x A numeric vector.
#' @return A numeric vector with integers after rounding downwards to the next integer.
#' @details A simple wrapper to \code{ceiling} for a more consistent naming convention
#' with other programming languages.
#' @examples
#' ceil(3.14)
#' ceil(c(-3.2, 4, 0, 7.6, 7.1))
#' @export
ceil <- function(x) {
  ceiling(x)
}
