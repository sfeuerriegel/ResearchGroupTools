#' Pretty summary statistics
#'
#' Calculates summary statistics for dataset and display key variables in a nice format.
#' @param x Dataset in the form of a matrix or a \code{data.frame}.
#' @param digits Number of digits to be printed (default: 3).
#' @param subset Variables to be selected.
#' @return Matrix with pretty output.
#' @note Class \code{RGT_descriptives} is added to recognize this value inside
#' \code{\link{export}}.
#' @examples
#' data(USArrests)
#' descriptiveStatistics(USArrests)
#' @export
descriptiveStatistics <- function(x, digits = 3,
                                  subset = c("mean", "median", "min", "max", "sd", "skew", "kurtosis")) {
  m <- psych::describeBy(x, group = rep("", nrow(x)), mat = TRUE, digits = digits)[, subset]

  # fix the concatenated "1" for the grouping in describeBy
  rownames(m) <- colnames(x)

  attr(m, "digits") <- digits
  class(m) <- c(class(m), "RGT_descriptives")

  return(m)
}

