#' Indices of rows with NA values
#'
#' Identifies rows that contain an \code{NA} value
#' @param d A matrix or dataframe.
#' @return Vector with indices
#' @examples
#' m <- matrix(letters[c(1, 2, NA, 3, NA, 4, 5, 6, 7, 8)],
#'             ncol = 2, byrow = FALSE)
#' m
#' findRowsNA(m)
#' showRowsNA(m)
#'
#' d <- data.frame(x = 1:10)
#' d
#' findRowsNA(d)
#' showRowsNA(d)
#' @seealso \code{\link{showRowsNA}}
#' @export
findRowsNA <- function(d) {
  idx <- apply(d, 1, function(x) any(is.na(x)))
  return(which(idx))
}

#' Show rows with NA values
#'
#' Show rows that contain an \code{NA} value
#' @param d A matrix or dataframe.
#' @return Subset of \code{d} containing NA values. Returns \code{NULL} if no NA values
#' are present.
#' @examples
#' m <- matrix(letters[c(1, 2, NA, 3, NA, 4, 5, 6, 7, 8)],
#'             ncol = 2, byrow = FALSE)
#' m
#' findRowsNA(m)
#' showRowsNA(m)
#'
#' d <- data.frame(x = 1:10)
#' d
#' findRowsNA(d)
#' showRowsNA(d)
#' @seealso \code{\link{findRowsNA}}
#' @export
showRowsNA <- function(d) {
  d <- d[findRowsNA(d), ]

  if (is.null(dim(d))) {
    return(NULL)
  }

  return(d)
}
