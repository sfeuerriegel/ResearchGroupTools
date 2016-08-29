#' Names of columns with \code{NA} values
#'
#' Identifies columns that contain an \code{NA} value
#' @param d A matrix or data frame.
#' @return Vector with column identifiers
#' @examples
#' m <- matrix(letters[c(1, 2, NA, 3, NA, 4, 5, 6, 7, 8)],
#'             ncol = 2, byrow = FALSE)
#' colnames(m) <- c("x", "y")
#' m
#' findColsNA(m)
#' showColsNA(m)
#'
#' d <- data.frame(x = 1:10)
#' d
#' findColsNA(d)
#' showColsNA(d)
#' @seealso \code{\link{showColsNA}}
#' @export
findColsNA <- function(d) {
  idx <- apply(d, 2, function(x) any(is.na(x)))
  return(colnames(d)[which(idx)])
}

#' Show columns with \code{NA} values
#'
#' Show columns that contain an \code{NA} value
#' @param d A matrix or dataframe.
#' @return Subset of \code{d} containing \code{NA} values. Returns \code{NULL} if no \code{NA} values
#' are present.
#' @examples
#' m <- matrix(letters[c(1, 2, NA, 3, NA, 4, 5, 6, 7, 8)],
#'             ncol = 2, byrow = FALSE)
#' colnames(m) <- c("x", "y")
#' m
#' findColsNA(m)
#' showColsNA(m)
#'
#' d <- data.frame(x = 1:10)
#' d
#' findColsNA(d)
#' showColsNA(d)
#' @seealso \code{\link{findColsNA}}
#' @export
showColsNA <- function(d) {
  d <- d[, findColsNA(d)]

  if ((is.vector(d) && length(d) == 0) ||
      (is.matrix(d) && ncol(d) == 0) ||
      (is.data.frame(d) && ncol(d) == 0)) {
    return(NULL)
  }

  return(d)
}

#' Indices of rows with \code{NA} values
#'
#' Identifies rows that contain an \code{NA} value
#' @param d A matrix or data frame.
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

#' Show rows with \code{NA} values
#'
#' Show rows that contain an \code{NA} value
#' @param d A matrix or dataframe.
#' @return Subset of \code{d} containing \code{NA} values. Returns \code{NULL} if no \code{NA} values
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

  if (is.null(dim(d)) || nrow(d) == 0) {
    return(NULL)
  }

  return(d)
}
