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

#' Returns the last non-NA entry
#'
#' Finds the last value in a vector which is not \code{NA} and
#' returns it. If all entries are \code{NA}, it returns an \code{NA}
#' @param d Vector with numeric entries and possibly \code{NA} values.
#' @return Last value in \code{d} with non-NA entry.
#' @examples
#' last_non_NA(c(1, 2, 3, 4, NA))
#'
#' values <- 1:100
#' values[sample(1:100, 10)] <- NA
#' df <- cbind(Year = c(rep(2000, 5), rep(2001, 5)),
#'             as.data.frame(matrix(values, nrow = 10)))
#'
#' library(dplyr)
#' df %>%
#'   group_by(Year) %>%
#'   summarize_each(funs(last_non_NA)) %>%
#'   ungroup()
#' @export
last_non_NA <- function(d) {
  if (all(is.na(d))) {
    return(NA)
  } else {
    return(d[max(which(!is.na(d)))])
  }
}
