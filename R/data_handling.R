#' Extracts a column from a tbl object
#'
#' @param object Object of type \code{\link[dplyr]{tbl}}
#' @param column Column identifier, name as string or integer depending on function.
#' @return Column as vector
#' @source Tommy O'Dell: \url{http://stackoverflow.com/questions/21618423/extract-a-dplyr-tbl-column-as-a-vector}
#' @examples
#' library(dplyr)
#'
#' d <- data_frame(x = 1:10,
#'                 y = rnorm(10))
#' d %>% pull(x)
#' d %>% pull("x")
#'
#' v <- "x"
#' d %>% pull_string(v)
#'
#' d %>% pull_ith(1)
#' @rdname pull
#' @export
pull <- function(object, column) {
  if (is.name(substitute(column))) {
    cols <- deparse(substitute(column))
  } else {
    cols <- column
  }

  return(object[, cols, drop = FALSE][[1]])
}

#' @rdname pull
#' @export
pull_string <- function(object, column) {
  return(object[, column, drop = FALSE][[1]])
}

#' @rdname pull
#' @export
pull_ith <- function(object, column) {
  return(object[, colnames(object)[column], drop = FALSE][[1]])
}
