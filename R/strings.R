#' Operator for concatenating
#'
#' Operator is a handy alternative to \code{paste()}, which allows you
#' to concatenate strings.
#' @param ... Individual objects that are to be concatenated.
#' @return String with concatenated objects.
#' @examples
#' "a" %+% "b"
#' 3 %+% 4
#' `%+%`(letters)
#' @export
concatenate <- function(...) {
  return(paste0(...))
}
