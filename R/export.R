#' Exports object to LaTeX
#'
#' Export of objects from this package to LaTeX.
#' @param object A return value from e.g. \code{\link{descriptiveStatistics}}.
#' @param filename String specifying the filname for export. If \code{NULL}, the
#' routine uses the name based on the class of \code{object}.
#' @param digits Number of digits. If not specified otherwise, the number of
#' digits is directly infered from the attribute of \code{object}.
#' @export
export <- function(object, filename = NULL, digits = attr(object, "digits")) {
  if (inherits(object, "RGT_descriptives")) {
    print(xtable::xtable(object, digits = digits),
                         only.contents = TRUE, include.colnames = FALSE, booktabs = TRUE,
                         file = ifelse(is.null(filename), "table_descriptives.tex", filename),
                         type = "latex")
  } else {
    stop("Class type of argument 'object' not recognized.")
  }
}
