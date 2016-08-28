#' Rapid line plot with ggplot2
#'
#' Creates simple line plot with ggplot2.
#' @param x Values on x-axis if a vector. If it contains two columns, that it plots the first column on the x-axis, the second on the y-axis.
#' @param y Optional values on the y-axis.
#' @return Object of type \code{\link[ggplot2]{ggplot}}.
#' @examples
#' linePlot(1:10)
#'
#' x <- seq(0, 4, length.out = 100)
#' linePlot(x, sin(x))
#' @export
linePlot <- function(x, y = NULL) {
 if (is.data.frame(x)) {
   if (ncol(x) == 2) {
     df <- x
   } else if (ncol(x) == 1 && !is.null(y)) {
     df <- cbind(x, y = y)
   } else {
     stop("Argument 'x' has invalid dimensions.")
   }
 } else if (is.null(y)) {
   df <- data.frame(x = 1:length(x), y = x)
 } else {
   df <- data.frame(x = x, y = y)
 }

 p <- ggplot2::ggplot(df) +
   ggplot2::geom_line(ggplot2::aes_string(x = colnames(df)[1], y = colnames(df)[2])) +
   ggplot2::theme_bw()

  return(p)
}

# #' @importFrom stats cor.test
# #' @export
# corPlot <- function(x, y = NULL, verbose = TRUE, ...) {
#   df <- cbind(x, y)
#
#   if (verbose) {
#     print(cor.test(df[, 1], df[, 2]))
#   }
#
#
# }
#
# # TODO: barplot with melt
