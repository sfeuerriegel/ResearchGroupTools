#' ggplot labels with exponential notation
#'
#' Allows to adjust the numbers display on both axes for ggplot2 plots. The format is such that the expontent is nicely set.
#' @param l Labels
#' @return Labels in format
#' @examples
#' library(ggplot2)
#' df <- data.frame(x=rnorm(100), y=rnorm(100))
#' ggplot(df, aes(x=x, y=y)) +
#'   geom_point() +
#'   scale_x_continuous(labels=scientific_labels) +
#'   scale_y_continuous(labels=scientific_labels)
#' @source Brian Diggs \url{https://groups.google.com/forum/#!topic/ggplot2/a_xhMoQyxZ4}
#' @export
scientific_labels <- function(l) {
  # turn in to character string in scientific notation
  l <- format(l, scientific = TRUE)
  # quote the part before the exponent to keep all the digits
  l <- gsub("^(.*)e", "'\\1'e", l)
  # turn the 'e+' into plotmath format
  l <- gsub("e", "%*%10^", l)
  # return this as an expression
  parse(text=l)
}
