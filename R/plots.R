#' @export
linePlot <- function(x, y = NULL, ...) {
  if (is.data.frame(x) && ncol(x) == 2) {
    df <- x
  } else {
    df <- cbind(x, y)
  }

  p <- ggplot2::ggplot(df) +
    ggplot2::geom_line(ggplot2::aes(x = colnames(df)[1], y = colnames(df)[2])) +
    ggplot2::theme_bw()

  return(p)
}

#' @importFrom stats cor.test
#' @export
corPlot <- function(x, y = NULL, verbose = TRUE, ...) {
  df <- cbind(x, y)

  if (verbose) {
    print(cor.test(df[, 1], df[, 2]))
  }


}

# TODO: barplot with melt
