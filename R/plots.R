linePlot <- function(x, y = NULL, ...) {
  if (is.data.frame(x) && ncol(x) == 2) {
    df <- x
  } else {
    df <- cbind(x, y)
  }

  p <- ggplot(df) +
    geom_line(aes(x = colnames(df)[1], y = colnames(df)[2])) +
    theme_bw()

  return(p)
}

corPlot <- function(x, y = NULL, verbose = TRUE, ...) {


  if (verbose) {
    print(cor.test(df[, 1], df[, 2]))
  }


}
