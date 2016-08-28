# TODO: better summary

#' @export
getRowsOutlierRemoval <- function(model, cutoff = 0.5) {
  # Finally, we give justice to extreme stock price effects and remove outliers at the 0.5 % level at both ends.
  if (cutoff <= 0 || cutoff >= 100) {
    stop("Argument 'cutoff' must be in the range 0 .. 100 (in %).")
  }

  res <- resid(model)
  res.qt <- quantile(res, probs = c(cutoff/100, 1 - cutoff/100))
  idx_remove <- which(res < res.qt[1] | res > res.qt[2])

  return(idx_remove)
}
