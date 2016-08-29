# TODO: better summary

#' Create formula from strings
#'
#' @param dependent Name of dependent variable.
#' @param independent String or vector of strings with independent variables.
#' @param dummies Optional name for dummies. If \code{NULL} (default), then dummies are omitted.
#' @examples
#' makeFormula("y", "x")
#' makeFormula("y", c("x1", "x2", "x3"))
#' makeFormula("y", c("x1", "x2", "x3"), "dummies")
#' @export
makeFormula <- function(dependent, independent, dummies = NULL) {
  f <- paste0(dependent, " ~ ", paste0(independent, collapse = " + "))

  if (!is.null(dummies)) {
    f <- paste0(f, " + dummies")
  }

  return(as.formula(f))
}

#' @importFrom stats AIC BIC as.formula pf quantile resid
#' @export
extractRegressionStatistics <- function(model) {
  s <- summary(model)

  # Calculate P-value for F-test manually as it is not stored in the summary
  f <- as.vector(s$fstatistic)
  p <- pf(f[1], f[2], f[3], lower.tail=F)
  attributes(p) <- NULL

  return(data.frame(Observations = length(model$residuals),
                    DegreesFreedom = model$df.residual,
                    ResidualError = s$sigma,
                    Rsquared = s$r.squared,
                    AdjRsquared = s$adj.r.squared,
                    AIC = AIC(model),
                    BIC = BIC(model),
                    Fstatistic = f[1],
                    Fsignficance = p,
                    Fstars = signifianceToStars(p)))
}



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
