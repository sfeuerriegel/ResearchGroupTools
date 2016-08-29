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

#' Extracts key statistics of regression
#'
#' Function extracts various parameter from an \code{lm} object, including the (adjuste) R-squared,
#' AIC, BIC and the outcome of the F-test.
#' @param model Object of type \code{lm}.
#' @return Returns \code{data.frame} with named columns.
#' @examples
#' x <- 1:10
#' y <- 1 + x + rnorm(10)
#' m <- lm(y ~ x)
#'
#' extractRegressionStatistics(m)
#' @importFrom stats AIC BIC
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

#' Identify row numbers for outlier removal
#'
#' Function removes outliers at a relative percentage at both ends.
#' @param model Object of type \code{lm}.
#' @param cutoff Relative cutoff on each side in percent (default: \code{0.5}, i.e. 0.5\% at each end).
#' @return Returns vector with indices of the observations to be removed.
#' @examples
#' d <- data.frame(x = 1:200, y = 1:200 + rnorm(200))
#' m <- lm(y ~ x, d)
#'
#' idx_rm <- getRowsOutlierRemoval(m)
#'
#' m <- lm(y ~ x, d[-idx_rm, ])
#' @importFrom stats as.formula pf quantile resid
#' @export
getRowsOutlierRemoval <- function(model, cutoff = 0.5) {
  if (cutoff <= 0 || cutoff >= 100) {
    stop("Argument 'cutoff' must be in the range 0 .. 100 (in %).")
  }

  res <- resid(model)
  res.qt <- quantile(res, probs = c(cutoff/100, 1 - cutoff/100))
  idx_remove <- which(res < res.qt[1] | res > res.qt[2])

  return(idx_remove)
}
