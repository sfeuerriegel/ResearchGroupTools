#' Hides variables in coefficient test
#'
#' Excludes variables -- usually dummies variables -- following a certain naming scheme. It also appends significance stars.
#' @param model Object of type \code{lm}.
#' @param vcov. A specification of the covariance matrix as used by
#' \code{\link[lmtest]{coeftest}}.
#' @param hide A string. All variables starting with that name are excluded.
#' @examples
#' x1 <- 1:100
#' x2 <- rep(c(1, 2), 50)
#' y <- x1 + x2 + rnorm(100)
#'
#' m <- lm(y ~ x1 + x2)
#'
#' showCoeftest(m, hide = "x") # leaves only the intercept
#' @importFrom stats coef
#' @export
showCoeftest <- function(model, vcov. = NULL, hide = NULL) {
  if (is.null(hide)) {
    idx <- 1:length(coef(model))
  } else {
    # Note: "-" to invert selection
    idx <- -grep(paste0("^", hide), names(coef(model)))
  }

  ct <- lmtest::coeftest(model, vcov. = vcov.)[idx, ]
  if (is.null(dim(ct))) {
    # only one variable left; change layout
    ct <- as.data.frame(t(ct))
    rownames(ct) <- names(coef(model))[idx]
  }

  return(data.frame(ct, Stars = as.vector(unlist(lapply(ct[, 4], signifianceToStars)))))
}

#' Create formula from strings
#'
#' @param dependent Name of dependent variable.
#' @param independent String or vector of strings with independent variables.
#' @param dummies Optional name for dummies. If \code{NULL} (default), then dummies are omitted.
#' @examples
#' makeFormula("y", "x")
#' makeFormula("y", c("x1", "x2", "x3"))
#' makeFormula("y", c("x1", "x2", "x3"), "dummies")
#' @importFrom stats formula
#' @export
makeFormula <- function(dependent, independent, dummies = NULL) {
  f <- paste0(dependent, " ~ ", paste0(independent, collapse = " + "))

  if (!is.null(dummies)) {
    f <- paste0(f, " + dummies")
  }

  f <- formula(f)

  # formula is associated to an environment; change to parent
  environment(f) <- globalenv()

  return(f)
}

#' Extracts key statistics of regression
#'
#' Function extracts various parameter from an \code{lm} object, including the (adjuste) R-squared,
#' AIC, BIC and the outcome of the F-test.
#' @param model Object of type \code{lm}.
#' @return Returns data frame with named columns.
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

#' Customized all-in-one regression function
#'
#' Function performs default regression via ordinary least squares. It also supports dummy
#' variables which are not included in the dataset \code{data}, but in a global variable
#' attached to a formula. With this input, this function can filter for a subset, remove
#' outliers at a certain cutoff and remove dummies that are NA.
#' @param formula of type \code{formula}.
#' @param data An optional data frame contain the variables in the model (excluding the
#' dummy variables).
#' @param subset Vector of integers or booleans defining the subset of observations to be
#' used.
#' @param dummies String denoting the name of the variable (i.e. matrix or data frame)
#' containing all dummy variables.
#' @param cutoff Relative cutoff on each side in percent (default: \code{NULL}). Values
#' are given in percient, e.g. \code{0.5} represents 0.5\%
#' at each end).
#' @param rmDummyNA Boolean indicating whether to remove dummy variables with NA
#' coefficient (default: removal).
#' @examples
#' x <- 1:100
#' clusters <- rep(c(1, 2), 50)
#' dummies <- model.matrix(~ clusters)
#' y <- x + clusters + rnorm(100)
#' d <- data.frame(x = x, y = y)
#'
#' m <- regression(formula("y ~ x + dummies"), data = d, subset = 1:90,
#'                 dummies = "dummies", cutoff = 0.5)
#' summary(m)
#' @importFrom stats coef lm
#' @export
regression <- function(formula, data = NULL, subset = NULL, dummies = NULL, cutoff = NULL, rmDummyNA = TRUE) {
  dummies_copy <- NULL
  if (!is.null(dummies)) {
    dummies_copy <- get(dummies, envir = attr(formula, ".Environment"))
  }

  # Select subset
  if (!is.null(subset)) {
    if (is.null(data)) {
      stop("Argument 'subset' chooses a subset of observation, but no data is given to choose from.")
    } else {
      data <- data[subset, ]
    }

    if (!is.null(dummies)) {
      dummies_copy <- get(dummies, envir = attr(formula, ".Environment"))
      assign(dummies, dummies_copy[subset, ], envir = attr(formula, ".Environment"))
    }
  }

  if (!is.null(cutoff)) {
    m <- lm(formula, data)
    idx_rm <- getRowsOutlierRemoval(m, cutoff)

    cat("Removing", length(idx_rm), "observations; i.e.", length(idx_rm) / nrow(data), "percent.\n")

    if (!is.null(data)) {
      data <- data[-idx_rm, ]
    }
    if (!is.null(dummies)) {
      dummies_local <- get(dummies, envir = attr(formula, ".Environment"))
      assign(dummies, dummies_local[-idx_rm, ], envir = attr(formula, ".Environment"))
    }
  }

  m <- lm(formula, data)

  if (rmDummyNA && any(is.na(coef(m)))) {
    if (is.null(dummies)) {
      stop("Argument 'rmDummyNA' specifies to remove NA dummies but no dummies are present. Instead, at least one coefficient is NA.")
    }

    cat("Dropping", sum(is.na(coef(m))), "coefficients:", paste(names(coef(m))[is.na(coef(m))]), "\n")

    dummies_local <- get(dummies, envir = attr(formula, ".Environment"))
    dummies_local <- dummies_local[, -which(colnames(dummies_local) %in% gsub(dummies, "", names(coef(m))[is.na(coef(m))]))]
    assign(dummies, dummies_local, envir = attr(formula, ".Environment"))

    m <- lm(formula, data)
  }

  if (!is.null(dummies_copy)) {
    # restore dummy object
    assign(dummies, dummies_copy, envir = attr(formula, ".Environment"))
  }

  return(m)
}

