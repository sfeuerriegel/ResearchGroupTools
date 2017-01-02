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

#' Extract t-values from lm object
#'
#' Function extracts the t-values from an \code{lm} object as given by the \code{summary} function.
#' @param model Object of type \code{lm}.
#' @param hide A string. All variables starting with that name are excluded. If \code{NULL} (default),
#' no variables are omitted.
#' @param se Computation scheme for standard errors in case of a quantile regression.
#' @return Vector of type \code{numeric}.
#' @examples
#' \dontrun{
#' x1 <- 1:100
#' x2 <- rep(c(1, 2), 50)
#' y <- x1 + x2 + rnorm(100)
#'
#' m <- lm(y ~ x1 + x2)
#' extract_tvalues(m)
#'
#' qr <- rq(y ~ x1 + x2)
#' extract_tvalues(m)
#' extract_tvalues(m, se = "ker")
#' }
#' @importFrom stats lm
#' @export
#' @rdname extract_tvalues
extract_tvalues <- function(model, hide = NULL, se = "nid") {
  UseMethod("extract_tvalues", model)
}

#' @export
#' @rdname extract_tvalues
extract_tvalues.lm <- function(model, hide = NULL, se = "nid") {
  if (is.null(hide)) {
    idx <- 1:length(coef(model))
  } else {
    # Note: "-" to invert selection
    idx <- -grep(paste0("^", hide), names(coef(model)))
  }

  return(summary(model)$coefficients[idx, "t value"])
}

#' @export
#' @rdname extract_tvalues
extract_tvalues.rq <- function(model, hide = NULL, se = "nid") {
  if (is.null(hide)) {
    idx <- 1:length(coef(model))
  } else {
    # Note: "-" to invert selection
    idx <- -grep(paste0("^", hide), names(coef(model)))
  }

  return(summary(model, se = se)$coefficients[idx, "t value"])
}

#' texreg output with t-values
#'
#' Function is a customized interface to the \code{\link[texreg]{texreg}} function. It replaces
#' standard errors in the output by t-values.
#' @param model Object of type \code{lm} or a \code{list} of \code{lm} and \code{rq} (quantile
#' regression) objects.
#' @param hide A string. All variables starting with that name are excluded. If \code{NULL} (default),
#' no variables are omitted.
#' @param ... Additional parameters that are passed to default \code{\link[texreg]{texreg}} function.
#' @return Object of type \code{\link[texreg]{texregTable}}.
#' @examples
#' x1 <- 1:100
#' x2 <- rep(c(1, 2), 50)
#' y <- x1 + x2 + rnorm(100)
#'
#' m <- lm(y ~ x1 + x2)
#' texreg_tvalues(m, digits = 4)
#'
#' m2 <- lm(y ~ x1)
#' texreg_tvalues(list(m, m2))
#'
#' library(quantreg)
#' data(stackloss)
#'
#' qr25 <- rq(stack.loss ~ stack.x, 0.25)
#' qr50 <- rq(stack.loss ~ stack.x, 0.50)
#' qr75 <- rq(stack.loss ~ stack.x, 0.75)
#' texreg_tvalues(list(qr25, qr50, qr75))
#' @importFrom texreg texreg
#' @export
texreg_tvalues <- function(model, hide = NULL, ...) {
  tvalues <- list()

  if (class(model) == "lm" || class(model) == "rq") {
    tvalues <- extract_tvalues(model, hide = hide)
  } else {
    tvalues <- lapply(model, extract_tvalues, hide)
  }

  return(suppressWarnings(texreg(model, override.se = tvalues, ...)))
}

#' Function to perform \code{textreg} export of \code{spikeslab} objects.
#' @param model Object of type \code{spikeslab}.
#' @param standarized A logical value. If \code{TRUE}, standardized coefficient are used (default).
#' @param exludeNoise A logical value. If \code{TRUE}, non-relevant regressors are excluded (default).
#' @param ... Further parameters (currently ignore for \code{spikeslab}).
#' @return Object of type \code{\link[texreg]{texreg}}.
#' @export
extract_spikeslab <- function(model, standardized = TRUE, excludeNoise = TRUE, ...) {
  # extract information from model object
  coefnames <- model$names
  if (standardized == TRUE) {
    coefs <- model$gnet.scale
  } else {
    coefs <- model$gnet
  }

  predictors <- data.frame(coefnames, coefs, stringsAsFactors = F)
  predictors$inclusionProb <- sapply(c(1:model$verbose[[6]]),
                                     function(x)
                                       sum(sapply(model$model,
                                                  function(y)
                                                    x %in% y)) / model$verbose[[8]])

  if (excludeNoise == TRUE) {
    predictors <- predictors[predictors$coefs != 0,]
  }

  # create and return a texreg object
  tr <- texreg::createTexreg(
    coef.names = predictors$coefnames,
    coef = predictors$coefs,
    gof.names = c("Observations", "Regressors", "MSE"),
    gof = c(length(model$y), sum(predictors$coefs != 0), model$verbose[[9]]),
    gof.decimal = c(TRUE, TRUE, TRUE),
    se = predictors$inclusionProb
  )
  return(tr)
}

#' @export
extract.spikeslab <- function(model, standardized = TRUE, excludeNoise = TRUE, ...) {
  extract_spikeslab(model, standarized, excludeNoise, ...)
}

#' @importFrom texreg extract
#' @export
setMethod("extract",
          signature = className("spikeslab", "spikeslab"),
          definition = extract.spikeslab)

#' Test assumptions of least squares
#'
#' Function tests the main assumption of ordinary least squares (OLS), including
#' non-multicollinearity, non-autocorrelation and homoskedasticity.
#' @param model Linear model to test.
#' @param alpha Critical value of hypothesis tests.
#' @return Named vector with outcomes of main test statistics.
#' @examples
#' library(car)
#' m <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)
#'
#' testDiagnostics(m)
#' @export
testDiagnostics <- function(model, alpha = 0.05){
  output <- rep(FALSE, 4)
  names(output) <- c("Non-multicollinearity", "Homoskedasticity", "Non-autocorrelation", "Non-serial correlation")

  # Multicollinearity

  vifs <- car::vif(model)
  if (any(vifs > 4)) {
    cat("* VIF: The following variables do have a variance inflation factor of 4 or greater:", paste0(colnames(vifs)[vifs > 4], collapse = ", "), ".\n")
  } else {
    cat("* VIF: The variance inflation factor for all variables is <= 4. Multicollinearity appears not to be an issue.\n")
    output["Non-multicollinearity"] <- TRUE
  }

  # Homoskedasticity

  bp <- lmtest::bptest(model)
  if (bp$p.value <= alpha){
    cat("* Breusch-Pagan Test: The hypothesis of homoskedasticity needs to be rejected for the given significance level. Hence, heteroskedasticity.\n")
  } else {
    cat("* Breusch-Pagan Test: The hypothesis of homoskedasticity cannot be rejected for the given significance level. Hence, homoskedasticity\n")
    output["Homoskedasticity"] <- TRUE
  }

  # Serial correlation
  # print("Testing for serial correlation")

  bg <- lmtest::bgtest(model)
  if(bg$p.value <= alpha){
    cat("* Breusch-Godfrey Test: The hypothesis of no serial correlation needs to be rejected for the given significance level. Hence, serial correlation.\n")
  } else {
    cat("* Breusch-Godfrey Test: The hypothesis of no serial correlation cannot be rejected for the given significance level. Hence, no serial correlation.\n")
    output["Non-serial correlation"] <- TRUE
  }

  # Autocorrelation
  # print("Testing for autocorrelation")

  dw <- lmtest::dwtest(model)
  if(dw$p.value <= alpha){
    cat("* Durbin-Watson Test: The hypothesis of no autocorrelation needs to be rejected for the given significance level. Hence, autocorrelation.\n")
  } else {
    cat("* Durbin-Watson Test: The hypothesis of no autocorrelation cannot be rejected for the given significance level. Hence, no autocorrelation.\n")
    output["Non-autocorrelation"] <- TRUE
  }

  # Summary

  if (all(output)) {
    cat("All diagnostic tests seem fine. No autocrrelation, no serial correlation, homoskedasticity and no multicollinearity.\n")
  } else {
    cat("The following diagnostic test caused troubles: ", paste0(names(output)[which(!output)], collapse = ", "), ".\n")
  }

  return(output)
}

#' Step-wise regression
#'
#' Function performs default regression via ordinary least squares in step-wise fashion.
#' That is, it iteratively includes an additional regressor one-by-one. It also supports dummy
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
#' @return Returns a list with different, estimated models.
#' @note The dummy term is included in all regressions (if present).
#' @examples
#' x1 <- 1:100
#' x2 <- sin(1:100)
#' clusters <- rep(c(1, 2), 50)
#' dummies <- model.matrix(~ clusters)
#' y <- x1 + x2 + clusters + rnorm(100)
#' d <- data.frame(x1 = x1, x2 = x2, y = y)
#'
#' models <- regressionStepwise(formula("y ~ x1 + x2 + dummies"), data = d, subset = 1:90,
#'                              dummies = "dummies", cutoff = 0.5)
#'
#' length(models)
#'
#' library(texreg)
#' texreg(models, omit.coef = "dummies")
#' @export
regressionStepwise <- function(formula, data = NULL, subset = NULL, dummies = NULL,
                               cutoff = NULL, rmDummyNA = TRUE) {
  formula_string <- toString(formula)

  independent_var <- strsplit(formula_string, ",")[[1]][2]
  dependent_vars <- strsplit(formula_string, ",")[[1]][3]
  dependent_vars <- trimws(strsplit(dependent_vars, fixed = TRUE, "+")[[1]])
  dependent_vars <- setdiff(dependent_vars, dummies)

  dummy_var <- ifelse(is.null(dummies), NULL, paste0("+ ", dummies))

  new_formula <- paste(independent_var, "~", dependent_vars[1], dummy_var)
  formula_list <- formula(new_formula)

  for (i in 2:length(dependent_vars)) {
    new_formula <- paste(new_formula, "+", dependent_vars[i], dummy_var)
    formula_list <- c(formula_list, formula(new_formula))
  }

  formula_list <- lapply(formula_list,
                         function (x) {
                           attr(x, ".Environment") <- attr(formula, ".Environment")
                           return(x)
                         })

  model_list <- lapply(formula_list,
                       regression,
                       data = data, subset = subset, dummies = dummies,
                       cutoff = cutoff, rmDummyNA = rmDummyNA)

  return(model_list)
}

# TODO: ivreg

# TODO: clustered regression
