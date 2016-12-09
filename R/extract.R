#' Standardized coefficients
#'
#' Computes standardized coefficient for a regression or time series. Three scores are
#' returned:
#' \itemize{
#'   \item{"Coef:"} This returns the original coefficient \eqn{\beta}.
#'   \item{"SdChange"} A one standard deviation in the independent variable
#'   is linked to percentage increase in the dependent variable. Hence, it is
#'   \eqn{\beta var(\beta)}.
#'   \item{"StdCoef:"} Returns the standardized coefficent. It is calculated through
#'   \eqn{\beta var(\beta) / var(y)} where \eqn{y} is independent variable.
#' }
#' @param model Model. Supported types are linear models (\code{lm}), quantile
#' regressions (\code{rq} from the \code{quantreg} package) or time series
#' (\code{varest} or \code{svarest} from the \code{vars} package).
#' @param hide A string. All variables starting with that name are excluded.
#' @return Coefficients (with transformations) for model variables. In case of
#' multivariate models, it returns a list with an element for each dependent variable.
#' @examples
#' library(dplyr)
#' library(vars)
#' data(Canada)
#'
#' prod <- differences(as.numeric(Canada[, 2]))
#' production <- data.frame(Prod = prod, Lag1 = dplyr::lag(prod), Lag2 = dplyr::lag(prod, 2))
#'
#' m <- lm(Prod ~ Lag1, data = production)
#' standardizeCoefficients(m)
#'
#' m <- lm(Prod ~ Lag1 + Lag2, data = production)
#' standardizeCoefficients(m)
#'
#' var.2c <- VAR(Canada, p = 2, type = "none")
#' standardizeCoefficients(var.2c$varresult$e)
#' std <- standardizeCoefficients(var.2c)
#' std$e
#'
#' library(quantreg)
#' data(stackloss)
#'
#' qr <- rq(stack.loss ~ stack.x, 0.25)
#' standardizeCoefficients(qr)
#' @rdname standardizeCoefficients
#' @export
standardizeCoefficients <- function(model, hide = NULL) {
  UseMethod("standardizeCoefficients", model)
}

#' @rdname standardizeCoefficients
#' @export
standardizeCoefficients.lm <- function(model, hide = NULL) {
  vars <- model$model
  coefs <- model$coefficients

  if ("(Intercept)" %in% names(coefs)) {
    coefs <- coefs[names(coefs) != "(Intercept)"]
  }

  if (length(coefs) == 0) {
    stop("No coefficient to standardize.")
  } else if (length(coefs) == 1) {
    sd_change <- coefs * sd(vars[, -1])
    std_coef <- coefs * sd(vars[, -1]) / sd(vars[, 1])
  } else {
    sd_change <- coefs * apply(vars[, -1], 2, sd)
    std_coef <- coefs * apply(vars[, -1], 2, sd) / sd(vars[, 1])
  }

  result <- data.frame(Coef = coefs, SdChange = sd_change, StandardizedCoef = std_coef)

  if (is.null(hide)) {
    return(result)
  } else {
    # Note: "-" to invert selection
    idx <- -grep(paste0("^", hide), coefs)
    return(result[idx, ])
  }
}

#' @rdname standardizeCoefficients
#' @export
standardizeCoefficients.rq <- function(model, hide = NULL) {
  class(model) <- "lm"
  standardizeCoefficients(model)
}

#' @rdname standardizeCoefficients
#' @export
standardizeCoefficients.varest <- function(model, hide = NULL) {
  l <- lapply(names(model$varresult), function(v) standardizeCoefficients(model$varresult[[v]]))
  names(l) <- names(model$varresult)
  return(l)
}

#' @rdname standardizeCoefficients
#' @export
standardizeCoefficients.svarest <- function(model, hide = NULL) {
  l <- lapply(names(model$var$varresult), function(v) standardizeCoefficients(model$var$varresult[[v]]))
  names(l) <- names(model$var$varresult)
  return(l)
}
