#' @importFrom utils assignInNamespace
.onLoad <- function(libname, pkgname) {
  packageStartupMessage("ResearchGroupTools: Initializing seed to 0.")
  set.seed(0)

  packageStartupMessage("ResearchGroupTools: Setting ggplot theme to 'theme_bw'.")

  ggplot2::theme_set(ggplot2::theme_bw())
  ggplot2::theme_update(legend.position = c(0.15, 0.8))

  packageStartupMessage("ResearchGroupTools: Fixing behavior of texreg regarding '-0.0...'.")

  coeftostring_internal <- texreg::coeftostring
  coeftostring_fixed <- function(x, lead.zero = FALSE, digits = 2) {
    num <- coeftostring_internal(x, lead.zero = lead.zero, digits = digits)
    if (lead.zero) {
      if (num == paste0("-0.", paste0(rep("0", digits), collapse = "")))
      {
        return(paste0("0.", paste0(rep("0", digits), collapse = "")))
      }
    } else {
      if (num == paste0("-.", paste0(rep("0", digits), collapse = "")))
      {
        return(paste0(".", paste0(rep("0", digits), collapse = "")))
      }
    }

    return(num)
  }

  unlockBinding("coeftostring", as.environment("package:texreg"))
  assignInNamespace("coeftostring", coeftostring_fixed, ns="texreg", envir=as.environment("package:texreg"))
  assign("coeftostring", coeftostring_fixed, as.environment("package:texreg"))
  lockBinding("coeftostring", as.environment("package:texreg"))

  packageStartupMessage("ResearchGroupTools: Fixing behavior of xtable regarding '-0.0...'.")

  sanitize_numbers_internal <- xtable::sanitize.numbers
  sanitize_numbers_fixed <- function(str, ...) {
    if (length(str) > 1) {
      return(unlist(lapply(str, sanitize_numbers_fixed, ...)))
    }

    num <- sanitize_numbers_internal(str, ...)

    if (nchar(num) > 4 && num == paste0("-0.", paste0(rep("0", nchar(num) - 3), collapse = ""))) {
      return(paste0("0.", paste0(rep("0", nchar(num) - 3), collapse = "")))
    } else if (nchar(num) > 3 && num == paste0("-.", paste0(rep("0", nchar(num) - 2), collapse = ""))) {
      return(paste0(".", paste0(rep("0", nchar(num) - 2), collapse = "")))
    }

    return(num)
  }

  unlockBinding("sanitize.numbers", as.environment("package:xtable"))
  assignInNamespace("sanitize.numbers", sanitize_numbers_fixed, ns="xtable", envir=as.environment("package:xtable"))
  assign("sanitize.numbers", sanitize_numbers_fixed, as.environment("package:xtable"))
  lockBinding("sanitize.numbers", as.environment("package:xtable"))

  packageStartupMessage("ResearchGroupTools: Changing summary.lm to work with custom covariance estimators'.")

  summary_lm_internal <- stats::summary.lm
  summary_lm_fixed <- function(object, correlation = FALSE, symbolic.cor = FALSE, ...) {
    s <- summary_lm_internal(object, correlation, symbolic.cor, ...)

    if ("RGT" %in% class(object)) {
      coef_col_names <- colnames(s$coefficients)
      coef_row_names <- rownames(s$coefficients)

      s$coefficients <- unclass(lmtest::coeftest(object, vcov. = object$vcov))
      colnames(s$coefficients) <- coef_col_names
      rownames(s$coefficients) <- coef_row_names
    }

    return(s)
  }

  unlockBinding("summary.lm", as.environment("package:stats"))
#  assignInNamespace("summary.lm", summary_lm_fixed, ns="stats", envir=as.environment("package:stats"))
  assign("summary.lm", summary_lm_fixed, as.environment("package:stats"))
  lockBinding("summary.lm", as.environment("package:stats"))

  coeftest_internal <- lmtest::coeftest.default
  coeftest_fixed <- function(x, vcov. = NULL, df = NULL, ...) {
    if ("RGT" %in% class(x)) {
      return(coeftest_internal(x, vcov. = x$vcov, df = df))
    } else {
      return(coeftest_internal(x, vcov. = vcov., df = df))
    }
  }

  unlockBinding("coeftest.default", as.environment("package:lmtest"))
  assignInNamespace("coeftest.default", coeftest_fixed, ns="lmtest", envir=as.environment("package:lmtest"))
  assign("coeftest.default", coeftest_fixed, as.environment("package:lmtest"))
  lockBinding("coeftest.default", as.environment("package:lmtest"))
}
