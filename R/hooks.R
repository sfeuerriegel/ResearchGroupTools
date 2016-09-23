#' @importFrom utils assignInNamespace
.onLoad <- function(libname, pkgname) {
  packageStartupMessage("ResearchGroupTools: Initializing seed to 0.")
  set.seed(0)

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
}
