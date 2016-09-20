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
}
