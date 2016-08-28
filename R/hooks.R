.onLoad <- function(libname, pkgname) {
  packageStartupMessage("ResearchGroupTools: Initializing seed to 0.")
  set.seed(0)
}
