#' Wrapper function for loading and installing packages
#'
#' Installs package if these are not yet available on the current machine. Afterwards,
#' all packages are loaded.
#' @param packages String or vector of strings identifying the packages.
#' @param ... Strings with further package names
#' @examples
#' \dontrun{
#' Library("lmtest")
#' Library("lmtest", "psych")
#' }
#' @importFrom utils install.packages installed.packages
#' @export
Library <- function(packages, ...) {
  if (!is.character(packages)) {
    stop("Argument 'packages' expects string or vector of strings.")
  }
  if (!missing(...) && !is.character(packages)) {
    stop("Optional arguments in '...' must be of string or vector of strings.")
  }

  for (p in c(packages, ...)) {
    cat(paste0(p, "\n"))
    if (!(p %in% rownames(installed.packages()))) {
      install.packages(p)
    }

    library(p, character.only = TRUE)
  }
}

#' Loads and installs default libraries for regressions
#'
#' Bundles routines for regressions as the mapping between test routine
#' and originating package is not always obvious.
#' @examples
#' \dontrun{
#'   loadRegressionLibraries()
#' }
#' @export
loadRegressionLibraries <- function() {
  # Libraries for data handling
  Library("readr",       # for read_csv
          "dplyr",       # data handling
          "magrittr",    # %>% operator
          "xtable")      # export of LaTeX tables

  # Libraries for econometrics
  Library("psych",       # for descriptive statistics
          "lmtest",      # dwtest, bgtest, bptest
          "sandwich",    # NeweyWest
          "car",         # car
          "strucchange") # Fstats
}

