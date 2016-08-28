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
  for (p in c(packages, ...)) {
    cat(paste0(p, "\n"))
    if (!(p %in% rownames(installed.packages()))) {
      install.packages(p)
    }

    library(p, character.only = TRUE)
  }
}
