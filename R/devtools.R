#' Builds, loads and checks package
#'
#' Simple wrapper to rebuild, load and check package. This function specifically
#' updates the manual as well.
#' @param  updateReadme Flag whether to update the README.Rmd (default: FALSE).
#' @examples
#' \dontrun{
#'   rebuildPackage()
#' }
#' @export
rebuildPackage <- function(updateReadme = FALSE) {
  devtools::build()
  devtools::load_all()
  devtools::check()

  if (updateReadme) {
    system("Rscript -e \"rmarkdown::render('README.Rmd')\"")
  }
}
