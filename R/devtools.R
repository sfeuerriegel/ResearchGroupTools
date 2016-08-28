#' Builds, loads and checks package
#'
#' Simple wrapper to rebuild, load and check package. This function specifically
#' updates the manual as well.
#' @examples
#' \dontrun{
#'   remakePackage()
#' }
#' @export
remakePackage <- function() {
  devtools::build()
  devtools::load_all()
  devtools::check()
}
