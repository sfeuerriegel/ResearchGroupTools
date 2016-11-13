#' Rapid line plot with ggplot2
#'
#' Creates simple line plot with ggplot2.
#' @param x Values on x-axis if a vector. If it contains two columns, that it plots the first column on the x-axis, the second on the y-axis.
#' @param y Optional values on the y-axis.
#' @return Object of type \code{\link[ggplot2]{ggplot}}.
#' @examples
#' linePlot(1:10)
#'
#' x <- seq(0, 4, length.out = 100)
#' linePlot(x, sin(x))
#' @export
linePlot <- function(x, y = NULL) {
 if (is.data.frame(x)) {
   if (ncol(x) == 2) {
     df <- x
   } else if (ncol(x) == 1 && !is.null(y)) {
     df <- cbind(x, y = y)
   } else {
     stop("Argument 'x' has invalid dimensions.")
   }
 } else if (is.null(y)) {
   df <- data.frame(x = 1:length(x), y = x)
 } else {
   df <- data.frame(x = x, y = y)
 }

 p <- ggplot2::ggplot(df) +
   ggplot2::geom_line(ggplot2::aes_string(x = colnames(df)[1], y = colnames(df)[2])) +
   ggplot2::theme_bw()

  return(p)
}

# #' @importFrom stats cor.test
# #' @export
# corPlot <- function(x, y = NULL, verbose = TRUE, ...) {
#   df <- cbind(x, y)
#
#   if (verbose) {
#     print(cor.test(df[, 1], df[, 2]))
#   }
#
#
# }
#
# # TODO: barplot with melt


discrete_colours <- function(colours) {
  force(colours)
  function(n) {
    if (n > length(colours)) {
      stop("Internal RGT colors not sufficient for ", length(colours), " colors.", call.=FALSE)
    } else {
      colours[1:n]
    }
  }
}

set_theme_journal <- function(palette, pos = 1, envir = as.environment(pos)) {

  colours <- palette$swatch[-1]

  updateGeoms <- function(geoms, parameters)
    for (geom in geoms) update_geom_defaults(geom, parameters)

  # Geoms that only require a default colour.
  updateGeoms(c("abline", "point", "density", "errorbar", "errorbarh", "hline", "line"), list(colour = colours[1]))

  update_geom_defaults("text", list(colour = palette$swatch[1]))

  # Geoms that only require a default fill.
  updateGeoms(c("area", "bar", "dotplot"), list(fill = colours[1]))

  # Special geoms.
  update_geom_defaults("boxplot", list(colour = palette$swatch[1], fill = colours[1]))
  update_geom_defaults("smooth", list(colour = colours[2], fill = colours[2]))
  update_geom_defaults("dotplot", list(colour = colours[1], fill = colours[1]))

  scale_updates <- list(
    scale_colour_discrete = function(...) discrete_scale("colour", "RGT", discrete_colours(colours), ...),
    scale_fill_discrete = function(...) discrete_scale("fill", "RGT", discrete_colours(colours), ...),
    scale_colour_continuous = function(...) continuous_scale("colour", "RGT", seq_gradient_pal(palette$gradient[["low"]], palette$gradient[["high"]]), ...),
    scale_fill_continuous = function(...) continuous_scale("fill", "RGT", seq_gradient_pal(palette$gradient[["low"]], palette$gradient[["high"]]), ...),
    scale_colour_gradient = function(...) continuous_scale("colour", "RGT", seq_gradient_pal(palette$gradient[["low"]], palette$gradient[["high"]]), ...),
    scale_fill_gradient = function(...) continuous_scale("fill", "RGT", seq_gradient_pal(palette$gradient[["low"]], palette$gradient[["high"]]), ...)
  )

  Map(function (name, f) assign(name, f, envir = envir),
      names(scale_updates),
      scale_updates)
}

reset_theme_journal <- function () {
  scales <- expand.grid(c("colour", "fill"), c("continuous", "discrete", "gradient"))
  rm(list = apply(scales, 1L, function (x) sprintf("scale_%s_%s", x[1L], x[2L])), envir = .GlobalEnv)

  # Find list of all geoms to reset.
  geoms <- ls(pattern = "^geom_", envir = as.environment("package:ggplot2"))
  for (geom in geoms) {

    # Get the short goem name. E.g. for "geom_bar" it would be "bar".
    geom_name <- strsplit(geom, "_")[[1L]][-1L]

    # Some geoms cannot be reset because they have unusual parameters (e.g. geom_map).
    try({

      # Extract defaults.
      geom_defaults <- get(geom)()$geom$default_aes()

      # Set default again.
      update_geom_defaults(geom_name, geom_defaults)

    }, silent = TRUE)
  }
}

#' Plotting routine in journal-style
#'
#' Wrapper to \code{ggplot} with automated change to gray-scale coloring.
#' @param data Default dataset used for plotting.
#' @param mapping Default list of aesthetics.
#' @param ... Further arguments passed to \code{\link[ggplot2]{ggplot}}.
#' @param environment Alternative search space if a varaible is not found in
#' \code{data}.
#' @return Returns a masked \code{ggplot} object that is encoded as class
#' "\code{RGT_plot}".
#' @examples
#' library(ggplot2)
#'
#' df <- data.frame(x = 1:20,
#'                  y = 1:20,
#'                  z = as.factor(rep(1:4, each = 5)))
#'
#' ggplot(df) +
#'   geom_line(aes(x = x, y = y, color = z))
#'
#' jplot(df) +
#'   geom_line(aes(x = x, y = y, color = z))
#'
#' ggplot(df) +
#'   geom_point(aes(x = x, y = y, color = z))
#'
#' jplot(df) +
#'   geom_point(aes(x = x, y = y, color = z))
#' @export
jplot <- function(data = NULL, mapping = aes(), ..., environment = parent.frame()) {
  p <- ggplot(data, ...)
  class(p) <- c("RGT_plot", class(p))
  return(p)
}

#' Routine to call plotting functionality
#'
#' Function call is redirected to \code{ggplot2} in order to plot the argument.
#' @param x Object of type \code{RGT_plot}.
#' @param ... Further arguemnts passed on to \code{ggplot2}.
#' @examples
#' library(ggplot2)
#'
#' df <- data.frame(x = 1:20,
#'                  y = 1:20,
#'                  z = as.factor(rep(1:4, each = 5)))
#'
#' ggplot(df) +
#'   geom_line(aes(x = x, y = y, color = z))
#'
#' jplot(df) +
#'   geom_line(aes(x = x, y = y, color = z))
#'
#' ggplot(df) +
#'   geom_point(aes(x = x, y = y, color = z))
#'
#' jplot(df) +
#'   geom_point(aes(x = x, y = y, color = z))
#' @export
print.RGT_plot <- function(x, ...) {
  greyscale <- list(
    background = "#ffffff",
    text = c(inner = "#444444", outer = "#444444"),
    line = c(inner = "#909090", outer = "#909090"),
    gridline = "#D0D0D0",
    swatch = c("#000000",
               "#515151", "#909090",
               "#D0D0D0", "#444444",
               "#111111", "#EAEAEA",
               "#666666", "#000000"),
    gradient = c(low = "#D0D0D0", high = "#000000")
  )

  set_theme_journal(greyscale)

  class(x) <- class(x)[-1]
  out <- print(x)
  force(out)

  reset_theme_journal()

  return(out)
}



