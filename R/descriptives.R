#' Pretty summary statistics
#'
#' Calculates summary statistics for dataset and display key variables in a nice
#' format. It also writes the table into a LaTeX file if desired.
#' @param x Dataset in the form of a matrix or a data frame.
#' @param digits Number of digits to be printed (default: 3).
#' @param subset Variables to be selected.
#' @param filename Path to which a LaTeX copy is exported (default is
#' \code{table_descriptives.tex}). If \code{NULL}, no file is generated.
#' @return Matrix with pretty output.
#' @examples
#' data(USArrests)
#' descriptiveStatistics(USArrests)
#' unlink("table_descriptives.tex")
#' @export
descriptiveStatistics <- function(x, digits = 3,
                                  subset = c("mean", "median", "min", "max", "sd", "skew", "kurtosis"),
                                  filename = "table_descriptives.tex") {
  m <- psych::describeBy(x, group = rep("", nrow(x)), mat = TRUE, digits = digits)[, subset]

  # fix the concatenated "1" for the grouping in describeBy
  rownames(m) <- colnames(x)

  if (!is.null(filename)) {
    cat("Column names: ", paste(colnames(m), collapse = ", "))
    print(xtable::xtable(m, digits = digits),
          only.contents = TRUE, include.colnames = FALSE, booktabs = TRUE,
          file = filename, type = "latex")
  }

  return(m)
}

#' Pretty correlation matrix
#'
#' Calculates a correlation matrix for a dataset and display variables, as well
#' as P-values in asteriks notation nicely. The function also writes the table
#' into a LaTeX file if desired.
#' @param x A numeric matrix for which to compute the correlation matrix.
#' @param y Optional second matrix. If omitted or \code{NULL}, it is replaced
#' by \code{x}.
#' @param method Method, i.e. Pearson's \code{r} or Spearman's \code{rho} rank
#' correlation coefficients.
#' @param removeTriangle Parameter indicating whether to remove a duplicated
#' triangle of the matrix. Defaul is the upper triangle that is removed.
#' @param diagonale If \code{FALSE} (default), the diagonale is omitted. If
#' \code{TRUE}, it is included as 1 along with significance stars.
#' @param digits Number of digits to be printed (default: 3).
#' @param filename Path to which a LaTeX copy is exported (default is
#' \code{NULL}). If \code{NULL}, no file is generated.
#' @return Correlation matrix with pretty output.
#' @details To embed this correlation table inside LaTeX, requires a specific
#' format as follows:
#' \enumerate{
#'   \item Insert the library \code{SIunitx} inside the preamble of your
#'   LaTeX document.
#'   \item At the same location, define the following macro
#'   \code{\\newcommand\{\\sym}[1]{\\rlap\{$^\{#1\}$\}\}}
#'   \item Also in the preamble, change the sisetup
#'   \code{\\sisetup\{input-symbols=\{()*\}\}}
#'   \item Finally, choose \code{S} as your column alignment.
#' }
#' Details: \url{http://tex.stackexchange.com/questions/47418/siunitx-specifying-custom-command-as-input-symbol};
#' also refer to the example in the README.
#' @examples
#' data(USArrests)
#' correlationMatrix(USArrests)
#'
#' # alternatively, with table exported into a LaTeX file
#' correlationMatrix(USArrests, filename = "table_cor.tex")
#' unlink("table_cor.tex")
#' @source Idea: \url{http://www.sthda.com/english/wiki/elegant-correlation-table-using-xtable-r-package}
#' @export
correlationMatrix <- function(x, y = NULL,
                              method = c("pearson", "spearman"),
                              removeTriangle = c("upper", "lower", "none"), diagonale = FALSE,
                              digits = 3, filename = NULL) {
  x <- as.matrix(x)
  if (!is.null(y)) {
    y <- as.matrix(y)
  }

  # Compute correlation matrix
  cor_mat <- Hmisc::rcorr(x, y, type = method[1])

  cor_coef <- cor_mat$r    # Matrix of correlation coefficients
  cor_Pvalues <- cor_mat$P # Matrix of P-values

  # round to desired digits (incl. workaround for R-functions)
  cor_coef <- format(round(cbind(pi, cor_coef), digits))[, -1]

  # matrix with stars
  cor_stars_screen <- apply(cor_Pvalues, 1:2, signifianceToStars)
  cor_stars_file <- apply(cor_Pvalues, 1:2, signifianceToTeX)

  output_screen <- matrix(paste0(cor_coef, cor_stars_screen),
                          ncol = ifelse(is.null(y), ncol(x), ncol(x) + ncol(y)))
  output_file <- matrix(paste0(cor_coef, cor_stars_file),
                        ncol = ifelse(is.null(y), ncol(x), ncol(x) + ncol(y)))

  if (!is.null(y)) {
    # Hmisc:rcorr concatenates x and y; so extract submatrix
    output_screen <- output_screen[1:ncol(x), (ncol(x) + 1) : (ncol(x) + ncol(y))]
    output_file <- output_file[1:ncol(x), (ncol(x) + 1) : (ncol(x) + ncol(y))]
  } else {
    # remove diagonale

    if (diagonale) {
      # incl. dirty workaround for rounding in R
      diag(output_screen) <- paste0(format(round(c(pi, 1), digits))[2] , signifianceToStars(0))
      diag(output_file) <- paste0(format(round(c(pi, 1), digits))[2], signifianceToTeX(0))
    } else {
      diag(output_screen) <- ""
      diag(output_file) <- ""
    }

    if (removeTriangle[1] == "upper") {
      output_screen[upper.tri(output_screen)] <- ""
      output_file[upper.tri(output_file)] <- ""
    } else if (removeTriangle[1] == "lower") {
      output_screen[lower.tri(output_screen)] <- ""
      output_file[lower.tri(output_file)] <- ""
    }
  }

  rownames(output_screen) <- colnames(x)
  rownames(output_file) <- colnames(x)
  if (is.null(y)) {
    colnames(output_screen) <- colnames(x)
    colnames(output_file) <- colnames(x)
  } else {
    colnames(output_screen) <- colnames(y)
    colnames(output_file) <- colnames(y)
  }

  if (!is.null(filename)) {
    print(xtable::xtable(output_file, digits = digits),
          only.contents = TRUE, booktabs = TRUE,
          file = filename, type = "latex",
          sanitize.text.function = identity)
  }

  # Hmisc:rcorr concatenates x and y; so extract submatrix
  return(as.data.frame(output_screen))
}

