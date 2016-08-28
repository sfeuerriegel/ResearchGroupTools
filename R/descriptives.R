#' Pretty summary statistics
#'
#' Calculates summary statistics for dataset and display key variables in a nice
#' format. It also writes the table into a LaTeX file if desired.
#' @param x Dataset in the form of a matrix or a \code{data.frame}.
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
#' @examples
#' data(USArrests)
#' correlationMatrix(USArrests)
#'
#' # alternatively, with table exported into a LaTeX file
#' correlationMatrix(USArrests, filename = "table_cor.tex")
#' unlink("table_cor.tex")
#' @export
correlationMatrix <- function(x, y = NULL,
                              method = c("pearson", "spearman"),
                              removeTriangle = c("upper", "lower", "none"),
                              diagonale = FALSE,
                              digits = 3,
                              filename = NULL) {
  x <- as.matrix(x)
  if (!is.null(y)) {
    y <- as.matrix(y)
  }

  # Compute correlation matrix
  cor_mat <- Hmisc::rcorr(x, y, type = method[1])

  cor_coef <- cor_mat$r    # Matrix of correlation coefficients
  cor_Pvalues <- cor_mat$P # Matrix of P-values

  # round to desired digits
  cor_coef <- round(cor_coef, digits)

  # matrix with stars
  cor_stars <- apply(cor_Pvalues, 1:2, function(p) {
    s <- signifianceToStars(p)
    return(ifelse(s == "", "", paste0("^{", s, "}")))
    })

  output <- matrix(paste0(format(cor_coef), cor_stars), ncol = ncol(x))

  if (diagonale) {
    diag(output) <- paste0(format(round(1, digits)) , "^{***}")
  } else {
    diag(output) <- ""
  }

  rownames(output) <- colnames(x)
  if (is.null(y)) {
    colnames(output) <- colnames(x)
  } else {
    colnames(output) <- colnames(y)
  }

  if (removeTriangle[1] == "upper") {
    output[upper.tri(output)] <- ""
  } else if (removeTriangle[1] == "lower") {
    output[lower.tri(output)] <- ""
  }

  if (!is.null(filename)) {
    print(xtable::xtable(output, digits = digits),
          only.contents = TRUE, booktabs = TRUE,
          file = filename, type = "latex")
  }

  return(cor_coef)
}

