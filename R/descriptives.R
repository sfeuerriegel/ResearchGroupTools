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

  # fix kurtosis (as psych::describeBy is actuall computing the excess kurtosis)
  colnames(m) <- plyr::mapvalues(colnames(m), c("kurtosis"), c("excess_kurtosis"))

  if (!is.null(filename)) {
    col_names <- plyr::mapvalues(colnames(m),
                                 c("mean", "median", "min", "max", "sd", "skew", "excess_kurtosis"),
                                 c("Mean", "Median", "Min.", "Max", "Std. dev.", "Skewness", "Excess kurtosis"))
    showColumns(col_names)

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
    showColumns(colnames(output_file))
    print(xtable::xtable(output_file, digits = digits),
          only.contents = TRUE, booktabs = TRUE,
          file = filename, type = "latex",
          sanitize.text.function = identity)
  }

  # Hmisc:rcorr concatenates x and y; so extract submatrix
  return(as.data.frame(output_screen))
}

#' Removes observations with outliers
#'
#' Function removes outliers for a given set of variables at a relative
#' percentage at both ends.
#' @param df Data frame, matrix or tibble with input variables
#' @param variables Variables used for trimming. By default, all variables are
#' included.
#' @param cutoff Relative cutoff on each side in percent (default: \code{0.5},
#' i.e. 0.5\% at each end).
#' @return Returns vector with indices of the observations to be removed.
#' @examples
#' d <- data.frame(x1 = rnorm(200), x2 = rnorm(200), y = rnorm(200))
#'
#' d_trimmed <- removeOutlierObservations(d)
#' dim(d_trimmed)
#'
#' d_trimmed <- removeOutlierObservations(d, variables = "y", cutoff = 2.0)
#' dim(d_trimmed)
#'
#' d_trimmed <- removeOutlierObservations(d, variables = c("x1", "x2"), cutoff = 2.0)
#' dim(d_trimmed)
#' @importFrom stats quantile
#' @export
removeOutlierObservations <- function(df, variables = colnames(df), cutoff = 0.5) {
  if (cutoff <= 0 || cutoff >= 100) {
    stop("Argument 'cutoff' must be in the range 0 .. 100 (in %).")
  }

  if (inherits(df, "tbl") || inherits(df, "tbl_df")) {
    idx_remove <- unique(unlist(lapply(variables,
                                       function(v) {
                                         qt <- quantile(pull_string(df, v), probs = c(cutoff/100, 1 - cutoff/100))
                                         idx_remove <- which(df[, v] < qt[1] | df[, v] > qt[2])
                                         return(idx_remove)
                                       })))
  } else if (inherits(df, "data.frame") || inherits(df, "matrix")) {
    idx_remove <- unique(unlist(lapply(variables,
                                       function(v) {
                                         qt <- quantile(df[, v], probs = c(cutoff/100, 1 - cutoff/100))
                                         idx_remove <- which(df[, v] < qt[1] | df[, v] > qt[2])
                                         return(idx_remove)
                                       })))
  } else {
    stop("Class for argument 'df' is not supported. Expected are tbl, data.frame or matrix.")
  }

  cat("Dropping", length(idx_remove), "observations, i.e.", length(idx_remove)/nrow(df), "%.")

  return(df[-idx_remove, ])
}

