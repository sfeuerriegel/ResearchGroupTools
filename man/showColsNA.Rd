% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/matrix.R
\name{showColsNA}
\alias{showColsNA}
\title{Show columns with \code{NA} values}
\usage{
showColsNA(d)
}
\arguments{
\item{d}{A matrix or dataframe.}
}
\value{
Subset of \code{d} containing \code{NA} values. Returns \code{NULL} if no \code{NA} values
are present.
}
\description{
Show columns that contain an \code{NA} value
}
\examples{
m <- matrix(letters[c(1, 2, NA, 3, NA, 4, 5, 6, 7, 8)],
            ncol = 2, byrow = FALSE)
colnames(m) <- c("x", "y")
m
findColsNA(m)
showColsNA(m)

d <- data.frame(x = 1:10)
d
findColsNA(d)
showColsNA(d)
}
\seealso{
\code{\link{findColsNA}}
}

