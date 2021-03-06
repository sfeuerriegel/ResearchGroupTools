% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ts.R
\name{logReturns}
\alias{logReturns}
\title{Lagged log-returns}
\usage{
logReturns(x, lag = 1, na_padding = TRUE, base = exp(1))
}
\arguments{
\item{x}{A numeric vector containing the values for which returns are calculated.}

\item{lag}{Integer indicating which lag to use (default: 1).}

\item{na_padding}{Flag whether to pad truncated values with NA's (default: TRUE).}

\item{base}{A positive number giving the base with respect to which the logarithm is
computed. Default is e, i.e. \code{exp(1)}.}
}
\value{
Numeric vector with log-returns of specified lag.
}
\description{
Calculates log-returns between values with specified lag.
}
\note{
Parameter \code{na_padding} behaves slightly different from \code{\link{diff}}
in order to achieve consistency with \code{\link[dplyr]{lead}} and \code{\link[dplyr]{lag}}.
}
\examples{
logReturns(1:10)
logReturns(c(1, 2, 4, 8, 16, 32), base = 2)
logReturns(c(1, 2, 4, 8, 16, 32), base = 2, na_padding = FALSE)
}
\seealso{
\code{\link{diff}}, \code{\link{returns}}
}

