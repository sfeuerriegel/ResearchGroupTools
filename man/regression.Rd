% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/regression.R
\name{regression}
\alias{regression}
\title{Customized all-in-one regression function}
\usage{
regression(formula, data = NULL, subset = NULL, dummies = NULL,
  cutoff = NULL, rmDummyNA = TRUE, vcov = NULL)
}
\arguments{
\item{formula}{of type \code{formula}.}

\item{data}{An optional data frame contain the variables in the model (excluding the
dummy variables).}

\item{subset}{Vector of integers or booleans defining the subset of observations to be
used.}

\item{dummies}{String denoting the name of the variable (i.e. matrix or data frame)
containing all dummy variables.}

\item{cutoff}{Relative cutoff on each side in percent (default: \code{NULL}). Values
are given in percient, e.g. \code{0.5} represents 0.5\%
at each end).}

\item{rmDummyNA}{Boolean indicating whether to remove dummy variables with NA
coefficient (default: removal).}

\item{vcov}{Estimator used for computing the covariance matrix. Default is \code{NULL}
which results in to the ordinary least squares estimator. Alternatives, are, for
instance, \code{NeweyWest} from the \code{sandwich} package.}
}
\description{
Function performs default regression via ordinary least squares. It also supports dummy
variables which are not included in the dataset \code{data}, but in a global variable
attached to a formula. With this input, this function can filter for a subset, remove
outliers at a certain cutoff and remove dummies that are NA.
}
\examples{
x <- 1:100
clusters <- rep(c(1, 2), 50)
dummies <- model.matrix(~ clusters)
y <- x + clusters + rnorm(100)
d <- data.frame(x = x, y = y)

m <- regression(formula("y ~ x + dummies"), data = d, subset = 1:90,
                dummies = "dummies", cutoff = 0.5)
summary(m)

library(sandwich)
m <- regression(formula("y ~ x + dummies"), data = d, subset = 1:90,
                dummies = "dummies", cutoff = 0.5, vcov = NeweyWest)
summary(m)
}

