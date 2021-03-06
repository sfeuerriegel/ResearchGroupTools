% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/regression.R
\name{testDiagnostics}
\alias{testDiagnostics}
\title{Test assumptions of least squares}
\usage{
testDiagnostics(model, alpha = 0.05)
}
\arguments{
\item{model}{Linear model to test.}

\item{alpha}{Critical value of hypothesis tests.}
}
\value{
Named vector with outcomes of main test statistics.
}
\description{
Function tests the main assumption of ordinary least squares (OLS), including
non-multicollinearity, non-autocorrelation and homoskedasticity.
}
\examples{
library(car)
m <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)

testDiagnostics(m)
}

