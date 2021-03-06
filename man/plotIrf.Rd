% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ts.R
\name{plotIrf}
\alias{plotIrf}
\title{Pretty plot of impulse response function}
\usage{
plotIrf(irf, name = NULL, ylab = NULL, alpha = 0.3, n.ahead = NULL,
  filename = NULL, width = 10, height = 6, ...)
}
\arguments{
\item{irf}{Object package with impulse response function, i.e. type
\code{varirf} from the \code{vars}}

\item{name}{Name of variable of interest.}

\item{ylab}{Text on y-axis. By default, this argument is \code{NULL} and the
variable name is taken automatically instead.}

\item{alpha}{Opacity for confidence interval. Default is 0.3}

\item{n.ahead}{Optional parameter to later choose a smaller x-range for
plotting. Argument expects a numeric value with the maximum step.}

\item{filename}{Filename to export the table as LaTeX. Default is
\code{NULL}, i.e. no export.}

\item{width}{Width of exported figure (default: 10).}

\item{height}{Height of exported figure (default: 6).}

\item{...}{Further arguments passed on to \code{\link[ggplot2]{ggsave}}}
}
\value{
Object of \code{\link[ggplot2]{ggplot}}.
}
\description{
Plots the impulse response function in black/white manner for
suitability with common journals.
}
\examples{
library(vars)
data(Canada)
# For VAR
var.2c <- VAR(Canada, p = 2, type = "const")
\dontrun{
irf <- irf(var.2c, impulse = "e", response = "prod", boot = TRUE)
plotIrf(irf, ylab = "Production")
plotIrf(irf, ylab = "Production", filename = "irf_e_prod.pdf")
}
}

