\name{regression_results}
\alias{print.regression_results}
\alias{regression_results}
\alias{regression_results.coxph}
\alias{regression_results.lm}
\alias{regression_results.survfit}
\title{Function for generating a nice, human readable, summary table of a regression
model.}
\usage{
  regression_results(fit, ...)

  \method{regression_results}{coxph} (fit, ...)

  \method{regression_results}{lm} (fit, ...)

  \method{regression_results}{survfit} (fit,
    alpha = getOption("qwraps.alpha", 0.05), ...)

  \method{print}{regression_results} (tab, file = "",
    title = "",
    ctable = getOption("qwraps.regression_results.ctable", TRUE),
    rgroup, n.rgroup, rowname, col.just, ...)
}
\arguments{
  \item{fit}{a lm, glm, coxph, or survfit object}

  \item{\dots}{arguments to pass to params()}
}
\description{
  produce a list object with a matrix and labels for
  printing a latex version of the table via
  print.regression.results()
}
\examples{
## TO DO
}
\author{
  Peter DeWitt
}
\seealso{
  \code{\link{params}}
}
\keyword{regression}
\keyword{results}

