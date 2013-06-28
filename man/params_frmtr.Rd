\name{params_frmtr}
\alias{params_frmtr}
\title{formating helper function, not intended to be called by end users}
\usage{
  params_frmtr(rtn, param = NULL,
    digits = getOption("qwraps.digits", 3),
    pdigits = getOption("qwraps.pdigits", 4),
    show.ci = getOption("qwraps.show.ci", TRUE),
    show.pval = getOption("qwraps.show.pval", TRUE),
    alpha = getOption("qwraps.alpha", 0.05), fun = NULL,
    show.equal.sign = FALSE, unit = "", big.mark = "",
    small.mark = "")
}
\arguments{
  \item{rtn}{object from params.xxx}

  \item{param}{if NULL (default) then a full matrix of of
  all coeffients will be returned.  A character
  represtation of the parameters of interest can be
  returned if specified.}

  \item{digits}{number of digits after the decimal point,
  included trailing zeros, to print numbers to: see
  \code{\link{frmt}}}

  \item{pdigits}{number of digits to format p-values: see
  \code{\link{frmtp}}}

  \item{show.ci}{logical, return confidence intervals}

  \item{show.pval}{logical, return the p-values}

  \item{alpha}{significant level, reporting 100(1-alpha)%
  CIs}

  \item{fun}{funciton for transforming results.
  Particularly useful is \code{fun = exp} when working with
  logisitic regression models, for example.}

  \item{show.equal.sign}{passed to \code{frmtp}}

  \item{unit}{can be added to the strings returned such
  that the string could be xx mg (95% CI: yy, zz; p-value =
  0.pppp) instead of just xx (95% CI: yy, zz; p-vaue =
  0.pppp)}

  \item{big.mark}{passed to frmt}

  \item{small.mark}{passed to frmt}
}
\value{
  a list with three elements
}
\description{
  helper function, not intended to be called by end users,
  for the \code{\link{params}} function.
}
\details{
  %% ~~ If necessary, more details than the description
  above ~~
}
\note{
  ?
}
\examples{
## See \\code{\\link{params}}  \\code{params_frmtr} is not intended to be called
## by the end user
}
\author{
  Peter DeWitt
}
\seealso{
  \code{\link{params}} \code{\link{frmt}}
}

