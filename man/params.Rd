\name{params}
\alias{params}
\alias{params.coxph}
\alias{params.glm}
\alias{params.lm}
\alias{params.survfit}
\title{generic call for reporting the parameter estiamtes from different regression
models}
\usage{
  params(fit, alpha = getOption("qwraps.alpha", 0.05), ...)

  \method{params}{coxph} (fit,
    alpha = getOption("qwraps.alpha", 0.05), ...)

  \method{params}{glm} (fit,
    alpha = getOption("qwraps.alpha", 0.05), ...)

  \method{params}{lm} (fit,
    alpha = getOption("qwraps.alpha", 0.05), ...)

  \method{params}{survfit} (fit,
    alpha = getOption("qwraps.alpha", 0.05), ...)
}
\arguments{
  \item{fit}{a lm, glm, coxph, or survfit object}

  \item{alpha}{significance level, 100(1-alpha)% CIs will
  be generated}

  \item{\dots}{arguments to pass to params_frmtr as noted
  in the following}

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
\description{
  Parameter estiamtes, confidence intervals, and p-values
  form regression models.  Results are presented in three
  forms, numeric matrix, character matrix, and individual
  strings.  The character matrix and strings are intended
  to be used when knitting with LaTeX.
}
\details{
  %% ~~ If necessary, more details than the description
  above ~~
}
\examples{
fit <- lm(mpg ~ wt + cyl, data = mtcars)
params(fit)
params(fit, param = "wt")

## logisitic regression
fit <- glm(I(mgp > 25) ~ wt + cyl, data = mtcars,
           family = binomial(link = "logit"))
# log odds
params(fit)
# odds ratios
params(fit, fun = exp)
}
\author{
  Peter DeWitt
}
\seealso{
  \code{\link{params_frmtr}}
}
\keyword{regression}
\keyword{results}

