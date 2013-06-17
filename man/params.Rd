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

  \item{\dots}{}
}
\description{
  Parameter estiamtes, confidence intervals, and p-values
  form regression
}
\details{
  %% ~~ If necessary, more details than the description
  above ~~
}
\examples{
## TO BE WRITTEN
}
\author{
  Peter DeWitt
}
\keyword{regression}
\keyword{results}

