\name{params.coxph}
\alias{params.coxph}
\title{parameter estimates from a cox ph model fit}
\usage{
  params.coxph(fit,
    alpha = getOption("qwraps.alpha", 0.05), ...)
}
\arguments{
  \item{fit}{}

  \item{alpha}{}

  \item{\dots}{}
}
\value{
  ?
}
\description{
  %% ~~ A concise (1-5 lines) description of what the
  function does. ~~
}
\details{
  %% ~~ If necessary, more details than the description
  above ~~
}
\note{
  ?
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (fit, alpha = getOption("qwraps.alpha", 0.05), ...)
{
   rtn <- summary(fit)$coef[, -2]
   rtn[, 3] <- rtn[, 1] + qnorm(1 - alpha/2) * rtn[, 2]
   rtn[, 2] <- rtn[, 1] + qnorm(alpha/2) * rtn[, 2]
   return(params_frmtr(rtn, ...))
 }
}
\author{
  Peter DeWitt
}
\references{
  ?
}
\seealso{
  ?
}
\keyword{~kwd1}
\keyword{~kwd2}

