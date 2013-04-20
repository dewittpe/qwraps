\name{params.survfit}
\alias{params.survfit}
\title{parameter estimates from survfit}
\usage{
  params.survfit(fit,
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
   if (1 - alpha != fit$conf.int) {
       warning("Refitting survfit with requested confidence level")
       fit <- update(fit, conf.int = 1 - alpha)
   }
   rtn <- summary(fit)$table[, c(5:7, 7)]
   rtn[, 4] <- NA
   show.pval <- FALSE
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

