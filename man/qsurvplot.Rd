\name{qsurvplot}
\alias{qsurvplot}
\title{KM plots}
\usage{
qsurvplot(fit, show.ci = getOption("qwraps.show.ci", TRUE))
}
\arguments{
  \item{fit}{a \code{survfit} object}

  \item{show.ci}{logicial, show the confidence intervals
  for the KM fit}

  \item{l.name:}{deprecated}

  \item{labels:}{deprecated}
}
\value{
? a ggplot object showing the KM curve(s)
}
\description{
Create Kaplan-Meier plots via ggplot2
}
\examples{
library(survival)
leukemia.surv <- survfit(Surv(time, status) ~ x, data = aml)
plot(leukemia.surv, lty = 2:3)
qsurvplot(leukemia.surv)
}
\author{
Peter DeWitt
}
\references{
?
}
\seealso{
\code{\link{survfit}}
}
\keyword{~kwd1}
\keyword{~kwd2}

