\name{frmt}
\alias{frmt}
\title{format numbers with a set number of digits, including trailing zeros.}
\usage{
  frmt(x, digits = getOption("qwraps.digits", 3),
    big.mark = ",")
}
\arguments{
  \item{x}{vector of numbers to be formated.}

  \item{digits}{digits to follow the decimal mark,
  including trailing zeros.}

  \item{big.mark}{Default is a comma to separate, i.e.,
  1000 would be reported as 1,000.}
}
\value{
  a string.
}
\description{
  This function is a wrapper for a call to formatC.
}
\details{
  Used to format numbers by both the end user and by
  several of the other functions in the qwraps package.
}
\note{
  %% ~~further notes~~
}
\examples{
x <- c(1.12, 1.231, 4.338, 10.4, 1.1)
 frmt(x, 2)
}
\author{
  Peter DeWitt
}
\references{
  %% ~put references to the literature/web site here ~
}
\seealso{
  %% ~~objects to See Also as \code{\link{help}}, ~~~
}
\keyword{~format}
\keyword{~trailing}

