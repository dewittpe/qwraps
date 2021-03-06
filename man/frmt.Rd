% Generated by roxygen2 (4.0.1): do not edit by hand
\name{frmt}
\alias{frmt}
\alias{frmtp}
\title{format numbers with a set number of digits, including trailing zeros.}
\usage{
frmt(x, digits = getOption("qwraps.digits", 3), big.mark = ",")

frmtp(x, pdigits = getOption("qwraps.pdigits", 4), show.equal.sign = FALSE)
}
\arguments{
\item{x}{vector of numbers to be formated.}

\item{digits}{digits to follow the decimal mark, including trailing zeros.  If
x is an integer digits are set to 0.}

\item{big.mark}{Default is a comma to separate, i.e., 1000 would be reported
as 1,000.}

\item{pdigits}{number of digits to report p-values to.}

\item{show.equal.sign}{}
}
\value{
a string of formated numbers.  Non-numbers are empty strings.
If the p-value is less than 10^(-pdigits) the return will be '< 10^(-pdigits)
}
\description{
This function is a wrapper for a call to formatC.
}
\details{
Used to format numbers by both the end user and by several of the other
functions in the qwraps package.  frmt is intended for any numeric response
frmtp for formating p-values.
}
\examples{
x <- c(1.12, 1.231, 4.338, 10.4, 1.1, NA, pi, NaN, exp(1))
frmt(x, 2)
frmt(x, 3)
frmt(x*100, 3)

pvals <- c(0.000001, 0.12, 0.0001)
frmtp(pvals)
}
\author{
Peter DeWitt
}
\keyword{~format}
\keyword{~trailing}

