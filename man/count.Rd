\name{count}
\alias{count}
\title{Count and percentage of a factor level.}
\usage{
  count(var, val, data, equal.or.in = "equal",
    show.n = TRUE, show.percent = TRUE,
    digits = getOption("qwraps.percent.digits", 1),
    big.mark = ",",
    latex = getOption("qwraps.latex", TRUE))
}
\arguments{
  \item{var}{name of a factor in a data frame.}

  \item{val}{value(s)/level(s) of interest to count}

  \item{data}{data.frame with var included}

  \item{equal.or.in}{Use either a '==' or '\%in\%' for the
  logical operator in var == val or var \%in\% val.}

  \item{show.n}{Boolean, default to TRUE, to show the raw
  counts.}

  \item{show.percent}{Boolean, default to TRUE, for
  reporting the percentage with the count.}

  \item{digits}{Number of decimal places to report a
  percentage with.  This includes trailing zeros.}

  \item{latex}{Not currently implemented.}
}
\value{
  A sting of the form n, (\%).
}
\description{
  Returns a string with the count and/or percentage of a
  particular level of a factor for easy reporting in
  general but specifically for sweaved/knitted documents.
}
\details{
  %% ~~ If necessary, more details than the description
  above ~~
}
\note{
  Intended to be used with data in data frames, not in
  parent.evn
}
\examples{
## Load example data set diamonds from the ggplot2 package
data(diamonds, package = "ggplot2")

## find a report the number and percentage of "Fair" cut diamonds
count(var = "cut", val = "Fair", data = diamonds)
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
\keyword{~kwd1}
\keyword{~kwd2}

