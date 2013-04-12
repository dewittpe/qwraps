\name{count}
\alias{count}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  count and percentage of a factor with a given value
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
count(var, val, data, equal.or.in = "equal", show.n = TRUE, show.percent = TRUE, digits = getOption("qwraps.percent.digits", 1), latex = getOption("qwraps.latex", TRUE))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{var}{
%%     ~~Describe \code{var} here~~
}
  \item{val}{
%%     ~~Describe \code{val} here~~
}
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{equal.or.in}{
%%     ~~Describe \code{equal.or.in} here~~
}
  \item{show.n}{
%%     ~~Describe \code{show.n} here~~
}
  \item{show.percent}{
%%     ~~Describe \code{show.percent} here~~
}
  \item{digits}{
%%     ~~Describe \code{digits} here~~
}
  \item{latex}{
%%     ~~Describe \code{latex} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (var, val, data, equal.or.in = "equal", show.n = TRUE, 
    show.percent = TRUE, digits = getOption("qwraps.percent.digits", 
        1), latex = getOption("qwraps.latex", TRUE)) 
{
    if (equal.or.in == "equal") {
        n <- sum(data[, var] == val)
        p <- n/sum(!is.na(data[, var]))
    }
    else {
        n <- sum(data[, var] \%in\% val)
        p <- n/dim(data)[1]
    }
    paste(n, " (", frmt(p * 100, digits), "\\\%) ", sep = "")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
