\name{frmt}
\alias{frmt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
format numbers with a set number of digits, including trailing zeros.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
frmt(x, digits = getOption("qwraps.digits", 3), big.mark = ",")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{digits}{
%%     ~~Describe \code{digits} here~~
}
  \item{big.mark}{
%%     ~~Describe \code{big.mark} here~~
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
function (x, digits = getOption("qwraps.digits", 3), big.mark = ",") 
{
    idx <- is.na(x)
    y <- formatC(x, digits = digits, format = "f", big.mark = big.mark)
    y[idx] <- ""
    return(y)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
