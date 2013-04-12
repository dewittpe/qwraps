\name{frmtp}
\alias{frmtp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Format p-values
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
frmtp(x, pdigits = getOption("qwraps.pdigits", 4), equal.sign = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{pdigits}{
%%     ~~Describe \code{pdigits} here~~
}
  \item{equal.sign}{
%%     ~~Describe \code{equal.sign} here~~
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
function (x, pdigits = getOption("qwraps.pdigits", 4), equal.sign = FALSE) 
{
    out <- vector("character", length(x))
    out <- formatC(x, pdigits, format = "f")
    idx <- (x < 0.1^pdigits)
    out[idx] <- paste("\\textless", formatC(0.1^pdigits, pdigits, 
        format = "f"))
    if (equal.sign) 
        out[!idx] <- paste("=", out[!idx])
    out[is.na(x)] <- ""
    return(out)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
