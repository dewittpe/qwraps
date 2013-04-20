\name{frmtp}
\alias{frmtp}
\title{Format p-values}
\usage{
  frmtp(x, pdigits = getOption("qwraps.pdigits", 4),
    equal.sign = FALSE)
}
\arguments{
  \item{x}{%% ~~Describe \code{x} here~~}

  \item{pdigits}{%% ~~Describe \code{pdigits} here~~}

  \item{equal.sign}{%% ~~Describe \code{equal.sign} here~~}
}
\value{
  %% ~Describe the value returned %% If it is a LIST, use
  %% \item{comp1 }{Description of 'comp1'} %% \item{comp2
  }{Description of 'comp2'} %% ...
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
  %% ~~further notes~~
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
\author{
  %% ~~who you are~~
}
\references{
  %% ~put references to the literature/web site here ~
}
\seealso{
  %% ~~objects to See Also as \code{\link{help}}, ~~~
}
\keyword{~kwd1}
\keyword{~kwd2}

