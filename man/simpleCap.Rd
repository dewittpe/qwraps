\name{simpleCap}
\alias{simpleCap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Simple Capitalization
}
\description{
  Remove a "." from a string and capitalize the first letter of every word.
}
\usage{
simpleCap(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    string to edit
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  The requested reformatted string.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
  Peter DeWitt
}
\note{
  Slight modification of the function noted in the exampleof the chartr {base}
  help file.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
s <- "this.is.a.string"
simpleCap(s)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
