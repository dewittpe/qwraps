\name{qacf}
\alias{qacf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Auto correlation plot in ggplot
}
\description{
  Reproduce the stats::acf plots from the base R graphics package in ggplot2.
}
\usage{
qacf(x, conf.level = 0.95, lag.max = NULL, type = c("correlation", "covariance", "partial"), show.sig = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    vector, or data.frame, to be plotted.
}
  \item{conf.level}{
    confidence intervals for determining 'significant' autocorrelations.
}
  \item{lag.max}{
    how many lags to present.  default is the same as the stats::acf plot
}
  \item{type}{
    same options as stats::acf, either a correlation (default), covariance, or
    partial correlation plot
}
  \item{show.sig}{
    Extension to the stats::acf function.  If TRUE the lags are colored to
    indicate statistically significant correlations different from zero.  This
    option is only used for the corrleation plot.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  a ggplot object
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
  Peter DeWitt
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# Generate a random data set
set.seed(42)
n <- 250
x1 <- x2 <- x3 <- x4 <- vector('numeric', length = n)
x1[1] <- runif(1)
x2[1] <- runif(1)
x3[1] <- runif(1)
x4[1] <- runif(1)

# white noise
Z.1 <- rnorm(n, 0, 1)
Z.2 <- rnorm(n, 0, 2)
Z.3 <- rnorm(n, 0, 5)

for(i in 2:n)
{
	x1[i] <- x1[i-1] + Z.1[i] - Z.1[i-1] + x4[i-1] - x2[i-1]
	x2[i] <- x2[i-1] - 2 * Z.2[i] + Z.2[i-1] - x4[i-1]
	x3[i] <- x3[i-1] + x2[i-1] + 0.2 * Z.3[i] + Z.3[i-1]
	x4[i] <- x4[i-1] + runif(1, 0.5, 1.5) * x4[i-1]
}
testdf <- data.frame(x1, x2, x3, x4)

# Base acf plot for one variable
acf(testdf$x1)

# qacf plot for one variable
qacf(testdf$x1)

# more than one variable
acf(testdf)
qacf(testdf)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
