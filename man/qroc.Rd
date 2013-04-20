\name{qroc}
\alias{qroc}
\title{ROC plots in ggplot}
\usage{
  qroc(fit, newdata = NULL, show.auc = FALSE, n = 200)
}
\arguments{
  \item{fit}{%% ~~Describe \code{fit} here~~}

  \item{newdata}{%% ~~Describe \code{newdata} here~~}

  \item{show.auc}{%% ~~Describe \code{show.auc} here~~}

  \item{n}{%% ~~Describe \code{n} here~~}
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
function (fit, newdata = NULL, show.auc = FALSE, n = 200)
{
   require(ggplot2)
   pred.vals <- predict(fit, newdata, type = "response")
   true.pos <- function(threshold) {
       sum((pred.vals >= threshold) & (fit$y))
   }
   true.neg <- function(threshold) {
       sum((pred.vals < threshold) & !(fit$y))
   }
   false.pos <- function(threshold) {
       sum((pred.vals >= threshold) & !(fit$y))
   }
   false.neg <- function(threshold) {
       sum((pred.vals < threshold) & (fit$y))
   }
   x <- matrix(seq(1, 0, length = n))
   true.positives <- apply(x, 1, true.pos)
   true.negatives <- apply(x, 1, true.neg)
   false.positives <- apply(x, 1, false.pos)
   false.negatives <- apply(x, 1, false.neg)
   sensitivity <- true.positives/(true.positives + false.negatives)
   specificity <- true.negatives/(true.negatives + false.positives)
   df <- data.frame(false.positives = 1 - specificity, true.positives = sensitivity)
   auc <- sum((df[2:n, 1] - df[1:(n - 1), 1]) * 1/2 * (df[2:n,
       2] + df[1:(n - 1), 2]))
   q <- ggplot(data = df, aes(x = false.positives, y = true.positives)) +
       geom_line(size = 1.25) + geom_segment(aes(x = 0, y = 0),
       xend = 1, yend = 1, colour = "blue", lty = 2) + xlab("1-Specificity") +
       ylab("Sensitivity")
   if (show.auc) {
       q <- q + geom_text(aes(x = 0.8, y = 0), hjust = 0, vjust = 0,
           label = paste("AUC:", formatC(auc, digits = 4, format = "f")))
   }
   return(list(auc = auc, data = df, plot = q))
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

