\name{qroc}
\alias{qroc}
\title{ROC plots in ggplot}
\usage{
  qroc(fit, newdata = NULL, show.auc = FALSE, n = 200)
}
\arguments{
  \item{a}{glm object from a family = binomial}

  \item{newdata}{if omitted the data used to fit the model
  is used, else this newdata is passed to the
  \code{predict} function and the resulting ROC plot is
  based on this data.}

  \item{show.auc}{show the area under the curve on the plot
  via text.}

  \item{n}{number of thresholds to use, defualt is 200}
}
\value{
  a list with the plot, the auc, and the data.frame used to
  plot the ROC
}
\description{
  plot a ROC in ggplot for a given logistic regression fit.
}
\details{
  %% ~~ If necessary, more details than the description
  above ~~
}
\examples{
fit <- glm(I(mpg > 15) ~ wt + cyl, data = mtcars,
           family = binomial(link = "logit"))
roc <- qrocc(fit)
roc$auc
roc$plot
}
\author{
  Peter DeWitt
}
\keyword{ROC}

