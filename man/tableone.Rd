\name{tableone}
\alias{tableone}
\title{Tableone summarizes a selection of the data set as would be presented in a
table 1 for a manuscript.}
\usage{
  tableone(vars, by = NULL, data = NULL, complete = TRUE,
    margin = 2, overall = TRUE, stat.con.1 = mean,
    stat.con.2 = sd, tests = c("chisq.test", "t.test"),
    fisher = TRUE, big.mark = ",")
}
\arguments{
  \item{vars}{characer vector of the names of the variables
  in data to summarize.}

  \item{by}{character name of the variable in data to
  sumamrize vars by}

  \item{data}{the data.frame}

  \item{complete}{logical reported for the complete data
  records only?  If TRUE, the dataset will be subseted via
  \code{na.omit(data[, c(vars, by)])}, if FALSE, then all
  avaiable data records will be used.}

  \item{margin}{default to report column percentages for
  categorical variables, can be set to 1 for row
  percentages or 1:2 for overall percentages.}

  \item{overall}{logical if TRUE include a column with the
  summary statistics for the whole sample in addition to
  the summary of the \code{vars} \code{by}}

  \item{stat.con.1}{function to summarize continuous
  variables with, default is the \code{mean}}

  \item{stat.con.2}{second function to sumamry continuous
  variables with, adefault is \code{sd}}

  \item{tests}{a vector of length two with the statistical
  tests to report for 1 categorical variables and 2
  continuous variables.  Defaults are for the
  \code{chisq.test} and the \code{t.test}.  If the value is
  set to NULL the tests will be omitted from the output.
  Only \code{chisq.test} and \code{fisher.test} are allowed
  for categorical variables at this time. Regardless of the
  test function specified, the function should return a
  list value named \code{p.value}.  The second test can be
  either a t.test or a aov. more tests will be allowed in
  later versions.}

  \item{fisher}{logicial if TRUE and tests[1] ==
  "chisq.test" then if \code{any(chisq.test()$expected) <
  5} the test is redone using a \code{fisher.exact}. This
  option is ignored if \code{tests[1] == "fisher.test"}.}

  \item{big.mark}{Default is a comma to separate, i.e.,
  1000 would be reported as 1,000.}

  \item{pdigits}{number of digits to report p-values to.}

  \item{show.equal.sign}{}
}
\value{
  a string of formated numbers.  Non-numbers are empty
  strings. If the p-value is less than 10^(-pdigits) the
  return will be '< 10^(-pdigits)
}
\description{
  Summarize variables by a categorical variable within a
  given data.frame. Overall and by results are presented
  along with simiple hypothesis tests.
}
\details{
  EXPLAIN THE DETAILS MORE, FACTORS AND NUMERS ARE
  IMPORANT, WILL TRANSFORM CHARACTERS TO FACTORS
}
\examples{
data(diamonds, package = "ggplot2")
tableone("cut", "color", diamonds[1:50, ])
tableone("price", "color", diamonds[1:50, ], tests = c("chisq.test", "aov"))
tableone(c("cut", "price"), "color", diamonds[1:50, ],
         tests = c("chisq.test", "aov"))
}
\author{
  Peter DeWitt
}
\keyword{~summary}
\keyword{table}

