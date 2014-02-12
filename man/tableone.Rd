\name{tableone}
\alias{print.tableone}
\alias{tableone}
\title{Tableone summarizes a selection of the data set as would be presented in a
table 1 for a manuscript.}
\usage{
tableone(vars, by = NULL, data = NULL, complete = TRUE, margin = 2,
  overall = TRUE, stat.con.1 = mean, stat.con.2 = sd,
  tests = c("chisq.test", "t.test"), fisher = TRUE,
  digits = getOption("qwraps.digits", 3), big.mark = ",")

\method{print}{tableone}(tab1, file = "", title = "",
  ctable = getOption("qwraps.tableone.ctable", TRUE), cgroup, n.cgroup,
  rgroup, n.rgroup, rowname, col.just, ...)
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
  tests to report for 1) categorical variables and 2)
  continuous variables.  If set to NULL, then the tests and
  associated p-values are omitted from the output.
  Defaults are for the \code{chisq.test} and the
  \code{t.test}.  If the value is set to NULL the tests
  will be omitted from the output.  Only \code{chisq.test}
  and \code{fisher.test} are allowed for categorical
  variables at this time. Regardless of the test function
  specified, the function should return a list value named
  \code{p.value}.  The second test can be either a t.test
  or a aov. more tests will be allowed in later versions.}

  \item{fisher}{logicial if TRUE and tests[1] ==
  "chisq.test" then if \code{any(chisq.test()$expected) <
  5} the test is redone using a \code{fisher.exact}. This
  option is ignored if \code{tests[1] == "fisher.test"}.}

  \item{big.mark}{Default is a comma to separate, i.e.,
  1000 would be reported as 1,000.}

  \item{digits}{number of digits to round stat.con.1 and
  stat.con.2}

  \item{pdigits}{number of digits to report p-values to.}

  \item{show.equal.sign}{}
}
\value{
a list with the elements intended to be used with
\code{\link{Hmisc::latex}} \item{tab}{a string of formated
numbers.  Non-numbers are empty strings. If the p-value is
less than 10^(-pdigits) the return will be '<
10^(-pdigits)} \item{cgrp}{a character vector for the
column groups, the levels of \code{by}} \item{ncgrp}{a
numeric vector for n.cgroup} \item{rgrp}{a character vector
for the row groups} \item{rcgrp}{a numeric vector for
n.cgroup} \item{rwnms}{a character vector of row names}
}
\description{
Summarize variables by a categorical variable within a
given data.frame. Overall and by results are presented
along with simiple hypothesis tests.
}
\details{
EXPLAIN THE DETAILS MORE, FACTORS AND NUMERS ARE IMPORANT,
WILL TRANSFORM CHARACTERS TO FACTORS
}
\examples{
data(diamonds, package = "ggplot2")

## simple examples for the just getting the matrix for table one
## this could be used in xtable, Hmisc::latex, and likely other functions
tableone("cut", "color", diamonds[1:50, ])$tab
tableone("price", "color", diamonds[1:50, ],
         tests = c("chisq.test", #' "aov"))$tab
tableone(c("cut", "price"), "color", diamonds[1:50, ],
         tests = c("chisq.test", "aov"))$tab

## A LaTeX table via Hmisc::latex

tab1 <- tableone(c("cut", "price"), "color", diamonds[1:50, ],
                 tests = c("chisq.test", "aov"))
Hmisc::latex(tab1$tab,
             file     = "",
             title    = "Example from qwraps",
             ctable   = TRUE,
             cgroup   = tab1$cgrp,
             n.cgroup = tab1$ncgrp,
             colhead  = NULL,
             rgroup   = tab1$rgrp,
             n.rgroup = tab1$nrgrp,
             rowname  = tab1$rwnm,
             caption  = "Example Table 1 from the qwraps with Hmisc.",
             label    = "tab:tableone",
             col.just = rep("r", ncol(tab1$tab)))

## A better and easier way to get the table into a LaTeX file
print(tab1)
}
\author{
Peter DeWitt
}
\keyword{table}}
\keyword{{summary}

