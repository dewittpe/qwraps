#'Count and percentage of a factor level.
#'
#'Returns a string with the count and/or percentage of a particular level of a
#'factor for easy reporting in general but specifically for sweaved/knitted
#'documents.
#'
#'%% ~~ If necessary, more details than the description above ~~
#'
#'@param var name of a factor in a data frame.
#'@param val value(s)/level(s) of interest to count
#'@param data data.frame with var included
#'@param equal.or.in Use either a '==' or '\%in\%' for the logical operator in
#'var == val or var \%in\% val.
#'@param show.n Boolean, default to TRUE, to show the raw counts.
#'@param show.percent Boolean, default to TRUE, for reporting the percentage
#'with the count.
#'@param digits Number of decimal places to report a percentage with.  This
#'includes trailing zeros.
#'@param latex Not currently implemented.
#'@return A sting of the form n, (\%).
#'@note Intended to be used with data in data frames, not in parent.evn
#'@author Peter DeWitt
#'@seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#'@references %% ~put references to the literature/web site here ~
#'@keywords ~kwd1 ~kwd2
#'@examples
#'
#'## Load example data set diamonds from the ggplot2 package
#'data(diamonds, package = "ggplot2")
#'
#'## find a report the number and percentage of "Fair" cut diamonds
#'count(var = "cut", val = "Fair", data = diamonds)
#'
#'
count <-
function(var, val, data, equal.or.in = "equal",
                  show.n = TRUE,
                  show.percent = TRUE,
                  digits = getOption("qwraps.percent.digits", 1),
                  big.mark = ",",
                  latex  = getOption("qwraps.latex", TRUE))
{
  if (equal.or.in == "equal")
  {
    n <- sum(data[, var] == val)
    p <- n / sum(!is.na(data[, var]))
  }
  else
  {
    n <- sum(data[, var] %in% val) 
    p <- n / dim(data)[1]
  }

  if (show.n & show.percent) {
    paste(frmt(n, digits = 0, big.mark = big.mark), " (", frmt(p*100, digits), "\\%) ", sep = "")
  }
  else if(show.n & !show.percent) 
  {
    paste(frmt(n, digits = 0, big.mark = big.mark))
  }
  else if (!show.n & show.percent) {
    paste(frmt(p*100, digits), "\\% ", sep = "")
  }
  else 
  {
    stop("Show something dude.")
  }

}
