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
#'@param data data.frame with var included, default to NULL.  Function is
#' intended to be used with data in data frames.
#'@param operator The boolean operator to use.  Options are == by default if
#' 'length(value) == 1' and '\%in\%' by default if 'length(value) > 1.'  The
#' match uses the '\%in\%' operator.
#'@param na.rm a logical indicating whether missing values should be removed.
#'@param show.n Boolean, default to TRUE, to show the raw counts.
#'@param show.percent Boolean, default to TRUE, for reporting the percentage
#'with the count.
#'@param digits Number of decimal places to report a percentage with.  This
#'includes trailing zeros.
#'@param latex Not currently implemented.
#'@return A sting of the form n, (\%).
#@note 
#@author Peter DeWitt
#@seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#@references %% ~put references to the literature/web site here ~
#'@keywords ~kwd1 ~kwd2
#'@examples
#'
#' ## Load example data set diamonds from the ggplot2 package
#' data(diamonds, package = "ggplot2")
#'
#' ## find a report the number and percentage of "Fair", and/or "Good" cut diamonds
#` count(var = "cut", value = "Fair", data = diamonds, operator = "equal")
#` count(var = "cut", value = "Good", data = diamonds, operator = "equal")
#` count(var = "cut", value = c("Fair", "Good"), data = diamonds, operator = "equal")
#` count(var = "cut", value = c("Fair", "Good"), data = diamonds, operator = "match")
#`
#` ## value can be a character string or a number
#` count(var = "price", value = "1000", data = diamonds, operator = "equal")
#` count(var = "price", value = "1000", data = diamonds, operator = "less")
#` count(var = "price", value = 1000, data = diamonds, operator = "less")
#` count(var = "price", value = 1000, data = diamonds, operator = "lesseq")
#'
#'
count <- function(var, value, data = NULL, 
                  operator = c("equal", "match", "less", "greater", "lesseq",
                               "greatereq", "is.na", "isnot.na"), 
                  na.rm = FALSE, 
                  show.n = TRUE,
                  show.percent = TRUE,
                  digits = getOption("qwraps.percent.digits", 1),
                  big.mark = ",",
                  latex  = getOption("qwraps.latex", TRUE)) {

  if (!show.percent & !show.n) {
    stop("show.percent and/or show.n must be set to TRUE.")
  }

  if (is.null(data)) {
    stop("data needs to be specified")
  }

  if (length(var) != 1) {
    stop("length(var) == 1 required.")
  }

  n <- switch(operator,
              equal     = data[, var] == value,
              match     = data[, var] %in% value,
              less      = data[, var] < value,
              greater   = data[, var] > value,
              lesseq    = data[, var] <= value,
              greatereq = data[, var] >= value, 
              is.na     = is.na(data[, var]),
              isnot.na  = !is.na(data[, var]),
              stop("unknown operator")) 

  N <- sum(n, na.rm = na.rm)

  if (na.rm) {
    p <- N / length(na.omit(n))
  } else {
    p <- N / length(n)
  }

  if (show.n & show.percent) {
    paste(frmt(N, digits = 0, big.mark = big.mark), " (", frmt(p*100, digits), "\\%)", sep = "")
  } else if(show.n & !show.percent) 
  {
    paste(frmt(N, digits = 0, big.mark = big.mark))
  } else if (!show.n & show.percent) {
    paste(frmt(p*100, digits), "\\%", sep = "")
  } 
}

