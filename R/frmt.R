#'format numbers with a set number of digits, including trailing zeros.
#'
#'This function is a wrapper for a call to formatC.
#'
#'Used to format numbers by both the end user and by several of the other
#'functions in the qwraps package.
#'
#'@param x vector of numbers to be formated.
#'@param digits digits to follow the decimal mark, including trailing zeros.
#'@param big.mark Default is a comma to separate, i.e., 1000 would be reported
#'as 1,000.
#'@return a string.
#'@note %% ~~further notes~~
#'@author Peter DeWitt
#'@seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#'@references %% ~put references to the literature/web site here ~
#'@keywords ~format ~trailing
#'@examples
#'
#'  x <- c(1.12, 1.231, 4.338, 10.4, 1.1)
#'  frmt(x, 2)
#'
frmt <-
function(x, digits = getOption("qwraps.digits", 3), big.mark = ","){
    idx <- is.na(x)
    y <- formatC(x, digits = digits, format = "f", big.mark = big.mark)
    y[idx] <- ""
    return(y)
}
