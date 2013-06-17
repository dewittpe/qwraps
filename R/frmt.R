#'format numbers with a set number of digits, including trailing zeros.
#'
#'This function is a wrapper for a call to formatC.
#'
#'Used to format numbers by both the end user and by several of the other
#'functions in the qwraps package.  frmt is intended for any numeric response
#'frmtp for formating p-values.
#'
#'@param x vector of numbers to be formated.
#'@param digits digits to follow the decimal mark, including trailing zeros.
#'@param big.mark Default is a comma to separate, i.e., 1000 would be reported
#'as 1,000.
#'@param pdigits number of digits to report p-values to.  
#'@param show.equal.sign
#'@return a string of formated numbers.  Non-numbers are empty strings.  
#' If the p-value is less than 10^(-pdigits) the return will be '< 10^(-pdigits) 
#'@author Peter DeWitt
#'@keywords ~format ~trailing
#'@examples
#'
#' x <- c(1.12, 1.231, 4.338, 10.4, 1.1, NA, pi, NaN, exp(1))
#' frmt(x, 2)
#' frmt(x, 3)
#' frmt(x*100, 3)
#' 
#' pvals <- c(0.000001, 0.12, 0.0001)
#' frmtp(pvals)

#' @rdname frmt
#' @export frmt
frmt <-
function(x, digits = getOption("qwraps.digits", 3), big.mark = ","){
    idx <- is.na(x)
    y <- formatC(x, digits = digits, format = "f", big.mark = big.mark)
    y[idx] <- ""
    return(y)
}

#' @rdname frmt
#' @export frmtp
frmtp <-
function(x, pdigits = getOption("qwraps.pdigits", 4), equal.sign = FALSE)
{
  out <- vector("character", length(x))
  out <- formatC(x, pdigits, format = "f")

  idx <- (x < 0.1^pdigits)
  out[idx] <- paste("\\textless", formatC(0.1^pdigits, pdigits, format = "f"))

  if (equal.sign) out[!idx] <- paste("=", out[!idx])

  out[is.na(x)] <- ""
  return(out)
}
