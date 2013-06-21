#'formating helper function, not intended to be called by end users
#'
#'helper function, not intended to be called by end users, for the
#'\code{\link{params}} function.
#'
#'%% ~~ If necessary, more details than the description above ~~
#'
#'@param rtn object from params.xxx
#'@param param if NULL (default) then a full matrix of of all coeffients will be
#'returned.  A character represtation of the parameters of interest can be
#'returned if specified.
#'@param digits number of digits after the decimal point, included trailing
#'zeros, to print numbers to: see \code{\link{frmt}}
#'@param pdigits number of digits to format p-values: see \code{\link{frmtp}}
#'@param show.ci logical, return confidence intervals
#'@param show.pval logical, return the p-values
#'@param alpha significant level, reporting 100(1-alpha)% CIs
#'@param fun funciton for transforming results.  Particularly useful is
#' \code{fun = exp} when working with logisitic regression models, for example.
#'@param show.equal.sign passed to \code{frmtp}
#'@param unit can be added to the strings returned such that the string could be
#' xx mg (95% CI: yy, zz; p-value = 0.pppp) instead of just 
#' xx (95% CI: yy, zz; p-vaue = 0.pppp)
#'@param big.mark passed to frmt 
#'@param small.mark passed to frmt
#'@return 
#' a list with three elements
#'@note ?
#'@author Peter DeWitt
#'@seealso \code{\link{params}} \code{\link{frmt}}
#'@examples
#' ## See \code{\link{params}}  \code{params_frmtr} is not intended to be called
#' by the end user
#'
#' @export 
params_frmtr <- function(rtn, 
                         param   = NULL, 
                         digits  = getOption("qwraps.digits", 3),
                         pdigits = getOption("qwraps.pdigits", 4),
                         show.ci = getOption("qwraps.show.ci", TRUE),
                         show.pval = getOption("qwraps.show.pval", TRUE),
                         alpha   = getOption("qwraps.alpha", 0.05),
                         fun     = NULL,
                         show.equal.sign = FALSE,
                         unit    = "",
                         big.mark = "",
                         small.mark = "")
{
  colnames(rtn) <- c("Estimate", "lwr", "upr", "pvalue")

  if (!is.null(param))
  {
    rtn <- matrix(rtn[rownames(rtn) %in% param, ],
                  nrow = length(param))
  }

  if (!is.null(fun)){
    fun <- match.fun(fun)
    rtn[, 1:3] <- fun(rtn[, 1:3])
  }

  rtn.frmt <- as.data.frame(rtn)
  rtn.frmt[, 1:3] <- formatC(rtn[, 1:3], 
                             digits = digits, 
                             format = "f", 
                             big.mark = big.mark,
                             small.mark = small.mark) 
 
  rtn.frmt[, 4] <- frmtp(rtn[, 4], pdigits, show.equal.sign = FALSE)

  if (show.ci && show.pval){
    rtn.strings <- paste(rtn.frmt[, 1], unit, 
                         " (", (1-alpha)*100, "\\% CI: ",
                         rtn.frmt[, 2], ", ", rtn.frmt[, 3], "; p ",
                         frmtp(rtn[, 4], pdigits, show.equal.sign = TRUE), ")", 
                         sep = "")
  } 
  else if(show.ci && !show.pval){
    rtn.strings <- paste(rtn.frmt[, 1], unit, 
                         " (", (1-alpha)*100, "\\% CI: ",
                         rtn.frmt[, 2], ", ", rtn.frmt[, 3], ")", 
                         sep = "")
  }
  else if(!show.ci && show.pval){
    rtn.strings <- paste(rtn.frmt[, 1], unit, 
                         " (p ",
                         frmtp(rtn[, 4], pdigits, show.equal.sign = TRUE), ")", 
                         sep = "")
  }
  else if(!show.ci && !show.pval){
    rtn.strings <- paste(rtn.frmt[, 1], unit, sep = "")
  }

  names(rtn.strings) <- rownames(rtn)

  return(list(tab = rtn, tab.frmt = rtn.frmt, strings = rtn.strings))
}
