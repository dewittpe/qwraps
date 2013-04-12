params_frmtr <-
function(rtn,          # object coming from params.xxx
                         param   = NULL, 
                         digits  = getOption("qwraps.digits", 3),
                         pdigits = getOption("qwraps.pdigits", 4),
                         show.ci = getOption("qwraps.show.ci", TRUE),
                         show.pval = getOption("qwraps.show.pval", TRUE),
                         alpha   = getOption("qwraps.alpha", 0.05),
                         fun     = NULL,
                         equal.sign = FALSE,
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
 
  rtn.frmt[, 4] <- frmtp(rtn[, 4], pdigits, equal.sign = FALSE)

  if (show.ci && show.pval){
    rtn.strings <- paste(rtn.frmt[, 1], unit, 
                         " (", (1-alpha)*100, "\\% CI: ",
                         rtn.frmt[, 2], ", ", rtn.frmt[, 3], "; p ",
                         frmtp(rtn[, 4], pdigits, equal.sign = TRUE), ")", 
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
                         frmtp(rtn[, 4], pdigits, equal.sign = TRUE), ")", 
                         sep = "")
  }
  else if(!show.ci && !show.pval){
    rtn.strings <- paste(rtn.frmt[, 1], unit, sep = "")
  }

  names(rtn.strings) <- rownames(rtn)

  return(list(tab = rtn, tab.frmt = rtn.frmt, strings = rtn.strings))
}
