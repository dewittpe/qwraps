params.survfit <-
function(fit, alpha = getOption("qwraps.alpha", 0.05), ...)
{
  if (1 - alpha != fit$conf.int){
    warning("Refitting survfit with requested confidence level")
    fit <- update(fit, conf.int = 1 - alpha)
  }
  rtn <- summary(fit)$table[, c(5:7, 7)]
  rtn[, 4] <- NA   # needed only for correct dims in params.frmt
  show.pval <- FALSE

  return(params_frmtr(rtn, ...))
}
