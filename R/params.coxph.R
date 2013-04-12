params.coxph <-
function(fit, alpha = getOption("qwraps.alpha", 0.05), ...)
{
  rtn <- summary(fit)$coef[, -2]
  rtn[, 3] <- rtn[, 1] + qnorm(1 - alpha / 2) * rtn[, 2]
  rtn[, 2] <- rtn[, 1] + qnorm(alpha / 2) * rtn[, 2]

  return(params_frmtr(rtn, ...))
}
