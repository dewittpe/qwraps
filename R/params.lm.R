params.lm <-
function(fit, alpha = getOption("qwraps.alpha", 0.05), ...)
{
  rtn <- summary(fit)$coef
  rtn[, 3] <- rtn[, 1] + qt(1 - alpha / 2, df.residual(fit)) * rtn[, 2]
  rtn[, 2] <- rtn[, 1] + qt(alpha / 2, df.residual(fit)) * rtn[, 2]

  return(params_frmtr(rtn, ...))
}
