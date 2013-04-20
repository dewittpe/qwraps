#'parameter estiamtes from linear regression models
#'
#'%% ~~ A concise (1-5 lines) description of what the function does. ~~
#'
#'%% ~~ If necessary, more details than the description above ~~
#'
#'@param fit
#'@param alpha
#'@param \dots
#'@return ?
#'@note ?
#'@author Peter DeWitt
#'@seealso ?
#'@references ?
#'@keywords ~kwd1 ~kwd2
#'@examples
#'
#'##---- Should be DIRECTLY executable !! ----
#'##-- ==>  Define data, use random,
#'##--	or do  help(data=index)  for the standard data sets.
#'
#'## The function is currently defined as
#'function (fit, alpha = getOption("qwraps.alpha", 0.05), ...) 
#'{
#'    rtn <- summary(fit)$coef
#'    rtn[, 3] <- rtn[, 1] + qt(1 - alpha/2, df.residual(fit)) * 
#'        rtn[, 2]
#'    rtn[, 2] <- rtn[, 1] + qt(alpha/2, df.residual(fit)) * rtn[, 
#'        2]
#'    return(params_frmtr(rtn, ...))
#'  }
#'
params.lm <-
function(fit, alpha = getOption("qwraps.alpha", 0.05), ...)
{
  rtn <- summary(fit)$coef
  rtn[, 3] <- rtn[, 1] + qt(1 - alpha / 2, df.residual(fit)) * rtn[, 2]
  rtn[, 2] <- rtn[, 1] + qt(alpha / 2, df.residual(fit)) * rtn[, 2]

  return(params_frmtr(rtn, ...))
}
