#'generic call for reporting the parameter estiamtes from different regression
#'models
#'
#'Parameter estiamtes, confidence intervals, and p-values form regression
#`models.
#'
#'%% ~~ If necessary, more details than the description above ~~
#'
#'@param fit a lm, glm, coxph, or survfit object
#'@param alpha significance level, 100(1-alpha)% CIs will be generated
#'@param \dots
#'@author Peter DeWitt
#'@keywords regression results
#'@examples
#' ## TO BE WRITTEN
#'
#' @rdname params
#' @export params
params <-
function(fit, alpha = getOption("qwraps.alpha", 0.05), ...){
  UseMethod("params")
}

#' @rdname params
#' @method params coxph
#' @S3method params coxph
params.coxph <-
function(fit, alpha = getOption("qwraps.alpha", 0.05), ...)
{
  rtn <- matrix(summary(fit)$coef[, -2], ncol = 4)
  rtn[, 3] <- rtn[, 1] + qnorm(1 - alpha / 2) * rtn[, 2]
  rtn[, 2] <- rtn[, 1] + qnorm(alpha / 2) * rtn[, 2]

  return(params_frmtr(rtn, ...))
}

#' @rdname params
#' @method params glm
#' @S3method params glm 
params.glm <-
function(fit, alpha = getOption("qwraps.alpha", 0.05), ...)
{
  rtn <- summary(fit)$coef
  rtn[, 3] <- rtn[, 1] + qt(1 - alpha / 2, df.residual(fit)) * rtn[, 2]
  rtn[, 2] <- rtn[, 1] + qt(alpha / 2, df.residual(fit)) * rtn[, 2]

  return(params_frmtr(rtn, ...))
}

#' @rdname params
#' @method params lm
#' @S3method params lm 
params.lm <-
function(fit, alpha = getOption("qwraps.alpha", 0.05), ...)
{
  rtn <- summary(fit)$coef
  rtn[, 3] <- rtn[, 1] + qt(1 - alpha / 2, df.residual(fit)) * rtn[, 2]
  rtn[, 2] <- rtn[, 1] + qt(alpha / 2, df.residual(fit)) * rtn[, 2]

  return(params_frmtr(rtn, ...))
}

#' @rdname params
#' @method params survfit
#' @S3method params survfit
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



