#'generic call for reporting the parameter estiamtes from different regression
#'models
#'
#'Parameter estiamtes, confidence intervals, and p-values form regression
#'models.  Results are presented in three forms, numeric matrix, character
#' matrix, and individual strings.  The character matrix and strings are
#' intended to be used when knitting with LaTeX.
#'
#'%% ~~ If necessary, more details than the description above ~~
#'
#'@param fit a lm, glm, coxph, or survfit object
#'@param alpha significance level, 100(1-alpha)% CIs will be generated
#'@param \dots arguments to pass to params_frmtr as noted in the following
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
#'@author Peter DeWitt
#'@seealso \code{\link{params_frmtr}}
#'@keywords regression results
#'@examples
#' fit <- lm(mpg ~ wt + cyl, data = mtcars)
#' params(fit)
#' params(fit, param = "wt")
#'
#' ## logisitic regression
#' fit <- glm(I(mgp > 25) ~ wt + cyl, data = mtcars, 
#'            family = binomial(link = "logit"))
#' # log odds
#' params(fit)
#' # odds ratios
#' params(fit, fun = exp)
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



