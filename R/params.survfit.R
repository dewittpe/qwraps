#'parameter estimates from survfit
#'
#'%% ~~ A concise (1-5 lines) description of what the function does. ~~
#'
#'%% ~~ If necessary, more details than the description above ~~
#'
#'@param fit %% ~~Describe \code{fit} here~~
#'@param alpha %% ~~Describe \code{alpha} here~~
#'@param \dots %% ~~Describe \code{\dots} here~~
#'@return %% ~Describe the value returned %% If it is a LIST, use %%
#'\item{comp1 }{Description of 'comp1'} %% \item{comp2 }{Description of
#''comp2'} %% ...
#'@note %% ~~further notes~~
#'@author %% ~~who you are~~
#'@seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#'@references %% ~put references to the literature/web site here ~
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
#'    if (1 - alpha != fit$conf.int) {
#'        warning("Refitting survfit with requested confidence level")
#'        fit <- update(fit, conf.int = 1 - alpha)
#'    }
#'    rtn <- summary(fit)$table[, c(5:7, 7)]
#'    rtn[, 4] <- NA
#'    show.pval <- FALSE
#'    return(params_frmtr(rtn, ...))
#'  }
#'
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
