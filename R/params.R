#'generic call for reporting the parameter estiamtes from different regression
#'models
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
#'    UseMethod("params")
#'  }
#'
params <-
function(fit, alpha = getOption("qwraps.alpha", 0.05), ...){
  UseMethod("params")
}
