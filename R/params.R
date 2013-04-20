#'generic call for reporting the parameter estiamtes from different regression
#'models
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
#'    UseMethod("params")
#'  }
#'
params <-
function(fit, alpha = getOption("qwraps.alpha", 0.05), ...){
  UseMethod("params")
}
