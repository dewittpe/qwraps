params <-
function(fit, alpha = getOption("qwraps.alpha", 0.05), ...){
  UseMethod("params")
}
