#'KM plots
#'
#'%% Creat Kaplam-Meier plots via ggplot2
#'
#'%% ~~ If necessary, more details than the description above ~~
#'
#'@param fit
#'@param show.ci 
#'@param l.name
#'@param labels
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
#'function (fit, l.name = NULL, labels = NULL) 
#'{
#'    n <- length(names(fit$strata))
#'    plot.data <- data.frame(time = c(fit$time, rep(0, n)), n.risk = c(fit$n.risk, 
#'        rep(NA, n)), n.event = c(fit$n.event, rep(NA, n)), n.censor = c(fit$n.censor, 
#'        rep(0, n)), surv = c(fit$surv, rep(1, n)), strata = c(summary(fit, 
#'        censored = TRUE)$strata, 1:n), upper = c(fit$upper, rep(1, 
#'        n)), lower = c(fit$lower, rep(1, n)))
#'    plot.data$strata <- factor(plot.data$strata, levels = 1:n, 
#'        labels = names(fit$strata))
#'    if (is.null(l.name)) {
#'        name <- "Strata"
#'    }
#'    else {
#'        name <- l.name
#'    }
#'    q <- ggplot(plot.data, aes(x = time, y = surv, colour = strata)) + 
#'        geom_step() + ylim(c(0, 1)) + geom_point(data = subset(plot.data, 
#'        n.censor > 0), shape = 3, alpha = 0.9) + if (is.null(labels)) {
#'        scale_colour_hue(name = name, breaks = levels(plot.data$strata))
#'    }
#'    else {
#'        scale_colour_hue(name = name, breaks = levels(plot.data$strata), 
#'            labels = labels)
#'    }
#'    return(q + theme(legend.position = "bottom", legend.direction = "horizontal"))
#'  }
#'
qsurvplot <-
function(fit, show.ci = getOption("qwraps.show.ci", TRUE), 
         l.name = NULL, labels = NULL){

  n <- length(names(fit$strata))

  plot.data <- data.frame(
    time     = c(fit$time, rep(0, n)),
    n.risk   = c(fit$n.risk, rep(NA, n)),
    n.event  = c(fit$n.event, rep(NA, n)),
    n.censor = c(fit$n.censor, rep(0, n)),
    surv     = c(fit$surv, rep(1, n)),
    strata   = c(summary(fit, censored = TRUE)$strata, 1:n), 
    upper    = c(fit$upper, rep(1, n)),
    lower    = c(fit$lower, rep(1, n))) 

  plot.data$strata <- factor(plot.data$strata,
                             levels = 1:n,
                             labels = names(fit$strata))               

  if(is.null(l.name)){
    name <- "Strata"
  }else{
    name <- l.name
  }

  g <- ggplot(plot.data, 
    aes(x = time, y = surv, colour = strata)) + 
    geom_step() + ylim(c(0, 1)) + 
    geom_point(data = subset(plot.data, n.censor > 0), shape = 3, alpha = 0.9) + 
  if (is.null(labels)){
	  scale_colour_hue(name = name, breaks = levels(plot.data$strata))
	}else{
	  scale_colour_hue(name = name, breaks = levels(plot.data$strata),
	    labels = labels)
	}

  if (show.ci) {
    g <- g + geom_ribbon(aes(ymin = lower, ymax = upper, fill = strata), 
                         alpha = 0.5)
  }

  return(g + theme(legend.position = "bottom", legend.direction = "horizontal"))
}
