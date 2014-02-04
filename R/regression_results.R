#'Function for generating a nice, human readable, summary table of a regression
#'model.
#'
#'produce a list object with a matrix and labels for printing a latex version of
#'the table via print.regression.results()
#'
#'
#'@param fit a lm, glm, coxph, or survfit object
#'@param \dots arguments to pass to params()
#'@author Peter DeWitt
#'@seealso \code{\link{params}}
#'@keywords regression results
#'@examples
#' ## TO DO
#'
#' @rdname regression_results
regression_results <-
function(fit, ...){
  UseMethod("regression_results")
}

#' @rdname regression_results
#' @method regression_results coxph
#' @S3method regression_results coxph
regression_results.coxph <-
function(fit,  ...)
{
  stop("regression_results.coxph is not yet implemented")
}

#' @rdname regression_results
#' @method regression_results lm
#' @S3method regression_results lm 
regression_results.lm <-
function(fit, ...)
{
  # fit <- glm(tms.change ~ unit + age + primaryd, data = music.therapy, family = binomial())
  # str(fit, max.level = 1)
  # fit$xlevels
  dat <- droplevels(fit$data)

  x <- attr(fit$terms, "term.labels")
  rgrp <- do.call(c, lapply(x, 
                            function(x) { 
                              if (is.factor(dat[, x])) {
                                nlevels(dat[, x]) 
                              } else { 
                                1
                            }}))
  names(rgrp) <- x
  rwnm <- do.call(c, lapply(x, 
                            function(x) { 
                              if (is.factor(dat[, x])) {
                                levels(dat[, x]) 
                              } else { 
                                x
                            }}))
  rwnm2 <- do.call(c, lapply(x, 
                            function(x) { 
                              if (is.factor(dat[, x])) {
                                paste0(x, levels(dat[, x]))
                              } else { 
                                x
                            }}))
  reg.out <- matrix(NA, ncol = 4, nrow = length(rwnm2))
  rownames(reg.out) <- rwnm2


  # params
  p <- params(fit, ...)

  reg.out[rwnm2 %in% names(coef(fit)), ] <- 
    as.matrix(p$tab.frmt)[rownames(p$tab.frmt) %in% rwnm2, ]

  reg.out[is.na(reg.out[, 1]), 1] <- "Reference"
  reg.out[is.na(reg.out)]         <- ""
  rownames(reg.out)               <- rwnm
  colnames(reg.out)               <- c("Estimate", "lcl", "ucl", "p-value")

  out <- list(tab = reg.out,
              rgrp = rgrp,
              rwnm = rwnm)
  class(out) <- "regression_results"

  return(out)
}

#' @rdname regression_results
#' @method regression_results survfit
#' @S3method regression_results survfit
regression_results.survfit <-
function(fit, alpha = getOption("qwraps.alpha", 0.05), ...)
{
  stop("regression_results.survfit not yet implemented")
}



#' @rdname regression_results
#' @method print regression_results
#' @S3method print regression_results
print.regression_results <- function(tab, 
                           file = "", 
                           title = "",
                           ctable = getOption("qwraps.regression_results.ctable", TRUE),
                           # cgroup,
                           # n.cgroup,
                           rgroup,
                           n.rgroup,
                           rowname,
                           col.just, 
                           ...) {
latex(tab[["tab"]],
      file     = file,
      title    = title,
      ctable   = ctable,
      # cgroup   = if (missing(cgroup))   tab[['cgrp']] else  cgroup, 
      # n.cgroup = if (missing(n.cgroup)) tab[['ncgrp']] else n.cgroup, 
      rgroup   = if (missing(rgroup))   names(tab[['rgrp']]) else  rgroup, 
      n.rgroup = if (missing(n.rgroup)) tab[['rgrp']] else n.rgroup, 
      rowname  = if (missing(rowname))  tab[['rwnm']] else  rowname, 
      col.just = if (missing(col.just)) rep("r", ncol(tab[['tab']])) else col.just, 
      ...) 
}

