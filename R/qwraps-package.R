#'@exportPattern "^[[:alpha:]]+"
NULL

#'Quick Wraps
#'
#'Wrapper functions for producing common R base plots not explicitly defined in
#'the ggplot2 package such as ACF and ROC plots.  Other wrapper functions for
#'formating results and extracting parameter estimates to be used in
#'Sweaved/Knited reports.
#'
#'\tabular{ll}{ Package: \tab qwraps\cr Type: \tab Package\cr Version: \tab
#'1.0\cr Date: \tab 2013-04-12\cr License: \tab GPL >=2\cr } ~~ An overview of
#'how to use the package, including the most important ~~ ~~ functions ~~
#'
#'@name qwraps-package
#'@aliases qwraps-package qwraps
#'@docType package
#'@author Peter DeWitt
#'
#'Maintainer: Peter DeWitt <dewittpe@@gmail.com> 
#'@keywords ACF, ROC, AUC, counts, wraps, 
#'@examples 
#'
#'library(ggplot2)
#'library(qwraps)
#'
#'# count of, and percentage, of differnt levels of a factor
#'count(var = "cut", val = "Fair", data = diamonds)
#'count(var = "cut", val = c("Fair", "Good"), data = diamonds, equal.or.in = "in")
#'
#'# parameter estimates from a regression model
#'fit <- lm(price ~ cut + table, data = diamonds)
#'params(fit)
#'
#'# Example of the qacf function. First generate an AR(1) sequence
#'set.seed(42)
#'eg.dat <- vector("numeric", 500)
#'eg.dat[1] <- 0
#'for(i in 2:length(eg.dat)) {
#'  eg.dat[i] <- 0.7 * eg.dat[i - 1] + rnorm(1)
#'}
#'qplot(x = seq_along(eg.dat), y = eg.dat, geom = "path")
#'qacf(eg.dat)
#'


