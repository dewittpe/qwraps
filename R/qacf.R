#'Auto correlation plot in ggplot
#'
#'Reproduce the stats::acf plots from the base R graphics package in ggplot2.
#'
#'%% ~~ If necessary, more details than the description above ~~
#'
#'@param x vector, or data.frame, to be plotted.
#'@param conf.level confidence intervals for determining 'significant'
#'autocorrelations.
#'@param lag.max how many lags to present.  default is the same as the
#'stats::acf plot
#'@param type same options as stats::acf, either a correlation (default),
#'covariance, or partial correlation plot
#'@param show.sig Extension to the stats::acf function.  If TRUE the lags are
#'colored to indicate statistically significant correlations different from
#'zero.  This option is only used for the corrleation plot.
#'@return a ggplot object
#'@note %% ~~further notes~~
#'@author Peter DeWitt
#'@seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#'@references %% ~put references to the literature/web site here ~
#'@keywords ~kwd1 ~kwd2
#'@examples
#'
#'# Generate a random data set
#'set.seed(42)
#'n <- 250
#'x1 <- x2 <- x3 <- x4 <- vector('numeric', length = n)
#'x1[1] <- runif(1)
#'x2[1] <- runif(1)
#'x3[1] <- runif(1)
#'x4[1] <- runif(1)
#'
#'# white noise
#'Z.1 <- rnorm(n, 0, 1)
#'Z.2 <- rnorm(n, 0, 2)
#'Z.3 <- rnorm(n, 0, 5)
#'
#'for(i in 2:n)
#'{
#'	x1[i] <- x1[i-1] + Z.1[i] - Z.1[i-1] + x4[i-1] - x2[i-1]
#'	x2[i] <- x2[i-1] - 2 * Z.2[i] + Z.2[i-1] - x4[i-1]
#'	x3[i] <- x3[i-1] + x2[i-1] + 0.2 * Z.3[i] + Z.3[i-1]
#'	x4[i] <- x4[i-1] + runif(1, 0.5, 1.5) * x4[i-1]
#'}
#'testdf <- data.frame(x1, x2, x3, x4)
#'
#'# Base acf plot for one variable
#'acf(testdf$x1)
#'
#'# qacf plot for one variable
#'qacf(testdf$x1)
#'
#'# more than one variable
#'acf(testdf)
#'qacf(testdf)
#'
#' @export qacf
qacf <-
function(x, 
                 conf.level = 0.95, 
                 lag.max = NULL, 
                 type = c("correlation", "covariance", "partial"), 
                 show.sig = FALSE)
                 
{
  require(reshape2)
  require(ggplot2)

  series <- deparse(substitute(x))

  x <- as.data.frame(x)

  bacf <- acf(x, plot = FALSE, lag.max = lag.max, type = type)

  ciline <- qnorm((1-conf.level)/2) / sqrt(with(bacf, n.used))

  bacfsnames <- with(bacf, snames)

  bacfacf <- as.data.frame(with(bacf, acf)) 
  bacflag <- as.data.frame(with(bacf, lag))
  bacfdf <- cbind(melt(bacflag, id.var = NULL), 
                  melt(bacfacf, id.var = NULL)[,2])
  significant <- as.numeric(abs(bacfdf[,3]) > abs(ciline))
  bacfdf <- cbind(bacfdf, significant)

  if (dim(x)[2] > 1)
  {
    vars <- length(bacfsnames)  
    lags <- dim(bacflag)[1]
    column <- row <- c()

    for(i in 1:vars){
      row <- c(row, rep(bacfsnames[i], lags*vars))
      for(j in 1:vars){
        column <- c(column, rep(bacfsnames[j], lags))
      }
    }
    row <- factor(row, levels = bacfsnames)
    column <- factor(column, levels = bacfsnames)
    bacfdf <- cbind(bacfdf, row, column)
    names(bacfdf) <- c("plot", "lag", "acf", "significant", "row", "column")
  }
  else
  {
    names(bacfdf) <- c("plot", "lag", "acf", "significant")
  }

  rtn <- ggplot(data = bacfdf, aes(x = lag, y = acf)) + 
           geom_bar(stat="identity", position="identity") + 
           ylab(with(bacfdf, type))

  if (dim(x)[2] > 1)
  {
    rtn <- rtn + facet_wrap(column ~ row, scales = "free_x")
  }


  if(with(bacf,type) %in% c("correlation", "partial")){
    rtn <- rtn + geom_hline(yintercept = -ciline, color="blue", size = 0.2)  
    rtn <- rtn + geom_hline(yintercept = ciline,  color="blue", size = 0.2) 
    rtn <- rtn + geom_hline(yintercept = 0,       color="red",  size = 0.3) 

    if (show.sig)
    { 
      rtn <- rtn + aes(fill = factor(significant))
      rtn <- rtn + scale_fill_hue(name = paste("Significant at the\n", 
                                               1 - conf.level, "level"), 
                                  breaks = 0:1, 
                                  labels = c("False", "True")) 
    }
  }
  return(rtn)
}
