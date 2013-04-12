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
                                               conf.level, "level"), 
                                  breaks = 0:1, 
                                  labels = c("False", "True")) 
    }
  }
  return(rtn)
}
