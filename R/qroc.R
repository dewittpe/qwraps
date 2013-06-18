#'ROC plots in ggplot
#'
#' plot a ROC in ggplot for a given logistic regression fit.
#'
#'%% ~~ If necessary, more details than the description above ~~
#'
#'@param a glm object from a family = binomial
#'@param newdata if omitted the data used to fit the model is used, else this
#' newdata is passed to the \code{predict} function and the resulting ROC plot
#' is based on this data.
#'@param show.auc show the area under the curve on the plot via text.
#'@param n number of thresholds to use, defualt is 200
#'@return 
#' a list with the plot, the auc, and the data.frame used to plot the ROC
#'@author Peter DeWitt
#'@keywords ROC
#'@examples
#'
#' fit <- glm(I(mpg > 15) ~ wt + cyl, data = mtcars,
#'            family = binomial(link = "logit"))
#' roc <- qrocc(fit)
#' roc$auc
#' roc$plot
#'
#' @export qroc
qroc <-
function(fit, newdata = NULL, show.auc = FALSE, n = 200){
    require(ggplot2)

    pred.vals <- predict(fit, newdata, type = "response")

    true.pos <- function(threshold){sum((pred.vals >= threshold) & (fit$y))}
    true.neg <- function(threshold){sum((pred.vals <  threshold) & !(fit$y))}

    false.pos <- function(threshold){sum((pred.vals >= threshold) & !(fit$y))}
    false.neg <- function(threshold){sum((pred.vals <  threshold) & (fit$y))}

    # n <- 200 # number of thresholds to check
    x <- matrix(seq(1, 0, length = n))
  
    true.positives  <- apply(x, 1, true.pos)
    true.negatives  <- apply(x, 1, true.neg)
    false.positives <- apply(x, 1, false.pos)
    false.negatives <- apply(x, 1, false.neg)

    sensitivity <- true.positives / (true.positives + false.negatives)
    specificity <- true.negatives / (true.negatives + false.positives)

    df <- data.frame(false.positives = 1 - specificity, 
                     true.positives  = sensitivity)

    auc <- sum((df[2:n,1] - df[1:(n-1),1]) * 1/2 * (df[2:n,2] + df[1:(n-1),2]))

    q <- ggplot(data = df, aes(x = false.positives, y = true.positives)) + 
           geom_line(size = 1.25) + #geom_point() + 
           geom_segment(aes(x = 0, y = 0), 
			xend = 1, yend = 1, 
			colour = "blue", lty = 2) +
           xlab("1-Specificity") + ylab("Sensitivity")
  
    if (show.auc) {
      q <- q + geom_text(aes(x = 0.8, y = 0.0), 
                         hjust = 0, vjust = 0, 
                         label = paste("AUC:", formatC(auc, digits = 4, format = "f")))
    } 

    return(list(auc = auc, data = df, plot = q) )
}
