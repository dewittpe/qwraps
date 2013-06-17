#'Simple Capitalization
#'
#'Remove a "." from a string and capitalize the first letter of every word.
#'
#'%% ~~ If necessary, more details than the description above ~~
#'
#'@param x string to edit
#'@return The requested reformatted string.
#'@note Slight modification of the function noted in the exampleof the chartr
#'base help file.
#'@author Peter DeWitt
#'@seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#'@references %% ~put references to the literature/web site here ~
#'@keywords ~kwd1 ~kwd2
#'@examples
#'
#'s <- "this.is.a.string"
#'simpleCap(s)
#'
#' @export simpleCap
simpleCap <- function(x) {
  s <- gsub(".", " ", x, fixed = TRUE)
  s <- strsplit(s, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}
