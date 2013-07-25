#'Trim White Space from strings
#'
#'Leading and trailing white space is removed from strings.
#'
#'@param x string 
#'@return sting with no leading or trailing white space.
#'@author Peter DeWitt
#'@examples
#'
#'s <- c("   this is a string", "what about the end     "), "  or both?  ")
#'trimWhite(s)
#'
#' @export trimWhite
trimWhite <- function(x) {
  s <- gsub("^[ \t]+", "", x)
  s <- gsub("[ \t]+$", "", s)
  return(s)
}


