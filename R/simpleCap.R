
simpleCap <- function(x) {
  s <- gsub(".", " ", x, fixed = TRUE)
  s <- strsplit(s, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}
