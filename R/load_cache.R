#'load cache
#'
#' Lazy load the objects in a cache directory.  This is helpful for those
#' knitting projects.
#'
#'@param path the direct, or realtive, path to the cache director to lazyLoad
#'@return Nothing, but this call has lazyLoad-ed all the objects in \code{path}
#'@author Peter DeWitt
#'
#'@export load_cache
load_cache <-
function(path = "cache/")
{
  # cwd <- getwd()
  fs <- grep(".rdx", dir(path), value = TRUE)
  fs <- strsplit(fs, ".rdx", fixed = TRUE)
  # setwd(path)
  cat("Loading: ")
  for(f in fs) {
    print(f)
    lazyLoad(paste(path, f, sep = ""), envir = parent.frame(n = 2))
  }
  # setwd(cwd)
}




