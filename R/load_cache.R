#'load cache
#'
#'%% ~~ A concise (1-5 lines) description of what the function does. ~~
#'
#'%% ~~ If necessary, more details than the description above ~~
#'
#'@param dir
#'@return ?
#'@note ?
#'@author Peter DeWitt
#'@seealso ?
#'@references ?
#'@examples ?
#'

load_cache <-
function(path = "cache/")
{
  cwd <- getwd()
  fs <- grep(".rdx", dir(path), value = TRUE)
  fs <- strsplit(fs, ".rdx", fixed = TRUE)
  setwd(path)
  for(f in fs) {
    cat(f, "\n")
    lazyLoad(f, envir = parent.frame(n = 2))
  }
  setwd(cwd)
}




