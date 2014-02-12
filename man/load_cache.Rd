\name{load_cache}
\alias{load_cache}
\title{load cache}
\usage{
load_cache(path = "cache/")
}
\arguments{
  \item{path}{the direct, or realtive, path to the cache
  director to lazyLoad}
}
\value{
Nothing, but this call has lazyLoad-ed all the objects in
\code{path}
}
\description{
Lazy load the objects in a cache directory.  This is
helpful for those knitting projects.
}
\author{
Peter DeWitt
}

