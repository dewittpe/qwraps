frmtp <-
function(x, 
                  pdigits = getOption("qwraps.pdigits", 4), 
                  equal.sign = FALSE)
{
  out <- vector("character", length(x))
  out <- formatC(x, pdigits, format = "f")

  idx <- (x < 0.1^pdigits)
  out[idx] <- paste("\\textless", formatC(0.1^pdigits, pdigits, format = "f"))

  if (equal.sign) out[!idx] <- paste("=", out[!idx])

  out[is.na(x)] <- ""
  return(out)
}
