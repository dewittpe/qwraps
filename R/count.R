count <-
function(var, val, data, equal.or.in = "equal",
                  show.n = TRUE,
                  show.percent = TRUE,
                  digits = getOption("qwraps.percent.digits", 1),
                  big.mark = ",",
                  latex  = getOption("qwraps.latex", TRUE))
{
  if (equal.or.in == "equal")
  {
    n <- sum(data[, var] == val)
    p <- n / sum(!is.na(data[, var]))
  }
  else
  {
    n <- sum(data[, var] %in% val) 
    p <- n / dim(data)[1]
  }
  paste(frmt(n, digits = 0, big.mark = big.mark),  " (", frmt(p*100, digits), "\\%) ", sep = "")
}
