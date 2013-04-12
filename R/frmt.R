frmt <-
function(x, digits = getOption("qwraps.digits", 3), big.mark = ","){
    idx <- is.na(x)
    y <- formatC(x, digits = digits, format = "f", big.mark = big.mark)
    y[idx] <- ""
    return(y)
}
