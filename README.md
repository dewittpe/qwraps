qwraps
======

Wrapper functions for producing base R plots using ggplot2.  Included functions are qacf(), qroc(), qsurvfit(), with more to come.

This is a very rough package as is.  A lot of work is left to do. 

## TODOs

General TODOs

1) help files

2) examples

3) code clean

Additional functionality:
 
1) In the count.R function add the ability to work with non factor variables.
Specifically for something along the lines of: count("price", "1000", data =
diamonds, operator = "&lt") which would return the count and percentage of
records with a price less than 1,000.


## Development

To install the development version of qwraps, it's easiest to use the `devtools` package:

    # install.packages("devtools")
    library(devtools)
    install_github("qwraps", username = "dewittpe")
