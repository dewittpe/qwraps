qwraps
======

Wrapper functions for producing base R plots using ggplot2.  Included functions are `qacf()`, `qroc()`, `qsurvfit()`, with more to come.

Other wrapper functions include `params` which gives point estimators,
confidence intervals, and p-values from different types of regression models and
allows for (back)transforms of the coefficients.  

May of the functions are particularly useful for writing data analysis reports
via knitr and LaTeX.


## Development

To install the development version of qwraps, it's easiest to use the `devtools` package:

    # install.packages("devtools")
    library(devtools)
    install_github("qwraps", username = "dewittpe")
