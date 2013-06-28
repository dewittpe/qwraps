qwraps
======

Wrapper functions for producing base R plots using `ggplot2` 
https://github.com/hadley/ggplot2 .  Included functions
are `qacf()`, `qroc()`, `qsurvplot()`, with more to come.

Other wrapper functions include `params` which gives point estimators,
confidence intervals, and p-values from different types of regression models and
allows for (back)transforms of the coefficients.  

May of the functions are particularly useful for writing data analysis reports
via `knitr` http://yihui.name/knitr/ and LaTeX.


## Development

To install the development version of qwraps, it's easiest to use the
`devtools`, https://github.com/hadley/devtools, package:

    # install.packages("devtools")
    library(devtools)
    install_github("qwraps", username = "dewittpe")
