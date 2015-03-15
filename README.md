# qwraps

So, I don't even use this stuff very much any more.  Most of the tasks for
summarizing data can be achieved with ease by using
[`dplyr`](https://github.com/hadley/dplyr) and
[`tidyr`](https://github.com/hadley/tidyr).  Some of the functions for formating
strings and what not are still helpful, but may not be stream lined with the
paradigms of the Hadley-verse.  

Graphics are still useful, but this package needs to be deprecated and the
contents redesigned.

---

Wrapper functions for producing base R plots using `ggplot2` 
https://github.com/hadley/ggplot2 .  Included functions
are `qacf()`, `qroc()`, `qsurvplot()`, with more to come.

Other wrapper functions include `params` which gives point estimators,
confidence intervals, and p-values from different types of regression models and
allows for (back)transforms of the coefficients.  

The function `tableone` is being developed to allow for quick summary tables for
a dataset overall, and by a grouping variable.  

I'm hoping to get a more and more people to start to use this package, find
errors, and suggest enhancements.

May of the functions are particularly useful for writing data analysis reports
via `knitr` http://yihui.name/knitr/ and LaTeX.

## How to get qwraps in R

To install the development version of qwraps, it's easiest to use the
`devtools`, https://github.com/hadley/devtools, package:

    # install.packages("devtools")
    library(devtools)
    install_github("qwraps", username = "dewittpe")

If you are working on a Windows machine you will need to download and install
`Rtools` http://cran.r-project.org/bin/windows/Rtools/ before `devtools` will
work for you.


