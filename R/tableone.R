#' Tableone summarizes a selection of the data set as would be presented in a
#' table 1 for a manuscript.
#'
#' Summarize variables by a categorical variable within a given data.frame.
#' Overall and by results are presented along with simiple hypothesis tests.
#'
#' EXPLAIN THE DETAILS MORE, FACTORS AND NUMERS ARE IMPORANT, WILL TRANSFORM
#' CHARACTERS TO FACTORS
#'
#'@param vars characer vector of the names of the variables in data to
#' summarize.
#'@param by character name of the variable in data to sumamrize vars by
#'@param data the data.frame
#'@param complete logical reported for the complete data records only?  If TRUE,
#' the dataset will be subseted via \code{na.omit(data[, c(vars, by)])}, if FALSE,
#' then all avaiable data records will be used.
#'@param margin default to report column percentages for categorical variables,
#' can be set to 1 for row percentages or 1:2 for overall percentages.  
#'@param overall logical if TRUE include a column with the summary statistics
#' for the whole sample in addition to the summary of the \code{vars} \code{by}
#'@param stat.con.1 function to summarize continuous variables with, default is
#' the \code{mean}
#'@param stat.con.2 second function to sumamry continuous variables with,
#' adefault is \code{sd} 
#'@param tests a vector of length two with the statistical tests to report for 1
#' categorical variables and 2 continuous variables.  Defaults are for the
#' \code{chisq.test} and the \code{t.test}.  If the value is set to NULL the tests
#' will be omitted from the output.  Only \code{chisq.test} and
#' \code{fisher.test} are allowed for categorical variables at this time.
#' Regardless of the test function specified, the function should return a list
#' value named \code{p.value}.  The second test can be either a t.test or a aov.
#' more tests will be allowed in later versions.
#'@param fisher logicial if TRUE and tests[1] == "chisq.test" then if
#' \code{any(chisq.test()$expected) < 5} the test is redone using a 
#' \code{fisher.exact}. This option is ignored if \code{tests[1] == "fisher.test"}.
#'@param big.mark Default is a comma to separate, i.e., 1000 would be reported
#'as 1,000.
#'@param pdigits number of digits to report p-values to.  
#'@param show.equal.sign
#'@return a list with the elements intended to be used with
#' \code{\link{Hmisc::latex}}
#' \item{tab}{a string of formated numbers.  Non-numbers are empty strings.  
#' If the p-value is less than 10^(-pdigits) the return will be '<
#' 10^(-pdigits)}
#' \item{cgrp}{a character vector for the column groups, the levels of
#' \code{by}}
#' \item{ncgrp}{a numeric vector for n.cgroup}
#' \item{rgrp}{a character vector for the row groups}
#' \item{rcgrp}{a numeric vector for n.cgroup}
#' \item{rwnms}{a character vector of row names}
#'
#'@author Peter DeWitt
#'@keywords ~summary table 
#'@examples
#' data(diamonds, package = "ggplot2")
#' 
#' ## simple examples for the just getting the matrix for table one
#' ## this could be used in xtable, Hmisc::latex, and likely other functions
#' tableone("cut", "color", diamonds[1:50, ])$tab
#' tableone("price", "color", diamonds[1:50, ], 
#'          tests = c("chisq.test", #' "aov"))$tab
#' tableone(c("cut", "price"), "color", diamonds[1:50, ],
#'          tests = c("chisq.test", "aov"))$tab
#'
#' ## A LaTeX table via Hmisc::latex
#' 
#' tab1 <- tableone(c("cut", "price"), "color", diamonds[1:50, ],
#'                  tests = c("chisq.test", "aov"))
#' Hmisc::latex(tab1$tab, 
#'              file     = "",
#'              title    = "Example from qwraps",
#'              ctable   = TRUE,
#'              cgroup   = tab1$cgrp,
#'              n.cgroup = tab1$ncgrp,
#'              colhead  = NULL,
#'              rgroup   = tab1$rgrp,
#'              n.rgroup = tab1$nrgrp,
#'              rowname  = tab1$rwnm,
#'              caption  = "Example Table 1 from the qwraps with Hmisc.",
#'              label    = "tab:tableone",
#'              col.just = rep("r", ncol(tab1$tab)))
#'
#' @export tableone 
tableone <- function(vars, by = NULL, data = NULL, complete = TRUE,
                     margin = 2, overall = TRUE, 
                     stat.con.1 = mean,
                     stat.con.2 = sd,
                     tests = c("chisq.test", "t.test"), 
                     fisher = TRUE,
                     big.mark = "," ) {

  if (is.null(data)) { 
    stop("data must be specified")
  }

  if (complete) { 
    data <- na.omit(data[, c(vars, by)])
  }

  modes <- sapply(data, mode) %in% "numeric"
  if (any(!(modes))) {
    e <- paste(names(modes)[!mode], "need to be factors or numeric")
    stop(e)
  }

  rtn <- lapply(vars, 
                function(v) {
                  if (is.factor(data[, v])) {
                    cattab(v, by, data, margin, tests[1], fisher)
                  } else {
                    contab(v, by, data, stat.con.1, stat.con.2, tests[2])
                  }})

  # the numeric result
  rtn <- do.call(rbind, rtn)

  # frmted return 
  rtn.frmt <- lapply(vars, 
                     function(v) {
                       if (is.factor(data[, v])) {
                         cattab(v, by, data, margin, tests[1], fisher, frmt = TRUE)
                       } else {
                         contab(v, by, data, stat.con.1, stat.con.2, tests[2], frmt = TRUE)
                     }})
  rtn.frmt <- do.call(rbind, rtn.frmt)

  # formating returns
  cgrp <- ncgrp <- rgrp <- nrgrp <- rwnm <- NULL

  if (!overall) {
    rtn   <- rtn[-(1:2), ]
    cgrp  <- levels(data[, by])
  }

  if (overall) { 
    cgrp  <- c("Overall", levels(data[, by]))
  } 

  ncgrp <- rep(2, length(cgrp))

  if (!is.null(tests)) { 
    cgrp  <- c(cgrp, "p-value")
    ncgrp <- c(ncgrp, 1)
  }

  rgrp <- sapply(vars, simpleCap)
  nrgrp <- sapply(vars, 
                  function(v) { 
                    nlevels(data[, v]) + as.numeric(is.null(levels(data[, v]))) 
                  })
  rwnm <- lapply(vars, 
                 function(v) { 
                   if (is.null(levels(data[, v]))) { 
                     simpleCap(v)
                   } else {
                     levels(data[, v]) 
                   }
                 }) 
  rwnm <- do.call(c, rwnm)

  out <- list(tab  = rtn,  tab.frmt = rtn.frmt,
              cgrp = cgrp, ncgrp = ncgrp, 
              rgrp = rgrp, nrgrp = nrgrp,
              rwnm = rwnm)
  class(out) <- "tableone"
  return(out)
}

# cattab is a helperfunction for tablone
cattab <- function(var, by, data, margin, test, fisher, frmt = FALSE) { 
  tab <- table(data[, var], data[, by])
  tabp <- prop.table(tab, margin)

  out <- matrix(NA, nrow = nrow(tab), ncol = ncol(tab) * 2 + 2)
  out[, 1] <- table(data[, var])
  out[, 2] <- prop.table(out[, 1])
  out[, seq(3, ncol(out), by = 2)] <- tab
  out[, seq(4, ncol(out), by = 2)] <- tabp

  if (!is.null(test)) { 
    if (test == "chisq.test") {
      chitest <- suppressWarnings(chisq.test(tab))
      if (any(chitest$expected < 5)) {
        if (fisher) {
          chitest <- fisher.test(tab)
        } else { 
          warning("Expected cell count less than 5")
        }
      }
    } else if (test == "fisher.test"){
      chitest <- fisher.test(tab)
    } else {
      stop("test[1] only chisq.test or fisher.test is currently allowed")
    }

    out <- cbind(out, c(chitest$p.value, rep(NA, nrow(out) - 1))) 
  }

  if (!frmt) { 
    return(out)
  } else {
    outf <- out
    for(i in seq(1, ncol(out) - 1, by = 2)) {
      outf[, i]     <- frmt(out[, i],           digits = 0)
      outf[, i + 1] <- frmt(out[, i + 1] * 100, digits = 1)
    }
    
    if (!is.null(test)) { 
      outf[, ncol(out)] <- frmtp(out[, ncol(out)])
    }
    return(outf)
  }
}

# helper function for tableone
contab <- function(var, by, data, stat.con.1, stat.con.2, test, frmt = FALSE) {
  f1   <- match.fun(stat.con.1)
  f2   <- match.fun(stat.con.2)
  tab1 <- tapply(data[, var], data[, by], FUN = f1)
  tab2 <- tapply(data[, var], data[, by], FUN = f2)
  out  <- matrix(NA, nrow = 1, ncol = nlevels(data[, by]) * 2 + 2)
  out[, 1] <- f1(data[, var])
  out[, 2] <- f2(data[, var])
  out[, seq(3, ncol(out), by = 2)] <- tab1
  out[, seq(4, ncol(out), by = 2)] <- tab2

  if (!is.null(test)) { 
    if (test == "t.test") { 
      tout <- t.test(as.formula(paste(var, "~",  by)), data)
      out <- cbind(out, c(tout$p.value, rep(NA, nrow(out) - 1)))
    } else if (test == "aov") { 
      aout <- aov(as.formula(paste(var, "~",  by)), data)
      out <- cbind(out, 
                   c(summary(aout)[[1]][["Pr(>F)"]][1], rep(NA, nrow(out) - 1)))
    } else {
      stop("only t.test and aov can be called at this time.")
    }
  } 

  if (!frmt) { 
    return(out)
  } else {
    outf <- out
    for(i in seq(1, ncol(out) - 1, by = 2)) {
      outf[, i] <- frmt(out[, i], digits = 0)
      outf[, i + 1] <- frmt(out[, i + 1] * 100, digits = 1)
    }
    if (!is.null(test)) { 
      outf[, ncol(out)] <- frmtp(out[, ncol(out)])
    }
    return(outf)
  }
}




#' @rdname tableone
#' @method print tableone
#' @S3method print tableone
print.tableone <- function(tab1, file = getOption("qwraps.tableone.file", ""), 
                           title = getOption("qwraps.tableone.title", "",),
                           ctable = getOption("qwraps.tableone.ctable", TRUE),
                           ...) {
latex(tab1[["tab.frmt"]],
      file     = file,
      title    = title,
      ctable   = ctable,
      cgroup   = tab1[['cgrp']],
      n.cgroup = tab1[['ncgrp']],
      rgroup   = tab1[['rgrp']],
      n.rgroup = tab1[['nrgrp']],
      rowname  = tab1[['rwnm']],
      col.just = rep("r", ncol(tab1$tab.frmt)),
      ...) 
}
