summary.pyears <- function(object, ...) {
    if (!is.null(cl<- object$call)) {
        cat(gettext("Call:", domain = "R-survival"), "\n", sep = "")
        dput(cl)
        cat("\n")
        }

        cat(gettextf("Total number of person-years tabulated: %s\nTotal number of person-years off table: %s", 
             format(sum(object$data$pyears)), format(object$offtable), domain = "R-survival"))
    if (is.null(object$data)) {
        cat("\n")
        }
    
    if (!is.null(object$summary)) {
        cat(gettextf("Matches to the chosen rate table:\n  %s",  object$summary), domain = "R-survival")
        }
    cat(gettext("Observations in the data set: ", domain = "R-survival"), object$observations, "\n", sep = "")
    if (!is.null(object$na.action))
      cat("  (", naprint(object$na.action), ")\n", sep='')
    cat("\n")
    }

                               
