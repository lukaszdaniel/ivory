# $Id: print.coxph.null.S 11166 2008-11-24 22:10:34Z therneau $
print.coxph.null <-
 function(x, digits=max(options()$digits - 4, 3), ...)
    {
    if (!is.null(cl<- x$call)) {
	cat(gettext("Call: ", domain = "R-survival"))
	dput(cl)
	cat("\n")
	}

    cat(gettext("Null model", domain = "R-survival"), "\n  ", gettextf("log likelihood=%s", format(x$loglik), domain = "R-survival"), "\n", sep = "")
    omit <- x$na.action
    if (length(omit))
	cat("  n=", x$n, " (", naprint(omit), ")\n", sep = "")
    else cat("  n=", x$n, "\n", sep = "")
    }
