print.aareg <- function(x, maxtime, test=c('aalen', 'nrisk'), scale=1, ...) {
    if (!inherits(x, 'aareg')) stop(gettextf("'%s' argument is not an object of class %s", "x", dQuote("aareg")))
    if (!is.null(cl<- x$call)) {
	cat(gettext("Call:", domain = "R-survival"), "\n", sep = "")
	dput(cl)
	cat("\n")
	}

    if (missing(test)) test <- x$test
    else test <- match.arg(test)

    if (missing(maxtime)) summ <- summary(x, test=test, scale=scale)
    else                  summ <- summary(x, maxtime=maxtime, test=test, scale=scale)

    omit <- x$na.action
    if (length(omit))
	cat("  n=", x$n[1], " (", naprint(omit), ")\n", sep="")
    else cat("  n=", x$n[1], "\n")
    cat("   ", gettextf("%d out of %d unique event times used", summ$n[2], x$n[3], domain = "R-survival"), "\n\n", sep = "")
    print(signif(summ$table,3))
    chi <- summ$chisq
    df <- nrow(summ$table) -1
    pdig <- max(1, getOption("digits")-4)  # default it too high IMO
    cat("\n", sprintf(gettext("Chisq= %s on %d df, p=%s; test weights= %s", domain = "R-survival"), format(round(chi,2)), df, format.pval(pchisq(chi,df, lower.tail=FALSE), digits=pdig), paste(x$test, collapse = " "), domain = NA), "\n", sep="")
    invisible(x)
    }

