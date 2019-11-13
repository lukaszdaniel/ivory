# $Date: 2006-08-28 14:31:20 $ $Id: print.survdiff.S 11166 2008-11-24 22:10:34Z therneau $
print.survdiff <- function(x, digits = max(options()$digits - 4, 3), ...) {

    saveopt <-options(digits=digits)
    on.exit(options(saveopt))

    if (!inherits(x, 'survdiff'))
	stop(gettextf("'%s' argument is not an object of class %s", "x", dQuote("survdiff")))
    if (!is.null(cl<- x$call)) {
	cat(gettext("Call:", domain = "R-survival"),"\n", sep = "")
	dput(cl)
	cat("\n")
	}

    omit <- x$na.action
    if (length(omit)) cat("n=", sum(x$n), ", ", naprint(omit),
					  ".\n\n", sep='')

    if (length(x$n)==1)  {
	z <- sign(x$exp - x$obs) * sqrt(x$chisq)
	temp <- c(x$obs, x$exp, z, signif(pchisq(x$chisq, 1, lower.tail=FALSE), digits))
	names(temp) <- c(gettext("Observed"), gettext("Expected"), "Z", "p")
	print(temp)
	}
    else {
	if (is.matrix(x$obs)){
	    otmp <- apply(x$obs,1,sum)
	    etmp <- apply(x$exp,1,sum)
	    }
	else {
	    otmp <- x$obs
	    etmp <- x$exp
	    }
	df <- (sum(1*(etmp>0))) -1
	temp <- cbind(x$n, otmp, etmp, ((otmp-etmp)^2)/ etmp,
					 ((otmp-etmp)^2)/ diag(x$var))
	dimnames(temp) <- list(names(x$n), c("N", gettext("Observed"), gettext("Expected"), "(O-E)^2/E", "(O-E)^2/V"))
	print(temp)
	cat("\n ", sprintf(gettext("Chisq= %s on %d degrees of freedom, p= %s", domain = "R-survival"), format(round(x$chisq,1)), df, format.pval(pchisq(x$chisq, df, lower.tail=FALSE)), domain = NA), "\n", sep = "")
       }
    invisible(x)
    }
