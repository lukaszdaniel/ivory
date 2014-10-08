# $Id: print.summary.survreg.S 11166 2008-11-24 22:10:34Z therneau $
print.summary.survreg <- function(x, digits = max(options()$digits - 4, 3),
				  ...) {
    correl <- x$correlation
    n <- x$n

    if(is.null(digits))
        digits <- options()$digits
    cat("\n", gettext("Call:", domain = "R-survival"), "\n", sep = "")
    dput(x$call)

    print(x$table, digits = digits)
    if (nrow(x$var)==length(x$coefficients)) 
	    cat("\n", gettextf("Scale fixed at %s", format(x$scale, digits=digits), domain = "R-survival"), "\n", sep = "") 
    else if (length(x$scale)==1) 
	    cat ("\n", gettextf("Scale= %s", format(x$scale, digits=digits), domain = "R-survival"), "\n", sep = "")
    else {
	cat("\n", gettext("Call:", domain = "R-survival"), "\n", sep = "")
	print(x$scale, digits=digits, ...)
	}

    cat("\n", x$parms, "\n", sep='')
    df  <- sum(x$df) - x$idf   # The sum is for penalized models
    cat(gettextf("Loglik(model)= %s   Loglik(intercept only)= %s", format(round(x$loglik[2],1)), format(round(x$loglik[1],1)), domain = "R-survival"))
    if (df > 0)
	    cat("\n\t", gettextf("Chisq= %s on %d degrees of freedom, p= %s", format(round(x$chi,2)), round(df,1), format(signif(1-pchisq(x$chi, df),2)), domain = "R-survival"), "\n", sep = "")
    else cat("\n")
    if (x$robust) cat(gettext("(Loglikelihood assumes independent observations)", domain = "R-survival"), "\n", sep = "")
    cat(gettext("Number of Newton-Raphson Iterations: ", domain = "R-survival"), format(trunc(x$iter)), "\n", sep = "")
    omit <- x$na.action
    if (length(omit))
	cat("n=", x$n, " (", naprint(omit), ")\n", sep="")
    else cat("n=", x$n, "\n")

    if(!is.null(correl)) {
        p <- dim(correl)[2]
        if(p > 1) {
            cat("\n", gettext("Correlation of Coefficients:", domain = "R-survival"), "\n", sep = "")
            ll <- lower.tri(correl)
            correl[ll] <- format(round(correl[ll], digits=digits))
            correl[!ll] <- ""
            print(correl[-1,  - p, drop = FALSE], quote = FALSE)
            }
        }
    cat("\n")
    invisible(NULL)
    }
