print.survreg <- function(x, ...)
{
    if(!is.null(cl <- x$call)) {
        cat(gettext("Call:", domain = "R-survival"), "\n", sep = "")
        dput(cl)
        }
    if (!is.null(x$fail)) {
	cat(gettext(" Survreg failed. ", domain = "R-survival"), x$fail, "\n", sep = "")
	return(invisible(x))
	}
    coef <- x$coef
    if(any(nas <- is.na(coef))) {
	if(is.null(names(coef))) names(coef) <- paste("b", 1:length(coef), sep = "")
        cat("\n", sprintf(ngettext(sum(nas), "Coefficients: (%d not defined because of singularity)", "Coefficients: (%d not defined because of singularities)", domain = "R-survival"), sum(nas)), "\n", sep = "")
        }
    else cat("\n", gettext("Coefficients:", domain = "R-survival"), "\n", sep = "")
    print(coef, ...)
    
    if (nrow(x$var)==length(coef)) 
	    cat("\n", gettextf("Scale fixed at %s",format(x$scale), domain = "R-survival"), "\n", sep = "") 
    else if (length(x$scale)==1) cat("\n", gettextf("Scale= %s", format(x$scale), domain = "R-survival"), "\n", sep = "")
    else {
	cat("\n", gettext("Scale:", domain = "R-survival"), "\n", sep = "")
	print(x$scale, ...)
	}

    pdig <- max(1, getOption("digits")-4)  # default it too high IMO
    nobs <- length(x$linear)
    chi <- 2*diff(x$loglik)
    df  <- sum(x$df) - x$idf   # The sum is for penalized models
    cat("\n", gettextf("Loglik(model)= %s   Loglik(intercept only)= %s", format(round(x$loglik[2],1)), format(round(x$loglik[1],1)), domain = "R-survival"), sep = "")
    if (df > 0)
	    cat("\n\t", gettextf("Chisq= %s on %d degrees of freedom, p= %s", format(round(chi,2)), round(df,1), format.pval(pchisq(chi, df, lower.tail=FALSE), digits=pdig), domain = "R-survival"), "\n", sep = "")
    else cat("\n")

    omit <- x$na.action
    if (length(omit))
	cat("n=", nobs, " (", naprint(omit), ")\n", sep="")
    else cat("n=", nobs, "\n")
    invisible(x)
    }
