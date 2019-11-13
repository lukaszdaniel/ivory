print.coxph <-
 function(x, digits=max(1L, getOption("digits") - 3L), signif.stars=FALSE, ...)
    {
    if (!is.null(cl<- x$call)) {
	cat(gettext("Call:", domain = "R-survival"), "\n", sep = "")
	dput(cl)
	cat("\n")
	}
    if (!is.null(x$fail)) {
	cat(gettext(" Coxph failed. ", domain = "R-survival"), x$fail, "\n", sep = "")
	return()
	}
    savedig <- options(digits = digits)
    on.exit(options(savedig))

    coef <- x$coefficients
    se <- sqrt(diag(x$var))
    if(is.null(coef) | is.null(se))
        stop("input is not valid")

    if (is.null(x$naive.var)) {
	tmp <- cbind(coef, exp(coef), se, coef/se,
               pchisq((coef/se)^2, 1, lower.tail=FALSE))
	dimnames(tmp) <- list(names(coef), c("coef", "exp(coef)",
	    "se(coef)", "z", "p"))
	}
    else {
	nse <- sqrt(diag(x$naive.var))
	tmp <- cbind(coef, exp(coef), nse, se, coef/se,
	       pchisq((coef/se)^2, 1, lower.tail=FALSE))
	dimnames(tmp) <- list(names(coef), c("coef", "exp(coef)",
	    "se(coef)", "robust se", "z", "p"))
	}
    
    if (inherits(x, "coxphms")) {
        # print it group by group
        tmap <- x$cmap[-1,,drop=FALSE]  # ignore the intercept (strata)
        cname <- colnames(tmap)
        for (i in seq_along(cname)) {
            tmp2 <- tmp[tmap[,i],, drop=FALSE]
            names(dimnames(tmp2)) <- c(cname[i], "")
            # restore character row names
            rownames(tmp2) <- rownames(tmap)[tmap[,i]>0]
            printCoefmat(tmp2, digits=digits, P.values=TRUE, has.Pvalue=TRUE,
                 signif.stars = signif.stars, ...)
            cat("\n")
        }

        cat(gettext(" States: ", domain = "R-survival"), paste(paste(seq(along=x$states), x$states, sep='= '), 
                               collapse=", "), '\n', sep = "")
        # cat(" States: ", paste(x$states, collapse=", "), '\n')
        if (FALSE) { # alternate forms, still deciding which I like
            stemp <- x$states
            names(stemp) <- seq_along(stemp)
            print(stemp, quote=FALSE)
        }
    }
    else printCoefmat(tmp, digits=digits, P.values=TRUE, has.Pvalue=TRUE,
                 signif.stars = signif.stars, ...)

    logtest <- -2 * (x$loglik[1] - x$loglik[2])
    if (is.null(x$df)) df <- sum(!is.na(coef))
    else  df <- round(sum(x$df),2)
    cat("\n")
    cat(gettextf("Likelihood ratio test = %s on %s df, p=%s", format(round(logtest, 2)), df, format.pval(pchisq(logtest, df, lower.tail=FALSE), digits=digits), domain = "R-survival"))
    cat("\n")
    omit <- x$na.action
    cat("n=", x$n)
    if (!is.null(x$nevent)) cat(", ", gettextf("number of events= %d", x$nevent, domain = "R-survival"), "\n", sep = "")
    else cat("\n")
    if (length(omit))
	cat("   (", naprint(omit), ")\n", sep="")
    invisible(x)
    }

coef.coxphms <- function(object, type=c("vector", "matrix"), ...) {
    type <- match.arg(type)
    if (type=="matrix") {
        cmap2 <- object$cmap[-1,, drop=FALSE]
        cmat <- 0*cmap2  # all the right names
        cmat[cmap2>0] <- object$coefficient[cmap2]
        attr(cmat, "states") <- object$states
        cmat
    }
    else NextMethod(object, ...)
}

        
