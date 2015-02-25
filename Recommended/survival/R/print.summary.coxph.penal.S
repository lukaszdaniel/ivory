print.summary.coxph.penal <-
 function(x,  digits = max(options()$digits - 3, 3),
           signif.stars = getOption("show.signif.stars"), ...) {
    if (!is.null(x$call)) {
	cat(gettext("Call:", domain = "R-survival"), "\n", sep = "")
	dput(x$call)
	cat("\n")
	}
    if (!is.null(x$fail)) {
	cat(gettext(" Coxreg failed. ", domain = "R-survival"), x$fail, "\n", sep = "")
	return()
	}
    savedig <- options(digits = digits)
    on.exit(options(savedig))

    omit <- x$na.action
    cat("  n= ", x$n, sep = "")
    if (!is.null(x$nevent)) cat(", ", gettextf("number of events= %d", x$nevent, domain = "R-survival"), "\n", sep = "")
    else cat("\n")
    if (length(omit))
	cat("   (", naprint(omit), ")\n\n", sep="")
    else cat("\n")

    # Format out the NA in the coef matrix
    print1 <- x$coefficients
    temp <- cbind(format(print1[,1]), format(print1[,2]), 
		       format(print1[,3]),
		       format(round(print1[,4], 2)),
		       format(round(print1[,5], 2)),
		       format(signif(print1[,6], 2)))
    temp <- ifelse(is.na(print1), "", temp)
    dimnames(temp) <- dimnames(print1)
    print(temp, quote=FALSE)

    if(length(x$conf.int) >0 ) {
        cat("\n")
        print(x$conf.int)
        }
    logtest <- -2 * (x$loglik[1] - x$loglik[2])
    sctest <- x$score

    cat("\n", gettextf("Iterations: %d outer, %d Newton-Raphson", x$iter[1], x$iter[2], domain = "R-survival"), "\n", sep = "")
    if (length(x$print2)) {
        for (i in 1:length(x$print2)) cat("    ", x$print2[i], "\n")
        }
    if (is.null(x$df)) df <- sum(!is.na(coef))
    else  df <- round(sum(x$df),2)
    cat(gettextf("Degrees of freedom for terms= %s", paste(format(round(x$df,1)), collapse = " "), domain = "R-survival"), "\n", sep = "")
    if (!is.null(x$concordance)) {
        cat(gettextf("Concordance= %s (se = %s)", format(round(x$concordance[1],3)), format(round(x$concordance[2], 3)), domain = "R-survival"), "\n", sep = "")
    }
    cat(gettextf("Likelihood ratio test = %s on %s df, p=%s", format(round(logtest, 2)),  df, format(1 - pchisq(logtest, df)), domain = "R-survival"), "\n", sep = "")
    if (!is.null(x$wald.test))
        cat(gettextf("Wald test = %s on %s df, p=%s", format(round(x$wald.test, 2)), df, format(1 - pchisq(x$wald.test, df)), domain = "R-survival"), "\n", sep = "")
    if (!is.null(x$score))
	cat("\n", gettextf("Score (logrank) test = %s on %s df, p=%s", format(round(sctest, 2)), df, format(1 - pchisq(sctest, df)), domain = "R-survival"), sep = "")
    if (is.null(x$rscore)) cat("\n")
    else cat(",   ", gettextf("Robust = %s p=%s", format(round(x$rscore, 2)), format(1 - pchisq(x$rscore, df)), domain = "R-survival"), "\n", sep = "")

    invisible()
    }
