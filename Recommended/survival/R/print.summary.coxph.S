print.summary.coxph <-
 function(x, digits = max(getOption('digits')-3, 3),  
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
	cat("   (", naprint(omit), ")\n", sep = "")

    if (nrow(x$coef) == 0) {   # Null model
	cat ("   ", gettext("Null model", domain = "R-survival"), "\n", sep = "")
	return()
        }


    if(!is.null(x$coefficients)) {
        cat("\n")
        printCoefmat(x$coefficients, digits=digits,
                                 signif.stars=signif.stars, ...)
    }
    if(!is.null(x$conf.int)) {
        cat("\n")
        print(x$conf.int)
        }
    cat("\n")

    if (!is.null(x$concordance)) {
        cat(gettextf("Concordance= %s (se = %s)", format(round(x$concordance[1],3)), format(round(x$concordance[2], 3)), domain = "R-survival"), "\n", sep = "")
    }
    cat(gettextf("Rsquare= %s  (max possible=%s)", format(round(x$rsq["rsq"],3)), format(round(x$rsq["maxrsq"],3)), domain = "R-survival"), "\n", sep = "")

    cat(gettextf("Likelihood ratio test = %s on %s df, p=%s", format(round(x$logtest["test"], 2)),  x$logtest["df"], format(x$logtest["pvalue"]), domain = "R-survival"), "\n", sep = "")
    cat(gettextf("Wald test = %s on %s df, p=%s", format(round(x$waldtest["test"], 2)), x$waldtest["df"], format(x$waldtest["pvalue"]), domain = "R-survival"), "\n", sep = "")
    cat(gettextf("Score (logrank) test = %s on %s df, p=%s", format(round(x$sctest["test"], 2)), x$sctest["df"], format(x$sctest["pvalue"]), domain = "R-survival"))
    if (is.null(x$robscore))
        cat("\n\n")
    else cat(",   ", gettextf("Robust = %s p=%s", format(round(x$robscore["test"], 2)), format(x$robscore["pvalue"]), domain = "R-survival"), "\n\n", sep = "")

    if (x$used.robust)
	cat(gettext("  (Note: the likelihood ratio and score tests assume independence of\n     observations within a cluster, the Wald and robust score tests do not).", domain = "R-survival"), "\n", sep = "")
    invisible()
}
