print.summary.coxph <-
 function(x, digits = max(getOption('digits')-3, 3),  
             signif.stars = getOption("show.signif.stars"), expand=FALSE, ...) {
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

    if (expand && !is.null(x$cmap)) { # this was a coxphms object
        signif.stars <- FALSE  #work around issue with printCoefmat
        # print it group by group
        tmap <- x$cmap
        cname <- colnames(tmap)
        printed <- rep(FALSE, length(cname))
        for (i in seq_along(cname)) {
            # if multiple colums of tmat are identical, only print that
            #  set of coefficients once
            if (!printed[i]) { # this column hasn't been printed
                j <- apply(tmap, 2, function(x) all(x == tmap[,i])) 
                printed[j] <- TRUE  # mark all that match as 'printed'
               
                tmp2 <- x$coefficients[tmap[,i],, drop=FALSE]
                names(dimnames(tmp2)) <- c(paste(cname[j], collapse=", "), "")
                # restore character row names
                rownames(tmp2) <- rownames(tmap)[tmap[,i]>0]
 
                printCoefmat(tmp2, digits=digits, P.values=TRUE, 
                             has.Pvalue=TRUE, signif.legend=FALSE,
                             signif.stars = signif.stars, ...)

                if (!is.null(x$conf.int)) {
                    tmp2 <- x$conf.int[tmap[,i],, drop=FALSE]
                    rownames(tmp2) <- rownames(tmap)[tmap[,i] >0]
                    names(dimnames(tmp2)) <- c(paste(cname[j], collapse=", "),"")
                    print(tmp2, digits=digits, ...)
                }   
            } 
        }        
        cat("\n ", gettext("States:", domain = "R-survival"), paste(paste(seq(along=x$states), x$states, sep='= '), 
                               collapse=", "), '\n')
    } else {
        if(!is.null(x$coefficients)) {
            cat("\n")
            printCoefmat(x$coefficients, digits=digits,
                         signif.stars=signif.stars, ...)
        }
        if(!is.null(x$conf.int)) {
            cat("\n")
            print(x$conf.int)
        }
    }       
    cat("\n")

    if (!is.null(x$concordance)) {
        cat(gettextf("Concordance = %s (se = %s)", format(round(x$concordance[1],3)), format(round(x$concordance[2], 3)), domain = "R-survival"), "\n", sep = "")
    }
#    cat(gettextf("Rsquare= %s  (max possible=%s)", format(round(x$rsq["rsq"],3)), format(round(x$rsq["maxrsq"],3)), domain = "R-survival"), "\n", sep = "")

    pdig <- max(1, getOption("digits")-4)  # default it too high IMO
    cat(gettextf("Likelihood ratio test = %s on %s df, p=%s", format(round(x$logtest["test"], 2)),  x$logtest["df"], format.pval(x$waldtest["pvalue"], digits=pdig), domain = "R-survival"), "\n", sep = "")
    cat(gettextf("Wald test = %s on %s df, p=%s", format(round(x$waldtest["test"], 2)), x$waldtest["df"], format.pval(x$waldtest["pvalue"], digits=pdig), domain = "R-survival"), "\n", sep = "")
    cat(gettextf("Score (logrank) test = %s on %s df, p=%s", format(round(x$sctest["test"], 2)), x$sctest["df"], format.pval(x$waldtest["pvalue"], digits=pdig), domain = "R-survival"))
    if (is.null(x$robscore))
        cat("\n\n")
    else cat(",   ", gettextf("Robust = %s p=%s", format(round(x$robscore["test"], 2)), format.pval(x$robscore["pvalue"], digits=pdig), domain = "R-survival"), "\n\n", sep = "")

    if (x$used.robust)
	cat(gettext("  (Note: the likelihood ratio and score tests assume independence of\n     observations within a cluster, the Wald and robust score tests do not).", domain = "R-survival"), "\n", sep = "")
    invisible()
}
