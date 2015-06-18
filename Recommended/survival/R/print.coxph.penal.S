print.coxph.penal <-  function(x, terms=FALSE, maxlabel=25,
			       digits=max(options()$digits - 4, 3), ...) {
    if (!inherits(x, 'coxph.penal')) stop(gettextf("'%s' argument is not an object of class %s", "x", dQuote("coxph.penal")))

    if (!is.null(x$call)) {
	cat(gettext("Call:", domain = "R-survival"), "\n", sep = "")
	dput(x$call)
	cat("\n")
	}
    if (!is.null(x$fail)) {
	cat(gettext(" Coxph failed. ", domain = "R-survival"), x$fail, "\n", sep = "")
	return()
	}
    savedig <- options(digits = digits)
    on.exit(options(savedig))

    coef <- x$coefficients
    if (length(coef)==0 && length(x$frail)==0)
	    stop("Penalized print function can't be used for a null model")

    #
    # Map terms to special print functions, and the list of iteration histories
    #
    pterms <- x$pterms
    nterms <- length(pterms)
    npenal <- sum(pterms>0)
    print.map <- rep(0,nterms)
    if (!is.null(x$printfun)) {
	temp <- unlist(lapply(x$printfun, is.null))  #which ones are missing
	print.map[pterms>0] <- (1:npenal) * (!temp)
	}

    # Tedious, but build up the coef matrix a term at a time
    print1 <- NULL
    pname1 <- NULL
    if (is.null(x$assign2)) alist <- x$assign[-1]
    else alist <- x$assign2

    print2 <- NULL
    for (i in 1:nterms) {
	kk <- alist[[i]]
	if (print.map[i] >0) {
	    j <- print.map[i]	
	    if (pterms[i]==2) 
		 temp <- (x$printfun[[j]])(x$frail, x$fvar, ,x$df[i], 
					   x$history[[j]])
	    else temp <- (x$printfun[[j]])(coef[kk], x$var[kk,kk], 
					   x$var2[kk,kk], 
					   x$df[i], x$history[[j]])
	    print1 <- rbind(print1, temp$coef)
	    if (is.matrix(temp$coef)) {
		xx <- dimnames(temp$coef)[[1]]
		if (is.null(xx))
			xx <- rep(names(pterms)[i], nrow(temp$coef))
		else    xx <- paste(names(pterms)[i], xx, sep=', ')
		pname1 <- c(pname1, xx)
		}
	    else  pname1 <- c(pname1, names(pterms)[i])
	    print2 <- c(print2, temp$history)
	    }

	else if (terms && length(kk)>1) {
	    pname1 <- c(pname1, names(pterms)[i])
	    temp <- coxph.wtest(x$var[kk,kk], coef[kk])$test
	    print1 <- rbind(print1, c(NA, NA, NA,
				      temp, x$df[i], 1-pchisq(temp, 1)))
	    }
	else {
	    pname1 <- c(pname1, names(coef)[kk])
	    tempe<- (diag(x$var))[kk]
	    temp <- coef[kk]^2/ tempe
	    print1 <- rbind(print1, cbind(coef[kk], sqrt(tempe),
				      sqrt((diag(x$var2))[kk]), 
				      temp, 1, 1-pchisq(temp, 1)))
	    }
	}

    # Format out the NA's 
#    temp <- cbind(format(print1[,1]), format(print1[,2]), 
#		       format(print1[,3]),
#		       format(round(print1[,4], 2)),
#		       format(round(print1[,5], 2)),
#		       format(signif(print1[,6], 2)))
#    temp <- ifelse(is.na(print1), "", temp)
#    dimnames(temp) <- list(substring(pname1,1, maxlabel), 
#			     c("coef","se(coef)", "se2", "Chisq","DF","p"))
#    print(temp, quote=FALSE)
    dimnames(print1) <-  list(substring(pname1,1, maxlabel), 
                              c("coef","se(coef)", "se2", "Chisq","DF","p"))
    printCoefmat(print1, signif.stars=FALSE, P.values=TRUE, has.Pvalue=TRUE,
                 na.print="")
	
    #
    # Write out the remaider of the info
    #
    cat("\n", gettextf("Iterations: %d outer, %d Newton-Raphson", x$iter[1], x$iter[2], domain = "R-survival"), "\n", sep = "")
    if (length(print2)) {
#	cat("Penalized terms:\n")
	for (i in 1:length(print2)) cat("    ", print2[i], "\n")
	}

    logtest <- -2 * (x$loglik[1] - x$loglik[2])
    if (is.null(x$df)) df <- sum(!is.na(coef))
    else  df <- round(sum(x$df),2)
#    cat("\n")
    cat(gettextf("Degrees of freedom for terms= %s", paste(format(round(x$df,1)), collapse = " "), domain = "R-survival"), "\n", sep = "")
#    cat("Cox PL (initial,final) = ", format(round(x$loglik,2)),
#	"  Penalty = ", format(x$penalty), "\n")
    omit <- x$na.action
    if (length(omit)) {
    cat(gettextf("Likelihood ratio test=%s on %s df, p=%s n=%d", format(round(logtest, 2)), format(round(df,2)), format(1 - pchisq(logtest, df)), x$n, domain = "R-survival"), " (", naprint(omit), ")", "\n",  sep = "")
    }
    else {
    cat(gettextf("Likelihood ratio test=%s on %s df, p=%s n=%d", format(round(logtest, 2)), format(round(df,2)), format(1 - pchisq(logtest, df)), x$n, domain = "R-survival"), "\n", sep = "")
	}
    invisible()
    }
