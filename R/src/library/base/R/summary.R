#  File src/library/base/R/summary.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2016 The R Core Team
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  https://www.R-project.org/Licenses/

summary <- function (object, ...) UseMethod("summary")

summary.default <- function(object, ..., digits)
{
    if(is.factor(object))
	return(summary.factor(object, ...))
    else if(is.matrix(object)) {
	if(missing(digits))
            return(summary.matrix(object, ...))
        else
            return(summary.matrix(object, digits = digits, ...))
    }

    value <- if(is.logical(object)) {# scalar or array!
	c(Mode = "logical",
          {tb <- table(object, exclude = NULL, useNA = "ifany") # incl. NA s
           if(!is.null(n <- dimnames(tb)[[1L]]) && any(iN <- is.na(n)))
               dimnames(tb)[[1L]][iN] <- gettext("NA's")
           tb
           })# incl. NA s
    } else if(is.numeric(object)) {
	nas <- is.na(object)
	object <- object[!nas]
	qq <- stats::quantile(object)
	if(any(nas)) {
	    qq <- c(signif(c(qq[1L:3L], mean(object), qq[4L:5L]), digits), sum(nas))
	    names(qq) <- c(gettext("Min."), gettext("1st Qu."), gettext("Median"), gettext("Mean"), gettext("3rd Qu."), gettext("Max."), gettext("NA's"))
	    qq
	}
	else {
	    qq <- signif(c(qq[1L:3L], mean(object), qq[4L:5L]), digits)
	    names(qq) <- c(gettext("Min."), gettext("1st Qu."), gettext("Median"), gettext("Mean"), gettext("3rd Qu."), gettext("Max."))
	    qq
	}
    } else if(is.recursive(object) && !is.language(object) &&
	      (n <- length(object))) { # do not allow long dims
	sumry <- array("", c(n, 3L), list(names(object), c(gettext("Length", domain = "R-base"), gettext("Class", domain = "R-base"), gettext("Mode", domain = "R-base"))))
	ll <- numeric(n)
	for(i in seq_len(n)) {
	    ii <- object[[i]]
	    ll[i] <- length(ii)
	    cls <- oldClass(ii)
	    sumry[i, 2L] <- if(length(cls)) cls[1L] else "-none-"
	    sumry[i, 3L] <- mode(ii)
	}
	sumry[, 1L] <- format(as.integer(ll))
	sumry
    }
    else { 
	yy <- c(length(object), class(object), mode(object))
	names(yy) <- c(gettext("Length"), gettext("Class"), gettext("Mode"))
	yy
    }
    class(value) <- c("summaryDefault", "table")
    value
}

format.summaryDefault <- function(x, digits = max(3L, getOption("digits") - 3L), ...)
{
    xx <- x
    if(is.numeric(x) || is.complex(x)) {
      finite <- is.finite(x)
      xx[finite] <- zapsmall(x[finite])
    }
    class(xx) <- class(x)[-1]
    m <- match(gettext("NA's"), names(x), 0)
    if(inherits(x, "Date") || inherits(x, "POSIXct")) {
        if(length(a <- attr(x, "NAs"))) {
            xx <- c(format(xx, digits=digits, ...), as.character(a))
            names(xx) <- c(gettext("Min."), gettext("1st Qu."), gettext("Median"), gettext("Mean"), gettext("3rd Qu."), gettext("Max."), gettext("NA's"))
	    xx
	}
        else xx <- format(xx, digits=digits)
    } else if(m && !is.character(x)) {
        yy <- c(format(xx[-m], digits=digits, ...), as.character(xx[m]))
        names(yy) <- c(gettext("Min."), gettext("1st Qu."), gettext("Median"), gettext("Mean"), gettext("3rd Qu."), gettext("Max."), gettext("NA's"))
	yy
	}
    else format(xx, digits=digits, ...)
}

print.summaryDefault <- function(x, digits = max(3L, getOption("digits") - 3L), ...)
{
    xx <- x
    if(is.numeric(x) || is.complex(x)) {
      finite <- is.finite(x)
      xx[finite] <- zapsmall(x[finite])
    }
    class(xx) <- class(x)[-1] # for format
    m <- match(gettext("NA's"), names(xx), 0)
    if(inherits(x, "Date") || inherits(x, "POSIXct")) {
        if(length(a <- attr(x, "NAs"))) {
            xx <- c(format(xx, digits=digits), as.character(a))
            names(xx) <- c(gettext("Min."), gettext("1st Qu."), gettext("Median"), gettext("Mean"), gettext("3rd Qu."), gettext("Max."), gettext("NA's"))
	    xx
	}
        else xx <- format(xx, digits=digits)
        print(xx, digits=digits, ...)
        return(invisible(x))
    } else if(m && !is.character(x)) {
        xx <- c(format(xx[-m], digits=digits), as.character(xx[m]))
        names(xx) <- c(gettext("Min."), gettext("1st Qu."), gettext("Median"), gettext("Mean"), gettext("3rd Qu."), gettext("Max."), gettext("NA's"))
	xx
    }
    print.table(xx, digits=digits, ...)
    invisible(x)
}

summary.factor <- function(object, maxsum = 100L, ...)
{
    nas <- is.na(object)
    ll <- levels(object)
    if(ana <- any(nas)) maxsum <- maxsum - 1L
    tbl <- table(object)
    tt <- c(tbl) # names dropped ...
    names(tt) <- dimnames(tbl)[[1L]]
    if(length(ll) > maxsum) {
	drop <- maxsum:length(ll)
	o <- sort.list(tt, decreasing = TRUE)
	tt <- c(tt[o[ - drop]], sum(tt[o[drop]]))
        names(tt) <- c(names(tt)[ - drop], gettext("(Other)"))
	tt
	
    }
    if(ana) {
	tt2 <- c(tt, sum(nas))
	names(tt2) <- c(names(tt), gettext("NA's"))
	tt2
     }
    else tt
}

summary.matrix <- function(object, ...) {
    ## we do want this changed into separate columns, so use data.frame method
    summary.data.frame(as.data.frame.matrix(object), ...)
}

summary.data.frame <-
    function(object, maxsum = 7L, digits = max(3L, getOption("digits") - 3L), ...)
{
    ncw <- function(x) {
        z <- nchar(x, type="w")
        if (any(na <- is.na(z))) {
            # FIXME: can we do better
            z[na] <- nchar(encodeString(z[na]), "b")
        }
        z
    }
    # compute results to full precision.
    z <- lapply(X = as.list(object), FUN = summary,
                maxsum = maxsum, digits = 12L, ...)
    nv <- length(object)
    nm <- names(object)
    lw <- numeric(nv)
    nr <- if (nv)
	      max(vapply(z, function(x) NROW(x) + !is.null(attr(x, "NAs")), integer(1)))
	  else 0
    for(i in seq_len(nv)) {
        sms <- z[[i]]
        if(is.matrix(sms)) {
            ## need to produce a single column, so collapse matrix
            ## across rows
            cn <- paste(nm[i], gsub("^ +", "", colnames(sms), useBytes = TRUE),
                        sep=".")
	    tmp <- format(sms)# <- digits = ??  --currently take getOption("digits") !!!
            if(nrow(sms) < nr)
                tmp <- rbind(tmp, matrix("", nr - nrow(sms), ncol(sms)))
            sms <- apply(tmp, 1L, function(x) paste(x, collapse="  "))
            ## produce a suitable colname: undoing padding
            wid <- sapply(tmp[1L, ], nchar, type="w") # might be NA
            blanks <- paste(character(max(wid)), collapse = " ")
            wcn <- ncw(cn)
            pad0 <- floor((wid - wcn)/2)
            pad1 <- wid - wcn - pad0
            cn <- paste0(substring(blanks, 1L, pad0), cn,
                         substring(blanks, 1L, pad1))
            nm[i] <- paste(cn, collapse="  ")
        } else {
            sms <- format(sms, digits = digits) # may add NAs row
            lbs <- format(names(sms))
            sms <- paste0(lbs, ":", sms, "  ")
            lw[i] <- ncw(lbs[1L])
            length(sms) <- nr
        }
	z[[i]] <- sms
    }
    if (nv) {
	z <- unlist(z, use.names=TRUE)
	dim(z) <- c(nr, nv)
	if(anyNA(lw))
	warning(gettextf("probably wrong encoding in 'names(.)' of column %s", paste(which(is.na(lw)), collapse = ", ")))
	blanks <- paste(character(max(lw, na.rm=TRUE) + 2L), collapse = " ")
	pad <- floor(lw - ncw(nm)/2)
	nm <- paste0(substring(blanks, 1, pad), nm)
	dimnames(z) <- list(rep.int("", nr), nm)
    } else {
	z <- character()
	dim(z) <- c(nr, nv)
    }
    attr(z, "class") <- c("table") #, "matrix")
    z
}
