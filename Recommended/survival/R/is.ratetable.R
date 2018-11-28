is.ratetable <- function(x, verbose=FALSE) {
    datecheck <- function(x) 
        inherits(x, c("Date", "POSIXt", "date", "chron"))
    att <- attributes(x)
    dlist <- c("dim", "dimnames", "cutpoints")
    isDate <- sapply(att$cutpoints, datecheck)
    dimid <- names(att$dimnames)
    if (is.null(dimid)) dimid <- att$dimid

    if (!verbose) {
	if (!inherits(x, 'ratetable')) return(FALSE)
	if (any(is.na(match(dlist, names(att))))) return(FALSE)
        if (is.null(att$dimid)) att$dimid <- names(att$dimnames)
	nd <- length(att$dim)
	if (length(x) != prod(att$dim)) return(FALSE)
        if (is.null(dimid)) return(FALSE)
        if (any(is.na(dimid) | dimid == "")) return(FALSE)
        
	if (!(is.list(att$dimnames) && is.list(att$cutpoints)))
		 return(FALSE)
	if (length(att$dimnames) != nd || length(att$cutpoints) != nd) return(FALSE)
        # One of 'factor' (old style table) or 'type' (new style) should exist
        if (!is.null(att$type)) {
            if (length(att$type) != nd) return(FALSE)
            if (any(is.na(match(att$type, 1:4)))) return(FALSE)
            if (sum(att$type==4) > 1) return(FALSE)
            if (any((att$type >2) != isDate)) return(FALSE)
            fac <- ifelse(att$type==1, 1, 0)  # is a 'factor'
        }
       else if (!is.null(att$factor)) {
            fac <- as.numeric(att$factor)
            if (any(is.na(fac))) return(FALSE)
            if (any(fac <0)) return(FALSE)
            if (length(att$factor)!=nd ) return(FALSE)
            if (sum(fac>1) >1) return(FALSE)  # more than 1 col marked as US year
            if (any(fac==1 & isDate)) return(FALSE)
        }
        else return(FALSE)

        if (length(att$dimid) != nd) return(FALSE)
	for (i in seq_len(nd)) {
	    n <- att$dim[i]
	    if (length(att$dimnames[[i]]) !=n) return(FALSE)
	    if (fac[i]!=1 && length(att$cutpoints[[i]])!=n) return(FALSE)
	    if (fac[i]!=1 && any(order(att$cutpoints[[i]])!= seq_len(n))) return(FALSE)
	    if (fac[i]==1 && !is.null(att$cutpoints[[i]]))  return(FALSE)
	    if (fac[i]>1 && i<nd) return(FALSE)
	    }
	return(TRUE)
	}

    # verbose return messages, useful for debugging
    msg <- NULL
    if (!inherits(x, 'ratetable')) msg <- c(msg, stop(gettextf("'%s' argument is not an object of class %s", "x", dQuote("ratetable"))))

    temp <- is.na(match(dlist, names(att)))
    if (any(temp)) 
        msg <- c(msg, paste(gettext("missing attribute:"), dlist[temp]))
    if (is.null(att$dimid)) att$dimid <- names(att$dimnames)

    # This next error is unlikely, since R itself squawks when you
    #   try to set a wrong dimension.  Ditto with dimnames issues.
    nd <- length(att$dim)
    if (length(x) != prod(att$dim)) 
        msg <- c(msg, gettext("length of the data does not match 'prod(dim)'"))

    if (!is.list(att$dimnames))
	     msg <- c(msg, gettextf("'%s' component is not a list", "dimnames"))
    if (!is.list(att$cutpoints))
	     msg <- c(msg, gettextf("'%s' component is not a list", "cutpoints"))

    if (length(att$dimnames)!=nd)
        msg <- c(msg, gettextf("wrong length for '%s' component", "dimnames"))
    if (length(att$dimid)!=nd)
        msg <- c(msg, gettextf("wrong length for '%s' component, or dimnames do not have names", "dimid"))
    if (any(att$dimid==""))
        msg <- c(msg, gettext("one of the dimnames identifiers is blank"))

    if (length(att$cutpoints)!=nd) 
        msg <- c(msg, gettextf("wrong length for '%s' component", "cutpoints"))

    if (!is.null(att$type)) {
        if (any(is.na(match(att$type, 1:4))))
            msg <- c(msg, gettext("'type' attribute must be 1, 2, 3, or 4"))
        type <- att$type
        if (length(type)!=nd)
            msg <- c(msg, gettext("wrong length for 'type' attribute"))
        else {
            indx <- which(type > 2 & ! isDate)
            if (length(indx) > 0)
                msg <- c(msg, gettextf("type[%d] is 3 or 4 but the cutpoint is not one of the date types", indx))
            indx <- which(type <3 & isDate)
            if (length(indx)>0) msg <- c(msg, gettextf("type[%d] is numeric or factor but the cutpoint is a date", indx))
        }
        if (sum(type==4) >1)
            msg <- c(msg, gettext("two dimenesions idenitied as US ratetable years")) 
    }
    else if (!is.null(att$factor)) {
        fac <- as.numeric(att$factor)
        if (any(is.na(fac))) msg <- c(msg, gettext("illegal 'factor' attribute of NA"))
        if (any(fac <0)) msg <- c(msg, gettext("illegal 'factor' attribute of <0"))
        if (length(att$factor)!=nd)
            msg <- c(msg, gettext('wrong length for factor'))
        type <- 1*(fac==1) + 2*(fac==0) + 4*(fac>1)
        if (sum(fac>1) >1)
            msg <- c(msg, gettext("two dimenesions idenitied as US ratetable years")) 
    }
   else msg <- c(msg, gettext("missing the 'type' attribute"))

    for (i in seq_len(nd)) {
	n <- att$dim[i]
	if (length(att$dimnames[[i]]) !=n) 
		msg <- c(msg, gettextf("dimname %d is the wrong length", i))

	if (type[i] >1) { #continuous variable
            if (length(att$cutpoints[[i]]) != n)
                msg <- c(msg, gettextf("wrong length for cutpoints %d", i))
            else if (any(order(att$cutpoints[[i]])!= seq_len(n))) 
		msg <- c(msg, gettextf("unsorted cutpoints for dimension %d", i))
                }

	if (type[i]==1 && !is.null(att$cutpoints[[i]]))  
		msg <- c(msg, gettextf("attribute type[ %d ] is continuous; cutpoint should be null", i))
        # This message only applies to old style rate table
	if (!is.null(att$fac) && type[i]==4 && i<nd) 
		msg <- c(msg, gettext("only the last dimension can be interpolated"))
	}
    if (length(msg)==0) TRUE
    else msg
    }
