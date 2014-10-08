# $Id$
#
# Test out if a distribution object found in survreg is legal.  Mostly called
#  by the survreg routine, but a user might use it when developing a new
#  distribution object
#
# Short form, returns just T or F
# Long form, returns all of the issues with the object, or T if it is ok
#
survregDtest <- function(dlist, verbose=F) {
    errlist <- NULL

    if (is.null(dlist$name)) errlist <- c(errlist, gettext("missing a distribution name"))
    else if (length(dlist$name) !=1 || !is.character(dlist$name))
        errlist <- c(errlist, gettext("invalid distribution name"))

    #
    # First case, the object is a reference to another distribution
    #
    if (!is.null(dlist$dist)) {
        if (!is.character(dlist$dist) || 
            is.null(match(dlist$dist, names(survreg.distributions))))
            errlist <- c(errlist, gettext("reference distribution was not found"))

        else {
            if (!is.function(dlist$trans))
                errlist <- c(errlist, gettextf("missing or invalid '%s' component", "trans"))
            if (!is.function(dlist$itrans))
                errlist <- c(errlist, gettextf("missing or invalid '%s' component", "itrans"))
            if (!is.function(dlist$dtrans))
                errlist <- c(errlist, gettextf("missing or invalid '%s' component", "dtrans"))
            }

        if (is.null(errlist)) {
            if (!all.equal(dlist$itrans(dlist$trans(1:10)), 1:10))
                errlist <- c(errlist, 
                             gettext("'trans' and 'itrans' components must be inverses of each other"))
            if (length(dlist$dtrans(1:10)) != 10)
                errlist <- c(errlist, gettext("'dtrans()' component must be a 1-1 function"))
            }
        }

    # Second case, the actual definition of a distribution
    else {
	# Comment out the next line, until some function uses the variance
	#if (!is.function(dlist$variance))
	#    errlist <- c(errlist, "Missing or invalid variance function")
	if (!is.function(dlist$init))
	    errlist <- c(errlist, gettextf("missing or invalid '%s' function", "init"))
	if (!is.function(dlist$deviance))
	    errlist <- c(errlist, gettextf("missing or invalid '%s' function", "deviance"))
	if (!is.function(dlist$density))
	    errlist <- c(errlist, gettextf("missing or invalid '%s' function", "density"))
	else {
	    if (is.null(dlist$parms))
		    temp <- dlist$density(1:10/10)
	    else    temp <- dlist$density(1:10/10, unlist(dlist$parms))
	    if (!is.numeric(temp) || !is.matrix(temp) ||
		nrow(temp) != 10 || ncol(temp) != 5)
	         errlist <- c(errlist, 
			     gettext("density function must return a 5 column matrix"))
	    }
	
	if (!is.function(dlist$quantile))
	    errlist <- c(errlist, gettextf("missing or invalid '%s' function", "quantile"))
	}

    if (is.null(errlist)) T
    else if (verbose) errlist else F
    }

