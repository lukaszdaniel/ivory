#  File src/library/base/R/all.equal.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2014 The R Core Team
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
#  http://www.r-project.org/Licenses/

all.equal <- function(target, current, ...) UseMethod("all.equal")

all.equal.default <-
    function(target, current, ...)
{
    ## Really a dispatcher given mode() of args :
    ## use data.class as unlike class it does not give "integer"
    if(is.language(target) || is.function(target))
	return(all.equal.language(target, current, ...))
    if(is.environment(target) || is.environment(current))# both: unclass() fails on env.
	return(all.equal.environment(target, current, ...))
    if(is.recursive(target))
	return(all.equal.list(target, current, ...))
    msg <- switch (mode(target),
                   integer = ,
                   complex = ,
                   numeric = all.equal.numeric(target, current, ...),
                   character = all.equal.character(target, current, ...),
                   logical = ,
                   raw = all.equal.raw(target, current, ...),
		   ## assumes that slots are implemented as attributes :
		   S4 = attr.all.equal(target, current, ...),
                   if(data.class(target) != data.class(current)) {
                       gettextf("'target' argument's class is %s, 'current' argument's class is %s",
                                data.class(target), data.class(current))
                   } else NULL)
    if(is.null(msg)) TRUE else msg
}

all.equal.numeric <-
    function(target, current, tolerance = .Machine$double.eps ^ .5,
             scale = NULL, ..., check.attributes = TRUE)
{
    if (!is.numeric(tolerance))
        stop(gettextf("'%s' argument should be numeric", "tolerance"))
    if (!is.numeric(scale) && !is.null(scale))
        stop(gettextf("'%s' argument should be numeric or NULL", "scale"))
    if (!is.logical(check.attributes))
        stop(gettextf("'%s' argument must be logical", "check.attributes"), domain = "R-base")
    msg <- if(check.attributes)
	attr.all.equal(target, current, tolerance = tolerance, scale = scale,
                       ...)
    if(data.class(target) != data.class(current)) {
	msg <- c(msg, gettextf("'target' argument's class is %s, 'current' argument's class is %s", data.class(target), data.class(current)))
	return(msg)
    }

    lt <- length(target)
    lc <- length(current)
    cplx <- is.complex(target) # and so current must be too.
    if(lt != lc) {
	## *replace* the 'Lengths' msg[] from attr.all.equal():
	if(!is.null(msg)) msg <- msg[- grep("\\bLengths\\b", msg)]
	if(cplx) {
	msg <- c(msg, gettextf("Complex: lengths (%d, %d) differ", lt, lc))
	} else {
	msg <- c(msg, gettextf("Numeric: lengths (%d, %d) differ", lt, lc))
	}
	return(msg)
    }
    ## remove atttributes (remember these are both numeric or complex vectors)
    ## one place this is needed is to unclass Surv objects in the rpart test suite.
    target <- as.vector(target)
    current <- as.vector(current)
    out <- is.na(target)
    if(any(out != is.na(current))) {
	msg <- c(msg, gettextf("'is.NA' value mismatch: %d in 'current' argument, %d in 'target' argument", sum(is.na(current)), sum(out)))
	return(msg)
    }
    out <- out | target == current
    if(all(out)) { if (is.null(msg)) return(TRUE) else return(msg) }

    target <- target[!out]
    current <- current[!out]
    if(is.integer(target) && is.integer(current)) target <- as.double(target)
    xy <- mean((if(cplx) Mod else abs)(target - current))

	if(is.null(scale)) {
	    xn <- mean(abs(target))
	    if(is.finite(xn) && xn > tolerance) {
		xy <- xy/xn
	    }
	} else {
	    xy <- xy/scale

	}


    if(is.na(xy) || xy > tolerance) {
	 if(cplx) {# PR#10575
	  if(is.null(scale) && is.finite(xn) && xn > tolerance) {
        msg <- c(msg, paste(gettext("Mean relative Mod difference:", domain = "R-base"), format(xy)))
	  } else if(is.null(scale)) {
	    msg <- c(msg, paste(gettext("Mean absolute Mod difference:", domain = "R-base"), format(xy)))
	  } else if(scale == 1) {
	    msg <- c(msg, paste(gettext("Mean absolute Mod difference:", domain = "R-base"), format(xy)))
	 } else {
        msg <- c(msg, paste(gettext("Mean scaled Mod difference:", domain = "R-base"), format(xy)))
	 }
} else {
	  if(is.null(scale) && is.finite(xn) && xn > tolerance) {
        msg <- c(msg, paste(gettext("Mean relative difference:", domain = "R-base"), format(xy)))
	  } else if(is.null(scale)) {
	    msg <- c(msg, paste(gettext("Mean absolute difference:", domain = "R-base"), format(xy)))
	  } else if(scale == 1) {
	    msg <- c(msg, paste(gettext("Mean absolute difference:", domain = "R-base"), format(xy)))
      } else {
	    msg <- c(msg, paste(gettext("Mean scaled difference:", domain = "R-base"), format(xy)))
	}
	} }
    if(is.null(msg)) TRUE else msg
}

all.equal.character <-
    function(target, current, ..., check.attributes = TRUE)
{
    if (!is.logical(check.attributes))
        stop(gettextf("'%s' argument must be logical", "check.attributes"), domain = "R-base")
    msg <-  if(check.attributes) attr.all.equal(target, current, ...)
    if(data.class(target) != data.class(current)) {
	msg <- c(msg, gettextf("'target' argument's class is %s, 'current' argument's class is %s", data.class(target), data.class(current)))
	return(msg)
    }
    lt <- length(target)
    lc <- length(current)
    if(lt != lc) {
	if(!is.null(msg)) msg <- msg[- grep("\\bLengths\\b", msg)]
	msg <- c(msg,
		gettextf("Lengths (%d, %d) differ (string compare on first %d)", lt, lc, ll <- min(lt, lc)))
	ll <- seq_len(ll)
	target <- target[ll]
	current <- current[ll]
    }
    nas <- is.na(target); nasc <- is.na(current)
    if (any(nas != nasc)) {
	msg <- c(msg, gettextf("'is.NA' value mismatch: %d in 'current' argument, %d in 'target' argument", sum(nasc), sum(nas)))
	return(msg)
    }
    ne <- !nas & (target != current)
    if(!any(ne) && is.null(msg)) TRUE
    else if(sum(ne) >= 1L) c(msg, sprintf(ngettext(sum(ne), "%d string mismatch", "%d string mismatches"), sum(ne)))
    else msg
}

## In 'base' these are all visible, so need to test both args:

all.equal.envRefClass <- function (target, current, ...) {
    if(!is (target, "envRefClass")) return(gettextf("'%s' argument is not an envRefClass", "target", domain = "R-base"))
    if(!is(current, "envRefClass")) return(gettextf("'%s' argument is not an envRefClass", "current", domain = "R-base"))
    if(!isTRUE(ae <- all.equal(class(target), class(current), ...)))
	return(gettextf("Classes differ: %s", paste(ae, collapse = " ")))
    getCl <- function(x) { cl <- tryCatch(x$getClass(), error=function(e) NULL)
			   if(is.null(cl)) class(x) else cl }
    if(!identical(cld <- getCl(target), c2 <- getCl(current))) {
	hasCA <- any("check.attributes" == names(list(...)))
	ae <-
	    if(hasCA) all.equal(cld, c2, ...)
	    else all.equal(cld, c2, check.attributes=FALSE, ...)
        if(isTRUE(ae) && !hasCA) ae <- all.equal(cld, c2, ...) #LUKI
	return(gettextf("Class definitions are not identical%s",
		       if(isTRUE(ae)) "" else paste(":", ae, collapse=" ")))
    }
    if(!isS4(cld)) ## prototype / incomplete
	return(if(identical(target, current)) TRUE
	       else "different prototypical 'envRefClass' objects")
    flds <- names(cld@fieldClasses) ## else NULL
    asL <- function(O) sapply(flds, function(ch) O[[ch]], simplify = FALSE)
    ## ## ?setRefClass explicitly says users should not use ".<foo>" fields:
    ## if(is.na(all.names)) all.names <- FALSE
    ## ## try preventing infinite recursion by not looking at  .self :
    ## T <- function(ls) ls[is.na(match(names(ls), c(".self", methods:::.envRefMethods)))]
    ## asL <- function(E) T(as.list(as.environment(E), all.names=all.names, sorted=TRUE))
    n <- all.equal.list(asL(target), asL(current), ...)
    ## Can have slots (apart from '.xData'), though not recommended; check these:
    sns <- names(cld@slots); sns <- sns[sns != ".xData"]
    msg <- if(length(sns)) {
	L <- lapply(sns, function(sn)
	    all.equal(slot(target, sn), slot(current, sn), ...))
	unlist(L[vapply(L, is.character, NA)])
    }
    if(is.character(n)) msg <- c(msg, n)
    if(is.null(msg)) TRUE else msg
}

all.equal.environment <- function (target, current, all.names=TRUE, ...) {
    if(!is.environment (target)) return( "'target' is not an environment")
    if(!is.environment(current)) return("'current' is not an environment")
    ae.run <- dynGet("__all.eq.E__", NULL)
    if(is.null(ae.run))
	"__all.eq.E__" <- environment() # -> 5 visible + 6 ".<..>" objects
    else { ## ae.run contains previous target, current, ..

	## If we exactly match one of these, we return TRUE here,
	## otherwise, divert to all.equal(as.list(.), ...) below

	## needs recursive function -- a loop with  em <- em$mm	 destroys the env!
	do1 <- function(em) {
	    if(identical(target, em$target) && identical(current, em$current))
		TRUE
	    else if(!is.null(em$ mm)) ## recurse
		do1(em$ mm)
	    else {
		## add the new (target, current) pair, and return FALSE
		e <- new.env(parent = emptyenv())
		e$target  <- target
		e$current <- current
		em$ mm <- e
		FALSE
	    }
	}

	if(do1(ae.run)) return(TRUE)
	## else, continue:
    }
    all.equal.list(as.list.environment(target , all.names=all.names, sorted=TRUE),
		   as.list.environment(current, all.names=all.names, sorted=TRUE), ...)
}

all.equal.factor <- function(target, current, ..., check.attributes = TRUE)
{
    if(!inherits(target, "factor"))
	return(gettextf("'%s' argument is not a factor", "target"))
    if(!inherits(current, "factor"))
	return(gettextf("'%s' argument is not a factor", "current"))
    msg <-  if(check.attributes) attr.all.equal(target, current, ...)
    n <- all.equal(as.character(target), as.character(current),
                   check.attributes = check.attributes, ...)
    if(is.character(n)) msg <- c(msg, n)
    if(is.null(msg)) TRUE else msg
}

all.equal.formula <- function(target, current, ...)
{
    ## NB: this assumes the default method for class formula, not
    ## the misguided one in package Formula
    if(length(target) != length(current))
	return(gettextf("'target' and 'current' arguments differ in having response: %s, %s", length(target) == 3L, length(current) == 3L))
    ## <NOTE>
    ## This takes same-length formulas as all equal if they deparse
    ## identically.  As of 2010-02-24, deparsing strips attributes; if
    ## this is changed, the all equal behavior will change unless the
    ## test is changed.
    ## </NOTE>
    if(!identical(deparse(target), deparse(current)))
	gettext("formulas differ in contents")
    else TRUE
}

all.equal.language <- function(target, current, ...)
{
    mt <- mode(target)
    mc <- mode(current)
    if(mt == "expression" && mc == "expression")
	return(all.equal.list(target, current, ...))
    ttxt <- paste(deparse(target), collapse = "\n")
    ctxt <- paste(deparse(current), collapse = "\n")
    msg <- c(if(mt != mc)
	     paste0(gettext("Modes of 'target' and 'current' arguments: ", domain = "R-base"), mt, ", ", mc),
	     if(ttxt != ctxt) {
		 if(pmatch(ttxt, ctxt, 0L)) {
		     gettext("'target' argument is a subset of 'current' argument")
		 } else if(pmatch(ctxt, ttxt, 0L)) {
		     gettext("'current' argument is a subset of 'target' argument")
		 } else {gettext("'target' and 'current' arguments do not match when deparsed") }
	     })
    if(is.null(msg)) TRUE else msg
}

## use.names is new in 3.1.0: avoid partial/positional matching
all.equal.list <- function(target, current, ...,
                           check.attributes = TRUE, use.names = TRUE)
{
    if (!is.logical(check.attributes))
        stop(gettextf("'%s' argument must be logical", "check.attributes"),
             domain = "R-base")
    if (!is.logical(use.names))
        stop(gettextf("'%s' argument must be logical", "use.names"), domain = "R-base")
    msg <- if(check.attributes) attr.all.equal(target, current, ...)
    ## Unclass to ensure we get the low-level components
    target <- unclass(target) # "list"
    current <- unclass(current)# ??
    ## Comparing the data.class() is not ok, as a list matrix is 'matrix' not 'list'
    if(!is.list(target) && !is.vector(target))
	return(c(msg, gettext("'target' argument's class is not list-like")))
    if(!is.list(current) && !is.vector(current))
	return(c(msg, gettext("'current' argument's class is not list-like")))
    if((n <- length(target)) != length(current)) {
	if(!is.null(msg)) msg <- msg[- grep("\\bLengths\\b", msg)]
	n <- min(n, length(current))
	msg <- c(msg, gettextf("Length mismatch: comparison on first %d components", n))
    }
    iseq <- seq_len(n)
    if(use.names)
	use.names <- (length(nt <- names(target)[iseq]) == n &&
		      length(nc <- names(current)[iseq]) == n)
    for(i in iseq) {
	mi <- all.equal(target[[i]], current[[i]],
			check.attributes=check.attributes, use.names=use.names, ...)
	if(is.character(mi))
	    msg <- c(msg, paste0("Component ",
				 if(use.names && nt[i] == nc[i]) dQuote(nt[i]) else i,
				 ": ", mi))
    }
    if(is.null(msg)) TRUE else msg
}

## also used for logical
all.equal.raw <-
    function(target, current, ..., check.attributes = TRUE)
{
    if (!is.logical(check.attributes))
        stop(gettextf("'%s' argument must be logical", "check.attributes"), domain = "R-base")
    msg <-  if(check.attributes) attr.all.equal(target, current, ...)
    if(data.class(target) != data.class(current)) {
	msg <- c(msg, gettextf("'target' argument's class is %s, 'current' argument's class is %s", data.class(target), data.class(current)))
	return(msg)
    }
    lt <- length(target)
    lc <- length(current)
    if(lt != lc) {
	if(!is.null(msg)) msg <- msg[- grep("\\bLengths\\b", msg)]
	msg <- c(msg, gettextf("Lengths (%d, %d) differ (comparison on first %d components)", lt, lc, ll <- min(lt, lc)))
	ll <- seq_len(ll)
	target <- target[ll]
	current <- current[ll]
    }
    # raws do not have NAs, but logicals do
    nas <- is.na(target); nasc <- is.na(current)
    if (any(nas != nasc)) {
	msg <- c(msg, gettextf("'is.NA' value mismatch: %d in 'current' argument, %d in 'target' argument", sum(nasc), sum(nas)))
	return(msg)
    }
    ne <- !nas & (target != current)
    if(!any(ne) && is.null(msg)) TRUE
    else if(sum(ne) >= 1L) c(msg, sprintf(ngettext(sum(ne), "%d element mismatch", "%d element mismatches"), sum(ne)))
    else msg
}


## attributes are a pairlist, so never 'long'
attr.all.equal <- function(target, current, ...,
                           check.attributes = TRUE, check.names = TRUE)
{
    ##--- "all.equal(.)" for attributes ---
    ##---  Auxiliary in all.equal(.) methods --- return NULL or character()
    if (!is.logical(check.attributes))
        stop(gettextf("'%s' argument must be logical", "check.attributes"), domain = "R-base")
    if (!is.logical(check.names))
        stop(gettextf("'%s' argument must be logical", "check.names"), domain = "R-base")
    msg <- NULL
    if(mode(target) != mode(current))
	msg <- paste0(gettext("Modes: ", domain = "R-base"), mode(target), ", ", mode(current), collapse = "")
    if(length(target) != length(current))
	msg <- c(msg,
                 paste0(gettext("Lengths: ", domain = "R-base"), length(target), ", ", length(current), collapse = ""))
    ax <- attributes(target)
    ay <- attributes(current)
    if(check.names) {
        nx <- names(target)
        ny <- names(current)
        if((lx <- length(nx)) | (ly <- length(ny))) {
            ## names() treated now; hence NOT with attributes()
            ax$names <- ay$names <- NULL
            if(lx && ly) {
                if(is.character(m <- all.equal.character(nx, ny, check.attributes = check.attributes)))
                    msg <- c(msg, paste(gettext("Names:", domain = "R-base"), m))
            } else if(lx)
                 msg <- c(msg, gettextf("names for '%s' argument but not for '%s' argument", "target", "current", domain = "R-base"))
            else msg <- c(msg, gettextf("names for '%s' argument but not for '%s' argument", "current", "target", domain = "R-base"))
        }
    } else {
	ax[["names"]] <- NULL
	ay[["names"]] <- NULL
    }

    if(check.attributes && (length(ax) || length(ay))) {# some (more) attributes
	## order by names before comparison:
	nx <- names(ax)
	ny <- names(ay)
	if(length(nx)) ax <- ax[order(nx)]
	if(length(ny)) ay <- ay[order(ny)]
	tt <- all.equal(ax, ay, ..., check.attributes = check.attributes)
#        if(is.character(tt)) msg <- c(msg, paste("Attributes: <", tt, ">"))
	if(is.character(tt)) msg <- c(msg, paste(gettext("Attributes:", domain = "R-base"), paste("<", tt, ">")))
    }
    msg # NULL or character
}

## formerly in datetime.R
## force absolute comparisons
all.equal.POSIXt <- function(target, current, ..., tolerance = 1e-3, scale)
{
    target <- as.POSIXct(target); current <- as.POSIXct(current)
    check_tzones(target, current)
    attr(target, "tzone") <- attr(current, "tzone") <- NULL
    all.equal.numeric(target, current, ..., tolerance = tolerance, scale = 1)
}
