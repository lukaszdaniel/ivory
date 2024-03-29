#  File src/library/stats/R/contr.poly.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2019 The R Core Team
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

contr.poly <- function (n, scores = seq_len(n), contrasts = TRUE, sparse = FALSE)
{
## sparse.model.matrix() may call this one with sparse=TRUE anyway ..
##     if(sparse)
## 	stop("orthogonal polynomial contrasts cannot be sparse")
    make.poly <- function(n, scores)
    {
	y <- scores - mean(scores)
	X <- outer(y, seq_len(n) - 1, "^")
	QR <- qr(X)
	z <- QR$qr
	z <- z *(row(z) == col(z))
	raw <- qr.qy(QR, z)
	Z <- sweep(raw, 2L, apply(raw, 2L, function(x) sqrt(sum(x^2))), "/",
		   check.margin=FALSE)
	colnames(Z) <- paste0("^", seq_len(n) - 1L)
	Z
    }

    if (is.numeric(n) && length(n) == 1L) levs <- seq_len(n)
    else {
	levs <- n
	n <- length(levs)
    }
    if (n < 2)
        stop(gettextf("contrasts not defined for %d degrees of freedom", n - 1), domain = "R-stats")
    if (n > 95)
        stop(gettextf("orthogonal polynomials cannot be represented accurately enough for %d degrees of freedom", n-1), domain = "R-stats")
    if (length(scores) != n)
        stop(gettextf("'%s' argument is of the wrong length", "scores"))
    if (!is.numeric(scores) || anyDuplicated(scores))
        stop("'scores' must all be different numbers")
    contr <- make.poly(n, scores)
    if(sparse) contr <- .asSparse(contr)
    if (contrasts) {
	dn <- colnames(contr)
	dn[2:min(4,n)] <- c(".L", ".Q", ".C")[seq_len(min(3, n-1))]
	colnames(contr) <- dn
	contr[, -1, drop = FALSE]
    }
    else {
	contr[, 1] <- 1
	contr
    }
}

poly <- function(x, ..., degree = 1, coefs = NULL, raw = FALSE, simple = FALSE)
{
    dots <- list(...)
    if(nd <- length(dots)) {
	dots_deg <- nd == 1L && length(dots[[1L]]) == 1L
	if(dots_deg) # unnamed degree, nothing else in '...'
            degree <- dots[[1L]]
        else return(polym(x, ..., degree = degree, coefs=coefs, raw = raw))
    }
    if(is.matrix(x)) {
	m <- unclass(as.data.frame(if(nd && dots_deg) x else cbind(x, ...)))
	return(do.call(polym, c(m, degree = degree, raw = raw,
				list(coefs=coefs))))
    }
    if(degree < 1)
        stop(gettextf("'%s' argument must be at least %d", "degree", 1))
    if(raw) {
        Z <- outer(x, seq_len(degree), "^")
        colnames(Z) <- seq_len(degree)
    } else {
	if(is.null(coefs)) { # fitting
	    if(anyNA(x)) stop("missing values are not allowed in 'poly()'")
	    if(degree >= length(unique(x)))
		stop("'degree' must be less than number of unique points")
	    xbar <- mean(x)
	    x <- x - xbar
	    X <- outer(x, 0L:degree, "^")
	    QR <- qr(X)
	    if(QR$rank < degree)
		stop("'degree' must be less than number of unique points")
	    z <- QR$qr
	    z <- z * (row(z) == col(z))
	    Z <- qr.qy(QR, z)
	    norm2 <- colSums(Z^2)
	    alpha <- (colSums(x*Z^2)/norm2 + xbar)[seq_len(degree)]
	    norm2 <- c(1, norm2) # to use "common" code below
	} else {            # prediction
	    alpha <- coefs$alpha; norm2 <- coefs$norm2
	    Z <- matrix(1, length(x), degree + 1L) # Z[,1] == 1
	    Z[, 2] <- x - alpha[1L]
	    if(degree > 1)
		for(i in 2:degree)
		    Z[, i+1] <- (x - alpha[i]) * Z[, i]  -
				    (norm2[i+1] / norm2[i]) * Z[, i-1]
        }
        Z <- Z / rep(sqrt(norm2[-1L]), each = length(x))
        colnames(Z) <- 0L:degree
        Z <- Z[, -1, drop = FALSE]
        if(!simple) ## we may want to use the prediction to clone another prediction
            attr(Z, "coefs") <- list(alpha = alpha, norm2 = norm2)
    }
    if(simple) Z else structure(Z, degree = seq_len(degree), class = c("poly", "matrix"))
}

predict.poly <- function(object, newdata, ...)
{
    if(missing(newdata))
	object
    else if(is.null(attr(object, "coefs")))
	poly(newdata, degree = max(attr(object, "degree")),
             raw = TRUE, simple = TRUE)
    else
	poly(newdata, degree = max(attr(object, "degree")),
	     coefs = attr(object, "coefs"), simple = TRUE)
}

makepredictcall.poly  <- function(var, call)
{
    if(as.character(call)[1L] == "poly" || (is.call(call) && identical(eval(call[[1L]]), poly)))
	call$coefs <- attr(var, "coefs")
    call
}

polym <- function (..., degree = 1, coefs = NULL, raw = FALSE)
{
    dots <- list(...)
    nd <- length(coefs %||% dots)  # number of variables
    if (nd == 0)
        stop("must supply one or more vectors")
    ## z:= combinations of (0:degree) of all variables
    z <- do.call(expand.grid,
                 c(rep.int(list(0:degree), nd), KEEP.OUT.ATTRS = FALSE))
    ## sum of all degrees must be in  1:degree :
    s <- rowSums(z)
    ind <- 0 < s  &  s <= degree
    z <- z[ind, , drop=FALSE]
    s <- s[ind]
    if(is.null(coefs)) {
	aPoly <- poly(dots[[1L]], degree, raw = raw, simple = raw && nd > 1)
	if (nd == 1)
	    return(aPoly)
	## nd >= 2 from here on
	n <- lengths(dots)
	if (any(n != n[1L]))
	    stop("arguments must have the same length")
	res <- cbind(1, aPoly)[, 1L +z[, 1], drop=FALSE]
	## attribute "coefs" = list of coefs from individual variables
	if (!raw) coefs <- list(attr(aPoly, "coefs"))
	for (i in 2:nd) {
	    aPoly <- poly(dots[[i]], degree, raw = raw, simple = raw)
	    res <- res * cbind(1, aPoly)[, 1L +z[, i], drop=FALSE]
	    if (!raw) coefs <- c(coefs, list(attr(aPoly, "coefs")))
	}
	colnames(res) <- apply(z, 1L, function(x) paste(x, collapse = "."))
	structure(res,
		  degree =  as.vector(s),
		  coefs = if (!raw) coefs,
		  class = c("poly", "matrix"))
    } else { ## use 'coefs' for prediction
	newdata <- as.data.frame(dots) # new data
	if (nd != ncol(newdata))
	    stop("wrong number of columns in new data: ", deparse1(substitute(...)))
	res <- 1
	for (i in seq_len(nd))
            res <- res*cbind(1, poly(newdata[[i]], degree=degree,
				     coefs=coefs[[i]], simple=TRUE))[, 1L +z[, i], drop=FALSE]
	colnames(res) <- apply(z, 1L, function(x) paste(x, collapse = "."))
        ## no 'coefs' and 'degree', nor "poly" class
	res
    }
}
