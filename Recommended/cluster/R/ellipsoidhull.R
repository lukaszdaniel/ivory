#### ellipsoidhull : Find (and optionally draw)
#### -----------   the smallest ellipsoid containining a set of points
####
#### Just making the algorithms in clusplot() available more generally
#### ( --> ./plotpart.q )

### Author: Martin Maechler, Date: 21 Jan 2002, 15:41

ellipsoidhull <-
    function(x, tol = 0.01, maxit = 5000,
             ret.wt = FALSE, ret.sqdist = FALSE, ret.pr = FALSE)
{
    if(!is.matrix(x) || !is.numeric(x))
        stop("'x' must be numeric  n x p matrix")
    if(anyNA(x)) {
        warning("omitting NAs")
        x <- na.omit(x)
    }
    n <- nrow(x)
    if(n == 0) stop("no points without missing values")
    p <- ncol(x)

    res <- .C(spannel,
              n,
              ndep= p,
              dat = cbind(1., x),
              sqdist = double(n),
              l1 = double((p+1) ^ 2),
              double(p),
              double(p),
              prob = double(n),
              double(p+1),
              eps = as.double(tol),
              maxit = as.integer(maxit),
              ierr = integer(1))# 0 or non-zero
    if(res$ierr != 0)
        cat(gettext("Error in Fortran routine computing the spanning ellipsoid. Probably collinear data", domain = "R-cluster"), "\n", sep="")
    if(any(res$prob < 0) || all(res$prob == 0))
        stop("computed some negative or all 0 probabilities")
    conv <- res$maxit  < maxit
    if(!conv)
        warning(gettextf("algorithm possibly not converged in %d iterations", maxit))
    conv <- conv && res$ierr == 0

    cov <- cov.wt(x, res$prob)
    ## cov.wt() in R has extra wt[] scaling; revert here
    res <- list(loc = cov$center,
                cov = cov$cov * (1 - sum(cov$wt^2)),
                d2  = weighted.mean(res$sqdist, res$prob),
                wt  = if(ret.wt) cov$wt,
                sqdist = if(ret.sqdist) res$sqdist,
                prob= if(ret.pr) res$prob,
                tol = tol,
                eps = max(res$sqdist) - p,
                it  = res$maxit,
                maxit= maxit,
                ierr = res$ierr,
                conv = conv)
    class(res) <- "ellipsoid"
    res
}


print.ellipsoid <- function(x, digits = max(1, getOption("digits") - 2), ...)
{
    d <- length(x$loc)
    cat(gettextf("ellipsoid in %d dimensions:\n center = (%s); squared ave.radius d^2 = %s\n and shape matrix =\n", d, paste(format(x$loc, digits=digits), collapse = " "), format(x$d2, digits=digits), domain = "R-cluster"))
    print(x$cov, digits = digits, ...)
    Vx <- volume(x)
    chV <- if(!is.finite(Vx))
        paste0("exp(", format(volume(x, log=TRUE), digits=digits),")")
    else
        format(Vx, digits=digits)
    if(d==2) cat(gettextf("  ellipsoid's area = %s", chV, domain = "R-cluster"), "\n", sep = "")
	else cat(gettextf("  ellipsoid's volume = %s", chV, domain = "R-cluster"), "\n", sep = "")
    if(!is.null(x$conv) && !x$conv) {
        if(x$ierr)
          cat("\n", gettext("** Warning: ** the algorithm did not terminate reliably!\n  most probably because of collinear data", domain = "R-cluster"), "\n", sep = "")
        else
	  cat("\n", gettext("** Warning: ** the algorithm did not terminate reliably!\n  (in the available number of iterations)", domain = "R-cluster"), "\n", sep = "")
    }
    invisible(x)
}

volume <- function(object, ...) UseMethod("volume")

if(FALSE) ## correct only for dimension d = 2  -- was used up to May 2019 :
volume.ellipsoid <- function(object) {
    A <- object$cov
    pi * object$d2 * sqrt(det(A))
}

## modified MM from a proposal by Keefe Murphy, e-mail 2019-05-15
volume.ellipsoid <- function(object, log=FALSE, ...) {
    stopifnot((p <- length(object$loc)) >= 1)
    lDet2 <- as.numeric(determinant(object$cov)$modulus) / 2 # = log(sqrt(det(.)))
    lV <- p/2 * log(pi * object$d2) + lDet2 - lgamma(p/2 + 1)
    if(log) lV else exp(lV)
}


## For p = 2 :
##   Return (x[i],y[i]) points, i = seq_len(n), on boundary of ellipse, given
##   by 2 x 2 matrix A[], origin 'loc' and d(xy, loc) ^2 = 'd2'
ellipsoidPoints <- function(A, d2, loc, n.half = 201)
{
    if(length(d <- dim(A)) != 2 || (p <- d[1]) != d[2])
        stop("'A' must be p x p  cov-matrix defining an ellipsoid")
    if(p == 2) {
        detA <- A[1, 1] * A[2, 2] - A[1, 2]^2
        yl2 <- A[2, 2] * d2 # = (y_max - y_loc)^2
        y <- seq( - sqrt(yl2), sqrt(yl2), length = n.half)
        sqrt.discr <- sqrt(detA * pmax(0, yl2 - y^2))/A[2, 2]
        sqrt.discr[c(1, n.half)] <- 0
        b <- loc[1] + A[1, 2]/A[2, 2] * y
        y <- loc[2] + y
        return(rbind(cbind(    b - sqrt.discr,      y),
                     cbind(rev(b + sqrt.discr), rev(y))))
    } else { ## p >= 3
        detA <- det(A)
        ##-- need something like polar coordinates
        stop("ellipsoidPoints() not yet implemented for p >= 3 dim.")
    }
}

predict.ellipsoid <- function(object, n.out = 201, ...)
    ellipsoidPoints(object$cov, d2 = object$d2, loc= object$loc, n.half = n.out)
