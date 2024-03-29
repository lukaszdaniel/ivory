# file lqs/R/lqs.R
# copyright (C) 1998-2014 B. D. Ripley
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 or 3 of the License
#  (at your option).
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/
#

lqs <- function(x, ...) UseMethod("lqs")

lqs.formula <-
    function(formula, data, ...,
	     method = c("lts" ,"lqs", "lms", "S", "model.frame"),
	     subset, na.action,
	     model = TRUE, x.ret = FALSE, y.ret = FALSE, contrasts = NULL)
{
    method <- match.arg(method)
    mf <- match.call(expand.dots = FALSE)
    mf$method <- mf$contrasts <- mf$model <- mf$x.ret <- mf$y.ret <- mf$... <- NULL
    mf[[1L]] <- quote(stats::model.frame)
    mf <- eval.parent(mf)
    if (method == "model.frame") return(mf)
    mt <- attr(mf, "terms")
    y <- model.extract(mf, "response")
    offset <- model.offset(mf)
    if(!is.null(offset)) y <- y - offset
    x <- model.matrix(mt, mf, contrasts)
    contr <- attr(x, "contrasts")
    xint <- match("(Intercept)", colnames(x), nomatch = 0L)
    if(xint) x <- x[, -xint, drop = FALSE]
    fit <- lqs.default(x, y, intercept = (xint > 0), method = method, ...)
    fit$terms <- mt
    fit$call <- match.call()
    fit$contrasts <- contr
    fit$xlevels <- .getXlevels(mt, mf)
    fit$na.action <- attr(mf, "na.action")
    if(model) fit$model <- mf
    if(x.ret) fit$x <- x
    if(y.ret) fit$y <- y
    fit
}

lqs.default <-
    function(x, y, intercept = TRUE, method = c("lts", "lqs", "lms", "S"),
	     quantile, control = lqs.control(...), k0 = 1.548, seed, ...)
{
    lqs.control <- function(psamp = NA, nsamp = "best", adjust = TRUE)
	list(psamp = psamp, nsamp = nsamp, adjust = adjust)

    n <- length(y)
    nmx <- deparse(substitute(x))
    if(is.null(dim(x))) {
	x <- as.matrix(x)
	colnames(x) <- nmx
    } else x <- as.matrix(x)
    p <- ncol(x)
    if(any(is.na(x)) || any(is.na(y)))
	stop("missing values are not allowed")
    nm <- colnames(x)
    if(is.null(nm))
	nm <- if(p > 1) paste("X", seq_len(p), sep = "") else if(p == 1) "X" else NULL
    if(intercept) {
        att <- attr(x, "contrasts")
	x <- cbind(1, x)
	nm <- c("(Intercept)", nm)
        attr(x, "contrasts") <- att
    }
    p <- ncol(x)
    if(nrow(x) != n) stop("'x' and 'y' must have the same number of rows")
    method <- match.arg(method)
    lts <- 0; beta <- 0
    if(method == "lqs" && missing(quantile)) quantile <- floor((n+p+1)/2)
    if(method == "lms") quantile <- floor((n+1)/2)
    if(method == "lts") {
	lts <- 1
	if(missing(quantile)) quantile <- floor(n/2) + floor((p+1)/2)
    }
    if(method == "S") {
	lts <- 2
	beta <- 0.5
	quantile <- ceiling(n/2)
	chi <- function(u, k0)
	{ u <- (u/k0)^2; ifelse(u < 1, 3*u - 3*u^2 + u^3, 1) }
    }
    if(quantile > n-1)
        stop(gettextf("'quantile' must be at most %d", n-1), domain = "R-MASS")
    ps <- control$psamp
    if(is.na(ps)) ps <- p
    if(ps < p) {
	ps <- p
	warning("'ps' must be at least 'p'")
    }
    adj <- control$adjust & intercept
    nsamp <- eval(control$nsamp)
    nexact <- choose(n, ps)
    if(is.character(nsamp) && nsamp == "best") {
	nsamp <- if(nexact < 5000) "exact" else "sample"
    } else if(is.numeric(nsamp) && nsamp > nexact) {
        warning(sprintf(ngettext(nexact,
                                 "only %d set, so all sets will be tried",
                                 "only %d sets, so all sets will be tried", domain = "R-MASS"),
                        nexact), domain = NA)
	nsamp <- "exact"
    }
    samp <- nsamp != "exact"
    if(samp) {
	if(nsamp == "sample") nsamp <- min(500*ps, 3000)
    } else
	nsamp <- nexact

    if(samp && !missing(seed)) {
	if(exists(".Random.seed", envir=.GlobalEnv, inherits=FALSE))  {
	    seed.keep <- get(".Random.seed", envir=.GlobalEnv, inherits=FALSE)
	    on.exit(assign(".Random.seed", seed.keep, envir=.GlobalEnv))
	}
	assign(".Random.seed", seed, envir=.GlobalEnv)
    }
    z <-  .C(lqs_fitlots,
	     as.double(x), as.double(y), as.integer(n), as.integer(p),
	     as.integer(quantile), as.integer(lts), as.integer(adj),
	     as.integer(samp), as.integer(ps), as.integer(nsamp),
	     crit=double(1), sing=integer(1L), bestone=integer(ps),
	     coefficients=double(p), as.double(k0), as.double(beta)
	     )[c("crit", "sing", "coefficients", "bestone")]
    if(z$sing == nsamp)
        stop("'lqs' failed: all the samples were singular", call.=FALSE)
    z$sing <- sprintf(ngettext(z$sing, "%d singular sample of size %d out of %d", "%d singular samples of size %d out of %d", domain = "R-MASS"), z$sing, ps, nsamp)
    z$bestone <- sort(z$bestone)
    names(z$coefficients) <- nm
    fitted <- drop(x %*% z$coefficients)
    z$fitted.values <- fitted
    z$residuals <- y - fitted
    c1 <- 1/qnorm((n + quantile)/(2*n))
    s <-
        if(lts == 1)
            sqrt(z$crit/quantile)/sqrt(1 - 2*n*dnorm(1/c1)/(quantile*c1))
        else if(lts == 0) sqrt(z$crit)*c1 else z$crit
    res <- z$residuals
    ind <- abs(res) <= 2.5*s
    s2 <- sum(res[ind]^2)/(sum(ind) - p)
    z$scale <- c(s, sqrt(s2))
    if(method == "S") { # IWLS refinement
	psi <- function(u, k0) (1  - pmin(1, abs(u/k0))^2)^2
	resid <- z$residuals
	scale <- s
	for(i in seq_len(30)) {
	    w <- psi(resid/scale, k0)
	    temp <- lm.wfit(x, y, w, method="qr")
	    resid <- temp$residuals
	    s2 <- scale*sqrt(sum(chi(resid/scale, k0))/((n-p)*beta))
	    if(abs(s2/scale - 1) < 1e-5) break
	    scale <- s2
	}
	z$coefficents <- temp$coefficients
	z$fitted.values <- temp$fitted.values
	z$residuals <- resid
	z$scale <- scale
    }
    class(z) <- "lqs"
    z
}

print.lqs <- function (x, digits = max(3, getOption("digits") - 3), ...)
{
    if(!is.null(cl <- x$call)) {
	cat(gettext("Call:", domain = "R-MASS"), "\n", sep = "")
	dput(cl, control=NULL)
	cat("\n")
    }
    cat(gettext("Coefficients:", domain = "R-MASS"), "\n", sep = "")
    print.default(format(coef(x), digits = digits), print.gap = 2, quote = FALSE)
    cat("\n", gettextf("Scale estimates %s", paste(format(x$scale, digits = digits), collapse = " "), domain = "R-MASS"), "\n\n", sep = "")
    invisible(x)
}

predict.lqs <- function (object, newdata, na.action = na.pass, ...)
{
    if (missing(newdata)) return(fitted(object))
    ## work hard to predict NA for rows with missing data
    Terms <- delete.response(terms(object))
    m <- model.frame(Terms, newdata, na.action = na.action,
                     xlev = object$xlevels)
    if(!is.null(cl <- attr(Terms, "dataClasses"))) .checkMFClasses(cl, m)
    X <- model.matrix(Terms, m, contrasts = object$contrasts)
    drop(X %*% object$coefficients)
}


cov.rob <- function(x, cor = FALSE, quantile.used = floor((n+p+1)/2),
		    method = c("mve", "mcd", "classical"), nsamp = "best", seed)
{
    method <- match.arg(method)
    x <- as.matrix(x)
    if(any(is.na(x)) || any(is.infinite(x)))
	stop("missing or infinite values in the data are not allowed")
    n <- nrow(x); p <- ncol(x)
    if(n < p+1)
        stop(gettextf("at least %d cases are needed", p+1), domain = "R-MASS")
    if(method == "classical") {
	ans <- list(center = colMeans(x), cov = var(x))
    } else {
	if(quantile.used < p+1)
            stop(gettextf("'quantile' must be at least %d", p+1), domain = "R-MASS")
	if(quantile.used > n-1)
            stop(gettextf("'quantile' must be at most %d", n-1), domain = "R-MASS")
	## re-scale to roughly common scale
	divisor <- apply(x, 2, IQR)
        if(any(divisor == 0)) stop("at least one column has IQR 0")
	x <- x /rep(divisor, rep(n,p))
	qn <- quantile.used
	ps <- p + 1
	nexact <- choose(n, ps)
	if(is.character(nsamp) && nsamp == "best")
	    nsamp <- if(nexact < 5000) "exact" else "sample"
	if(is.numeric(nsamp) && nsamp > nexact) {
            warning(sprintf(ngettext(nexact,
                                     "only %d set, so all sets will be tried",
                                     "only %d sets, so all sets will be tried", domain = "R-MASS"),
                            nexact), domain = NA)
	    nsamp <- "exact"
	}
	samp <- nsamp != "exact"
	if(samp) {
	    if(nsamp == "sample") nsamp <- min(500*ps, 3000)
	} else nsamp <- nexact
        if (nsamp > 2147483647) {
            if(samp)
                stop(gettextf("Too many samples (%.3g)", nsamp))
            else
                stop(gettextf('Too many combinations (%.3g) for nsamp = "exact"', nsamp))
        }
	if(samp && !missing(seed)) {
	    if(exists(".Random.seed", envir=.GlobalEnv, inherits=FALSE))  {
		seed.keep <- get(".Random.seed", envir=.GlobalEnv, inherits=FALSE)
		on.exit(assign(".Random.seed", seed.keep, envir=.GlobalEnv))
	    }
            assign(".Random.seed", seed, envir=.GlobalEnv)
	}
	z <-  .C(mve_fitlots,
		 as.double(x), as.integer(n), as.integer(p),
		 as.integer(qn), as.integer(method=="mcd"),
		 as.integer(samp), as.integer(ps), as.integer(nsamp),
		 crit=double(1), sing=integer(1L), bestone=integer(n))
        z$sing <- sprintf(ngettext(z$sing, "%d singular sample of size %d out of %d", "%d singular samples of size %d out of %d", domain = "R-MASS"), z$sing, ps, nsamp)
	crit <- z$crit + 2*sum(log(divisor)) +
	    if(method=="mcd") - p * log(qn - 1) else 0
	best <- seq(n)[z$bestone != 0]
        if(!length(best)) stop("'x' argument is probably collinear")
	means <- colMeans(x[best, , drop = FALSE])
	rcov <- var(x[best, , drop = FALSE]) * (1 + 15/(n - p))^2
	dist <- mahalanobis(x, means, rcov)
	cut <- qchisq(0.975, p) * quantile(dist, qn/n)/qchisq(qn/n, p)
	cov <- divisor * var(x[dist < cut, , drop = FALSE]) *
	    rep(divisor, rep(p, p))
	attr(cov, "names") <- NULL
	ans <- list(center =
		    colMeans(x[dist < cut, , drop = FALSE]) * divisor,
		    cov = cov, msg = z$sing, crit = crit, best = best)
    }
    if(cor) {
	sd <- sqrt(diag(ans$cov))
	ans <- c(ans, list(cor = (ans$cov/sd)/rep(sd, rep(p, p))))
    }
    ans$n.obs <- n
    ans
}

## compatibility functions for R users.

lmsreg <- function(...)
{
    oc <- sys.call()
    oc$method <- "lms"
    oc[[1L]] <- quote(MASS::lqs)
    eval.parent(oc)
}

ltsreg <- function(...)
{
    oc <- sys.call()
    oc$method <- "lts"
    oc[[1L]] <- quote(MASS::lqs)
    eval.parent(oc)
}

cov.mve <- function(...)
{
    oc <- sys.call()
    oc$method <- "mve"
    oc[[1L]] <- quote(MASS::cov.rob)
    eval.parent(oc)
}

cov.mcd <- function(...)
{
    oc <- sys.call()
    oc$method <- "mcd"
    oc[[1L]] <- quote(MASS::cov.rob)
    eval.parent(oc)
}
