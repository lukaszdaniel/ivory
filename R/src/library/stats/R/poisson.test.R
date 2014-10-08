#  File src/library/stats/R/poisson.tests.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2012 The R Core Team
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


poisson.test <- function(x, T = 1, r = 1, alternative =
                         c("two.sided", "less", "greater"),
                         conf.level = 0.95)
{

    DNAME <- gettextf("%s time base: %s", deparse(substitute(x)), deparse(substitute(T)), domain = "R-stats")
    if ((l <- length(x)) != length(T))
        if (length(T) == 1L)
            T <- rep(T, l)
        else
            stop("'x' and 'T' arguments have incompatible length")
    xr <- round(x)

    if(any(!is.finite(x) | (x < 0)) || max(abs(x-xr)) > 1e-7)
        stop("'x' argument must be finite, nonnegative, and integer")
    x <- xr

    if(any(is.na(T) | (T < 0)))
        stop("'T' argument must be nonnegative")


    if ((k <- length(x)) < 1L)
        stop(gettextf("not enough '%s' observations", "x"))

    if (k > 2L)
        stop("the case k > 2 is unimplemented")

    if(!missing(r) && (length(r) > 1 || is.na(r) || r < 0 ))
        stop(gettextf("'%s' argument must be a single positive number", "r"))
    alternative <- match.arg(alternative)


    if (k == 2) {

        RVAL <- binom.test(x, sum(x), r * T[1L]/(r * T[1L] + T[2L]), alternative=alternative, conf.level=conf.level)

        RVAL$data.name <- DNAME
        RVAL$statistic <- c(count1 = x[1L])
        RVAL$parameter <- sum(x) * r * T[1L]/sum(T * c(1, r))
	names(RVAL$parameter) <- gettext("expected count1", domain = "R-stats")
        RVAL$estimate  <- (x[1L]/T[1L])/(x[2L]/T[2L])
	names(RVAL$estimate) <- gettext("rate ratio", domain = "R-stats")
        pp <- RVAL$conf.int
        RVAL$conf.int <- pp/(1 - pp)*T[2L]/T[1L]
        names(r) <- gettext("rate ratio", domain = "R-stats")
        RVAL$null.value <- r
        RVAL$alt.name <- switch(alternative,
                           two.sided = gettextf("true rate ratio is not equal to %s", r, domain = "R-stats"),
                           less = gettextf("true rate ratio is less than %s", r, domain = "R-stats"),
                           greater = gettextf("true rate ratio is greater than %s", r, domain = "R-stats"))

        RVAL$method <- gettext("Comparison of Poisson rates", domain = "R-stats")
        return (RVAL)
    } else {
        m <- r * T
        PVAL <- switch(alternative,
                       less = ppois(x, m),
                       greater = ppois(x - 1, m, lower.tail = FALSE),
                       two.sided = {
                           if(m == 0)
                               (x == 0)
                           else {
                               ## Do
                               ##   d <- dpois(0 : inf, r * T)
                               ##   sum(d[d <= dpois(x, r * T)])
                               ## a bit more efficiently ...
                               ## Note that we need a little fuzz.
                               relErr <- 1 + 1e-7
                               d <- dpois(x, r * T)
                               ## This is tricky: need to be sure
                               ## only to sum values in opposite tail
                               ## and not count x twice.

                               ## For the Poisson dist., the mode will
                               ## equal the mean if it is an integer.
                               if (x == m)
                                   1
                               else if (x < m) {
                                   ## Slightly trickier than in the binomial
                                   ## because we cannot use infinite-length i
                                   N <- ceiling(2 * m - x)
                                   while (dpois(N, m) > d)
                                       N <- 2 * N
                                   i <- seq.int(from = ceiling(m), to = N)
                                   y <- sum(dpois(i, m) <= d * relErr)
                                   ppois(x, m) +
                                       ppois(N - y, m, lower.tail = FALSE)
                               } else {
                                   i <- seq.int(from = 0, to = floor(m))
                                   y <- sum(dpois(i, m) <= d * relErr)
                                   ppois(y - 1, m) +
                                       ppois(x - 1, m, lower.tail = FALSE)
                               }
                           }
                       })
        ## Determine m s.t. Prob(Pois(m) >= x) = alpha.
        ## Use that for x > 0,
        ##   Prob(Pois >= x) = pgamma(m, x).
        p.L <- function(x, alpha) {
            if(x == 0)                      # No solution
                0
            else
                qgamma(alpha, x)
        }
        ## Determine p s.t. Prob(B(n,p) <= x) = alpha.
        ## Use that for x < n,
        ##   Prob(Pois(m) <= x) = 1 - pgamma(m, x + 1).

        p.U <- function(x, alpha)
            qgamma(1 - alpha, x + 1)

        CINT <- switch(alternative,
                       less = c(0, p.U(x, 1 - conf.level)),
                       greater = c(p.L(x, 1 - conf.level), Inf),
                       two.sided = {
                           alpha <- (1 - conf.level) / 2
                           c(p.L(x, alpha), p.U(x, alpha))
                       }) / T
        attr(CINT, "conf.level") <- conf.level

        ESTIMATE <- x / T

        names(x) <- gettext("number of events", domain = "R-stats")	# or simply "x" ??
        names(T) <- gettext("time base", domain = "R-stats")	# or simply "n" ??
        names(ESTIMATE) <-
            names(r) <- gettext("event rate", domain = "R-stats") # or simply "p" ??
		METHOD <- gettext("Exact Poisson test", domain = "R-stats")
       
       alt.name <- switch(alternative,
                           two.sided = gettextf("true event rate is not equal to %s", r, domain = "R-stats"),
                           less = gettextf("true event rate is less than %s", r, domain = "R-stats"),
                           greater = gettextf("true event rate is greater than %s", r, domain = "R-stats"))
	
        RVAL <- list(statistic = x,
                       parameter = T,
                       p.value = PVAL,
                       conf.int = CINT,
                       estimate = ESTIMATE,
                       null.value = r,
                       alternative = alternative,
                       alt.name = alt.name,
                       method = METHOD,
                       data.name = DNAME)
    class(RVAL) <- "htest"
    return(RVAL)

    }
}


### test cases:

## SMR, Welsh Nickel workers
## poisson.test(137, 24.19893)

## eba1977, compare Fredericia to other three cities for ages 55-59
## poisson.test(c(11,6+8+7),c(800, 1083+1050+878))
