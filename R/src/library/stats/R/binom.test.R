#  File src/library/stats/R/binom.test.R
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

binom.test <-
function(x, n, p = 0.5, alternative = c("two.sided", "less", "greater"),
         conf.level = 0.95)
{
    DNAME <- deparse(substitute(x))
    xr <- round(x)

    if(any(is.na(x) | (x < 0)) || max(abs(x-xr)) > 1e-7)
        stop("'x' argument must be non-negative and integer")
    x <- xr
    if(length(x) == 2L) {
        ## x gives successes and failures
        n <- sum(x)
        x <- x[1L]
    }
    else if(length(x) == 1L) {
        ## x gives successes, n gives trials
        nr <- round(n)
        if((length(n) > 1L) || is.na(n) || (n < 1) || abs(n-nr) > 1e-7
           || (x > nr))
            stop("'n' argument must be a positive integer >= 'x' argument")
        DNAME <- gettextf("%s and %s", deparse(substitute(x)), deparse(substitute(n)), domain = "R-stats")
        n <- nr
    }
    else
        stop(gettextf("invalid length of '%s' argument", "x"))

    if(!missing(p) && (length(p) > 1L || is.na(p) || p < 0 || p > 1))
        stop(gettextf("'%s' argument must be a single number between 0 and 1", "p"))
    alternative <- match.arg(alternative)

    if(!((length(conf.level) == 1L) && is.finite(conf.level) &&
         (conf.level > 0) && (conf.level < 1)))
        stop(gettextf("'%s' argument must be a single number between 0 and 1", "conf.level"))

    PVAL <- switch(alternative,
                   less = pbinom(x, n, p),
                   greater = pbinom(x - 1, n, p, lower.tail = FALSE),
                   two.sided = {
                       if(p == 0)
                           (x == 0)
                       else if(p == 1)
                           (x == n)
                       else {
                           ## Do
                           ##   d <- dbinom(0 : n, n, p)
                           ##   sum(d[d <= dbinom(x, n, p)])
                           ## a bit more efficiently ...
                           ## Note that we need a little fuzz.
                           relErr <- 1 + 1e-7
                           d <- dbinom(x, n, p)
			   ## This is tricky: need to be sure
			   ## only to sum values in opposite tail
			   ## and not count x twice.
			   ## For the binomial dist., the mode will
			   ## equal the mean if it is an integer.
			   m <- n * p
			   if (x == m)
			   	1
                           else if (x < m) {
                               i <- seq.int(from = ceiling(m), to = n)
                               y <- sum(dbinom(i, n, p) <= d * relErr)
                               pbinom(x, n, p) +
                                   pbinom(n - y, n, p, lower.tail = FALSE)
                           } else {
                               i <- seq.int(from = 0, to = floor(m))
                               y <- sum(dbinom(i, n, p) <= d * relErr)
                               pbinom(y - 1, n, p) +
                                   pbinom(x - 1, n, p, lower.tail = FALSE)
                           }
                       }
                   })
    ## Determine p s.t. Prob(B(n,p) >= x) = alpha.
    ## Use that for x > 0,
    ##   Prob(B(n,p) >= x) = pbeta(p, x, n - x + 1).
    p.L <- function(x, alpha) {
        if(x == 0)                      # No solution
            0
        else
            qbeta(alpha, x, n - x + 1)
    }
    ## Determine p s.t. Prob(B(n,p) <= x) = alpha.
    ## Use that for x < n,
    ##   Prob(B(n,p) <= x) = 1 - pbeta(p, x + 1, n - x).
    p.U <- function(x, alpha) {
        if(x == n)                      # No solution
            1
        else
            qbeta(1 - alpha, x + 1, n - x)
    }
    CINT <- switch(alternative,
                   less = c(0, p.U(x, 1 - conf.level)),
                   greater = c(p.L(x, 1 - conf.level), 1),
                   two.sided = {
                       alpha <- (1 - conf.level) / 2
                       c(p.L(x, alpha), p.U(x, alpha))
                   })
    attr(CINT, "conf.level") <- conf.level

    ESTIMATE <- x / n

    names(x) <- gettext("number of successes", domain = "R-stats")	# or simply "x" ??
    names(n) <- gettext("number of trials", domain = "R-stats")	# or simply "n" ??
    names(ESTIMATE) <-
    names(p) <- gettext("probability of success", domain = "R-stats") # or simply "p" ??
	METHOD <- gettext("Exact binomial test", domain = "R-stats")
   alt.name <- switch(alternative,
                           two.sided = gettextf("true probability of success is not equal to %s", p, domain = "R-stats"),
                           less = gettextf("true probability of success is less than %s", p, domain = "R-stats"),
                           greater = gettextf("true probability of success is greater than %s", p, domain = "R-stats"))

    RVAL <- list(statistic = x,
                   parameter = n,
                   p.value = PVAL,
                   conf.int = CINT,
                   estimate = ESTIMATE,
                   null.value = p,
                   alternative = alternative,
                   alt.name = alt.name,
                   method = METHOD,
                   data.name = DNAME)
    class(RVAL) <- "htest"
    return(RVAL)
}
