#  File src/library/stats/R/prop.test.R
#  Part of the R package, https://www.R-project.org
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
#  https://www.R-project.org/Licenses/

prop.test <-
function(x, n, p = NULL, alternative = c("two.sided", "less", "greater"),
         conf.level = 0.95, correct = TRUE)
{
    DNAME <- deparse(substitute(x))

    if (is.table(x) && length(dim(x)) == 1L) {
        if (dim(x) != 2L)
            stop("table 'x' should have 2 entries")
        l <- 1
        n <- sum(x)
        x <- x[1L]
    }
    else if (is.matrix(x)) {
	if (ncol(x) != 2L)
	    stop("'x' must have 2 columns")
	l <- nrow(x)
	n <- rowSums(x)
	x <- x[, 1L]
    }
    else {
        DNAME <- gettextf("%s out of %s", paste(deparse(substitute(x)), collapse = ""), paste(deparse(substitute(n)), collapse = ""), domain = "R-stats")
	if ((l <- length(x)) != length(n))
	    stop(gettextf("'%s' and '%s' arguments must have the same length", "x", "n"))
    }

    OK <- complete.cases(x, n)
    x <- x[OK]
    n <- n[OK]
    if ((k <- length(x)) < 1L)
	stop("not enough data")
    if (any(n <= 0))
	stop("elements of 'n' must be positive")
    if (any(x < 0))
	stop("elements of 'x' must be nonnegative")
    if (any(x > n))
	stop("elements of 'x' must not be greater than those of 'n'")

    if (is.null(p) && (k == 1))
	p <- .5
    if (!is.null(p)) {
	DNAME <- paste0(DNAME, ", ", sprintf(ngettext(k, "null probability %s", "null probabilities %s", domain = "R-stats"), paste(deparse(substitute(p)), collapse = "")))
	if (length(p) != l)
	    stop(gettextf("'%s', '%s' and '%s' arguments must have the same length", "x", "n", "p"))
	p <- p[OK]
	if (any((p <= 0) | (p >= 1)))
	    stop("elements of 'p' must be in (0,1)")
    }

    alternative <- match.arg(alternative)
    if (k > 2 || (k == 2) && !is.null(p))
	alternative <- "two.sided"

    if ((length(conf.level) != 1L) || is.na(conf.level) ||
	(conf.level <= 0) || (conf.level >= 1))
	stop(gettextf("'%s' argument must be a single number between 0 and 1", "conf.level"))

    correct <- as.logical(correct)

    ESTIMATE <- setNames(x/n,
			 if (k == 1) "proportion" else paste("prop", 1L:l)[OK])
    NVAL <- p
    CINT <- NULL
    YATES <- if(correct && (k <= 2)) .5 else 0

    if (k == 1) {
	z <- qnorm(if(alternative == "two.sided")
		   (1 + conf.level) / 2 else conf.level)
	YATES <- min(YATES, abs(x - n * p))
        z22n <- z^2 / (2 * n)
	p.c <- ESTIMATE + YATES / n
	p.u <- if(p.c >= 1) 1 else (p.c + z22n
                  + z * sqrt(p.c * (1 - p.c) / n + z22n / (2 * n))) / (1+2*z22n)
	p.c <- ESTIMATE - YATES / n
	p.l <- if(p.c <= 0) 0 else (p.c + z22n
                  - z * sqrt(p.c * (1 - p.c) / n + z22n / (2 * n))) / (1+2*z22n)
	CINT <- switch(alternative,
		       "two.sided" = c(max(p.l, 0), min(p.u, 1)),
		       "greater" = c(max(p.l, 0), 1),
		       "less" = c(0, min(p.u, 1)))
    }
    else if ((k == 2) & is.null(p)) {
	DELTA <- ESTIMATE[1L] - ESTIMATE[2L]
	YATES <- min(YATES, abs(DELTA) / sum(1/n))
	WIDTH <- (switch(alternative,
			 "two.sided" = qnorm((1 + conf.level) / 2),
			 qnorm(conf.level))
		  * sqrt(sum(ESTIMATE * (1 - ESTIMATE) / n))
		  + YATES * sum(1/n))
	CINT <- switch(alternative,
		       "two.sided" = c(max(DELTA - WIDTH, -1),
		       min(DELTA + WIDTH, 1)),
		       "greater" = c(max(DELTA - WIDTH, -1), 1),
		       "less" = c(-1, min(DELTA + WIDTH, 1)))
    }
    if (!is.null(CINT))
	attr(CINT, "conf.level") <- conf.level


    if(is.null(p))
    {
     if(YATES)
     METHOD <- sprintf(ngettext(k, "%d-sample proportions test with continuity correction",
				"%d-sample test for equality of proportions with continuity correction", domain = "R-stats"),
				k)
     else
     METHOD <- sprintf(ngettext(k, "%d-sample proportions test without continuity correction",
				"%d-sample test for equality of proportions without continuity correction", domain = "R-stats"),
				k)
    }
    else
    {
     if(YATES)
     METHOD <- sprintf(ngettext(k, "%d-sample proportions test with continuity correction",
                    		"%d-sample test for given proportions with continuity correction", domain = "R-stats"),
				k)
     else
     METHOD <- sprintf(ngettext(k, "%d-sample proportions test without continuity correction",
                    		"%d-sample test for given proportions without continuity correction", domain = "R-stats"),
				k)
    }

    if (is.null(p)) {
	p <- sum(x)/sum(n)
	PARAMETER <- k - 1
    }
    else {
	PARAMETER <- k
	names(NVAL) <- if (k == 1) gettext("proportion", domain = "R-stats") else gettext("all proportions", domain = "R-stats")
    }
    names(PARAMETER) <- "df"

    x <- cbind(x, n - x)
    E <- cbind(n * p, n * (1 - p))
    if (any(E < 5))
	warning("Chi-squared approximation may be incorrect")
    STATISTIC <- sum((abs(x - E) - YATES)^2 / E)
    names(STATISTIC) <- gettext("X-squared", domain = "R-stats")

    if (alternative == "two.sided")
	PVAL <- pchisq(STATISTIC, PARAMETER, lower.tail = FALSE)
    else {
	if (k == 1)
	    z <- sign(ESTIMATE - p) * sqrt(STATISTIC)
	else
	    z <- sign(DELTA) * sqrt(STATISTIC)
	PVAL <- pnorm(z, lower.tail = (alternative == "less"))
    }
    if(k == 1) {
    alt.name <- switch(alternative,
                           two.sided = gettextf("true proportion is not equal to %s", NVAL, domain = "R-stats"),
                           less = gettextf("true proportion is less than %s", NVAL, domain = "R-stats"),
                           greater = gettextf("true proportion is greater than %s", NVAL, domain = "R-stats"))
    } else {
    alt.name <- switch(alternative,
                           two.sided = gettextf("true proportion (%s) is not equal to %s", paste((1:l)[OK], collapse = ", "), NVAL, domain = "R-stats"),
                           less = gettextf("true proportion (%s) is less than %s", paste((1:l)[OK], collapse = ", "), NVAL, domain = "R-stats"),
                           greater = gettextf("true proportion (%s) is greater than %s", paste((1:l)[OK], collapse = ", "), NVAL, domain = "R-stats"))
    }
    RVAL <- list(statistic = STATISTIC,
		 parameter = PARAMETER,
		 p.value = as.numeric(PVAL),
		 estimate = ESTIMATE,
		 null.value = NVAL,
		 conf.int = CINT,
		 alternative = alternative,
		 alt.name = alt.name,
		 method = METHOD,
		 data.name = DNAME)
    class(RVAL) <- "htest"
    return(RVAL)
}
