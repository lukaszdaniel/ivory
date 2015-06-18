#  File src/library/stats/R/bartlett.test.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2015 The R Core Team
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

bartlett.test <- function(x, ...) UseMethod("bartlett.test")

bartlett.test.default <-
function(x, g, ...)
{
    LM <- FALSE
    if (is.list(x)) {
        if (length(x) < 2L)
            stop("'x' must be a list with at least 2 elements")
        DNAME <- deparse(substitute(x))
        if (all(sapply(x, function(obj) inherits(obj, "lm"))))
            LM <- TRUE
        else
            x <- lapply(x, function(x) x <- x[is.finite(x)])
        k <- length(x)
    }
    else {
        if (length(x) != length(g))
            stop(gettextf("'%s' and '%s' arguments must have the same length", "x", "g"))
        DNAME <- gettextf("%s and %s", paste(deparse(substitute(x)), collapse = ""), paste(deparse(substitute(g)), collapse = ""))
        OK <- complete.cases(x, g)
        x <- x[OK]
        g <- factor(g[OK])
        k <- nlevels(g)
        if (k < 2)
            stop("all observations are in the same group")
        x <- split(x, g)
    }

    if (LM) {
        n <- sapply(x, function(obj) obj$df.resid)
        v <- sapply(x, function(obj) sum(obj$residuals^2))
    } else {
        n <- sapply(x, "length") - 1
        if (any(n <= 0))
            stop("there must be at least 2 observations in each group")
        v <- sapply(x, "var")
    }

    n.total <- sum(n)
    v.total <- sum(n * v) / n.total
    STATISTIC <- ((n.total * log(v.total) - sum(n * log(v))) /
                  (1 + (sum(1 / n) - 1 / n.total) / (3 * (k - 1))))
    PARAMETER <- k - 1
    PVAL <- pchisq(STATISTIC, PARAMETER, lower.tail = FALSE)
    names(STATISTIC) <- gettext("Bartlett's K-squared", domain = "R-stats")
    names(PARAMETER) <- "df"
	METHOD <- gettext("Bartlett test of homogeneity of variances", domain = "R-stats")
    RVAL <- list(statistic = STATISTIC,
                 parameter = PARAMETER,
                 p.value = PVAL,
                 data.name = DNAME,
                 method = METHOD)
    class(RVAL) <- "htest"
    return(RVAL)
}

bartlett.test.formula <-
function(formula, data, subset, na.action, ...)
{
    if(missing(formula) || (length(formula) != 3L))
        stop(gettextf("'%s' argument is missing or incorrect", "formula"))
    m <- match.call(expand.dots = FALSE)
    if(is.matrix(eval(m$data, parent.frame())))
        m$data <- as.data.frame(data)
    ## need stats:: for non-standard evaluation
    m[[1L]] <- quote(stats::model.frame)
    mf <- eval(m, parent.frame())
    if(length(mf) != 2L)
        stop("'formula' argument should be of the form response ~ group")
    DNAME <- gettextf("%s by %s", names(mf[1]), names(mf[2]))
    names(mf) <- NULL
    y <- do.call("bartlett.test", as.list(mf))
    y$data.name <- DNAME
    y
}
