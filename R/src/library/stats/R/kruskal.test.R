#  File src/library/stats/R/kruskal.test.R
#  Part of the R package, https://www.R-project.org
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
#  https://www.R-project.org/Licenses/

kruskal.test <- function(x, ...) UseMethod("kruskal.test")

kruskal.test.default <-
function(x, g, ...)
{
    if (is.list(x)) {
        if (length(x) < 2L)
            stop("'x' argument must be a list with at least 2 elements")
        if (!missing(g))
            warning("'x' argument is a list, so ignoring argument 'g'")
        DNAME <- deparse(substitute(x))
        x <- lapply(x, function(u) u <- u[complete.cases(u)])
        if (!all(sapply(x, is.numeric)))
            warning("some elements of 'x' argument are not numeric and will be coerced to numeric")
        k <- length(x)
        l <- lengths(x)
        if (any(l == 0L))
            stop("all groups must contain data")
        g <- factor(rep.int(seq_len(k), l))
        x <- unlist(x)
    }
    else {
        if (length(x) != length(g))
            stop(gettextf("'%s' and '%s' arguments must have the same length", "x", "g"))
        DNAME <- gettextf("%s and %s", paste(deparse(substitute(x)), collapse = ""), paste(deparse(substitute(g)), collapse = ""), domain = "R-stats")
        OK <- complete.cases(x, g)
        x <- x[OK]
        g <- g[OK]
        g <- factor(g)
        k <- nlevels(g)
        if (k < 2L)
            stop("all observations are in the same group")
    }

    n <- length(x)
    if (n < 2L)
        stop(gettextf("not enough '%s' observations", "x"))
    r <- rank(x)
    TIES <- table(x)
    STATISTIC <- sum(tapply(r, g, "sum")^2 / tapply(r, g, "length"))
    ## keep as n+1 to avoid (implausible) integer overflows
    STATISTIC <- ((12 * STATISTIC / (n * (n + 1)) - 3 * (n + 1)) /
                  (1 - sum(TIES^3 - TIES) / (n^3 - n)))
    PARAMETER <- k - 1L
    PVAL <- pchisq(STATISTIC, PARAMETER, lower.tail = FALSE)
    names(STATISTIC) <- gettext("Kruskal-Wallis chi-squared", domain = "R-stats")
    names(PARAMETER) <- "df"
	METHOD <- gettext("Kruskal-Wallis rank sum test", domain = "R-stats")
    RVAL <- list(statistic = STATISTIC,
                 parameter = PARAMETER,
                 p.value = PVAL,
                 method = METHOD,
                 data.name = DNAME)
    class(RVAL) <- "htest"
    return(RVAL)
}

kruskal.test.formula <-
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
    if(length(mf) > 2L)
        stop("'formula' argument should be of the form response ~ group")
    DNAME <- gettextf("%s by %s", names(mf[1]), names(mf[2]))
    names(mf) <- NULL
    y <- do.call("kruskal.test", as.list(mf))
    y$data.name <- DNAME
    y
}
