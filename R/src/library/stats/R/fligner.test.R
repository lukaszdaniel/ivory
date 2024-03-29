#  File src/library/stats/R/fligner.test.R
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

fligner.test <- function(x, ...) UseMethod("fligner.test")

fligner.test.default <-
function(x, g, ...)
{
    ## FIXME: This is the same code as in kruskal.test(), and could also
    ## rewrite bartlett.test() accordingly ...
    if (is.list(x)) {
        if (length(x) < 2L)
            stop("'x' must be a list with at least 2 elements")
        DNAME <- deparse1(substitute(x))
        x <- lapply(x, function(u) u <- u[complete.cases(u)])
        k <- length(x)
        l <- lengths(x)
        if (any(l == 0))
            stop("all groups must contain data")
        g <- factor(rep(seq_len(k), l))
        x <- unlist(x)
    }
    else {
        if (length(x) != length(g))
            stop(gettextf("'%s' and '%s' arguments must have the same length", "x", "g"))
        DNAME <- gettextf("%s and %s", paste(deparse1(substitute(x)), collapse = ""), paste(deparse1(substitute(g)), collapse = ""), domain = "R-stats")
        OK <- complete.cases(x, g)
        x <- x[OK]
        g <- g[OK]
        g <- factor(g)
        k <- nlevels(g)
        if (k < 2)
            stop("all observations are in the same group")
    }
    n <- length(x)
    if (n < 2)
        stop("not enough observations")
    ## FIXME: now the specific part begins.

    ## Careful. This assumes that g is a factor:
    x <- x - tapply(x,g,median)[g]

    a <- qnorm((1 + rank(abs(x)) / (n + 1)) / 2)
    a <- a - mean(a)
    v <- sum(a^2) / (n - 1)
    a <- split(a, g)
    STATISTIC <- sum(lengths(a) * vapply(a, mean, 0)^2) / v
    PARAMETER <- k - 1
    PVAL <- pchisq(STATISTIC, PARAMETER, lower.tail = FALSE)
    names(STATISTIC) <- gettext("Fligner-Killeen:med chi-squared", domain = "R-stats")
    names(PARAMETER) <- "df"
    METHOD <- gettext("Fligner-Killeen test of homogeneity of variances", domain = "R-stats")

    RVAL <- list(statistic = STATISTIC,
                 parameter = PARAMETER,
                 p.value = PVAL,
                 method = METHOD,
                 data.name = DNAME)
    class(RVAL) <- "htest"
    return(RVAL)
}

fligner.test.formula <-
function(formula, data, subset, na.action, ...)
{
    if(missing(formula) || (length(formula) != 3L))
        stop(gettextf("'%s' argument is missing or incorrect", "formula"))
    m <- match.call(expand.dots = FALSE)
    if(is.matrix(eval(m$data, parent.frame())))
        m$data <- as.data.frame(data)
    m[[1L]] <- quote(model.frame)
    mf <- eval(m, parent.frame())
    if(length(mf) != 2L)
        stop("'formula' argument should be of the form response ~ group")
    DNAME <- gettextf("%s by %s", names(mf[1]), names(mf[2]))
    names(mf) <- NULL
    y <- do.call("fligner.test", as.list(mf))
    y$data.name <- DNAME
    y
}
