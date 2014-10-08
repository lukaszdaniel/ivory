#  File src/library/stats/R/friedman.test.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2013 The R Core Team
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

friedman.test <- function(y, ...) UseMethod("friedman.test")

friedman.test.default <-
function(y, groups, blocks, ...)
{
    DNAME <- deparse(substitute(y))
    if (is.matrix(y)) {
        groups <- factor(c(col(y)))
        blocks <- factor(c(row(y)))
    }
    else {
        if (anyNA(groups) || anyNA(blocks))
            stop("NA values are not allowed in 'groups' or 'blocks' arguments")
        if (any(diff(c(length(y), length(groups), length(blocks))) != 0L))
            stop(gettextf("'%s', '%s' and '%s' arguments must have the same length", "y", "groups", "blocks"))
        DNAME <- gettextf("%s and %s and %s", DNAME, deparse(substitute(groups)), deparse(substitute(blocks)))
        if (any(table(groups, blocks) != 1))
            stop("not an unreplicated complete block design")
        groups <- factor(groups)
        blocks <- factor(blocks)
        ## Need to ensure consistent order of observations within
        ## blocks.
        o <- order(groups, blocks)
        y <- y[o]
        groups <- groups[o]
        blocks <- blocks[o]
    }

    k <- nlevels(groups)
    y <- matrix(unlist(split(y, blocks)), ncol = k, byrow = TRUE)
    y <- y[complete.cases(y), ]
    n <- nrow(y)
    r <- t(apply(y, 1L, rank))
    TIES <- tapply(r, row(r), table)
    STATISTIC <- ((12 * sum((colSums(r) - n * (k + 1) / 2)^2)) /
                  (n * k * (k + 1)
                   - (sum(unlist(lapply(TIES, function (u) {u^3 - u}))) /
                      (k - 1))))
    PARAMETER <- k - 1
    PVAL <- pchisq(STATISTIC, PARAMETER, lower.tail = FALSE)
    names(STATISTIC) <- gettext("Friedman chi-squared", domain = "R-stats")
    names(PARAMETER) <- "df"
	METHOD <- gettext("Friedman rank sum test", domain = "R-stats")
    RVAL <- list(statistic = STATISTIC,
                   parameter = PARAMETER,
                   p.value = PVAL,
                   method = METHOD,
                   data.name = DNAME)
    class(RVAL) <- "htest"
    return(RVAL)
}

friedman.test.formula <-
function(formula, data, subset, na.action, ...)
{
    ## <FIXME>
    ## Maybe put this into an internal rewriteTwoWayFormula() when
    ## adding support for strata()
    if(missing(formula)
       || (length(formula) != 3L)
       || (length(formula[[3L]]) != 3L)
       || (formula[[3L]][[1L]] != as.name("|"))
       || (length(formula[[3L]][[2L]]) != 1L)
       || (length(formula[[3L]][[3L]]) != 1L))
        stop(gettextf("'%s' argument is missing or incorrect", "formula"))
    formula[[3L]][[1L]] <- as.name("+")
    ## </FIXME>
    m <- match.call(expand.dots = FALSE)
    m$formula <- formula
    if(is.matrix(eval(m$data, parent.frame())))
        m$data <- as.data.frame(data)
    m[[1L]] <- quote(stats::model.frame)
    mf <- eval(m, parent.frame())
    if(length(mf) != 3L) stop("invalid formula")
    DNAME <- gettextf("%s and %s and %s", names(mf[1]), names(mf[2]), names(mf[3]))
    names(mf) <- NULL
    y <- do.call("friedman.test", as.list(mf))
    y$data.name <- DNAME
    y
}
