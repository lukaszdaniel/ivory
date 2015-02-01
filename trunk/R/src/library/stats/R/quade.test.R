#  File src/library/stats/R/quade.test.R
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

quade.test <- function(y, ...) UseMethod("quade.test")

quade.test.default <-
function(y, groups, blocks, ...)
{
    DNAME <- deparse(substitute(y))
    if(is.matrix(y)) {
        groups <- factor(c(col(y)))
        blocks <- factor(c(row(y)))
    }
    else {
        if(anyNA(groups) || anyNA(blocks))
            stop("NA values are not allowed in 'groups' or 'blocks' arguments")
        if(any(diff(c(length(y), length(groups), length(blocks))) != 0L))
            stop(gettextf("'%s', '%s' and '%s' arguments must have the same length", "y", "groups", "blocks"))
        DNAME <- gettextf("%s and %s and %s", DNAME, deparse(substitute(groups)), deparse(substitute(blocks)))
        if(any(table(groups, blocks) != 1))
            stop("not an unreplicated complete block design")
        groups <- factor(groups)
        blocks <- factor(blocks)
    }
    k <- nlevels(groups)
    b <- nlevels(blocks)
    ## <FIXME split.matrix>
    y <- matrix(unlist(split(c(y), blocks)), ncol = k, byrow = TRUE)
    y <- y[complete.cases(y), ]
#    n <- nrow(y)
    r <- t(apply(y, 1L, rank))
    q <- rank(apply(y, 1, function(u) max(u) - min(u)))
    s <- q * (r - (k+1)/2)
    ## S is a matrix of ranks within blocks (minus the average rank)
    ## multiplied by the ranked ranges of the blocks
    A <- sum(s^2)
    B <- sum(colSums(s)^2) / b
    if(A == B) {
        ## Treat zero denominator case as suggested by Conover (1999),
        ## p.374.
        STATISTIC <- NaN
        PARAMETER <- c(NA, NA)
        PVAL <- (gamma(k+1))^(1-b)
    } else {
        STATISTIC <- (b - 1) * B / (A - B)
        ## The same as 2-way ANOVA on the scores S.
        PARAMETER <- c(k - 1, (b-1) * (k-1))
        PVAL <- pf(STATISTIC, PARAMETER[1L], PARAMETER[2L], lower.tail = FALSE)
    }
    names(STATISTIC) <- gettext("Quade F", domain = "R-stats")
    names(PARAMETER) <- c("num df", "denom df")
	METHOD <- gettext("Quade test", domain = "R-stats")
    RVAL <- list(statistic = STATISTIC,
                   parameter = PARAMETER,
                   p.value = PVAL,
                   method = METHOD,
                   data.name = DNAME)
    class(RVAL) <- "htest"
    return(RVAL)
}

quade.test.formula <-
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
    DNAME <- gettextf("%s and %s and %s", names(mf[1]), names(mf[2]), names(mf[3]), domain = "R-stats")
    names(mf) <- NULL
    y <- do.call("quade.test", as.list(mf))
    y$data.name <- DNAME
    y
}
