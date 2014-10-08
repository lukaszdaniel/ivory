#  File src/library/stats/R/mcnemar.test.R
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

mcnemar.test <- function(x, y = NULL, correct = TRUE)
{
    if (is.matrix(x)) {
        r <- nrow(x)
        if ((r < 2) || (ncol (x) != r))
            stop("'x' must be square with at least two rows and columns")
        if (any(x < 0) || anyNA(x))
            stop("all entries of 'x' must be nonnegative and finite")
        DNAME <- deparse(substitute(x))
    }
    else {
        if (is.null(y))
            stop(gettextf("if '%s' argument is not a matrix, '%s' argument must be given", "x", "y"))
        if (length(x) != length(y))
            stop(gettextf("'%s' and '%s' arguments must have the same length", "x", "y"))
        DNAME <- gettextf("%s and %s", paste(deparse(substitute(x)), collapse = ""), paste(deparse(substitute(y)), collapse = ""), domain = "R-stats")
        OK <- complete.cases(x, y)
        x <- as.factor(x[OK])
        y <- as.factor(y[OK])
        r <- nlevels(x)
        if ((r < 2) || (nlevels(y) != r))
            stop("'x' and 'y' must have the same number of levels (minimum 2)")
        x <- table(x, y)
    }

    PARAMETER <- r * (r-1) / 2
    METHOD <- gettext("McNemar's Chi-squared test", domain = "R-stats")

    if (correct && (r == 2) && any(x - t(x) != 0)) {
        y <- (abs(x - t(x)) - 1)
        METHOD <- gettext("McNemar's Chi-squared test with continuity correction", domain = "R-stats")
    }
    else
        y <- x - t(x)
    x <- x + t(x)

    STATISTIC <- sum(y[upper.tri(x)]^2 / x[upper.tri(x)])
    PVAL <- pchisq(STATISTIC, PARAMETER, lower.tail = FALSE)
    names(STATISTIC) <- gettext("McNemar's chi-squared", domain = "R-stats")
    names(PARAMETER) <- gettext("df", domain = "R-stats")

    RVAL <- list(statistic = STATISTIC,
                 parameter = PARAMETER,
                 p.value = PVAL,
                 method = METHOD,
                 data.name = DNAME)
    class(RVAL) <- "htest"
    return(RVAL)
}
