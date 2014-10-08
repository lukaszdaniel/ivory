#  File src/library/stats/R/medpolish.R
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

medpolish <-
    function (x, eps = 0.01, maxiter = 10L, trace.iter = TRUE, na.rm = FALSE)
{
    z <- as.matrix(x)
    nr <- nrow(z)
    nc <- ncol(z)
    t <- 0
    r <- numeric(nr)
    c <- numeric(nc)
    oldsum <- 0
    for(iter in 1L:maxiter) {
	rdelta <- apply(z, 1L, median, na.rm = na.rm)
	z <- z - matrix(rdelta, nrow = nr, ncol = nc)
	r <- r + rdelta
	delta <- median(c, na.rm = na.rm)
	c <- c - delta
	t <- t + delta
	cdelta <- apply(z, 2L, median, na.rm = na.rm)
	z <- z - matrix(cdelta, nrow = nr, ncol = nc, byrow = TRUE)
	c <- c + cdelta
	delta <- median(r, na.rm = na.rm)
	r <- r - delta
	t <- t + delta
	newsum <- sum(abs(z), na.rm = na.rm)
	converged <- newsum == 0 || abs(newsum - oldsum) < eps*newsum
	if(converged) break
	oldsum <- newsum
	if(trace.iter) cat(iter, ": ", newsum, "\n", sep = "")
    }
    if(converged) {
        if(trace.iter) cat(gettext("Final: ", domain = "R-stats"), newsum, "\n", sep = "")
    } else
    warning(sprintf(ngettext(maxiter,
                             "medpolish() did not converge in %d iteration",
                             "medpolish() did not converge in %d iterations", domain = "R-stats"),
                    maxiter), domain = NA)
    names(r) <- rownames(z)
    names(c) <- colnames(z)
    ans <- list(overall = t, row = r, col = c, residuals = z,
		name = deparse(substitute(x)))
    class(ans) <- "medpolish"
    ans
}

print.medpolish <- function(x, digits = getOption("digits"), ...)
{
    cat("\n", gettextf("Median Polish Results (Dataset: %s)", sQuote(x$name), domain = "R-stats"), "\n", sep = "")
    cat("\n", gettext("Overall: ", domain = "R-stats"), x$overall, "\n\n", gettext("Row Effects:", domain = "R-stats"), "\n", sep = "")
    print(x$row, digits = digits, ...)
    cat("\n", gettext("Column Effects:", domain = "R-stats"), "\n", sep = "")
    print(x$col, digits = digits, ...)
    cat("\n", gettext("Residuals:", domain = "R-stats"), "\n", sep = "")
    print(x$residuals, digits = max(2L, digits - 2L), ...)
    cat("\n")
    invisible(x)
}

plot.medpolish <- function(x, main = gettext("Tukey Additivity Plot", domain = "R-stats"), ...)
{
    plot(outer(x$row,x$col)/x$overall, x$residuals,
	 main = main, xlab = gettext("Diagnostic Comparison Values", domain = "R-stats"),
	 ylab = gettext("Residuals", domain = "R-stats"), ...)
    abline(h = 0, v = 0, lty = "dotted")
}
