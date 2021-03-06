#  File src/library/stats/R/integrate.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2018 The R Core Team
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

integrate <- function(f, lower, upper, ..., subdivisions = 100L,
                      rel.tol = .Machine$double.eps^.25,
                      abs.tol = rel.tol, stop.on.error = TRUE,
                      keep.xy = FALSE, aux = NULL)
{
    f <- match.fun(f)
    ff <- function(x) f(x, ...)
    limit <- as.integer(subdivisions)
    if (limit < 1L || (abs.tol <= 0 &&
	rel.tol < max(50*.Machine$double.eps, 0.5e-28)))
	stop("invalid parameter values")
    stopifnot(length(lower) == 1, length(upper) == 1)
    if(is.finite(lower) && is.finite(upper)) {
	wk <- .External(C_call_dqags,
			ff, rho = environment(),
			as.double(lower), as.double(upper),
			as.double(abs.tol), as.double(rel.tol),
			limit = limit)
    } else { # indefinite integral
	if(is.na(lower) || is.na(upper)) stop("a limit is NA or NaN")
	if (is.finite(lower)) {
	    inf <- 1L
	    bound <- lower
	} else if (is.finite(upper)) {
	    inf <- -1L
	    bound <- upper
	} else {
	    inf <- 2L
	    bound <- 0.0
	}
	wk <- .External(C_call_dqagi,
			ff, rho = environment(),
			as.double(bound), inf,
			as.double(abs.tol), as.double(rel.tol),
			limit = limit)
    }
    res <- wk[c("value", "abs.error", "subdivisions")]
    res$message <-
	switch(wk$ierr + 1L,
	       "OK",
	       gettext("maximum number of subdivisions reached"),
	       gettext("roundoff error was detected"),
	       gettext("extremely bad integrand behaviour"),
	       gettext("roundoff error is detected in the extrapolation table"),
	       gettext("the integral is probably divergent"),
	       gettext("the input is invalid"))
    if(wk$ierr == 6L || (wk$ierr > 0L && stop.on.error)) stop(res$message)
    res$call <- match.call()
    class(res) <- "integrate"
    res
}

print.integrate <- function (x, digits = getOption("digits"), ...)
{
    if(x$message == "OK")
       cat(gettextf("%s with absolute error < %s", format(x$value, digits = digits), format(x$abs.error, digits = 2L), domain = "R-stats"), "\n", sep = "")
    else cat(gettextf("failed with message %s", sQuote(x$message), domain = "R-stats"), "\n", sep = "")
    invisible(x)
}
