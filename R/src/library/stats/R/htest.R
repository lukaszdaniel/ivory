#  File src/library/stats/R/htest.R
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

print.htest <- function(x, digits = getOption("digits"), prefix = "\t", ...)
{
    cat("\n")
    cat(strwrap(x$method, prefix = prefix), sep = "\n")
    cat("\n")
    cat(gettextf("data: %s", x$data.name, domain = "R-stats"), "\n", sep = "")
    out <- character()
    if(!is.null(x$statistic))
	out <- c(out, paste(names(x$statistic), "=",
			    format(signif(x$statistic, max(1L, digits - 2L)))))
    if(!is.null(x$parameter))
	out <- c(out, paste(names(x$parameter), "=",
			    format(signif(x$parameter, max(1L, digits - 2L)))))
    if(!is.null(x$p.value)) {
	fp <- format.pval(x$p.value, digits = max(1L, digits - 3L))
	out <- c(out, paste(gettext("p-value", domain = "R-stats"),
			    if(substr(fp, 1L, 1L) == "<") fp else paste("=",fp)))
    }
    cat(strwrap(paste(out, collapse = ", ")), sep = "\n")
    if(!is.null(x$alternative)) {
	cat(gettext("alternative hypothesis: ", domain = "R-stats"))
	if(!is.null(x$null.value)) {
	    if(length(x$null.value) == 1L) {
		   cat(x$alt.name, "\n", sep = "")
	    }
	    else {
		cat(x$alternative, "\n", gettext("null values:", domain = "R-stats"), "\n", sep = "")
		print(x$null.value, digits=digits, ...)
	    }
	}
	else cat(x$alternative, "\n", sep = "")
    }
    if(!is.null(x$conf.int)) {
        cat(gettextf("%s percent confidence interval:", format(100 * attr(x$conf.int, "conf.level")), domain = "R-stats"), "\n ", sep = "")
        cat(format(c(x$conf.int[1L], x$conf.int[2L])))
	cat("\n")
    }
    if(!is.null(x$estimate)) {
	cat(gettext("sample estimates:", domain = "R-stats"), "\n", sep = "")
	print(x$estimate, digits=digits, ...)
    }
    cat("\n")
    invisible(x)
}
