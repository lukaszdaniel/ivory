#  File src/library/stats/R/smooth.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2016 The R Core Team
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

## do.ends = TRUE  is compatible with older behavior in R
## --------------  but *NOT*  with Colin Goodalls "smoother" "spl()"

smooth <- function(x, kind = c("3RS3R", "3RSS", "3RSR", "3R", "3", "S"),
                   twiceit = FALSE,
                   endrule = c("Tukey", "copy"), do.ends = FALSE)
{
    if(!is.numeric(x)) stop("attempt to smooth non-numeric values")
    if(anyNA(x)) stop("attempt to smooth NA values")
    endrule <- match.arg(endrule)
    rules <- c("copy","Tukey")#- exact order matters!
    if(is.na(iend <- pmatch(endrule, rules))) stop(gettextf("invalid '%s' argument", "endrule"))
    kind <- match.arg(kind)
    if(startsWith(kind, "3RS") && !do.ends) iend <- -iend
    else if(kind == "S") iend <- as.logical(do.ends)
    type <- match(kind, c("3RS3R", "3RSS", "3RSR", "3R", "3", "S"))
    smo <- .Call(C_Rsm, as.double(x), type, iend)

    if(twiceit) {
        ## c2 <- match.call() and re-call with twiceit = FALSE
        r <- smooth(x - smo$y, kind = kind, twiceit = FALSE,
                    endrule = endrule, do.ends = do.ends)
        smo$y <- smo$y + r
        if(!is.null(smo$iter))
            smo$iter <- smo$iter + attr(r, "iter")
        if(!is.null(smo$changed))
            smo$changed <- smo$changed || attr(r,"changed")
    }
    if(is.ts(x))
	smo$y <- ts(smo$y, start=start(x), frequency=frequency(x))

    structure(smo$y, kind = kind, twiced = twiceit,
              iter = smo$iter, changed = smo$changed,
              endrule = if(startsWith(kind, "3")) rules[iend],
              call = match.call(),
              class = c("tukeysmooth",if(is.ts(x)) "ts"))
}

print.tukeysmooth <- function(x, ...) {
    cat(gettextf("%s Tukey smoother resulting from %s", attr(x,"kind"), sQuote(paste(deparse(attr(x, "call")), collapse = "")), domain = "R-stats"),"\n", sep = "")
    if(attr(x,"twiced")) {
      if(!is.null(it <- attr(x,"iter")))		cat(sprintf(ngettext(it, "__twiced__ used %d iteration", "__twiced__ used %d iterations", domain = "R-stats"), it), "\n", sep = "")
      if(is.null(it <- attr(x,"iter")) && !is.null(ch <- attr(x,"changed"))) {
	 if(!ch) cat(gettext("__twiced__ NOT changed:", domain = "R-stats"), "\n", sep = "")
	 else cat(gettext("__twiced__ changed:", domain = "R-stats"), "\n", sep = "")
	 }
    else if(!is.null(ch <- attr(x,"changed"))) if(!ch) cat(gettext("NOT changed:", domain = "R-stats"), "\n", sep = "") else cat(gettext("changed:", domain = "R-stats"), "\n", sep = "")
    } else {
     if(!is.null(it <- attr(x,"iter")))		cat(sprintf(ngettext(it, " used %d iteration", " used %d iterations", domain = "R-stats"), it), "\n", sep = "")
     if(!is.null(ch <- attr(x,"changed")))	if(!ch) cat(gettext("NOT changed:", domain = "R-stats"), "\n", sep = "") else cat(gettext("changed:", domain = "R-stats"), "\n", sep = "")
    }

    if(length(oldClass(x)) > 1L)
	NextMethod()
    else {
	y <- x
	attributes(y) <- NULL
	print(y, ...)
	invisible(x)
    }
}

summary.tukeysmooth <- function(object, ...) {
    cat(gettextf("%s Tukey smoother resulting from %s", attr(object,"kind"), sQuote(paste(deparse(attr(object, "call")), collapse = "")), domain = "R-stats"), ";  n = ", length(object), "\n", sep = "")
    if(attr(object,"twiced")) {
      if(!is.null(it <- attr(object,"iter")))		cat(sprintf(ngettext(it, "__twiced__ used %d iteration", "__twiced__ used %d iterations", domain = "R-stats"), it), "\n", sep = "")
      if(is.null(it <- attr(object,"iter")) && !is.null(ch <- attr(object,"changed"))) {
	 if(!ch) cat(gettext("__twiced__ NOT changed:", domain = "R-stats"), "\n", sep = "")
	 else cat(gettext("__twiced__ changed:", domain = "R-stats"), "\n", sep = "")
	 }
    else if(!is.null(ch <- attr(object,"changed"))) if(!ch) cat(gettext("NOT changed:", domain = "R-stats"), "\n", sep = "") else cat(gettext("changed:", domain = "R-stats"), "\n", sep = "")
    } else {
     if(!is.null(it <- attr(object,"iter")))		cat(sprintf(ngettext(it, " used %d iteration", " used %d iterations", domain = "R-stats"), it), "\n", sep = "")
     if(!is.null(ch <- attr(object,"changed")))	if(!ch) cat(gettext("NOT changed:", domain = "R-stats"), "\n", sep = "") else cat(gettext("changed:", domain = "R-stats"), "\n", sep = "")
    }

    if(length(oldClass(object)) > 1L)
	NextMethod()
    else {
	y <- object
	attributes(y) <- NULL
	summary(y, ...)
    }
}


