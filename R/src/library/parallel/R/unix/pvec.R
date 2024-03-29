#  File src/library/parallel/R/unix/pvec.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2017 The R Core Team
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

### Derived from multicore version 0.1-6 by Simon Urbanek

pvec <- function(v, FUN, ..., mc.set.seed = TRUE, mc.silent = FALSE,
                 mc.cores = getOption("mc.cores", 2L), mc.cleanup = TRUE)
{
    if (!is.vector(v)) stop("'v' argument must be a vector")

    cores <- as.integer(mc.cores)
    if(cores < 1L) stop(gettextf("'%s' argument must be >= 1", "mc.cores"))
    if(cores == 1L) return(FUN(v, ...))
    .check_ncores(cores)

    if(mc.set.seed) mc.reset.stream()

    n <- length(v)
    l <- if (n <= cores) as.list(v) else {
        ## compute the scheduling, making it as fair as possible
        il <- as.integer(n / cores)
        xc <- n - il * cores
        sl <- rep(il, cores)
        if (xc) sl[1:xc] <- il + 1L
        si <- cumsum(c(1L, sl))
        se <- si + c(sl, 0L) - 1L
        lapply(seq_len(cores), function(ix) v[si[ix]:se[ix]])
    }
    jobs <- NULL
    ## all processes created from now on will be terminated by cleanup
    prepareCleanup()
    on.exit(cleanup())
    FUN <- match.fun(FUN)
    ## may have more cores than tasks ....
    jobs <- lapply(seq_len(min(n, cores)),
                   function(i) mcparallel(FUN(l[[i]], ...), name = i,
                                          mc.set.seed = mc.set.seed,
                                          silent = mc.silent))
    res <- mccollect(jobs)
    names(res) <- NULL
    res <- do.call(c, res)
    if (length(res) != n)
        warning("some results may be missing, folded or caused an error")
    res
}
