###          Extract variance components of lme models.
###
### Copyright 2007-2017 The R Core team
### Copyright 1997-2003  Jose C. Pinheiro,
###                      Douglas M. Bates <bates@stat.wisc.edu>
###
### This program is free software; you can redistribute it and/or modify
### it under the terms of the GNU General Public License as published by
### the Free Software Foundation; either version 2 of the License, or
### (at your option) any later version.
###
### This program is distributed in the hope that it will be useful,
### but WITHOUT ANY WARRANTY; without even the implied warranty of
### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
### GNU General Public License for more details.
###
### A copy of the GNU General Public License is available at
### http://www.r-project.org/Licenses/

VarCorr <- function(x, sigma = 1, ...) UseMethod("VarCorr")

VarCorr.lme <- function(x, sigma = x$sigma, rdig = 3, ...)
{
  m <- lapply(rev(x$modelStruct$reStruct), VarCorr,
              sigma = sigma, rdig = rdig, ...)
  Q <- length( m )
  if (Q <= 1) {
    nm <- names(m)
    m <- m[[1]]
    mm <- rbind(m, Residual = c(Variance = sigma^2, StdDev = sigma))
    v <- array( "", dim(mm), dimnames(mm) )
    v[, 1] <- format( mm[, 1] )
    v[, 2] <- format( mm[, 2] )
    if (!is.null(corr <- attr(m, "corr"))) {
      v <- cbind(v, rbind(corr, Residual = rep("", ncol(corr))))
    }
    return(structure(v, title = paste(nm, "=", attr( m, "formStr" )),
                     class = "VarCorr.lme"))
  }
  ## multiple nested levels case: Q >= 2
  nrows <- vapply(m, nrow, 1L)
  trows <- 1L + c(0L, cumsum(1L + nrows))[seq_len(Q)]
  bd <- rbind(do.call(rbind, m),
              c(Variance = sigma^2, StdDev = sigma) )
  corr <- lapply( m, attr, which = "corr")
  colnames <- colnames(bd)
  maxCorr <- 0L
  if (!all( Nulls <- vapply(corr, is.null, NA) )) {
    maxCorr <- max(vapply(corr[!Nulls], ncol, 1L))
    colnames <- c( colnames, "Corr", rep("", maxCorr - 1L) )
  }
  v <- array("", c(sum(nrows) + Q + 1L, 2L + maxCorr), list(NULL, colnames))
  v[-trows, 1] <- format(bd[, 1])
  v[-trows, 2] <- format(bd[, 2])
  v[trows, 1] <- sapply( m, attr, which = "formStr" )
  rownames <- rep("", sum(nrows) + Q)
  rownames[trows] <- paste( names( m ), "=" )
  rr <- 1L
  for (i in seq_along( m ) ) {
    ri <- rr + seq_len(nrows[i])
    rownames[ri] <- rownames(m[[i]])
    if (!is.null(corr[[i]])) {
      v[ri, 2L + seq_len(ncol(corr[[i]])) ] <- corr[[i]]
    }
    rr <- rr + nrows[i] + 1L
  }
  rownames(v) <- c(rownames, "Residual")
  class(v) <- "VarCorr.lme"
  v
}

print.VarCorr.lme <- function(x, ...)
{
  if(hasT <- !is.null(tit <- attr(x, "title"))) {
    cat(tit, "\n")
    xo <- x  ## print(x, *)  must return 'x' unchanged
    attr(x, "title") <- NULL
  }
  print(unclass(x), ..., quote = FALSE)
  invisible(if(hasT) xo else x)
}


VarCorr.pdMat <- function( x, sigma = 1., rdig = 3, ...)
{
  sx <- summary( x )
  sd <- sigma * attr( sx, "stdDev" )
  var <- sd^2
  p <- dim(sx)[2]
  v <- array(c(var, sd), c(p, 2), list( names(sd), c( "Variance", "StdDev" )))
#   attr(v, "formStr") <- deparse(as.call(list(as.name(class(x)[[1]]),
#                                        as.vector(attr(x, "formula")))))
# ## puts in an extra blank.  We'll do it the clunky way instead
  attr(v, "formStr") <-
    if ( inherits( attr(x, "formula"), "listForm" ) ) {# an nlme'ism
      paste0(class(x)[[1]], "(list(",
             paste( sapply(attr(x, "formula"),
                           function(x) as.character(deparse(x))),
                   collapse=","), "))")
    } else {
      paste0(class(x)[[1]], "(",
             substring(deparse(attr(x, "formula")), 2), ")")
    }

  if (p >= 2L && !attr(sx, "noCorrelation")) {
    ll <- lower.tri(sx)
    sx[ll] <- format(round(sx[ll], digits = rdig))
    sx[!ll] <- ""
    if (!is.null(colnames(sx))) {
        sx[1,] <- abbreviate(colnames(sx), minlength = rdig + 3)
    }
    dimnames(sx) <- list(names(sd), c("Corr", rep("", p - 1L)))
    attr(v, "corr") <- sx[, -p, drop = FALSE ]
  }
  v
}

VarCorr.pdBlocked <- function( x, sigma = 1., rdig = 3, ...)
{
  m <- lapply(X=x, FUN=VarCorr, sigma = sigma, rdig = rdig, ...)
  bd <- do.call(rbind, m)
## the following code does not appear to be used at all
##   corr <- lapply( m, attr, which = "corr")
##   maxCorr <- 0
##   if ( !all( Nulls <- sapply( corr, is.null ) ) ) {
##     maxCorr <- max( sapply( corr[!Nulls], ncol ) )

##   }
  attr(bd, "formStr") <-
    paste( sapply( m, attr, which = "formStr" ), collapse = ", ")
  bd
}
