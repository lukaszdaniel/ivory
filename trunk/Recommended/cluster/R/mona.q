
mona <- function(x)
{
    ## check type of input matrix
    if(!is.matrix(x) && !is.data.frame(x))
        stop("'x' must be a matrix or data frame.")
    if(!all(vapply(lapply(as.data.frame(x),
			  function(y) levels(as.factor(y))),
		   length, 1) == 2))
        stop("All variables must be binary (e.g., factor with 2 levels).")
    n <- nrow(x)
    jp <- ncol(x)
    ## change levels of input matrix

    x2 <- apply(as.matrix(x), 2, function(x) as.integer(factor(x))) - 1L
    x2[is.na(x2)] <- 2:2
## was
##     x2 <- apply(as.matrix(x), 2, factor)
##     x2[x2 == "1"] <- "0"
##     x2[x2 == "2"] <- "1"
##     x2[is.na(x2)] <- "2"
##     storage.mode(x2) <- "integer"

    ## call Fortran routine
    res <- .Fortran(cl_mona,
                    as.integer(n),
                    as.integer(jp),
                    x2 = x2,# x[,]
                    error = 0L,
                    nban = integer(n),
                    ner = integer(n),
                    integer(n),
                    lava = integer(n),
                    integer(jp))

    ## stop with a message when two many missing values:
    if(res$error != 0) {
        ## NB: Need "full simple strings below, to keep it translatable":
        switch(res$error,
               ## 1 :
               stop("No clustering performed, an object was found with all values missing."),
               ## 2 :
               stop("No clustering performed, found variable with more than half values missing."),
               ## 3 : never triggers because of binary check above
               stop("No clustering performed, a variable was found with all non missing values identical."),
               ## 4 :
               stop("No clustering performed, all variables have at least one missing value.")
               )
    }
    ##O res$x2 <- matrix(as.numeric(substring(res$x2,
    ##O                                      seq_len(nchar(res$x2)), seq_len(nchar(res$x2)))),
    ##O                      n, jp)
    storage.mode(res$x2) <- "integer" # keeping dim()
    dimnames(res$x2) <- dnx <- dimnames(x)
    ## add labels to Fortran output
    if(length(dnx[[2]]) != 0) {
        lava <- as.character(res$lava)
        lava[lava != "0"] <- dnx[[2]][res$lava]
        lava[lava == "0"] <- "NULL"
        res$lava <- lava
    }
    ## construct "mona" object
    clustering <- list(data = res$x2, order = res$ner,
                       variable = res$lava[ -1 ], step = res$nban[-1],
                       call = match.call())
    if(length(dnx[[1]]) != 0)
        clustering$order.lab <- dnx[[1]][res$ner]
    class(clustering) <- "mona"
    clustering
}

print.mona <- function(x, ...)
{
    cat(gettext("Revised data:", domain = "R-cluster"), "\n", sep = "")
    print(x$data, quote = FALSE, ...)
    cat(gettext("Order of objects:", domain = "R-cluster"), "\n", sep = "")
    print(if (length(x$order.lab) != 0) x$order.lab else x$order,
          quote = FALSE, ...)
    cat(gettext("Variable used:", domain = "R-cluster"), "\n", sep = "")
    print(x$variable, quote = FALSE, ...)
    cat(gettext("Separation step:", domain = "R-cluster"), "\n", sep = "")
    print(x$step, ...)
    cat("\n", gettext("Available components:", domain = "R-cluster"), "\n", sep = "")
    print(names(x), ...)
    invisible(x)
}

## FIXME: These should differ from print()

summary.mona <- function(object, ...)
{
    class(object) <- "summary.mona"
    object
}

print.summary.mona <- function(x, ...)
{
    print.mona(x, ...)
    invisible(x)
}

