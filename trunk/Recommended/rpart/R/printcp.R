## print out the cptable, along with some summary of the tree
printcp <- function(x, digits = getOption("digits") - 2L)
{
    if (!inherits(x, "rpart")) stop(gettextf("'%s' argument is not an object of class %s", "x", dQuote("rpart")))
    cat(switch(x$method,
               anova = gettext("\nRegression tree:\n", domain = "R-rpart") ,
               class = gettext("\nClassification tree:\n", domain = "R-rpart") ,
               poisson = gettext("\nRates regression tree:\n", domain = "R-rpart"),
               exp = gettext("\nSurvival regression tree:\n", domain = "R-rpart"))
        )

    if (!is.null(cl <- x$call)) {
	dput(cl, control = NULL)
	cat("\n")
    }
    frame <- x$frame
    leaves <- frame$var == "<leaf>"
    used <- unique(frame$var[!leaves])

    if (!is.null(used)) {
        cat(gettext("Variables actually used in tree construction:\n", domain = "R-rpart"))
        print(sort(as.character(used)), quote = FALSE)
        cat("\n")
    }


    cat(gettext("Root node error: ", domain = "R-rpart"), format(frame$dev[1L], digits = digits), "/", frame$n[1L], " = ", format(frame$dev[1L]/frame$n[1L], digits = digits), "\n\n", sep = "")


    n <- x$frame$n
    omit <- x$na.action
    if (length(omit)) cat("n=", n[1L], " (", naprint(omit), ")\n\n", sep = "")
    else cat("n=", n[1L], "\n\n")
    print(x$cptable, digits = digits)
    invisible(x$cptable)
}
