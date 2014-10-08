meanvar.rpart <- function(tree, xlab = gettext("ave(y)"), ylab = gettext("ave(deviance)"), ...)

{
    if (!inherits(tree, "rpart"))
        stop(gettextf("'%s' argument is not an object of class %s", "tree", dQuote("rpart")))
    if (!tree$method == "anova")
        stop("Plot is not useful for classification or poisson trees")
    frame <- tree$frame
    frame <- frame[frame$var == "<leaf>", ]
    x <- frame$yval
    y <- frame$dev/frame$n
    label <- row.names(frame)
    plot(x, y, xlab = xlab, ylab = ylab, type = "n", ...)
    text(x, y, label)
    invisible(list(x = x, y = y, label = label))
}

meanvar <- function(tree, ...) UseMethod("meanvar")
