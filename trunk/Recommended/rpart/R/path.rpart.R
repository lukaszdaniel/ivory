## submitted by Anantha Prasad 1/26/98

path.rpart <- function(tree, nodes, pretty = 0, print.it = TRUE)
{
    if (!inherits(tree, "rpart"))
	stop(gettextf("'%s' argument is not an object of class %s", "tree", dQuote("rpart")))
    splits <- labels.rpart(tree, pretty = pretty)
    frame <- tree$frame
    n <- row.names(frame)
    node <- as.numeric(n)
    which <- descendants(node)          # ancestors are columns
    path <- list()
    if (missing(nodes)) {
        xy <- rpartco(tree)
        while(length(i <- identify(xy, n = 1L, plot = FALSE)) > 0L) {
            path[[n[i]]] <- path.i <- splits[which[, i]]
            if (print.it) {
                cat("\n", gettext("node number: ", domain = "R-rpart"), n[i], "\n", sep = "")
                cat(paste("  ", path.i), sep = "\n")
            }
        }
    } else {
        if (length(nodes <- node.match(nodes, node)) == 0L)
            return(invisible())
        for (i in nodes) {
            path[[n[i]]] <- path.i <- splits[which[, i]]
            if (print.it) {
                cat("\n", gettext("node number: ", domain = "R-rpart"), n[i], "\n", sep = "")
                cat(paste("  ", path.i), sep = "\n")
            }
        }
    }
    invisible(path)
}
