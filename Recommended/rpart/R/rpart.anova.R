rpart.anova <- function(y, offset, parms, wt)
{
    if (!is.null(offset)) y <- y - offset
    list(y = y, parms = NULL, numresp = 1L, numy = 1L,
	 summary = function(yval, dev, wt, ylevel, digits ) {
	     gettextf("  mean=%s, MSE=%s", formatg(yval, digits), formatg(dev/wt, digits), domain = "R-rpart")
         },
	 text = function(yval, dev, wt, ylevel, digits, n, use.n ) {
	     if (use.n) paste0(formatg(yval, digits), "\nn=", n) else
             formatg(yval, digits)
         })
}
