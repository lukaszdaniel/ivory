

rfs <-
    function(model, layout = c(2,1), xlab = gettext("f-value", domain = "R-lattice"), ylab = NULL,
             distribution = qunif,
             panel = function(...) {
                 panel.grid(h = -1, v = -1)
                 panel.qqmath(...)
             },
             prepanel = NULL, strip = TRUE, ...)
{
    if (!is.function(panel)) panel <- eval(panel)
    if (!is.function(strip)) strip <- eval(strip)
    fitval <- fitted.values(model) - mean(fitted.values(model))
    resids <- residuals(model)
    nf <- length(fitval)
    nr <- length(resids)
    data <- list(y = c( fitval, resids),
                 f = c( rep(gettext("Fitted Values minus Mean", domain = "R-lattice"), nf),
                 rep(gettext("Residuals", domain = "R-lattice"), nr)))
    qqmath(~y|f, data = data, layout = layout, xlab = xlab, ylab = ylab,
           distribution = distribution, panel = panel,
           prepanel = prepanel, strip = strip, ...)
}
