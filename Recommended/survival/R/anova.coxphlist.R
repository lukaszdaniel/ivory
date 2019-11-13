# This is usually called from anova.coxph, not a user
#  It's first argument must be a list of coxph models
anova.coxphlist <- function (object, test =  'Chisq' ,...) {
    if (!is.list(object)) stop(gettextf("'%s' argument must be a list", "object"))
    is.coxmodel <- sapply(object, function(x) inherits(x, "coxph"))
    if (!all(is.coxmodel))
        stop("All arguments must be Cox models")    

    is.robust <- sapply(object, function(x) !is.null(object$rscore))
    if (any(is.robust)) stop("Can't do anova tables with robust variances")
    
    ties <- sapply(object, function(x) x$method)
    if (any(ties != ties[1]))
        stop("all models must have the same ties option")

    responses <- as.character(unlist(lapply(object, 
				     function(x) deparse(formula(x)[[2]]))))
    sameresp <- (responses == responses[1])
    if (!all(sameresp)) {
        object <- object[sameresp]
	warning(gettextf("models with response %s removed because response differs from model 1", deparse(responses[!sameresp])))
    }

    ns <- sapply(object, function(x) length(x$residuals))
    if (any(ns != ns[1])) 
        stop("models were not all fit to the same size of dataset")

    nmodels <- length(object)
    if (nmodels == 1) # only one model remains
        return(anova.coxph(object[[1]], test = test))
    # The model ~1 only has one loglik value, hence the rev(x)[1], not x[2]
    loglik <- unlist(lapply(object, function(x) rev(x$loglik)[1]))
    df <- unlist(lapply(object, function(x) 
                        if (!is.null(x$df)) sum(x$df)
                        else if (is.null(coef(x))) 0
                        else sum(!is.na(coef(x)))))

    table <- data.frame(loglik, Chisq= c(NA, abs(2*diff(loglik))), 
                        Df= abs(c(NA, diff(df))))

    tfun <- function(x) paste(as.character(delete.response(terms(formula(x)))),
                              collapse=' ')
    variables <- lapply(object, tfun)
    dimnames(table) <- list(seq_len(nmodels), 
			    c("loglik", "Chisq", "Df"))
    title <- paste(gettext("Analysis of Deviance Table"), "\n ", gettextf("Cox model: response is %s", responses[1]), sep = "")
    topnote <- paste(gettextf(" Model %s:", format(seq_len(nmodels))), variables, collapse = "\n")
    if (!is.null(test)) {
        table[['P(>|Chi|)']] <- pchisq(table$Chisq, table$Df, lower.tail=FALSE)
        }
    structure(table, heading = c(title, topnote), 
			  class = c("anova", "data.frame"))
} 

