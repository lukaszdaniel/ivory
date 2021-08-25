#  File src/library/stats/R/t.test.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2019 The R Core Team
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

t.test <- function(x, ...) UseMethod("t.test")

t.test.default <-
function(x, y = NULL, alternative = c("two.sided", "less", "greater"),
         mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95,
         ...)
{
    alternative <- match.arg(alternative)

    if(!missing(mu) && (length(mu) != 1 || is.na(mu)))
        stop("'mu' must be a single number")
    if(!missing(conf.level) &&
       (length(conf.level) != 1 || !is.finite(conf.level) ||
        conf.level < 0 || conf.level > 1))
        stop(gettextf("'%s' argument must be a single number between 0 and 1", "conf.level"))
    if( !is.null(y) ) {
        dname <- gettextf("%s and %s", paste(deparse1(substitute(x)), collapse = ""), paste(deparse1(substitute(y)), collapse = ""), domain = "R-stats")
	if(paired)
	    xok <- yok <- complete.cases(x,y)
	else {
	    yok <- !is.na(y)
	    xok <- !is.na(x)
	}
	y <- y[yok]
    }
    else {
	dname <- deparse1(substitute(x))
	if (paired) stop("'y' is missing for paired test")
	xok <- !is.na(x)
	yok <- NULL
    }
    x <- x[xok]
    if (paired) {
	x <- x-y
	y <- NULL
    }
    nx <- length(x)
    mx <- mean(x)
    vx <- var(x)
    if(is.null(y)) {
        if(nx < 2) stop(gettextf("not enough '%s' observations", "x"))
	df <- nx-1
	stderr <- sqrt(vx/nx)
        if(stderr < 10 *.Machine$double.eps * abs(mx))
            stop("data are essentially constant")
	STATISTIC <- (mx-mu)/stderr
	METHOD <- if(paired) gettext("Paired t-test") else gettext("One Sample t-test")
	ESTIMATE <- setNames(mx, if(paired) gettext("mean difference") else gettext("mean of x"))
    } else {
	ny <- length(y)
        if(nx < 1 || (!var.equal && nx < 2))
            stop(gettextf("not enough '%s' observations", "x"))
	if(ny < 1 || (!var.equal && ny < 2))
            stop(gettextf("not enough '%s' observations", "y"))
        if(var.equal && nx+ny < 3) stop("not enough observations")
	my <- mean(y)
	vy <- var(y)
	METHOD <- if(!var.equal) gettext("Welch Two Sample t-test") else gettext("Two Sample t-test")
	ESTIMATE <- c(mx,my)
	names(ESTIMATE) <- c(gettext("mean of x"),gettext("mean of y"))
	if(var.equal) {
	    df <- nx+ny-2
            v <- 0
            if(nx > 1) v <- v + (nx-1)*vx
            if(ny > 1) v <- v + (ny-1)*vy
	    v <- v/df
	    stderr <- sqrt(v*(1/nx+1/ny))
	} else {
	    stderrx <- sqrt(vx/nx)
	    stderry <- sqrt(vy/ny)
	    stderr <- sqrt(stderrx^2 + stderry^2)
	    df <- stderr^4/(stderrx^4/(nx-1) + stderry^4/(ny-1))
	}
        if(stderr < 10 *.Machine$double.eps * max(abs(mx), abs(my)))
            stop("data are essentially constant")
        STATISTIC <- (mx - my - mu)/stderr
    }
    if (alternative == "less") {
	PVAL <- pt(STATISTIC, df)
	CINT <- c(-Inf, STATISTIC + qt(conf.level, df) )
    }
    else if (alternative == "greater") {
	PVAL <- pt(STATISTIC, df, lower.tail = FALSE)
	CINT <- c(STATISTIC - qt(conf.level, df), Inf)
    }
    else {
	PVAL <- 2 * pt(-abs(STATISTIC), df)
	alpha <- 1 - conf.level
        CINT <- qt(1 - alpha/2, df)
	CINT <- STATISTIC + c(-CINT, CINT)
    }
    CINT <- mu + CINT * stderr
    names(STATISTIC) <- "t"
    names(df) <- "df"
    names(mu) <- if(paired) gettext("mean difference", domain = "R-stats")
                 else if(!is.null(y)) gettext("difference in means", domain = "R-stats")
                 else gettext("mean", domain = "R-stats")
    attr(CINT,"conf.level") <- conf.level
    if(paired || !is.null(y)) {
    alt.name <- switch(alternative,
                           two.sided = gettextf("true difference in means is not equal to %s", mu, domain = "R-stats"),
                           less = gettextf("true difference in means is less than %s", mu, domain = "R-stats"),
                           greater = gettextf("true difference in means is greater than %s", mu, domain = "R-stats"))
   } else {
    alt.name <- switch(alternative,
                           two.sided = gettextf("true mean is not equal to %s", mu, domain = "R-stats"),
                           less = gettextf("true mean is less than %s", mu, domain = "R-stats"),
                           greater = gettextf("true mean is greater than %s", mu, domain = "R-stats"))
   }

    rval <- list(statistic = STATISTIC, parameter = df, p.value = PVAL,
	       conf.int = CINT, estimate = ESTIMATE, null.value = mu,
	       stderr = stderr,
	       alternative = alternative,
	       alt.name = alt.name,
	       method = METHOD, data.name = dname)
    class(rval) <- "htest"
    rval
}

t.test.formula <-
function (formula, data, subset, na.action, ...) 
{
    if (missing(formula) || (length(formula) != 3L))
        stop(gettextf("'%s' argument is missing or incorrect", "formula"))
    oneSampleOrPaired <- FALSE
    if (length(attr(terms(formula[-2L]), "term.labels")) != 1L) 
        if (formula[[3]] == 1L)
            oneSampleOrPaired <- TRUE
        else
            stop(gettextf("'%s' argument is missing or incorrect", "formula"))
    m <- match.call(expand.dots = FALSE)
    if (is.matrix(eval(m$data, parent.frame()))) 
        m$data <- as.data.frame(data)
    ## need stats:: for non-standard evaluation
    m[[1L]] <- quote(stats::model.frame)
    m$... <- NULL
    mf <- eval(m, parent.frame())
    if(length(names(mf)) == 2)
         DNAME <- gettextf("%s by %s ", names(mf)[1], names(mf)[2]) # works in all cases
    else DNAME <- names(mf)
    names(mf) <- NULL
    response <- attr(attr(mf, "terms"), "response")
    if (! oneSampleOrPaired) {
        g <- factor(mf[[-response]])
        if (nlevels(g) != 2L) 
            stop("grouping factor must have exactly 2 levels")
        DATA <- setNames(split(mf[[response]], g), c("x", "y"))
        y <- do.call("t.test", c(DATA, list(...)))
        if (length(y$estimate) == 2L) {
            names(y$estimate) <- gettextf("mean in group %s", levels(g))
            names(y$null.value) <-
                gettextf("difference in means between group %s and group %s", levels(g)[1], levels(g)[2])
        }
    }
    else { # 1-sample and paired tests
        respVar <- mf[[response]]
        if (inherits(respVar, "Pair")){
            DATA <- list(x = respVar[,1], y = respVar[,2], paired=TRUE)
            y <- do.call("t.test", c(DATA, list(...)))
        }
        else {
            DATA <- list(x = respVar)
            y <- do.call("t.test", c(DATA, list(...)))
        }
    }
    y$data.name <- DNAME
    y
}
