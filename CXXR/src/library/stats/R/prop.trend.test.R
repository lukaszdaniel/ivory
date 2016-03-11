#  File src/library/stats/R/prop.trend.test.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2015 The R Core Team
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 3 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  https://www.R-project.org/Licenses/

prop.trend.test <- function (x, n, score = seq_along(x))
{
    METHOD <- gettext("Chi-squared Test for Trend in Proportions", domain = "R-stats")
    DNAME <- gettextf("%s out of %s,\n using scores: %s", paste(deparse(substitute(x)), collapse = ""), paste(deparse(substitute(n)), collapse = ""), paste(score, collapse = " "), domain = "R-stats")

    ## Tabular input has caused grief, get rid of dim() attributes:
    x <- as.vector(x)
    n <- as.vector(n)

    p <- sum(x)/sum(n)
    w <- n/p/(1 - p) # <- workaround 'codetools' inability to see the 'weights' in 'data':
    a <- anova(lm(freq ~ score, data = list(freq = x/n, score = as.vector(score)),
		  weights = w))
    chisq <- a["score", "Sum Sq"]
    names(chisq) <- gettext("X-squared", domain = "R-stats")
    DF <- 1
    names(DF) <- "df"
    structure(list(statistic = chisq,
                   parameter = DF,
                   p.value = pchisq(as.numeric(chisq), 1, lower.tail = FALSE),
                   method = METHOD, data.name = DNAME),
              class = "htest")
}
