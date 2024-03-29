#  File src/library/utils/R/browseVignettes.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2015 The R Core Team
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



browseVignettes <- function(package = NULL, lib.loc = NULL, all = TRUE)
{

    vinfo <- tools::getVignetteInfo(package, lib.loc, all)
    pkgs <- unique(vinfo[, "Package"])
    db <- lapply(pkgs, function(p) vinfo[vinfo[,"Package"] == p,,drop=FALSE])
    names(db) <- pkgs
    attr(db, "call") <- sys.call()
    attr(db, "footer") <-
        if (all) ""
        else sprintf(gettext("Use <code> %s </code> \n to list the vignettes in all <strong>available</strong> packages."),
                     "browseVignettes(all = TRUE)")
    class(db) <- "browseVignettes"
    return(db)
}

print.browseVignettes <- function(x, ...)
{
    if (length(x) == 0L) {
        message(gettextf("No vignettes found by %s", deparse1(attr(x, "call")), domain = "R-utils"),
                domain = NA)
        return(invisible(x))
    }

    oneLink <- function(s) {
        if (length(s) == 0L) return(character(0L))
        title <- s[, "Title"]
        if (port > 0L)
            prefix <- sprintf("/library/%s/doc", pkg)
        else
            prefix <- sprintf("file://%s/doc", s[, "Dir"])
        src <- s[, "File"]
        pdf <- s[, "PDF"]
        rcode <- s[, "R"]
        pdfext <- sub("^.*\\.", "", pdf)
        sprintf("  <li>%s  -  \n    %s  \n    %s  \n    %s \n  </li>\n",
                title,
                ifelse(nzchar(pdf),
                       sprintf("<a href='%s/%s'>%s</a>&nbsp;", prefix, pdf, toupper(pdfext)), ""),
		sprintf("<a href='%s/%s'>source</a>&nbsp;", prefix, src),
		ifelse(nzchar(rcode), sprintf("<a href='%s/%s'>R code</a>&nbsp;", prefix, rcode), ""))
    }

    port <- tools::startDynamicHelp(NA)

    file <- tempfile("Rvig.", fileext=".html")
    sink(file)
    if (port > 0L)
    	css_file <- "/doc/html/R.css"
    else
    	css_file <- file.path(R.home("doc"), "html", "R.css")
    cat(sprintf("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\">
<head>
<title>R Vignettes</title>
<meta http-equiv='Content-Type' content='text/html; charset=utf-8' />
<meta name='viewport' content='width=device-width, initial-scale=1.0, user-scalable=yes' />
<link rel='stylesheet' type='text/css' href='%s' />
</head>\n<body>\n<div class='container'>\n", css_file))
    cat(sprintf("<h2>Vignettes found by <code><q>%s</q></code></h2>",
                deparse1(attr(x, "call"))))
    cat("<div class=\"vignettes\">")
    for (pkg in names(x))
    {
        cat(sprintf("<h3>Vignettes in package <code>%s</code></h3>\n", pkg))
        cat("<ul>\n")
        links <- oneLink(x[[pkg]])
        cat(paste(links), collapse = "\n")
        cat("\n</ul>\n")
    }
    cat("</div>")
    cat("<hr/>")
    if(nzchar(foot <- attr(x, "footer")))
        cat(sprintf("<p>%s</p>", foot))
    cat("</div>\n</body>\n</html>\n")
    sink()
    ## the first two don't work on Windows with browser=NULL.
    ## browseURL(URLencode(sprintf("file://%s", file)))
    ## browseURL(URLencode(file))
    if (port > 0L)
	browseURL(sprintf("http://127.0.0.1:%d/session/%s", port, basename(file)))
    else
    	browseURL(sprintf("file://%s", file))
    ## browseURL(file)
    invisible(x)
}
