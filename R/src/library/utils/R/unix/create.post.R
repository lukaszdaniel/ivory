#  File src/library/utils/R/unix/create.post.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2017 The R Core Team
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

create.post <- function(instructions = character(),
                        description = "post",
                        subject = "",
                        method = getOption("mailer"),
                        address = gettext("the relevant mailing list", domain = "R-utils"),
                        ccaddress = getOption("ccaddress", ""),
                        filename = "R.post",
                        info = character())
{
    method <-
	if(is.null(method)) "none"
	else match.arg(method, c("mailto", "mailx", "gnudoit", "none", "ess"))
    open_prog <- if(grepl("-apple-darwin", R.version$platform)) "open" else "xdg-open"
    if (method == "mailto")
        if(!nzchar(Sys.which(open_prog))) {
            browser <- Sys.getenv("R_BROWSER", "")
            if(!nzchar(browser)) {
                warning("cannot find program to open 'mailto:' URIs: reverting to 'method=\"none\"'")
                flush.console()
                Sys.sleep(5)
            } else {
                message("Using the browser to open a mailto: URI")
                open_prog <- browser
            }
        }

    body <- c(instructions,
              gettext("--please do not edit the information below--", domain = "R-utils"), "",
              info)

    none_method <- function() {
        disclaimer <-
            paste0(gettextf("# Your mailer is set to \"none\",\n# hence we cannot send the, %s directly from R.\n# Please copy the %s (after finishing it) to\n# your favorite email program and send it to\n#\n#       %s", description, description, address),
		   "\n#\n######################################################\n\n\n")

        cat(c(disclaimer, body), file = filename, sep = "\n")
        cat(gettextf("The %s is being opened for you to edit.", description, domain = "R-utils"), "\n", sep = "")
        flush.console()
        file.edit(filename)
        cat(gettextf("The unsent %s can be found in file %s", description, sQuote(filename), domain = "R-utils"), "\n", sep ="")
    }

    if(method == "none") {
        none_method()
    } else if(method == "mailx") {
        if(missing(address)) stop("must specify 'address'")
        if(!nzchar(subject)) stop("'subject' is missing")
        if(length(ccaddress) != 1L) stop(gettextf("'%s' argument must be of length 1", "ccaddress"))

	cat(body, file=filename, sep = "\n")
        cat(gettextf("The %s is being opened for you to edit.", description, domain = "R-utils"), "\n", sep = "")
        file.edit(filename)

        if(is.character(ccaddress) && nzchar(ccaddress)) {
            cmdargs <- paste("-s", shQuote(subject),
                             "-c", shQuote(ccaddress),
                             shQuote(address),
                             "<", filename, "2>/dev/null")
        }
        else
            cmdargs <- paste("-s", shQuote(subject),
                             shQuote(address), "<",
                             filename, "2>/dev/null")
        status <- 1L
        answer <- askYesNo(gettextf("Email the %s now?", description))
        if(length(answer)) {
            cat(gettext("Sending email ...", domain = "R-utils"), "\n", sep = "")
            status <- system(paste("mailx", cmdargs), , TRUE, TRUE)
            if(status)
                status <- system(paste("Mail", cmdargs), , TRUE, TRUE)
            if(status)
                status <- system(paste("/usr/ucb/mail", cmdargs), , TRUE, TRUE)

            if(status == 0L) unlink(filename)
            else {
                cat(gettext("Sending email failed!", domain = "R-utils"), "\n", sep = "")
                cat(gettextf("The unsent %s can be found in file %s", description, sQuote(filename), domain = "R-utils"), "\n", sep = "")
            }
        } else
            cat(gettextf("The unsent %s can be found in file %s", description, filename, domain = "R-utils"), "\n", sep = "")
    } else if(method == "ess") {
	cat(body, sep = "\n")
    } else  if(method == "gnudoit") {
        ## FIXME: insert subject and ccaddress
	cmd <- paste0("gnudoit -q '",
		     "(mail nil \"", address, "\")",
		     "(insert \"", paste(body, collapse = "\\n"), "\")",
		     "(search-backward \"Subject:\")",
		     "(end-of-line)'")
	system(cmd)
    } else if(method == "mailto") {
        if (missing(address)) stop("must specify 'address'")
        if (!nzchar(subject)) subject <- "<<Enter Meaningful Subject>>"
        if(length(ccaddress) != 1L) stop(gettextf("'%s' argument must be of length 1", "ccaddress"))
        cat(gettextf("The %s is being opened in your default mail program\nfor you to complete and send.", description, domain = "R-utils"), "\n", sep = "")
        ## The mailto: standard (RFC2368) says \r\n for the body
        uri <- paste0("mailto:", address,
                     "?subject=", subject,
                     if(is.character(ccaddress) && nzchar(ccaddress))
                         paste0("&cc=", ccaddress),
                     "&body=", paste(body, collapse = "\r\n"))
        if(system2(open_prog, shQuote(URLencode(uri)), FALSE, FALSE)) {
            cat(gettext("opening the mailer failed, so reverting to 'mailer=\"none\"'"), "\n", sep = "")
            flush.console()
            none_method()
        }
    }
    invisible()
}
