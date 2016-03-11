#  File src/library/utils/R/windows/create.post.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2012 The R Core Team
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
        cat(gettextf("The unsent %s can be found in file %s", description, normalizePath(filename), domain = "R-utils"), "\n", sep ="")
    }

    if (method == "none")
        none_method()
    else if(method == "ess")
	cat(body, sep = "\n")
    else if(method == "gnudoit") {
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
        uri <- paste0("mailto:", address,
                     "?subject=", subject,
                     if(is.character(ccaddress) && nzchar(ccaddress))
                         paste0("&cc=", ccaddress),
                     "&body=", paste(body, collapse="\r\n"))
        tryCatch(shell.exec(URLencode(uri)), error = function(e) {
            cat(gettext("opening the mailer failed, so reverting to 'mailer=\"none\"'"), "\n", sep = "")
            flush.console()
            Sys.sleep(5)
            none_method()
        })
    } else if(method == "mailx")
	stop("method 'mailx' is Unix-only")

    invisible()
}
