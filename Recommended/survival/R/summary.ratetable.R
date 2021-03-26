#
# Print out information about a rate table: it's dimensions and keywords
#
summary.ratetable <- function(object, ...) {
    rtable<-object
    if (!inherits(rtable, 'ratetable')) stop(gettextf("'%s' argument is not an object of class %s", "object", dQuote("ratetable")))

    att <- attributes(rtable)
    ncat <- length(dim(rtable))
    cat(sprintf(ngettext(ncat, "Rate table with %d dimension:", "Rate table with %d dimensions:", domain = "R-survival"), ncat), "\n", sep = "")
    if (is.null(att$dimid)) dimid <- names(dimnames(rtable))
    else dimid <- att$dimid
    for (i in seq_len(ncat)) {
        # One of 'factor' (old style table) or "type" (new style) should exist
        if (!is.null(att$factor)) {
            if (att$factor[i]==0) {
                cat("\t", gettextf("%s ranges from %s to %s; with %d categories\n", dimid[i], format(min(att$cutpoints[[i]])), format(max(att$cutpoints[[i]])), att$dim[i], domain = "R-survival"), sep = "")
                }
            else if(att$factor[i]==1) {
                cat("\t", gettextf("%s has levels of: %s", dimid[i], paste(att$dimnames[[i]], collapse = ' '), domain = "R-survival"), "\n", sep = '')
                }
            else {
                cat("\t", gettextf("%s ranges from %s to %s; with %d categories linearly interpolated in %d steps per division\n", dimid[i], format(min(att$cutpoints[[i]])), format(max(att$cutpoints[[i]])), att$dim[i], att$factor[i], domain = "R-survival"), sep = "")
                }
            }
        else {
            if (att$type[i]==1) {
                cat("\t", gettextf("%s has levels of: %s", dimid[i], paste(att$dimnames[[i]], collapse = ' '), domain = "R-survival"), "\n", sep = '')
                }
            else if (att$type[i]>2) { #date
                if (is.numeric(att$cutpoints[[i]])) { #old, numeric
                    # This format is > 5 years out of date
                    #  but some user might keep an old rate table around
                    cat("\t", gettextf("%s ranges from %s to %s; with %d categories", dimid[i], format(as.Date(min(att$cutpoints[[i]]), origin='1960/01/01')), format(as.Date(max(att$cutpoints[[i]]), origin='1960/01/01')), att$dim[i], domain = "R-survival"), "\n", sep='')
                    }
                else # newer, Date
                    cat("\t", dimid[i], " ranges from " , 
                        format(min(att$cutpoints[[i]])), " to ", 
                        format(max(att$cutpoints[[i]])), "; with ", att$dim[i],
                        " categories\n", sep='')
                }

            else {
                cat("\t", gettextf("%s ranges from %s to %s; with %d categories\n", dimid[i], format(min(att$cutpoints[[i]])),
		format(max(att$cutpoints[[i]])), att$dim[i], domain = "R-survival"), sep = '')
                }
            }
        }

    invisible(att)
    }

