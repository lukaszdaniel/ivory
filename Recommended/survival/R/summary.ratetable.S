# $Id: summary.ratetable.S 11437 2010-10-28 02:21:16Z therneau $
#
# Print out information about a rate table: it's dimensions and keywords
#
summary.ratetable <- function(object, ...) {
    rtable<-object
    if (!inherits(rtable, 'ratetable')) stop(gettextf("'%s' argument is not an object of class %s", "object", dQuote("ratetable")))

    att <- attributes(rtable)
    ncat <- length(dim(rtable))
    cat(gettextf(" Rate table with %d dimensions:\n", ncat, domain = "R-survival"))
    for (i in 1:ncat) {
        # One of 'factor' (old style table) or "type" (new style) should exist
        if (!is.null(att$factor)) {
            if (att$factor[i]==0) {
                cat("\t", gettextf("%s ranges from %s to %s; with %d categories\n", att$dimid[i], format(min(att$cutpoints[[i]])), format(max(att$cutpoints[[i]])), att$dim[i], domain = "R-survival"), sep = "")
                }
            else if(att$factor[i]==1) {
                cat("\t", gettextf("%s has levels of: %s", att$dimid[i], paste(att$dimnames[[i]], collapse = ' '), domain = "R-survival"), "\n", sep = '')
                }
            else {
                cat("\t", gettextf("%s ranges from %s to %s; with %d categories linearly interpolated in %d steps per division\n", att$dimid[i], format(min(att$cutpoints[[i]])), format(max(att$cutpoints[[i]])), att$dim[i], att$factor[i], domain = "R-survival"), sep = "")
                }
            }
        else {
            if (att$type[i]==1) {
                cat("\t", gettextf("%s has levels of: %s", att$dimid[i], paste(att$dimnames[[i]], collapse = ' '), domain = "R-survival"), "\n", sep = '')
                }
            else if (att$type[i]>2) { #date
                cat("\t", gettextf("%s ranges from %s to %s; with %d categories\n", att$dimid[i], format(as.Date(min(att$cutpoints[[i]]), origin='1960/01/01')),
                 format(as.Date(max(att$cutpoints[[i]]), origin='1960/01/01')), att$dim[i], domain = "R-survival"), sep = '')
                }

            else {
                cat("\t", gettextf("%s ranges from %s to %s; with %d categories\n", att$dimid[i], format(min(att$cutpoints[[i]])),
		format(max(att$cutpoints[[i]])), att$dim[i], domain = "R-survival"), sep = '')
                }
            }
        }
            
    invisible(att)
    }

