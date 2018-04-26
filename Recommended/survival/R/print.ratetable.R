
print.ratetable <- function(x, ...) {
    if (is.null(attr(x, 'dimid')))
        cat(sprintf(ngettext(length(x), "Rate table with %d dimension:", "Rate table with %d dimesions:", domain = "R-survival"), length(x)), names(dimnames(x)), "\n")
    else  cat(sprintf(ngettext(length(attr(x, 'dimid')), "Rate table with %d dimension:", "Rate table with %d dimensions:", domain = "R-survival"), length(attr(x, 'dimid'))), " ", paste(attr(x, 'dimid'), collapse = ", "), "\n", sep = "")
  attributes(x) <- attributes(x)[c("dim", "dimnames")]
  NextMethod()
}
