## $Id: print.ratetable.S 11166 2008-11-24 22:10:34Z therneau $

print.ratetable <- function(x, ...) {
  cat(sprintf(ngettext(length(attr(x, 'dimid')), "Rate table with %d dimension:", "Rate table with %d dimensions:", domain = "R-survival"), length(attr(x, 'dimid'))), " ", paste(attr(x, 'dimid'), collapse = ", "), "\n", sep = "")
  attributes(x) <- attributes(x)[c("dim", "dimnames")]
  NextMethod()
}
