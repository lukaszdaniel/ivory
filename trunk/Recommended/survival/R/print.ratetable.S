## $Id: print.ratetable.S 11166 2008-11-24 22:10:34Z therneau $

print.ratetable <- function(x, ...) {
  cat(ngettext(length(attr(x, 'dimid')), "Rate table with dimension: ", "Rate table with dimensions: ", domain = "R-survival"), paste(attr(x, 'dimid'), collapse = ", "), "\n", sep = "")
  attributes(x) <- attributes(x)[c("dim", "dimnames")]
  NextMethod()
}
