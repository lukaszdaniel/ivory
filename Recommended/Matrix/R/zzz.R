### Note that "in theory" even base::as.vector() should be overloaded.
### In practice that could be too much of a performance penalty in some cases.

.MatrixEnv <- new.env(parent = emptyenv(), hash = FALSE)#  e.g., for  once-per-session warnings

.chm_common <- new.env(parent = emptyenv())
## environment in which to store some settings from cholmod_common

.onLoad <- function(libname, pkgname)
{
    .Call(CHM_set_common_env, .chm_common)
    ## S4 method dispatch ambiguity warnings
    if(is.null(getOption("ambiguousMethodSelection"))) {
	if((!is.null(v <- getOption("Matrix.verbose")) && v >= 1) ||
	   isTRUE(getOption("Matrix.ambiguityNotes")) ||
	   interactive() && identical(Sys.info()[["user"]], "maechler")) {
	    ## nothing
	} else { # ambiguity notices are trashed
	    N <- function(cond) NULL; environment(N) <- emptyenv()
	    options(ambiguousMethodSelection = N)
	    assign("no.methods.ambiguityNotes", TRUE, envir=.MatrixEnv)
	}
    }
}

Rv <- getRversion() # removed at end

## Instead, simply re-assign the [cr]bind()s which are recursively
## based on [cr]bind2 :
##
## save to cBind / rBind  ("rename")
## R >= "3.2.0" :
## New (2015-02)  base :: cbind(), rbind() which dispatch on S4 "when needed":
    cBind <- function (..., deparse.level = 1) {
	    .Defunct(msg = gettext("'cBind' is defunct. Since R version 3.2.0, base's cbind() should work fine with S4 objects"))
    }
    rBind <- function (..., deparse.level = 1) {
	    .Defunct(msg = gettext("'rBind' is defunct. Since R version 3.2.0, base's rbind() should work fine with S4 objects"))
    }

if(Rv < "4.0.0") {
  deparse1 <- function (expr, collapse = " ", width.cutoff = 500L, ...)
      paste(deparse(expr, width.cutoff, ...), collapse = collapse)
  ## not equivalent ...
  ...length <- function() eval(quote(length(list(...))), sys.frame(-1L))

  if(Rv < "3.5.0") {
    isFALSE <- function (x) is.logical(x) && length(x) == 1L && !is.na(x) && !x
    isTRUE  <- function (x) is.logical(x) && length(x) == 1L && !is.na(x) && x
  }
}
rm(Rv)


.onUnload <- function(libpath)
{
    library.dynam.unload("Matrix", libpath)
    if(isTRUE(.MatrixEnv $ no.methods.ambiguityNotes))# revert
	options(ambiguousMethodSelection = NULL)
}

.SuiteSparse_version <- function() {
    ssv <- .Call(get_SuiteSparse_version)
    package_version(list(major = ssv[1], minor = paste(ssv[2:3], collapse=".")))
}
