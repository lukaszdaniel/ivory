#  File src/library/methods/R/show.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2019 The R Core Team
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

showDefault <- function(object, oldMethods = TRUE)
{
    clDef <- getClass(cl <- class(object), .Force=TRUE)
    if(!missing(oldMethods))
        .Deprecated(msg =
	gettext("the 'oldMethods' argument is deprecated, as it has been unused since R 1.7.0", domain = "R-methods"))
    if(!is.null(clDef) && isS4(object) && is.na(match(clDef@className, .BasicClasses)) ) {
        cat(gettextf("An object of class %s", classLabel(cl), domain = "R-methods"), "\n", sep = "")
        slots <- slotNames(clDef)
        dataSlot <- .dataSlot(slots)
        if(length(dataSlot) > 0) { # show data part and remove it from slots :
            show(slot(object, dataSlot))
            slots <- slots[is.na(match(slots, dataSlot))]
        }
        else if(length(slots) == 0L)
            show(unclass(object))
        for(what in slots) {
            if(what == ".Data")
                next ## should have been done above
            cat(gettextf("Slot %s:", deparse(what), domain = "R-methods"), "\n", sep = "")
            print(slot(object, what))
            cat("\n")
        }
    }
    else
        ## NBB:  This relies on the delicate fact (as of version 1.7 at least)
        ## that print will NOT recursively call show if it gets more than one argument!
        print(object, useS4 = FALSE)
    invisible() # documented return for show().
}

.extraSlotsDone <- new.env() # any unique reference value would do

showExtraSlots <- function(object, ignore) {
    if(is(ignore, "classRepresentation"))
      ignore <- slotNames(ignore)
    else if(!is(ignore, "character"))
      stop(gettextf("invalid 'ignore' argument; should be a class definition or a character vector, got an object of class %s", dQuote(class(ignore))),
           domain = "R-methods")
    slots <- slotNames(class(object))
    for(s in slots[is.na(match(slots, ignore))]) {
        cat(gettextf("Slot %s:", dQuote(s), domain = "R-methods"), "\n", sep = "")
        show(slot(object, s))
    }
    .extraSlotsDone # a signal not to call this function (again)
}

## temporary definition of show, to become the default method
## when .InitShowMethods is called
show <- function(object) showDefault(object)

.InitShowMethods <- function(envir) {
    if(!isGeneric("show", envir))
        setGeneric("show", where = envir, simpleInheritanceOnly = TRUE)
    setMethod("show", "MethodDefinition",
              function(object) {
                  cl <- class(object)
		      if(.identC(cl, "MethodDefinition"))
                  	cat(gettext("Method Definition:", domain = "R-methods"), "\n\n", sep = "")
		      else cat(gettextf("Method Definition (Class %s):", dQuote(classLabel(cl)), domain = "R-methods"), "\n\n", sep = "")
                  show(object@.Data)
                  mm <- methodSignatureMatrix(object)
                  cat("\n", gettext("Signatures:", domain = "R-methods"), "\n", sep = "")
                  print(mm)
              },
              where = envir)
    setMethod("show", "MethodWithNext",
              function(object)  {
                  callNextMethod()
                  cat("\n", gettext("Excluded from nextMethod:", domain = "R-methods"), "\n", sep = "")
                  print(unlist(object@excluded))
              },
              where = envir)
    setMethod("show", "genericFunction",
              function(object)  {
                  nam <- as.character(object@generic)
		if(length(object@group) && length(object@valueClass))
                  cat(sprintf(gettext("Class %s for generic %s defined from package %s\n  belonging to group(s): %s\n  defined with value class: %s\n", domain = "R-methods"), dQuote(class(object)), dQuote(nam), sQuote(object@package), paste(unlist(object@group), collapse =", "), dQuote(object@valueClass)))
		else if(!length(object@group) && length(object@valueClass))
                  cat(gettextf("Class %s for generic %s defined from package %s\n  defined with value class: %s\n", dQuote(class(object)), dQuote(nam), sQuote(object@package), dQuote(object@valueClass), domain = "R-methods"))
		else if(length(object@group) && !length(object@valueClass))
                  cat(sprintf(gettext("Class %s for generic %s defined from package %s\n  belonging to group(s): %s\n", domain = "R-methods"), dQuote(class(object)), dQuote(nam), sQuote(object@package), paste(unlist(object@group), collapse =", ")))
		else
                  cat(gettextf("Class %s for generic %s defined from package %s", dQuote(class(object)), dQuote(nam), sQuote(object@package), domain = "R-methods"), "\n", sep = "")

                  cat("\n")
                  show(object@.Data)
                  ns <- asNamespace(object@package)
                  exported <- nam %in% names(.getNamespaceInfo(ns, "exports"))
                  showGen <- if(exported) dQuote(nam, NULL)
                             else paste(object@package, nam, sep=":::")
                  cat(sprintf(gettext("Methods may be defined for arguments: %s\nUse 'showMethods(\"%s\")' for currently available ones.", domain = "R-methods"), paste(object@signature, collapse=", "), showGen), "\n", sep = "")
                  if(.simpleInheritanceGeneric(object))
                      cat(gettext("(This generic function excludes non-simple inheritance; see '?setIs')"), "\n", sep = "")
              },
              where = envir)
    setMethod("show", "classRepresentation",
              function(object){
                  if(!.identC(class(object), "classRepresentation"))
                    cat(gettextf("Extended class definition (%s)", classLabel(class(object)), domain = "R-methods"), "\n", sep = "")
                  printClassRepresentation(object)
              },
              where = envir)

    ## a show() method for the signature class
    setMethod("show", "signature", function(object) {
        message(gettextf("An object of class %s", dQuote(class(object))), domain = "R-methods")
        val <- object@.Data
        names(val) <- object@names
        callNextMethod(val)
    } ,
              where = envir)
}

.showPackage <- function(className) {
    if(is.logical(opt <- getOption("showPackageForClass")))
        opt
    else
        is.list(.Call(C_R_getClassFromCache, as.character(className), .classTable))
}
## an informative string label for a class
classLabel <- function(Class) {
    if(is.character(Class) && length(Class)) {
        className <- Class[[1L]]
        packageName <- attr(Class, "package")
        if(is.null(packageName))
            packageName <- ""
    }
    else {
        if(is(Class, "classRepresentation")) {
            className <- Class@className
            packageName <- Class@package
        }
        else stop(gettextf("invalid call passed to 'classLabel': expected a name or a class definition, got an object of class %s", classLabel(class(Class))), domain = "R-methods")
    }
    if(.showPackage(className)) {
	    if(identical(packageName, ".GlobalEnv"))
		gettextf("Class %s (from the global environment)", dQuote(className))
	    else
		gettextf("Class %s (from package %s)", dQuote(className), sQuote(packageName))
   }
   else
       dQuote(className)
}
