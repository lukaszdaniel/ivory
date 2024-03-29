#  File src/library/tcltk/R/unix/zzz.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2021 The R Core Team
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

.TkUp <- FALSE

.onLoad <- if (!grepl("darwin", R.version$os)) {
    function(libname, pkgname)
    {
        ## Use local = FALSE to allow easy loading of Tcl extensions
        library.dynam("tcltk", pkgname, libname, local = FALSE)
        routines <- getDLLRegisteredRoutines("tcltk", addNames = FALSE)
        ns <- asNamespace(pkgname)
        for(i in c(1,4))                # .C and .External
            lapply(routines[[i]],
                   function(sym) assign(paste0(".C_", sym$name), sym, envir = ns))
        .TkUp <<- .C(.C_tcltk_init, 0L)[[1L]] == 1L
        addTclPath(system.file("exec", package = "tcltk"))
        invisible()
    }
} else {
    function(libname, pkgname)
    {
        ## This might be built against Aqua or X11 Tk

        ## If built against X11 Tk, check the Tcl/Tk libraries are
        ## installed, and check libX11 is present since this is a
        ## common cause of problems with CRAN binary installs reported
        ## for Rcmdr.
        if (file.exists("/usr/bin/otool")) {
            ## otool is part of the OS nowadays, but in recent versions
            ## is a stub requiring the CLT to be installed
            DSO <- file.path(libname, pkgname, "libs", .Platform$r_arch, "tcltk.so")
            out <- system2("/usr/bin/otool", c("-L", shQuote(DSO)), stdout = TRUE)
            ind <- grep("libtk[.0-9]+[.]dylib", out)
            if(length(ind)) {
                this <- sub(" .*", "", sub("^\t", "", out[ind]))
                if(!file.exists(this)) {
                    ## one issue here is that libtk built from unpatched
                    ## sources has wrong id, so we report what it is looking for
                    ## (/opt/R/arm64/lib:/usr/X11R6/lib/libtk8.6.dylib is wrong)
                    message("tcltk DLL is linked to ", shQuote(this))
                    stop("Tcl/Tk libraries are missing: install the Tcl/Tk component from the R installer")
                }
            }
            ind <- grep("libX11[.][0-9]+[.]dylib", out)
            if(length(ind)) {
                this <- sub(" .*", "", sub("^\t", "", out[ind]))
                if(!file.exists(this)) {
                    message("tcltk DLL is linked to ", shQuote(this))
                    stop("X11 library is missing: install XQuartz from www.xquartz.org")
                }
            }
        }

        library.dynam("tcltk", pkgname, libname, local = FALSE)
        routines <- getDLLRegisteredRoutines("tcltk", addNames = FALSE)
        ns <- asNamespace(pkgname)
        for(i in c(1,4))                # .C and .External
            lapply(routines[[i]],
                   function(sym) assign(paste0(".C_", sym$name), sym, envir = ns))
        .TkUp <<- .C(.C_tcltk_init, 0L)[[1L]] == 1L
        addTclPath(system.file("exec", package = "tcltk"))
        invisible()
    }
}

## This would no longer work since registration changed is.loaded
## .onUnload <- function(libpath) {
##     ## precaution in case the DLL has been unloaded without the namespace
##     if(is.loaded("delTcl", PACKAGE="tcltk")) {
##         .C("delTcl", PACKAGE="tcltk")
##         ## if we unload the DLL, get a segfault if we try to use tcltk again.
##         library.dynam.unload("tcltk", libpath)
##     }
## }
