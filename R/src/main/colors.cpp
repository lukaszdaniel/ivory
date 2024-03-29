/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2012-2014  The R Core Team
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

/* This should be regarded as part of the graphics engine:
   it is now a stub for code in grDevices */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#define R_NO_REMAP

#include <Localization.h>
#include <Defn.h>
#include <Rinternals.h>
#include <R_ext/GraphicsEngine.h>

/* same as src/library/grDevices/src/colors.cpp */
using F1 = unsigned int (*)(SEXP x, int i, unsigned int bg);
using F2 = const char* (*)(unsigned int col);
using F3 = unsigned int (*)(const char* s);
using F4 = void (*)(Rboolean save);

static F1 ptr_RGBpar3;
static F2 ptr_col2name;
static F3 ptr_R_GE_str2col;
static F4 ptr_savePalette;

void Rg_set_col_ptrs(F1 f1, F2 f2, F3 f3, F4 f4)
{
    ptr_RGBpar3 = f1;
    ptr_col2name = f2;
    ptr_R_GE_str2col = f3;
    ptr_savePalette = f4;
}

/* used in grid/src/gpar.cpp with bg = R_TRANWHITE,
   in packages Cairo, canvas and jpeg */
/* in GraphicsEngine.h */
unsigned int Rf_RGBpar3(SEXP x, int i, unsigned int bg)
{
    if (!ptr_RGBpar3) error(_("package 'grDevices' must be loaded"));
    return (ptr_RGBpar3)(x, i, bg);
}

/* in GraphicsEngine.h, used by devices */
unsigned int Rf_RGBpar(SEXP x, int i)
{
    return RGBpar3(x, i, R_TRANWHITE);
}

/* used in grid */
/* in GraphicsEngine.h */
const char *Rf_col2name(unsigned int col)
{
    if (!ptr_col2name)
        error(_("package 'grDevices' must be loaded"));
    return (ptr_col2name)(col);
}

/* used in grDevices for fg and bg of devices */
/* in GraphicsEngine.h */
unsigned int R_GE_str2col(const char *s)
{
    if (!ptr_R_GE_str2col)
        error(_("package 'grDevices' must be loaded"));
    return (ptr_R_GE_str2col)(s);
}

/* used in engine.cpp */
HIDDEN void savePalette(Rboolean save)
{
    if (!ptr_savePalette)
        error(_("package 'grDevices' must be loaded"));
    (ptr_savePalette)(save);
}
