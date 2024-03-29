/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2005-12   The R Core Team.
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <CXXR/VectorBase.hpp>
#include <CXXR/BuiltInFunction.hpp>
#include <Defn.h>
#include <Rinternals.h>
#include "localization.h"

SEXP R_get_primname(SEXP object)
{
    if (TYPEOF(object) != BUILTINSXP && TYPEOF(object) != SPECIALSXP)
        Rf_error(_("'R_get_primname' called on a non-primitive"));
    return Rf_mkString(PRIMNAME(object));
}

/* The main entry point is in Rinternals.h, but we want to register
   the call in *this* package */
SEXP new_object(SEXP class_def)
{
    return R_do_new_object(class_def);
}
