/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
 *
 *  Rho is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the Rho website.
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

/** @file FixedVector.cpp
 *
 * @brief Class FixedVector and associated C interface.
 */

#include <CXXR/FixedVector.hpp>

using namespace std;
using namespace CXXR;

namespace CXXR
{
    // Force the creation of non-inline embodiments of functions callable
    // from C:
    namespace ForceNonInline
    {
    } // namespace ForceNonInline
} // namespace CXXR

namespace R
{
} // namespace R

// ***** C interface *****

Rboolean Rf_isNumeric(SEXP s)
{
    switch (TYPEOF(s))
    {
    case INTSXP:
        if (Rf_inherits(s, "factor"))
            return FALSE;
    case LGLSXP:
    case REALSXP:
        return TRUE;
    default:
        return FALSE;
    }
}

Rboolean Rf_isNumber(SEXP s)
{
    switch (TYPEOF(s))
    {
    case INTSXP:
        if (Rf_inherits(s, "factor"))
            return FALSE;
    case LGLSXP:
    case REALSXP:
    case CPLXSXP:
        return TRUE;
    default:
        return FALSE;
    }
}
