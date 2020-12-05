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

/** @file S4Object.cpp
 *
 * @brief Class S4Object and associated C interface.
 */

#include <CXXR/S4Object.hpp>

namespace CXXR
{
    // Force the creation of non-inline embodiments of functions callable
    // from C:
    namespace ForceNonInline
    {
        const auto &IS_S4_OBJECTptr = IS_S4_OBJECT;
        const auto &SET_S4_OBJECTptr = SET_S4_OBJECT;
        const auto &UNSET_S4_OBJECTptr = UNSET_S4_OBJECT;

    } // namespace ForceNonInline

    S4Object *S4Object::clone() const
    {
        return new S4Object(*this);
    }

    const char *S4Object::typeName() const
    {
        return S4Object::staticTypeName();
    }
} // namespace CXXR

// ***** C interface *****

SEXP Rf_allocS4Object()
{
    return new CXXR::S4Object();
}
