/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999--2020  The R Core Team.
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *
 *  This header file is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This file is part of R. R is distributed under the terms of the
 *  GNU General Public License, either Version 2, June 1991 or Version 3,
 *  June 2007. See doc/COPYRIGHTS for details of the copyright status of R.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

/** @file BuiltInFunction.cpp
 *
 * Implementation of class BuiltInFunction and associated
 * C interface.
 */

#include <CXXR/BuiltInFunction.hpp>
#include <Defn.h>

namespace CXXR
{
    // Force the creation of non-inline embodiments of functions callable
    // from C:
    namespace ForceNonInline
    {
        const auto &PRIMOFFSETptr = PRIMOFFSET;
        const auto &SET_PRIMOFFSETptr = SET_PRIMOFFSET;
    } // namespace ForceNonInline

    BuiltInFunction::~BuiltInFunction()
    {
        assert(0 && "BuiltInFunction's destructor should never be called");
    }

    const char *BuiltInFunction::typeName() const
    {
        return sexptype() == SPECIALSXP ? "special" : "builtin";
    }

    /* Primitive Access Methods */
    /** @brief Get offset of a CXXR::BuiltInFunction.
     *
     * @param x Pointer to a CXXR::BuiltInFunction.
     *
     * @return The offset of this function within the function table.
     */
    int BuiltInFunction::primoffset(RObject *x) { return x ? x->u.primsxp.m_offset : 0; }

    /** @brief Set the new offset for a CXXR::BuiltInFunction.
     *
     * @param x Pointer to a CXXR::BuiltInFunction (checked).
     *
     * @param v The new offset for this function within the function table.
     */
    void BuiltInFunction::set_primoffset(RObject *x, int v)
    {
        if (!x)
            return;
        x->u.primsxp.m_offset = v;
    }

    CCODE PRIMFUN(RObject *x) { return R_FunTab[PRIMOFFSET(x)].cfun(); }
    const char *PRIMNAME(RObject *x) { return R_FunTab[PRIMOFFSET(x)].name(); }
    int PRIMVAL(RObject *x) { return R_FunTab[PRIMOFFSET(x)].code(); }
    int PRIMARITY(RObject *x) { return R_FunTab[PRIMOFFSET(x)].arity(); }
    PPinfo PPINFO(RObject *x) { return R_FunTab[PRIMOFFSET(x)].gram(); }
    int PRIMPRINT(RObject *x) { return ((R_FunTab[PRIMOFFSET(x)].evalargs()) / 100) % 10; }
    int PRIMINTERNAL(RObject *x) { return ((R_FunTab[PRIMOFFSET(x)].evalargs()) % 100) / 10; }
} // namespace CXXR
