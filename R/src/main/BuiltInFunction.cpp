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

namespace R
{
    // Force the creation of non-inline embodiments of functions callable
    // from C:
    namespace ForceNonInline
    {
        const auto &PRIMOFFSETptr = PRIMOFFSET;
        const auto &SET_PRIMOFFSETptr = SET_PRIMOFFSET;
    }

    /* Primitive Access Methods */
    /** @brief Get offset of a R::BuiltInFunction.
     *
     * @param x Pointer to a R::BuiltInFunction.
     *
     * @return The offset of this function within the function table.
     */
    int RObject::primoffset(RObject *x) { return x ? x->u.primsxp.m_offset : 0; }

    /** @brief Set the new offset for a R::BuiltInFunction.
     *
     * @param x Pointer to a R::BuiltInFunction (checked).
     *
     * @param v The new offset for this function within the function table.
     */
    void RObject::set_primoffset(RObject *x, int v)
    {
        if (!x)
            return;
        x->u.primsxp.m_offset = v;
    }
} // namespace R
