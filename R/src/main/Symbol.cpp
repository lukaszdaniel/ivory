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

/** @file Symbol.cpp
 *
 * @brief Implementation of class Symbol and associated C
 * interface.
 */

#include <CXXR/Symbol.hpp>
#include <Rinternals.h>

namespace R
{
    // Force the creation of non-inline embodiments of functions callable
    // from C:
    namespace ForceNonInline
    {
        const auto &DDVALptr = DDVAL;
        const auto &installptr = Rf_install;
        const auto &isSymbolptr = Rf_isSymbol;
        const auto &PRINTNAMEptr = PRINTNAME;
        const auto &SYMVALUEptr = SYMVALUE;
        const auto &INTERNALptr = INTERNAL;
        const auto &SET_PRINTNAMEptr = SET_PRINTNAME;
        const auto &SET_SYMVALUEptr = SET_SYMVALUE;
        const auto &SET_INTERNALptr = SET_INTERNAL;
        const auto &SET_DDVALptr = SET_DDVAL;
    } // namespace ForceNonInline

    /* Symbol Access Methods */
    RObject *RObject::printname(RObject *x) { return x ? x->u.symsxp.m_pname : nullptr; }

    RObject *RObject::symvalue(RObject *x) { return x ? x->u.symsxp.m_value : nullptr; }

    RObject *RObject::internal(RObject *x) { return x ? x->u.symsxp.m_internal : nullptr; }

    unsigned int RObject::ddval(RObject *x) { return x ? (x->m_gpbits & DDVAL_MASK) : 0; } /* for ..1, ..2 etc */

    void RObject::set_ddval_bit(RObject *x)
    {
        if (!x)
            return;
        x->m_gpbits |= DDVAL_MASK;
    }

    void RObject::unset_ddval_bit(RObject *x)
    {
        if (!x)
            return;
        x->m_gpbits &= ~DDVAL_MASK;
    }

    void RObject::set_ddval(RObject *x, bool v)
    {
        if (v)
        {
            set_ddval_bit(x);
        }
        else
        {
            unset_ddval_bit(x);
        }
    } /* for ..1, ..2 etc */

    void RObject::set_printname(RObject *x, RObject *v)
    {
        if (!x)
            return;
        x->u.symsxp.m_pname = v;
    }

    void RObject::set_symvalue(RObject *x, RObject *v)
    {
        if (!x)
            return;
        x->u.symsxp.m_value = v;
    }

    void RObject::set_internal(RObject *x, RObject *v)
    {
        if (!x)
            return;
        x->u.symsxp.m_internal = v;
    }
} // namespace R
