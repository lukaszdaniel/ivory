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

#include <Symbol.hpp>

namespace R
{
    /* Symbol Access Methods */
    RObject *RObject::printname(SEXP x) { return x ? x->u.symsxp.pname : nullptr; }

    RObject *RObject::symvalue(SEXP x) { return x ? x->u.symsxp.value : nullptr; }

    RObject *RObject::internal(SEXP x) { return x ? x->u.symsxp.internal : nullptr; }

    unsigned int RObject::ddval(SEXP x) { return x ? (x->m_gpbits & DDVAL_MASK) : 0; } /* for ..1, ..2 etc */

    void RObject::set_ddval_bit(SEXP x)
    {
        if (!x)
            return;
        x->m_gpbits |= DDVAL_MASK;
    }

    void RObject::unset_ddval_bit(SEXP x)
    {
        if (!x)
            return;
        x->m_gpbits &= ~DDVAL_MASK;
    }

    void RObject::set_ddval(SEXP x, bool v)
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

    void RObject::set_printname(SEXP x, SEXP v)
    {
        if (!x)
            return;
        x->u.symsxp.pname = v;
    }

    void RObject::set_symvalue(SEXP x, SEXP v)
    {
        if (!x)
            return;
        x->u.symsxp.value = v;
    }

    void RObject::set_internal(SEXP x, SEXP v)
    {
        if (!x)
            return;
        x->u.symsxp.internal = v;
    }
} // namespace R
