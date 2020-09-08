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

#include <CXXR/Promise.hpp>

namespace R
{
    /* Promise Access Methods */
    RObject *RObject::prcode(RObject *x) { return x ? x->u.promsxp.expr : nullptr; }

    void RObject::set_prcode(RObject *x, RObject *v)
    {
        if (!x)
            return;
        x->u.promsxp.expr = v;
    }

    RObject *RObject::prenv(RObject *x) { return x ? x->u.promsxp.env : nullptr; }

    RObject *RObject::prvalue(RObject *x) { return x ? x->u.promsxp.value : nullptr; }

    void RObject::set_prvalue(RObject *x, RObject *v)
    {
        if (!x)
            return;
        x->u.promsxp.value = v;
    }

    unsigned int RObject::prseen(RObject *x) { return x ? x->m_gpbits : 0; }

    void RObject::set_prenv(RObject *x, RObject *v)
    {
        if (!x)
            return;
        x->u.promsxp.env = v;
    }

    void RObject::set_prseen(RObject *x, unsigned int v)
    {
        if (!x)
            return;
        x->m_gpbits = v;
    }
} // namespace R