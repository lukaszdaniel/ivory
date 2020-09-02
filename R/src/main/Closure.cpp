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

#include <Closure.hpp>

namespace R
{
    /* Closure Access Methods */
    RObject *RObject::formals(RObject *x) { return x ? x->u.closxp.formals : nullptr; }

    void RObject::set_formals(RObject *x, RObject *v)
    {
        if (!x)
            return;
        x->u.closxp.formals = v;
    }

    RObject *RObject::body(RObject *x) { return x ? x->u.closxp.body : nullptr; }

    void RObject::set_body(RObject *x, RObject *v)
    {
        if (!x)
            return;
        x->u.closxp.body = v;
    }

    RObject *RObject::cloenv(RObject *x) { return x ? x->u.closxp.env : nullptr; }

    void RObject::set_cloenv(RObject *x, RObject *v)
    {
        if (!x)
            return;
        x->u.closxp.env = v;
    }

    bool RObject::rdebug(RObject *x) { return x && x->m_debug; }

    void RObject::set_rdebug(RObject *x, bool v)
    {
        if (!x)
            return;
        x->m_debug = v;
    }

    bool RObject::rstep(RObject *x) { return x && x->m_spare; }

    void RObject::set_rstep(RObject *x, bool v)
    {
        if (!x)
            return;
        x->m_spare = v;
    }
} // namespace R