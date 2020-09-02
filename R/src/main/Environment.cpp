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

#include <Environment.hpp>

namespace R
{
    /* Environment Access Methods */
    RObject *RObject::frame(RObject *x) { return x ? x->u.envsxp.frame : nullptr; }

    RObject *RObject::enclos(RObject *x) { return x ? x->u.envsxp.enclos : nullptr; }

    RObject *RObject::hashtab(RObject *x) { return x ? x->u.envsxp.hashtab : nullptr; }

    unsigned int RObject::envflags(RObject *x) { return x ? x->m_gpbits : 0; } /* for environments */

    void RObject::set_envflags(RObject *x, unsigned int v)
    {
        if (!x)
            return;
        x->m_gpbits = v;
    }

    void RObject::set_frame(RObject *x, RObject *v)
    {
        if (!x)
            return;
        x->u.envsxp.frame = v;
    }

    void RObject::set_enclos(RObject *x, RObject *v)
    {
        if (!x)
            return;
        x->u.envsxp.enclos = v;
    }

    void RObject::set_hashtab(RObject *x, RObject *v)
    {
        if (!x)
            return;
        x->u.envsxp.hashtab = v;
    }

    unsigned int RObject::frame_is_locked(RObject *e) { return e ? (envflags(e) & FRAME_LOCK_MASK) : 0; }

    unsigned int RObject::is_global_frame(RObject *e) { return e ? (envflags(e) & GLOBAL_FRAME_MASK) : 0; }
} // namespace R