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

/** @file Environment.cpp
 *
 *
 * @brief Implementation of class rho:Environment and associated C
 * interface.
 */

#include <CXXR/Environment.hpp>
#include <Rinternals.h>

namespace R
{
    // Force the creation of non-inline embodiments of functions callable
    // from C:
    namespace ForceNonInline
    {
        const auto &ENCLOSptr = ENCLOS;
        const auto &isEnvironmentptr = Rf_isEnvironment;
        const auto &FRAMEptr = FRAME;
    } // namespace ForceNonInline

    /* Environment Access Methods */
    RObject *RObject::frame(RObject *x) { return x ? x->u.envsxp.m_frame : nullptr; }

    RObject *RObject::enclos(RObject *x) { return x ? x->u.envsxp.m_enclos : nullptr; }

    RObject *RObject::hashtab(RObject *x) { return x ? x->u.envsxp.m_hashtab : nullptr; }

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
        x->u.envsxp.m_frame = v;
    }

    void RObject::set_enclos(RObject *x, RObject *v)
    {
        if (!x)
            return;
        x->u.envsxp.m_enclos = v;
    }

    void RObject::set_hashtab(RObject *x, RObject *v)
    {
        if (!x)
            return;
        x->u.envsxp.m_hashtab = v;
    }

    unsigned int RObject::frame_is_locked(RObject *e) { return e ? (envflags(e) & FRAME_LOCK_MASK) : 0; }

    unsigned int RObject::is_global_frame(RObject *e) { return e ? (envflags(e) & GLOBAL_FRAME_MASK) : 0; }
} // namespace R
