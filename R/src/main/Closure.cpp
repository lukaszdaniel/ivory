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

/** @file Closure.cpp
 *
 * @brief Implementation of class Closure and associated C
 * interface.
 */

#include <CXXR/Closure.hpp>
#include <Rinternals.h>

namespace R
{
    // Force the creation of non-inline embodiments of functions callable
    // from C:
    namespace ForceNonInline
    {
        const auto &BODYptr = BODY;
        const auto &CLOENVptr = CLOENV;
        const auto &FORMALSptr = FORMALS;
        const auto &RDEBUGptr = RDEBUG;
        const auto &RSTEPptr = RSTEP;
        const auto &SET_CLOENVptr = SET_CLOENV;
        const auto &SET_RDEBUGptr = SET_RDEBUG;
        const auto &SET_RSTEPptr = SET_RSTEP;
    } // namespace ForceNonInline

    /* Closure Access Methods */
    RObject *RObject::formals(RObject *x) { return x ? x->u.closxp.m_formals : nullptr; }

    void RObject::set_formals(RObject *x, RObject *v)
    {
        if (!x)
            return;
        x->u.closxp.m_formals = v;
    }

    RObject *RObject::body(RObject *x) { return x ? x->u.closxp.m_body : nullptr; }

    void RObject::set_body(RObject *x, RObject *v)
    {
        if (!x)
            return;
        x->u.closxp.m_body = v;
    }

    RObject *RObject::cloenv(RObject *x) { return x ? x->u.closxp.m_env : nullptr; }

    void RObject::set_cloenv(RObject *x, RObject *v)
    {
        if (!x)
            return;
        x->u.closxp.m_env = v;
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
