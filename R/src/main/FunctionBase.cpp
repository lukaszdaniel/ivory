/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2007   The R Development Core Team.
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

/** @file FunctionBase.cpp
 *
 * Class FunctionBase and associated C interface functions.
 */

#include <CXXR/FunctionBase.hpp>

#include <cstdarg>
#include <R_ext/Print.h>
#include <Rinternals.h>

using namespace CXXR;

namespace CXXR
{
    // Force the creation of non-inline embodiments of functions callable
    // from C:
    namespace ForceNonInline
    {
        const auto &RDEBUGptr = RDEBUG;
        const auto &RTRACEptr = RTRACE;
        const auto &SET_RDEBUGptr = SET_RDEBUG;
        const auto &SET_RTRACEptr = SET_RTRACE;
    } // namespace ForceNonInline

    bool FunctionBase::rdebug(RObject *x)
    {
        if (!x)
            return false;
        const FunctionBase *fb = SEXP_downcast<FunctionBase *>(x);
        return fb->debugging();
    }

    void FunctionBase::set_rdebug(RObject *x, bool v)
    {
        if (!x)
            return;
        FunctionBase *fb = SEXP_downcast<FunctionBase *>(x);
        fb->setDebugging(v);
    }

    bool FunctionBase::rtrace(RObject *x)
    {
        return x && x->m_trace;
    }

    void FunctionBase::set_rtrace(RObject *x, bool v)
    {
        if (!x)
            return;
        x->m_trace = v;
    }
} // namespace CXXR

// ***** C interface *****

int RTRACE(SEXP x)
{
    return CXXR::FunctionBase::rtrace(x);
}

void SET_RTRACE(SEXP x, int v)
{
    CXXR::FunctionBase::set_rtrace(x, v);
}

int RDEBUG(SEXP x)
{
    return CXXR::FunctionBase::rdebug(x);
}

void SET_RDEBUG(SEXP x, int v)
{
    CXXR::FunctionBase::set_rdebug(x, v);
}

Rboolean Rf_isPrimitive(SEXP s)
{
    return Rboolean(TYPEOF(s) == BUILTINSXP || TYPEOF(s) == SPECIALSXP);
}

Rboolean Rf_isFunction(SEXP s)
{
    return Rboolean(TYPEOF(s) == CLOSXP || Rf_isPrimitive(s));
}