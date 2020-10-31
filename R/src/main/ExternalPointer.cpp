/*
 *  R : A Computer Language for Statistical Data Analysis
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

/** @file ExternalPointer.cpp
 *
 * @brief Class ExternalPointer and associated C interface.
 */

#include "CXXR/ExternalPointer.hpp"
#include <Rinternals.h>
#include <Localization.h>
// #include "CXXR/GCStackRoot.hpp"

using namespace std;
using namespace R;

namespace R
{
    // Force the creation of non-inline embodiments of functions callable
    // from C:
    namespace ForceNonInline
    {
        const auto &R_ExternalPtrAddrptr = R_ExternalPtrAddr;
        const auto &R_ExternalPtrTagptr = R_ExternalPtrTag;
        const auto &R_ExternalPtrProtectedptr = R_ExternalPtrProtected;
        const auto &R_ClearExternalPtrptr = R_ClearExternalPtr;
    } // namespace ForceNonInline

    const char *ExternalPointer::typeName() const
    {
        return ExternalPointer::staticTypeName();
    }

    /* External pointer access methods */
    RObject *ExternalPointer::extptr_prot(RObject *x) { return RObject::cdr(x); }

    RObject *ExternalPointer::extptr_tag(RObject *x) { return RObject::tag(x); }

    void ExternalPointer::set_extptr_tag(RObject *x, RObject *v) { RObject::set_tag(x, v); }

    void ExternalPointer::set_extptr_prot(RObject *x, RObject *v) { RObject::set_cdr(x, v); }

    RObject *ExternalPointer::extptr_ptr(RObject *e) { return e ? e->u.listsxp.m_carval : nullptr; }

    void ExternalPointer::set_extptr_ptr(RObject *x, RObject *v)
    {
        if (!x)
            return;
        x->u.listsxp.m_carval = v;
    }

} // namespace R
