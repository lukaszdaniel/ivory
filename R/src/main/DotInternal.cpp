/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999--2020  The R Core Team.
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
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
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

/** @file DotInternal.cpp
 *
 * @brief Table of functions invoked \e via <tt>.Internal()</tt>.
 */

#include <CXXR/DotInternal.hpp>
#include <CXXR/BuiltInFunction.hpp>
#include <CXXR/Expression.hpp>
#include <CXXR/Symbol.hpp>
#include <Internal.h>

using namespace std;
using namespace R;
using namespace CXXR;

namespace CXXR
{
    // Force the creation of non-inline embodiments of functions callable
    // from C:
    namespace ForceNonInline
    {
        const auto &INTERNALptr = INTERNAL;
        const auto &SET_INTERNALptr = R::SET_INTERNAL;
    } // namespace ForceNonInline
} // namespace CXXR

// ***** C interface *****

SEXP INTERNAL(SEXP x)
{
    if (!x)
        return nullptr;
    Symbol::checkST(x);
    const Symbol *sym = SEXP_downcast<const Symbol *>(x);
    return const_cast<BuiltInFunction *>(sym->internalFunction());
    // TODO: sometime in the future:
    // return BuiltInFunction::obtainInternal(sym);
}

void R::SET_INTERNAL(SEXP x, SEXP v)
{
    if (!x)
        return;
    Symbol::checkST(x);
    Symbol *sym = SEXP_downcast<Symbol *>(x);
    BuiltInFunction *fun = SEXP_downcast<BuiltInFunction *>(v);
    sym->setInternalFunction(fun);
}
