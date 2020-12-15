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

/** @file ExpressionVector.cpp
 *
 * Implementation of class ExpressionVector and related functions.
 */

#include <CXXR/ExpressionVector.hpp>
#include <CXXR/ListVector.hpp>
#include <CXXR/Symbol.hpp>

using namespace CXXR;

namespace CXXR
{
    // Force the creation of non-inline embodiments of functions callable
    // from C:
    namespace ForceNonInline
    {
        // const auto &isExpressionptr = Rf_isExpression;
        // const auto &XVECTOR_ELTptr = XVECTOR_ELT;
    } // namespace ForceNonInline

    ExpressionVector::ExpressionVector(const ListVector &lv)
        : EdgeVector<RObject *, EXPRSXP>(lv.size())
    {
        // The following results in unnecessary invocations of
        // devolveAge() on the nodes pointed to.
        for (R_xlen_t i = 0; i < size(); ++i)
            (*this)[i] = lv[i];
        SEXP names = Rf_getAttrib(const_cast<ListVector *>(&lv), R_NamesSymbol);
        if (names)
            Rf_setAttrib(this, R_NamesSymbol, names);
    }
} // namespace CXXR

// ***** C interface *****
