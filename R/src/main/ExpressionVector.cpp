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

using namespace std;
using namespace CXXR;

namespace CXXR
{
    // Force the creation of non-inline embodiments of functions callable
    // from C:
    namespace ForceNonInline
    {
        const auto &isExpressionptr = Rf_isExpression;
        const auto &XVECTOR_ELTptr = XVECTOR_ELT;
    } // namespace ForceNonInline

    ExpressionVector::ExpressionVector(ListVector &lv)
        : HandleVector<RObject, EXPRSXP>(lv.size())
    {
        // The following results in unnecessary invocations of
        // propagateAge() on the nodes pointed to.
        for (R_xlen_t i = 0; i < size(); ++i)
            (*this)[i] = lv[i];
        SEXP names = Rf_getAttrib(const_cast<ListVector *>(&lv), R_NamesSymbol);
        if (names)
            Rf_setAttrib(this, R_NamesSymbol, names);
    }

    ExpressionVector *ExpressionVector::clone(bool deep) const
    {
        return new ExpressionVector(*this, deep);
    }
} // namespace CXXR

// ***** C interface *****

SEXP SET_XVECTOR_ELT(SEXP x, R_xlen_t i, SEXP v)
{
    if (TYPEOF(x) != EXPRSXP)
    {
        Rf_error(_("'%s' function can only be applied to a list, not a '%s'"), "SET_XVECTOR_ELT()",
                 Rf_type2char(TYPEOF(x)));
    }
    if (i < 0 || i >= XLENGTH(x))
        Rf_error(_("attempt to set index %ld/%ld in 'SET_XVECTOR_ELT()' function"), (long long)i, (long long)XLENGTH(x));
    if (x)
        x->xfix_refcnt(XVECTOR_ELT(x, i), v);
    // EXPRVECTOR_ELT(x, i) = v;
    ExpressionVector *ev = CXXR::SEXP_downcast<ExpressionVector *>(x, false);
    (*ev)[i] = v;
    return v;
}

SEXP XVECTOR_ELT(SEXP x, R_xlen_t i)
{
    /* We need to allow vector-like types here */
    if (TYPEOF(x) != EXPRSXP)
        Rf_error(_("'%s' function can only be applied to an expression, not a '%s'"), "XVECTOR_ELT()",
                 Rf_type2char(TYPEOF(x)));
    // return EXPRVECTOR_ELT(x, i);
    ExpressionVector *ev = CXXR::SEXP_downcast<CXXR::ExpressionVector *>(x, false);
    return (*ev)[i];
}
