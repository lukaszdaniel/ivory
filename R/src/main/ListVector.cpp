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

/** @file ListVector.cpp
 *
 * Implementation of class ListVector and related functions.
 *
 * @todo Tidy up handling of names attribute, in particular to get rid
 * of <tt>const_cast</tt>.
 */

#include <CXXR/ListVector.hpp>
#include <CXXR/ExpressionVector.hpp>
#include <CXXR/Symbol.hpp>

using namespace std;
using namespace CXXR;

namespace CXXR
{
    // Force the creation of non-inline embodiments of functions callable
    // from C:
    namespace ForceNonInline
    {
        const auto &SET_VECTOR_ELTptr = SET_VECTOR_ELT;
        const auto &VECTOR_ELTptr = VECTOR_ELT;
    } // namespace ForceNonInline

    ListVector::ListVector(ExpressionVector &ev)
        : RObjectVector<RObject, VECSXP>(ev.size())
    {
        // The following results in unnecessary invocations of
        // propagateAge() on the nodes pointed to.
        for (R_xlen_t i = 0; i < size(); ++i)
            (*this)[i] = ev[i];
        SEXP names = Rf_getAttrib(const_cast<ExpressionVector *>(&ev), R_NamesSymbol);
        if (names)
            Rf_setAttrib(this, R_NamesSymbol, names);
    }

    ListVector *ListVector::clone(bool deep) const
    {
        return new ListVector(*this, deep);
    }
} // namespace CXXR

// ***** C interface *****

SEXP SET_VECTOR_ELT(SEXP x, R_xlen_t i, SEXP v)
{
    /*  we need to allow vector-like types here */
    if (TYPEOF(x) != VECSXP &&
        TYPEOF(x) != EXPRSXP &&
        TYPEOF(x) != WEAKREFSXP)
    {
        Rf_error(_("'%s' function can only be applied to a list, not a '%s'"), "SET_VECTOR_ELT()",
                 Rf_type2char(TYPEOF(x)));
    }
    if (TYPEOF(x) == EXPRSXP)
    {
        return SET_XVECTOR_ELT(x, i, v);
    }
    if (i < 0 || i >= XLENGTH(x))
        Rf_error(_("attempt to set index %ld/%ld in 'SET_VECTOR_ELT()' function"), (long long)i, (long long)XLENGTH(x));
    RObject::fix_refcnt(x, VECTOR_ELT(x, i), v);
    // LISTVECTOR_ELT(x, i) = v;
    ListVector *lv = SEXP_downcast<ListVector *>(x, false);
    (*lv)[i] = v;
    return v;
}

Rboolean Rf_isNewList(SEXP s)
{
    return Rboolean(s == R_NilValue || TYPEOF(s) == VECSXP);
}