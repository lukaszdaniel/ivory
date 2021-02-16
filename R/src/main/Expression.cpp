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

/** @file Expression.cpp
 *
 * @brief Class Expression and associated C interface.
 */

#include <CXXR/Expression.hpp>

#include <iostream>

#include <R_ext/Error.h>
#include <Localization.h>
#include <CXXR/BuiltInFunction.hpp>
#include <CXXR/Closure.hpp>
#include <CXXR/Environment.hpp>
#include <CXXR/FunctionBase.hpp>
#include <CXXR/RAllocStack.hpp>
#include <CXXR/Symbol.hpp>
#include <Rinternals.h>

#undef match

using namespace std;
using namespace CXXR;

namespace CXXR
{
    // Force the creation of non-inline embodiments of functions callable
    // from C:
    namespace ForceNonInline
    {
        const auto &lconsptr = Rf_lcons;
    }

    SEXP R_CurrentExpr = nullptr;
    SEXP R_ReturnedValue;
    bool R_Visible;

    Expression *Expression::clone(bool deep) const
    {
        return new Expression(*this, deep);
    }

    const char *Expression::typeName() const
    {
        return staticTypeName();
    }
} // namespace CXXR

// ***** C interface *****

SEXP Rf_currentExpression();

SEXP Rf_lcons(SEXP cr, SEXP tl);

void Rf_setCurrentExpression(SEXP e);

/* Language based list constructs.  These are identical to the list */
/* constructs, but the results can be evaluated. */

/**
 * @brief Return a (language) dotted pair with the given car and cdr
 */

SEXP Rf_lcons(SEXP cr, SEXP tl);

SEXP Rf_lang1(SEXP s)
{
    return LCONS(s, R_NilValue);
}

SEXP Rf_lang2(SEXP s, SEXP t)
{
    GCRoot<> ss(s);
    s = LCONS(s, Rf_list1(t));
    return s;
}

SEXP Rf_lang3(SEXP s, SEXP t, SEXP u)
{
    GCRoot<> ss(s);
    s = LCONS(s, Rf_list2(t, u));
    return s;
}

SEXP Rf_lang4(SEXP s, SEXP t, SEXP u, SEXP v)
{
    GCRoot<> ss(s);
    s = LCONS(s, Rf_list3(t, u, v));
    return s;
}

SEXP Rf_lang5(SEXP s, SEXP t, SEXP u, SEXP v, SEXP w)
{
    GCRoot<> ss(s);
    s = LCONS(s, Rf_list4(t, u, v, w));
    return s;
}

SEXP Rf_lang6(SEXP s, SEXP t, SEXP u, SEXP v, SEXP w, SEXP x)
{
    GCRoot<> ss(s);
    s = LCONS(s, Rf_list5(t, u, v, w, x));
    return s;
}

Rboolean Rf_isLanguage(SEXP s)
{
    return Rboolean(s == R_NilValue || TYPEOF(s) == LANGSXP);
}