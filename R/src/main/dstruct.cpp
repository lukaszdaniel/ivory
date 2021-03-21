/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 2001-2014  The R Core Team
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#define R_NO_REMAP

#include <CXXR/BuiltInFunction.hpp>
#include <CXXR/String.hpp>
#include <CXXR/Symbol.hpp>
#include <CXXR/PairList.hpp>
#include <CXXR/Environment.hpp>
#include <CXXR/Closure.hpp>
#include <Localization.h>
#include <Defn.h>

using namespace CXXR;

HIDDEN SEXP R::mkPRIMSXP(int offset, bool evaluate)
{
    SEXP result;
    SEXPTYPE type = evaluate ? BUILTINSXP : SPECIALSXP;
    static SEXP PrimCache = nullptr;
    static int FunTabSize = 0;

    if (PrimCache == nullptr)
    {
        /* compute the number of entires in R_FunTab */
        FunTabSize = CXXR::R_FunTab.size();

        /* allocate and protect the cache */
        PrimCache = Rf_allocVector(VECSXP, FunTabSize);
        R_PreserveObject(PrimCache);
    }

    if (offset < 0 || offset >= FunTabSize)
        Rf_error(_("offset is out of R_FunTab range"));

    result = VECTOR_ELT(PrimCache, offset);

    if (result == R_NilValue)
    {
        result = new BuiltInFunction(offset, evaluate);
        result->expose();
        SET_VECTOR_ELT(PrimCache, offset, result);
    }
    else if (TYPEOF(result) != type)
        Rf_error(_("requested primitive type is not consistent with cached value"));

    return result;
}

SEXP R::mkCLOSXP(SEXP formal_args, SEXP body, SEXP env)
{
    GCRoot<const PairList> formalsr(SEXP_downcast<const PairList *>(formal_args));
    GCRoot<const RObject> bodyr(body);
    GCRoot<Environment> rhor(SEXP_downcast<Environment *>(env));

    switch (TYPEOF(body))
    {
    case CLOSXP:
    case BUILTINSXP:
    case SPECIALSXP:
    case DOTSXP:
    case ANYSXP:
        Rf_error(_("invalid body argument for 'function'"));
        break;
    default:
        break;
    }

    Closure *c = new Closure(formalsr, bodyr, rhor);
    c->expose();

    return c;
}

HIDDEN SEXP R::mkSYMSXP(SEXP name, SEXP value)
{
    GCRoot<const CachedString> namert(SEXP_downcast<const CachedString *>(name));
    GCRoot<> valuert(value);
    Symbol *ans = new Symbol(namert, valuert);
    ans->expose();
    return ans;
}
