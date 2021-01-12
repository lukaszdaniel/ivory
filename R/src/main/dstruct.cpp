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
#include <Localization.h>
#include <Defn.h>

using namespace CXXR;

/**
 * @brief Create a CXXR::BuiltInFunction object
 * 
 * @param offset offset for the CXXR::BuiltInFunction
 * 
 * @param eval parameter to determine whether CXXR::BuiltInFunction
 *              is a "builtin" or "special"
 * 
 * @return builtin function, either "builtin" or "special"
 * 
 * @note The value produced is cached do avoid the need for GC protection
 *       in cases where a .Primitive is produced by unserializing or
 *       reconstructed after a package has clobbered the value assigned to
 *       a symbol in the base package.
 */

HIDDEN SEXP R::mkPRIMSXP(int offset, bool eval)
{
    SEXP result;
    SEXPTYPE type = eval ? BUILTINSXP : SPECIALSXP;
    static SEXP PrimCache = nullptr;
    static int FunTabSize = 0;

    if (PrimCache == nullptr)
    {
        /* compute the number of entires in R_FunTab */
        FunTabSize = CXXR::R_FunTab.size();

        /* allocate and protect the cache */
        PrimCache = allocVector(VECSXP, FunTabSize);
        R_PreserveObject(PrimCache);
    }

    if (offset < 0 || offset >= FunTabSize)
        error(_("offset is out of R_FunTab range"));

    result = VECTOR_ELT(PrimCache, offset);

    if (result == R_NilValue)
    {
        result = new BuiltInFunction(offset, eval);
        SET_VECTOR_ELT(PrimCache, offset, result);
    }
    else if (TYPEOF(result) != type)
        error(_("requested primitive type is not consistent with cached value"));

    return result;
}

/**
 * @brief Create a CXXR::Closure object
 * 
 * @param formals formal arguments to be assigned to the CXXR::Closure
 * 
 * @param body function body to be assigned to the CXXR::Closure
 * 
 * @param rho environment to be assigned to the CXXR::Closure
 * 
 * @return return a closure with formals f, body b, and environment rho
 * 
 * @note This is called by function() {}, where an invalid
 *       body should be impossible. When called from
 *       other places (eg do_asfunction) they
 *       should do this checking in advance.
 */

SEXP R::mkCLOSXP(SEXP formals, SEXP body, SEXP rho)
{
    SEXP c;
    PROTECT(formals);
    PROTECT(body);
    PROTECT(rho);
    c = new RObject(CLOSXP);

    switch (TYPEOF(body))
    {
    case CLOSXP:
    case BUILTINSXP:
    case SPECIALSXP:
    case DOTSXP:
    case ANYSXP:
        error(_("invalid body argument for 'function'"));
        break;
    default:
        break;
    }

    SET_FORMALS(c, formals);
    SET_BODY(c, body);
    SET_CLOENV(c, rho ? rho : R_GlobalEnv);

    UNPROTECT(3);
    return c;
}

/**
 * @brief Create a CXXR::Symbol object
 * 
 * @param name name of the CXXR::Symbol
 * 
 * @param value value to be assigned
 * 
 * @return symsxp with the string name inserted in the name field
 */

HIDDEN SEXP R::mkSYMSXP(SEXP name, SEXP value)
{
    GCRoot<const String> namert(SEXP_downcast<const String *>(name));
    GCRoot<> valuert(value);
    return new Symbol(namert, valuert);
}
