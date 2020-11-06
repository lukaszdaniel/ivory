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

#include <Localization.h>
#include <Defn.h>


/**
 * @brief Create a R::BuiltInFunction object
 * 
 * @param offset offset for the R::BuiltInFunction
 * 
 * @param eval parameter to determine whether R::BuiltInFunction
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
        FunTabSize = R_FunTab.size();
        // while (R_FunTab[FunTabSize].name())
        //     FunTabSize++;

        /* allocate and protect the cache */
        PrimCache = allocVector(VECSXP, FunTabSize);
        R_PreserveObject(PrimCache);
    }

    if (offset < 0 || offset >= FunTabSize)
        error(_("offset is out of R_FunTab range"));

    result = VECTOR_ELT(PrimCache, offset);

    if (result == R_NilValue)
    {
        result = allocSExp(type);
        SET_PRIMOFFSET(result, offset);
        SET_VECTOR_ELT(PrimCache, offset, result);
    }
    else if (TYPEOF(result) != type)
        error(_("requested primitive type is not consistent with cached value"));

    return result;
}

/**
 * @brief Create a R::Closure object
 * 
 * @param formals formal arguments to be assigned to the R::Closure
 * 
 * @param body function body to be assigned to the R::Closure
 * 
 * @param rho environment to be assigned to the R::Closure
 * 
 * @return return a closure with formals f, body b, and environment rho
 * 
 * @note This is called by function() {}, where an invalid
 *       body should be impossible. When called from
 *       other places (eg do_asfunction) they
 *       should do this checking in advance.
 */

HIDDEN SEXP R::mkCLOSXP(SEXP formals, SEXP body, SEXP rho)
{
    SEXP c;
    PROTECT(formals);
    PROTECT(body);
    PROTECT(rho);
    c = allocSExp(CLOSXP);

#ifdef not_used_CheckFormals
    if (isList(formals))
        SET_FORMALS(c, formals);
    else
        error(_("invalid formal arguments for 'function'"));
#else
    SET_FORMALS(c, formals);
#endif
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
        SET_BODY(c, body);
        break;
    }

    if (rho == R_NilValue)
        SET_CLOENV(c, R_GlobalEnv);
    else
        SET_CLOENV(c, rho);
    UNPROTECT(3);
    return c;
}

static bool isDDName(SEXP name)
{
    const char *buf;
    char *endp;

    buf = CHAR(name);
    if (streqln(buf, "..", 2) && strlen(buf) > 2)
    {
        buf += 2;
        strtol(buf, &endp, 10); // discard value
        return (*endp == '\0');
    }
    return false;
}

/**
 * @brief Create a R::Symbol object
 * 
 * @param name name of the R::Symbol
 * 
 * @param value value to be assigned
 * 
 * @return symsxp with the string name inserted in the name field
 */
 
HIDDEN SEXP R::mkSYMSXP(SEXP name, SEXP value)
{
    PROTECT(name);
    PROTECT(value);
    bool i = isDDName(name);
    SEXP c = allocSExp(SYMSXP);
    SET_PRINTNAME(c, name);
    SET_SYMVALUE(c, value);
    SET_DDVAL(c, i);
    UNPROTECT(2);
    return c;
}
