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

/** @file IntVector.cpp
 *
 * @brief Implementation of class IntVector and related functions.
 */

#include <CXXR/IntVector.hpp>
#include <Rinternals.h>

namespace CXXR
{
    // Force the creation of non-inline embodiments of functions callable
    // from C:
    namespace ForceNonInline
    {
        const auto &INTEGERptr = INTEGER;
        const auto &INTEGER_ROptr = INTEGER_RO;
    } // namespace ForceNonInline

    template <>
    const char *IntVector::staticTypeName()
    {
        return "integer";
    }
} // namespace CXXR

// ***** C interface *****

#ifdef STRICT_TYPECHECK
#define CHECK_VECTOR_INT(x)                                \
    do                                                     \
    {                                                      \
        if (!(TYPEOF(x) == INTSXP || TYPEOF(x) == LGLSXP)) \
            Rf_error(_("bad %s vector"), "INTSXP");        \
    } while (0)

#define CHECK_STDVEC_INT(x)                                  \
    do                                                       \
    {                                                        \
        CHECK_VECTOR_INT(x);                                 \
        if (ALTREP(x))                                       \
            Rf_error(_("bad standard %s vector"), "INTSXP"); \
    } while (0)

#define CHECK_SCALAR_INT(x)                         \
    do                                              \
    {                                               \
        CHECK_STDVEC_INT(x);                        \
        if (XLENGTH(x) != 1)                        \
            Rf_error(_("bad %s scalar"), "INTSXP"); \
    } while (0)

#define CHECK_VECTOR_INT_ELT(x, i)          \
    do                                      \
    {                                       \
        SEXP ce__x__ = (x);                 \
        R_xlen_t ce__i__ = (i);             \
        CHECK_VECTOR_INT(ce__x__);          \
        CHECK_BOUNDS_ELT(ce__x__, ce__i__); \
    } while (0)
#else
#define CHECK_STDVEC_INT(x) \
    do                      \
    {                       \
    } while (0)
#define CHECK_SCALAR_INT(x) \
    do                      \
    {                       \
    } while (0)
#define CHECK_VECTOR_INT(x) \
    do                      \
    {                       \
    } while (0)
#define CHECK_VECTOR_INT_ELT(x, i) \
    do                             \
    {                              \
    } while (0)
#endif

int *INTEGER(SEXP x)
{
    if (TYPEOF(x) != INTSXP && TYPEOF(x) != LGLSXP)
        Rf_error(_("'%s' function can only be applied to an integer, not a '%s'"), "INTEGER()",
                 Rf_type2char(TYPEOF(x)));
    CXXR::VectorBase::chkzln(x);
    return INTVECTOR_INTEGER(x);
}

const int *INTEGER_RO(SEXP x)
{
    if (TYPEOF(x) != INTSXP && TYPEOF(x) != LGLSXP)
        Rf_error(_("'%s' function can only be applied to an integer, not a '%s'"),
                 "INTEGER()", Rf_type2char(TYPEOF(x)));
    CXXR::VectorBase::chkzln(x);
    return INTVECTOR_INTEGER_RO(x);
}

int *INTEGER0(SEXP x)
{
    CHECK_STDVEC_INT(x);
    return static_cast<int *>(STDVEC_DATAPTR(x));
}

int SCALAR_IVAL(SEXP x)
{
    CHECK_SCALAR_INT(x);
    return INTEGER0(x)[0];
}

void SET_SCALAR_IVAL(SEXP x, int v)
{
    CHECK_SCALAR_INT(x);
    INTEGER0(x)[0] = v;
}

const int *INTEGER_OR_NULL(SEXP x)
{
    CHECK_VECTOR_INT(x);
    return static_cast<const int *>(ALTREP(x) ? ALTVEC_DATAPTR_OR_NULL(x) : STDVEC_DATAPTR(x));
}

int INTEGER_ELT(SEXP x, R_xlen_t i)
{
    CHECK_VECTOR_INT_ELT(x, i);
    return ALTREP(x) ? ALTINTEGER_ELT(x, i) : INTEGER0(x)[i];
}

void SET_INTEGER_ELT(SEXP x, R_xlen_t i, int v)
{
    CHECK_VECTOR_INT_ELT(x, i);
    if (ALTREP(x))
        ALTINTEGER_SET_ELT(x, i, v);
    else
        INTEGER0(x)[i] = v;
}

Rboolean Rf_isInteger(SEXP s)
{
    return Rboolean(s && TYPEOF(s) == INTSXP && !Rf_inherits(s, "factor"));
}

Rboolean Rf_isFactor(SEXP s)
{
    return Rboolean(s && TYPEOF(s) == INTSXP && Rf_inherits(s, "factor"));
}

int Rf_nlevels(SEXP f)
{
    if (!Rf_isFactor(f))
        return 0;
    return LENGTH(Rf_getAttrib(f, R_LevelsSymbol));
}

SEXP Rf_ScalarInteger(int x)
{
    SEXP ans = Rf_allocVector(INTSXP, 1);
    SET_SCALAR_IVAL(ans, x);
    return ans;
}