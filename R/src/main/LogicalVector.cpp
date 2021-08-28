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

/** @file LogicalVector.cpp
 *
 * @brief Implementation of class LogicalVector and related functions.
 */

#include <CXXR/Logical.hpp>
#include <CXXR/LogicalVector.hpp>
#include <Rinternals.h>

using namespace R;
using namespace CXXR;

namespace CXXR
{
    // Force the creation of non-inline embodiments of functions callable
    // from C:
    namespace ForceNonInline
    {
        const auto &isLogicalptr = Rf_isLogical;
        const auto &LOGICALptr = LOGICAL;
        const auto &LOGICAL_ROptr = LOGICAL_RO;
    } // namespace ForceNonInline

    template <>
    const char *LogicalVector::staticTypeName()
    {
        return "logical";
    }
} // namespace CXXR

// ***** C interface *****

#ifdef STRICT_TYPECHECK
#define CHECK_VECTOR_LGL(x)                         \
    do                                              \
    {                                               \
        if (TYPEOF(x) != LGLSXP)                    \
            Rf_error(_("bad %s vector"), "LGLSXP"); \
    } while (0)

#define CHECK_STDVEC_LGL(x)                                  \
    do                                                       \
    {                                                        \
        CHECK_VECTOR_LGL(x);                                 \
        if (ALTREP(x))                                       \
            Rf_error(_("bad standard %s vector"), "LGLSXP"); \
    } while (0)

#define CHECK_SCALAR_LGL(x)                         \
    do                                              \
    {                                               \
        CHECK_STDVEC_LGL(x);                        \
        if (XLENGTH(x) != 1)                        \
            Rf_error(_("bad %s scalar"), "LGLSXP"); \
    } while (0)

#define CHECK_VECTOR_LGL_ELT(x, i)          \
    do                                      \
    {                                       \
        SEXP ce__x__ = (x);                 \
        R_xlen_t ce__i__ = (i);             \
        CHECK_VECTOR_LGL(ce__x__);          \
        CHECK_BOUNDS_ELT(ce__x__, ce__i__); \
    } while (0)
#else
#define CHECK_VECTOR_LGL(x) \
    do                      \
    {                       \
    } while (0)
#define CHECK_STDVEC_LGL(x) \
    do                      \
    {                       \
    } while (0)
#define CHECK_SCALAR_LGL(x) \
    do                      \
    {                       \
    } while (0)
#define CHECK_VECTOR_LGL_ELT(x, i) \
    do                             \
    {                              \
    } while (0)
#endif

Rboolean Rf_isLogical(SEXP s)
{
    return Rboolean(s && TYPEOF(s) == LGLSXP);
}

int *LOGICAL(SEXP x)
{
    if (TYPEOF(x) != LGLSXP)
        Rf_error(_("'%s' function can only be applied to a logical, not a '%s'"), "LOGICAL()",
                 Rf_type2char(TYPEOF(x)));
    CXXR::VectorBase::chkzln(x);
    return LOGICALVECTOR_LOGICAL(x);
}

const int *LOGICAL_RO(SEXP x)
{
    if (TYPEOF(x) != LGLSXP)
        Rf_error(_("'%s' function can only be applied to a logical, not a '%s'"),
                 "LOGICAL()", Rf_type2char(TYPEOF(x)));
    CXXR::VectorBase::chkzln(x);
    return LOGICALVECTOR_LOGICAL_RO(x);
}

int *LOGICAL0(SEXP x)
{
    CHECK_STDVEC_LGL(x);
    return static_cast<int *>(STDVEC_DATAPTR(x));
}

int R::SCALAR_LVAL(SEXP x)
{
    CHECK_SCALAR_LGL(x);
    return LOGICAL0(x)[0];
}

void R::SET_SCALAR_LVAL(SEXP x, int v)
{
    CHECK_SCALAR_LGL(x);
    LOGICAL0(x)[0] = v;
}

const int *LOGICAL_OR_NULL(SEXP x)
{
    CHECK_VECTOR_LGL(x);
    return static_cast<const int *>(ALTREP(x) ? ALTVEC_DATAPTR_OR_NULL(x) : STDVEC_DATAPTR(x));
}

int LOGICAL_ELT(SEXP x, R_xlen_t i)
{
    CHECK_VECTOR_LGL_ELT(x, i);
    return ALTREP(x) ? ALTLOGICAL_ELT(x, i) : LOGICAL0(x)[i];
}

void SET_LOGICAL_ELT(SEXP x, R_xlen_t i, int v)
{
    CHECK_VECTOR_LGL_ELT(x, i);
    if (ALTREP(x))
        ALTLOGICAL_SET_ELT(x, i, v);
    else
        LOGICAL0(x)[i] = v;
}

SEXP Rf_ScalarLogical(int x)
{
    if (x == NA_LOGICAL)
        return R_LogicalNAValue;
    else if (x != 0)
        return R_TrueValue;
    else
        return R_FalseValue;
}