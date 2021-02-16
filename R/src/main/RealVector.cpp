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

/** @file RealVector.cpp
 *
 * @brief Implementation of class RealVector and related functions.
 */

#include <CXXR/RealVector.hpp>
#include <Rinternals.h>

namespace CXXR
{
    // Force the creation of non-inline embodiments of functions callable
    // from C:
    namespace ForceNonInline
    {
        const auto &isRealptr = Rf_isReal;
        const auto &REALptr = REAL;
        const auto &REAL_ROptr = REAL_RO;
    } // namespace ForceNonInline

    template <>
    const char *RealVector::staticTypeName()
    {
        return "numeric";
    }
} // namespace CXXR

// ***** C interface *****

#ifdef STRICT_TYPECHECK
#define CHECK_VECTOR_REAL(x)                         \
    do                                               \
    {                                                \
        if (TYPEOF(x) != REALSXP)                    \
            Rf_error(_("bad %s vector"), "REALSXP"); \
    } while (0)

#define CHECK_STDVEC_REAL(x)                                  \
    do                                                        \
    {                                                         \
        CHECK_VECTOR_REAL(x);                                 \
        if (ALTREP(x))                                        \
            Rf_error(_("bad standard %s vector"), "REALSXP"); \
    } while (0)

#define CHECK_SCALAR_REAL(x)                         \
    do                                               \
    {                                                \
        CHECK_STDVEC_REAL(x);                        \
        if (XLENGTH(x) != 1)                         \
            Rf_error(_("bad %s scalar"), "REALSXP"); \
    } while (0)

#define CHECK_VECTOR_REAL_ELT(x, i)         \
    do                                      \
    {                                       \
        SEXP ce__x__ = (x);                 \
        R_xlen_t ce__i__ = (i);             \
        CHECK_VECTOR_REAL(ce__x__);         \
        CHECK_BOUNDS_ELT(ce__x__, ce__i__); \
    } while (0)
#else
#define CHECK_STDVEC_REAL(x) \
    do                       \
    {                        \
    } while (0)
#define CHECK_SCALAR_REAL(x) \
    do                       \
    {                        \
    } while (0)
#define CHECK_VECTOR_REAL(x) \
    do                       \
    {                        \
    } while (0)
#define CHECK_VECTOR_REAL_ELT(x, i) \
    do                              \
    {                               \
    } while (0)
#endif

double *REAL0(SEXP x)
{
    CHECK_STDVEC_REAL(x);
    return (double *)STDVEC_DATAPTR(x);
}

double SCALAR_DVAL(SEXP x)
{
    CHECK_SCALAR_REAL(x);
    return REAL0(x)[0];
}

void SET_SCALAR_DVAL(SEXP x, double v)
{
    CHECK_SCALAR_REAL(x);
    REAL0(x)[0] = v;
}

const double *REAL_OR_NULL(SEXP x)
{
    CHECK_VECTOR_REAL(x);
    return (double *)(ALTREP(x) ? ALTVEC_DATAPTR_OR_NULL(x) : STDVEC_DATAPTR(x));
}

double REAL_ELT(SEXP x, R_xlen_t i)
{
    CHECK_VECTOR_REAL_ELT(x, i);
    return ALTREP(x) ? ALTREAL_ELT(x, i) : REAL0(x)[i];
}

void SET_REAL_ELT(SEXP x, R_xlen_t i, double v)
{
    CHECK_VECTOR_REAL_ELT(x, i);
    if (ALTREP(x))
        ALTREAL_SET_ELT(x, i, v);
    else
        REAL0(x)[i] = v;
}

SEXP Rf_ScalarReal(double x)
{
    SEXP ans = Rf_allocVector(REALSXP, 1);
    SET_SCALAR_DVAL(ans, x);
    return ans;
}