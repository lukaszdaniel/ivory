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

/** @file ComplexVector.cpp
 *
 * @brief Implementation of class ComplexVector and related functions.
 */

#include <CXXR/ComplexVector.hpp>
#include <Rinternals.h>

namespace CXXR
{
    // Force the creation of non-inline embodiments of functions callable
    // from C:
    namespace ForceNonInline
    {
        const auto &isComplexptr = Rf_isComplex;
        const auto &COMPLEXptr = COMPLEX;
        const auto &COMPLEX_ROptr = COMPLEX_RO;
    } // namespace ForceNonInline

    template <>
    const char *ComplexVector::staticTypeName()
    {
        return "complex";
    }
} // namespace CXXR

// ***** C interface *****

#ifdef STRICT_TYPECHECK
#define CHECK_VECTOR_CPLX(x)                         \
    do                                               \
    {                                                \
        if (TYPEOF(x) != CPLXSXP)                    \
            Rf_error(_("bad %s vector"), "CPLXSXP"); \
    } while (0)

#define CHECK_STDVEC_CPLX(x)                                  \
    do                                                        \
    {                                                         \
        CHECK_VECTOR_CPLX(x);                                 \
        if (ALTREP(x))                                        \
            Rf_error(_("bad standard %s vector"), "CPLXSXP"); \
    } while (0)

#define CHECK_SCALAR_CPLX(x)                         \
    do                                               \
    {                                                \
        CHECK_STDVEC_CPLX(x);                        \
        if (XLENGTH(x) != 1)                         \
            Rf_error(_("bad %s scalar"), "CPLXSXP"); \
    } while (0)

#define CHECK_VECTOR_CPLX_ELT(x, i)         \
    do                                      \
    {                                       \
        SEXP ce__x__ = (x);                 \
        R_xlen_t ce__i__ = (i);             \
        CHECK_VECTOR_CPLX(ce__x__);         \
        CHECK_BOUNDS_ELT(ce__x__, ce__i__); \
    } while (0)
#else
#define CHECK_STDVEC_CPLX(x) \
    do                       \
    {                        \
    } while (0)
#define CHECK_SCALAR_CPLX(x) \
    do                       \
    {                        \
    } while (0)
#define CHECK_VECTOR_CPLX(x) \
    do                       \
    {                        \
    } while (0)
#define CHECK_VECTOR_CPLX_ELT(x, i) \
    do                              \
    {                               \
    } while (0)
#endif

Rcomplex *COMPLEX(SEXP x)
{
    if (TYPEOF(x) != CPLXSXP)
        Rf_error(_("'%s' function can only be applied to a complex, not a '%s'"), "COMPLEX()",
                 Rf_type2char(TYPEOF(x)));
    CXXR::VectorBase::chkzln(x);
    return COMPLEXVECTOR_COMPLEX(x);
}

const Rcomplex *COMPLEX_RO(SEXP x)
{
    if (TYPEOF(x) != CPLXSXP)
        Rf_error(_("'%s' function can only be applied to a complex, not a '%s'"),
                 "COMPLEX()", "complex", Rf_type2char(TYPEOF(x)));
    CXXR::VectorBase::chkzln(x);
    return COMPLEXVECTOR_COMPLEX_RO(x);
}

Rcomplex *COMPLEX0(SEXP x)
{
    CHECK_STDVEC_CPLX(x);
    return static_cast<Rcomplex *>(STDVEC_DATAPTR(x));
}

Rcomplex SCALAR_CVAL(SEXP x)
{
    CHECK_SCALAR_CPLX(x);
    return COMPLEX0(x)[0];
}

void SET_SCALAR_CVAL(SEXP x, Rcomplex v)
{
    CHECK_SCALAR_CPLX(x);
    COMPLEX0(x)[0] = v;
}

const Rcomplex *COMPLEX_OR_NULL(SEXP x)
{
    CHECK_VECTOR_CPLX(x);
    return static_cast<const Rcomplex *>(ALTREP(x) ? ALTVEC_DATAPTR_OR_NULL(x) : STDVEC_DATAPTR(x));
}

Rcomplex COMPLEX_ELT(SEXP x, R_xlen_t i)
{
    CHECK_VECTOR_CPLX_ELT(x, i);
    return ALTREP(x) ? ALTCOMPLEX_ELT(x, i) : COMPLEX0(x)[i];
}

void SET_COMPLEX_ELT(SEXP x, R_xlen_t i, Rcomplex v)
{
    CHECK_VECTOR_CPLX_ELT(x, i);
    if (ALTREP(x))
        ALTCOMPLEX_SET_ELT(x, i, v);
    else
        COMPLEX0(x)[i] = v;
}

SEXP Rf_ScalarComplex(Rcomplex x)
{
    SEXP ans = Rf_allocVector(CPLXSXP, 1);
    SET_SCALAR_CVAL(ans, x);
    return ans;
}