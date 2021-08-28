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

/** @file RawVector.cpp
 *
 * @brief Implementation of class RawVector and related functions.
 */

#include <CXXR/RawVector.hpp>
#include <Rinternals.h>

using namespace R;
using namespace CXXR;

namespace CXXR
{
    // Force the creation of non-inline embodiments of functions callable
    // from C:
    namespace ForceNonInline
    {
        const auto &isRawptr = Rf_isRaw;
        const auto &RAWptr = RAW;
        const auto &RAW_ROptr = RAW_RO;
    } // namespace ForceNonInline

    template <>
    const char *RawVector::staticTypeName()
    {
        return "raw";
    }
} // namespace CXXR

// ***** C interface *****

#ifdef STRICT_TYPECHECK
#define CHECK_VECTOR_RAW(x)                         \
    do                                              \
    {                                               \
        if (TYPEOF(x) != RAWSXP)                    \
            Rf_error(_("bad %s vector"), "RAWSXP"); \
    } while (0)

#define CHECK_STDVEC_RAW(x)                                  \
    do                                                       \
    {                                                        \
        CHECK_VECTOR_RAW(x);                                 \
        if (ALTREP(x))                                       \
            Rf_error(_("bad standard %s vector"), "RAWSXP"); \
    } while (0)

#define CHECK_SCALAR_RAW(x)                         \
    do                                              \
    {                                               \
        CHECK_STDVEC_RAW(x);                        \
        if (XLENGTH(x) != 1)                        \
            Rf_error(_("bad %s scalar"), "RAWSXP"); \
    } while (0)

#define CHECK_VECTOR_RAW_ELT(x, i)          \
    do                                      \
    {                                       \
        SEXP ce__x__ = (x);                 \
        R_xlen_t ce__i__ = (i);             \
        CHECK_VECTOR_RAW(ce__x__);          \
        CHECK_BOUNDS_ELT(ce__x__, ce__i__); \
    } while (0)
#else
#define CHECK_STDVEC_RAW(x) \
    do                      \
    {                       \
    } while (0)
#define CHECK_SCALAR_RAW(x) \
    do                      \
    {                       \
    } while (0)
#define CHECK_VECTOR_RAW(x) \
    do                      \
    {                       \
    } while (0)
#define CHECK_VECTOR_RAW_ELT(x, i) \
    do                             \
    {                              \
    } while (0)
#endif

Rboolean Rf_isRaw(SEXP s)
{
    return Rboolean(s && TYPEOF(s) == RAWSXP);
}

Rbyte *RAW(SEXP x)
{
    if (TYPEOF(x) != RAWSXP)
        Rf_error(_("'%s' function can only be applied to a raw, not a '%s'"), "RAW()",
                 Rf_type2char(TYPEOF(x)));
    CXXR::VectorBase::chkzln(x);
    return RAWVECTOR_RAW(x);
}

const Rbyte *RAW_RO(SEXP x)
{
    if (TYPEOF(x) != RAWSXP)
        Rf_error(_("'%s' function can only be applied to a raw, not a '%s'"),
                 "RAW()", Rf_type2char(TYPEOF(x)));
    CXXR::VectorBase::chkzln(x);
    return RAWVECTOR_RAW_RO(x);
}

Rbyte *RAW0(SEXP x)
{
    CHECK_STDVEC_RAW(x);
    return static_cast<Rbyte *>(STDVEC_DATAPTR(x));
}
Rbyte R::SCALAR_BVAL(SEXP x)
{
    CHECK_SCALAR_RAW(x);
    return RAW0(x)[0];
}
void R::SET_SCALAR_BVAL(SEXP x, Rbyte v)
{
    CHECK_SCALAR_RAW(x);
    RAW0(x)[0] = v;
}

const Rbyte *RAW_OR_NULL(SEXP x)
{
    CHECK_VECTOR_RAW(x);
    return static_cast<const Rbyte *>(ALTREP(x) ? ALTVEC_DATAPTR_OR_NULL(x) : STDVEC_DATAPTR(x));
}

Rbyte RAW_ELT(SEXP x, R_xlen_t i)
{
    CHECK_VECTOR_RAW_ELT(x, i);
    return ALTREP(x) ? ALTRAW_ELT(x, i) : RAW0(x)[i];
}

void SET_RAW_ELT(SEXP x, R_xlen_t i, Rbyte v)
{
    CHECK_VECTOR_RAW_ELT(x, i);
    if (ALTREP(x))
        ALTRAW_SET_ELT(x, i, v);
    else
        RAW0(x)[i] = v;
}

SEXP Rf_ScalarRaw(Rbyte x)
{
    SEXP ans = Rf_allocVector(RAWSXP, 1);
    SET_SCALAR_BVAL(ans, x);
    return ans;
}