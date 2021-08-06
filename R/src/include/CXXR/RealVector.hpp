/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2006   The R Development Core Team.
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
 *
 *  Rho is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the Rho website.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

/** @file RealVector.hpp
 * @brief Class CXXR::RealVector and associated C interface.
 */

#ifndef REALVECTOR_HPP
#define REALVECTOR_HPP

#include <cmath>
#include <CXXR/FixedVector.hpp>
#include <CXXR/SEXP_downcast.hpp>
#include <CXXR/VectorBase.hpp>
#include <R_ext/Arith.h>

namespace CXXR
{
    /** @brief Vector of real numbers.
     */
    using RealVector = CXXR::FixedVector<double, REALSXP>;

    inline double *REALVECTOR_REAL(RObject *x)
    {
        return static_cast<double *>(DATAPTR(x));
    }

    inline const double *REALVECTOR_REAL_RO(RObject *x)
    {
        return static_cast<const double *>(DATAPTR_RO(x));
    }
} // namespace CXXR

namespace R
{
    double SCALAR_DVAL(SEXP x);
    void SET_SCALAR_DVAL(SEXP x, double v);
} // namespace R

extern "C"
{
    /**
     * @param s Pointer to an RObject.
     *
     * @return TRUE iff the RObject pointed to by \a s is a real vector.
     */
    Rboolean Rf_isReal(SEXP s);

    /**
     * @param x Pointer to an \c RealVector (i.e. an R numeric vector).
     *          An error is generated if \a x is not a non-null pointer to an \c
     *          RealVector.
     *
     * @return Pointer to element 0 of \a x.
     */
    double *REAL(SEXP x);

    /**
     * @param x Pointer to an \c RealVector (i.e. an R numeric vector).
     *          An error is generated if \a x is not a non-null pointer to an \c
     *          RealVector.
     *
     * @return Pointer to constant element 0 of \a x.
     */
    const double *REAL_RO(SEXP x);
    double *REAL0(SEXP x);
    const double *REAL_OR_NULL(SEXP x);
    double REAL_ELT(SEXP x, R_xlen_t i);
    void SET_REAL_ELT(SEXP x, R_xlen_t i, double v);
    SEXP Rf_ScalarReal(double x);
} // extern "C"

#if (defined(R_NO_REMAP) && defined(COMPILING_IVORY)) && defined(__cplusplus)
const auto ScalarReal = Rf_ScalarReal;
#endif

#endif // REALVECTOR_HPP
