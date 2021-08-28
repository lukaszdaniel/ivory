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

/** @file ComplexVector.hpp
 * @brief Class CXXR::ComplexVector and associated C interface.
 */

#ifndef COMPLEXVECTOR_HPP
#define COMPLEXVECTOR_HPP

#include <CXXR/FixedVector.hpp>
#include <CXXR/Complex.hpp>

namespace CXXR
{
    /** @brief Vector of complex numbers.
     */
    using ComplexVector = CXXR::FixedVector<Complex, CPLXSXP>;

    inline Rcomplex *COMPLEXVECTOR_COMPLEX(RObject *x)
    {
        return static_cast<Rcomplex *>(DATAPTR(x));
    }

    inline const Rcomplex *COMPLEXVECTOR_COMPLEX_RO(RObject *x)
    {
        return static_cast<const Rcomplex *>(DATAPTR_RO(x));
    }
} // namespace CXXR

namespace R
{
    void SET_SCALAR_CVAL(SEXP x, Rcomplex v);
    Rcomplex SCALAR_CVAL(SEXP x);
} // namespace R

extern "C"
{
    /** @brief Is this a complex vector?
     *
     * @param s Pointer to CXXR::RObject.
     *
     * @return TRUE iff the RObject pointed to by \a s is a complex vector.
     */
    Rboolean Rf_isComplex(SEXP s);

    /**
     * @param x Pointer to a CXXR::ComplexVector (i.e. an R complex vector).
     *          An error is generated if \a x is not a non-null pointer to a
     *          CXXR::ComplexVector.
     *
     * @return Pointer to element 0 of \a x.
     */
    Rcomplex *COMPLEX(SEXP x);

    /**
     * @param x Pointer to a CXXR::ComplexVector (i.e. an R complex vector).
     *          An error is generated if \a x is not a non-null pointer to a
     *          CXXR::ComplexVector.
     *
     * @return Pointer to constant element 0 of \a x.
     */
    const Rcomplex *COMPLEX_RO(SEXP x);
    Rcomplex *COMPLEX0(SEXP x);

    const Rcomplex *COMPLEX_OR_NULL(SEXP x);
    Rcomplex COMPLEX_ELT(SEXP x, R_xlen_t i);
    void SET_COMPLEX_ELT(SEXP x, R_xlen_t i, Rcomplex v);
    SEXP Rf_ScalarComplex(Rcomplex x);
} // extern "C"

#if (defined(R_NO_REMAP) && defined(COMPILING_IVORY)) && defined(__cplusplus)
const auto ScalarComplex = Rf_ScalarComplex;
#endif

#endif // COMPLEXVECTOR_HPP
