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

/** @file IntVector.hpp
 * @brief Class CXXR::IntVector and associated C interface.
 */

#ifndef INTVECTOR_HPP
#define INTVECTOR_HPP

#include <CXXR/ElementTraits.hpp>
#include <CXXR/FixedVector.hpp>

namespace CXXR
{
    /** @brief Vector of integer values.
     */
    using IntVector = CXXR::FixedVector<int, INTSXP>;

    template <>
    struct VectorTypeFor<int>
    {
        typedef IntVector type;
    };

    inline int *INTVECTOR_INTEGER(RObject *x)
    {
        return static_cast<int *>(DATAPTR(x));
    }

    inline const int *INTVECTOR_INTEGER_RO(RObject *x)
    {
        return static_cast<const int *>(DATAPTR_RO(x));
    }

    // Template specializations of ElementTraits:
    namespace ElementTraits
    {
        template <>
        inline const int &NAFunc<int>::operator()() const
        {
            static int na = NA_INTEGER;
            return na;
        }

        template <>
        struct MustConstruct<int> : boost::mpl::false_
        {
        };

        template <>
        struct MustDestruct<int> : boost::mpl::false_
        {
        };
    } // namespace ElementTraits
} // namespace CXXR

namespace R
{
    int SCALAR_IVAL(SEXP x);
    void SET_SCALAR_IVAL(SEXP x, int v);
} // namespace R

extern "C"
{
    /**
     * @param x Pointer to an \c IntVector or a \c LogicalVector (i.e. an
     *          R integer or logical vector).
     *          An error is generated if \a x is not a non-null pointer to an \c
     *          IntVector or a \c LogicalVector.
     *
     * @note    Maybe this should exclude logicals, but it is widely used
     *
     * @return Pointer to element 0 of \a x.
     */
    int *INTEGER(SEXP x);

    /**
     * @param x Pointer to an \c IntVector or a \c LogicalVector (i.e. an
     *          R integer or logical vector).
     *          An error is generated if \a x is not a non-null pointer to an \c
     *          IntVector or a \c LogicalVector.
     * @return Pointer to constant element 0 of \a x.
     */
    const int *INTEGER_RO(SEXP x);
    int *INTEGER0(SEXP x);

    const int *INTEGER_OR_NULL(SEXP x);
    int INTEGER_ELT(SEXP x, R_xlen_t i);
    void SET_INTEGER_ELT(SEXP x, R_xlen_t i, int v);
    Rboolean Rf_isInteger(SEXP s);
    Rboolean Rf_isFactor(SEXP s);
    int Rf_nlevels(SEXP f);
    SEXP Rf_ScalarInteger(int x);
} // extern "C"

#if (defined(R_NO_REMAP) && defined(COMPILING_IVORY)) && defined(__cplusplus)
const auto isFactor = Rf_isFactor;
const auto isInteger = Rf_isInteger;
const auto nlevels = Rf_nlevels;
const auto ScalarInteger = Rf_ScalarInteger;
#endif

#endif // INTVECTOR_HPP
