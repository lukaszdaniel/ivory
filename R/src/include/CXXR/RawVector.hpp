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

/** @file RawVector.hpp
 * @brief Class CXXR::RawVector and associated C interface.
 */

#ifndef RAWVECTOR_HPP
#define RAWVECTOR_HPP

#include <CXXR/ElementTraits.hpp>
#include <CXXR/FixedVector.hpp>

namespace CXXR
{
    /** @brief Vector of 'raw bytes'.
     */
    using RawVector = CXXR::FixedVector<Rbyte, RAWSXP>;

    template <>
    struct VectorTypeFor<Rbyte>
    {
        typedef RawVector type;
    };

    inline Rbyte *RAWVECTOR_RAW(RObject *x)
    {
        return static_cast<Rbyte *>(DATAPTR(x));
    }

    inline const Rbyte *RAWVECTOR_RAW_RO(RObject *x)
    {
        return static_cast<const Rbyte *>(DATAPTR_RO(x));
    }

    // Template specializations of ElementTraits:
    namespace ElementTraits
    {
        template <>
        struct MustConstruct<Rbyte> : boost::mpl::false_
        {
        };

        template <>
        struct MustDestruct<Rbyte> : boost::mpl::false_
        {
        };

        template <>
        inline const Rbyte &NAFunc<Rbyte>::operator()() const
        {
            static Rbyte s_na = 0;
            return s_na;
        }

        template <>
        inline bool IsNA<Rbyte>::operator()(const Rbyte &) const
        {
            return false;
        }
    } // namespace ElementTraits
} // namespace CXXR

namespace R
{
    void SET_SCALAR_BVAL(SEXP x, Rbyte v);
    Rbyte SCALAR_BVAL(SEXP x);
} // namespace R

extern "C"
{
    /** @brief Is this a raw vector?
     *
     * @param s Pointer to CXXR::RObject.
     *
     * @return TRUE iff the RObject pointed to by \a s is a raw vector.
     */
    Rboolean Rf_isRaw(SEXP s);
    /**
     * @param x Pointer to a CXXR::RawVector (i.e. a RAWSXP).  An error is
     *          generated if \a x is not a non-null pointer to a
     *          CXXR::RawVector.
     *
     * @return Pointer to element 0 of \a x.
     */
    Rbyte *RAW(SEXP x);

    /**
     * @param x Pointer to a CXXR::RawVector (i.e. a RAWSXP).  An error is
     *          generated if \a x is not a non-null pointer to a
     *          CXXR::RawVector.
     *
     * @return Pointer to constant element 0 of \a x.
     */
    const Rbyte *RAW_RO(SEXP x);
    Rbyte *RAW0(SEXP x);

    const Rbyte *RAW_OR_NULL(SEXP x);
    Rbyte RAW_ELT(SEXP x, R_xlen_t i);
    void SET_RAW_ELT(SEXP x, R_xlen_t i, Rbyte v);
    SEXP Rf_ScalarRaw(Rbyte x);
} // extern "C"

#if (defined(R_NO_REMAP) && defined(COMPILING_IVORY)) && defined(__cplusplus)
const auto ScalarRaw = Rf_ScalarRaw;
#endif

#endif // RAWVECTOR_HPP
