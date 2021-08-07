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

/** @file ListVector.hpp
 * @brief Class CXXR::ListVector and associated C interface.
 *
 * (ListVector implements VECSXP.)
 */

#ifndef LISTVECTOR_HPP
#define LISTVECTOR_HPP

#include <CXXR/HandleVector.hpp>
#include <CXXR/SEXP_downcast.hpp>

namespace CXXR
{
    class ExpressionVector;

    // Template specialization:
    template <>
    inline const char *HandleVector<RObject, VECSXP>::staticTypeName()
    {
        return "list";
    }

    /** @brief General vector of RObject::Handle<RObject>.
     */
    class ListVector : public HandleVector<RObject, VECSXP>
    {
    public:
        /** @brief Create a ListVector.
         *
         * Each element will initially encapsulate a null pointer.
         * @param sz Number of elements required.  Zero is
         *          permissible.
         */
        explicit ListVector(size_type sz)
            : HandleVector<RObject, VECSXP>(sz)
        {
        }

        /** @brief Copy constructor.
         *
         * Copy the ListVector, using the RObject::Handle copying semantics.
         *
         * @param pattern ListVector to be copied.
         *
         * @param deep Indicator whether to perform deep or shallow copy.
         */
        ListVector(const ListVector &pattern, Duplicate deep)
            : HandleVector<RObject, VECSXP>(pattern, deep)
        {
        }

        /** @brief Construct from ExpressionVector.
         *
         * @param ev The ExpressionVector on which the constructed
         *          ListVector is to be modelled.  The ListVector
         *          created will encapsulate exactly the same sequence of
         *          pointers to RObject as \a ev.
         *
         * @note The objects pointed to by \a pattern are never
         * themselves copied in creating the ListVector.  This is
         * rather at variance with the general semantics of
         * HandleVector, and perhaps ought to be changed.
         */
        explicit ListVector(ExpressionVector &ev);

        // Virtual function of RObject:
        ListVector *clone(Duplicate deep) const override;

    private:
        // Declared private to ensure that ListVectors are
        // allocated only using 'new'.
        ~ListVector() {}
    };

    inline RObject **LISTVECTOR_PTR(RObject *x)
    {
        return static_cast<RObject **>(DATAPTR(x));
    }

#define LISTVECTOR_ELT(x, i) (LISTVECTOR_PTR(x))[i]
} // namespace CXXR

extern "C"
{
    /** @brief Set element of CXXR::ListVector.
     *
     * @param x Pointer to a CXXR::ListVector.
     *
     * @param i Index of the required element.  There is no bounds checking.
     *
     * @param v Pointer, possibly null, to CXXR::RObject representing the
     *          new value.
     *
     * @return The new value \a v.
     */
    SEXP SET_VECTOR_ELT(SEXP x, R_xlen_t i, SEXP v);

    /** @brief Examine element of CXXR::ListVector.
     *
     * @param x Non-null pointer to a CXXR::ListVector.
     *
     * @param i Index of the required element.  There is no bounds checking.
     *
     * @return The value of the \a i 'th element.
     */
    SEXP VECTOR_ELT(SEXP x, R_xlen_t i);
    Rboolean Rf_isNewList(SEXP s);
} // extern "C"

#if (defined(R_NO_REMAP) && defined(COMPILING_IVORY)) && defined(__cplusplus)
const auto isNewList = Rf_isNewList;
#endif

#endif /* LISTVECTOR_HPP */
