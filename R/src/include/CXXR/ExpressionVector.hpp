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

/** @file ExpressionVector.hpp
 * @brief Class CXXR::ExpressionVector and associated C interface.
 *
 * (CXXR::ExpressionVector implements EXPRSXP.)
 *
 * @todo Constrain the elements to be Expression objects?  However, as
 * currently used, the elements of an ExpressionVector may be Symbols
 * rather than Expressions.
 */

#ifndef EXPRESSIONVECTOR_HPP
#define EXPRESSIONVECTOR_HPP

#include <CXXR/VectorBase.hpp>
#include <CXXR/HandleVector.hpp>
#include <CXXR/SEXP_downcast.hpp>

namespace CXXR
{
    class ListVector;

    template <>
    inline const char *HandleVector<RObject, EXPRSXP>::staticTypeName()
    {
        return "expression";
    }

    /** @brief Expression vector.
     *
     * The vector contains smart pointers of type
     * RObject::Handle<RObject>, where the intention is that these
     * pointers should point to language objects.
     *
     * @todo Replace the encapsulated pointer type RObject* with something
     * stricter (but is needs to embrace Symbol as well as Expression).
     */
    class ExpressionVector : public HandleVector<RObject, EXPRSXP>
    {
    public:
        /** @brief Create an ExpressionVector.
         *
         * @param sz Number of elements required.  Zero is permissible.
         */
        explicit ExpressionVector(R_xlen_t sz)
            : HandleVector<RObject, EXPRSXP>(sz)
        {
        }

        /** @brief Copy constructor.
         *
         * Copy the ExpressionVector, using the RObject::Handle
         * copying semantics.
         *
         * @param pattern ExpressionVector to be copied.
         *
         * @param deep Indicator whether to perform deep or shallow copy.
         */
        ExpressionVector(const ExpressionVector &pattern, bool deep)
            : HandleVector<RObject, EXPRSXP>(pattern, deep)
        {
        }

        /** @brief Create an ExpressionVector from a ListVector.
         *
         * @param lv The ListVector to be copied.  The
         *          ExpressionVector created will comprise exactly
         *          the same sequence of pointers to RObject as \a
         *          lv.
         *
         * @note The objects pointed to by \a lv are never themselves
         * copied in creating the ExpressionVector.  This is rather at
         * variance with the general semantics of HandleVector, and
         * perhaps ought to be changed.
         *
         * @note Q: Of all the possible coercions to ExpressionVector,
         * why have a constructor to implement this one?  A: Because
         * in all other cases, existing code in coerce.cpp needed at
         * most trivial modification.
         */
        explicit ExpressionVector(ListVector &lv);

        // Virtual function of RObject:
        ExpressionVector *clone(bool deep) const override;

    private:
        // Declare private to ensure that ExpressionVector objects are
        // allocated only using 'new':
        ~ExpressionVector() {}
    };

    /** @brief Expression vector.
     *
     * The vector contains smart pointers of type
     * RObject::Handle<RObject>, where the intention is that these
     * pointers should point to language objects.
     *
     * @todo Replace the encapsulated pointer type RObject* with something
     * stricter (but is needs to embrace Symbol as well as Expression).
     */
    // typedef FixedVector<GCEdge<>, EXPRSXP> ExpressionVector;

    inline RObject **EXPRVECTOR_PTR(RObject *x)
    {
        return static_cast<RObject **>(DATAPTR(x));
    }
#define EXPRVECTOR_ELT(x, i) (EXPRVECTOR_PTR(x))[i]
} // namespace CXXR

extern "C"
{
    /**
     * @param s Pointer to a CXXR::RObject.
     *
     * @return TRUE iff the CXXR::RObject pointed to by \a s is an expression.
     */
    Rboolean Rf_isExpression(SEXP s);

    /** @brief Set element of CXXR::ExpressionVector.
     *
     * @param x Pointer to a CXXR::ExpressionVector.
     *
     * @param i Index of the required element.  There is no bounds checking.
     *
     * @param v Pointer, possibly null, to CXXR::RObject representing the
     *          new value.
     *
     * @return The new value \a v.
     */
    SEXP SET_XVECTOR_ELT(SEXP x, R_xlen_t i, SEXP v);

    /** @brief Examine element of CXXR::ExpressionVector.
     *
     * @param x Non-null pointer to a CXXR::ExpressionVector.
     *
     * @param i Index of the required element.  There is no bounds checking.
     *
     * @return The value of the \a i 'th element.
     */
    SEXP XVECTOR_ELT(SEXP x, R_xlen_t i);
} // extern "C"

#endif /* EXPRESSIONVECTOR_HPP */
