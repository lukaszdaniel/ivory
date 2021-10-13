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

/** @file DottedArgs.hpp
 * @brief Class CXXR::DottedArgs.
 */

#ifndef DOTTEDARGS_HPP
#define DOTTEDARGS_HPP

#include <CXXR/ConsCell.hpp>
#include <CXXR/PairList.hpp>

namespace CXXR
{
    /** @brief List of Promise objects corresponding to an R ... argument
     * specification.
     *
     * At present the class makes no attempt to enforce the
     * requirement that it should contain Promise objects.
     */
    class DottedArgs : public ConsCell
    {
    public:
        /**
         * @param cr Pointer to the 'car' of the element to be
         *           constructed.
         *
         * @param tl Pointer to the 'tail' (LISP cdr) of the element
         *           to be constructed.
         *
         * @param tg Pointer to the 'tag' of the element to be constructed.
         */
        explicit DottedArgs(RObject *cr = nullptr, PairList *tl = nullptr, const RObject *tg = nullptr)
            : ConsCell(DOTSXP, cr, tl, tg)
        {
        }

        /** @brief Copy constructor.
         *
         * @param pattern DottedArgs to be copied.
         */
        DottedArgs(const DottedArgs &pattern, Duplicate deep)
            : ConsCell(pattern, deep)
        {
        }

        /** @brief The name by which this type is known in R.
         *
         * @return The name by which this type is known in R.
         */
        static const char *staticTypeName()
        {
            return "...";
        }

        // Virtual functions of RObject:
        DottedArgs *clone(Duplicate deep) const override;
        RObject *evaluate(Environment *env) override;
        const char *typeName() const override;

    private:
        // Declared private to ensure that DottedArgs objects are
        // allocated only using 'new':
        ~DottedArgs() {}

        // Not implemented yet.  Declared to prevent
        // compiler-generated versions:
        DottedArgs &operator=(const DottedArgs &);
    };
} // namespace CXXR

namespace R
{
} // namespace R

extern "C"
{
    /** @brief Is an object "dotted" expression?
     *
     * @param s Pointer to an CXXR::RObject.
     *
     * @return \c true if s is a null pointer or a dotted expression.
     */
    Rboolean Rf_isDottedArgs(SEXP s);
}

#endif /* DOTTEDARGS_HPP */
