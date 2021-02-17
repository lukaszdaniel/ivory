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

/** @file RAltRep.hpp
 * @brief Class CXXR::AltRep.
 */

#ifndef RALTREP_HPP
#define RALTREP_HPP

#include <CXXR/PairList.hpp>

namespace CXXR
{
    /** @brief Singly linked list of pairs for Alternative Representation (ALTREP)
     */
    class AltRep : public PairList
    {
    public:
        /**
         * @param cr Pointer to the 'car' of the element to be
         *           constructed.
         * @param tl Pointer to the 'tail' (LISP cdr) of the element
         *           to be constructed.
         * @param tg Pointer to the 'tag' of the element to be constructed.
         */
        explicit AltRep(RObject *cr = nullptr, PairList *tl = nullptr, RObject *tg = nullptr)
            : PairList(cr, tl, tg)
        {
            setAltrep(true);
        }

        /** @brief The name by which this type is known in R.
         *
         * @return the name by which this type is known in R.
         */
        static const char *staticTypeName()
        {
            return "altrep";
        }

        static void set_wrapper_type(AltRep *x, SEXPTYPE v);
        static SEXPTYPE wrapper_type(AltRep *x);

        // Virtual function of RObject:
        const char *typeName() const override;

    private:
        SEXPTYPE m_wrapper_type;
        // Declared private to ensure that AltRep objects are
        // allocated only using 'new':
        ~AltRep() {}

        // Not implemented yet.  Declared to prevent
        // compiler-generated versions:
        AltRep(const AltRep &);
        AltRep &operator=(const AltRep &);
    };
} // namespace CXXR

extern "C"
{
    SEXP ALTREP_CLASS(SEXP x);
    SEXP R_altrep_data1(SEXP x);
    SEXP R_altrep_data2(SEXP x);
    void R_set_altrep_data1(SEXP x, SEXP v);
    void R_set_altrep_data2(SEXP x, SEXP v);
} // extern "C"

#endif /* RALTREP_HPP */
