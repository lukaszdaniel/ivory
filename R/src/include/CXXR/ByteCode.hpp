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

/** @file ByteCode.hpp
 * @brief Class CXXR::ByteCode.
 */

#ifndef BYTECODE_HPP
#define BYTECODE_HPP

#include <CXXR/ConsCell.hpp>
#include <CXXR/PairList.hpp>

namespace CXXR
{
    /** @brief Stub for future ByteCode class. 
     */
    class ByteCode : public ConsCell
    {
    public:
        /**
         * @param cr Pointer to the 'car' of the element to be
         *           constructed.
         * @param tl Pointer to the 'tail' (LISP cdr) of the element
         *           to be constructed.
         * @param tg Pointer to the 'tag' of the element to be constructed.
         */
        explicit ByteCode(RObject *cr = nullptr, PairList *tl = nullptr, RObject *tg = nullptr)
            : ConsCell(BCODESXP, cr, tl, tg)
        {
        }

        /** @brief The name by which this type is known in R.
         *
         * @return the name by which this type is known in R.
         */
        static const char *staticTypeName()
        {
            return "bytecode";
        }

        // Virtual functions of RObject:
        RObject *evaluate(Environment *env) override;
        const char *typeName() const override;

    private:
        // Declared private to ensure that ByteCode objects are
        // allocated only using 'new':
        ~ByteCode() {}

        // Not implemented yet.  Declared to prevent
        // compiler-generated versions:
        ByteCode(const ByteCode &);
        ByteCode &operator=(const ByteCode &);
    };
} // namespace CXXR

extern "C"
{
    SEXP R_PromiseExpr(SEXP);
    SEXP R_ClosureExpr(SEXP);
    SEXP R_BytecodeExpr(SEXP e);
    void R_initialize_bcode(void);
    SEXP R_bcEncode(SEXP);
    SEXP R_bcDecode(SEXP);
    void R_registerBC(SEXP, SEXP);
    Rboolean R_checkConstants(Rboolean);
    Rboolean R_BCVersionOK(SEXP);
}
#endif /* BYTECODE_HPP */
