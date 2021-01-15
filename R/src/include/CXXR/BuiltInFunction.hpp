/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999--2020  The R Core Team.
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 2007-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
 *
 *  This header file is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This file is part of R. R is distributed under the terms of the
 *  GNU General Public License, either Version 2, June 1991 or Version 3,
 *  June 2007. See doc/COPYRIGHTS for details of the copyright status of R.
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

/** @file BuiltInFunction.hpp
 * @brief Class CXXR::BuiltInFunction and associated C interface.
 */

#ifndef BUILTINFUNCTION_HPP
#define BUILTINFUNCTION_HPP

#include <CXXR/FunctionBase.hpp>

namespace CXXR
{
    /** @brief The type of the do_xxxx functions.
     *
     * These are the built-in R functions.
     */
    using CCODE = RObject *(*)(RObject *, RObject *, RObject *, RObject *);

    /** @brief R function implemented within the interpreter.
     *
     * A BuiltInFunction object represents an R function that is
     * implemented within the interpreter by a function in C++ or C.
     * These objects are of two kinds, according to whether the
     * arguments passed to BuiltInFunction::apply() are evaluated
     * before being passed on to the encapsulated C/C++ function (CR's
     * BUILTINSXP), or are passed on unevaluated (SPECIALSXP).
     */
    class BuiltInFunction : public FunctionBase
    {
    public:
        /**
         * @param offset The required table offset.  (Not
         * range-checked in any way.)
         *
         * @param evaluate true iff this is to be a BUILTINSXP;
         *          otherwise it will be a SPECIALSXP.
         */
        explicit BuiltInFunction(unsigned int offset, bool evaluate = true)
            : FunctionBase(evaluate ? BUILTINSXP : SPECIALSXP), m_offset(offset)
        {
        }

    private:
        int m_offset;
        // Declared private to ensure that BuiltInFunction objects are
        // allocated only using 'new'.
        ~BuiltInFunction();

    public:
        // Virtual function of RObject:
        const char *typeName() const override;

        /** @brief The names by which this type is known in R.
         *
         * @return The names by which this type is known in R.
         */
        static const char *staticTypeName()
        {
            return "(builtin or special)";
        }

        /** @brief Get table offset.
         *
         * @return The offset into the table of functions.
         */
        int offset() const { return m_offset; }
        /* Primitive Access Methods */
        static int primoffset(RObject *x);
        static void set_primoffset(RObject *x, int v);
    };

    /* Information for Deparsing Expressions */
    /** @brief Kind of function, used mainly in deparsing.
	 */
    enum Kind
    {
        PP_INVALID = 0,
        PP_ASSIGN = 1,
        PP_ASSIGN2 = 2,
        PP_BINARY = 3,
        PP_BINARY2 = 4,
        PP_BREAK = 5,
        PP_CURLY = 6,
        PP_FOR = 7,
        PP_FUNCALL = 8,
        PP_FUNCTION = 9,
        PP_IF = 10,
        PP_NEXT = 11,
        PP_PAREN = 12,
        PP_RETURN = 13,
        PP_SUBASS = 14,
        PP_SUBSET = 15,
        PP_WHILE = 16,
        PP_UNARY = 17,
        PP_DOLLAR = 18,
        PP_FOREIGN = 19,
        PP_REPEAT = 20
    };

    /** @brief Precedence level of function.
	 */
    enum Precedence
    {
        PREC_FN = 0,
        PREC_EQ = 1,
        PREC_LEFT = 2,
        PREC_RIGHT = 3,
        PREC_TILDE = 4,
        PREC_OR = 5,
        PREC_AND = 6,
        PREC_NOT = 7,
        PREC_COMPARE = 8,
        PREC_SUM = 9,
        PREC_PROD = 10,
        PREC_PERCENT = 11,
        PREC_COLON = 12,
        PREC_SIGN = 13,
        PREC_POWER = 14,
        PREC_SUBSET = 15,
        PREC_DOLLAR = 16,
        PREC_NS = 17
    };

    struct PPinfo
    {
        Kind kind;             /* deparse kind */
        Precedence precedence; /* operator precedence */
        bool rightassoc;       /* right associative? */
    };

    /* The type definitions for the table of built-in functions. */
    /* This table can be found in ../main/names.cpp */
    class FUNTAB
    {
    private:
        const char *m_name; /* print name */
        CCODE m_cfun;       /* c-code address */
        int m_code;         /* offset within c-code */
        int m_eval;         /* evaluate args? */
        int m_arity;        /* function arity */
        PPinfo m_gram;      /* pretty-print info */
    public:
        FUNTAB(const char *name, CCODE cfun, int code, int evalargs, int arity, PPinfo gram)
            : m_name(name), m_cfun(cfun), m_code(code), m_eval(evalargs), m_arity(arity), m_gram(gram)
        {
        }
        auto name() const { return m_name; }
        auto code() const { return m_code; }
        auto evalargs() const { return m_eval; }
        auto arity() const { return m_arity; }
        auto cfun() const { return m_cfun; }
        auto gram() const { return m_gram; }
    };

    /** @brief Get offset of a CXXR::BuiltInFunction.
     * 
     * @param x Pointer to a BuiltInFunction.
     * @return The offset of this function within the function table.
     */
    int PRIMOFFSET(RObject *x);

/* Defined and initialized in names.cpp (not main.cpp) :*/
#ifndef __R_Names__
    extern std::vector<CXXR::FUNTAB> R_FunTab; /* Built in functions */
#endif

    CCODE PRIMFUN(RObject *x);
    const char *PRIMNAME(RObject *x);
    int PRIMVAL(RObject *x);
    int PRIMARITY(RObject *x);
    PPinfo PPINFO(RObject *x);
    int PRIMPRINT(RObject *x);
    int PRIMINTERNAL(RObject *x);

} // namespace CXXR

/** @brief Create a CXXR::BuiltInFunction object.
 *
 * @param offset The required table offset.  (Not
 * range-checked in any way.)
 *
 * @param evaluate true iff this is to be a BUILTINSXP;
 *          otherwise it will be a SPECIALSXP.
 *
 * @return Pointer to the created CXXR::BuiltInFunction object.
 */
namespace R
{
    SEXP mkPRIMSXP(int offset, bool eval);
}

#endif /* BUILTINFUNCTION_HPP */
