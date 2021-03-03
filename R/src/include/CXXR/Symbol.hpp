/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999--2020  The R Core Team.
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
 *
 *  Rho is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the Rho website.
 *
 *  This header file is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
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

/** @file Symbol.hpp
 * @brief Class CXXR::Symbol and associated C interface.
 */

#ifndef SYMBOL_HPP
#define SYMBOL_HPP

#include <CXXR/RObject.hpp>
#include <CXXR/BuiltInFunction.hpp>
#include <CXXR/String.hpp>
#include <CXXR/SEXP_downcast.hpp>
#include <R_ext/Boolean.h>

namespace R
{
    extern void InitNames();
}

extern "C"
{
    extern SEXP R_UnboundValue;
    /* Symbol Table Shortcuts */
#define PREDEFINED_SYMBOL(C_NAME, CXXR_NAME, R_NAME) \
    extern SEXP C_NAME;
#include <CXXR/PredefinedSymbols.hpp>
#undef PREDEFINED_SYMBOL
} // extern "C"

namespace CXXR
{
    /** @brief Class used to represent R symbols.
     *
     * A Symbol is an R identifier.  Each Symbol (except for
     * pseudo-symbols, see below) has a name, namely a String giving the
     * textual representation of the identifier.  Generally speaking,
     * however, a Symbol object is identified by its address rather
     * than by its name.  Consequently, the class enforces the
     * invariant that there is a most one Symbol object with a given
     * name (but this does not apply to pseudo-symbols).
     *
     * Symbols come in two varieties, standard symbols and pseudo-symbols,
     * both implemented by this class.  Dot-dot symbols are a
     * subvariety of standard symbols.
     *
     * Standard symbols are generated using the static member function
     * obtain(), and (as explained above) have the property that there
     * is at most one standard symbol with a given name.  This is
     * enforced by an internal table mapping names to standard
     * symbols.
     *
     * Dot-dot symbols have names of the form '<tt>..</tt><i>n</i>',
     * where <i>n</i> is a positive integer.  These are preferably
     * generated using the static member function obtainDotDotSymbol()
     * (though they can also be generated using obtain() ), and are
     * used internally by the interpreter to refer to elements of a
     * '<tt>...</tt>' argument list.  (Note that CR does not
     * consistently enforce the 'at most one Symbol per name' rule for
     * dot-dot symbols; CXXR does.)
     *
     * Pseudo-symbols are used to implement certain pseudo-objects
     * (::R_MissingArg and ::R_UnboundValue) that CR expects to have
     * ::SEXPTYPE SYMSXP.  Each psuedo-symbol has a blank string as
     * its name, but despite this each of them is a distinct symbol.
     *
     * @note Following the practice with CR's symbol table, Symbol
     * objects, once created, are permanently preserved against
     * garbage collection.  There is no inherent reason for this in
     * CXXR, but some packages may rely on it.  Consequently there is
     * no need to use smart pointers such as GCStackRoot<Symbol> or
     * GCEdge<Symbol>: plain pointers will do fine.
     */
    class Symbol : public RObject
    {
    public:
        /** @brief Missing argument.
         *
         * @return a pointer to the 'missing argument' pseudo-object,
         * which is identified as such by its address, not by its
         * content.
         */
        static Symbol *missingArgument()
        {
            return s_missing_arg;
        }

        /** @brief Restart token.
         *
         * @return a pointer to the 'restart token' pseudo-object,
         * which is identified as such by its address, not by its
         * content.
         */
        static Symbol *restartToken()
        {
            return s_restart_token;
        }

        static Symbol *inBCInterpreter()
        {
            return s_in_bc_interpreter;
        }

        static Symbol *currentExpression()
        {
            return s_current_expression;
        }

        /** @brief Unbound value.
         *
         * This is used as the 'value' of a Symbol that has not been
         * assigned any actual value.
         *
         * @return a pointer to the 'unbound value' pseudo-object,
         * which is identified as such by its address, not by its
         * content.
         */
        static Symbol *unboundValue()
        {
            return s_unbound_value;
        }

        /** @brief Index of a double-dot symbol.
         *
         * @return If this is a Symbol whose name is of the form
         * <tt>..<em>n</em></tt>, where <em>n</em> is a nonnegative integer,
         * returns <em>n</em>.  Otherwise returns <em>-1</em>.
         */
        unsigned int dotDotIndex() const { return m_dd_index; }

        /** @brief Is this a double-dot symbol?
         *
         * @return true iff this symbol relates to an element of a
         *         <tt>...</tt> argument list.
         */
        bool isDotDotSymbol() const { return m_dd_index != -1; }

        /**
         * @param the_name Pointer to String object representing the name
         *          of the symbol.  Names of the form
         *          <tt>..<em>n</em></tt>, where n is a (non-negative)
         *          decimal integer signify that the Symbol to be
         *          constructed relates to an element of a
         *          <tt>...</tt> argument list.
         *
         * @param val Value to be associated with the constructed
         *          Symbol object.  The default value is a placeholder
         *          signifying that no value has yet been associated
         *          with the Symbol.
         *
         * @param internal_func Pointer to an internal function to be
         *          denoted by the constructed Symbol.
         */
        explicit Symbol(const String *the_name, RObject *val = unboundValue(),
                        const BuiltInFunction *internal_func = nullptr);

        /** @brief Access internal function.
         *
         * @return const pointer to the internal function (if any)
         *         denoted by this Symbol.
         */
        const BuiltInFunction *internalFunction() const
        {
            return m_internalfunc;
        }

        /** @brief Access name.
         *
         * @return const pointer to the name of this Symbol.
         */
        const String *name() const
        {
            return m_name;
        }

        /** @brief Set internal function.
         *
         * @param func Pointer to the internal function now to be
         *          denoted by this symbol.  A null pointer is
         *          permissible.
         *
         * @note It would be better if this was set exclusively during
         * construction.
         */
        void setInternalFunction(const BuiltInFunction *fun)
        {
            xfix_refcnt(const_cast<BuiltInFunction *>(m_internalfunc), const_cast<BuiltInFunction *>(fun));
            m_internalfunc = fun;
            propagateAge(m_internalfunc);
        }

        /** @brief Set value.
         *
         * @param val Pointer to the RObject now to be considered as
         *            the value of this symbol.  A null pointer or
         *            unboundValue() are permissible values of \a val.
         */
        void setValue(RObject *val)
        {
            xfix_binding_refcnt(m_value, val);
            m_value = val;
            propagateAge(m_value);
        }

        /** @brief The name by which this type is known in R.
         *
         * @return The name by which this type is known in R.
         */
        static const char *staticTypeName()
        {
            return "symbol";
        }

        /** @brief Access value.
         *
         * @return pointer to the value of this Symbol.  Returns
         *         unboundValue() if no value is currently associated
         *         with the Symbol.
         */
        RObject *value()
        {
            return m_value;
        }

        /** @brief Access value (const variant).
         *
         * @return const pointer to the value of this Symbol.  Returns
         *         unboundValue() if no value is currently associated
         *         with the Symbol.
         */
        const RObject *value() const
        {
            return m_value;
        }

        bool baseSymbol() const
        {
            return m_base_symbol;
        }

        void setBaseSymbol(bool on)
        {
            // if (on)
            //     m_gpbits |= BASE_SYM_CACHED_MASK;
            // else
            //     m_gpbits &= (~BASE_SYM_CACHED_MASK);
            m_base_symbol = on;
        }

        bool specialSymbol() const
        {
            return m_special_symbol;
        }

        void setSpecialSymbol(bool on)
        {
            // if (on)
            //     m_gpbits |= SPECIAL_SYMBOL_MASK;
            // else
            //     m_gpbits &= (~SPECIAL_SYMBOL_MASK);
            m_special_symbol = on;
        }

        static void checkST(const RObject *);

        // Virtual functions of RObject:
        unsigned int packGPBits() const override;
        void unpackGPBits(unsigned int gpbits) override;
        const char *typeName() const override;

        // Virtual function of GCNode:
        void visitChildren(const_visitor *v) const override;

    private:
        static GCRoot<Symbol> s_missing_arg;
        static GCRoot<Symbol> s_restart_token;
        static GCRoot<Symbol> s_unbound_value;
        static GCRoot<Symbol> s_current_expression;
        static GCRoot<Symbol> s_in_bc_interpreter;

        const String *m_name;
        RObject *m_value;
        const BuiltInFunction *m_internalfunc;
        int m_dd_index : 31;
        bool m_base_symbol;
        bool m_special_symbol;

        // Special constructor for 'pseudo-objects':
        Symbol();

        // Declared private to ensure that Symbol objects are
        // allocated only using 'new':
        ~Symbol() {}

        // Not (yet) implemented.  Declared to prevent
        // compiler-generated versions:
        Symbol(const Symbol &);
        Symbol &operator=(const Symbol &);

        // Initialize the static data members:
        static void initialize();
        friend void ::R::InitNames();
    };
    SEXP install_(const std::string &name);
} // namespace CXXR

extern "C"
{
    extern SEXP R_MissingArg;        /* Missing argument marker */
    extern SEXP R_RestartToken;      /* Marker for restarted function calls */
    extern SEXP R_UnboundValue;      /* Unbound marker */
    extern SEXP R_CurrentExpression; /* Use current expression (marker) */
    extern SEXP R_InBCInterpreter;   /* To be found in BC interp. state (marker) */

    /** @brief Test if SYMSXP.
     *
     * @param s Pointer to a CXXR::RObject.
     *
     * @return TRUE iff s points to a CXXR::RObject with ::SEXPTYPE
     *         SYMSXP. 
     */
    Rboolean Rf_isSymbol(SEXP s);

    /** @brief Symbol name.
     *
     * @param x Pointer to a CXXR::Symbol (checked).
     *
     * @return Pointer to a CXXR::String representings \a x's name.
     */
    SEXP PRINTNAME(SEXP x);

    /** @brief Symbol's value in the base environment.
     *
     * @param x Pointer to a CXXR::Symbol (checked).
     *
     * @return Pointer to a CXXR::RObject representings \a x's value.
     *         Returns R_UnboundValue if no value is currently
     *         associated with the Symbol.
     */
    SEXP SYMVALUE(SEXP x);

    /** @brief Internal function value.
     *
     * @param x Pointer to a CXXR::Symbol (checked).
     *
     * @return If \a x denotes an internal function, a pointer to
     *         the appropriate CXXR::BuiltInFunction, otherwise a null
     *         pointer..
     */
    SEXP INTERNAL(SEXP x);

    /** @brief Does symbol relate to a <tt>...</tt> expression?
     *
     * @param x Pointer to a CXXR::Symbol (checked).
     *
     * @return \c TRUE iff this symbol denotes an element of a
     *         <tt>...</tt> expression.
     */
    int DDVAL(SEXP x);

    /** @brief Set Symbol name.
     *
     * @param x Pointer to a CXXR::Symbol (checked).
     *
     * @param v Pointer to a CXXR::String representing \a x's name. 
     */
    void SET_PRINTNAME(SEXP x, SEXP v);

    /** @brief Set symbol's value in the base environment.
     *
     * @param x Pointer to a CXXR::Symbol (checked).
     *
     * @param val Pointer to the RObject now to be considered as
     *            the value of this symbol.  A null pointer or
     *            R_UnboundValue are permissible values of \a val.
     *
     * @todo No binding to R_UnboundValue ought to be created.
     */
    void SET_SYMVALUE(SEXP x, SEXP v);

    /** @brief Set internal function denoted by a symbol.
     *
     * @param x Pointer to a CXXR::Symbol (checked).
     *
     * @param func Pointer to the CXXR::BuiltInFunction (checked) to
     *          be denoted by this symbol.  A null pointer is
     *          permissible.
     *
     * @note It would be better if this was set exclusively during
     * construction.
     */
    void SET_INTERNAL(SEXP x, SEXP v);
    Rboolean Rf_isUserBinop(SEXP s);
    void SET_BASE_SYM_CACHED(SEXP b);
    void UNSET_BASE_SYM_CACHED(SEXP b);
    Rboolean BASE_SYM_CACHED(SEXP b);

    void SET_SPECIAL_SYMBOL(SEXP b);
    void UNSET_SPECIAL_SYMBOL(SEXP b);
    Rboolean IS_SPECIAL_SYMBOL(SEXP b);
    void SET_NO_SPECIAL_SYMBOLS(SEXP b);
    void UNSET_NO_SPECIAL_SYMBOLS(SEXP b);
    Rboolean NO_SPECIAL_SYMBOLS(SEXP b);

    void SET_DDVAL(SEXP x, int v);
    SEXP Rf_installDDVAL(int i);
    SEXP Rf_install(const char *name);
} // extern "C"

namespace R
{
    /** @brief Create a CXXR::Symbol object.
     *
     * @param name Pointer to a CXXR::String object (checked) to be
     *          taken as the name of the constructed symbol.
     *
     * @param val Pointer to the CXXR::RObject to be considered as
     *          the value of the constructed symbol.  A null pointer or
     *          R_UnboundValue are permissible values of \a val.
     *
     * @return Pointer to the created CXXR::Symbol object.
     */
    SEXP mkSYMSXP(SEXP name, SEXP value);
} // namespace R

#if defined(R_NO_REMAP) && defined(COMPILING_IVORY) && defined(__cplusplus)
const auto isUserBinop = Rf_isUserBinop;
#endif

#endif /* SYMBOL_HPP */
