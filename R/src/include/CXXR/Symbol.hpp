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

#include <CXXR/GCRoot.hpp>
#include <CXXR/GCStackRoot.hpp>
#include <CXXR/RObject.hpp>
#include <CXXR/BuiltInFunction.hpp>
#include <CXXR/CachedString.hpp>
#include <CXXR/SEXP_downcast.hpp>
#include <R_ext/Boolean.h>

namespace R
{
    extern void InitNames();
} // namespace R

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
    private:
        // This table is used to ensure that, for standard symbols,
        // there is at most one Symbol object with a particular name.
        typedef std::unordered_map<const CachedString *, Symbol *,
                                   std::hash<const CachedString *>,
                                   std::equal_to<const CachedString *>,
                                   CXXR::Allocator<std::pair<const CachedString *const,
                                                             Symbol *>>>
            Table;

    public:
        /** @brief const_iterator for iterating over all standard Symbols.
         *
         * This is used in BuiltInSize() and BuiltInNames().
         */
        typedef Table::const_iterator const_iterator;

        static const_iterator begin() { return getTable()->begin(); }

        static const_iterator end() { return getTable()->end(); }

        /** @brief Restart token.
         *
         * @return a pointer to the 'restart token' pseudo-object,
         * which is identified as such by its address, not by its
         * content.
         */
        static Symbol *restartToken();

        static Symbol *inBCInterpreter();

        static Symbol *currentExpression();

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

        /** @brief Access internal function.
         *
         * @return const pointer to the internal function (if any)
         *         denoted by this Symbol.
         */
        const BuiltInFunction *internalFunction() const
        {
            return m_internalfunc;
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
            // m_internalfunc = fun;
            // m_internalfunc.propagateAge(this);
            m_internalfunc.retarget(this, fun);
        }

        /** @brief Set value.
         *
         * @param val Pointer to the RObject now to be considered as
         *            the value of this symbol.  A null pointer or
         *            unboundValue() are permissible values of \a val.
         */
        void setValue(RObject *val)
        {
            if (value() == val)
                return;
            if (trackrefs() && assignmentPending())
            {
                setAssignmentPending(false);
                GCNode::incRefCount(m_value);
            }
            // m_value = val;
            // m_value.propagateAge(this);
            m_value.retarget(this, val);
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

        /** @brief Is this a special symbol?
         *
         * Special symbols are symbols that get special lookup optimizations.
         * Environments on the function call stack that
         * have never contained such a symbol are marked as such, so they can
         * be quickly skipped when searching for a function named by such a
         * special symbol.
         *
         * In principle, any symbol could be marked as special, but ones that
         * contain special characters, or are reserved words, are the ones
         * unlikely to be defined in any environment other than base, and hence
         * the ones where this is most likely to help.
         *
         * @return true iff this symbol is marked as a special symbol.
         */
        bool isSpecialSymbol() const
        {
            return m_is_special_symbol;
        }

        void setSpecialSymbol(bool on)
        {
            // if (on)
            //     m_gpbits |= SPECIAL_SYMBOL_MASK;
            // else
            //     m_gpbits &= (~SPECIAL_SYMBOL_MASK);
            m_is_special_symbol = on;
        }

        /** @brief Missing argument.
         *
         * @return a pointer to the 'missing argument' pseudo-object,
         * which is identified as such by its address, not by its
         * content.
         */
        static Symbol *missingArgument();

        /** @brief Access name.
         *
         * @return const pointer to the name of this Symbol.
         */
        const CachedString *name() const
        {
            return m_name;
        }

        /** @brief Get a pointer to a regular Symbol object.
         *
         * If no Symbol with the specified name currently exists, one
         * will be created, and a pointer to it returned.  Otherwise a
         * pointer to the existing Symbol will be returned.
         *
         * @param name The name of the required Symbol.
         *
         * @return Pointer to a Symbol (preexisting or newly
         * created) with the required name.
         */
        static Symbol *obtain(const CachedString *name);

        /** @brief Get a pointer to a regular Symbol object.
         *
         * If no Symbol with the specified name currently exists, one
         * will be created, and a pointer to it returned.  Otherwise a
         * pointer to the existing Symbol will be returned.
         *
         * @param name The name of the required Symbol (CE_UTF8
         *          encoding is assumed).  At present no check is made
         *          that the supplied string is a valid symbol name.
         *
         * @return Pointer to a Symbol (preexisting or newly
         * created) with the required name.
         */
        static Symbol *obtain(const std::string &name);

        /** @brief Get a pointer to a regular Symbol object.
         *
         * If no Symbol with the specified name currently exists, one
         * will be created, and a pointer to it returned.  Otherwise a
         * pointer to the existing Symbol will be returned.
         *
         * @param name The name of the required Symbol in \a enc
         *          encoding.  At present no check is made
         *          that the supplied string is a valid symbol name.
         *
         * @return Pointer to a Symbol (preexisting or newly
         * created) with the required name.
         */
        static Symbol *obtainCE(const std::string &name, cetype_t enc = CE_UTF8);

        /** @brief Create a double-dot symbol.
         *
         * @param n Index number of the required symbol; must be
         *          strictly positive.
         *
         * @return a pointer to the created symbol, whose name will be
         * <tt>..</tt><i>n</i>.
         */
        static Symbol *obtainDotDotSymbol(unsigned int n);

        /** @brief Get a pointer to a Symbol for an S3 method.
         *
         * If no Symbol with the specified signature currently exists, one
         * will be created, and a pointer to it returned.  Otherwise a
         * pointer to the existing Symbol will be returned.
         *
         * @param className The name of the class that the method is for.
         *
         * @param methodName The name of the function that the method is for.
         *
         * @return Pointer to a Symbol (preexisting or newly
         * created) with the required signature.
         */
        static Symbol *obtainS3Signature(const char *className, const char *methodName);

        /** @brief The name by which this type is known in R.
         *
         * @return The name by which this type is known in R.
         */
        static const char *staticTypeName()
        {
            return "symbol";
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
        static Symbol *unboundValue();

        static void checkST(const RObject *);

        /** @brief Largest symbol size.
         *
         * Largest symbol size in bytes excluding terminator.
         *
         * Was 256 prior to 2.13.0, now just a sanity check.
         */
        static constexpr int MAXIDSIZE = 10000;

        /** @brief Conduct a visitor to all standard symbols.
         *
         * @param v Pointer to the visitor object.
         */
        static void visitTable(const_visitor *v);

        // Virtual functions of RObject:
        unsigned int packGPBits() const override;
        void unpackGPBits(unsigned int gpbits) override;
        const char *typeName() const override;
        RObject *evaluate(Environment *env) override;

        // Virtual function of GCNode:
        void visitReferents(const_visitor *v) const override;

        /** @brief Return a symbol object that has no name.
         *
         *  An unnamed symbol is useful in places where an illegal symbol is
         *  useful.  Examples are missing arguments and unbound values.
         *
         *  Note that unlike other symbols, unnamed symbols are not
         *  automatically protected from garbage collection.
         */
        static Symbol *createUnnamedSymbol();

    protected:
    private:
        // static GCRoot<Symbol> s_missing_arg;
        // static GCRoot<Symbol> s_restart_token;
        static GCStackRoot<Symbol> s_unbound_value;
        // static GCRoot<Symbol> s_current_expression;
        // static GCRoot<Symbol> s_in_bc_interpreter;

        static Table *getTable(); // Vector of
                                  // pointers to all Symbol objects in existence, other than
                                  // psuedo-symbols and deserialization temporaries, used to
                                  // protect them against garbage collection.
        GCEdge<const CachedString> m_name;
        GCEdge<> m_value;
        GCEdge<const BuiltInFunction> m_internalfunc;
        int m_dd_index : 31;
        bool m_base_symbol;
        bool m_is_special_symbol;

        /**
         * @param name Pointer to String object representing the name
         *          of the symbol.  Names of the form
         *          <tt>..<em>n</em></tt>, where n is a (non-negative)
         *          decimal integer signify that the Symbol to be
         *          constructed relates to an element of a
         *          <tt>...</tt> argument list.  A null pointer
         *          signifies a psuedo-symbol, which is not entered
         *          into the table.
         */
        explicit Symbol(const CachedString *name = CachedString::blank());

        // Declared private to ensure that Symbol objects are
        // allocated only using 'new':
        ~Symbol();

        // Not (yet) implemented.  Declared to prevent
        // compiler-generated versions:
        Symbol(const Symbol &);
        Symbol &operator=(const Symbol &);

        // Initialize the static data members:
        static void initialize();
        friend void ::R::InitNames();

        static const std::vector<std::string> s_special_symbol_names;

        // Precondition: there is not already a Symbol identified by
        // 'name'.
        //
        // Creates a new Symbol identified by 'name', enters it into
        // the table of standard Symbols, and returns a pointer to it.
        static Symbol *make(const CachedString *name);
    };

    /** @brief Does Symbol's name start with '.'?
     *
     * @param symbol pointer to Symbol to be tested, or a null pointer
     *          in which case the function returns false.
     *
     * @return true if the Symbol's name starts with '.'.
     */
    bool isDotSymbol(const Symbol *symbol);

    /** @brief Does Symbol's name start with '..'?
     *
     * @param symbol pointer to Symbol to be tested, or a null pointer
     *          in which case the function returns false.
     *
     * @return true if the Symbol's name starts with '..'.
     */
    inline bool isDotDotSymbol(const Symbol *symbol)
    {
        return symbol && symbol->isDotDotSymbol();
    }

// Predefined Symbols visible in CXXR namespace
#define PREDEFINED_SYMBOL(C_NAME, CXXR_NAME, R_NAME) \
    extern Symbol *CXXR_NAME;
#include <CXXR/PredefinedSymbols.hpp>
#undef PREDEFINED_SYMBOL
} // namespace CXXR

namespace R
{
    SEXP Rf_installDDVAL(int i);
    SEXP Rf_installS3Signature(const char *methodName, const char *className);
    void SET_DDVAL(SEXP x, int v);

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

    /** @brief Create a CXXR::Symbol object.
     *
     * @param name Pointer to a CXXR::String object (checked) to be
     *          taken as the name of the constructed symbol.
     *
     * @param value Pointer to the CXXR::RObject to be considered as
     *          the value of the constructed symbol.  A null pointer or
     *          R_UnboundValue are permissible values of \a value.
     *
     * @return Pointer to the created CXXR::Symbol object.
     */
    SEXP mkSYMSXP(SEXP name, SEXP value);
} // namespace R

extern "C"
{
    /** @brief Missing argument marker
     */
    extern SEXP R_MissingArg;

    /** @brief Marker for restarted function calls
     */
    extern SEXP R_RestartToken;

    /** @brief Unbound marker
     */
    extern SEXP R_UnboundValue;

    /** @brief Use current expression (marker)
     */
    extern SEXP R_CurrentExpression;

    /** @brief To be found in BC interp. state (marker)
     */
    extern SEXP R_InBCInterpreter;

    /* Symbol Table Shortcuts */
#define PREDEFINED_SYMBOL(C_NAME, CXXR_NAME, R_NAME) \
    extern SEXP C_NAME;
#include <CXXR/PredefinedSymbols.hpp>
#undef PREDEFINED_SYMBOL

    /** @brief Is this a symbol?
     *
     * @param s Pointer to CXXR::RObject.
     *
     * @return TRUE iff the RObject pointed to by \a s is a symbol.
     */
    Rboolean Rf_isSymbol(SEXP s);

    /** @brief Find value of a <tt>..<em>n</em></tt> Symbol.
     *
     * @param symbol Pointer to a Symbol (checked) whose name is of
     *          the form <tt>..<em>n</em></tt>, where <em>n</em> is a
     *          positive integer.
     *
     * @param rho Pointer to an Environment, which must bind the
     *          symbol <tt>...</tt> to a PairList comprising at least
     *          <em>n</em> elements.  (All checked.)
     *
     * @return The 'car' of the <em>n</em>th element of the PairList to
     * which <tt>...</tt> is bound.
     */
    SEXP Rf_ddfindVar(SEXP symbol, SEXP rho);

    /** @brief Symbol name.
     *
     * @param x Pointer to a CXXR::Symbol (checked).
     *
     * @return Pointer to a CXXR::CachedString representing \a x's name.
     */
    SEXP PRINTNAME(SEXP x);

    /** @brief Get a pointer to a regular Symbol object.
     *
     * If no Symbol with the specified name currently exists, one will
     * be created, and a pointer to it returned.  Otherwise a pointer
     * to the existing Symbol will be returned.
     *
     * @param name The name of the required Symbol (CE_NATIVE encoding
     *          is assumed).
     *
     * @return Pointer to a Symbol (preexisting or newly created) with
     * the required name.
     */
    SEXP Rf_install(const char *name);

    /** @brief Symbol's value in the base environment.
     *
     * @param x Pointer to a CXXR::Symbol (checked).
     *
     * @return Pointer to a CXXR::RObject representing \a x's value.
     *         Returns R_UnboundValue if no value is currently
     *         associated with the Symbol.
     */
    SEXP SYMVALUE(SEXP x);

    /** @brief Does symbol relate to a <tt>...</tt> expression?
     *
     * @param x Pointer to a CXXR::Symbol (checked).
     *
     * @return \c TRUE iff this symbol denotes an element of a
     *         <tt>...</tt> expression.
     */
    int DDVAL(SEXP x);

    Rboolean Rf_isUserBinop(SEXP s);
    void SET_BASE_SYM_CACHED(SEXP b);
    void UNSET_BASE_SYM_CACHED(SEXP b);
    Rboolean BASE_SYM_CACHED(SEXP b);

    void SET_SPECIAL_SYMBOL(SEXP b);
    void UNSET_SPECIAL_SYMBOL(SEXP b);
    Rboolean IS_SPECIAL_SYMBOL(SEXP b);

    /* This function is equivalent to Rf_install(R_CHAR(charSXP)), but faster.
   Like the equivalent code pattern, it discards the encoding information,
   hence in almost all cases installTrChar should be used, instead. */
    SEXP Rf_installNoTrChar(SEXP charSXP);
} // extern "C"

#if (defined(R_NO_REMAP) && defined(COMPILING_IVORY)) && defined(__cplusplus)
const auto isUserBinop = Rf_isUserBinop;
#endif

#endif /* SYMBOL_HPP */
