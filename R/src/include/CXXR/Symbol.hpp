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
#include <CXXR/SEXP_downcast.hpp>
#include <R_ext/Boolean.h>

/* Symbol Table Shortcuts */
extern "C" SEXP R_AsCharacterSymbol;  /* "as.character" */
extern "C" SEXP R_baseSymbol;         // <-- backcompatible version of:
extern "C" SEXP R_BaseSymbol;         // "base"
extern "C" SEXP R_BraceSymbol;        /* "{" */
extern "C" SEXP R_Bracket2Symbol;     /* "[[" */
extern "C" SEXP R_BracketSymbol;      /* "[" */
extern "C" SEXP R_ClassSymbol;        /* "class" */
extern "C" SEXP R_DeviceSymbol;       /* ".Device" */
extern "C" SEXP R_DimNamesSymbol;     /* "dimnames" */
extern "C" SEXP R_DimSymbol;          /* "dim" */
extern "C" SEXP R_DollarSymbol;       /* "$" */
extern "C" SEXP R_DotsSymbol;         /* "..." */
extern "C" SEXP R_DoubleColonSymbol;  // "::"
extern "C" SEXP R_DropSymbol;         /* "drop" */
extern "C" SEXP R_EvalSymbol;         /* "eval" */
extern "C" SEXP R_LastvalueSymbol;    /* ".Last.value" */
extern "C" SEXP R_LevelsSymbol;       /* "levels" */
extern "C" SEXP R_ModeSymbol;         /* "mode" */
extern "C" SEXP R_NaRmSymbol;         /* "na.rm" */
extern "C" SEXP R_NameSymbol;         /* "name" */
extern "C" SEXP R_NamesSymbol;        /* "names" */
extern "C" SEXP R_NamespaceEnvSymbol; // ".__NAMESPACE__."
extern "C" SEXP R_PackageSymbol;      /* "package" */
extern "C" SEXP R_PreviousSymbol;     /* "previous" */
extern "C" SEXP R_QuoteSymbol;        /* "quote" */
extern "C" SEXP R_RowNamesSymbol;     /* "row.names" */
extern "C" SEXP R_SeedsSymbol;        /* ".Random.seed" */
extern "C" SEXP R_SortListSymbol;     /* "sort.list" */
extern "C" SEXP R_SourceSymbol;       /* "source" */
extern "C" SEXP R_SpecSymbol;         // "spec"
extern "C" SEXP R_TripleColonSymbol;  // ":::"
extern "C" SEXP R_TspSymbol;          /* "tsp" */

extern "C" SEXP R_dot_defined;     /* ".defined" */
extern "C" SEXP R_dot_Method;      /* ".Method" */
extern "C" SEXP R_dot_packageName; // ".packageName"
extern "C" SEXP R_dot_target;      /* ".target" */
extern "C" SEXP R_dot_Generic;     /* ".Generic" */

/* Missing Values - others from Arith.h */
extern "C" SEXP R_NaString;          /* NA_STRING as a CHARSXP */
extern "C" SEXP R_BlankString;       /* "" as a CHARSXP */
extern "C" SEXP R_BlankScalarString; /* "" as a STRSXP */

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
        RObject *m_pname;
        RObject *m_value;
        RObject *m_internal;
        // bool m_ddval;
        // Declared private to ensure that Symbol objects are
        // allocated only using 'new':
        ~Symbol(){};

    public:
        // Virtual functions of RObject:
        const char *typeName() const override;

        /** @brief The name by which this type is known in R.
         *
         * @return The name by which this type is known in R.
         */
        static const char *staticTypeName()
        {
            return "symbol";
        }
        auto printname() const { return this->m_pname; }
        auto symvalue() const { return this->m_value; }
        auto internal() const { return this->m_internal; }

        /* Symbol Access Methods */
        static constexpr int DDVAL_MASK = 1;
        static RObject *printname(RObject *x);
        static RObject *symvalue(RObject *x);
        static RObject *internal(RObject *x);
        static unsigned int ddval(RObject *x); /* for ..1, ..2 etc */
        static void set_ddval_bit(RObject *x);
        static void unset_ddval_bit(RObject *x);
        static void set_ddval(RObject *x, bool v); /* for ..1, ..2 etc */
        static void set_printname(RObject *x, RObject *v);
        static void set_symvalue(RObject *x, RObject *val);
        static void set_internal(RObject *x, RObject *v);
    };
} // namespace CXXR

extern "C"
{
    /**
     * @param s Pointer to an RObject.
     * @return TRUE iff the RObject pointed to by \a s is a symbol.
     */
    Rboolean Rf_isSymbol(SEXP s);

    /* Accessor functions. */

    /* Symbol Access Functions */

    /**
     * Symbol name.
     * @param x Pointer to an \c Symbol.
     * @return Pointer to \c RObject representings \a x's name.
     */
    SEXP PRINTNAME(SEXP x);

    /**
     * Symbol value.
     * @param x Pointer to an \c Symbol.
     * @return Pointer to \c RObject representings \a x's value.
     */
    SEXP SYMVALUE(SEXP x);

    /**
     * Internal function value.
     * @param x Pointer to an \c Symbol.
     * @return ? If \a x represents and internal function, the corresponding
     * \c RObject, otherwise NULL..
     */
    SEXP INTERNAL(SEXP x);

    /**
     * Did symbol arise from ... expression?
     * @param x Pointer to an \c Symbol.
     * @return \c true iff this symbol arose from a ... expression.
     */
    int DDVAL(SEXP x);

    /**
     * @deprecated Ought to be private.
     */
    void SET_DDVAL(SEXP x, int v);

    /**
     * Set symbol's name.
     * @param x Pointer to an \c Symbol.
     * @param v Pointer to an \c RObject representing the new name.
     */
    void SET_PRINTNAME(SEXP x, SEXP v);

    /**
     * Set symbol's value.
     * @param x Pointer to an \c Symbol.
     * @param v Pointer to an \c RObject representing the new value.
     */
    void SET_SYMVALUE(SEXP x, SEXP v);

    /**
     * Set internal function.
     * @param x Pointer to an \c Symbol.
     * @param v Pointer to an \c RObject representing an internal function.
     */
    void SET_INTERNAL(SEXP x, SEXP v);
} // extern "C"

#endif /* SYMBOL_HPP */
