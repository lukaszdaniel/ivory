/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999--2020  The R Core Team.
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
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

/** @file Symbol.cpp
 *
 * @brief Implementation of class Symbol and associated C
 * interface.
 */

#include <CXXR/Symbol.hpp>
#include <Rinternals.h>

namespace CXXR
{
    // Force the creation of non-inline embodiments of functions callable
    // from C:
    namespace ForceNonInline
    {
        const auto &DDVALptr = DDVAL;
        const auto &installptr = Rf_install;
        const auto &isSymbolptr = Rf_isSymbol;
        const auto &PRINTNAMEptr = PRINTNAME;
        const auto &SYMVALUEptr = SYMVALUE;
        const auto &INTERNALptr = INTERNAL;
        const auto &SET_PRINTNAMEptr = SET_PRINTNAME;
        const auto &SET_SYMVALUEptr = SET_SYMVALUE;
        const auto &SET_INTERNALptr = SET_INTERNAL;
        const auto &SET_DDVALptr = SET_DDVAL;
    } // namespace ForceNonInline

    const char *Symbol::typeName() const
    {
        return staticTypeName();
    }

    void Symbol::visitChildren(const_visitor *v) const
    {
        RObject::visitChildren(v);
        if (m_name)
            m_name->conductVisitor(v);
        if (m_value)
            m_value->conductVisitor(v);
        if (m_internalfunc)
            m_internalfunc->conductVisitor(v);
    }

    /** @brief Symbol name.
     *
     * @param x Pointer to a CXXR::Symbol (checked).
     *
     * @return Pointer to a R::String representing \a x's name.
     */
    RObject *Symbol::printname(RObject *x)
    {
        if (!x)
            return nullptr;
#ifdef ENABLE_ST_CHECKS
        switch (x->sexptype())
        {
        case SYMSXP:
            break;
        default:
            std::cerr << LOCATION << "Inappropriate SEXPTYPE (" << x->sexptype() << ") for Symbol." << std::endl;
            abort();
        }
#endif
        const Symbol &sym = *SEXP_downcast<Symbol *>(x);
        return const_cast<String *>(sym.name());
    }

    /** @brief Symbol's value in the base environment.
     *
     * @param x Pointer to a CXXR::Symbol (checked).
     *
     * @return Pointer to a CXXR::RObject representings \a x's value.
     *         Returns R_UnboundValue if no value is currently
     *         associated with the Symbol.
     */
    RObject *Symbol::symvalue(RObject *x)
    {
        if (!x)
            return nullptr;
#ifdef ENABLE_ST_CHECKS
        switch (x->sexptype())
        {
        case SYMSXP:
            break;
        default:
            std::cerr << LOCATION << "Inappropriate SEXPTYPE (" << x->sexptype() << ") for Symbol." << std::endl;
            abort();
        }
#endif
        Symbol &sym = *SEXP_downcast<Symbol *>(x);
        return sym.value();
    }

    RObject *Symbol::internal(RObject *x)
    {
        if (!x)
            return nullptr;
#ifdef ENABLE_ST_CHECKS
        switch (x->sexptype())
        {
        case SYMSXP:
            break;
        default:
            std::cerr << LOCATION << "Inappropriate SEXPTYPE (" << x->sexptype() << ") for Symbol." << std::endl;
            abort();
        }
#endif
        Symbol &sym = *SEXP_downcast<Symbol *>(x);
        return const_cast<BuiltInFunction *>(sym.internalFunction());
    }

    /** @brief Does symbol relate to a <tt>...</tt> expression?
     *
     * @param x Pointer to a CXXR::Symbol (checked).
     *
     * @return \c TRUE iff this symbol denotes an element of a
     *         <tt>...</tt> expression.
     */
    unsigned int Symbol::ddval(RObject *x) { return x ? (x->m_gpbits & DDVAL_MASK) : 0; } /* for ..1, ..2 etc */

 
    void Symbol::set_ddval(RObject *x, bool v)
    {
    } /* for ..1, ..2 etc */

    /** @brief Set Symbol name.
     *
     * @param x Pointer to a CXXR::Symbol (checked).
     *
     * @param v Pointer to a R::String representing \a x's name. 
     */
    void Symbol::set_printname(RObject *x, RObject *v)
    {
        if (!x)
            return;
#ifdef ENABLE_ST_CHECKS
        switch (x->sexptype())
        {
        case SYMSXP:
            break;
        default:
            std::cerr << LOCATION << "Inappropriate SEXPTYPE (" << x->sexptype() << ") for Symbol." << std::endl;
            abort();
        }
#endif
    }

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
    void Symbol::set_symvalue(RObject *x, RObject *val)
    {
        if (!x)
            return;
#ifdef ENABLE_ST_CHECKS
        switch (x->sexptype())
        {
        case SYMSXP:
            break;
        default:
            std::cerr << LOCATION << "Inappropriate SEXPTYPE (" << x->sexptype() << ") for Symbol." << std::endl;
            abort();
        }
#endif
        Symbol &sym = *SEXP_downcast<Symbol *>(x);
        sym.setValue(val);
    }

    void Symbol::set_internal(RObject *x, RObject *v)
    {
        if (!x)
            return;
#ifdef ENABLE_ST_CHECKS
        switch (x->sexptype())
        {
        case SYMSXP:
            break;
        default:
            std::cerr << LOCATION << "Inappropriate SEXPTYPE (" << x->sexptype() << ") for Symbol." << std::endl;
            abort();
        }
#endif
        Symbol &sym = *SEXP_downcast<Symbol *>(x);
        BuiltInFunction *fun = SEXP_downcast<BuiltInFunction *>(v);
        sym.setInternalFunction(fun);
    }
} // namespace CXXR
