/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999--2020  The R Core Team.
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
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

/** @file Symbol.cpp
 *
 * @brief Implementation of class Symbol and associated C
 * interface.
 */

#include <CXXR/Symbol.hpp>
#include <CXXR/CachedString.hpp>
#include <Rinternals.h>
#include <boost/regex.hpp>
#include <sstream>

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

    Symbol::Symbol(const String *the_name, RObject *val, const BuiltInFunction *internal_func)
        : RObject(SYMSXP), m_name(the_name), m_value(val), m_internalfunc(internal_func), m_dd_index(-1), m_base_symbol(false), m_special_symbol(false)
    {
        // If this is a ..n symbol, extract the value of n.
        // boost::regex_match (libboost_regex1_36_0-1.36.0-9.5) doesn't
        // seem comfortable with empty strings, hence the size check.
        if (m_name && m_name->size() > 2)
        {
            // Versions of GCC prior to 4.9 don't support std::regex, so use
            // boost::regex instead.
            static const boost::regex *regex = new boost::regex("\\.\\.(\\d+)");

            std::string name(m_name->c_str());
            boost::smatch dd_match;
            if (boost::regex_match(name, dd_match, *regex))
            {
                std::istringstream iss(dd_match[1]);
                int n;
                iss >> n;
                m_dd_index = n;
            }
        }
    }

    Symbol::Symbol()
        : RObject(SYMSXP), m_name(nullptr), m_value(s_unbound_value),
          m_internalfunc(nullptr), m_dd_index(-1), m_base_symbol(false), m_special_symbol(false)
    {
    }

    GCRoot<Symbol> Symbol::s_unbound_value(new Symbol(), true);
    GCRoot<Symbol> Symbol::s_missing_arg(new Symbol(String::blank()), true);
    GCRoot<Symbol> Symbol::s_restart_token(new Symbol(String::blank()), true);
    GCRoot<Symbol> Symbol::s_in_bc_interpreter(new Symbol(CachedString::obtain("<in-bc-interp>")), true);
    GCRoot<Symbol> Symbol::s_current_expression(new Symbol(CachedString::obtain("<current-expression>")), true);

    namespace
    {
        // Used in {,un}packGPBits():
        constexpr unsigned int SPECIAL_SYMBOL_MASK = 1 << 12;
        constexpr unsigned int BASE_SYM_CACHED_MASK = 1 << 13;
    } // namespace

    unsigned int Symbol::packGPBits() const
    {
        unsigned int ans = RObject::packGPBits();
        if (m_special_symbol)
            ans |= SPECIAL_SYMBOL_MASK;
        if (m_base_symbol)
            ans |= BASE_SYM_CACHED_MASK;
        return ans;
    }

    void Symbol::unpackGPBits(unsigned int gpbits)
    {
        RObject::unpackGPBits(gpbits);
        m_special_symbol = ((gpbits & SPECIAL_SYMBOL_MASK) != 0);
        m_base_symbol = ((gpbits & BASE_SYM_CACHED_MASK) != 0);
    }

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

    void Symbol::initialize()
    {
        /* Create marker values */
        R_UnboundValue = Symbol::unboundValue();
        R_MissingArg = Symbol::missingArgument();
        R_RestartToken = Symbol::restartToken();
        R_InBCInterpreter = Symbol::inBCInterpreter();
        R_CurrentExpression = Symbol::currentExpression();

#if CXXR_FALSE
        R_MissingArg = missingArgument();
        R_UnboundValue = unboundValue();

        for (int i = 0; s_special_symbol_names[i] != nullptr; i++)
        {
            Symbol *symbol = Symbol::obtain(s_special_symbol_names[i]);
            symbol->m_is_special_symbol = true;
        }

#define PREDEFINED_SYMBOL(C_NAME, CXXR_NAME, R_NAME) \
    C_NAME = CXXR::Symbols::CXXR_NAME = Symbol::obtain(R_NAME);
#include <CXXR/PredefinedSymbols.hpp>
#undef PREDEFINED_SYMBOL
        // DISABLE_REFCNT(Symbols::LastvalueSymbol);
#endif
    }

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

    unsigned int Symbol::ddval(RObject *x) /* for ..1, ..2 etc */
    {
        return x ? SEXP_downcast<Symbol *>(x)->isDotDotSymbol() : false;
    }

    void Symbol::set_ddval(RObject *x, bool v) /* for ..1, ..2 etc */
    {
    }

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

    void Symbol::set_base_sym_cached(RObject *x)
    {
        if (!x)
            return;
        // x->m_gpbits |= BASE_SYM_CACHED_MASK;
        SEXP_downcast<Symbol *>(x)->m_base_symbol = true;
    }

    void Symbol::unset_base_sym_cached(RObject *x)
    {
        if (!x)
            return;
        // x->m_gpbits &= (~BASE_SYM_CACHED_MASK);
        SEXP_downcast<Symbol *>(x)->m_base_symbol = false;
    }

    unsigned int Symbol::base_sym_cached(RObject *x)
    {
        return x && SEXP_downcast<Symbol *>(x)->m_base_symbol;
    }

    unsigned int Symbol::no_special_symbols(RObject *x)
    {
        return x && SEXP_downcast<Symbol *>(x)->m_special_symbol;
    }

    void Symbol::set_no_special_symbols(RObject *x)
    {
        if (!x)
            return;
        // x->m_gpbits |= SPECIAL_SYMBOL_MASK;
        SEXP_downcast<Symbol *>(x)->m_special_symbol = true;
    }

    unsigned int Symbol::is_special_symbol(RObject *x)
    {
        return x && SEXP_downcast<Symbol *>(x)->m_special_symbol;
    }

    void Symbol::set_special_symbol(RObject *x)
    {
        if (!x)
            return;
        // x->m_gpbits |= SPECIAL_SYMBOL_MASK;
        SEXP_downcast<Symbol *>(x)->m_special_symbol = true;
    }

    void Symbol::unset_no_special_symbols(RObject *x)
    {
        if (!x)
            return;
        // x->m_gpbits &= (~SPECIAL_SYMBOL_MASK);
        SEXP_downcast<Symbol *>(x)->m_special_symbol = false;
    }

    void Symbol::unset_special_symbol(RObject *x)
    {
        if (!x)
            return;
        // x->m_gpbits &= (~SPECIAL_SYMBOL_MASK);
        SEXP_downcast<Symbol *>(x)->m_special_symbol = false;
    }
} // namespace CXXR

// ***** C interface *****

Rboolean Rf_isUserBinop(SEXP s)
{
    if (TYPEOF(s) == SYMSXP)
    {
        const char *str = R_CHAR(PRINTNAME(s));
        if (strlen(str) >= 2 && str[0] == '%' && str[strlen(str) - 1] == '%')
            return TRUE;
    }
    return FALSE;
}
