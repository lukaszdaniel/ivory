/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999--2020  The R Core Team.
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
 *
 *  This program is free software; you can redistribute it and/or modify
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

#define R_NO_REMAP

#include <CXXR/GCManager.hpp>
#include <CXXR/GCRoot.hpp>
#include <CXXR/Symbol.hpp>
#include <CXXR/CachedString.hpp>
#include <CXXR/Environment.hpp>
#include <CXXR/Promise.hpp>
#include <Rinternals.h>
#include <Rinterface.h>
#include <boost/regex.hpp>
#include <sstream>

using namespace CXXR;

extern "C"
{
    SEXP Rf_findVar(SEXP symbol, SEXP rho);
}

namespace R
{
    SEXP ddfindVar(SEXP symbol, SEXP rho);
    const char *EncodeChar(SEXP x);
}

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

    // Symbol::s_special_symbol_names is in names.cpp

    Symbol::Symbol(const CachedString *the_name)
        : RObject(SYMSXP), m_dd_index(-1), m_base_symbol(false), m_is_special_symbol(false)
    {
        m_name = the_name;
        m_value = unboundValue();
        m_internalfunc = nullptr;

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

    Symbol::~Symbol()
    {
        // nothing needed.
    }

    Symbol *Symbol::obtain(const CachedString *name)
    {
        // return make(name);
        return (name->m_symbol ? name->m_symbol : make(name));
    }

    Symbol *Symbol::createUnnamedSymbol()
    {
        return GCNode::expose(new Symbol());
    }

    GCStackRoot<Symbol> Symbol::s_unbound_value(GCNode::expose(new Symbol()));
    // GCRoot<Symbol> Symbol::s_missing_arg(GCNode::expose(new Symbol()));
    // GCRoot<Symbol> Symbol::s_restart_token(GCNode::expose(new Symbol()));
    // GCRoot<Symbol> Symbol::s_in_bc_interpreter(GCNode::expose(new Symbol(CachedString::obtain("<in-bc-interp>"))));
    // GCRoot<Symbol> Symbol::s_current_expression(GCNode::expose(new Symbol(CachedString::obtain("<current-expression>"))));

    namespace
    {
        // Used in {,un}packGPBits():
        constexpr unsigned int SPECIAL_SYMBOL_MASK = 1 << 12;
        constexpr unsigned int BASE_SYM_CACHED_MASK = 1 << 13;
    } // namespace

    unsigned int Symbol::packGPBits() const
    {
        unsigned int ans = RObject::packGPBits();
        if (m_is_special_symbol)
            ans |= SPECIAL_SYMBOL_MASK;
        if (m_base_symbol)
            ans |= BASE_SYM_CACHED_MASK;
        return ans;
    }

    void Symbol::unpackGPBits(unsigned int gpbits)
    {
        RObject::unpackGPBits(gpbits);
        m_is_special_symbol = ((gpbits & SPECIAL_SYMBOL_MASK) != 0);
        m_base_symbol = ((gpbits & BASE_SYM_CACHED_MASK) != 0);
    }

    const char *Symbol::typeName() const
    {
        return staticTypeName();
    }

    void Symbol::visitReferents(const_visitor *v) const
    {
        RObject::visitReferents(v);
        if (m_name)
            m_name->conductVisitor(v);
        if (m_value)
            m_value->conductVisitor(v);
        if (m_internalfunc)
            m_internalfunc->conductVisitor(v);
    }

    RObject *Symbol::evaluate(Environment *env)
    {
        if (this == R_DotsSymbol)
            Rf_error(_("'...' used in an incorrect context"));
        GCStackRoot<> val;
        if (isDotDotSymbol())
            val = R::ddfindVar(this, env);
        else
            val = Rf_findVar(this, env);
        if (val == unboundValue())
            Rf_error(_("object '%s' was not found"), R::EncodeChar(PRINTNAME(this)));
        /* if ..d is missing then ddfindVar will signal */
        else if (val == R_MissingArg && !isDotDotSymbol())
        {
            if (name())
                Rf_error(_("'%s' argument is missing, with no default"), name()->c_str());
            else
                Rf_error(_("'%s' argument is missing, with no default"), "expr");
        }
        else if (TYPEOF(val) == PROMSXP)
        {
            if (PRVALUE(val) == unboundValue())
            {
                val = SEXP_downcast<Promise *>(val.get())->force();
            }
            else
                val = PRVALUE(val);
            ENSURE_NAMEDMAX(val);
        }
        else
            ENSURE_NAMED(val); /* should not really be needed - LT */
        return val;
    }

    void Symbol::initialize()
    {
        /* Create marker values */
        R_UnboundValue = Symbol::unboundValue();
        R_MissingArg = Symbol::missingArgument();
        R_RestartToken = Symbol::restartToken();
        R_InBCInterpreter = Symbol::inBCInterpreter();
        R_CurrentExpression = Symbol::currentExpression();

        /* Set up a set of globals so that a symbol table search can be
       avoided when matching something like dim or dimnames. */
#define PREDEFINED_SYMBOL(C_NAME, CXXR_NAME, R_NAME) \
    C_NAME = Symbol::obtain(R_NAME);
#include <CXXR/PredefinedSymbols.hpp>
#undef PREDEFINED_SYMBOL
        /* The last value symbol is used by the interpreter for recording
       the value of the most recently evaluated top level
       expression. To avoid creating an additional reference that
       would requires duplicating on modification this symbol does not
       increment reference counts on its symbol value.  This is safe
       since the symbol value corresponds to the base environment
       where complex assignments are not allowed.  */
        DISABLE_REFCNT(R_LastvalueSymbol);
        DISABLE_REFCNT(R_LastWarningSymbol);

        /* Special base functions */
        for (const auto &sname : s_special_symbol_names)
        {
            Symbol *symbol = Symbol::obtain(sname);
            symbol->m_is_special_symbol = true;
        }
    }

    Symbol *Symbol::missingArgument()
    {
#if CXXR_TRUE
        static GCRoot<Symbol> missing(GCNode::expose(createUnnamedSymbol()));
        return missing.get();
#else
        return s_missing_arg;
#endif
    }

    Symbol *Symbol::unboundValue()
    {
#if CXXR_FALSE
        static GCRoot<Symbol> unbound(GCNode::expose(createUnnamedSymbol()));
        return unbound.get();
#else
        return s_unbound_value;
#endif
    }

    Symbol *Symbol::restartToken()
    {
#if CXXR_TRUE
        static GCRoot<Symbol> restartTkn(GCNode::expose(createUnnamedSymbol()));
        return restartTkn.get();
#else
        return s_restart_token;
#endif
    }

    Symbol *Symbol::inBCInterpreter()
    {
#if CXXR_TRUE
        static GCRoot<Symbol> InBCInterpreter(GCNode::expose(new Symbol(CachedString::obtain("<in-bc-interp>"))));
        return InBCInterpreter.get();
#else
        return s_in_bc_interpreter;
#endif
    }

    Symbol *Symbol::currentExpression()
    {
#if CXXR_TRUE
        static GCRoot<Symbol> CurrentExpression(GCNode::expose(new Symbol(CachedString::obtain("<current-expression>"))));
        return CurrentExpression.get();
#else
        return s_current_expression;
#endif
    }

    Symbol::Table *Symbol::getTable()
    {
        static Table *table = new Table();
        return table;
    }

    Symbol *Symbol::make(const CachedString *name)
    {
        if (name->size() == 0)
        {
            Rf_error(_("attempt to use zero-length variable name"));
        }

        {
            /* Even though CachedString uses string + encoding pairs to cache strings
             * CR's Rf_install and Rf_installNoTrChar use only string part of the key
             * to obtain the corresponding cached string.
             *
             * Below logic replicates such behaviour, i.e. it tries to find corresponding string
             * regardless of the encoding.
             */
            const cetype_t All[] = {CE_NATIVE, CE_UTF8, CE_LATIN1, CE_BYTES, CE_SYMBOL, CE_ANY};
            for (const auto enc : All)
            {
                auto resp = CachedString::findInCache(name->stdstring(), enc);
                if (resp)
                {
                    auto search = getTable()->find(resp);
                    if (search != getTable()->end())
                        return search->second;
                }
            }
        }

        std::pair<Table::iterator, bool> pr = getTable()->insert(Table::value_type(name, nullptr));
        Table::iterator it = pr.first;
        Table::value_type &val = *it;
        if (pr.second)
        {
            try
            {
                val.second = GCNode::expose(new Symbol(name));
                name->m_symbol = val.second;
            }
            catch (...)
            {
                name->m_symbol = nullptr;
                getTable()->erase(it);
                throw;
            }
        }
        return val.second;
    }

    Symbol *Symbol::obtain(const std::string &name)
    {
        return Symbol::obtainCE(name, CE_NATIVE);
    }

    Symbol *Symbol::obtainCE(const std::string &name, cetype_t enc)
    {
        GCStackRoot<const CachedString> str(CachedString::obtain(name, enc));
        return Symbol::obtain(str);
    }

    Symbol *Symbol::obtainS3Signature(const char *className,
                                      const char *methodName)
    {
        assert(className != nullptr);
        assert(methodName != nullptr);

        std::string signature = std::string(className) + "." + std::string(methodName);
        constexpr int maxLength = 512;
        if (signature.length() >= maxLength)
            Rf_error(_("signature is too long in '%s'"), signature.c_str());
        return obtain(signature);
    }

    Symbol *Symbol::obtainDotDotSymbol(unsigned int n)
    {
        if (n < 0)
            Rf_error(_("..n symbol name for a negative n is not permitted"));
        const std::string ddval = ".." + std::to_string(n);
        GCStackRoot<const CachedString> name(CachedString::obtain(ddval));
        return obtain(name);
    }

    bool isDotSymbol(const Symbol *symbol)
    {
        return symbol && symbol->name()->c_str()[0] == '.';
    }

    void Symbol::checkST(const RObject *x)
    {
#ifdef ENABLE_ST_CHECKS
        switch (x->sexptype())
        {
        case SYMSXP:
            break;
        default:
            std::cerr << "Inappropriate SEXPTYPE (" << x->sexptype() << ") for Symbol." << std::endl;
            abort();
        }
#endif
    }

    void Symbol::visitTable(const_visitor *v)
    {
        for (Table::iterator it = getTable()->begin(); it != getTable()->end(); ++it)
        {
            // Beware that a garbage collection may occur in
            // Symbol::obtain(), after a new entry has been placed in the
            // symbol table but not yet made to point to a Symbol.  In
            // that case we need to visit the table key (i.e. the symbol
            // name); otherwise we don't bother, because it will be
            // reached via the Symbol anyway.
            const Symbol *symbol = (*it).second;
            if (symbol)
            {
                symbol->conductVisitor(v);
                if (symbol->attributes())
                    GCManager::gc_error("****found a symbol with attributes\n");
            }
            else
                (*it).first->conductVisitor(v);
        }
    }
} // namespace CXXR

// ***** C interface *****

/* Symbol Table Shortcuts */
#define PREDEFINED_SYMBOL(C_NAME, CXXR_NAME, R_NAME) \
    SEXP C_NAME = nullptr;
#include <CXXR/PredefinedSymbols.hpp>
#undef PREDEFINED_SYMBOL

SEXP R_MissingArg = nullptr;
SEXP R_RestartToken = nullptr;
SEXP R_UnboundValue = nullptr;
SEXP R_CurrentExpression = nullptr;
SEXP R_InBCInterpreter = nullptr;

SEXP PRINTNAME(SEXP x)
{
    if (!x)
        return nullptr;
    Symbol::checkST(x);
    const Symbol *sym = SEXP_downcast<const Symbol *>(x);
    return const_cast<CachedString *>(sym->name());
}

SEXP SYMVALUE(SEXP x)
{
    if (!x)
        return nullptr;
    Symbol::checkST(x);
    Symbol *sym = SEXP_downcast<Symbol *>(x);
    return sym->value();
}

SEXP INTERNAL(SEXP x)
{
    if (!x)
        return nullptr;
    Symbol::checkST(x);
    const Symbol *sym = SEXP_downcast<const Symbol *>(x);
    return const_cast<BuiltInFunction *>(sym->internalFunction());
}

int DDVAL(SEXP x)
{
    return x ? SEXP_downcast<const Symbol *>(x)->isDotDotSymbol() : false;
}

void SET_PRINTNAME(SEXP x, SEXP v)
{
    if (!x)
        return;
    Symbol::checkST(x);
}

void SET_SYMVALUE(SEXP x, SEXP v)
{
    if (!x)
        return;
    Symbol::checkST(x);
    if (SEXP_downcast<Symbol *>(x)->value() == v)
        return;
    Symbol *sym = SEXP_downcast<Symbol *>(x);
    sym->setValue(v);
}

void SET_INTERNAL(SEXP x, SEXP v)
{
    if (!x)
        return;
    Symbol::checkST(x);
    Symbol *sym = SEXP_downcast<Symbol *>(x);
    BuiltInFunction *fun = SEXP_downcast<BuiltInFunction *>(v);
    sym->setInternalFunction(fun);
}

void SET_DDVAL(SEXP x, int v)
{
}

void SET_BASE_SYM_CACHED(SEXP b)
{
    if (!b)
        return;
    SEXP_downcast<Symbol *>(b)->setBaseSymbol(true);
}

void UNSET_BASE_SYM_CACHED(SEXP b)
{
    if (!b)
        return;
    SEXP_downcast<Symbol *>(b)->setBaseSymbol(false);
}

Rboolean BASE_SYM_CACHED(SEXP b)
{
    return Rboolean(b && SEXP_downcast<const Symbol *>(b)->baseSymbol());
}

void SET_SPECIAL_SYMBOL(SEXP b)
{
    if (!b)
        return;
    SEXP_downcast<Symbol *>(b)->setSpecialSymbol(true);
}

void UNSET_SPECIAL_SYMBOL(SEXP b)
{
    if (!b)
        return;
    SEXP_downcast<Symbol *>(b)->setSpecialSymbol(false);
}

Rboolean IS_SPECIAL_SYMBOL(SEXP b)
{
    return Rboolean(b && SEXP_downcast<const Symbol *>(b)->isSpecialSymbol());
}

Rboolean Rf_isUserBinop(SEXP s)
{
    if (TYPEOF(s) == SYMSXP)
    {
        const char *str = SEXP_downcast<Symbol *>(s)->name()->c_str();
        if (strlen(str) >= 2 && str[0] == '%' && str[strlen(str) - 1] == '%')
            return TRUE;
    }
    return FALSE;
}
