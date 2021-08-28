/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999--2020  The R Core Team.
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
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

/** @file Environment.cpp
 *
 *
 * @brief Implementation of class rho:Environment and associated C
 * interface.
 */

#include <CXXR/Environment.hpp>
#include <CXXR/IntVector.hpp>
#include <CXXR/Symbol.hpp>
#include <Rinternals.h>

#include <typeinfo>

using namespace std;
using namespace CXXR;

namespace R
{
    SEXP R_NewHashedEnv(SEXP enclos, SEXP size);
}

namespace CXXR
{
    // Force the creation of non-inline embodiments of functions callable
    // from C:
    namespace ForceNonInline
    {
        const auto &ENCLOSptr = ENCLOS;
        const auto &ENVFLAGSptr = ENVFLAGS;
        const auto &HASHTABptr = HASHTAB;
        const auto &isEnvironmentptr = Rf_isEnvironment;
        const auto &FRAMEptr = FRAME;
        const auto &ENV_RDEBUGptr = ENV_RDEBUG;
        const auto &SET_ENCLOSptr = SET_ENCLOS;
        const auto &SET_ENVFLAGSptr = SET_ENVFLAGS;
        const auto &SET_FRAMEp = SET_FRAME;
        const auto &SET_ENV_RDEBUGptr = R::SET_ENV_RDEBUG;
        const auto &SET_HASHTABptr = SET_HASHTAB;
    } // namespace ForceNonInline

    namespace
    {
        // Used in {,un}packGPBits():
        constexpr unsigned int FRAME_LOCK_MASK = 1 << 14;
    } // namespace

    unsigned int Environment::packGPBits() const
    {
        unsigned int ans = RObject::packGPBits();
        if (m_locked)
            ans |= FRAME_LOCK_MASK;
        return ans;
    }

    void Environment::unpackGPBits(unsigned int gpbits)
    {
        RObject::unpackGPBits(gpbits);
        // Be careful with precedence!
        m_locked = ((gpbits & FRAME_LOCK_MASK) != 0);
    }

    GCRoot<Environment> Environment::s_empty_env(GCNode::expose(new Environment()));
    GCRoot<Environment> Environment::s_base_env(GCNode::expose(new Environment(s_empty_env)));
    GCRoot<Environment> Environment::s_global_env(GCNode::expose(SEXP_downcast<Environment *>(R::R_NewHashedEnv(s_base_env, Rf_ScalarInteger(0)))));
    GCRoot<Environment> Environment::s_base_namespace(GCNode::expose(new Environment(s_global_env)));

    void Environment::initialize()
    {
        R_EmptyEnv = empty();
        R_BaseEnv = base();

        // base()->setOnSearchPath(true);
        R_GlobalEnv = global();

        // global()->setOnSearchPath(true);

        R_BaseNamespace = baseNamespace();
    }

    const char *Environment::typeName() const
    {
        return staticTypeName();
    }

    // Environment::findNamespace() is in envir.cpp

    // Environment::findPackage() is in envir.cpp

    // Environment::namespaceSpec() is in envir.cpp

    void Environment::visitReferents(const_visitor *v) const
    {
        RObject::visitReferents(v);
        if (m_enclosing)
            m_enclosing->conductVisitor(v);
        if (m_frame)
            m_frame->conductVisitor(v);
    }

    void Environment::nullEnvironmentError()
    {
        Rf_error(_("use of NULL environment is defunct"));
    }

    void Environment::checkST(const RObject *x)
    {
#ifdef ENABLE_ST_CHECKS
        switch (x->sexptype())
        {
        case ENVSXP:
            break;
        default:
            std::cerr << "Inappropriate SEXPTYPE (" << x->sexptype() << ") for Environment." << std::endl;
            abort();
        }
#endif
    }

    // Utility intended to be called from a debugger.  Prints out the
    // names of the Symbols in an Environment, together with the addresses
    // the Symbols are bound to.

    void LS(RObject *s)
    {
#if CXXR_TRUE
        std::cerr << "LS(...) not yet implemented" << std::endl;
        abort();
#else
        const Environment *env = SEXP_downcast<Environment *>(s);
        const Frame *frame = env->frame();
        vector<const Symbol *> syms = frame->symbols(true);
        for (const auto &sym : syms)
        {
            const RObject *val = frame->binding(sym)->rawValue();
            cout << "\"" << sym->name()->stdstring() << "\" (\'RObject\'*)" << val;
            if (val)
                cout << " [" << typeid(*val).name() << "]";
            cout << "\n";
        }
#endif
    }
} // namespace CXXR

// ***** C interface *****

SEXP R_EmptyEnv = nullptr;
SEXP R_BaseEnv = nullptr;
SEXP R_GlobalEnv = nullptr;

Rboolean Rf_isEnvironment(SEXP s)
{
    return Rboolean(s && TYPEOF(s) == ENVSXP);
}

SEXP FRAME(SEXP x)
{
    if (!x)
        return nullptr;
    Environment::checkST(x);
    Environment *env = SEXP_downcast<Environment *>(x);
    return env->frame();
}

SEXP ENCLOS(SEXP x)
{
    if (!x)
        return nullptr;
    Environment::checkST(x);
    const Environment *env = SEXP_downcast<const Environment *>(x);
    return env->enclosingEnvironment();
}

SEXP HASHTAB(SEXP x)
{
    return nullptr;
}

int ENVFLAGS(SEXP x)
{
    if (!x)
        return 0;

    return SEXP_downcast<const Environment *>(x)->packGPBits();
}

int ENV_RDEBUG(SEXP x)
{
    if (!x)
        return 0;
    const Environment *env = SEXP_downcast<const Environment *>(x);
    return env->singleStepping();
}

void SET_FRAME(SEXP x, SEXP v)
{
    if (!x)
        return;
    Environment::checkST(x);
    Environment *env = SEXP_downcast<Environment *>(x);
    PairList *pl = SEXP_downcast<PairList *>(v);
    env->setFrame(pl);
}

void SET_ENCLOS(SEXP x, SEXP v)
{
    if (!x)
        return;
    Environment::checkST(x);
    Environment *env = SEXP_downcast<Environment *>(x);
    Environment *enc = SEXP_downcast<Environment *>(v);
    env->setEnclosingEnvironment(enc);
}

void SET_HASHTAB(SEXP x, SEXP v)
{
}

void SET_ENVFLAGS(SEXP x, int v)
{
    if (!x)
        return;
    SEXP_downcast<Environment *>(x)->unpackGPBits(v);
}

void R::SET_ENV_RDEBUG(SEXP x, int v)
{
    if (!x)
        return;
    Environment *env = SEXP_downcast<Environment *>(x);
    env->setSingleStepping(v);
}

void SET_NO_SPECIAL_SYMBOLS(SEXP b)
{
    if (!b)
        return;
    SEXP_downcast<Environment *>(b)->setNoSpecialSymbols(true);
}

void UNSET_NO_SPECIAL_SYMBOLS(SEXP b)
{
    if (!b)
        return;
    SEXP_downcast<Environment *>(b)->setNoSpecialSymbols(false);
}

Rboolean NO_SPECIAL_SYMBOLS(SEXP b)
{
    return Rboolean(b && SEXP_downcast<Environment *>(b)->noSpecialSymbols());
}
