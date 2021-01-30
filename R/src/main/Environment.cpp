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

/** @file Environment.cpp
 *
 *
 * @brief Implementation of class rho:Environment and associated C
 * interface.
 */

#include <CXXR/Environment.hpp>
#include <Rinternals.h>

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
        const auto &SET_ENV_RDEBUGptr = SET_ENV_RDEBUG;
        const auto &SET_HASHTABptr = SET_HASHTAB;
    } // namespace ForceNonInline

    namespace
    {
        // Used in {,un}packGPBits():
        constexpr unsigned int FRAME_LOCK_MASK = 1 << 14;
        constexpr unsigned int GLOBAL_FRAME_MASK = 1 << 15;
    } // namespace

    unsigned int Environment::packGPBits() const
    {
        unsigned int ans = RObject::packGPBits();
        if (m_locked)
            ans |= FRAME_LOCK_MASK;
        if (m_globally_cached)
            ans |= GLOBAL_FRAME_MASK;
        return ans;
    }

    void Environment::unpackGPBits(unsigned int gpbits)
    {
        RObject::unpackGPBits(gpbits);
        // Be careful with precedence!
        m_locked = ((gpbits & FRAME_LOCK_MASK) != 0);
        m_globally_cached = ((gpbits & GLOBAL_FRAME_MASK) != 0);
    }

    GCRoot<Environment> Environment::s_empty_env(new Environment(), true);
    GCRoot<Environment> Environment::s_base_env(new Environment(s_empty_env), true);
    GCRoot<Environment> Environment::s_global_env(SEXP_downcast<Environment *>(R::R_NewHashedEnv(s_base_env, Rf_ScalarInteger(0))), true);

    void Environment::initialize()
    {
        // R_EmptyEnv = empty();
        R_EmptyEnv = const_cast<Environment *>(Environment::emptyEnvironment());
        // R_BaseEnv = base();
        R_BaseEnv = Environment::base();

        // base()->setOnSearchPath(true);
        // R_GlobalEnv = global();
        R_GlobalEnv = Environment::global();

        // global()->setOnSearchPath(true);

        // R_BaseNamespace = baseNamespace();
    }

    const char *Environment::typeName() const
    {
        return staticTypeName();
    }

    void Environment::visitChildren(const_visitor *v) const
    {
        RObject::visitChildren(v);
        if (m_enclosing)
            m_enclosing->conductVisitor(v);
        if (m_frame)
            m_frame->conductVisitor(v);
        if (m_hashtable)
            m_hashtable->conductVisitor(v);
    }

    RObject *Environment::frame(RObject *x)
    {
        if (!x)
            return nullptr;
#ifdef ENABLE_ST_CHECKS
        switch (x->sexptype())
        {
        case ENVSXP:
            break;
        default:
            std::cerr << LOCATION << "Inappropriate SEXPTYPE (" << x->sexptype() << ") for Environment." << std::endl;
            abort();
        }
#endif
        Environment *env = SEXP_downcast<Environment *>(x);
        return env->frame();
    }

    RObject *Environment::enclos(RObject *x)
    {
        if (!x)
            return nullptr;
#ifdef ENABLE_ST_CHECKS
        switch (x->sexptype())
        {
        case ENVSXP:
            break;
        default:
            std::cerr << LOCATION << "Inappropriate SEXPTYPE (" << x->sexptype() << ") for PairList." << std::endl;
            abort();
        }
#endif
        const Environment *env = SEXP_downcast<Environment *>(x);
        return env->enclosingEnvironment();
    }

    RObject *Environment::hashtab(RObject *x)
    {
        if (!x)
            return nullptr;
#ifdef ENABLE_ST_CHECKS
        switch (x->sexptype())
        {
        case ENVSXP:
            break;
        default:
            std::cerr << LOCATION << "Inappropriate SEXPTYPE (" << x->sexptype() << ") for Environment." << std::endl;
            abort();
        }
#endif
        Environment *env = SEXP_downcast<Environment *>(x);
        return env->hashTable();
    }

    unsigned int Environment::envflags(RObject *x) /* for environments */
    {
        if (!x)
            return 0;

        return SEXP_downcast<Environment *>(x)->packGPBits();
    }

    void Environment::set_envflags(RObject *x, unsigned int v)
    {
        if (!x)
            return;
        x->m_gpbits = v;
        SEXP_downcast<Environment *>(x)->unpackGPBits(v);
    }

    void Environment::set_frame(RObject *x, RObject *v)
    {
        if (!x)
            return;
#ifdef ENABLE_ST_CHECKS
        switch (x->sexptype())
        {
        case ENVSXP:
            break;
        default:
            std::cerr << LOCATION << "Inappropriate SEXPTYPE (" << x->sexptype() << ") for Environment." << std::endl;
            abort();
        }
#endif
        Environment *env = SEXP_downcast<Environment *>(x);
        PairList *pl = SEXP_downcast<PairList *>(v);
        env->setFrame(pl);
    }

    void Environment::set_enclos(RObject *x, RObject *v)
    {
        if (!x)
            return;
#ifdef ENABLE_ST_CHECKS
        switch (x->sexptype())
        {
        case ENVSXP:
            break;
        default:
            std::cerr << LOCATION << "Inappropriate SEXPTYPE (" << x->sexptype() << ") for Environment." << std::endl;
            abort();
        }
#endif
        Environment *env = SEXP_downcast<Environment *>(x);
        Environment *enc = SEXP_downcast<Environment *>(v);
        env->setEnclosingEnvironment(enc);
    }

    void Environment::set_hashtab(RObject *x, RObject *v)
    {
        if (!x)
            return;
#ifdef ENABLE_ST_CHECKS
        switch (x->sexptype())
        {
        case ENVSXP:
            break;
        default:
            std::cerr << LOCATION << "Inappropriate SEXPTYPE (" << x->sexptype() << ") for Environment." << std::endl;
            abort();
        }
#endif
        Environment *env = SEXP_downcast<Environment *>(x);
        ListVector *lv = SEXP_downcast<ListVector *>(v);
        env->setHashTable(lv);
    }

    unsigned int Environment::frame_is_locked(RObject *e)
    {
        return e && SEXP_downcast<Environment *>(e)->isLocked();
    }

    void Environment::lock_frame(RObject *x)
    {
        if (!x)
            return;
        x->m_gpbits |= FRAME_LOCK_MASK;
        SEXP_downcast<Environment *>(x)->setLocking(true);
    }

    bool Environment::is_global_frame(RObject *e)
    {
        return e && SEXP_downcast<Environment *>(e)->inGlobalCache();
    }

    void Environment::mark_as_global_frame(RObject *x)
    {
        if (!x)
            return;
        x->m_gpbits |= GLOBAL_FRAME_MASK;
        SEXP_downcast<Environment *>(x)->setGlobalCaching(true);
    }
    void Environment::mark_as_local_frame(RObject *x)
    {
        if (!x)
            return;
        x->m_gpbits &= ~(GLOBAL_FRAME_MASK);
        SEXP_downcast<Environment *>(x)->setGlobalCaching(false);
    }

    bool Environment::env_rdebug(RObject *x)
    {
        const Environment *env = SEXP_downcast<const Environment *>(x);
        return env->singleStepping();
    }

    void Environment::set_env_rdebug(RObject *x, bool v)
    {
        if (!x)
            return;
        Environment *env = SEXP_downcast<Environment *>(x);
        env->setSingleStepping(v);
    }
} // namespace CXXR
