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

/** @file Closure.cpp
 *
 * @brief Implementation of class Closure and associated C
 * interface.
 */

#include <CXXR/Closure.hpp>
#include <Rinternals.h>

namespace CXXR
{
    // Force the creation of non-inline embodiments of functions callable
    // from C:
    namespace ForceNonInline
    {
        const auto &BODYptr = BODY;
        const auto &CLOENVptr = CLOENV;
        const auto &FORMALSptr = FORMALS;
        const auto &RSTEPptr = RSTEP;
        const auto &SET_CLOENVptr = SET_CLOENV;
        const auto &SET_RSTEPptr = SET_RSTEP;
    } // namespace ForceNonInline

    namespace
    {
        // Used in {,un}packGPBits():
        constexpr unsigned int NOJIT_MASK = 1 << 5;
        constexpr unsigned int MAYBEJIT_MASK = 1 << 6;
    } // namespace

    unsigned int Closure::packGPBits() const
    {
        unsigned int ans = RObject::packGPBits();
        if (m_no_jit)
            ans |= NOJIT_MASK;
        if (m_maybe_jit)
            ans |= MAYBEJIT_MASK;
        return ans;
    }

    void Closure::unpackGPBits(unsigned int gpbits)
    {
        RObject::unpackGPBits(gpbits);
        m_no_jit = ((gpbits & NOJIT_MASK) != 0);
        m_maybe_jit = ((gpbits & MAYBEJIT_MASK) != 0);
    }

    const char *Closure::typeName() const
    {
        return staticTypeName();
    }

    void Closure::visitChildren(const_visitor *v) const
    {
        RObject::visitChildren(v);
        if (m_formals)
            m_formals->conductVisitor(v);
        if (m_body)
            m_body->conductVisitor(v);
        if (m_environment)
            m_environment->conductVisitor(v);
    }

    RObject *Closure::formals(RObject *x)
    {
        if (!x)
            return nullptr;
#ifdef ENABLE_ST_CHECKS
        switch (x->sexptype())
        {
        case CLOSXP:
            break;
        default:
            std::cerr << LOCATION << "Inappropriate SEXPTYPE (" << x->sexptype() << ") for Closure." << std::endl;
            abort();
        }
#endif
        const Closure *clo = SEXP_downcast<Closure *>(x);
        return const_cast<PairList *>(clo->formalArgs());
    }

    void Closure::set_formals(RObject *x, RObject *v)
    {
        if (!x)
            return;
#ifdef ENABLE_ST_CHECKS
        switch (x->sexptype())
        {
        case CLOSXP:
            break;
        default:
            std::cerr << LOCATION << "Inappropriate SEXPTYPE (" << x->sexptype() << ") for Closure." << std::endl;
            abort();
        }
#endif
        Closure *clos = SEXP_downcast<Closure *>(x);
        PairList *formal_args = SEXP_downcast<PairList *>(v);
        clos->setFormalArgs(formal_args);
    }

    RObject *Closure::body(RObject *x)
    {
        if (!x)
            return nullptr;
#ifdef ENABLE_ST_CHECKS
        switch (x->sexptype())
        {
        case CLOSXP:
            break;
        default:
            std::cerr << LOCATION << "Inappropriate SEXPTYPE (" << x->sexptype() << ") for Closure." << std::endl;
            abort();
        }
#endif
        const Closure *clo = SEXP_downcast<Closure *>(x);
        return const_cast<RObject *>(clo->body());
    }

    void Closure::set_body(RObject *x, RObject *v)
    {
        if (!x)
            return;
#ifdef ENABLE_ST_CHECKS
        switch (x->sexptype())
        {
        case CLOSXP:
            break;
        default:
            std::cerr << LOCATION << "Inappropriate SEXPTYPE (" << x->sexptype() << ") for Closure." << std::endl;
            abort();
        }
#endif
        Closure *clos = SEXP_downcast<Closure *>(x);
        RObject *body = SEXP_downcast<RObject *>(v);
        clos->setBody(body);
    }

    RObject *Closure::cloenv(RObject *x)
    {
        if (!x)
            return nullptr;
#ifdef ENABLE_ST_CHECKS
        switch (x->sexptype())
        {
        case CLOSXP:
            break;
        default:
            std::cerr << LOCATION << "Inappropriate SEXPTYPE (" << x->sexptype() << ") for Closure." << std::endl;
            abort();
        }
#endif
        Closure *clo = SEXP_downcast<Closure *>(x);
        return clo->environment();
    }

    void Closure::set_cloenv(RObject *x, RObject *v)
    {
        if (!x)
            return;
#ifdef ENABLE_ST_CHECKS
        switch (x->sexptype())
        {
        case CLOSXP:
            break;
        default:
            std::cerr << LOCATION << "Inappropriate SEXPTYPE (" << x->sexptype() << ") for Closure." << std::endl;
            abort();
        }
#endif
        Closure *clos = SEXP_downcast<Closure *>(x);
        Environment *env = SEXP_downcast<Environment *>(v);
        clos->setEnvironment(env);
    }

    bool Closure::rstep(RObject *x)
    {
        return x && x->m_spare;
    }

    void Closure::set_rstep(RObject *x, bool v)
    {
        if (!x)
            return;
        x->m_spare = v;
    }

    /* JIT optimization support */

    unsigned int Closure::nojit(RObject *x)
    {
        if (!x)
            return 0;
        return SEXP_downcast<Closure *>(x)->m_no_jit;
    }

    void Closure::set_nojit(RObject *x)
    {
        if (!x)
            return;
        x->m_gpbits |= NOJIT_MASK;
        SEXP_downcast<Closure *>(x)->m_no_jit = true;
    }

    unsigned int Closure::maybejit(RObject *x)
    {
        if (!x)
            return 0;
        return SEXP_downcast<Closure *>(x)->m_maybe_jit;
    }

    void Closure::set_maybejit(RObject *x)
    {
        if (!x)
            return;
        x->m_gpbits |= MAYBEJIT_MASK;
        SEXP_downcast<Closure *>(x)->m_maybe_jit = true;
    }

    void Closure::unset_maybejit(RObject *x)
    {
        if (!x)
            return;
        x->m_gpbits &= ~MAYBEJIT_MASK;
        SEXP_downcast<Closure *>(x)->m_maybe_jit = false;
    }
} // namespace CXXR
