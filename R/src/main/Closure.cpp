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

/** @file Closure.cpp
 *
 * @brief Implementation of class Closure and associated C
 * interface.
 */

#include <CXXR/Closure.hpp>
#include <Rinternals.h>

using namespace CXXR;

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

    Closure::Closure(const PairList *formal_args, const RObject *body,
                     Environment *env)
        : FunctionBase(CLOSXP), m_formals(formal_args), m_body(body),
          m_environment(env ? env : Environment::global()), m_no_jit(false), m_maybe_jit(false)
    {
        if (body)
        {
            switch (body->sexptype())
            {
            case CLOSXP:
            case BUILTINSXP:
            case SPECIALSXP:
            case DOTSXP:
            case ANYSXP:
                Rf_error(_("invalid body argument for 'function'\nShould NEVER happen; please bug.report() [mkCLOSXP]"));
                break;
            default:
                break;
            }
        }
        if (m_formals)
            const_cast<PairList *>(m_formals)->incrementRefCount();
        if (m_body)
            const_cast<RObject *>(m_body)->incrementRefCount();
        if (m_environment)
            m_environment->incrementRefCount();
    }

    Closure *Closure::clone(bool deep) const
    {
        // return GCNode::expose(new Closure(*this, deep));
        return new Closure(*this, deep);
    }

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

    void Closure::visitReferents(const_visitor *v) const
    {
        RObject::visitReferents(v);
        if (m_formals)
            m_formals->conductVisitor(v);
        if (m_body)
            m_body->conductVisitor(v);
        if (m_environment)
            m_environment->conductVisitor(v);
    }

    void Closure::checkST(const RObject *x)
    {
#ifdef ENABLE_ST_CHECKS
        switch (x->sexptype())
        {
        case CLOSXP:
            break;
        default:
            std::cerr << "Inappropriate SEXPTYPE (" << x->sexptype() << ") for Closure." << std::endl;
            abort();
        }
#endif
    }
} // namespace CXXR

// ***** C interface *****

SEXP FORMALS(SEXP x)
{
    if (!x)
        return nullptr;
    Closure::checkST(x);
    const Closure *clo = SEXP_downcast<const Closure *>(x);
    return const_cast<PairList *>(clo->formalArgs());
}

SEXP BODY(SEXP x)
{
    if (!x)
        return nullptr;
    Closure::checkST(x);
    const Closure *clo = SEXP_downcast<const Closure *>(x);
    return const_cast<RObject *>(clo->body());
}

SEXP CLOENV(SEXP x)
{
    if (!x)
        return nullptr;
    Closure::checkST(x);
    const Closure *clo = SEXP_downcast<const Closure *>(x);
    return clo->environment();
}

int RSTEP(SEXP x)
{
    return x && x->rstep();
}

void SET_FORMALS(SEXP x, SEXP v)
{
    if (!x)
        return;
    Closure::checkST(x);
    Closure *clos = SEXP_downcast<Closure *>(x);
    PairList *formal_args = SEXP_downcast<PairList *>(v);
    clos->setFormalArgs(formal_args);
}

void SET_BODY(SEXP x, SEXP v)
{
    if (!x)
        return;
    Closure::checkST(x);
    Closure *clos = SEXP_downcast<Closure *>(x);
    RObject *body = SEXP_downcast<RObject *>(v);
    clos->setBody(body);
}

void SET_CLOENV(SEXP x, SEXP v)
{
    if (!x)
        return;
    Closure::checkST(x);
    Closure *clos = SEXP_downcast<Closure *>(x);
    Environment *env = SEXP_downcast<Environment *>(v);
    clos->setEnvironment(env);
}

void SET_RSTEP(SEXP x, int v)
{
    if (!x)
        return;
    x->setRstep(v);
}

int NOJIT(SEXP x)
{
    if (!x)
        return 0;
    return SEXP_downcast<const Closure *>(x)->noJIT();
}

int MAYBEJIT(SEXP x)
{
    if (!x)
        return 0;
    return SEXP_downcast<const Closure *>(x)->maybeJIT();
}

void SET_NOJIT(SEXP x)
{
    if (!x)
        return;
    SEXP_downcast<Closure *>(x)->setNoJIT();
}

void SET_MAYBEJIT(SEXP x)
{
    if (!x)
        return;
    SEXP_downcast<Closure *>(x)->setMaybeJIT(true);
}

void UNSET_MAYBEJIT(SEXP x)
{
    if (!x)
        return;
    SEXP_downcast<Closure *>(x)->setMaybeJIT(false);
}
