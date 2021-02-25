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

/** @file Promise.cpp
 *
 * @brief Implementation of class Promise and associated C
 * interface.
 */

#include <CXXR/Promise.hpp>
#include <Rinternals.h>

namespace CXXR
{
    // Force the creation of non-inline embodiments of functions callable
    // from C:
    namespace ForceNonInline
    {
        const auto &PRCODEptr = PRCODE;
        const auto &PRENVptr = PRENV;
        const auto &PRSEENptr = PRSEEN;
        const auto &PRVALUEptr = PRVALUE;
        const auto &SETPRSEENptr = SET_PRSEEN;
        const auto &SET_PRVALUEptr = SET_PRVALUE;
    } // namespace ForceNonInline

    void Promise::setValue(RObject *val)
    {
        m_value = val;
        propagateAge(m_value);
        if (val != Symbol::unboundValue())
            m_environment = nullptr;
    }

    void Promise::setEnvironment(Environment *val)
    {
        m_environment = val;
        propagateAge(m_environment);
    }

    void Promise::setValueGenerator(RObject *val)
    {
        m_valgen = val;
        propagateAge(m_valgen);
    }

    const char *Promise::typeName() const
    {
        return staticTypeName();
    }

    void Promise::visitChildren(const_visitor *v) const
    {
        RObject::visitChildren(v);
        if (m_value)
            m_value->conductVisitor(v);
        if (m_valgen)
            m_valgen->conductVisitor(v);
        if (m_environment)
            m_environment->conductVisitor(v);
    }

    RObject *Promise::prcode(RObject *x)
    {
        if (!x)
            return nullptr;
#ifdef ENABLE_ST_CHECKS
        switch (x->sexptype())
        {
        case PROMSXP:
        case SYMSXP:
            break;
        default:
            std::cerr << LOCATION << "Inappropriate SEXPTYPE (" << x->sexptype() << ") for Promise." << std::endl;
            abort();
        }
#endif
        const Promise *prom = SEXP_downcast<Promise *>(x);
        return const_cast<RObject *>(prom->valueGenerator());
    }

    void Promise::set_prcode(RObject *x, RObject *v)
    {
        if (!x)
            return;
#ifdef ENABLE_ST_CHECKS
        switch (x->sexptype())
        {
        case PROMSXP:
        case SYMSXP:
            break;
        default:
            std::cerr << LOCATION << "Inappropriate SEXPTYPE (" << x->sexptype() << ") for Promise." << std::endl;
            abort();
        }
#endif
        Promise *prom = SEXP_downcast<Promise *>(x);
        prom->setValueGenerator(v);
    }

    RObject *Promise::prenv(RObject *x)
    {
        if (!x)
            return nullptr;
#ifdef ENABLE_ST_CHECKS
        switch (x->sexptype())
        {
        case PROMSXP:
        case SYMSXP:
            break;
        default:
            std::cerr << LOCATION << "Inappropriate SEXPTYPE (" << x->sexptype() << ") for Promise." << std::endl;
            abort();
        }
#endif
        const Promise *prom = SEXP_downcast<Promise *>(x);
        return const_cast<Environment *>(prom->environment());
    }

    RObject *Promise::prvalue(RObject *x)
    {
        if (!x)
            return nullptr;
#ifdef ENABLE_ST_CHECKS
        switch (x->sexptype())
        {
        case PROMSXP:
        case SYMSXP:
            break;
        default:
            std::cerr << LOCATION << "Inappropriate SEXPTYPE (" << x->sexptype() << ") for Promise." << std::endl;
            abort();
        }
#endif
        const Promise *prom = SEXP_downcast<Promise *>(x);
        return const_cast<RObject *>(prom->value());
    }

    void Promise::set_prvalue(RObject *x, RObject *v)
    {
        if (!x)
            return;
#ifdef ENABLE_ST_CHECKS
        switch (x->sexptype())
        {
        case PROMSXP:
        case SYMSXP:
            break;
        default:
            std::cerr << LOCATION << "Inappropriate SEXPTYPE (" << x->sexptype() << ") for Promise." << std::endl;
            abort();
        }
#endif
        Promise *prom = SEXP_downcast<Promise *>(x);
        prom->setValue(v);
    }

    unsigned int Promise::prseen(RObject *x)
    {
        if (!x)
            return 0;
        Promise *prom = SEXP_downcast<Promise *>(x);
        if (prom->evaluationInterrupted())
        {
            return 2;
        }
        else if (prom->underEvaluation())
        {
            return 1;
        }
        return 0;
    }

    void Promise::set_prenv(RObject *x, RObject *v)
    {
        if (!x)
            return;
#ifdef ENABLE_ST_CHECKS
        switch (x->sexptype())
        {
        case PROMSXP:
        case SYMSXP:
            break;
        default:
            std::cerr << LOCATION << "Inappropriate SEXPTYPE (" << x->sexptype() << ") for Promise." << std::endl;
            abort();
        }
#endif
        Promise *prom = SEXP_downcast<Promise *>(x);
        prom->setEnvironment(SEXP_downcast<Environment *>(v));
    }

    void Promise::set_prseen(RObject *x, unsigned int v)
    {
        if (!x)
            return;
        Promise *prom = SEXP_downcast<Promise *>(x);
        if (v == 1)
        {
            prom->markUnderEvaluation(true);
            prom->markEvaluationInterrupted(false);
        }
        else if (v)
        {
            prom->markUnderEvaluation(false);
            prom->markEvaluationInterrupted(true);
        }
        else
        {
            prom->markUnderEvaluation(false);
            prom->markEvaluationInterrupted(false);
        }
    }
} // namespace CXXR

// ***** C interface *****

SEXP PRCODE(SEXP x)
{
    return CXXR::Promise::prcode(x);
}

SEXP PRENV(SEXP x)
{
    return CXXR::Promise::prenv(x);
}

SEXP PRVALUE(SEXP x)
{
    return CXXR::Promise::prvalue(x);
}

int PRSEEN(SEXP x)
{
    return CXXR::Promise::prseen(x);
}

void SET_PRENV(SEXP x, SEXP v)
{
    CXXR::RObject::fix_refcnt(x, CXXR::Promise::prenv(x), v);
    CXXR::Promise::set_prenv(x, v);
}

void SET_PRVALUE(SEXP x, SEXP v)
{
    CXXR::RObject::fix_refcnt(x, CXXR::Promise::prvalue(x), v);
    CXXR::Promise::set_prvalue(x, v);
}

void SET_PRCODE(SEXP x, SEXP v)
{
    CXXR::RObject::fix_refcnt(x, CXXR::Promise::prcode(x), v);
    CXXR::Promise::set_prcode(x, v);
}

void SET_PRSEEN(SEXP x, int v)
{
    CXXR::Promise::set_prseen(x, v);
}