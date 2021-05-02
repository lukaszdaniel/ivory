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
 *  This program is free software; you can redistribute it and/or modify
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

/** @file Promise.cpp
 *
 * @brief Implementation of class Promise and associated C
 * interface.
 */

#include <CXXR/Promise.hpp>
#include <CXXR/Evaluator.hpp>
#include <RContext.h>
#include <Localization.h>
#include <Rinternals.h>

using namespace CXXR;

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

    struct RPRSTACK *R_PendingPromises = nullptr; /* Pending promise stack */

    void Promise::setValue(RObject *val)
    {
        // m_value = val;
        // m_value.propagateAge(this);
        m_value.retarget(this, val);
        if (val != Symbol::unboundValue())
            m_environment = nullptr;
    }

    void Promise::setEnvironment(Environment *val)
    {
        // m_environment = val;
        // m_environment.propagateAge(this);
        m_environment.retarget(this, val);
    }

    void Promise::setValueGenerator(RObject *val)
    {
        // m_valgen = val;
        // m_valgen.propagateAge(this);
        m_valgen.retarget(this, val);
    }

    const char *Promise::typeName() const
    {
        return staticTypeName();
    }

    void Promise::visitReferents(const_visitor *v) const
    {
        RObject::visitReferents(v);
        if (m_value)
            m_value->conductVisitor(v);
        if (m_valgen)
            m_valgen->conductVisitor(v);
        if (m_environment)
            m_environment->conductVisitor(v);
    }

    RObject *Promise::evaluate(Environment * /*env*/)
    {
        if (m_value == Symbol::unboundValue())
        {
            RPRSTACK prstack;

            if (m_status == UNDER_EVALUATION)
                Rf_errorcall(R_GlobalContext->getCall(),
                             _("promise is already under evaluation: recursive default argument reference or earlier problems?"));
            else if (m_status == INTERRUPTED)
            {
                /* set PRSEEN to UNDER_EVALUATION to avoid infinite recursion */
                m_status = UNDER_EVALUATION;
                warningcall(R_GlobalContext->getCall(),
                            _("restarting interrupted promise evaluation"));
            }

            /* Mark the promise as under evaluation and push it on a stack
               that can be used to unmark pending promises if a jump out
               of the evaluation occurs. */
            m_status = UNDER_EVALUATION;
            try
            {
                prstack.promise = this;
                prstack.next = R_PendingPromises;
                R_PendingPromises = &prstack;

                RObject *val = Evaluator::evaluate(const_cast<RObject *>(valueGenerator()), environment());

                /* Pop the stack, unmark the promise and set its value field.
               Also set the environment to R_NilValue to allow GC to
               reclaim the promise environment; this is also useful for
               fancy games with delayedAssign() */
                R_PendingPromises = prstack.next;
                setValue(val);
                ENSURE_NAMEDMAX(val);
                setEnvironment(nullptr);
            }
            catch (...)
            {
                m_status = INTERRUPTED;
                throw;
            }
            m_status = DEFAULT;
        }
        return const_cast<RObject *>(value());
    }

    void Promise::checkST(const RObject *x)
    {
#ifdef ENABLE_ST_CHECKS
        switch (x->sexptype())
        {
        case PROMSXP:
        case SYMSXP:
            break;
        default:
            std::cerr << "Inappropriate SEXPTYPE (" << x->sexptype() << ") for Promise." << std::endl;
            abort();
        }
#endif
    }
} // namespace CXXR

// ***** C interface *****

SEXP PRCODE(SEXP x)
{
    if (!x)
        return nullptr;
    Promise::checkST(x);
    const Promise *prom = SEXP_downcast<const Promise *>(x);
    return const_cast<RObject *>(prom->valueGenerator());
}

SEXP PRENV(SEXP x)
{
    if (!x)
        return nullptr;
    Promise::checkST(x);
    const Promise *prom = SEXP_downcast<const Promise *>(x);
    return const_cast<Environment *>(prom->environment());
}

SEXP PRVALUE(SEXP x)
{
    if (!x)
        return nullptr;
    Promise::checkST(x);
    const Promise *prom = SEXP_downcast<const Promise *>(x);
    return const_cast<RObject *>(prom->value());
}

int PRSEEN(SEXP x)
{
    if (!x)
        return Promise::EvaluationStatus::DEFAULT;
    const Promise *prom = SEXP_downcast<const Promise *>(x);
    return prom->status();
}

void SET_PRENV(SEXP x, SEXP v)
{
    if (!x)
        return;
    Promise::checkST(x);
    Promise *prom = SEXP_downcast<Promise *>(x);
    prom->setEnvironment(SEXP_downcast<Environment *>(v));
}

void SET_PRVALUE(SEXP x, SEXP v)
{
    if (!x)
        return;
    Promise::checkST(x);
    Promise *prom = SEXP_downcast<Promise *>(x);
    prom->setValue(v);
}

void SET_PRCODE(SEXP x, SEXP v)
{
    if (!x)
        return;
    Promise::checkST(x);
    Promise *prom = SEXP_downcast<Promise *>(x);
    prom->setValueGenerator(v);
}

void SET_PRSEEN(SEXP x, int v)
{
    if (!x)
        return;
    if (!(v == Promise::EvaluationStatus::DEFAULT ||
          v == Promise::EvaluationStatus::UNDER_EVALUATION ||
          v == Promise::EvaluationStatus::INTERRUPTED))
    {
        std::cerr << "Incorrect EvaluationStatus = " << v << std::endl;
        abort();
    }
    Promise *prom = SEXP_downcast<Promise *>(x);
    prom->setStatus(Promise::EvaluationStatus(v));
}
