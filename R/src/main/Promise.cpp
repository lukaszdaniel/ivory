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
        const auto &PRVALUEptr = PRVALUE;
        const auto &PRSEENptr = PRSEEN;
    } // namespace ForceNonInline

    const char *Promise::typeName() const
    {
        return staticTypeName();
    }

    /* Promise Access Methods */
    /** @brief Access the expression of a CXXR::Promise.
     *
     * @param x Pointer to a CXXR::Promise (checked).
     *
     * @return Pointer to the expression to be evaluated by the
     *         CXXR::Promise. 
     */
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
        return x->u.promsxp.m_expr;
    }

    /** @brief Set the expression of a CXXR::Promise.
     *
     * @param x Pointer to a CXXR::Promise (checked).
     *
     * @param v Pointer to the expression to be assigned to the CXXR::Promise.
     */
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
        x->u.promsxp.m_expr = v;
    }

    /** @brief Access the environment of a CXXR::Promise.
     *
     * @param x Pointer to a CXXR::Promise (checked).
     *
     * @return Pointer to the environment of the CXXR::Promise. 
     */
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
        return x->u.promsxp.m_env;
    }

    /** @brief Access the value of a CXXR::Promise.
     *
     * @param x Pointer to a CXXR::Promise (checked).
     *
     * @return Pointer to the value of the CXXR::Promise, or to
     *         R_UnboundValue if it has not yet been evaluated..
     */
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
        return x->u.promsxp.m_value;
    }

    /** @brief Set the value of a CXXR::Promise.
     *
     * Once the value is set to something other than R_UnboundValue,
     * the environment pointer is set null.
     *
     * @param x Pointer to a CXXR::Promise (checked).
     *
     * @param v Pointer to the value to be assigned to the CXXR::Promise.
     *
     * @todo Replace this with a method call to evaluate the CXXR::Promise.
     */
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
        x->u.promsxp.m_value = v;
    }

    unsigned int Promise::prseen(RObject *x) { return x ? x->m_gpbits : 0; }

    /** @brief Set the environment of a CXXR::Promise.
     *
     * @param x Pointer to a CXXR::Promise (checked).
     *
     * @param v Pointer to the environment to be assigned to the CXXR::Promise. 
     */
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
        x->u.promsxp.m_env = v;
    }

    void Promise::set_prseen(RObject *x, unsigned int v)
    {
        if (!x)
            return;
        x->m_gpbits = v;
    }
} // namespace CXXR
