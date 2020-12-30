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

namespace CXXR
{
    // Force the creation of non-inline embodiments of functions callable
    // from C:
    namespace ForceNonInline
    {
        const auto &ENCLOSptr = ENCLOS;
        const auto &isEnvironmentptr = Rf_isEnvironment;
        const auto &FRAMEptr = FRAME;
    } // namespace ForceNonInline

    const char *Environment::typeName() const
    {
        return staticTypeName();
    }

    /* Environment Access Methods */
    /**
     * @param x Pointer to an CXXR::Environment.
     * @return Pointer to the frame of \a x .
     */
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
        return x->u.envsxp.m_frame;
    }

    /** @brief Access an environment's Frame, represented as a PairList.
     *
     * @param x Pointer to a CXXR::Environment (checked).
     *
     * @return Pointer to a PairList representing the contents of the
     * Frame of \a x (may be null).  This PairList is generated on the
     * fly, so this is a relatively expensive operation.  Alterations
     * to the returned PairList will not alter the Environment's Frame.
     *
     * @note Beware that since (unlike CR) this isn't a simple
     * accessor function, its return value will need protection from
     * garbage collection.
     */
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
        return x->u.envsxp.m_enclos;
    }

    /**
     * @param x Pointer to a CXXR::Environment.
     * @return Pointer to \a x 's hash table (may be NULL).
     */
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
        return x->u.envsxp.m_hashtab;
    }

    /**
     * @param x Pointer to a CXXR::Environment.
     * @return \a x 's environment flags.
     * @deprecated
     */
    unsigned int Environment::envflags(RObject *x) { return x ? x->m_gpbits : 0; } /* for environments */

    /**
     * Set environment flags.
     * @param x Pointer to a CXXR::Environment.
     * @param v The new flags.
     * @deprecated
     */
    void Environment::set_envflags(RObject *x, unsigned int v)
    {
        if (!x)
            return;
        x->m_gpbits = v;
    }

    /**
     * Set environment's frame.
     * @param x Pointer to a CXXR::Environment.
     * @param v Pointer to the new frame.
     * @todo Probably should be private.
     */
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
        x->u.envsxp.m_frame = v;
    }

    /**
     * Set environment's enclosing environment.
     * @param x Pointer to a CXXR::Environment.
     * @param v Pointer to the new enclosing environment.
     * @todo Probably should be private.
     */
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
        x->u.envsxp.m_enclos = v;
    }

    /**
     * Set environment's hash table.
     * @param x Pointer to a CXXR::Environment.
     * @param v Pointer to the hash table.
     * @todo Probably should be private.
     */
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
        x->u.envsxp.m_hashtab = v;
    }

    unsigned int Environment::frame_is_locked(RObject *e) { return e ? (envflags(e) & FRAME_LOCK_MASK) : 0; }

    void Environment::lock_frame(RObject *x)
    {
        if (!x)
            return;
        x->m_gpbits |= FRAME_LOCK_MASK;
    }

    bool Environment::is_global_frame(RObject *e) { return e ? (envflags(e) & GLOBAL_FRAME_MASK) : 0; }

    void Environment::mark_as_global_frame(RObject *x)
    {
        if (!x)
            return;

        x->m_gpbits |= GLOBAL_FRAME_MASK;
    }
    void Environment::mark_as_local_frame(RObject *x)
    {
        if (!x)
            return;

        x->m_gpbits &= ~(GLOBAL_FRAME_MASK);
    }
} // namespace CXXR
