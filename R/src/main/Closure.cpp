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
        const auto &MISSINGptr = MISSING;
        const auto &SET_MISSINGptr = SET_MISSING;
        const auto &RDEBUGptr = RDEBUG;
        const auto &RSTEPptr = RSTEP;
        const auto &SET_CLOENVptr = SET_CLOENV;
        const auto &SET_RDEBUGptr = SET_RDEBUG;
        const auto &SET_RSTEPptr = SET_RSTEP;
    } // namespace ForceNonInline

    const char *Closure::typeName() const
    {
        return staticTypeName();
    }

    /* Closure Access Methods */
    /** @brief Access formal arguments of a CXXR::Closure.
     *
     * @param x Pointer to a CXXR::Closure object (checked).
     *
     * @return Pointer to the formal argument list of \a x.
     */
    RObject *Closure::formals(RObject *x) { return x ? x->u.closxp.m_formals : nullptr; }

    /** @brief Set the formal arguments of a CXXR::Closure.
     *
     * @param x Pointer to a CXXR::Closure object (checked).
     *
     * @param v Pointer to the formal argument list.
     */
    void Closure::set_formals(RObject *x, RObject *v)
    {
        if (!x)
            return;
        x->u.closxp.m_formals = v;
    }

    /** @brief Access the body of a CXXR::Closure.
     *
     * @param x Pointer to a CXXR::Closure object (checked).
     *
     * @return Pointer to the body of \a x.
     */
    RObject *Closure::body(RObject *x) { return x ? x->u.closxp.m_body : nullptr; }

    /** @brief Set the body of a CXXR::Closure.
     *
     * @param x Pointer to a CXXR::Closure object (checked).
     *
     * @param v Pointer to the body of this CXXR::Closure.
     */
    void Closure::set_body(RObject *x, RObject *v)
    {
        if (!x)
            return;
        x->u.closxp.m_body = v;
    }

    /** @brief Access the environment of a CXXR::Closure.
     *
     * @param x Pointer to a CXXR::Closure object (checked).
     *
     * @return Pointer to the environment of x.
     */
    RObject *Closure::cloenv(RObject *x) { return x ? x->u.closxp.m_env : nullptr; }

    /** @brief Replace the environment of a CXXR::Closure.
     *
     * @param x Pointer to a CXXR::Closure object (checked).
     *
     * @param v Pointer to the environment now to be
     *          considered as the environment of this CXXR::Closure.  A
     *          null pointer is not permissible (not checked).
     */
    void Closure::set_cloenv(RObject *x, RObject *v)
    {
        if (!x)
            return;
        x->u.closxp.m_env = v;
    }

    /** @brief Query debugging status.
     *
     * @param x Pointer to a CXXR::Closure object.
     *
     * @return \c true if debugging is set, i.e. evaluations of the
     *         function should run under the browser.
     */
    bool Closure::rdebug(RObject *x) { return x && x->m_debug; }

    /**
     * Set the debugging state of a CXXR::Closure object.
     *
     * @param x Pointer a CXXR::Closure object (checked).
     *
     * @param v The new debugging state.
     */
    void Closure::set_rdebug(RObject *x, bool v)
    {
        if (!x)
            return;
        x->m_debug = v;
    }

    bool Closure::rstep(RObject *x) { return x && x->m_spare; }

    void Closure::set_rstep(RObject *x, bool v)
    {
        if (!x)
            return;
        x->m_spare = v;
    }
} // namespace CXXR

extern "C"
{
    int MISSING(SEXP x);
    void SET_MISSING(SEXP x, int v);
}
