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
        const auto &RSTEPptr = RSTEP;
        const auto &SET_CLOENVptr = SET_CLOENV;
        const auto &SET_RSTEPptr = SET_RSTEP;
    } // namespace ForceNonInline

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

    /** @brief Access formal arguments of a CXXR::Closure.
     *
     * @param x Pointer to a CXXR::Closure object (checked).
     *
     * @return Pointer to the formal argument list of \a x.
     */
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

    /** @brief Access the body of a CXXR::Closure.
     *
     * @param x Pointer to a CXXR::Closure object (checked).
     *
     * @return Pointer to the body of \a x.
     */
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

    /** @brief Access the environment of a CXXR::Closure.
     *
     * @param x Pointer to a CXXR::Closure object (checked).
     *
     * @return Pointer to the environment of x.
     */
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
