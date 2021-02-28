/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
 *
 *  Rho is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the Rho website.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

/** @file PairList.cpp
 *
 * @brief Class PairList and associated C interface.
 */

#include <CXXR/PairList.hpp>
#include <CXXR/StringVector.hpp>
#include <CXXR/ListVector.hpp>
#include <CXXR/Symbol.hpp>
#include <Localization.h>
#include <iostream>

using namespace std;
using namespace CXXR;

namespace CXXR
{
    // Force the creation of non-inline embodiments of functions callable
    // from C:
    namespace ForceNonInline
    {
        const auto &allocListptr = Rf_allocList;
        const auto &allocSExpptr = Rf_allocSExp;
        const auto &consptr = Rf_cons;
    } // namespace ForceNonInline

    GCRoot<> PairList::s_cons_car;
    GCRoot<PairList> PairList::s_cons_cdr;

    PairList::PairList(const PairList &pattern, bool deep)
        : ConsCell(pattern, deep, 0), m_argused(0)
    {
        // Clone the tail:
        PairList *c = this;
        const PairList *pl = pattern.m_tail;
        while (pl)
        {
            c->m_tail = new PairList(*pl, deep, 0);
            if (c->m_tail)
                c->m_tail->incrementRefCount();
            c = c->m_tail;
            pl = pl->m_tail;
        }
    }

    PairList *PairList::clone(bool deep) const
    {
        return new PairList(*this, deep);
    }

    PairList *PairList::makeList(size_t sz)
    {
        PairList *ans = nullptr;
        try
        {
            while (sz--)
                ans = construct(nullptr, ans);
        }
        catch (...)
        {
            if (ans)
                ans->expose();
            throw;
        }
        if (ans)
            ans->expose();
        return ans;
    }

    const char *PairList::typeName() const
    {
        return staticTypeName();
    }
} // namespace CXXR

// ***** C interface *****

SEXP CAR0(SEXP e)
{
    if (!e)
        return nullptr;
    ConsCell::checkST(e);
    ConsCell *cc = SEXP_downcast<ConsCell *>(e, false);
    return cc->car();
}

SEXP CDR(SEXP e)
{
    if (!e)
        return nullptr;
    ConsCell::checkST(e);
    ConsCell *cc = SEXP_downcast<ConsCell *>(e, false);
    return cc->tail();
}

SEXP CAAR(SEXP e)
{
    return CAR(CAR(e));
}

SEXP CDAR(SEXP e)
{
    return CDR(CAR(e));
}

SEXP CADR(SEXP e)
{
    return CAR(CDR(e));
}

SEXP CDDR(SEXP e)
{
    return CDR(CDR(e));
}

SEXP CDDDR(SEXP e)
{
    return CDR(CDR(CDR(e)));
}

SEXP CD4R(SEXP e)
{
    return CDR(CDR(CDR(CDR(e))));
}
SEXP CADDR(SEXP e)
{
    return CAR(CDR(CDR(e)));
}

SEXP CADDDR(SEXP e)
{
    return CAR(CDR(CDR(CDR(e))));
}

SEXP CAD3R(SEXP e)
{
    return CAR(CDR(CDR(CDR(e))));
}

SEXP CAD4R(SEXP e)
{
    return CAR(CDR(CDR(CDR(CDR(e)))));
}

SEXP CAD5R(SEXP e)
{
    return CAR(CDR(CDR(CDR(CDR(CDR(e))))));
}

/* inline version of CAR to support immediate bindings */
SEXP CAR(SEXP e)
{
    if (BNDCELL_TAG(e))
        Rf_error(_("bad binding access"));
    return CAR0(e);
}

Rboolean IS_ACTIVE_BINDING(SEXP b)
{
    if (!b)
        return FALSE;
    return Rboolean(b->isActiveBinding());
}

Rboolean BINDING_IS_LOCKED(SEXP b)
{
    if (!b)
        return FALSE;
    return Rboolean(b->bindingIsLocked());
}

void SET_ACTIVE_BINDING_BIT(SEXP b)
{
    if (b)
        b->setActiveBindingBit();
}

void LOCK_BINDING(SEXP b)
{
    if (b)
        b->lockBinding();
}

void UNLOCK_BINDING(SEXP b)
{
    if (b)
        b->unlockBinding();
}

SEXP Rf_elt(SEXP list, int i)
{
    SEXP result = list;

    if ((i < 0) || (i > Rf_length(list)))
        return R_NilValue;

    for (int j = 0; j < i; ++j)
        result = CDR(result);

    return CAR(result);
}

SEXP Rf_lastElt(SEXP list)
{
    SEXP result = R_NilValue;
    while (list != R_NilValue)
    {
        result = list;
        list = CDR(list);
    }
    return result;
}

/* Shorthands for creating small lists */

SEXP Rf_list1(SEXP s)
{
    return Rf_cons(s, nullptr);
}

SEXP Rf_list2(SEXP s, SEXP t)
{
    GCRoot<> ss(s);
    s = Rf_cons(s, Rf_list1(t));
    return s;
}

SEXP Rf_list3(SEXP s, SEXP t, SEXP u)
{
    GCRoot<> ss(s);
    s = Rf_cons(s, Rf_list2(t, u));
    return s;
}

SEXP Rf_list4(SEXP s, SEXP t, SEXP u, SEXP v)
{
    GCRoot<> ss(s);
    s = Rf_cons(s, Rf_list3(t, u, v));
    return s;
}

SEXP Rf_list5(SEXP s, SEXP t, SEXP u, SEXP v, SEXP w)
{
    GCRoot<> ss(s);
    s = Rf_cons(s, Rf_list4(t, u, v, w));
    return s;
}

SEXP Rf_list6(SEXP s, SEXP t, SEXP u, SEXP v, SEXP w, SEXP x)
{
    GCRoot<> ss(s);
    s = Rf_cons(s, Rf_list5(t, u, v, w, x));
    return s;
}

SEXP Rf_listAppend(SEXP s, SEXP t)
{
    SEXP r;
    if (s == R_NilValue)
        return t;
    r = s;
    while (CDR(r) != R_NilValue)
        r = CDR(r);
    SETCDR(r, t);
    return s;
}

Rboolean Rf_isVectorizable(SEXP s)
{
    if (s == R_NilValue)
        return TRUE;
    else if (Rf_isNewList(s))
    {
        R_xlen_t i, n;

        n = XLENGTH(s);
        for (i = 0; i < n; ++i)
            if (!Rf_isVector(VECTOR_ELT(s, i)) || XLENGTH(VECTOR_ELT(s, i)) > 1)
                return FALSE;
        return TRUE;
    }
    else if (Rf_isList(s))
    {
        for (; s != R_NilValue; s = CDR(s))
            if (!Rf_isVector(CAR(s)) || LENGTH(CAR(s)) > 1)
                return FALSE;
        return TRUE;
    }
    else
        return FALSE;
}

Rboolean Rf_isList(SEXP s)
{
    return Rboolean(s == R_NilValue || TYPEOF(s) == LISTSXP);
}

Rboolean Rf_isPairList(SEXP s)
{
    switch (TYPEOF(s))
    {
    case NILSXP:
    case LISTSXP:
    case LANGSXP:
    case DOTSXP:
        return TRUE;
    default:
        return FALSE;
    }
}