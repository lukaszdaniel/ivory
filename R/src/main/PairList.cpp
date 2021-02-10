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
                INCREMENT_REFCNT(c->m_tail);
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
