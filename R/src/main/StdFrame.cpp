/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999--2020  The R Core Team.
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
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

/** @file StdFrame.cpp
 *
 * @brief Implementation of class CXXR:StdFrame.
 */

// A StdFrame is implemented using two data structures.  First
// there is a PairList, each of whose elements represents a binding,
// and so maps a symbol (held as the tag) to a value (held as the
// 'car'), and also contains information about locking, active binding
// etc.  Secondly there is an unordered_map (i.e. hash table) which
// maps symbols to elements of the PairList.  Operations on the
// PairList are always done via the unordered_map.  When a symbol is
// erased from the enviroment, the continuity of the PairList will be
// broken, and in this event the PairList is marked as stale.  The
// private function refreshFrameList() is invoked when necessary to
// restring the PairList by iterating over the hash table.

#include <cmath>
#include <CXXR/StdFrame.hpp>
#include <CXXR/Symbol.hpp>
#include <CXXR/SEXP_downcast.hpp>
#include <Localization.h>
#include <R_ext/Error.h>

using namespace std;
using namespace CXXR;

namespace CXXR
{

    // We want to be able to determine quickly if a symbol is *not*
    // defined in an frame, so that we can carry on working up the
    // chain of enclosing frames.  On average the number of tests
    // needed to determine that a symbol is not present is 1 + 2L, where L
    // is the load factor.  So we keep the load factor small:
    namespace
    {
        const float maximum_load_factor = 0.5;
    }

    StdFrame::StdFrame(size_t initial_capacity)
        : m_map(ceil(initial_capacity / maximum_load_factor))
    {
        m_map.max_load_factor(maximum_load_factor);
    }

    PairList *StdFrame::asPairList() const
    {
        GCRoot<PairList> ans(nullptr);
        for (map::const_iterator it = m_map.begin(); it != m_map.end(); ++it)
            ans = (*it).second.asPairList(ans);
        return ans;
    }

    Frame::Binding *StdFrame::binding(const Symbol *symbol)
    {
        map::iterator it = m_map.find(symbol);
        if (it == m_map.end())
            return nullptr;
        return &(*it).second;
    }

    const Frame::Binding *StdFrame::binding(const Symbol *symbol) const
    {
        map::const_iterator it = m_map.find(symbol);
        if (it == m_map.end())
            return nullptr;
        return &(*it).second;
    }

    void StdFrame::clear()
    {
        m_map.clear();
    }

    bool StdFrame::erase(const Symbol *symbol)
    {
        if (isLocked())
            Rf_error(_("cannot remove bindings from a locked frame"));
        return m_map.erase(symbol);
    }

    void StdFrame::lockBindings()
    {
        for (map::iterator it = m_map.begin(); it != m_map.end(); ++it)
            (*it).second.setLocking(true);
    }

    Frame::Binding *StdFrame::obtainBinding(const Symbol *symbol)
    {
        Binding &bdg = m_map[symbol];
        // Was this binding newly created?
        if (!bdg.frame())
        {
            if (isLocked())
            {
                m_map.erase(symbol);
                Rf_error(_("cannot add bindings to a locked frame"));
            }
            bdg.initialize(this, symbol);
        }
        return &bdg;
    }

    size_t StdFrame::size() const
    {
        return m_map.size();
    }

    void StdFrame::visitReferents(const_visitor *v) const
    {
        Frame::visitReferents(v);
        for (map::const_iterator it = m_map.begin(); it != m_map.end(); ++it)
            (*it).second.visitReferents(v);
    }
} // namespace CXXR
