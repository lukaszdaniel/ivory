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

/** @file Frame.cpp
 *
 * @brief Implementation of class Frame and Frame::Binding.
 */

#include <algorithm>
#include <CXXR/Frame.hpp>
#include <CXXR/FunctionBase.hpp>
#include <CXXR/Promise.hpp>
#include <Localization.h>
#include <R_ext/Error.h>

using namespace std;
using namespace CXXR;

namespace CXXR
{
    // ***** Class Frame::Binding *****

    PairList *Frame::Binding::asPairList(PairList *tail) const
    {
        PairList *ans = new PairList(m_value, tail, const_cast<Symbol *>(symbol()));
        SET_MISSING(ans, missing());
        if (isActive())
            SET_ACTIVE_BINDING_BIT(ans);
        if (isLocked())
            LOCK_BINDING(ans);
        ans->expose();
        return ans;
    }

    // Frame::Binding::assign() is defined in envir.cpp (for the time being).

    void Frame::Binding::fromPairList(PairList *pl)
    {
        const RObject *tag = pl->tag();
        if (tag && tag != m_symbol)
            Rf_error(_("internal error in %s"), "Frame::Binding::fromPairList()");
        if (pl->m_active_binding)
            setFunction(SEXP_downcast<FunctionBase *>(pl->car()));
        else
            setValue(pl->car());
        setMissing(pl->m_missing);
        setLocking(pl->m_binding_locked);
    }

    void Frame::Binding::initialize(Frame *frame, const Symbol *sym)
    {
        if (m_frame)
            Rf_error(_("internal error: binding already initialized"));
        if (!frame || !sym)
            Rf_error(_("internal error in %s"), "Frame::Binding::initialize()");
        m_frame = frame;
        m_symbol.retarget(frame, sym);
    }

    void Frame::Binding::setFunction(FunctionBase *function)
    {
        // See if binding already has a non-null value:
        if (m_value)
        {
            if (!isActive())
                Rf_error(_("symbol already has a regular binding"));
            if (isLocked())
                Rf_error(_("cannot change active binding if binding is locked"));
        }
        m_value.retarget(m_frame, function);
        m_active = true;
        m_frame->monitorWrite(*this);
    }

    void Frame::Binding::setMissing(short int missingval)
    {
        if (isLocked())
            Rf_error(_("cannot change missing status of a locked binding"));
        m_missing = missingval;
    }

    void Frame::Binding::setValue(RObject *new_value)
    {
        if (isLocked())
            Rf_error(_("cannot change value of locked binding for '%s'"), symbol()->name()->c_str());
        if (isActive())
            Rf_error(_("internal error: use %s for active bindings"), "setFunction()");
        m_value.retarget(m_frame, new_value);
        m_frame->monitorWrite(*this);
    }

    // Frame::Binding::value() is defined in envir.cpp (for the time being).

    void Frame::Binding::visitReferents(const_visitor *v) const
    {
        // We assume the visitor has just come from m_frame, so we don't
        // visit that.
        m_symbol.conductVisitor(v);
        m_value.conductVisitor(v);
    }

    void frameReadPairList(Frame *frame, PairList *bindings)
    {
        for (PairList *pl = bindings; pl != nullptr; pl = pl->tail())
        {
            RObject *tag = pl->tag();
            Symbol *symbol = dynamic_cast<Symbol *>(tag);
            if (!symbol)
                Rf_error(_("list used to set frame bindings must have symbols as tags throughout"));
            Frame::Binding *bdg = frame->obtainBinding(symbol);
            bdg->fromPairList(pl);
        }
    }
} // namespace CXXR
