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
#include <CXXR/Evaluator.hpp>
#include <CXXR/GCStackRoot.hpp>
#include <Localization.h>
#include <R_ext/Error.h>

using namespace std;
using namespace R;
using namespace CXXR;

namespace CXXR
{
    Frame::monitor Frame::s_read_monitor = nullptr;
    Frame::monitor Frame::s_write_monitor = nullptr;

    // ***** Class Frame::Binding *****

    pair<RObject *, bool> Frame::Binding::forcedValueSlow() const
    {
        bool promise_forced = false;
        RObject *val = m_value;

        if (val && val->sexptype() == PROMSXP)
        {
            Promise *prom = static_cast<Promise *>(val);
            if (!prom->evaluated())
            {
                frame()->monitorRead(*this);
                promise_forced = true;
            }
            val = Evaluator::evaluate(prom, nullptr);
        }
        return make_pair(val, promise_forced);
    }

    // We want to be able to determine quickly if a symbol is *not*
    // defined in an frame, so that we can carry on working up the
    // chain of enclosing frames.  On average the number of tests
    // needed to determine that a symbol is not present is 1 + 2L, where L
    // is the load factor.  So we keep the load factor small:
    namespace
    {
        constexpr double maximum_load_factor = 0.5;
    }

    Frame::Frame(size_t initial_capacity)
        : m_cache_count(0), m_locked(false), m_no_special_symbols(true), m_read_monitored(false), m_write_monitored(false), m_map(nullptr)
    {
        m_map = new map(ceil(initial_capacity / maximum_load_factor));
        m_map->max_load_factor(maximum_load_factor);
    }

    PairList *Frame::asPairList() const
    {
        GCStackRoot<PairList> ans(nullptr);
        visitBindings([&](const Binding *binding)
                      { ans = binding->asPairList(ans); });
        return ans;
    }

    PairList *Frame::Binding::asPairList(PairList *tail) const
    {
        PairList *ans = new PairList(rawValue(), tail, symbol());
        SET_MISSING(ans, origin());
        SET_BNDCELL_TAG(ans, bndcellTag());
        SET_ASSIGNMENT_PENDING(ans, assignmentPending());
        if (isActive())
            SET_ACTIVE_BINDING_BIT(ans);
        if (isLocked())
            LOCK_BINDING(ans);
        ans->expose();
        return ans;
    }

    void frameReadPairList(Frame *frame, PairList *bindings)
    {
        for (PairList *pl = bindings; pl; pl = pl->tail())
        {
            const RObject *tag = pl->tag();
            const Symbol *symbol = dynamic_cast<const Symbol *>(tag);
            if (!symbol)
                Rf_error(_("list used to set frame bindings must have symbols as tags throughout"));
            Frame::Binding *bdg = frame->obtainBinding(symbol);
            bdg->fromPairList(pl);
        }
    }

    void Frame::Binding::fromPairList(PairList *pl)
    {
        const RObject *tag = pl->tag();
        if (tag && tag != m_symbol)
            Rf_error(_("internal error in %s"), "Frame::Binding::fromPairList()");
        Origin pl_origin = Origin(pl->missing());
        setBndCellTag(pl->bndcellTag());
        setAssignmentPending(pl->assignmentPending());
        if (pl->isActive())
        {
            setFunction(SEXP_downcast<FunctionBase *>(pl->car()), pl_origin);
        }
        else
        {
            setValue(pl->car(), pl_origin);
        }
        setLocking(pl->isLocked());
    }

    void Frame::Binding::handleSetValueError() const
    {
        if (isLocked())
            Rf_error(_("cannot change value of locked binding for '%s'"), symbol()->name()->c_str());
        if (isActive())
            Rf_error(_("internal error: use %s for active bindings"), "setFunction()");
    }

    void Frame::Binding::handleFunctionSetValueError() const
    {
        if (!isActive())
            Rf_error(_("symbol already has a regular binding"));
        if (isLocked())
            Rf_error(_("cannot change active binding if binding is locked"));
    }

    Frame::Binding *Frame::binding(const Symbol *symbol)
    {
        if (symbol->isSpecialSymbol() && m_no_special_symbols)
            return nullptr;
        return v_binding(symbol);
    }

    const Frame::Binding *Frame::binding(const Symbol *symbol) const
    {
        return const_cast<Frame *>(this)->binding(symbol);
    }

    // NB: this is always a normal frame, never an execution frame.
    Frame::Frame(const Frame &source)
        : m_cache_count(0), m_locked(source.m_locked),
          m_no_special_symbols(source.m_no_special_symbols),
          m_read_monitored(false), m_write_monitored(false), m_map(nullptr)
    {
        m_map = new map(source.m_map->size());
        m_map->max_load_factor(source.m_map->max_load_factor());
        importBindings(&source);
        if (source.isLocked())
            lock(false);
    }

    void Frame::clear()
    {
        statusChanged(nullptr);
        m_map->clear();
        m_no_special_symbols = true;
    }

    bool Frame::erase(const Symbol *symbol)
    {
        if (isLocked())
            Rf_error(_("cannot remove bindings from a locked frame"));
        return m_map->erase(symbol);
    }

    void Frame::enableReadMonitoring(bool on) const
    {
        if (on && !s_read_monitor)
            Rf_error("Internal error: Frame::s_read_monitor not set");
        m_read_monitored = on;
    }

    void Frame::enableWriteMonitoring(bool on) const
    {
        if (on && !s_write_monitor)
            Rf_error("Internal error: Frame::s_write_monitor not set");
        m_write_monitored = on;
    }

    void Frame::lockBindings()
    {
        modifyBindings([](Binding *binding)
                       { binding->setLocking(true); });
    }

    Frame::Binding *Frame::obtainBinding(const Symbol *symbol)
    {
        Frame::Binding *binding = nullptr;
        if (isLocked())
        {
            // If the frame is locked, we can only return pre-existing bindings.
            binding = Frame::binding(symbol);
            if (!binding)
            {
                Rf_error(_("cannot add bindings to a locked frame"));
            }
            return binding;
        }

        // If the binding exists, return that.
        binding = Frame::binding(symbol);
        if (binding)
            return binding;

        // If all else fails, go to the m_map.
        if (!binding)
        {
            if (!m_map)
            {
                m_map = new map();
            }
            binding = &((*m_map)[symbol]);
        }

        if (!binding->frame())
        {
            initializeBinding(binding, symbol);
        }
        return binding;
    }

    std::size_t Frame::size() const
    {
        return m_map->size();
    }

    void Frame::visitReferents(const_visitor *v) const
    {
        visitBindings([=](const Binding *binding)
                      { binding->visitReferents(v); });
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

    Frame::~Frame()
    {
        statusChanged(nullptr);
        delete m_map;
    }

    void Frame::Binding::setValue(RObject *new_value, Origin origin, bool quiet)
    {
        if (isLocked() || isActive())
        {
            handleSetValueError();
        }
        if (bndcellTag())
        {
            m_value.clearCar();
            setBndCellTag(NILSXP);
        }
        if (m_value == new_value)
        {
            return;
        }
        if (assignmentPending())
        {
            setAssignmentPending(false);
            GCNode::incRefCount(m_value);
        }
        m_value.retarget(m_frame, new_value);
        m_origin = origin;
        if (!quiet)
            m_frame->monitorWrite(*this);
    }

    void Frame::Binding::setFunction(FunctionBase *function, Origin origin)
    {
        // See if binding already has a non-default value:
        if (m_value != Symbol::missingArgument() && m_value != Symbol::unboundValue())
        {
            handleFunctionSetValueError();
        }
        if (bndcellTag())
        {
            m_value.clearCar();
            setBndCellTag(NILSXP);
        }
        if (m_value == function)
        {
            return;
        }
        if (assignmentPending())
        {
            setAssignmentPending(false);
            GCNode::incRefCount(m_value);
        }
        m_value.retarget(m_frame, function);
        m_active = true;
        m_frame->monitorWrite(*this);
    }

    Frame *Frame::clone() const
    {
        return new Frame(*this);
    }

    // Frame::Binding::assignSlow() is defined in envir.cpp (for the time being).
    void Frame::Binding::assign(RObject *new_value, Origin origin)
    {
        if (isLocked() || isActive())
        {
            assignSlow(new_value, origin);
        }
        else
        {
            setValue(new_value, origin);
        }
    }

    RObject *Frame::Binding::forcedValue() const
    {
        if (m_value && m_value->sexptype() == PROMSXP)
        {
            return forcedValueSlow().first;
        }
        return m_value;
    }

    std::pair<RObject *, bool> Frame::Binding::forcedValue2() const
    {
        if (m_value && m_value->sexptype() == PROMSXP)
        {
            return forcedValueSlow();
        }
        return std::make_pair(m_value, false);
    }

    void Frame::Binding::setMissing(short int missingval)
    {
        if (isLocked())
            Rf_error(_("cannot change missing status of a locked binding"));
        if (!(missingval == Origin::EXPLICIT || missingval == Origin::MISSING || missingval == Origin::DEFAULTED))
            Rf_error(_("incorrect missing status supplied"));
        m_origin = Origin(missingval);
    }

    vector<const Symbol *> Frame::symbols(bool include_dotsymbols,
                                          bool sorted) const
    {
        vector<const Symbol *> ans;
        visitBindings([&](const Binding *binding)
                      {
                          const Symbol *symbol = binding->symbol();
                          if (include_dotsymbols || !isDotSymbol(symbol))
                              ans.push_back(symbol);
                      });
        if (sorted)
        {
            std::sort(ans.begin(), ans.end(),
                      [](const Symbol *x, const Symbol *y)
                      { return String::Comparator()(x->name(), y->name()); });
        }

        return ans;
    }

    // Frame::Binding::unforcedValue() is defined in envir.cpp (for the time being).
    RObject *Frame::Binding::value() const
    {
        return unforcedValue();
    }

    Frame::Binding *Frame::v_binding(const Symbol *symbol)
    {
        auto location = m_map->find(symbol);
        if (location != m_map->end())
        {
            return &(location->second);
        }
        return nullptr;
    }

    void Frame::flush(const Symbol *sym)
    {
        // Environment::flushFromSearchPathCache(sym);
    }

    void Frame::initializeBinding(Frame::Binding *binding, const Symbol *symbol)
    {
        assert(!isLocked());
        binding->initialize(this, symbol);
        statusChanged(symbol);
        if (symbol->isSpecialSymbol())
        {
            m_no_special_symbols = false;
        }
    }

    void Frame::initializeBindingIfUnlocked(Frame::Binding *binding, const Symbol *symbol)
    {
        if (isLocked())
        {
            Rf_error(_("cannot add bindings to a locked frame"));
        }
        initializeBinding(binding, symbol);
    }

    void Frame::importBinding(const Binding *binding_to_import, bool quiet)
    {
        if (!binding_to_import)
            return;
        Binding *new_binding = obtainBinding(binding_to_import->symbol());
        *new_binding = *binding_to_import;
        new_binding->m_frame = this;
        if (!quiet)
            monitorWrite(*new_binding);
    }

    void Frame::importBindings(const Frame *frame, bool quiet)
    {
        frame->visitBindings([=](const Binding *binding)
                             { importBinding(binding, quiet); });
    }

    void Frame::visitBindings(std::function<void(const Binding *)> f) const
    {
        for (const auto &entry : *m_map)
        {
            f(&(entry.second));
        }
    }

    void Frame::modifyBindings(std::function<void(Binding *)> f)
    {
        for (auto &entry : *m_map)
        {
            f(&(entry.second));
        }
    }

    void Frame::Binding::visitReferents(const_visitor *v) const
    {
        // We assume the visitor has just come from m_frame, so we don't
        // visit that.
        m_symbol.conductVisitor(v);
        m_value.conductVisitor(v);
    }

    bool isMissingArgument(const Symbol *sym, Frame *frame)
    {
#if CXXR_TRUE
        std::cerr << "isMissingArgument(...) not yet implemented" << std::endl;
        abort();
        return false;
#else
        RObject *rawval;
        if (sym->isDotDotSymbol())
        {
            unsigned int ddv = sym->dotDotIndex();
            Frame::Binding *bdg = frame->binding(SEXP_downcast<Symbol *>(R_DotsSymbol));
            if (!bdg)
                return false; // This is what CR does.  Is it really right?
            ConsCell *cc = SEXP_downcast<ConsCell *>(bdg->rawValue());
            while (cc && ddv > 1)
            {
                cc = cc->tail();
                --ddv;
            }
            if (!cc)
                return true;
            rawval = cc->car();
        }
        else
        {
            // Not a ..n symbol:
            if (sym == Symbol::missingArgument())
                return true;
            Frame::Binding *bdg = frame->binding(sym);
            if (!bdg)
                return false;
            rawval = bdg->rawValue();
            if (bdg->origin() == Frame::Binding::MISSING || rawval == Symbol::missingArgument())
                return true;
            if (bdg->isActive())
                return false;
        }
        if (rawval && rawval->sexptype() == PROMSXP)
        {
            Promise *prom = static_cast<Promise *>(rawval);
            return prom->isMissingSymbol();
        }
        return false;
#endif
    }
} // namespace CXXR
