/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998-2007   The R Development Core Team.
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

/** @file WeakRef.cpp
 *
 * Class WeakRef.
 */

#include <cstdlib>
#include <iostream>

#include <CXXR/GCEdge.hpp>
#include <CXXR/WeakRef.hpp>
#include <Localization.h>

using namespace std;
using namespace R;

int WeakRef::s_count = 0;
bool WeakRef::R_finalizers_pending = false;

WeakRef::WeakRef(RObject *key, RObject *value, RObject *R_finalizer,
				 bool finalize_on_exit)
	: RObject(WEAKREFSXP), m_key(key), m_value(key, nullptr), m_Rfinalizer(key, nullptr), m_Cfinalizer(nullptr),
	  m_ready_to_finalize(false),
	  m_finalize_on_exit(finalize_on_exit)
{
	// m_key = key;
	// Force old-to-new checks:
	m_value.redirect(key, value);
	m_Rfinalizer.redirect(key, R_finalizer);

	getLive()->push_back(this);
	m_lit = std::prev(getLive()->end());

	if (!m_key)
		tombstone();
	else
		switch (m_key->sexptype())
		{
		case ENVSXP:
		case EXTPTRSXP:
		case BCODESXP:
			break;
		default:
			Rf_error(_("can only weakly reference/finalize reference objects"));
		}
	++s_count;
}

WeakRef::WeakRef(RObject *key, RObject *value, R_CFinalizer_t C_finalizer,
				 bool finalize_on_exit)
	: RObject(WEAKREFSXP), m_key(key), m_value(key, nullptr), m_Rfinalizer(key, nullptr), m_Cfinalizer(C_finalizer),
	  m_ready_to_finalize(false), m_finalize_on_exit(finalize_on_exit)
{
	// m_key = key;
	// Force old-to-new check:
	m_value.redirect(key, value);
	m_Rfinalizer.redirect(key, nullptr);

	getLive()->push_back(this);
	m_lit = std::prev(getLive()->end());

	if (!m_key)
		tombstone();
	++s_count;
}

WeakRef::~WeakRef()
{
	WRList *wrl = wrList();
	if (wrl)
		wrl->erase(m_lit);
	--s_count;
}

bool WeakRef::check()
{
#ifndef NDEBUG
	// Check sizes:
	size_t total_size = getLive()->size() + getFinalizationPending()->size() + getTombstone()->size();
	if (total_size != size_t(s_count))
	{
		cerr << "WeakRef::check() : tally error\n"
			 << "live size: " << getLive()->size()
			 << "\nfinalization pending size: "
			 << getFinalizationPending()->size()
			 << "\ntombstone size: " << getTombstone()->size()
			 << "\ns_count: " << s_count << '\n';
		abort();
	}
	// Check the live list:
	for (const WeakRef *wr : *getLive())
	{
		if (wr->m_ready_to_finalize)
		{
			cerr << "Node on live list set READY_TO_FINALIZE\n";
			abort();
		}
		if (!wr->m_key)
		{
			cerr << "Node on live list with null key\n";
			abort();
		}
	}
	// Check finalization pending:
	for (const WeakRef *wr : *getFinalizationPending())
	{
		if (!wr->m_ready_to_finalize)
		{
			cerr << "Node on finalization pending list not READY_TO_FINALIZE\n";
			abort();
		}
		if (!wr->m_key)
		{
			cerr << "Node on finalization pending list with null key\n";
			abort();
		}
		if (!wr->m_Rfinalizer && !wr->m_Cfinalizer)
		{
			cerr << "Node on finalization pending list without finalizer\n";
			abort();
		}
	}
	// Check tombstone:
	for (const WeakRef *wr : *getTombstone())
	{
		if (wr->m_ready_to_finalize)
		{
			cerr << "Node on tombstone list set READY_TO_FINALIZE\n";
			abort();
		}
		if (wr->m_key)
		{
			cerr << "Node on tombstone list with non-null key\n";
			abort();
		}
	}
#endif
	return true;
}

WeakRef::WRList *WeakRef::getLive()
{
	static WRList *live = new WRList();
	return live;
}

WeakRef::WRList *WeakRef::getFinalizationPending()
{
	static WRList *finalization_pending = new WRList();
	return finalization_pending;
}

WeakRef::WRList *WeakRef::getTombstone()
{
	static WRList *tombstone = new WRList();
	return tombstone;
}

void WeakRef::markThru(unsigned int max_gen)
{
	WeakRef::check();
	GCNode::Marker marker(max_gen);
	WRList newlive;

	WRList *live = getLive();
	WRList *finalization_pending = getFinalizationPending();

	// Step 2-3 of algorithm.  Mark the value and R finalizer if the
	// key is marked, or in a generation not being collected.
	{
		bool newmarks;
		do
		{
			newmarks = false;
			WRList::iterator lit = live->begin();
			while (lit != live->end())
			{
				WeakRef *wr = *lit++;
				RObject *key = wr->key();
				if (key->m_gcgen > max_gen || key->isMarked())
				{
					RObject *value = wr->value();
					if (value && value->conductVisitor(&marker))
						newmarks = true;
					RObject *Rfinalizer = wr->m_Rfinalizer;
					if (Rfinalizer && Rfinalizer->conductVisitor(&marker))
						newmarks = true;
					wr->transfer(live, &newlive);
				}
			}
		} while (newmarks);
	}
	// Step 4 of algorithm.  Process references with unmarked keys.
	WeakRef::R_finalizers_pending = false;
	{
		WRList::iterator lit = live->begin();
		while (lit != live->end())
		{
			WeakRef *wr = *lit++;
			RObject *Rfinalizer = wr->m_Rfinalizer;
			if (Rfinalizer)
				Rfinalizer->conductVisitor(&marker);
			if (Rfinalizer || wr->m_Cfinalizer)
			{
				wr->m_ready_to_finalize = true;
				wr->transfer(live, finalization_pending);
				WeakRef::R_finalizers_pending = true;
			}
			else
				wr->tombstone();
		}
	}
	// Step 5 of algorithm.  Mark all live references with reachable keys.
	{
		live->splice(live->end(), newlive);
		for (WRList::iterator lit = live->begin();
			 lit != live->end(); ++lit)
		{
			WeakRef *wr = *lit;
			wr->conductVisitor(&marker);
		}
	}
}

void WeakRef::tombstone()
{
	WRList *currentList = wrList();
	m_key = nullptr;
	m_value.redirect(nullptr, nullptr);
	m_Rfinalizer.redirect(nullptr, nullptr);
	m_Cfinalizer = nullptr;
	m_ready_to_finalize = false;
	transfer(currentList, getTombstone());
}

WeakRef::WRList *WeakRef::wrList() const
{
	return m_ready_to_finalize ? getFinalizationPending() : (m_key ? getLive() : getTombstone());
}

#if 1
void WeakRef::set_ready_to_finalize(RObject *x)
{
	if (!x)
		return;
	x->m_gpbits |= READY_TO_FINALIZE_MASK;
}

void WeakRef::clear_ready_to_finalize(RObject *x)
{
	if (!x)
		return;
	x->m_gpbits &= ~READY_TO_FINALIZE_MASK;
}

bool WeakRef::is_ready_to_finalize(RObject *x) { return x ? x->m_gpbits & READY_TO_FINALIZE_MASK : 0; }

void WeakRef::set_finalize_on_exit(RObject *x)
{
	if (!x)
		return;
	x->m_gpbits |= FINALIZE_ON_EXIT_MASK;
}

void WeakRef::clear_finalize_on_exit(RObject *x)
{
	if (!x)
		return;
	x->m_gpbits &= ~FINALIZE_ON_EXIT_MASK;
}

bool WeakRef::finalize_on_exit(RObject *x) { return x ? (x->m_gpbits & FINALIZE_ON_EXIT_MASK) : 0; }

#endif
