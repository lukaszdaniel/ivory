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

// #include <CXXR/GCEdge.hpp>
#include <CXXR/Environment.hpp>
#include <CXXR/Expression.hpp>
#include <CXXR/Evaluator.hpp>
#include <CXXR/GCStackRoot.hpp>
#include <CXXR/ProtectStack.hpp>
#include <CXXR/JMPException.hpp>
#include <CXXR/WeakRef.hpp>
#include <Localization.h>

using namespace std;
using namespace CXXR;

extern "C"
{
	extern SEXP R_HandlerStack;
	extern SEXP R_RestartStack;
	extern Rboolean R_interrupts_suspended;
}

namespace CXXR
{
	int WeakRef::s_count = 0;

	namespace
	{
		// Used in {,un}packGPBits():
		// constexpr unsigned int READY_TO_FINALIZE_MASK = 1;
		constexpr unsigned int FINALIZE_ON_EXIT_MASK = 2;
	} // namespace

	WeakRef::WeakRef(RObject *key, RObject *value, RObject *R_finalizer,
					 bool finalize_on_exit)
		: RObject(WEAKREFSXP), m_Cfinalizer(nullptr),
		  m_ready_to_finalize(false),
		  m_finalize_on_exit(finalize_on_exit)
	{
		m_key = key;
		m_value = value;
		m_Rfinalizer = R_finalizer;

		expose();
		if (m_key)
			m_key->expose();
		else
			tombstone();

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
		: RObject(WEAKREFSXP), m_Cfinalizer(C_finalizer),
		  m_ready_to_finalize(false), m_finalize_on_exit(finalize_on_exit)
	{
		m_key = key;
		m_value = value;
		m_Rfinalizer = nullptr;

		expose();
		if (m_key)
			m_key->expose();
		else
			tombstone();

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

	unsigned int WeakRef::packGPBits() const
	{
		unsigned int ans = RObject::packGPBits();
		if (m_finalize_on_exit)
			ans |= FINALIZE_ON_EXIT_MASK;
		return ans;
	}

	void WeakRef::unpackGPBits(unsigned int gpbits)
	{
		RObject::unpackGPBits(gpbits);
		m_finalize_on_exit = ((gpbits & FINALIZE_ON_EXIT_MASK) != 0);
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
				 << "\ns_count: " << s_count << "\n";
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

	// WeakRef::finalize() is in memory.cpp (for the time being, until
	// eval() is declared in a CXXR header).

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
					if (key && (key->m_gcgen > max_gen || key->isMarked()))
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
		{
			WRList::iterator lit = live->begin();
			while (lit != live->end())
			{
				WeakRef *wr = *lit++;
				RObject *key = wr->key();
				RObject *value = wr->value();
				RObject *Rfinalizer = wr->m_Rfinalizer;
				if (Rfinalizer || wr->m_Cfinalizer)
				{
					if (wr)
						wr->conductVisitor(&marker);
					if (key)
						key->conductVisitor(&marker);
					if (value)
						value->conductVisitor(&marker);
					if (Rfinalizer)
						Rfinalizer->conductVisitor(&marker);
					wr->m_ready_to_finalize = true;
					wr->transfer(live, finalization_pending);
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

	bool WeakRef::runFinalizers()
	{
		R_CHECK_THREAD;
		/* Prevent this function from running again when already in
		   progress. Jumps can only occur inside the top level context
		   where they will be caught, so the flag is guaranteed to be
		   reset at the end.
		 */
		static bool running = false;
		if (running)
			return false;
		running = true;

		WRList *finalization_pending = getFinalizationPending();
		bool finalizer_run = !finalization_pending->empty();

		WeakRef::check();

		while (finalization_pending->size())
		{
			WeakRef *wr = *finalization_pending->begin();
			/**** use R_ToplevelExec here? */
			RCNTXT thiscontext;
			RCNTXT *volatile saveToplevelContext;
			volatile bool oldvis;
			GCStackRoot<> oldHStack(R_HandlerStack);
			GCStackRoot<> oldRStack(R_RestartStack);
			GCStackRoot<> oldRVal(R_ReturnedValue);
			oldvis = Evaluator::resultPrinted();
			R_HandlerStack = nullptr;
			R_RestartStack = nullptr;

			/* A top level context is established for the finalizer to
			   insure that any errors that might occur do not spill
			   into the call that triggered the collection.
			 */
			thiscontext.start(CTXT_TOPLEVEL, nullptr, R_GlobalEnv, Environment::base(), nullptr, nullptr);
			saveToplevelContext = R_ToplevelContext;
			GCStackRoot<> topExp(R_CurrentExpr);
			auto savestack = ProtectStack::size();

			bool redo = false;
			bool jumped = false;
			do
			{
				redo = false;
				try
				{
					if (!jumped)
					{
						R_GlobalContext = R_ToplevelContext = &thiscontext;
						runWeakRefFinalizer(wr);
					}
					thiscontext.end();
				}
				catch (CXXR::JMPException &e)
				{
					if (e.context() != &thiscontext)
						throw;
					redo = true;
					jumped = true;
				}
			} while (redo);

			R_ToplevelContext = saveToplevelContext;
			ProtectStack::restoreSize(savestack);
			R_CurrentExpr = topExp;
			R_HandlerStack = oldHStack;
			R_RestartStack = oldRStack;
			R_ReturnedValue = oldRVal;
			Evaluator::enableResultPrinting(oldvis);
		}
		running = false;
		return finalizer_run;
	}

	void WeakRef::tombstone()
	{
		WRList *currentList = wrList();
		m_key = nullptr;
		m_value = nullptr;
		m_Rfinalizer = nullptr;
		m_Cfinalizer = nullptr;
		m_ready_to_finalize = false;
		transfer(currentList, getTombstone());
	}

	WeakRef::WRList *WeakRef::wrList() const
	{
		return m_ready_to_finalize ? getFinalizationPending() : (m_key ? getLive() : getTombstone());
	}

	void WeakRef::runPendingFinalizers()
	{
		WRList *finalization_pending = getFinalizationPending();
		if (!finalization_pending->empty())
			runFinalizers();
	}

	void WeakRef::runExitFinalizers()
	{
		WeakRef::check();
		WRList *live = getLive();
		WRList *finalization_pending = getFinalizationPending();
		for (WeakRef *wr : *finalization_pending)
		{
			if (wr->m_finalize_on_exit)
			{
				wr->m_ready_to_finalize = true;
				wr->transfer(live, finalization_pending);
			}
		}
		runFinalizers();
	}

	void WeakRef::runWeakRefFinalizer(RObject *x)
	{

		WeakRef *w = SEXP_downcast<WeakRef *>(x);
		if (TYPEOF(w) != WEAKREFSXP)
			Rf_error(_("not a weak reference"));

		Rboolean oldintrsusp = R_interrupts_suspended;
		R_interrupts_suspended = TRUE;
		w->finalize();

		R_interrupts_suspended = oldintrsusp;
	}

	void WeakRef::finalize()
	{
		R_CFinalizer_t Cfin = m_Cfinalizer;
		GCStackRoot<> key(m_key);
		GCStackRoot<> Rfin(m_Rfinalizer);
		// Do this now to ensure that finalizer is run only once, even if
		// an error occurs:
		tombstone();
		if (Cfin)
			Cfin(key);
		else if (Rfin)
		{
			GCStackRoot<PairList> tail(CXXR_cons(key, nullptr));
			GCStackRoot<Expression> e(new Expression(Rfin, tail), true);
			Rf_eval(e, Environment::global());
		}
	}
} // namespace CXXR

// ***** C interface *****

namespace
{
	void checkKey(SEXP key)
	{
		switch (TYPEOF(key))
		{
		case NILSXP:
		case ENVSXP:
		case EXTPTRSXP:
		case BCODESXP:
			break;
		default:
			Rf_error(_("can only weakly reference/finalize reference objects"));
		}
	}
} // namespace

SEXP R_MakeWeakRef(SEXP key, SEXP val, SEXP fin, Rboolean onexit)
{
	checkKey(key);
	switch (TYPEOF(fin))
	{
	case NILSXP:
	case CLOSXP:
	case BUILTINSXP:
	case SPECIALSXP:
		break;
	default:
		Rf_error(_("finalizer must be a function or NULL"));
	}

	return GCNode::expose(new WeakRef(key, val, fin, onexit));
}

SEXP R_MakeWeakRefC(SEXP key, SEXP val, R_CFinalizer_t fin, Rboolean onexit)
{
	checkKey(key);
	return GCNode::expose(new WeakRef(key, val, fin, onexit));
}

SEXP R_WeakRefKey(SEXP w)
{
	if (TYPEOF(w) != WEAKREFSXP)
		Rf_error(_("not a weak reference"));

	WeakRef *wr = SEXP_downcast<WeakRef *>(w, false);

	return wr->key();
}

SEXP R_WeakRefValue(SEXP w)
{
	if (TYPEOF(w) != WEAKREFSXP)
		Rf_error(_("not a weak reference"));

	WeakRef *wr = SEXP_downcast<WeakRef *>(w, false);

	SEXP v = wr->value();
	if (v)
		ENSURE_NAMEDMAX(v);
	return v;
}

void R_RunWeakRefFinalizer(SEXP x)
{
	WeakRef::runWeakRefFinalizer(x);
}

bool RunFinalizers(void)
{
	return WeakRef::runFinalizers();
}

void R_RunExitFinalizers(void)
{
	R_checkConstants(TRUE);
	WeakRef::runExitFinalizers();
}

void R_RunPendingFinalizers(void)
{
	WeakRef::runPendingFinalizers();
}

void R_RegisterFinalizerEx(SEXP s, SEXP fun, Rboolean onexit)
{
	R_MakeWeakRef(s, nullptr, fun, onexit);
}

void R_RegisterFinalizer(SEXP s, SEXP fun)
{
	R_RegisterFinalizerEx(s, fun, FALSE);
}

void R_RegisterCFinalizerEx(SEXP s, R_CFinalizer_t fun, Rboolean onexit)
{
	R_MakeWeakRefC(s, nullptr, fun, onexit);
}

void R_RegisterCFinalizer(SEXP s, R_CFinalizer_t fun)
{
	R_RegisterCFinalizerEx(s, fun, FALSE);
}
