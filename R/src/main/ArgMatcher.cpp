/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999--2020  The R Core Team.
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
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
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

/** @file ArgMatcher.cpp
 *
 * Implementation of class ArgMatcher.
 */

#include <CXXR/ArgMatcher.hpp>
#include <CXXR/ArgList.hpp>
#include <CXXR/DottedArgs.hpp>
#include <CXXR/Environment.hpp>
#include <CXXR/GCStackRoot.hpp>
#include <CXXR/PairList.hpp>
#include <CXXR/Promise.hpp>
#include <CXXR/Symbol.hpp>
#include <Localization.h>
#include "boost/functional/hash.hpp"

using namespace std;
using namespace CXXR;

namespace CXXR
{
	bool ArgMatcher::s_warn_on_partial_match = false;

	ArgMatcher::ArgMatcher(const PairList *formals)
		: m_dots_position(-1)
	{
		m_formals = formals;

		for (const PairList *f = formals; f; f = f->tail())
		{
			const Symbol *sym = dynamic_cast<const Symbol *>(f->tag());
			if (!sym)
				Rf_error(_("invalid formal arguments for 'function'"));
			if (sym == DotsSymbol)
			{
				if (has3Dots())
					Rf_error(_("formals list contains more than one '...'"));
				if (f->car() != Symbol::missingArgument())
					Rf_error(_("'...' formal cannot have a default value"));
				m_dots_position = m_formal_data.size();
			}
			else
			{
				pair<FormalMap::const_iterator, bool> pr =
					m_formal_index.insert(make_pair(sym->name(),
													m_formal_data.size()));
				if (!pr.second)
					Rf_error(_("duplicated name in formals list"));
			}

			FormalData fdata = {sym, has3Dots(), f->car(),
								static_cast<unsigned>(m_formal_data.size())};
			m_formal_data.push_back(fdata);
		}
	}

	bool ArgMatcher::isPrefix(const String *shorter, const String *longer)
	{
		size_t length = shorter->size();
		return strncmp(shorter->c_str(), longer->c_str(), length) == 0;
	}

	ArgMatcher *ArgMatcher::make(Symbol *fml1, Symbol *fml2, Symbol *fml3,
								 Symbol *fml4, Symbol *fml5, Symbol *fml6)
	{
		GCStackRoot<PairList> formals;
		if (fml6)
			formals = PairList::construct(Symbol::missingArgument(), formals, fml6);
		if (fml5)
			formals = PairList::construct(Symbol::missingArgument(), formals, fml5);
		if (fml4)
			formals = PairList::construct(Symbol::missingArgument(), formals, fml4);
		if (fml3)
			formals = PairList::construct(Symbol::missingArgument(), formals, fml3);
		if (fml2)
			formals = PairList::construct(Symbol::missingArgument(), formals, fml2);
		if (fml1)
			formals = PairList::construct(Symbol::missingArgument(), formals, fml1);
		return GCNode::expose(new ArgMatcher(formals));
	}

	void ArgMatcher::makeBinding(Environment *target_env, const FormalData &fdata,
								 Frame::Binding::Origin origin,
								 RObject *value)
	{
#if CXXR_TRUE
		std::cerr << "makeBinding(...) not yet implemented" << std::endl;
		abort();
#else
		if (origin == Frame::Binding::DEFAULTED)
		{
			if (fdata.default_value != Symbol::missingArgument())
				value = GCNode::expose(new Promise(fdata.default_value, target_env));
		}
		Frame::Binding *bdg = target_env->frame()->obtainBinding(fdata.symbol);
		// Don't trump a previous binding with Symbol::missingArgument() :
		if (value != Symbol::missingArgument())
			bdg->setValue(value, origin);
#endif
	}

	class ArgMatcher::MatchCallback
	{
	public:
		typedef ArgMatcher::FormalData FormalData;
		typedef ArgMatcher::SuppliedData SuppliedData;
		typedef ArgMatcher::SuppliedList SuppliedList;
		typedef boost::iterator_range<std::vector<int>::const_iterator> ArgIndices;

		virtual void matchedArgument(const FormalData &formal,
									 int arg_index,
									 RObject *value) = 0;

		virtual void defaultValue(const FormalData &formal) = 0;

		virtual void dottedArgs(const FormalData &formal,
								ArgIndices arg_indices,
								const ArgList &all_args) = 0;

		static DottedArgs *makeDottedArgs(ArgIndices arg_indices,
										  const ArgList &all_args)
		{
			DottedArgs *result = nullptr;
			ConsCell *dots = nullptr;
			for (int index : arg_indices)
			{
				RObject *value = all_args.get(index);
				const RObject *tag = all_args.getTag(index);
				if (dots)
				{
					PairList *next_item = GCNode::expose(new PairList(value, nullptr, tag));
					dots->setTail(next_item);
					dots = next_item;
				}
				else
				{
					dots = result = GCNode::expose(new DottedArgs(value, nullptr, tag));
				}
			}
			return result;
		}
	};

	void ArgMatcher::matchWithCache(const ArgList &supplied,
									MatchCallback *callback,
									const ArgMatchCache *matching) const
	{
		for (size_t findex = 0; findex < m_formal_data.size(); findex++)
		{
			const FormalData &fdata = m_formal_data[findex];
			int sindex = matching->getSindex(findex);
			if (sindex >= 0)
			{
				// This assumes that random access in an ArgList is O(1).
				callback->matchedArgument(fdata, sindex, supplied.get(sindex));
			}
			else if (fdata.symbol != DotsSymbol)
			{
				callback->defaultValue(fdata);
			}
			else
			{
				callback->dottedArgs(fdata, matching->getDotArgs(), supplied);
			}
		}
	}

	struct ArgMatchCache::Hash
	{
		size_t operator()(const ArgMatchCache *item) const
		{
			size_t seed = 0;
			boost::hash_combine(seed, item->m_num_formals);
			boost::hash_combine(seed, item->m_values);
			return seed;
		}
	};

	namespace
	{

		class ClosureMatchCallback : public ArgMatcher::MatchCallback
		{
		public:
			ClosureMatchCallback(Environment *target_env)
				: m_target_env(target_env) {}

			Environment *targetEnvironment() const
			{
				return m_target_env;
			}

			void matchedArgument(const FormalData &formal,
								 int arg_index, RObject *value) override
			{
#if CXXR_TRUE
				std::cerr << "matchedArgument(...) not yet implemented" << std::endl;
				abort();
#else
				// If the value was missing, then use the default instead.
				if (value == Symbol::missingArgument())
				{
					defaultValue(formal);
					return;
				}

				m_target_env->frame()->bind(formal.symbol, value,
											Frame::Binding::EXPLICIT);
#endif
			}

			void defaultValue(const FormalData &formal) override
			{
#if CXXR_TRUE
				std::cerr << "defaultValue(...) not yet implemented" << std::endl;
				abort();
#else
				if (formal.default_value == Symbol::missingArgument())
				{
					// Create a value bound to Symbol::missingArgument()
					m_target_env->frame()->obtainBinding(formal.symbol);
				}
				else
				{
					RObject *value = GCNode::expose(new Promise(formal.default_value, m_target_env));
					m_target_env->frame()->bind(formal.symbol, value,
												Frame::Binding::DEFAULTED);
				}
#endif
			}

			void dottedArgs(const FormalData &formal,
							ArgIndices arg_indices,
							const ArgList &all_args) override
			{
#if CXXR_TRUE
				std::cerr << "dottedArgs(...) not yet implemented" << std::endl;
				abort();
#else
				if (arg_indices.empty())
				{
					m_target_env->frame()->obtainBinding(DotsSymbol);
					return;
				}

				DottedArgs *dots = makeDottedArgs(arg_indices, all_args);
				m_target_env->frame()->bind(DotsSymbol, dots,
											Frame::Binding::EXPLICIT);
#endif
			}

		private:
			Environment *m_target_env;
		};

		class InitializerListMatchCallback : public ArgMatcher::MatchCallback
		{
		public:
			InitializerListMatchCallback(
				std::initializer_list<RObject **> matched_values)
				: m_matched_values(matched_values) {}

			void matchedArgument(const FormalData &formal,
								 int arg_index, RObject *value) override
			{
				*(m_matched_values.begin()[formal.index]) = value;
			}

			void defaultValue(const FormalData &formal) override
			{
				*(m_matched_values.begin()[formal.index]) = R_MissingArg;
			}

			void dottedArgs(const FormalData &formal,
							ArgIndices arg_indices,
							const ArgList &all_args) override
			{
				if (arg_indices.empty())
				{
					*(m_matched_values.begin()[formal.index]) = R_MissingArg;
				}
				else
				{
					*(m_matched_values.begin()[formal.index]) = makeDottedArgs(arg_indices, all_args);
				}
			}

		private:
			std::initializer_list<RObject **> m_matched_values;
		};

		class PairListMatchCallback : public ArgMatcher::MatchCallback
		{
		public:
			PairListMatchCallback(int size) : m_values(size) {}

			void matchedArgument(const FormalData &formal,
								 int arg_index, RObject *value) override
			{
				add(formal, value);
			}

			void defaultValue(const FormalData &formal) override
			{
				add(formal, R_MissingArg);
			}

			void dottedArgs(const FormalData &formal,
							ArgIndices arg_indices,
							const ArgList &all_args) override
			{
				if (arg_indices.empty())
				{
					add(formal, R_MissingArg);
				}
				else
				{
					add(formal, makeDottedArgs(arg_indices, all_args));
				}
			}

			void add(const FormalData &formal, RObject *value)
			{
				m_values[formal.index] = std::make_pair(formal.symbol, value);
			}

			PairList *get()
			{
				PairList *list = nullptr;
				while (!m_values.empty())
				{
					RObject *value = m_values.back().second;
					list = GCNode::expose(new PairList(value, list, m_values.back().first));
					m_values.pop_back();
				}
				return list;
			}

		private:
			std::vector<std::pair<const Symbol *, RObject *>> m_values;
		};

		class RecordArgMatchInfoCallback : public ArgMatcher::MatchCallback
		{
		public:
			RecordArgMatchInfoCallback(ArgMatchCache *matching) : m_matching(matching) {}

			void matchedArgument(const FormalData &formal,
								 int arg_index, RObject *value) override
			{
				m_matching->m_values[formal.index] = arg_index;
			}

			void defaultValue(const FormalData &formal) override
			{
				m_matching->m_values[formal.index] = -1;
			}

			void dottedArgs(const FormalData &formal,
							ArgIndices arg_indices,
							const ArgList &all_args) override
			{
				m_matching->m_values.insert(m_matching->m_values.end(),
											arg_indices.begin(), arg_indices.end());
			}

		private:
			ArgMatchCache *m_matching;
		};

		template <class T>
		struct DereferencedEquality
		{
			bool operator()(const T *lhs, const T *rhs) const
			{
				if (lhs != nullptr && rhs != nullptr)
				{
					return *lhs == *rhs;
				}
				return lhs == rhs;
			}
		};

	} // namespace

	ArgMatchCache::ArgMatchCache(int num_formals, const ArgList &args)
		: m_num_formals(num_formals), m_values(num_formals, -1)
	{
		for (const ConsCell &arg : args.getArgs())
		{
			m_tags.push_back(SEXP_downcast<const Symbol *>(arg.tag()));
		}
	}

	bool ArgMatchCache::arglistTagsMatch(const ArgList &args) const
	{
		if (m_tags.size() != args.size())
			return false;
		for (size_t index = 0; index < m_tags.size(); ++index)
		{
			if (m_tags[index] != args.getTag(index))
				return false;
		}
		return true;
	}

	const ArgMatchCache *ArgMatcher::createMatchCache(const ArgList &args) const
	{
		if (args.has3Dots())
			return nullptr;

		ArgMatchCache *matching = GCNode::expose(new ArgMatchCache(numFormals(), args));
		matching->m_matcher = this;
		RecordArgMatchInfoCallback callback(matching);
		match(args, &callback);

		// TODO: intern the values to conserve memory use.
		return matching;
	}

	void ArgMatcher::match(Environment *target_env, const ArgList &supplied) const
	{
		ClosureMatchCallback callback(target_env);
		match(supplied, &callback);
	}

	void ArgMatcher::match(const ArgList &supplied,
						   std::initializer_list<RObject **> matched_values) const
	{
		InitializerListMatchCallback callback(matched_values);
		match(supplied, &callback);
	}

	void ArgMatcher::match(Environment *target_env, const ArgList &supplied,
						   GCEdge<const ArgMatchCache> *cache) const
	{
		assert(supplied.status() != ArgList::RAW);
		if (!*cache || (*cache)->m_matcher != this || !(*cache)->arglistTagsMatch(supplied))
		{
			*cache = createMatchCache(supplied);
		}
		if (!(*cache))
			match(target_env, supplied);

		ClosureMatchCallback callback(target_env);
		matchWithCache(supplied, &callback, cache->get());
	}

	PairList *ArgMatcher::matchToPairList(const ArgList &supplied,
										  const Expression *expression) const
	{
		PairListMatchCallback callback(numFormals());
		match(supplied, &callback);
		return callback.get();
	}

	void ArgMatcher::matchByPosition(const ArgList &supplied,
									 MatchCallback *callback) const
	{
		const size_t num_formals = m_dots_position == -1 ? m_formal_data.size() : m_dots_position;
		unsigned int supplied_index = 0;
		size_t formals_index = 0;
		ArgList::const_iterator s = supplied.getArgs().begin();
		const ArgList::const_iterator end = supplied.getArgs().end();
		for (;
			 (s != end) && (formals_index < num_formals);
			 ++s, ++formals_index, ++supplied_index)
		{
			callback->matchedArgument(m_formal_data[formals_index],
									  supplied_index, s->car());
		}
		// Set unmatched arguments to their default values.
		for (; formals_index < m_formal_data.size(); ++formals_index)
		{
			if (int(formals_index) != m_dots_position)
				callback->defaultValue(m_formal_data[formals_index]);
		}

		// Any remaining supplied args are either rolled into ... or
		// there's an error:
		if (has3Dots())
		{
			// Pass unmatched arguments.
			vector<int> dotted_arg_indices;
			for (; s != end; ++supplied_index, ++s)
			{
				dotted_arg_indices.push_back(supplied_index);
			}
			callback->dottedArgs(m_formal_data[m_dots_position],
								 boost::make_iterator_range(dotted_arg_indices),
								 supplied);
		}
		else
		{
			if (s != end)
			{
				unusedArgsError(&*s);
			}
		}
	}

	void ArgMatcher::match(const ArgList &supplied,
						   MatchCallback *callback) const
	{
		// Short-circuit if none of the arguments are named.
		if (!supplied.hasTags())
		{
			matchByPosition(supplied, callback);
			return;
		}

		vector<MatchStatus, Allocator<MatchStatus>>
			formals_status(m_formal_data.size(), UNMATCHED);
		SuppliedList supplied_list;
		// Exact matches by tag:
		{
			unsigned int sindex = 0;
			for (const ConsCell &s : supplied.getArgs())
			{
				const Symbol *tag = static_cast<const Symbol *>(s.tag());
				const String *name = (tag ? tag->name() : nullptr);
				RObject *value = s.car();
				FormalMap::const_iterator fmit = (name ? m_formal_index.lower_bound(name)
													   : m_formal_index.end());
				if (fmit != m_formal_index.end() && (*fmit).first == name)
				{
					// Exact tag match:
					unsigned int findex = (*fmit).second;
					formals_status[findex] = EXACT_TAG;
					callback->matchedArgument(m_formal_data[findex], sindex,
											  value);
				}
				else
				{
					// No exact tag match, so place supplied arg on list:
					SuppliedData supplied_data = {tag, value, fmit, sindex};
					supplied_list.push_back(supplied_data);
				}
				++sindex;
			}
		}
		// Partial matches by tag:
		{
			SuppliedList::iterator slit = supplied_list.begin();
			while (slit != supplied_list.end())
			{
				SuppliedList::iterator next = slit;
				++next;
				const SuppliedData &supplied_data = *slit;
				const String *supplied_name = (supplied_data.tag ? supplied_data.tag->name() : nullptr);
				FormalMap::const_iterator fmit = supplied_data.fm_iter;
				// Within m_formal_index, skip formals formals following
				// '...' and formals with exact matches:
				while (fmit != m_formal_index.end() && (m_formal_data[(*fmit).second].follows_dots || formals_status[(*fmit).second] == EXACT_TAG))
					++fmit;
				if (fmit != m_formal_index.end() && isPrefix(supplied_name, (*fmit).first))
				{
					// This is a potential partial match.  Remember the formal:
					unsigned int findex = (*fmit).second;
					// Has this formal already been partially matched? :
					if (formals_status[(*fmit).second] == PARTIAL_TAG)
						Rf_error(_("formal argument '%s' matched by multiple actual arguments"),
								 (*fmit).first->c_str());
					// Does supplied arg partially match anything else? :
					do
					{
						++fmit;
					} while (fmit != m_formal_index.end() && formals_status[(*fmit).second] == EXACT_TAG);
					if (fmit != m_formal_index.end() && isPrefix(supplied_name, (*fmit).first))
						Rf_error(_("argument %d matches multiple formal arguments"),
								 supplied_data.index + 1);
					// Partial match is OK:
					if (s_warn_on_partial_match)
						Rf_warning(_("partial argument match of '%s' to '%s'"),
								   supplied_name->c_str(),
								   (*fmit).first->c_str());
					formals_status[findex] = PARTIAL_TAG;
					callback->matchedArgument(m_formal_data[findex], supplied_data.index, supplied_data.value);
					supplied_list.erase(slit);
				}
				slit = next;
			}
		}
		// Positional matching and default values:
		{
			const size_t numformals = m_formal_data.size();
			SuppliedList::iterator slit = supplied_list.begin();
			for (unsigned int findex = 0; findex < numformals; ++findex)
			{
				if (formals_status[findex] == UNMATCHED)
				{
					const FormalData &fdata = m_formal_data[findex];
					// Skip supplied arguments with tags:
					while (slit != supplied_list.end() && (*slit).tag)
						++slit;
					if (slit != supplied_list.end() && !fdata.follows_dots)
					{
						// Handle positional match:
						const SuppliedData &supplied_data = *slit;
						formals_status[findex] = POSITIONAL;
						callback->matchedArgument(fdata, supplied_data.index,
												  supplied_data.value);
						supplied_list.erase(slit++);
					}
					else if (int(findex) != m_dots_position)
					{
						callback->defaultValue(fdata);
					}
				}
			}
		}

		// Any remaining supplied args are either rolled into ... or
		// there's an error:
		if (has3Dots())
		{
			vector<int> dotted_arg_indices;
			for (const auto &sitem : supplied_list)
			{
				dotted_arg_indices.push_back(sitem.index);
			}
			callback->dottedArgs(m_formal_data[m_dots_position],
								 boost::make_iterator_range(dotted_arg_indices),
								 supplied);
		}
		else if (!supplied_list.empty())
			unusedArgsError(supplied_list);
	}

	void ArgMatcher::propagateFormalBindings(const Environment *fromenv,
											 Environment *toenv) const
	{
#if CXXR_TRUE
		std::cerr << "propagateFormalBindings(...) not yet implemented" << std::endl;
		abort();
#else
		const Frame *fromf = fromenv->frame();
		for (const FormalData &fdata : m_formal_data)
		{
			const Symbol *symbol = fdata.symbol;
			const Frame::Binding *frombdg = fromf->binding(symbol);
			if (!frombdg)
				Rf_error(_("could not find symbol \"%s\" in environment of the generic function"),
						 symbol->name()->c_str());
			RObject *val = frombdg->unforcedValue();
			if (frombdg->origin() == Frame::Binding::EXPLICIT)
			{
				makeBinding(toenv, fdata, Frame::Binding::EXPLICIT, val);
			}
			else
			{
				// Discard generic's defaults:
				makeBinding(toenv, fdata, Frame::Binding::DEFAULTED,
							fdata.default_value);
			}
		}
		// m_formal_data excludes '...', so:
		if (has3Dots())
		{
			const Frame::Binding *frombdg = fromf->binding(DotsSymbol);
			toenv->frame()->importBinding(frombdg);
		}
#endif
	}

	void ArgMatcher::stripFormals(Frame *input_frame) const
	{
		const PairList *fcell = m_formals;
		while (fcell)
		{
			input_frame->erase(static_cast<const Symbol *>(fcell->tag()));
			fcell = fcell->tail();
		}
	}

	// Implementation of ArgMatcher::unusedArgsError() is in match.cpp

	void ArgMatcher::visitReferents(const_visitor *v) const
	{
		if (m_formals)
			m_formals->conductVisitor(v);
	}

	PairList *ArgMatcher::makePairList(std::initializer_list<const char *> arg_names)
	{
		PairList *result = PairList::makeList(arg_names.size());
		auto result_iter = result->begin();
		for (const char *arg : arg_names)
		{
			result_iter->setTag(Symbol::obtain(arg));
			result_iter->setCar(R_MissingArg);
			++result_iter;
		}
		return result;
	}
} // namespace CXXR