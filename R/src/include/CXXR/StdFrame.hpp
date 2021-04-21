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
 *  This header file is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
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

/** @file StdFrame.hpp
 * @brief Class CXXR::StdFrame.
 */

#ifndef STDFRAME_HPP
#define STDFRAME_HPP

#include <unordered_map>
#include <CXXR/Allocator.hpp>
#include <CXXR/Frame.hpp>

namespace CXXR
{
	/** @brief General-purpose implementation of CXXR::Frame.
	 */
	class StdFrame : public Frame
	{
	private:
		typedef std::unordered_map<const Symbol *, Binding,
								   std::hash<const Symbol *>,
								   std::equal_to<const Symbol *>,
								   CXXR::Allocator<std::pair<const Symbol *,
															 Binding>>>
			map;

	public:
		/**
		 * @param initial_capacity A hint to the implementation that
		 *          the constructed StdFrame should be
		 *          configured to have capacity for at least \a
		 *          initial_capacity Bindings.  This does not impose an
		 *          upper limit on the capacity of the StdFrame,
		 *          but some reconfiguration (and consequent time
		 *          penalty) may occur if it is exceeded.
		 */
		explicit StdFrame(size_t initial_capacity = 15);
		// Why 15?  Because if the implementation uses a prime number
		// hash table sizing policy, this will result in the
		// allocation of a hash table array comprising 31 buckets.  On
		// a 32-bit architecture, this will fit well into two 64-byte
		// cache lines.

		// Virtual functions of Frame (qv):
		PairList *asPairList() const override;
		Binding *binding(const Symbol *symbol) override;
		const Binding *binding(const Symbol *symbol) const override;
		void clear() override;
		bool erase(const Symbol *symbol) override;
		void lockBindings() override;
		Binding *obtainBinding(const Symbol *symbol) override;
		size_t size() const override;

		// Virtual function of GCNode:
		void visitReferents(const_visitor *v) const override;

	private:
		map m_map;

		// Declared private to ensure that StdFrame objects are
		// created only using 'new':
		~StdFrame() {}

		// Not (yet) implemented.  Declared to prevent
		// compiler-generated versions:
		StdFrame(const Frame &);
		StdFrame &operator=(const Frame &);
	};
} // namespace CXXR

#endif // STDFRAME_HPP
