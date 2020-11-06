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
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

/** @file GCRoot.hpp
 *
 * @brief Templated class R::GCRoot and its untemplated base class
 * R::GCRootBase.
 */

#ifndef GCROOT_HPP
#define GCROOT_HPP

#include <vector>
#include <CXXR/GCNode.hpp>

namespace R
{
	class RObject;

	/** @brief Untemplated base class for GCRoot.
     */
	class GCRootBase
	{
	public:
		explicit GCRootBase(GCNode *node)
			: m_index(s_roots.size())
		{
			s_roots.push_back(node);
		}

		GCRootBase(const GCRootBase &source)
			: m_index(s_roots.size())
		{
			s_roots.push_back(s_roots[source.m_index]);
		}

		~GCRootBase()
		{
			s_roots.pop_back();
			if (m_index != s_roots.size())
				seq_error();
		}

		GCRootBase &operator=(const GCRootBase &source)
		{
			s_roots[m_index] = s_roots[source.m_index];
			return *this;
		}

		GCRootBase &operator=(GCNode *node)
		{
			s_roots[m_index] = node;
			return *this;
		}

		GCNode *ptr() const
		{
			return s_roots[m_index];
		}

		/**
	 * Conduct a GCNode::const_visitor object to each root GCNode.
	 *
	 * @param v Pointer to the const_visitor object.
	 */
		static void visitRoots(GCNode::const_visitor *v);

		/**
	 * Conduct a GCNode::visitor object to each root GCNode.
	 *
	 * @param v Pointer to the visitor object.
	 */
		static void visitRoots(GCNode::visitor *v);

	private:
		static std::vector<GCNode *> s_roots;

		unsigned int m_index;

		static void seq_error();
	};

	/**
     * This class encapsulates a pointer to an object of a type
     * derived from GCNode.  For as long as the GCRoot object exists,
     * the GCNode that it points to will not be garbage collected.
     *
     * GCRoot objects are intended to be allocated on the stack, or at
     * file or global scope: the class implementation requires that
     * GCRoot objects are destroyed in the reverse order of creation,
     * and the destructor checks this.
     *
     * @@tparam T A pointer to GCNode or a type publicly derived from
     *          GCNode.  There is at present no provision for const
     *          pointers to be encapsulated within a GCRoot.
     */
	template <class T = RObject *>
	class GCRoot : public GCRootBase
	{
	public:
		/**
	 * @param node Pointer the node to be pointed to, and
	 *          protected from the garbage collector, or a null
	 *          pointer.
	 */
		explicit GCRoot(T node = 0) : GCRootBase(node) {}

		/** @brief Copy constructor.
	 *
	 * The constructed GCRoot will protect the same GCNode as
	 * source.  (There is probably no reason to use this
	 * constructor.)
	 */
		GCRoot(const GCRoot &source) : GCRootBase(source) {}

		/** Upcast constructor
	 *
	 * This constructor enables a GCRoot<Derived*> to be
	 * implicitly converted to a GCRoot<Base*>.
	 */
		template <class U>
		GCRoot(const GCRoot<U> &source)
			: GCRootBase(T(source))
		{
		}

		/**
	 * This will cause this GCRoot to protect the same GCNode as
	 * is protected by source.  (There is probably no reason to
	 * use this method.)
	 */
		GCRoot &operator=(const GCRoot &source)
		{
			GCRootBase::operator=(source);
			return *this;
		}

		/**
	 * This will cause this GCRoot to point to and protect node,
	 * instead of the node (if any) it currently points to and
	 * protects.
	 *
	 * @param node Pointer to the GCNode that is now to be pointed
	 *          to and protected from the garbage collector.
	 */
		GCRoot operator=(T node)
		{
			GCRootBase::operator=(node);
			return *this;
		}

		/**
	 * @return the pointer currently encapsulated by the node.
	 * The pointer is of type T* const to prevent its use as an
	 * lvalue, the effect of which would probably not be what the
	 * programmer expected.
	 */
		operator T const() const
		{
			return static_cast<T>(ptr());
		}
	};
} // namespace R

#endif // GCROOT_HPP
