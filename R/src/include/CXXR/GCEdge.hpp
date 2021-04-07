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
 *  http://www.r-project.org/Licenses/
 */

/** @file GCEdge.hpp
 *
 * @brief Templated class CXXR::GCEdge.
 */

#ifndef GCEDGE_HPP
#define GCEDGE_HPP

#include <CXXR/GCNode.hpp>

namespace CXXR
{
	class RObject;

	/** @brief Directed edge in the graph whose nodes are GCNode objects.
	 *
	 * This class encapsulates a pointer from one GCNode to another,
	 * and carries out housekeeping required by the garbage collection
	 * scheme.  The class name reflects the fact that these objects
	 * represent directed edges in the directed graph with the GCNode
	 * objects as its nodes.
	 *
	 * Whenever an object of a type derived from GCNode needs to refer
	 * to another such object, it should do so by containing a GCEdge
	 * object, rather than by containing a pointer or reference
	 * directly.
	 *
	 * @tparam T GCNode or a type publicly derived from GCNode.  This
	 *          may be qualified by const, so for example a const
	 *          String* may be encapsulated in a GCEdge using the type
	 *          GCEdge<const String>.
	 */
	template <class T = RObject>
	class GCEdge
	{
	public:
		typedef T type;
		/** @brief Default constructor.
		 *
		 * @note Why can't I specify the target in the constructor?
		 * Suppose that <tt>Foo</tt>, Bar and \c Baz are all classes
		 * derived from GCNode, and that a \c Foo object in effect
		 * 'contains' a \c Bar and a <tt>Baz</tt>.  If it were
		 * possibly to initialize a GCEdge in its constructor, it
		 * would be tempting to implement the \c Foo constructor as
		 * follows:
		 * <pre>
		 * Foo()
		 *      : m_edge1(new Bar), m_edge2(new Baz)
		 * {}
		 * </pre>
		 * But now consider what would happen if the call <tt>new
		 * Bar</tt> resulted in a garbage collection.  Then the
		 * visitReferents() function of the object under construction
		 * may be called before the field <tt>m_edge2</tt> has been
		 * initialized, i.e. when it still contains junk, and this
		 * will result in undefined behaviour, probably a program
		 * crash.  This bug would remain latent until a garbage
		 * collection happened at precisely this point.
		 */
		GCEdge() : m_target(nullptr) {}

		T *operator->() const { return get(); }

		/** @brief Extract encapsulated pointer
		 *
		 * @return The encapsulated pointer.
		 */
		operator T *() const { return get(); }

		/** @brief Access the target pointer.
		 *
		 * @return pointer to the current target (if any) of the edge.
		 */
		T *get() const { return m_target; }

		/** @brief Present the target (if any) of this GCEdge to a
		 *  visitor.
		 *
		 * @param v Reference to the visitor object.
		 */
		void conductVisitor(GCNode::const_visitor *v) const
		{
			if (m_target)
				m_target->conductVisitor(v);
		}

		/** @brief Redirect the GCEdge to point at a (possibly) different node.
		 *
		 * @param from This \e must point to the GCNode object that
		 *          contains this GCEdge object.
		 *
		 * @param to Pointer to the object to which reference is now
		 *           to be made.
		 */
		void retarget(GCNode *from, T *to)
		{
			m_target = to;
			// from->propagateAge(to);
		}

	private:
		T *m_target;

		// Not implemented (yet).  Declared to prevent
		// compiler-generated versions:
		GCEdge(const GCEdge &);
		GCEdge &operator=(const GCEdge &);
	};
} // namespace CXXR

#endif // GCEDGE_HPP
