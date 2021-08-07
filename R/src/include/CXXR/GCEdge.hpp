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

#include <CXXR/ElementTraits.hpp>
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

		/** @brief Copy constructor.
		 *
		 * @param source GCEdge to be copied.  The constructed GCEdge
		 * will point to the same object (if any) as \a source.
		 */
		GCEdge(const GCEdge<T> &source) : m_target(source.m_target)
		{
			check_complete_type();
			GCNode::incRefCount(m_target);
			if (m_target)
				m_target->propagateAge(m_target);
		}

		~GCEdge()
		{
			check_complete_type();
			// TODO: Below code results in segfault during byte
			// compilation for package 'utils'
			// GCNode::decRefCount(m_target);
		}

		GCEdge<T> &operator=(const GCEdge<T> &source)
		{
			retarget(source);
			return *this;
		}

		GCEdge<T> &operator=(T *newtarget)
		{
			retarget(newtarget);
			return *this;
		}

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

		void detach()
		{
			check_complete_type();
			GCNode::decRefCount(m_target);
			m_target = nullptr;
		}

		/** @brief Propagate age to prevent old-to-new references.
		 *
		 * @param from This \e must point to the GCNode object that
		 *          contains this GCEdge object.
		 *
		 * @note To be removed in future.
		 */
		void propagateAge(GCNode *from)
		{
			if (from)
				from->propagateAge(m_target);
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
			// retarget(to);
			check_complete_type();
			if (m_target == to)
				return;
			if (from && from->trackrefs())
				GCNode::incRefCount(to);
			T *oldtarget = m_target;
			m_target = to;
			if (from && from->trackrefs())
				GCNode::decRefCount(oldtarget);
			propagateAge(from);
		}

		void clearCar()
		{
			m_target = nullptr;
		}

	protected:
		/** @brief Designate the node to be pointed to by this GCEdge.
		 *
		 * This function is like retarget(), but skips age
		 * propagation.  It should be used only in the constructors of
		 * derived classes.
		 *
		 * @param to Pointer to the object to this GCEdge is to refer.
		 */
		void setTarget(T *to)
		{
			GCNode::incRefCount(to);
			T *oldtarget = m_target;
			m_target = to;
			GCNode::decRefCount(oldtarget);
		}

	private:
		T *m_target;

		/** @brief Redirect the GCEdge to point at a (possibly) different node.
		 *
		 * @param newtarget Pointer to the object to which reference is now
		 *           to be made.
		 */
		void retarget(T *newtarget)
		{
			check_complete_type();
			if (m_target == newtarget)
				return;
			GCNode::incRefCount(newtarget);
			T *oldtarget = m_target;
			m_target = newtarget;
			GCNode::decRefCount(oldtarget);
			if (m_target)
				m_target->propagateAge(m_target);
		}

		// A GCEdge is a pointer, not an array.
		T &operator[](size_t) const = delete;

		static void check_complete_type()
		{
			static_assert(sizeof(T) >= 0, "T must be a complete type");
		}
	};
} // namespace CXXR

#endif // GCEDGE_HPP
