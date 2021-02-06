/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2006   The R Development Core Team.
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

/** @file S4Object.hpp
 *
 * @brief Class CXXR::S4Object and associated C interface.
 *
 * (S4Object implements S4SXP.)
 */

#ifndef S4OBJECT_HPP
#define S4OBJECT_HPP

#include <CXXR/RObject.hpp>
#include <CXXR/SEXP_downcast.hpp>

namespace CXXR
{
	/** @brief S4 object.
     *
     * This class is used to implement S4 classes that do not extend
     * objects of another R type, and corresponds to the ::SEXPTYPE
     * S4SXP.
     *
     */
	class S4Object : public RObject
	{
	public:
		/** @brief Default constructor.
		 */
		S4Object()
			: RObject(S4SXP)
		{
			setS4Object(true);
		}

		/** @brief Copy constructor.
		 *
		 * @param pattern S4Object to be copied.
		 *
		 * @param deep Indicator whether to perform deep or shallow copy.
		 */
		S4Object(const S4Object &pattern, bool deep)
			: RObject(pattern, deep)
		{
		}

		/**
         * @return a const pointer to the 'tag' of this S4Object
         * element.
         */
		const RObject *tag() const
		{
			return m_tag;
		}

		/**
         * @return a pointer to the 'tag' of this S4Object.
         */
		RObject *tag()
		{
			return m_tag;
		}

		/** @brief Set the 'tag' value.
         *
         * @param tg Pointer to the new tag object (or a null
         *           pointer).
         */
		void setTag(RObject *tg)
		{
			m_tag = tg;
			propagateAge(m_tag);
		}

		static RObject *tag(RObject *x);
		static void set_tag(RObject *x, RObject *v);
		// Virtual function of GCNode:
		void visitChildren(const_visitor *v) const override;

		/** @brief The name by which this type is known in R.
		 *
		 * @return the name by which this type is known in R.
		 */
		static const char *staticTypeName()
		{
			return "S4";
		}

		// Virtual functions of RObject:
		S4Object *clone(bool deep) const override;
		const char *typeName() const override;

	private:
		RObject *m_tag;
		// Declared private to ensure that S4Objects are allocated
		// only using 'new':
		~S4Object() {}

		// Not implemented.  Declared to prevent compiler-generated version:
		S4Object &operator=(const S4Object &);
	};
} // namespace CXXR

extern "C"
{
	/** @brief Get tag of CXXR::S4Object.
     *
     * @param e Pointer to a CXXR::S4Object (checked), or a null pointer.
     * @return Pointer to the tag of the list element, or 0 if \a e is
     * a null pointer.
     */
	SEXP S4TAG(SEXP e);

	/**
     * @brief Set the tag of a CXXR::S4Object.
     *
     * @param x Pointer to a CXXR::S4Object (checked).
     * @param y Pointer a CXXR::RObject representing the new tag of
     *          the CXXR::S4Object.
     */
	void SET_S4TAG(SEXP x, SEXP y);

	/** @brief Is this an S4 object?
     *
     * @param x Pointer to \c RObject.
     * @return true iff \a x is an S4 object.  Returns false if \a x
     * is 0.
     */
	int IS_S4_OBJECT(SEXP x);

	/**
     * @deprecated Ought to be private.
     */
	void SET_S4_OBJECT(SEXP x);

	/**
     * @deprecated Ought to be private.
     */
	void UNSET_S4_OBJECT(SEXP x);

	/** @brief Create an S4 object.
     *
     * @return Pointer to the created object.
     */
	SEXP Rf_allocS4Object();
} // extern "C"

#endif /* S4OBJECT_HPP */
