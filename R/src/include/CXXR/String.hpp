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

/** @file String.hpp
 * @brief Class CXXR::String and associated C interface.
 */

#ifndef CXXR_STRING_HPP
#define CXXR_STRING_HPP

// #include <Rinternals.h>
#include <CXXR/Allocator.hpp>
#include <CXXR/GCRoot.hpp>
#include <CXXR/SEXP_downcast.hpp>
#include <CXXR/VectorBase.hpp>
#include <CXXR/Symbol.hpp>
#include <string>
#include <unordered_map>

extern "C" SEXP R_NaString;

namespace CXXR
{
	/** @brief RObject representing a character string.
     *
     * At any one time, at most one String object with a particular
     * text and encoding may exist.
     *
     * @note When the method size() is applied to a
     * String, it returns the number of <tt>char</tt>s that the String
     * comprises.  If the string uses a multibyte encoding scheme,
     * this may be different from the number of Unicode characters
     * represented by the string.
     */
	class String : public VectorBase
	{
	public:
		/* @brief Comparison object for CXXR::String.
		 *
		 * STL-compatible comparison class for comparing CXXR::String
		 * objects.
		 */
		class Comparator
		{
		public:
			/**
			 * @param na_last if true, the 'not available' string will
			 *          come after all other strings in the sort
			 *          ordering; if false, it will come before all
			 *          other strings.
			 */
			Comparator(bool na_last = true)
				: m_na_last(na_last)
			{
			}

			/** @brief Comparison operation.
			 * @param l const reference to a string.
			 * @param r const reference to a string.
			 * @return true iff \a l < \a r in the defined ordering.
			 */
			bool operator()(const String *l, const String *r) const;

		private:
			bool m_na_last;
		};

		String *clone() const override { return const_cast<String *>(this); }

		/** @brief Create a string, leaving its contents
		 *         uninitialized. 
		 * @param sz Number of elements required.  Zero is
		 *          permissible.
		 */
		String(size_t sz);

		/** @brief Character access.
		 * @param index Index of required character (counting from
		 *          zero).  No bounds checking is applied.
		 * @return Reference to the specified character.
		 */
		char &operator[](R_xlen_t index)
		{
			return m_data[index];
		}

		/** @brief Read-only character access.
		 * @param index Index of required character (counting from
		 *          zero).  No bounds checking is applied.
		 * @return the specified character.
		 * @note For CXXR internal use only.
		 */
		char operator[](R_xlen_t index) const
		{
			return m_data[index];
		}

		virtual void *data() override
		{
			return m_data;
		}

		virtual const void *data() const override
		{
			return m_data;
		}

		const char *c_str() const
		{
			return m_data;
		}

		/** @brief 'Not available' string.
		 * @return <tt>const</tt> pointer to the string representing
		 *         'not available'.
		 */
		static const String *NA()
		{
			return static_cast<String *>(R_NaString);
		}

		/** @brief The name by which this type is known in R.
         *
         * @return the name by which this type is known in R.
         */
		static const char *staticTypeName()
		{
			return "char";
		}

		// Virtual functions of RObject:
		const char *typeName() const override;

	private:
		// Max. strlen stored internally:
		static const size_t s_short_strlen = 7;

		char *m_data; // pointer to the string's data block.

		// If there are fewer than s_short_strlen+1 chars in the
		// string (including the trailing null), it is stored here,
		// internally to the String object, rather than via a separate
		// allocation from CXXR::MemoryBank.  We put this last, so that it
		// will be adjacent to any trailing redzone.
		char m_short_string[s_short_strlen + 1];

		// Not implemented yet.  Declared to prevent
		// compiler-generated versions:
		String(const String &);
		String &operator=(const String &);

		// Declared private to ensure that Strings are
		// allocated only using 'new'.
		~String()
		{
			if (m_data != m_short_string)
				MemoryBank::deallocate(m_data, length() + 1);
		}
		friend class Symbol;
	};
} // namespace CXXR

#endif /* CXXR_STRING_HPP */
