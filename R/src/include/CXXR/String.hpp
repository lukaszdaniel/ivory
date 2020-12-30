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
#include <string>
#include <unordered_map>

extern "C" SEXP R_NaString;	   /* NA_STRING as a CHARSXP */
extern "C" SEXP R_BlankString; /* "" as a CHARSXP */

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
			explicit Comparator(bool na_last = true)
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
		explicit String(R_xlen_t sz);

		/** @brief Character access.
		 * @param index Index of required character (counting from
		 *          zero).  No bounds checking is applied.
		 * @return Reference to the specified character.
		 */
		char &operator[](R_xlen_t index)
		{
			m_hash = -1;
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

		/** @brief Blank string.
		 * @return <tt>const</tt> pointer to the string "".
		 */
		static const String *blank()
		{
			return static_cast<String *>(R_BlankString);
		}

		/** @brief Access encapsulated C-style string.
		 *
		 * @return Pointer to the encapsulated C-style (null
		 * terminated) string.
		 */
		const char *c_str() const
		{
			return m_data;
		}

		/** @brief Hash value.
		 * @return The hash value of this string.
		 */
		int hash() const;

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
		/* Hashing Methods */
		static constexpr int HASHASH_MASK = 1;
		static unsigned int hashash(RObject *x);
		/* CHARSXP charset bits */
		enum CharsetBit
		{
			NATIVE_MASK = 0,
			BYTES_MASK = (1 << 1),
			LATIN1_MASK = (1 << 2),
			UTF8_MASK = (1 << 3),
			/* (1 << 4) is taken by S4_OBJECT_MASK */
			CACHED_MASK = (1 << 5),
			ASCII_MASK = (1 << 6)
		};

		static unsigned int is_bytes(RObject *x);
		static void set_bytes(RObject *x);
		static unsigned int is_latin1(RObject *x);
		static void set_latin1(RObject *x);
		static unsigned int is_ascii(RObject *x);
		static void set_ascii(RObject *x);
		static unsigned int is_utf8(RObject *x);
		static void set_utf8(RObject *x);
		static unsigned int enc_known(RObject *x);
		static void set_cached(RObject *x);
		static unsigned int is_cached(RObject *x);

	private:
		// Max. strlen stored internally:
		static const R_xlen_t s_short_strlen = 7;

		mutable int m_hash; // negative signifies invalid
		R_xlen_t m_databytes; // includes trailing null byte
		char *m_data;		// pointer to the string's data block.

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
				MemoryBank::deallocate(m_data, m_databytes);
		}
		friend class Symbol;
	};

	inline const char *r_char(RObject *x)
	{
		return stdvec_dataptr<const char>(x);
	}
} // namespace CXXR

extern "C"
{
	/**
	 * @brief Writable char access.
     * @param x pointer to a CXXR::String 
     * @return pointer to character 0 of \a x.
     * @note For R internal use only.  May be removed in future.
     */
	inline char *CHAR_RW(SEXP x)
	{
		return CXXR::stdvec_dataptr<char>(x);
	}

	/**
     * @param x \c const pointer to a CXXR::String.
     * @return \c const pointer to character 0 of \a x.
     */
	const char *R_CHAR(SEXP x);

	/**
     * @param x Pointer to a CXXR::String.
     * @return true iff \a x is marked as having LATIN1 encoding.
     */
	int IS_LATIN1(SEXP x);

	/**
     * @brief Set LATIN1 encoding.
     * @param x Pointer to a CXXR::String.
     */
	void SET_LATIN1(SEXP x);

	/**
     * @brief Unset LATIN1 encoding.
     * @param x Pointer to a CXXR::String.
     */
	void UNSET_LATIN1(SEXP x);

	/**
     * @param x Pointer to a CXXR::String.
     * @return true iff \a x is marked as having UTF8 encoding.
     */
	int IS_UTF8(SEXP x);

	/**
     * @brief Set UTF8 encoding.
     * @param x Pointer to a CXXR::String.
     */
	void SET_UTF8(SEXP x);

	/**
     * @brief Unset UTF8 encoding.
     * @param x Pointer to a CXXR::String.
     */
	void UNSET_UTF8(SEXP x);

	/**
     * @brief Create a string object.
     *
     *  Allocate a string object.
     * @param length The length of the string to be created (excluding the
     *          trailing null byte).
     * @return Pointer to the created string.
     */
	SEXP Rf_allocString(R_len_t length);

	/* Hashing Functions */

	int HASHASH(SEXP x);

	int HASHVALUE(SEXP x);

	void SET_HASHASH(SEXP x, int v);

	void SET_HASHVALUE(SEXP x, int v);
} // extern "C"

#endif /* CXXR_STRING_HPP */
