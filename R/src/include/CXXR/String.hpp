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

namespace R
{
	extern void InitNames();
}

namespace CXXR
{
	/** @brief Base class for RObject representing a character string.
     *
     * @note When the method size() of VectorBase is applied to a
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

		/** @brief Read-only character access.
		 * @param index Index of required character (counting from
		 *          zero).  No bounds checking is applied.
		 * @return the specified character.
		 * @note For CXXR internal use only.
		 */
		char operator[](R_xlen_t index) const
		{
			return m_c_str[index];
		}

		/** @brief Blank string.
		 * @return <tt>const</tt> pointer to the string "".
		 */
		static const String *blank()
		{
			return s_blank;
		}

		/** @brief Access encapsulated C-style string.
		 *
		 * @return Pointer to the encapsulated C-style (null
		 * terminated) string.
		 */
		const char *c_str() const
		{
			return m_c_str.c_str();
		}

		/** @brief Character encoding.
         *
         * @return the character encoding.  At present the only types
         * of encoding are CE_NATIVE, CE_UTF8, CE_LATIN1 and CE_BYTES.
         */
		cetype_t encoding() const { return m_encoding; }

		/** @brief Extract encoding information from CR's \c gp bits
         * field.
         *
         * This function is used to extract the character encoding
         * information contained in the <tt>sxpinfo_struct.gp</tt>
         * field used in CR.  It should be used exclusively for
         * deserialization.  Refer to the 'R Internals' document for
         * details of this field.
         *
         * @param gpbits the \c gp bits field (within the
         *          least significant 16 bits).
         */
		static cetype_t GPBits2Encoding(unsigned int gpbits);

		/** @brief Is this String pure ASCII?
         *
         * @return true iff the String contains only ASCII characters.
         */
		bool isASCII() const { return m_ascii; }

		/** @brief Is this String encoded in UTF8?
         *
         * @return true iff the String is encoded in UTF8.
         */
		bool isUTF8() const { return encodingEquals(CE_UTF8); }

		/** @brief Is this String encoded in LATIN1?
         *
         * @return true iff the String is encoded in LATIN1.
         */
		bool isLATIN1() const { return encodingEquals(CE_LATIN1); }

		/** @brief Is this String encoded in BYTES?
         *
         * @return true iff the String is encoded in BYTES.
         */
		bool isBYTES() const { return encodingEquals(CE_BYTES); }

		/** @brief Check if String is encoded in \a t?
         *
         * @return true iff the String is encoded in \a t.
         */
		bool encodingEquals(const cetype_t &t) const { return encoding() == t; }

		/** @brief Hash value.
		 *
		 * @return The hash value of this string.
		 *
		 * @note The current hashing algorithm (taken from CR) does
		 * not deal satisfactorily with strings containing embedded
		 * null characters: the hashing processes characters only up
		 * to the first null.
		 */
		int hash() const;

		/** @brief Test if 'not available'.
		 *
		 * @return true iff this is the 'not available' string.
		 */
		bool isNA() const
		{
			return this == s_na.get();
		}

		/** @brief 'Not available' string.
		 *
		 * Note that although the 'not available' string contains the
		 * text "NA", it is identified as the 'not available' string
		 * by its <em>address</em>, not by its content.  It is
		 * entirely in order to create another string with the text
		 * "NA", and that string will not be considered 'not
		 * available'.
		 *
		 * @return <tt>const</tt> pointer to the string representing
		 *         'not available'.
		 */
		static const String *NA()
		{
			return s_na;
		}

		virtual void *data() override
		{
			return const_cast<char *>(m_c_str.c_str());
		}

		virtual const void *data() const override
		{
			return m_c_str.c_str();
		}

		/** @brief The name by which this type is known in R.
         *
         * @return the name by which this type is known in R.
         */
		static const char *staticTypeName()
		{
			return "char";
		}

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

	protected:
		/** @brief Create a string. 
		 *
		 * @param sz Number of <tt>char</tt>s in the string.  Zero is
		 *          permissible.  Note that if the string uses a
		 *          multibyte encoding scheme, this may be different
		 *          from the number of Unicode characters represented
		 *          by the string.
		 *
		 * @param encoding The intended encoding of the string, as
		 *          indicated by the LATIN1_MASK and UTF8_MASK bits.
		 *          Zero signifies ASCII encoding, and at most one of
		 *          the MASK bits may be set (checked).
		 *
		 * @param c_string Pointer to a representation of the string
		 *          as a C-style string (but possibly with embedded
		 *          null characters), with \a sz plus one bytes, the
		 *          last byte being a null byte.  (Because of the
		 *          possibility of embedded nulls the size of the
		 *          string is not checked.)  This string
		 *          representation must remain in existence for the
		 *          lifetime of the String object.  If a null pointer
		 *          is supplied here, a string pointer must be
		 *          supplied later in the construction of the derived
		 *          class object by calling setCString().
		 */
		String(size_t sz, cetype_t encoding, const std::string &c_string = "", bool isAscii = false)
			: VectorBase(CHARSXP, sz), m_c_str(c_string), m_encoding(encoding), m_ascii(isAscii), m_hash(-1)
		{
			if (encoding)
				checkEncoding(encoding);
			switch (m_encoding)
			{
			case CE_NATIVE:
				break;
			case CE_UTF8:
				m_gpbits |= UTF8_MASK;
				break;
			case CE_LATIN1:
				m_gpbits |= LATIN1_MASK;
				break;
			case CE_BYTES:
				m_gpbits |= BYTES_MASK;
				break;
			default:
				break;
			}
			if (isAscii)
				m_gpbits |= ASCII_MASK;
		}

		/** @brief Supply pointer to the string representation.
		 *
		 * @param c_string Pointer to a representation of the string
		 *          as a C-style string (but possibly with embedded
		 *          null characters), with size() plus one bytes, the
		 *          last byte being a null byte.  (Because of the
		 *          possibility of embedded nulls the size of the
		 *          string is not checked.)  This string
		 *          representation must remain in existence for the
		 *          lifetime of the String object.
		 */
		void setCString(const char *c_string)
		{
			m_c_str = std::string(c_string);
		}

		/** @brief Mark the hash value as invalid.
		 *
		 * This should be called in the event that the text of the
		 * String may have been changed.
		 */
		void invalidateHash() const
		{
			m_hash = -1;
		}

	private:
		static GCRoot<const String> s_na;
		static GCRoot<const String> s_blank;
		std::string m_c_str;
		cetype_t m_encoding;
		bool m_ascii;

		mutable int m_hash; // negative signifies invalid

		// Not implemented yet.  Declared to prevent
		// compiler-generated versions:
		String(const String &);
		String &operator=(const String &);
		// Report error if encoding is invalid:
		static void checkEncoding(cetype_t encoding);
		// Initialize the static data members:
		static void initialize();
		friend void ::R::InitNames();
	};

	inline const char *r_char(RObject *x)
	{
		return stdvec_dataptr<const char>(x);
	}

	/** @brief Is a std::string entirely ASCII?
     *
     * @param str The string to be examined.
     *
     * @return false if str contains at least one non-ASCII character,
     * otherwise true.  In particular the function returns true for an
     * empty string.
     */
	bool isASCII(const std::string &str);
} // namespace CXXR

extern "C"
{
	extern SEXP R_NaString;	   /* NA_STRING as a CHARSXP */
	extern SEXP R_BlankString; /* "" as a CHARSXP */

	/**
     * @param x \c const pointer to a CXXR::String.
     * @return \c const pointer to character 0 of \a x.
     */
	const char *R_CHAR(SEXP x);

	/**
     * @param x Pointer to a CXXR::String.
     * @return true iff \a x is marked as having LATIN1 encoding.
     */
	int(IS_LATIN1)(SEXP x);

	/**
     * @param x Pointer to a CXXR::String.
     * @return true iff \a x is marked as having UTF8 encoding.
     */
	int(IS_UTF8)(SEXP x);

	/* Hashing Functions */

	int HASHASH(SEXP x);

	int HASHVALUE(SEXP x);

	void SET_HASHASH(SEXP x, int v);

	void SET_HASHVALUE(SEXP x, int v);
} // extern "C"

#endif /* CXXR_STRING_HPP */
