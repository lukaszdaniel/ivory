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

#include <string>
#include <unordered_map>
#include <CXXR/Allocator.hpp>
#include <CXXR/GCRoot.hpp>
#include <CXXR/SEXP_downcast.hpp>
#include <CXXR/VectorBase.hpp>
#include <Rinternals.h>

namespace R
{
	extern void InitNames();
}

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
		/** @brief Comparison object for CXXR::String.
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
			 *
			 * @param l non-null pointer to a String.
			 *
			 * @param r non-null pointer to a String.
			 *
			 * @return true iff \a l < \a r in the defined ordering.
			 */
			bool operator()(const String *l, const String *r) const;

		private:
			bool m_na_last;
		};

		String *clone(bool deep) const override
		{
			return const_cast<String *>(this);
		}

		/** @brief Read-only character access.
		 *
		 * @param index Index of required character (counting from
		 *          zero).  No bounds checking is applied.
		 *
		 * @return the specified character.
		 *
		 * @note For CXXR internal use only.
		 */
		char operator[](R_xlen_t index) const
		{
			return m_data[index];
		}

		/** @brief Access encapsulated C-style string.
		 *
		 * @return Pointer to the encapsulated C-style (null
		 * terminated) string.
		 */
		virtual const char *c_str() const
		{
			return m_data.c_str();
		}

		/** @brief Access encapsulated std::string.
		 *
		 * @return The string's value as a std::string.
		 */
		virtual std::string stdstring() const
		{
			return m_data;
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

		bool hasHash() const
		{
			return m_hash != -1;
		}

		bool isCached() const
		{
			return m_cached;
		}

		void setCached()
		{
			// m_gpbits |= CACHED_MASK;
			m_cached = true;
		}

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
			return const_cast<char *>(m_data.c_str());
		}

		virtual const void *data() const override
		{
			return m_data.c_str();
		}

		/** @brief The name by which this type is known in R.
		 *
		 * @return the name by which this type is known in R.
		 */
		static const char *staticTypeName()
		{
			return "char";
		}

		static int clearCacheAndHashMasks(int levels);

		// Virtual function of RObject:
		unsigned int packGPBits() const override;

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
		String(size_t sz, cetype_t encoding, const std::string &c_string = "", bool isAscii = false, bool isCached = false)
			: VectorBase(CHARSXP, sz), m_data(c_string), m_encoding(encoding), m_ascii(isAscii), m_cached(isCached), m_hash(-1)
		{
			if (encoding)
				checkEncoding(encoding);
			setGPBits();
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
			m_data = std::string(c_string);
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
		std::string m_data;
		cetype_t m_encoding;
		bool m_ascii;
		bool m_cached;

		mutable int m_hash; // negative signifies invalid
		void setGPBits();

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

namespace R
{
	/** @brief used in package utils and graphics
	 */
	extern bool known_to_be_latin1;

	extern bool known_to_be_utf8;
	void ssort(CXXR::String **x, int n);
	bool Seql(SEXP a, SEXP b);
	bool streql(const char *s, const char *t);
	bool streqln(const char *s, const char *t, size_t n);

	/** @brief String Hashing
	 *
	 * This is taken from the second edition of the "Dragon Book" by
	 * Aho, Ullman and Sethi.
	 *
	 * @note This hash function seems to work well enough for symbol tables,
	 * and hash tables get saved as part of environments so changing it
	 * is a major decision.
	 */
	int R_Newhashpjw(const char *s);
}

extern "C"
{
	/** @brief NA_STRING as a CHARSXP
	 */
	extern SEXP R_NaString;

	/** @brief "" as a CHARSXP
	 */
	extern SEXP R_BlankString;

	/**
	 * @param x \c const pointer to a CXXR::String.
	 *
	 * @return \c const pointer to character 0 of \a x.
	 */
	const char *R_CHAR(SEXP x);

	/**
	 * @param x Pointer to a CXXR::String.
	 *
	 * @return true iff \a x is marked as having LATIN1 encoding.
	 */
	int IS_LATIN1(SEXP x);

	/** @brief Does a rho::String have UTF8 encoding?
	 *
	 * @param x Pointer to a rho::String (checked).
	 *
	 * @return true iff \a x is marked as having UTF8 encoding.
	 */
	int IS_UTF8(SEXP x);

	/** @brief Does a rho::String have bytecode encoding?
	 *
	 * @param x Pointer to a rho::String.
	 *
	 * @return true iff \a x is marked as having BYTES encoding.
	 */
	int IS_BYTES(SEXP x);

	/** @brief Is a rho::String pure ASCII?
	 *
	 * @param x Pointer to a rho::String.
	 *
	 * @return true iff \a x contains only ASCII characters.
	 */
	int IS_ASCII(SEXP x);

	/** @brief Is the encoding of a rho::String known?
	 *
	 * @param x Pointer to a rho::String.
	 *
	 * @return a non-zero value iff \a x is marked as having either
	 * LATIN1 encoding or UTF8 encoding.
	 */
	int ENC_KNOWN(SEXP x);

	int IS_CACHED(SEXP x);

	/** @brief Set LATIN1 encoding.
	 *
	 * @param x Pointer to a CXXR::String.
	 */
	void SET_LATIN1(SEXP x);

	/** @brief Set UTF8 encoding.
	 *
	 * @param x Pointer to a CXXR::String.
	 */
	void SET_UTF8(SEXP x);

	/** @brief Set BYTES encoding.
	 *
	 * @param x Pointer to a CXXR::String.
	 */
	void SET_BYTES(SEXP x);

	/** @brief Set ASCII encoding.
	 *
	 * @param x Pointer to a CXXR::String.
	 */
	void SET_ASCII(SEXP x);

	void SET_CACHED(SEXP x);

	/* Hashing Functions */

	int HASHASH(SEXP x);
	int HASHVALUE(SEXP x);
	void SET_HASHASH(SEXP x, int v);
	void SET_HASHVALUE(SEXP x, int v);
} // extern "C"

#endif /* CXXR_STRING_HPP */
