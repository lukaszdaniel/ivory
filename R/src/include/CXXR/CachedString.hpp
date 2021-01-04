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

/** @file CachedString.hpp
 * @brief Class CXXR::CachedString and associated C interface.
 */

#ifndef CACHEDSTRING_HPP
#define CACHEDSTRING_HPP

#include <CXXR/String.hpp>
#include <map>
#include <string>

namespace CXXR
{
    /** @brief String object held in a cache.
     *
     * At any one time, at most one CachedString object with a
     * particular text and encoding may exist.
     */
    class CachedString : public String
    {
    public:
        /** @brief Get a pointer to a CachedString object.
         *
         * If no CachedString with the specified text and encoding
         * currently exists, one will be created.  Otherwise a pointer
         * to the existing CachedString will be returned.
         *
         * @param str The text of the required CachedString.
         *          (Embedded null characters are permissible.)
         *
         * @param encoding The encoding of the required CachedString,
         *          as  indicated by the LATIN1_MASK and UTF8_MASK
         *          bits.  Zero signifies ASCII encoding, and at most
         *          one of the MASK bits may be set (checked).
         */
        static const CachedString *obtain(const std::string &str, unsigned int encoding = 0);

        /** @brief The name by which this type is known in R.
         *
         * @return the name by which this type is known in R.
         */
        static const char *staticTypeName()
        {
            return "char (cached)";
        }

        // Virtual function of RObject:
        const char *typeName() const override;

        virtual void *data() override
        {
            return nullptr;
        }

        virtual const void *data() const override
        {
            return nullptr;
        }

    private:
        // The first element of the key is the text, the second
        // element the encoding:
        typedef std::pair<std::string, unsigned int> key;

        // The cache is implemented as a mapping from keys to pointers
        // to CachedString objects.  Each CachedString simply contains an
        // iterator locating its text and encoding within the cache.
        // In the future this may be changed to a TR1 unordered_map.
        typedef std::map<key, CachedString *> map;

        static map s_cache;

        map::iterator m_it;

        explicit CachedString(map::iterator iter)
            : String((*iter).first.first.size(), (*iter).first.second,
                     (*iter).first.first.c_str()),
              m_it(iter)
        {
        }

        // Not implemented.  Declared to prevent
        // compiler-generated versions:
        CachedString(const CachedString &);
        CachedString &operator=(const CachedString &);

        // Declared private to ensure that CachedString objects are
        // allocated only using 'new'.
        ~CachedString()
        {
            s_cache.erase(m_it);
        }
    };
} // namespace CXXR

extern "C"
{
    /** @brief Get a pointer to a CXXR::String object.
     *
     * CE_NATIVE encoding is assumed.  If no CXXR::String with the
     * specified text and encoding currently exists, one will be
     * created.  Otherwise a pointer to the existing CXXR::String will
     * be returned.
     *
     * @param str The null-terminated text of the required string.
     *
     * @return Pointer to a string object representing the specified
     *         text.
     */
    SEXP Rf_mkChar(const char *const name);

    /** @brief Get a pointer to a CXXR::String object.
     *
     * If no CXXR::String with the specified text and encoding
     * currently exists, one will be created.  Otherwise a pointer to
     * the existing CXXR::String will be returned.
     *
     * @param str The null-terminated text of the required cached string.
     *
     * @param encoding The encoding of the required String.
     *          Only CE_NATIVE, CE_UTF8 or CE_LATIN1 are permitted in
     *          this context (checked).
     *
     * @return Pointer to a string object representing the specified
     *         text in the specified encoding.
     */
    SEXP Rf_mkCharCE(const char *str, cetype_t encoding);

    /** @brief Create a CXXR::String object for specified text and
     * encoding.
     *
     * If no CXXR::String with the specified text and encoding
     * currently exists, one will be created.  Otherwise a pointer to
     * the existing CXXR::String will be returned.
     *
     * @param text The text of the string to be created, possibly
     *          including embedded null characters.  The encoding is
     *          assumed to be CE_NATIVE.
     *
     * @param length The length of the string pointed to by \a text.
     *          Must be nonnegative.  The created string will comprise
     *          the text plus an appended null byte.
     *
     * @param encoding The encoding of the required String.
     *          Only CE_NATIVE, CE_UTF8 or CE_LATIN1 are permitted in
     *          this context (checked).
     *
     * @return Pointer to the created string.
     */
    SEXP Rf_mkCharLenCE(const char *const name, int len, cetype_t encoding);

    /** @brief Create a CXXR::String object for specified text.
     *
     * CE_NATIVE encoding is assumed.  If no CXXR::String with the
     * specified text and encoding currently exists, one will be
     * created.  Otherwise a pointer to the existing CXXR::String will
     * be returned.
     *
     * @param text The text of the string to be created, possibly
     *          including embedded null characters.  The encoding is
     *          assumed to be CE_NATIVE.
     *
     * @param length The length of the string pointed to by \a text.
     *          Must be nonnegative.  The created string will comprise
     *          the text plus an appended null byte.
     *
     * @return Pointer to the created string.
     */
    SEXP Rf_mkCharLen(const char *name, int len);

    /** @brief Convert contents of a CXXR::String to UTF8.
     *
     * @param x Non-null pointer to a CXXR::String.
     *
     * @return The text of \a x rendered in UTF8 encoding.
     *
     * @note The result is held in memory allocated using R_alloc().
     * The calling code must arrange for this memory to be released in
     * due course.
     */
    const char *Rf_translateCharUTF8(SEXP x);
    cetype_t Rf_getCharCE(SEXP x);
    const char *Rf_reEnc(const char *x, cetype_t ce_in, cetype_t ce_out, int subst);
} // extern "C"

#endif /* CACHEDSTRING_HPP */
