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

/** @file UncachedString.hpp
 * @brief Class CXXR::String and associated C interface.
 */

#ifndef UNCACHEDSTRING_HPP
#define UNCACHEDSTRING_HPP

#include <string>
#include <CXXR/String.hpp>

namespace CXXR
{
    /** @brief String object not held in a cache.
     *
     * Unlike CachedString objects, at any one time more than one
     * UncachedString with the same text and encoding may exist, and
     * the text and encoding of an UncachedString may duplicate that
     * of a CachedString.  Moreover, the content and encoding of an
     * UncachedString may be modified after it has been constructed;
     * however, since the length may \e not be modified, this is of
     * limited usefulness.
     *
     * @deprecated The use of CachedString objects is to be preferred.
     */
    class UncachedString : public String
    {
    public:
        /** @brief Create an UncachedString object, leaving its contents
         *         uninitialized.
         *
         * @param sz Number of elements required.  Zero is
         *          permissible.
         *
         * @param encoding The intended encoding of the string, as
         *          indicated by the LATIN1_MASK and UTF8_MASK bits.
         *          Zero signifies ASCII encoding, and at most one of
         *          the MASK bits may be set (checked).
         */
        explicit UncachedString(size_t sz, cetype_t encoding = CE_NATIVE)
            : String(sz, encoding), m_databytes(sz + 1), m_data(m_short_string)
        {
#ifdef R_MEMORY_PROFILING
            MemoryBank::R_ReportAllocation(convert2VEC<char>(sz + 1) * sizeof(VECREC));
#endif
            allocData(sz);
        }

        /** @brief Create an UncachedString object from a std::string.
         *
         * @param str The std::string whose text is to be copied into
         *          the constructed UncachedString.  (Embedded null
         *          characters are permissible.)
         *
         * @param encoding The intended encoding of the string, as
         *          indicated by the LATIN1_MASK and UTF8_MASK bits.
         *          Zero signifies ASCII encoding, and at most one of
         *          the MASK bits may be set (checked).
         */
        explicit UncachedString(const std::string &str, cetype_t encoding = CE_NATIVE);

        static UncachedString *obtain(const std::string &str, cetype_t encoding = CE_NATIVE);

        /** @brief Character access.
         *
         * @param index Index of required character (counting from
         *          zero).  No bounds checking is applied.
         *
         * @return Reference to the specified character.
         */
        char &operator[](size_type index)
        {
            invalidateHash();
            return m_data[index];
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
        char operator[](size_type index) const
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

        /** @brief Access encapsulated C-style string.
		 *
		 * @return Pointer to the encapsulated C-style (null
		 * terminated) string.
		 */
        virtual const char *c_str() const override
        {
            return m_data;
        }

        /** @brief Access encapsulated std::string.
		 *
		 * @return The string's value as a std::string.
		 */
        virtual std::string stdstring() const override
        {
            return std::string(m_data);
        }

        /** @brief The name by which this type is known in R.
         *
         * @return The name by which this type is known in R.
         */
        static const char *staticTypeName()
        {
            return "char (uncached)";
        }

        // Virtual function of RObject:
        const char *typeName() const override;

    private:
        // Max. strlen stored internally:
        static const size_type s_short_strlen = 7;

        size_type m_databytes; // includes trailing null byte
        char *m_data;         // pointer to the string's data block.

        // If there are fewer than s_short_strlen+1 chars in the
        // string (including the trailing null), it is stored here,
        // internally to the String object, rather than via a separate
        // allocation from CXXR::MemoryBank.  We put this last, so that it
        // will be adjacent to any trailing redzone.
        char m_short_string[s_short_strlen + 1];

        // Not implemented yet.  Declared to prevent
        // compiler-generated versions:
        UncachedString(const UncachedString &);
        UncachedString &operator=(const UncachedString &);

        // Declared private to ensure that Strings are
        // allocated only using 'new'.
        ~UncachedString()
        {
            if (m_data != m_short_string)
                MemoryBank::deallocate(m_data, m_databytes);
        }

        // Initialise m_data, if necessary by allocating a data block
        // from MemoryBank:
        void allocData(size_t sz);
    };
} // namespace CXXR

namespace R
{
} // namespace R

extern "C"
{
    /** @brief Read-write character access.
     *
     * @param x pointer to a CXXR::String (checked).
     *
     * @return pointer to character 0 of \a x .
     *
     * @note For R internal use only.  May be removed in future.  See
     * the remarks on UncachedString::operator[]().
     */
    inline char *CHAR_RW(SEXP x)
    {
        return CXXR::stdvec_dataptr<char>(x);
    }
} // extern "C"

#endif /* UNCACHEDSTRING_HPP */
