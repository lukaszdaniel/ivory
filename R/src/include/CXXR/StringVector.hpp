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

/** @file StringVector.hpp
 * @brief Class CXXR::StringVector and associated C interface.
 *
 * (StringVector implements STRSXP.)
 */

#ifndef STRINGVECTOR_HPP
#define STRINGVECTOR_HPP

#include <CXXR/FixedVector.hpp>
#include <CXXR/GCEdge.hpp>
#include <CXXR/CachedString.hpp>
#include <CXXR/HandleVector.hpp>
#include <CXXR/SEXP_downcast.hpp>

namespace CXXR
{
    /** @brief Vector of strings.
     *
     * Note that the <tt>StringVector(size_type)</tt> constructor will
     * fill the constructed vector with blank strings rather than
     * with NULL.
     */

    // Template specialization:
    template <>
    inline const char *HandleVector<String, STRSXP>::staticTypeName()
    {
        return "character";
    }

    /** @brief Vector of strings.
     */
    class StringVector : public CXXR::HandleVector<String, STRSXP>
    {
    public:
        /** @brief Create a StringVector.
         *
         * @param sz Number of elements required.  Zero is
         *          permissible.
         */
        explicit StringVector(size_t sz)
            : HandleVector<String, STRSXP>(sz, const_cast<CachedString *>(CachedString::blank()))
        {
        }

        /** @brief Copy constructor.
         *
         * Copy the StringVector, using the RObject::Handle copying
         * semantics.
         *
         * @param pattern StringVector to be copied.
         *
         * @param deep Indicator whether to perform deep or shallow copy.
         */
        StringVector(const StringVector &pattern, Duplicate deep)
            : HandleVector<String, STRSXP>(pattern, deep)
        {
        }

        // Virtual function of RObject:
        StringVector *clone(Duplicate deep) const override;

    private:
        /**
         * Declared private to ensure that StringVector objects are
         * allocated only using 'new'.
         */
        ~StringVector() {}
    };

    inline SEXP *STRINGVECTOR_STRING_PTR(RObject *x)
    {
        // return static_cast<StringVector **>(DATAPTR(x));
        return static_cast<SEXP *>(DATAPTR(x));
    }

    inline const SEXP *STRINGVECTOR_STRING_PTR_RO(RObject *x)
    {
        // return static_cast<StringVector *const *>(DATAPTR(x));
        return static_cast<const SEXP *>(DATAPTR_RO(x));
    }

    /** @brief Create a StringVector containing a single std::string.
     *
     * This constructor constructs a StringVector containing a single
     * element, and initializes that element to represent a specified
     * string and encoding.
     *
     * @param str The required text of the single vector element.
     *
     * @param encoding The required encoding of the single vector
     *          element.  Only CE_NATIVE, CE_UTF8 or CE_LATIN1 are
     *          permitted in this context (checked).
     */
    inline StringVector *asStringVector(const std::string &str, cetype_t encoding = CE_UTF8)
    {
        GCStackRoot<CachedString> cs(CachedString::obtain(str, encoding));
        StringVector *ans = GCNode::expose(new StringVector(1));
        (*ans)[0] = cs;
        return ans;
        // return StringVector::createScalar(cs);
    }

    /** @brief (For debugging.)
     *
     * @note The name and interface of this function may well change.
     */
    void strdump(std::ostream &os, const StringVector *sv, size_t margin = 0);
} // namespace CXXR

extern "C"
{
    /**
     * @param s Pointer to a CXXR::RObject.
     *
     * @return TRUE iff the CXXR::RObject pointed to by \a s is a vector of strings.
     */
    Rboolean Rf_isString(SEXP s);

    /** @brief Set element of CXXR::StringVector.
     *
     * @param x Non-null pointer to a CXXR::StringVector.
     *
     * @param i Index of the required element.  There is no bounds checking.
     *
     * @param v Non-null pointer to CXXR::String representing the new value.
     */
    void SET_STRING_ELT(SEXP x, R_xlen_t i, SEXP v);

    /** @brief Examine element of a CXXR::StringVector.
     *
     * @param x Non-null pointer to a CXXR::StringVector.  An error is
     *          raised if \a x is not a pointer to a CXXR::StringVector.
     *
     * @param i Index of the required element.  There is no bounds checking.
     *
     * @return Pointer to extracted \a i 'th element.
     */
    SEXP STRING_ELT(SEXP x, R_xlen_t i);

    /**
     * @param x Pointer to a CXXR::StringVector; an error is raised if \a x
     *          is not a pointer to a CXXR::StringVector.
     *
     * @return Pointer to the start of \a x 's data, interpreted (riskily)
     *         as an array of CXXR::String*.
     *
     * @deprecated This function puts the integrity of the write barrier
     * at the mercy of callers.  It is deliberately not made visible
     * to C code.
     */
    SEXP *STRING_PTR(SEXP x);

    /**
     * @param x Pointer to a CXXR::StringVector; an error is raised if \a x
     *          is not a pointer to a CXXR::StringVector.
     *
     * @return Constant pointer to the start of \a x 's data, interpreted (riskily)
     *         as an array of CXXR::String*.
     *
     * @deprecated This function puts the integrity of the write barrier
     * at the mercy of callers.  It is deliberately not made visible
     * to C code.
     */
    const SEXP *STRING_PTR_RO(SEXP x);

    /** @brief Obtaing index of a string vector
     *
     * @return index of a given C string in (translated) R string vector
     */
    int Rf_stringPositionTr(SEXP string, const char *translatedElement);

    Rboolean Rf_isValidString(SEXP x);

    /* non-empty ("") valid string :*/
    Rboolean Rf_isValidStringF(SEXP x);
    SEXP Rf_ScalarString(SEXP x);
} // extern "C"

#if (defined(R_NO_REMAP) && defined(COMPILING_IVORY)) && defined(__cplusplus)
const auto stringPositionTr = Rf_stringPositionTr;
const auto isValidString = Rf_isValidString;
const auto isValidStringF = Rf_isValidStringF;
const auto ScalarString = Rf_ScalarString;
#endif

#endif // STRINGVECTOR_HPP
