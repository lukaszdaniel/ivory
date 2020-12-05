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
#include <CXXR/String.hpp>

namespace CXXR
{
    /** @brief Vector of strings.
     *
     * Note that the <tt>StringVector(size_type)</tt> constructor will
     * fill the constructed vector with blank strings rather than
     * with NULL.
     */
    typedef FixedVector<GCEdge<String>, STRSXP> StringVector;

    inline SEXP *STRINGVECTOR_STRING_PTR(RObject *x)
    {
        return static_cast<SEXP *>(DATAPTR(x));
    }

    inline const SEXP *STRINGVECTOR_STRING_PTR_RO(RObject *x)
    {
        return static_cast<const SEXP *>(DATAPTR_RO(x));
    }
} // namespace CXXR

extern "C"
{
    /**
     * @param s Pointer to an RObject.
     * @return TRUE iff the RObject pointed to by \a s is a vector of
     *         strings.
     */
    Rboolean Rf_isString(SEXP s);

    /** @brief Set element of CXXR::StringVector.
     *
     * @param x Pointer to a CXXR::StringVector .
     * @param i Index of the required element.  There is no bounds checking.
     * @param v Pointer to CXXR::RObject representing the new value.
     */
    void SET_STRING_ELT(SEXP x, R_xlen_t i, SEXP v);

    /**
     * @brief Examine element of a CXXR::StringVector.
     * @param x Pointer to a CXXR::StringVector.  An error is raised if \a x
     *          is not a pointer to a StringVector.
     * @param i Index of the required element.  There is no bounds checking.
     * @return Pointer to extracted \a i 'th element.
     */
    SEXP STRING_ELT(SEXP x, R_xlen_t i);

    /**
     * @param x Pointer to a CXXR::StringVector; an error is raised if \a x
     *          is not a pointer to a CXXR::StringVector.
     * @return Pointer to the start of \a x 's data, interpreted (riskily)
     *         as an array of CXXR::String*.
     * @deprecated This function puts the integrity of the write barrier
     * at the mercy of callers.  It is deliberately not made visible
     * to C code.
     */
    SEXP *(STRING_PTR)(SEXP x);
} // extern "C"

#endif // STRINGVECTOR_HPP
