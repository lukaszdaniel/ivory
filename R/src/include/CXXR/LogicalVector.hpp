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

/** @file LogicalVector.hpp
 * @brief Class CXXR::LogicalVector and associated C interface.
 */

#ifndef LOGICALVECTOR_HPP
#define LOGICALVECTOR_HPP

#include <R_ext/Arith.h>
#include <CXXR/VectorBase.hpp>
#include <CXXR/FixedVector.hpp>
#include <CXXR/Logical.hpp>
#include <CXXR/SEXP_downcast.hpp>

namespace CXXR
{
    /** @brief Vector of truth values.
     */
    typedef CXXR::FixedVector<Logical, LGLSXP> LogicalVector;

    inline int *LOGICALVECTOR_LOGICAL(RObject *x)
    {
        return static_cast<int *>(DATAPTR(x));
    }

    inline const int *LOGICALVECTOR_LOGICAL_RO(RObject *x)
    {
        return static_cast<const int *>(DATAPTR_RO(x));
    }
} // namespace CXXR

extern "C"
{
    /**
     * @param s Pointer to a CXXR::RObject.
     * @return TRUE iff the CXXR::RObject pointed to by \a s is a logical vector.
     */
    Rboolean Rf_isLogical(SEXP s);

    /**
     * @param x Pointer to a CXXR::LogicalVector or an CXXR::IntVector (i.e. an
     *          R logical or integer vector).
     *          An error is generated if \a x is not pointer to a \c
     *          CXXR::LogicalVector or an CXXR::IntVector.
     * @return Pointer to element 0 of \a x.
     */
    int *LOGICAL(SEXP x);

    /**
     * @param x Pointer to a CXXR::LogicalVector or an CXXR::IntVector (i.e. an
     *          R logical or integer vector).
     *          An error is generated if \a x is not pointer to a \c
     *          CXXR::LogicalVector or an CXXR::IntVector.
     * @return Pointer to constant element 0 of \a x.
     */
    const int *LOGICAL_RO(SEXP x);
    int *LOGICAL0(SEXP x);
    int SCALAR_LVAL(SEXP x);
    void SET_SCALAR_LVAL(SEXP x, int v);
    const int *LOGICAL_OR_NULL(SEXP x);
    void SET_LOGICAL_ELT(SEXP x, R_xlen_t i, int v);
    int LOGICAL_ELT(SEXP x, R_xlen_t i);
    SEXP Rf_ScalarLogical(int x);
} // extern "C"

#if defined(R_NO_REMAP) && defined(COMPILING_IVORY) && defined(__cplusplus)
const auto ScalarLogical = Rf_ScalarLogical;
#endif

#endif // LOGICALVECTOR_HPP
