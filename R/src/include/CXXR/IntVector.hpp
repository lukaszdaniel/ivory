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

/** @file IntVector.hpp
 * @brief Class CXXR::IntVector and associated C interface.
 */

#ifndef INTVECTOR_HPP
#define INTVECTOR_HPP

#include <CXXR/FixedVector.hpp>

namespace CXXR
{
    /** @brief Vector of integer values.
     */
    typedef CXXR::FixedVector<int, INTSXP> IntVector;

    inline int *INTVECTOR_INTEGER(RObject *x)
    {
        return static_cast<int *>(DATAPTR(x));
    }

    inline const int *INTVECTOR_INTEGER_RO(RObject *x)
    {
        return static_cast<const int *>(DATAPTR_RO(x));
    }
} // namespace CXXR

#endif // INTVECTOR_HPP
