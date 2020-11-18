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

/** @file RealVector.hpp
 * @brief Class CXXR::RealVector and associated C interface.
 */

#ifndef REALVECTOR_HPP
#define REALVECTOR_HPP

#include <R_ext/Arith.h>
#include <CXXR/FixedVector.hpp>
#include <CXXR/SEXP_downcast.hpp>
#include <CXXR/VectorBase.hpp>
#include <cmath>

namespace CXXR
{
    /** @brief Vector of real numbers.
     */
    typedef CXXR::FixedVector<double, REALSXP> RealVector;

#define REALVECTOR_REAL(x) ((double *)DATAPTR(x))
#define REALVECTOR_REAL_RO(x) ((const double *)DATAPTR_RO(x))
} // namespace CXXR

#endif // REALVECTOR_HPP
