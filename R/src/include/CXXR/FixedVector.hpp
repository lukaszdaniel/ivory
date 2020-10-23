/*
 *  R : A Computer Language for Statistical Data Analysis
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

/** @file FixedVector.hpp
 *
 * @brief Class template R::FixedVector.
 */

#ifndef FIXEDVECTOR_HPP
#define FIXEDVECTOR_HPP

#include <CXXR/VectorBase.hpp>

namespace R
{
    /** @brief R data vector primarily intended for fixed-size use.
     *
     * This is a general-purpose class template to represent an R data
     * vector, and is intended particularly for the case where the
     * size of the vector is fixed when it is constructed.
     *
     * Having said that, the template \e does implement decreaseSizeInPlace(),
     * primarily to service CR code's occasional use of SETLENGTH().
     *
     * R implements all of CR's built-in vector types using this
     * template.
     *
     * @tparam T The type of the elements of the vector.
     *
     * @tparam ST The required ::SEXPTYPE of the vector.
     */
    template <typename T, SEXPTYPE ST>
    class FixedVector : public VectorBase
    {
    public:
    private:
    };
} // namespace R

#endif // FIXEDVECTOR_HPP
