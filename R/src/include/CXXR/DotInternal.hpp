/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
 *
 *  Rho is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the Rho website.
 *
 *  This header file is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

/** @file DotInternal.hpp
 *
 * @brief Table of functions invoked \e via <tt>.Internal()</tt>.
 */

#ifndef DOTINTERNAL_HPP
#define DOTINTERNAL_HPP

#include <CXXR/RObject.hpp>
#include <CXXR/BuiltInFunction.hpp>
#include <CXXR/SEXP_downcast.hpp>

namespace R
{
    /** @brief Associate a Symbol with a <tt>.Internal()</tt> function.
     *
     * @param x Pointer to a CXXR::Symbol (checked).
     *
     * @param v Pointer to the CXXR::BuiltInFunction (checked) to be
     * associated by this symbol.  A null pointer is permissible, and
     * signifies that any previous association of \a sym with a
     * function is to be removed from the table.
     */
    void SET_INTERNAL(SEXP x, SEXP v);
} // namespace R

extern "C"
{

    /** @brief Get function accessed via <tt>.Internal()</tt>.
     *
     * @param x Pointer to a CXXR::Symbol (checked).
     *
     * @return If \a x is associated with a function invoked in R \e
     * via <tt>.Internal()</tt>, then a pointer to the appropriate
     * CXXR::BuiltInFunction, otherwise a null pointer.
     */
    SEXP INTERNAL(SEXP x);
} // extern "C"

#endif /* DOTINTERNAL_HPP */
