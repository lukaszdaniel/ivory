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

/** @file Frame.hpp
 * @brief Class CXXR::Frame and associated functions.
 */

#ifndef RFRAME_HPP
#define RFRAME_HPP

#include <unordered_map>
#include <CXXR/Allocator.hpp>
#include <CXXR/GCNode.hpp>
#include <CXXR/Promise.hpp>
#include <CXXR/Symbol.hpp>

namespace CXXR
{

    /** @brief Mapping from Symbols to R objects.
     *
     * A Frame defines a mapping from (pointers to) CXXR::Symbol
     * objects to (pointers to) arbitrary objects of classes derived
     * from RObject.  A Frame is usually, but not necessarily,
     * associated with an Frame object.  Frame itself is an
     * abstract class; for most purposes its embodiment ListFrame
     * should be used.
     */
    class Frame : public GCNode
    {
    public:
    private:
    };

} // namespace CXXR

#endif // RFRAME_HPP
