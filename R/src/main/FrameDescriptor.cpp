/*
 *  R : A Computer Language for Statistical Data Analysis
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
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

/** @file FrameDescriptor.cpp
 *
 * Implementation of class FrameDescriptor.
 */

#include <algorithm>
#include <set>
#include <CXXR/FrameDescriptor.hpp>
#include <CXXR/Closure.hpp>
#include <CXXR/ConsCell.hpp>
#include <CXXR/Expression.hpp>
#include <CXXR/GCStackRoot.hpp>
#include <CXXR/PairList.hpp>
#include <CXXR/RObject.hpp>
#include <CXXR/Symbol.hpp>

using namespace std;

namespace CXXR
{
} // namespace CXXR
