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
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

/** @file Expression.cpp
 *
 * @brief Class Expression and associated C interface.
 */

#include <CXXR/Expression.hpp>

#include <iostream>

#include <R_ext/Error.h>
#include <Localization.h>
#include <CXXR/BuiltInFunction.hpp>
#include <CXXR/Closure.hpp>
#include <CXXR/Environment.hpp>
#include <CXXR/FunctionBase.hpp>
#include <CXXR/RAllocStack.hpp>
#include <CXXR/Symbol.hpp>
#include <Rinternals.h>

#undef match

using namespace std;
using namespace CXXR;

// Force the creation of non-inline embodiments of functions callable
// from C:
namespace CXXR
{
    namespace ForceNonInline
    {
        const auto &lconsptr = Rf_lcons;
    }
} // namespace CXXR

// ***** C interface *****

SEXP Rf_currentExpression();

SEXP Rf_lcons(SEXP cr, SEXP tl);

void Rf_setCurrentExpression(SEXP e);
