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

/** @file ListVector.cpp
 *
 * Implementation of class ListVector and related functions.
 *
 * @todo Tidy up handling of names attribute, in particular to get rid
 * of <tt>const_cast</tt>.
 */

#include <CXXR/ListVector.hpp>
#include <CXXR/ExpressionVector.hpp>
#include <CXXR/Symbol.hpp>

using namespace std;

namespace CXXR
{
    namespace ForceNonInline
    {
        // Force the creation of non-inline embodiments of functions callable
        // from C:
        const auto &SET_VECTOR_ELTp = SET_VECTOR_ELT;
        const auto &VECTOR_ELTp = VECTOR_ELT;
    } // namespace ForceNonInline

    ListVector::ListVector(const ExpressionVector &ev)
        : EdgeVector<RObject *, VECSXP>(ev.size())
    {
        for (unsigned int i = 0; i < size(); ++i)
            (*this)[i] = ev[i];
        SEXP names = Rf_getAttrib(const_cast<ExpressionVector *>(&ev),
                                  R_NamesSymbol);
        if (names)
            Rf_setAttrib(this, R_NamesSymbol, names);
    }
} // namespace CXXR
