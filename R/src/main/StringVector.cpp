/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999--2020  The R Core Team.
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
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
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

/** @file StringVector.cpp
 *
 * @brief Implementation of class StringVector and related functions.
 */

#include <CXXR/StringVector.hpp>
#include <Rinternals.h>
#include <iostream>

namespace CXXR
{
    // Force the creation of non-inline embodiments of functions callable
    // from C:
    namespace ForceNonInline
    {
        const auto &isStringptr = Rf_isString;
        const auto &SET_STRING_ELTptr = SET_STRING_ELT;
        const auto &STRING_ELTptr = STRING_ELT;
    } // namespace ForceNonInline

    namespace
    {
        // void indent(std::ostream &os, size_t margin)
        // {
        //     while (margin--)
        //         os << " ";
        // }
    } // namespace

    void strdump(std::ostream &os, const StringVector *sv, size_t margin)
    {
        if (!sv)
        {
            os << "sv is nullptr" << std::endl;
            return;
        }
        // indent(os, margin);
        os << "character:";
        for (R_xlen_t i = 0; i < sv->size(); ++i)
        {
            // indent(os, margin + 2);
            os << " " << (*sv)[i]->c_str() << ";";
        }
        os << "\n";
    }
} // namespace CXXR
