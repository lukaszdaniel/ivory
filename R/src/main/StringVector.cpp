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
#include <CXXR/GCRoot.hpp>
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

    StringVector *StringVector::clone(bool deep) const
    {
        return new StringVector(*this, deep);
    }

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

// ***** C interface *****

int Rf_stringPositionTr(SEXP string, const char *translatedElement)
{

    int slen = LENGTH(string);

    const void *vmax = vmaxget();
    for (int i = 0; i < slen; ++i)
    {
        bool found = (strcmp(Rf_translateChar(STRING_ELT(string, i)), translatedElement) == 0);
        vmaxset(vmax);
        if (found)
            return i;
    }
    return -1; /* not found */
}

Rboolean Rf_isValidString(SEXP x)
{
    return Rboolean(TYPEOF(x) == STRSXP && LENGTH(x) > 0 && TYPEOF(STRING_ELT(x, 0)) != NILSXP);
}

Rboolean Rf_isValidStringF(SEXP x)
{
    return Rboolean(Rf_isValidString(x) && R_CHAR(STRING_ELT(x, 0))[0]);
}

SEXP Rf_ScalarString(SEXP x)
{
    SEXP ans;
    CXXR::GCRoot<> xx(x);
    ans = Rf_allocVector(STRSXP, (R_xlen_t)1);
    SET_STRING_ELT(ans, (R_xlen_t)0, x);
    return ans;
}