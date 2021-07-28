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
#include <CXXR/GCStackRoot.hpp>
#include <Rinternals.h>

using namespace R;
using namespace CXXR;

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
        // return GCNode::expose(new StringVector(*this, deep));
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

SEXP *STRING_PTR(SEXP x)
{
    if (TYPEOF(x) != STRSXP)
        Rf_error(_("'%s' function can only be applied to a character, not a '%s'"),
                 "STRING_PTR()", Rf_type2char(TYPEOF(x)));
    CXXR::VectorBase::chkzln(x);
    return STRINGVECTOR_STRING_PTR(x);
}

const SEXP *STRING_PTR_RO(SEXP x)
{
    if (TYPEOF(x) != STRSXP)
        Rf_error(_("'%s' function can only be applied to a character, not a '%s'"),
                 "STRING_PTR_RO()", Rf_type2char(TYPEOF(x)));
    CXXR::VectorBase::chkzln(x);
    return STRINGVECTOR_STRING_PTR_RO(x);
}

void SET_STRING_ELT(SEXP x, R_xlen_t i, SEXP v)
{
    if (TYPEOF(x) != STRSXP)
        Rf_error(_("'%s' function can only be applied to a character vector, not a '%s'"), "SET_STRING_ELT()",
                 Rf_type2char(TYPEOF(x)));
    if (TYPEOF(v) != CHARSXP)
        Rf_error(_("value of 'SET_STRING_ELT()' function must be a 'CHARSXP' not a '%s'"),
                 Rf_type2char(TYPEOF(v)));
    if (i < 0 || i >= XLENGTH(x))
        Rf_error(_("attempt to set index %ld/%ld in 'SET_STRING_ELT()' function"), (long long)i, (long long)XLENGTH(x));

    x->propagateAge(v);
    if (ALTREP(x))
        ALTSTRING_SET_ELT(x, i, v);
    else
    {
        StringVector *sv = SEXP_downcast<StringVector *>(x);
        String *el = SEXP_downcast<String *>(v);
        (*sv)[i] = el;
    }
}

SEXP STRING_ELT(SEXP x, R_xlen_t i)
{
    if (TYPEOF(x) != STRSXP)
        Rf_error(_("'%s' function can only be applied to a character vector, not a '%s'"), "STRING_ELT()",
                 Rf_type2char(TYPEOF(x)));
    if (ALTREP(x))
        return ALTSTRING_ELT(x, i);
    else
    {
        SEXP *ps = CXXR::stdvec_dataptr<SEXP>(x);
        return ps[i];
    }
}

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
    CXXR::GCStackRoot<> xx(x);
    ans = Rf_allocVector(STRSXP, (R_xlen_t)1);
    SET_STRING_ELT(ans, (R_xlen_t)0, x);
    return ans;
}
