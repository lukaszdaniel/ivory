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

/** @file RAltRep.cpp
 *
 * @brief Class CXXR::AltRep.
 */

#include <CXXR/RAltRep.hpp>
#include <CXXR/String.hpp>
#include <CXXR/StringVector.hpp>

using namespace std;
using namespace CXXR;

namespace CXXR
{
    const char *AltRep::typeName() const
    {
        return staticTypeName();
    }

    AltRep *AltRep::clone(bool deep) const
    {
        GCStackRoot<const AltRep> thisroot(this); /* the methods should protect, but ... */
        SEXP ans = ALTREP_DUPLICATE_EX(const_cast<AltRep *>(this), Rboolean(deep));
        if (ans)
        {
            return SEXP_downcast<AltRep *>(ans);
        }
        else
        {
            GCStackRoot<RObject> t;
            if (altsexptype() == STRSXP)
            {
                R_xlen_t sz = XLENGTH(const_cast<AltRep *>(this));
                t = new StringVector(sz);
                memcpy(STRING_PTR(t), STRING_PTR(const_cast<AltRep *>(this)), sz * sizeof(String *));
                if (hasAttributes())
                {
                    t->cloneAttributes(*(this), deep);
                }
                SET_TRUELENGTH(t, XTRUELENGTH(const_cast<AltRep *>(this)));
            }
            else
            {
                std::cerr << "Cloning not implemented yet (" << altsexptype() << ")" << std::endl;
                abort();
            }
            return static_cast<AltRep *>(t.get());
        }
    }
} // namespace CXXR

// ***** C interface *****

SEXP ALTREP_CLASS(SEXP x)
{
    return TAG(x);
}

SEXP R_altrep_data1(SEXP x)
{
    return CAR(x);
}

SEXP R_altrep_data2(SEXP x)
{
#ifdef CXXR_OLD_ALTREP_IMPL
    return CDR(x);
#else
    return CADR(x);
#endif
}

void R_set_altrep_data1(SEXP x, SEXP v)
{
    SETCAR(x, v);
}

void R_set_altrep_data2(SEXP x, SEXP v)
{
#ifdef CXXR_OLD_ALTREP_IMPL
    SETCDR(x, v);
#else
    SETCADR(x, v);
#endif
}
