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

/** @file DottedArgs.cpp
 *
 * @brief Class CXXR::DottedArgs.
 */

#include <CXXR/DottedArgs.hpp>
#include <Localization.h>

using namespace std;
using namespace CXXR;

namespace CXXR
{
    DottedArgs *DottedArgs::clone(bool deep) const
    {
        // return GCNode::expose(new DottedArgs(*this, deep));
        return new DottedArgs(*this, deep);
    }

    RObject *DottedArgs::evaluate(Environment *env)
    {
        Rf_error(_("'...' used in an incorrect context"));
        return nullptr;
    }

    const char *DottedArgs::typeName() const
    {
        return staticTypeName();
    }
} // namespace CXXR

// ***** C interface *****

Rboolean Rf_isDottedArgs(SEXP s)
{
    return Rboolean(!s || TYPEOF(s) == DOTSXP);
}
