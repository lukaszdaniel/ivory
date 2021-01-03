/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999--2020  The R Core Team.
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
 *
 *  This header file is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This file is part of R. R is distributed under the terms of the
 *  GNU General Public License, either Version 2, June 1991 or Version 3,
 *  June 2007. See doc/COPYRIGHTS for details of the copyright status of R.
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

/** @file CachedString.cpp
 *
 * Implementation of class CXXR::CachedString and related functions.
 */

#include <CXXR/CachedString.hpp>

using namespace std;
using namespace CXXR;

namespace CXXR
{
    // Force the creation of non-inline embodiments of functions callable
    // from C:
    namespace ForceNonInline
    {
        // const auto &mkCharp = Rf_mkChar;
        // const auto &mkCharEncp = Rf_mkCharEnc;
    } // namespace ForceNonInline
} // namespace CXXR

CachedString::map CachedString::s_cache;

const CachedString *CachedString::obtain(const std::string &str, unsigned int encoding)
{
    // This will be checked again when we actually construct the
    // CachedString, but we precheck now so that we don't create an
    // invalid cache key:
    if (encoding != 0 && encoding != UTF8_MASK && encoding != LATIN1_MASK)
        Rf_error("unknown encoding mask: %d", encoding);
    pair<map::iterator, bool> pr = s_cache.insert(map::value_type(key(str, encoding), 0));
    map::iterator it = pr.first;
    if (pr.second)
    {
        try
        {
            (*it).second = new CachedString(it);
        }
        catch (...)
        {
            s_cache.erase(it);
            throw;
        }
    }
    return (*it).second;
}

const char *CachedString::typeName() const
{
    return CachedString::staticTypeName();
}