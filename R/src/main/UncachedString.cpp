/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999--2020  The R Core Team.
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
 *
 *  This program is free software; you can redistribute it and/or modify
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

/** @file UncachedString.cpp
 *
 * Implementation of class CXXR::UncachedString and related functions.
 */

#include <CXXR/UncachedString.hpp>

using namespace std;
using namespace CXXR;

namespace CXXR
{
    // Force the creation of non-inline embodiments of functions callable
    // from C:
    namespace ForceNonInline
    {
        const auto &SET_LATIN1ptr = SET_LATIN1;
        const auto &SET_UTF8ptr = SET_UTF8;
    } // namespace ForceNonInline

    UncachedString::UncachedString(const std::string &str, cetype_t encoding)
        : String(str.size(), encoding), m_databytes(str.size() + 1),
          m_data(m_short_string)
    {
        size_t sz = str.size();
        allocData(sz);
        memcpy(m_data, str.data(), sz);
    }

    UncachedString *UncachedString::obtain(const std::string &str, cetype_t encoding)
    {
        return GCNode::expose(new UncachedString(str, encoding));
    }

    void UncachedString::allocData(size_t sz)
    {
        GCStackRoot<> thisroot(this);
        if (sz > s_short_strlen)
            m_data = static_cast<char *>(MemoryBank::allocate(m_databytes));
        // Insert trailing null byte:
        m_data[sz] = '\0';
        setCString(m_data);
    }

    const char *UncachedString::typeName() const
    {
        return UncachedString::staticTypeName();
    }
} // namespace CXXR
