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

/** @file String.cpp
 *
 * Implementation of class String and related functions.
 */

#include <CXXR/String.hpp>

namespace CXXR
{
    // Force the creation of non-inline embodiments of functions callable
    // from C:
    namespace ForceNonInline
    {
        // const auto &ENC_KNOWNp = ENC_KNOWN;
        // const auto &IS_ASCIIp = IS_ASCII;
        // const auto &IS_BYTESp = IS_BYTES;
        // const auto &IS_LATIN1p = IS_LATIN1;
        // const auto &IS_UTF8p = IS_UTF8;
        // const auto &R_CHARp = R_CHAR;
        // const auto &mkCharp = Rf_mkChar;
        // const auto &mkCharCEp = Rf_mkCharCE;
        // const auto &mkCharLenp = Rf_mkCharLen;
    } // namespace ForceNonInline

    // String::Comparator::operator()(const String&, const String&) is in
    // sort.cpp

    String::String(size_t sz)
        : VectorBase(CHARSXP, sz), m_data(m_short_string)
    {
        if (sz > s_short_strlen)
        {
            GCRoot<> thisroot(this);
            m_data = reinterpret_cast<char *>(MemoryBank::allocate(sz + 1));
        }
        // Insert trailing null byte:
        m_data[sz] = 0;
    }

    const char *String::typeName() const
    {
        return staticTypeName();
    }
} // namespace CXXR
