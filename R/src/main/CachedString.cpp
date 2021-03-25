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

/** @file CachedString.cpp
 *
 * Implementation of class CXXR::CachedString and related functions.
 */

#include <CXXR/CachedString.hpp>
#include <boost/lambda/lambda.hpp>

using namespace std;
using namespace CXXR;

namespace CXXR
{
    // Force the creation of non-inline embodiments of functions callable
    // from C:
    namespace ForceNonInline
    {
        const auto &mkCharptr = Rf_mkChar;
    } // namespace ForceNonInline

    hash<std::string> CachedString::Hasher::s_string_hasher;

    CachedString::map *CachedString::getCache()
    {
        static map *cache = new map();
        return cache;
    }

    bool isASCII(const std::string &str)
    {
        using namespace boost::lambda;
        // Beware of the iterator dereferencing to a *signed* char, hence
        // the bitwise test:
        std::string::const_iterator it = std::find_if(str.begin(), str.end(), _1 & 0x80);
        return it == str.end();
    }

    CachedString *CachedString::obtain(const std::string &str, cetype_t encoding)
    {
        // This will be checked again when we actually construct the
        // CachedString, but we precheck now so that we don't create an
        // invalid cache key:
        switch (encoding)
        {
        case CE_NATIVE:
        case CE_UTF8:
        case CE_LATIN1:
        case CE_BYTES:
            break;
        default:
            Rf_error(_("unknown encoding: %d"), encoding);
        }

        bool ascii = CXXR::isASCII(str);
        if (ascii)
            encoding = CE_NATIVE;

        pair<map::iterator, bool> pr = getCache()->insert(map::value_type(key(str, encoding), nullptr));
        map::iterator it = pr.first;
        if (pr.second)
        {
            try
            {
                map::value_type &val = *it;
                val.second = new CachedString(str, encoding, ascii);
                val.second->expose();
                val.second->m_key_val_pr = &*it;
            }
            catch (...)
            {
                getCache()->erase(it);
                throw;
            }
        }
        return (*it).second;
    }

    CachedString *CachedString::findInCache(const std::string &str, cetype_t enc)
    {
        auto search = getCache()->find(key(str, enc));
        if (search != getCache()->end())
            return search->second;

        return nullptr;
    }

    const char *CachedString::typeName() const
    {
        return CachedString::staticTypeName();
    }
} // namespace CXXR
