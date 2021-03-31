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

/** @file GCRoot.cpp
 *
 * Implementation of class GCRootBase.
 */

#include <CXXR/GCRoot.hpp>
#include <CXXR/RObject.hpp>

#include <cstdlib>
#include <iostream>
#include <stdexcept>
#include <unordered_map>

using namespace std;
using namespace CXXR;

namespace CXXR
{
    // Force generation of non-inline embodiments of functions in the C
    // interface:
    namespace ForceNonInline
    {
    } // namespace ForceNonInline

    GCRootBase::List *GCRootBase::s_roots;

    GCRootBase::GCRootBase(const GCNode *node)
        : m_it(s_roots->insert(s_roots->end(), node))
    {
        // GCNode::maybeCheckExposed(node);
    }

    void GCRootBase::cleanup()
    {
        delete s_roots;
    }

    void GCRootBase::initialize()
    {
        s_roots = new List();
    }

    void GCRootBase::visitRoots(GCNode::const_visitor *v)
    {
        for (auto &n : *s_roots)
        {
            if (n)
                n->conductVisitor(v);
        }
    }
} // namespace CXXR

// ***** C interface *****

#if CXXR_FALSE
/** @brief List of Persistent Objects
 *
 * @note This is not a busy list, so we don't bother to use Allocator.
 */
static unordered_map<const RObject *, GCRoot<>> precious;

void R_PreserveObject(SEXP object)
{
    precious[object] = GCRoot<>(object);
}

void R_ReleaseObject(SEXP object)
{
    precious.erase(object);
}
#endif
