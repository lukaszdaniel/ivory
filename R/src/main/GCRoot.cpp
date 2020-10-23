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
using namespace R;

// Force the creation of non-inline embodiments of functions callable
// from C:
namespace R
{
    namespace ForceNonInline
    {
    }
} // namespace R

vector<GCNode *> GCRootBase::s_roots;

void GCRootBase::seq_error()
{
    cerr << "GCRoots must be destroyed in reverse order of creation\n";
    abort();
}

void GCRootBase::visitRoots(GCNode::const_visitor *v)
{
    for (vector<GCNode *>::iterator it = s_roots.begin();
         it != s_roots.end(); ++it)
    {
        GCNode *n = *it;
        if (n)
            n->conductVisitor(v);
    }
}

void GCRootBase::visitRoots(GCNode::visitor *v)
{
    for (vector<GCNode *>::iterator it = s_roots.begin();
         it != s_roots.end(); ++it)
    {
        GCNode *n = *it;
        if (n)
            n->conductVisitor(v);
    }
}
