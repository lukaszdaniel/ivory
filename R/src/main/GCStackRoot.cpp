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

/** @file GCStackRoot.cpp
 *
 * Implementation of class GCStackRootBase.
 */

#include <CXXR/GCStackRoot.hpp>
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

    vector<const GCNode *> *GCStackRootBase::s_roots;

    GCStackRootBase::GCStackRootBase(const GCNode *node, bool expose)
        : m_index(s_roots->size())
    {
        s_roots->push_back(node);
        if (expose && node)
            node->expose();
    }

    GCStackRootBase::GCStackRootBase(const GCStackRootBase &source)
        : m_index(s_roots->size())
    {
        s_roots->push_back((*s_roots)[source.m_index]);
    }

    void GCStackRootBase::initialize()
    {
        s_roots = new vector<const GCNode *>;
    }

    void GCStackRootBase::seq_error()
    {
        std::cerr << "Fatal error: GCStackRoots must be destroyed in reverse order of creation\n";
        abort();
    }

    void GCStackRootBase::visitRoots(GCNode::const_visitor *v)
    {
        for (auto &n : *s_roots)
        {
            if (n)
                n->conductVisitor(v);
        }
    }
} // namespace CXXR