/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998-2007   The R Development Core Team.
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
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

/** @file GCNode.cpp
 *
 * Class GCNode and associated C-callable functions.
 */

#include <CXXR/GCNode.hpp>

namespace R
{
    /* General Cons Cell Attributes */
    bool GCNode::gcgen(GCNode *x) { return x && x->m_gcgen; }

    void GCNode::set_gcgen(GCNode *x, bool v)
    {
        if (!x)
            return;
        x->m_gcgen = v;
    }
    unsigned int GCNode::gccls(GCNode *x) { return x ? x->m_gcclass : 0; }

    void GCNode::set_gccls(GCNode *x, unsigned int v)
    {
        if (!x)
            return;
        x->m_gcclass = v;
    }

    GCNode *GCNode::next_node(GCNode *x) { return x ? x->gengc_next_node : nullptr; }

    GCNode *GCNode::prev_node(GCNode *x) { return x ? x->gengc_prev_node : nullptr; }

    void GCNode::set_next_node(GCNode *x, GCNode *t)
    {
        if (!x)
            return;
        x->gengc_next_node = t;
    }

    void GCNode::set_prev_node(GCNode *x, GCNode *t)
    {
        if (!x)
            return;
        x->gengc_prev_node = t;
    }

    bool GCNode::mark(GCNode *x) { return x && x->m_marked; }

    void GCNode::set_mark(GCNode *x, bool v)
    {
        if (!x)
            return;
        x->m_marked = v;
    }
} // namespace R
