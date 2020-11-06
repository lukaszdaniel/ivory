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
#include <CXXR/RAllocStack.hpp>
#include <iostream>

namespace R
{
    unsigned int GCNode::s_last_gen;
    std::vector<GCNode *> GCNode::s_genpeg;
    std::vector<unsigned int> GCNode::s_gencount;
    size_t GCNode::s_num_nodes;

    GCNode::GCNode()
    {
        link(s_genpeg[0]->m_prev, this);
        link(this, s_genpeg[0]);
        ++s_gencount[0];
        ++s_num_nodes;
    }

    GCNode::~GCNode()
    {
        --s_num_nodes;
        --s_gencount[m_gcgen];
        link(m_prev, m_next);
    }

    bool GCNode::check()
    {
        if (s_genpeg.size() == 0)
        {
            std::cerr << "GCNode::check() : class not initialised.\n";
            abort();
        }
        if (s_genpeg.size() != s_last_gen + 1 || s_genpeg.size() != s_gencount.size())
        {
            std::cerr << "GCNode::check() : internal vectors inconsistently sized.\n";
            abort();
        }
        // Check each generation:
        {
            unsigned int numnodes = 0;
            for (unsigned int gen = 0; gen <= s_last_gen; ++gen)
            {
                unsigned int gct = 0;
                OldToNewChecker o2n(gen);
                for (const GCNode *node = s_genpeg[gen]->next();
                     node != s_genpeg[gen]; node = node->next())
                {
                    ++gct;
                    if (node->isMarked())
                    {
                        std::cerr << "GCNode::check() : marked node found for gen = " << gen << ".\n";
                        abort();
                    }
                    if (node->m_gcgen != gen)
                    {
                        std::cerr << "GCNode::check() : node has wrong generation for its list (node gen = " << node->m_gcgen << ", list gen = " << gen << ").\n";
                        abort();
                    }
                    node->visitChildren(&o2n);
                }
                if (gct != s_gencount[gen])
                {
                    std::cerr << "GCNode::check() : nodes in generation " << gen << " wrongly counted.\n";
                    std::cerr << "GCNode::check() : expected s_gencount[" << gen << "] = " << s_gencount[gen] << ", got: " << gct << ".\n";
                    abort();
                }
                numnodes += gct;
            }
            if (numnodes != s_num_nodes)
            {
                std::cerr << "GCNode::check() : generation node totals inconsistent with grand total.\n";
                std::cerr << "GCNode::check() : expected s_num_nodes = " << s_num_nodes << ", got: " << numnodes << "\n.";
                abort();
            }
        }
        return true;
    }

    void GCNode::initialize(unsigned int num_old_generations)
    {
        if (s_genpeg.size() == 0)
        {
            s_last_gen = num_old_generations;
            s_genpeg.resize(num_old_generations + 1);
            s_gencount.resize(num_old_generations + 1, 0);
            for (unsigned int gen = 0; gen <= s_last_gen; ++gen)
                s_genpeg[gen] = new GCNode(0);
        }
    }

    bool GCNode::Ager::operator()(const GCNode *node)
    {
        if (node->m_gcgen < m_mingen) // node is younger than the minimum age required
        {
            --s_gencount[node->m_gcgen];
            node->m_gcgen = m_mingen;
            s_genpeg[m_mingen]->splice(node);
            ++s_gencount[m_mingen];

            return true;
        }
        else
            return false;
    }

    bool GCNode::Marker::operator()(const GCNode *node)
    {
        if (!node->isMarked() && node->m_gcgen <= m_maxgen) // node is not yet marked and is below the number of generations to be collected
        {
            node->mark();
            return true;
        }
        else
            return false;
    }

    bool GCNode::OldToNewChecker::operator()(const GCNode *node)
    {
        if (node->m_gcgen < m_mingen)
        {
            std::cerr << "GCNode: old to new reference found (node's gen = " << node->m_gcgen << ", mingen = " << m_mingen << ").\n";
            abort();
        }
        return false;
    }

    /* General Cons Cell Attributes */
    unsigned int GCNode::gcgen(const GCNode *x) { return x->m_gcgen; }

    void GCNode::set_gcgen(const GCNode *x, unsigned int v)
    {
        if (!x)
            return;
        x->m_gcgen = v;
    }

    const GCNode *GCNode::next_node(const GCNode *x) { return x ? x->m_next : nullptr; }

    const GCNode *GCNode::prev_node(const GCNode *x) { return x ? x->m_prev : nullptr; }

    void GCNode::set_next_node(const GCNode *x, const GCNode *t)
    {
        if (!x)
            return;
        x->m_next = t;
    }

    void GCNode::set_prev_node(const GCNode *x, const GCNode *t)
    {
        if (!x)
            return;
        x->m_prev = t;
    }

    bool GCNode::is_marked(const GCNode *x) { return x && x->m_marked; }

    void GCNode::set_mark(const GCNode *x, bool v)
    {
        if (!x)
            return;
        x->m_marked = v;
    }

    void initializeMemorySubsystem()
    {
        static bool initialized = false;
        if (!initialized)
        {
            // MemoryBank::initialize();
            // GCNode::initialize();
            // ProtectStack::initialize();
            RAllocStack::initialize();

            initialized = true;
        }
    }
} // namespace R
