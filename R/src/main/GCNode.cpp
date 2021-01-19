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
#include <CXXR/GCManager.hpp>
#include <CXXR/GCRoot.hpp>
#include <iostream>

namespace CXXR
{
    unsigned int GCNode::SchwarzCtr::s_count = 0;
    unsigned int GCNode::s_num_generations = 0;
    const GCNode **GCNode::s_generation;
    unsigned int *GCNode::s_next_gen;
    size_t *GCNode::s_gencount;
    size_t GCNode::s_num_nodes;
    GCNode::AgedList *GCNode::s_aged_list;

    GCNode::SchwarzCtr::SchwarzCtr()
    {
        if (!s_count++)
        {
            GCNode::initialize();
            GCRootBase::initialize();
        }
    }

    GCNode::SchwarzCtr::~SchwarzCtr()
    {
        if (!--s_count)
        {
            GCRootBase::cleanup();
            GCNode::cleanup();
        }
    }

    GCNode::GCNode(int)
        : m_prev(this), m_next(this)
    {
    }

    /*
void* GCNode::operator new(size_t bytes)
{
    void* ans = MemoryBank::allocate(bytes);
    cout << "Node of " << bytes << " bytes allocated at " << ans << endl;
    return ans;
}
*/

    void GCNode::ageTo(unsigned int mingen) const
    {
        Ager ager(mingen);
        this->conductVisitor(&ager);
    }

    bool GCNode::check()
    {
        if (s_num_generations == 0)
        {
            std::cerr << "GCNode::check() : class not initialised.\n";
            abort();
        }
        // Check each generation:
        {
            unsigned int numnodes = 0;
            for (unsigned int gen = 0; gen < s_num_generations; ++gen)
            {
                unsigned int gct = 0;
                OldToNewChecker o2n(gen);
                for (const GCNode *node = s_generation[gen]->next();
                     node != s_generation[gen]; node = node->next())
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
            // s_num_nodes > numnodes is possible because of infant immunity.
            if (numnodes > s_num_nodes)
            {
                std::cerr << "GCNode::check() : generation node totals inconsistent with grand total.\n";
                std::cerr << "GCNode::check() : expected s_num_nodes = " << s_num_nodes << ", got: " << numnodes << "\n.";
                abort();
            }
        }
        return true;
    }

    void GCNode::cleanup()
    {
        delete s_aged_list;
        delete[] s_gencount;
        delete[] s_next_gen;
        delete[] s_generation;
    }

    void GCNode::initialize()
    {
        s_num_generations = GCManager::numGenerations();
        s_generation = new const GCNode *[s_num_generations];
        s_next_gen = new unsigned int[s_num_generations];
        s_gencount = new size_t[s_num_generations];
        s_aged_list = new std::vector<const GCNode *>;
        for (unsigned int gen = 0; gen < s_num_generations; ++gen)
        {
            s_generation[gen] = new GCNode(0);
            s_next_gen[gen] = gen + 1;
            s_gencount[gen] = 0;
        }
        s_next_gen[0] = 0;
        s_next_gen[s_num_generations - 1] = s_num_generations - 1;
    }

    void GCNode::propagateAges()
    {
    }

    // Structure used to marshal nodes awaiting transfer to a
    // different generation.  Nodes are added to the end of the
    // list to help maintain the generation lists as far as
    // possible in reverse order of node allocation.
    class GCNode::XferList
    {
    public:
        XferList()
            : m_peg(0), m_last(&m_peg)
        {
        }

        void append(const GCNode *node)
        {
            node->m_next = nullptr;
            m_last->m_next = node;
            m_last = node;
        }

        // Export the transfer list by prepending it to the list
        // whose first element is pointed to by *listp.  Following
        // this operation, the transfer list will itself be empty.
        void prependTo(const GCNode **listp)
        {
            m_last->m_next = *listp;
            *listp = m_peg.m_next;
            m_peg.m_next = nullptr;
            m_last = &m_peg;
        }

    private:
        const GCNode m_peg;   // Dummy first element of list
        const GCNode *m_last; // Pointer to last element
    };

    size_t GCNode::slaughterInfants()
    {
        return 0;
    }

    // In implementing sweep(), as far as possible: (i) visit each node
    // only once; (ii) deallocate nodes in the reverse order of
    // allocation.

    void GCNode::sweep(unsigned int max_generation)
    {
        // Sweep.  gen must be signed here or the loop won't terminate!
        for (int gen = max_generation; gen >= 0; --gen)
        {
            if (gen == int(s_num_generations - 1))
            {
                // Delete unmarked nodes and unmark the rest:
                const GCNode *node = s_generation[gen]->next();
                while (node != s_generation[gen])
                {
                    const GCNode *next = node->next();
                    if (!node->isMarked())
                    {
                        delete node;
                    }
                    else
                    {
                        node->m_marked = false;
                    }
                    node = next;
                }
            }
            else
            {
                // Delete unmarked nodes, unmark the rest and promote them
                // to the next generation:
                const GCNode *node = s_generation[gen]->next();
                while (node != s_generation[gen])
                {
                    const GCNode *next = node->next();
                    if (!node->isMarked())
                    {
                        delete node;
                    }
                    else
                    {
                        node->m_marked = false;
                        ++node->m_gcgen;
                    }
                    node = next;
                }
                s_generation[gen + 1]->splice(s_generation[gen]->next(), s_generation[gen]);
                s_gencount[gen + 1] += s_gencount[gen];
                s_gencount[gen] = 0;
            }
        }
    }

    bool GCNode::Ager::operator()(const GCNode *node)
    {
        node->expose();
        if (node->m_gcgen < m_mingen) // node is younger than the minimum age required
        {
            --s_gencount[node->m_gcgen];
            node->m_gcgen = m_mingen;
            s_generation[m_mingen]->splice(node);
            ++s_gencount[m_mingen];
            node->m_aged = false;
            return true;
        }
        else
            return false;
    }

    bool GCNode::Marker::operator()(const GCNode *node)
    {
        /*
    if (node->m_gcgen == 0)
	cerr << "Warning: Marker encountered gen 0 node.\n";
    */
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

    void GCNode::set_next_node(const GCNode *x, const GCNode *t)
    {
        if (!x)
            return;
        x->m_next = t;
    }

    const GCNode *GCNode::prev_node(const GCNode *x) { return x ? x->m_prev : nullptr; }

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
} // namespace CXXR
