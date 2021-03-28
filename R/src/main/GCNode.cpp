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
#include <CXXR/ProtectStack.hpp>
#include <CXXR/GCManager.hpp>
#include <CXXR/GCStackRoot.hpp>
#include <CXXR/GCRoot.hpp>
#include <iostream>

namespace CXXR
{
    unsigned int GCNode::s_num_generations = 0;
    const GCNode **GCNode::s_generation;
    unsigned int *GCNode::s_next_gen;
    size_t *GCNode::s_gencount;
    size_t GCNode::s_num_nodes;
    GCNode::AgedList *GCNode::s_aged_list;

    GCNode::GCNode(int)
        : m_next(nullptr), m_gcgen(0), m_marked(false), m_aged(false)
    {
        ++s_gencount[0];
        ++s_num_nodes;
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
        if (m_gcgen < mingen)
        {
            --s_gencount[m_gcgen];
            m_gcgen = mingen;
            ++s_gencount[m_gcgen];
            if (!m_aged)
            {
                m_aged = true;
                s_aged_list->push_back(this);
            }
        }
    }

    bool GCNode::check()
    {
        if (s_num_generations == 0)
        {
            std::cerr << "GCNode::check() : class not initialised.\n";
            abort();
        }
        std::vector<size_t> genct(s_num_generations);
        size_t numnodes = 0;
        // Check each generation:
        for (unsigned int gen = 0; gen < s_num_generations; ++gen)
        {
            OldToNewChecker o2n(gen);
            for (const GCNode *node = s_generation[gen]; node; node = node->next())
            {
                unsigned int ngen = node->m_gcgen;
                ++genct[ngen];
                if (node->isMarked())
                {
                    std::cerr << "GCNode::check() : marked node found.\n";
                    abort();
                }
                if (ngen >= s_num_generations)
                {
                    std::cerr << "GCNode::check() : Illegal generation number.\n";
                    abort();
                }
                if (ngen < gen)
                {
                    std::cerr << "GCNode::check() : node has wrong generation for its list.\n";
                    abort();
                }
                // Don't try visiting children of nodes in Generation
                // 0, because these nodes may still be under construction:
                if (gen > 0 && !node->m_aged)
                    node->visitChildren(&o2n);
            }
        }
        // Check generation counts:
        for (unsigned int gen = 0; gen < s_num_generations; ++gen)
        {
            if (genct[gen] != s_gencount[gen])
            {
                std::cerr << "GCNode::check() : nodes in generation " << gen
                          << " wrongly counted.\nCounted " << genct[gen]
                          << "; expected " << s_gencount[gen] << "\n";
                abort();
            }
            numnodes += genct[gen];
        }
        // Check total number of nodes:
        if (numnodes != s_num_nodes)
        {
            std::cerr << "GCNode::check() : generation node totals inconsistent with grand total.\n";
            abort();
        }
        return true;
    }

    void GCNode::cleanup()
    {
        ProtectStack::cleanup();
        GCStackRootBase::cleanup();
        GCRootBase::cleanup();
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
            s_generation[gen] = nullptr;
            s_next_gen[gen] = gen + 1;
            s_gencount[gen] = 0;
        }
        s_next_gen[0] = 0;
        s_next_gen[s_num_generations - 1] = s_num_generations - 1;
        GCRootBase::initialize();
        GCStackRootBase::initialize();
        ProtectStack::initialize();
    }

    void GCNode::nodeCheck(const GCNode *node)
    {
        if (node && node->m_gcgen >= s_num_generations)
            abort();
    }

    void GCNode::propagateAges()
    {
        for (AgedList::const_iterator it = s_aged_list->begin();
             it != s_aged_list->end(); ++it)
        {
            const GCNode *node = *it;
            // node may already have been visited by an Ager on an earlier
            // round of this loop, in which case m_aged will have been set
            // false and there's nothing more to do:
            if (node->m_aged)
            {
                Ager ager(node->m_gcgen);
                node->visitChildren(&ager);
                node->m_aged = false;
            }
        }
        s_aged_list->clear();
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
        std::vector<XferList *> xferlist(s_num_generations);
        for (unsigned int gen = 0; gen < s_num_generations; ++gen)
            xferlist[gen] = new XferList();
        size_t ans = 0;
        const GCNode *node = s_generation[0];
        while (node)
        {
            const GCNode *next = node->next();
            if (node->m_gcgen > 0)
                xferlist[node->m_gcgen]->append(node);
            else
            {
                delete node;
                ++ans;
            }
            node = next;
        }
        // Now move nodes on each transfer list into the generation list itself:
        for (unsigned int gen = 0; gen < s_num_generations; ++gen)
        {
            xferlist[gen]->prependTo(&s_generation[gen]);
            delete xferlist[gen];
        }
        return ans;
    }

    // In implementing sweep(), as far as possible: (i) visit each node
    // only once; (ii) deallocate nodes in the reverse order of
    // allocation.

    void GCNode::sweep(unsigned int max_generation)
    {
        std::vector<XferList *> xferlist(s_num_generations);
        for (unsigned int gen = 0; gen < s_num_generations; ++gen)
            xferlist[gen] = new XferList();
        // Process generations:
        for (unsigned int gen = 0; gen <= max_generation; ++gen)
        {
            // Scan through generation:
            const GCNode *node = s_generation[gen];
            while (node)
            {
                const GCNode *next = node->next();
                unsigned int ngen = node->m_gcgen;
                if (ngen <= max_generation && ngen != 0)
                {
                    if (!node->isMarked())
                    {
                        delete node;
                        node = nullptr;
                    }
                    else
                    {
                        // Advance generation:
                        --s_gencount[ngen];
                        node->m_gcgen = s_next_gen[ngen];
                        ++s_gencount[node->m_gcgen];
                    }
                }
                if (node)
                {
                    node->m_marked = false;
                    xferlist[node->m_gcgen]->append(node);
                }
                node = next;
            }
            // Zap the list:
            s_generation[gen] = nullptr;
        }
        // Now move nodes on each transfer list into the generation list itself:
        for (unsigned int gen = 0; gen < s_num_generations; ++gen)
        {
            xferlist[gen]->prependTo(&s_generation[gen]);
            delete xferlist[gen];
        }
        // std::cerr << "s_gencount[0] = " << s_gencount[0] << std::endl;
    }

    bool GCNode::Ager::operator()(const GCNode *node)
    {
        if (node->m_gcgen < m_mingen) // node is younger than the minimum age required
        {
            --s_gencount[node->m_gcgen];
            node->m_gcgen = m_mingen;
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
    unsigned int GCNode::gcgen(const GCNode *x)
    {
        return x->m_gcgen;
    }

    void GCNode::set_gcgen(const GCNode *x, unsigned int v)
    {
        if (!x)
            return;
        x->m_gcgen = v;
    }

    const GCNode *GCNode::next_node(const GCNode *x)
    {
        return x ? x->m_next : nullptr;
    }

    void GCNode::set_next_node(const GCNode *x, const GCNode *t)
    {
        if (!x)
            return;
        x->m_next = t;
    }

    bool GCNode::is_marked(const GCNode *x)
    {
        return x && x->m_marked;
    }

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

// ***** C interface *****

int MARK(SEXP x)
{
    return CXXR::GCNode::is_marked(static_cast<CXXR::GCNode *>(x));
}
