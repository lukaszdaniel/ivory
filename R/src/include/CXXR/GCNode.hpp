/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2006   The R Development Core Team.
 *  Andrew Runnalls (C) 2007
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
 */

/** @file GCNode.hpp
 * Class GCNode and associated C-callable routines.
 */

#ifndef GCNODE_HPP
#define GCNODE_HPP

#include <CXXR/MemoryBank.hpp>

namespace R
{
    class GCNode
    {
    private:
        GCNode *gengc_prev_node;
        GCNode *gengc_next_node;
        unsigned int m_gcgen : 2;
        unsigned int m_gcclass : 3; /* node class */
        bool m_marked : 1;

    public:
        GCNode();

     /** Allocate memory.
     *
	 * Allocates memory for a new object of a class derived from
	 * GCNode, and zero the memory thus allocated.
	 *
	 * @param bytes Number of bytes of memory required.
	 *
	 * @note Since objects of classes derived from RObject \e must
	 * be allocated on the heap, constructors of these classes may
	 * rely on the fact that operator new zeroes the allocated
	 * memory to elide member initializations.
	 */
        static void *operator new(size_t bytes)
        {
            return memset(MemoryBank::allocate(bytes), 0, bytes);
        }

        static void operator delete(void *p, size_t bytes)
        {
            MemoryBank::deallocate(p, bytes);
        }

     /** Delete a GCNode
	 *
	 * @note Because the class destructors are not public, objects
	 * of classes derived from GCNode must be deleted by calling
	 * this method.
	 */
        void destroy() const { delete this; }

        // To be protected in future:

     /** Destructor
	 *
	 * @note The destructor is protected to ensure that GCNodes
	 * are allocated on the heap.  (See Meyers 'More Effective
	 * C++' Item 27.) Derived classes should likewise declare
	 * their constructors private or protected.
	 */
        virtual ~GCNode();

        static bool gcgen(GCNode *v);
        static void set_gcgen(GCNode *v, bool x);
        static unsigned int gccls(GCNode *x);
        static void set_gccls(GCNode *x, unsigned int v);
        static GCNode *next_node(GCNode *s);
        static GCNode *prev_node(GCNode *s);
        static void set_next_node(GCNode *s, GCNode *t);
        static void set_prev_node(GCNode *s, GCNode *t);
        static bool mark(GCNode *x);
        static void set_mark(GCNode *x, bool v);
        // Special constructor for pegs.  The parameter is simply to
        // give this constructor a distinct signature. Note that the
        // node count isn't altered.
        explicit GCNode(int /*ignored*/)
            : gengc_prev_node(this), gengc_next_node(this)
        {
        }

        // Make t the successor of s:
        static void link(GCNode *s, GCNode *t)
        {
            s->gengc_next_node = t;
            t->gengc_prev_node = s;
        }
    };
} // namespace R

#endif /* GCNODE_HPP */