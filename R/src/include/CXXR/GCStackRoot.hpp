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
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

/** @file GCStackRoot.hpp
 *
 * @brief Templated class CXXR::GCStackRoot and its untemplated base class
 * CXXR::GCStackRootBase.
 */

#ifndef GCSTACKROOT_HPP
#define GCSTACKROOT_HPP

#include <vector>
#include <iostream>

#include <CXXR/RObject.hpp>
#include <CXXR/GCNode.hpp>

namespace CXXR
{
    class RObject;

    /** @brief Untemplated base class for GCStackRoot.
     *
     * The preferred method for C++ code to protect a GCNode
     * from the garbage collector is to use the templated class
     * GCStackRoot, of which this is the untemplated base class.
     *
     * However, GCStackRoot is not usable by C code, which should continue
     * to use ::PROTECT(), ::UNPROTECT() etc. as in CR.
     * However, these functions have been reimplemented to manipulate
     * a C pointer protection stack (as we shall call it, despite the
     * fact that it's implemented in C++) encapsulated as a static
     * member within GCStackRootBase.
     */
    class GCStackRootBase
    {
    public:
        /** @brief Conduct a const visitor to all 'root' GCNode objects.
         *
         * Conduct a GCNode::const_visitor object to each root GCNode
         * and each node on the C pointer protection stack.
         *
         * @param v Pointer to the const_visitor object.
         */
        static void visitRoots(GCNode::const_visitor *v);

    protected:
        /** @brief Primary constructor.
         *
         * @param node Pointer, possibly null, to the node to be protected.
         */
        GCStackRootBase(const GCNode *node, bool expose);

        /** @brief Copy constructor.
         *
         * @param source Pattern for the copy.
         */
        GCStackRootBase(const GCStackRootBase &source);

        ~GCStackRootBase()
        {
            s_roots->pop_back();
            if (m_index != s_roots->size())
                seq_error();
        }

        GCStackRootBase &operator=(const GCStackRootBase &source)
        {
            (*s_roots)[m_index] = (*s_roots)[source.m_index];
            return *this;
        }

        /** @brief Change the node protected by this GCStackRootBase.
         *
         * @param node Pointer to the node now to be protected, or a
         * null pointer.
         */
        void redirect(GCNode *node)
        {
            (*s_roots)[m_index] = node;
        }

        /** @brief Access the encapsulated pointer.
         *
         * @return the GCNode pointer encapsulated by this object.
         */
        const GCNode *ptr() const
        {
            return (*s_roots)[m_index];
        }

    private:
        friend class GCNode;

        // Note that we deliberately do not use CXXR::Allocator in
        // declaring the following vectors: we really don't want a
        // garbage collection happening just as we're trying to
        // protect something from the garbage collector!

        // There may be a case, at least in some C++ library
        // implementations, for using a deque instead of a vector in
        // the following, so that memory is released as the stack
        // shrinks.
        static std::vector<const GCNode *> *s_roots;

        unsigned int m_index;

        // Clean up static data at end of run (called by
        // GCNode::SchwarzCtr destructor:
        static void cleanup()
        {
            delete s_roots;
        }

        // Initialize static data (called by GCNode::SchwarzCtr
        // constructor):
        static void initialize();

        // Report out-of-sequence destructor call and abort program.
        // (We can't use an exception here because it's called from a
        // destructor.)
        static void seq_error();
    };

    /** @brief Smart pointer to protect a GCNode from garbage
     * collection.
     *
     * This class encapsulates a pointer to an object of a type
     * derived from GCNode.  For as long as the GCStackRoot object exists,
     * the GCNode that it points to will not be garbage collected.
     *
     * GCStackRoot objects are intended to be allocated on the stack, or at
     * file or global scope: the class implementation requires that
     * GCStackRoot objects are destroyed in the reverse order of creation,
     * and the destructor checks this.
     *
     * @param T GCNode or a type publicly derived from GCNode.  This
     *          may be qualified by const, so for example a const
     *          String* may be encapsulated in a GCStackRoot using the type
     *          GCStackRoot<const String>.
     *
     * \par Caller protects:
     * Suppose some code calls a function (or class method) that takes
     * a pointer or reference to a class derived from GCNode as an
     * argument, and/or returns a pointer to a class derived from
     * GCNode as its return value.  In CXXR, the preferred coding
     * approach is that the \e calling \e code should take
     * responsibility for protecting the arguments from the garbage
     * collector before calling the function, and likewise take
     * responsibility for protecting the returned value.  This is
     * because the calling code is in a better position to decide
     * whether any additional steps are necessary to achieve this, and
     * what they should be.  (The calling code may also need to protect
     * other objects: objects that are neither arguments to or values
     * returned from the called function, but which would otherwise be
     * vulnerable if the called function gave rise to a garbage
     * collection.)
     */
    template <class T = RObject>
    class GCStackRoot : public GCStackRootBase
    {
    public:
        /**
         * @param node Pointer the node to be pointed to, and
         *          protected from the garbage collector, or a null
         *          pointer.
         *
         * @param expose If true, and \a node is not a null pointer, a
         *          side effect of the constructor is to expose \a
         *          node and its descendants to the garbage collector.
         */
        explicit GCStackRoot(T *node = nullptr, bool expose = false)
            : GCStackRootBase(node, expose) {}

        /** @brief Copy constructor.
         *
         * The constructed GCStackRoot will protect the same GCNode as
         * source.  (There is probably no reason to use this
         * constructor.)
         */
        GCStackRoot(const GCStackRoot &source) : GCStackRootBase(source) {}

        /**
         * This will cause this GCStackRoot to protect the same GCNode as
         * is protected by source.  (There is probably no reason to
         * use this method.)
         */
        GCStackRoot &operator=(const GCStackRoot &source)
        {
            GCStackRootBase::operator=(source);
            return *this;
        }

        /**
         * This will cause this GCStackRoot to point to and protect node,
         * instead of the node (if any) it currently points to and
         * protects.
         *
         * @param node Pointer to the GCNode that is now to be pointed
         *          to and protected from the garbage collector.
         */
        GCStackRoot &operator=(T *node)
        {
            GCStackRootBase::redirect(node);
            return *this;
        }

        /** @brief Access member via encapsulated pointer.
         *
         * @return the pointer currently encapsulated by the node.
         */
        T *const operator->() const
        {
            return get();
        }

        /** @brief Dereference the encapsulated pointer.
         *
         * @return a reference to the object pointed to by the
         * encapsulated pointer.  The effect is undefined if this
         * object encapsulates a null pointer.
         */
        T &operator*() const
        {
            return *get();
        }

        /** @brief Implicit conversion to encapsulated pointer type.
         *
         * @return the pointer currently encapsulated by the node.
         * The pointer is of type \a T* const to prevent its use as
         * an lvalue, the effect of which would probably not be what
         * the programmer wanted.
         */
        operator T * const() const
        {
            return get();
        }

        /** @brief Access the encapsulated pointer.
         *
         * @return the pointer currently encapsulated by the node.
         */
        T *get() const
        {
            return static_cast<T *>(const_cast<GCNode *>(ptr()));
        }
    };
} // namespace CXXR

#endif // GCSTACKROOT_HPP
