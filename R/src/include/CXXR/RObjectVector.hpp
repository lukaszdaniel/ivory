/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2008  Andrew Runnalls
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
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

/** @file RObjectVector.hpp
 *
 * @brief Templated class CXXR::RObjectVector.
 */

#ifndef ROBJECTVECTOR_HPP
#define ROBJECTVECTOR_HPP

#include <algorithm>

#include <CXXR/Allocator.hpp>
#include <CXXR/Allocator.hpp>
#include <CXXR/GCRoot.hpp>
#include <CXXR/VectorBase.hpp>
#include <Localization.h>
#include <R_ext/Error.h>

namespace CXXR
{
    /** @brief Vector of pointers to RObject.
     *
     * This is a templated class to represent a vector whose elements
     * are pointers to objects of some type derived from RObject.
     * Copying the vector copies the objects pointed to.
     *
     * @param T This should be RObject or a type (publicly) derived
     * from RObject.  The vector elements will be of type \a T*.
     *
     * @param ST The required ::SEXPTYPE of the vector.
     */
    template <typename T, SEXPTYPE ST>
    class RObjectVector : public VectorBase
    {
    public:
        /** @brief Proxy object for an element of an RObjectVector<T, ST>.
         *
         * Objects of this class are used to allow the elements of an
         * RObjectVector<T, ST> to be examined and modified using
         * the same syntax as would be used for accessing an array of
         * \a T*, whilst nevertheless enforcing the write barrier.
         * See Item 30 of Scott Meyers's 'More Effective C++' for a
         * general discussion of proxy objects, but see the
         * <a href="http://www.aristeia.com/BookErrata/mec++-errata_frames.html">errata</a>.
         * (It may look complicated, but an optimising compiler should
         * be able to distil an invocation of RObjectVector<T,
         * ST>::operator[] into very few instructions.)
         */
        class ElementProxy
        {
        public:
            /** Copy the value of the proxied element from another
             *  proxied element.
             *
             * @param rhs Proxied element whose value is to be copied.
             *
             * @return Reference to this ElementProxy.
             */
            ElementProxy &operator=(const ElementProxy &rhs)
            {
                *m_it = *rhs.m_it;
                if (rhs.m_ev != m_ev)
                    m_ev->propagateAge(*m_it);
                return *this;
            }

            /** Redirect the pointer encapsulated by the proxied element.
             *
             * @param s New pointer value.
             *
             * @return Reference to this ElementProxy.
             */
            ElementProxy &operator=(T *s)
            {
                m_ev->propagateAge(s);
                *m_it = s;
                return *this;
            }

            /**
             * @return The pointer encapsulated by the proxied
             *         element.
             */
            operator T * const() const
            {
                return *m_it;
            }

        private:
            RObjectVector<T, ST> *m_ev;
            typename std::vector<T *, Allocator<T *>>::iterator m_it;

            ElementProxy(RObjectVector<T, ST> *ev, R_xlen_t index)
                : m_ev(ev), m_it(m_ev->m_data.begin() + index)
            {
            }

            // Not implemented:
            ElementProxy(const ElementProxy &);

            friend class RObjectVector<T, ST>;
        };

        /** @brief Create a vector.
         *
         * Create a vector.
         *
         * @param sz Number of elements required.  Zero is
         *          permissible.
         *
         * @param init Initial value for the destination of each
         *          \a T* in the RObjectVector.
         */
        explicit RObjectVector(R_xlen_t sz, T *init = nullptr)
            : VectorBase(ST, sz), m_data(sz, init)
        {
            if (sz > R_xlen_t(R_SIZE_T_MAX / sizeof(RObject *)))
                Rf_error(_("cannot allocate vector of length %d"), sz);
#ifdef R_MEMORY_PROFILING
            MemoryBank::R_ReportAllocation(convert2VEC<T>(sz) * sizeof(VECREC));
#endif
        }

        /** @brief Copy constructor.
         *
         * @param pattern RObjectVector to be copied.  Beware that if
         *          any of the elements of \a pattern are unclonable,
         *          they will be shared between \a pattern and the
         *          created object.  This is necessarily prejudicial
         *          to the constness of the \a pattern parameter.
         *
         * @param deep Indicator whether to perform deep or shallow copy.
         */
        RObjectVector(const RObjectVector<T, ST> &pattern, bool deep);
        RObjectVector(const RObjectVector<T, ST> &pattern, bool deep, int dummy);

        /** @brief Element access.
         *
         * @param index Index of required element (counting from
         *          zero).  No bounds checking is applied.
         *
         * @return Proxy for the specified element, via which the
         *         element can be examined or modified.
         */
        ElementProxy operator[](R_xlen_t index)
        {
            return ElementProxy(this, index);
        }

        /** @brief Read-only element access.
         *
         * @param index Index of required element (counting from
         *          zero).  No bounds checking is applied.
         *
         * @return the specified element.
         */
        const T *operator[](R_xlen_t index) const
        {
            return m_data[index];
        }

        /**
         * @return pointer to the start of this object's data,
         * interpreted (riskily) as an array of \a T*.
         *
         * @deprecated This function puts the integrity of the write barrier
         * at the mercy of class clients.
         */
        T **dataPtr()
        {
            return &m_data[0];
        }

        virtual void *data() override
        {
            return &m_data[0];
        }

        virtual const void *data() const override
        {
            return &m_data[0];
        }

        /** @brief Name by which this type is known in R.
         *
         * @return the name by which this type is known in R.
         *
         * @note This function is declared but not defined as part of
         * the RObjectVector template.  It must be defined as a
         * specialization for each instantiation of the template for
         * which it or typeName() is used.
         */
        inline static const char *staticTypeName();

        // Virtual function of RObject:
        const char *typeName() const override;

        // Virtual function of GCNode:
        void visitChildren(const_visitor *v) const override;

    protected:
        /**
         * Declared protected to ensure that RObjectVector objects are
         * allocated only using 'new'.
         */
        ~RObjectVector() {}

    private:
        std::vector<T *, Allocator<T *>> m_data;

        // Not implemented.  Declared to prevent
        // compiler-generated versions:
        RObjectVector &operator=(const RObjectVector &);

        friend class ElementProxy;
    };

    template <typename T, SEXPTYPE ST>
    RObjectVector<T, ST>::RObjectVector(const RObjectVector<T, ST> &pattern, bool deep)
        : VectorBase(pattern, deep), m_data(pattern.size())
    {
        R_xlen_t sz = size();
#ifdef R_MEMORY_PROFILING
        MemoryBank::R_ReportAllocation(convert2VEC<T>(sz) * sizeof(VECREC));
#endif
        for (R_xlen_t i = 0; i < sz; ++i)
        {
            m_data[i] = dup_child2(pattern.m_data[i], deep);
            if (m_data[i])
                m_data[i]->incrementRefCount();
        }
    }

    template <typename T, SEXPTYPE ST>
    RObjectVector<T, ST>::RObjectVector(const RObjectVector<T, ST> &pattern, bool deep, int dummy)
        : VectorBase(pattern, deep), m_data(pattern.size())
    {
        R_xlen_t sz = size();
#ifdef R_MEMORY_PROFILING
        MemoryBank::R_ReportAllocation(convert2VEC<T>(sz) * sizeof(VECREC));
#endif
        std::copy(pattern.m_data.begin(), pattern.m_data.end(), m_data.begin());
    }

    template <typename T, SEXPTYPE ST>
    const char *RObjectVector<T, ST>::typeName() const
    {
        return RObjectVector<T, ST>::staticTypeName();
    }

    template <typename T, SEXPTYPE ST>
    void RObjectVector<T, ST>::visitChildren(const_visitor *v) const
    {
        RObject::visitChildren(v);
        for (R_xlen_t i = 0; i < size(); ++i)
        {
            const T *ptr = (*this)[i];
            if (ptr)
                ptr->conductVisitor(v);
        }
    }
} // namespace CXXR

#endif // ROBJECTVECTOR_HPP
