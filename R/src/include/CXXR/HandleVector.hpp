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

/** @file HandleVector.hpp
 *
 * @brief Templated class CXXR::HandleVector.
 */

#ifndef HANDLEVECTOR_HPP
#define HANDLEVECTOR_HPP

#include <algorithm>

#include <CXXR/Allocator.hpp>
#include <CXXR/Allocator.hpp>
#include <CXXR/GCStackRoot.hpp>
#include <CXXR/VectorBase.hpp>
#include <Localization.h>
#include <R_ext/Error.h>

namespace CXXR
{
    /** @brief Vector of RObject::Handle smart pointers.
     *
     * This is a templated class to represent a vector whose elements
     * are are smart pointers of type \c RObject::Handle<T>.  As
     * explained in the documentation for \c RObject::Handle, copying
     * the vector will copy the objects pointed to, provided that they
     * are clonable.
     *
     * @param T This should be RObject or a type (publicly) derived
     * from RObject.  The vector elements will be of type \c Handle<T>.
     *
     * @param ST The required ::SEXPTYPE of the vector.
     */
    template <typename T, SEXPTYPE ST>
    class HandleVector : public VectorBase
    {
    public:
        /** @brief Proxy object for an element of an HandleVector<T, ST>.
         *
         * Objects of this class are used to allow the elements of an
         * HandleVector<T, ST> to be examined and modified using
         * the same syntax as would be used for accessing an array of
         * \a T*, whilst nevertheless enforcing the write barrier.
         * See Item 30 of Scott Meyers's 'More Effective C++' for a
         * general discussion of proxy objects, but see the
         * <a href="http://www.aristeia.com/BookErrata/mec++-errata_frames.html">errata</a>.
         * (It may look complicated, but an optimising compiler should
         * be able to distil an invocation of HandleVector<T,
         * ST>::operator[] into very few instructions.)
         */
        class ElementProxy
        {
        public:
            /** @brief Copy the value of the proxied element from another
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

            /** @brief Redirect the pointer encapsulated by the proxied element.
             *
             * @param s New pointer value.
             *
             * @return Reference to this ElementProxy.
             */
            ElementProxy &operator=(T *s)
            {
                m_ev->propagateAge(s);
                (*m_it).retarget(s);
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
            HandleVector<T, ST> *m_ev;
            typename std::vector<Handle<T>, Allocator<Handle<T>>>::iterator m_it;

            ElementProxy(HandleVector<T, ST> *ev, R_xlen_t index)
                : m_ev(ev), m_it(m_ev->m_data.begin() + index)
            {
            }

            // Not implemented:
            ElementProxy(const ElementProxy &);

            friend class HandleVector<T, ST>;
        };

        /** @brief Create a vector.
         *
         * Create a vector.
         *
         * @param sz Number of elements required.  Zero is
         *          permissible.
         *
         * @param init Initial value for the destination of each
         *          \a T* in the HandleVector.
         */
        explicit HandleVector(R_xlen_t sz, Handle<T> init = Handle<T>())
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
         * @param pattern HandleVector to be copied.  Beware that if
         *          any of the elements of \a pattern are unclonable,
         *          they will be shared between \a pattern and the
         *          created object.  This is necessarily prejudicial
         *          to the constness of the \a pattern parameter.
         *
         * @param deep Indicator whether to perform deep or shallow copy.
         */
        HandleVector(const HandleVector<T, ST> &pattern, bool deep)
            : VectorBase(pattern, deep), m_data(pattern.size())
        {
            R_xlen_t sz = size();
#ifdef R_MEMORY_PROFILING
            MemoryBank::R_ReportAllocation(convert2VEC<T>(sz) * sizeof(VECREC));
#endif
            for (R_xlen_t i = 0; i < sz; ++i)
            {
                (m_data[i]).clone(pattern.m_data[i], deep);
                if (m_data[i])
                    m_data[i]->incrementRefCount();
            }
        }

        /** @brief Copy constructor.
         *
         * @param pattern HandleVector to be copied.  Beware that if
         *          any of the elements of \a pattern are unclonable,
         *          they will be shared between \a pattern and the
         *          created object.  This is necessarily prejudicial
         *          to the constness of the \a pattern parameter.
         *
         * @param deep Indicator whether to perform deep or shallow copy.
         *
         * @param dummy This parameter is used simply to provide the
         *          constructor with a distinct signature.  Its value
         *          is ignored.
         */
        HandleVector(const HandleVector<T, ST> &pattern, bool deep, int dummy)
            : VectorBase(pattern, deep), m_data(pattern.size())
        {
            R_xlen_t sz = size();
#ifdef R_MEMORY_PROFILING
            MemoryBank::R_ReportAllocation(convert2VEC<T>(sz) * sizeof(VECREC));
#endif
            std::copy(pattern.m_data.begin(), pattern.m_data.end(), m_data.begin());
        }

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
            return reinterpret_cast<T **>(&m_data[0]);
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
         * the HandleVector template.  It must be defined as a
         * specialization for each instantiation of the template for
         * which it or typeName() is used.
         */
        inline static const char *staticTypeName();

        // Virtual function of RObject:
        const char *typeName() const override;

        // Virtual function of GCNode:
        void visitReferents(const_visitor *v) const override;

    protected:
        /**
         * Declared protected to ensure that HandleVector objects are
         * allocated only using 'new'.
         */
        ~HandleVector() {}

    private:
        std::vector<Handle<T>, Allocator<Handle<T>>> m_data;

        // Not implemented.  Declared to prevent
        // compiler-generated versions:
        HandleVector &operator=(const HandleVector &);

        friend class ElementProxy;
    };

    template <typename T, SEXPTYPE ST>
    const char *HandleVector<T, ST>::typeName() const
    {
        return HandleVector<T, ST>::staticTypeName();
    }

    template <typename T, SEXPTYPE ST>
    void HandleVector<T, ST>::visitReferents(const_visitor *v) const
    {
        VectorBase::visitReferents(v);
        for (R_xlen_t i = 0; i < size(); ++i)
        {
            const T *ptr = (*this)[i];
            if (ptr)
                ptr->conductVisitor(v);
        }
    }
} // namespace CXXR

#endif // HANDLEVECTOR_HPP
