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

/** @file FixedVector.hpp
 *
 * @brief Class template CXXR::FixedVector.
 */

#ifndef FIXEDVECTOR_HPP
#define FIXEDVECTOR_HPP

#include <CXXR/VectorBase.hpp>
#include <CXXR/MemoryBank.hpp>
#include <Localization.h>

namespace CXXR
{
    /** @brief R data vector primarily intended for fixed-size use.
     *
     * This is a general-purpose class template to represent an R data
     * vector, and is intended particularly for the case where the
     * size of the vector is fixed when it is constructed.
     *
     * R implements all of CR's built-in vector types using this
     * template.
     *
     * @tparam T The type of the elements of the vector.
     *
     * @tparam ST The required ::SEXPTYPE of the vector.
     */
    template <typename T, SEXPTYPE ST>
    class FixedVector : public VectorBase
    {

    public:
        /** @brief Create a vector, leaving its contents
         *         uninitialized. 
         * @param sz Number of elements required.  Zero is
         *          permissible.
         */
        FixedVector(R_xlen_t sz, R_allocator_t *allocator = nullptr)
            : VectorBase(ST, sz), m_data(&m_singleton), m_allocator(allocator)
        {
            if (sz > 1)
                allocData(sz, allocator);
#if VALGRIND_LEVEL >= 1
            else
                VALGRIND_MAKE_MEM_UNDEFINED(&m_singleton, sizeof(T));
#endif
        }

        /** @brief Create a vector, and fill with a specified initial
         *         value. 
         * @param sz Number of elements required.  Zero is
         *          permissible.
         * @param initializer Initial value to be assigned to every
         *          element.
         */
        FixedVector(R_xlen_t sz, const T &initializer, R_allocator_t *allocator = nullptr)
            : VectorBase(ST, sz), m_data(&m_singleton),
              m_singleton(initializer), m_allocator(allocator)
        {
            if (sz > 1)
                allocData(sz, true, allocator);
        }

        /** @brief Element access.
         * @param index Index of required element (counting from
         *          zero).  No bounds checking is applied.
         *
         * @return Reference to the specified element.
         */
        T &operator[](R_xlen_t index)
        {
            return m_data[index];
        }

        /** @brief Read-only element access.
         *
         * @param index Index of required element (counting from
         *          zero).  No bounds checking is applied.
         *
         * @return \c const reference to the specified element.
         */
        const T &operator[](R_xlen_t index) const
        {
            return m_data[index];
        }

        virtual void *data() override
        {
            return m_data;
        }

        virtual const void *data() const override
        {
            return m_data;
        }

        /** @brief Name by which this type is known in R.
         *
         * @return the name by which this type is known in R.
         *
         * @note This function is declared but not defined as part of
         * the FixedVector template.  It must be defined as a
         * specialization for each instantiation of the template for
         * which it or typeName() is used.
         */
        static const char *staticTypeName();

        // Virtual functions of RObject:
        const char *typeName() const override;

    protected:
        /**
         * Declared protected to ensure that FixedVector objects are
         * allocated only using 'new'.
         */
        ~FixedVector()
        {
            if (m_data != &m_singleton)
                MemoryBank::deallocate(m_data, m_databytes, m_allocator);
        }

    private:
        T *m_data;            // pointer to the vector's data block.
        R_xlen_t m_databytes; // used only if > 1 elements

        // If there is only one element, it is stored here, internally
        // to the FixedVector object, rather than via a separate
        // allocation from CXXR::MemoryBank.  We put this last, so that it
        // will be adjacent to any trailing redzone.
        T m_singleton;
        bool m_allocator; // indicator whether external allocator was used

        // Not implemented yet.  Declared to prevent
        // compiler-generated versions:
        FixedVector(const FixedVector &);
        FixedVector &operator=(const FixedVector &);

        // If there is more than one element, this function is used to
        // allocate the required memory block from CXXR::MemoryBank :
        void allocData(R_xlen_t sz, bool initialize = false, R_allocator_t *allocator = nullptr);
    };

    template <typename T, SEXPTYPE ST>
    void FixedVector<T, ST>::allocData(R_xlen_t sz, bool initialize, R_allocator_t *allocator)
    {
        m_databytes = sz * sizeof(T);
        // Check for integer overflow:
        if (R_xlen_t(m_databytes / sizeof(T)) != sz)
            Rf_error(_("Request to create impossibly large vector."));
        GCRoot<> thisroot(this);
        try
        {
            m_data = reinterpret_cast<T *>(MemoryBank::allocate(m_databytes, allocator));
        }
        catch (std::bad_alloc &e)
        {
            m_data = nullptr;
            tooBig(m_databytes);
            return;
        }
        if (initialize)
        {
            for (R_xlen_t i = 0; i < sz; ++i)
                m_data[i] = m_singleton;
        }
#if VALGRIND_LEVEL == 1
        // For VALGRIND_LEVEL > 1 this will already have been done:
        else
            VALGRIND_MAKE_MEM_UNDEFINED(m_data, m_databytes);
#endif
    }

    template <typename T, SEXPTYPE ST>
    const char *FixedVector<T, ST>::typeName() const
    {
        return FixedVector<T, ST>::staticTypeName();
    }

} // namespace CXXR

#endif // FIXEDVECTOR_HPP
