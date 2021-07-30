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
#include <Rinternals.h>
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
        typedef T value_type;
        typedef T *iterator;
        typedef const T *const_iterator;

        /** @brief Create a vector, leaving its contents
         *         uninitialized (for POD types) or default
         *         constructed.
         *
         * @param sz Number of elements required.  Zero is
         *          permissible.
         *
         * @param allocator Custom allocator.
         */
        static FixedVector *create(size_type sz, R_allocator_t *allocator = nullptr);

        /** @brief Create a vector, leaving its contents
         *         uninitialized.
         *
         * @param sz Number of elements required.  Zero is
         *          permissible.
         *
         * @param allocator Custom allocator.
         */
        FixedVector(R_xlen_t sz, R_allocator_t *allocator = nullptr)
            : VectorBase(ST, sz), m_data(&m_singleton), m_allocator(allocator)
        {
#ifdef R_MEMORY_PROFILING
            MemoryBank::R_ReportAllocation(convert2VEC<T>(sz) * sizeof(VECREC));
#endif
            if (sz > 1)
                allocData(sz, allocator);
        }

        /** @brief Create a vector, and fill with a specified initial
         *         value.
         *
         * @param sz Number of elements required.  Zero is
         *          permissible.
         *
         * @param initializer Initial value to be assigned to every
         *          element.
         *
         * @param allocator Custom allocator.
         */
        FixedVector(R_xlen_t sz, const T &initializer, R_allocator_t *allocator = nullptr)
            : VectorBase(ST, sz), m_data(&m_singleton),
              m_singleton(initializer), m_allocator(allocator)
        {
#ifdef R_MEMORY_PROFILING
            MemoryBank::R_ReportAllocation(convert2VEC<T>(sz) * sizeof(VECREC));
#endif
            if (sz > 1)
                allocData(sz, true, allocator);
        }

        /** @brief Copy constructor.
         *
         * @param pattern FixedVector to be copied.
         *
         * @param deep Indicator whether to perform deep or shallow copy.
         */
        FixedVector(const FixedVector<T, ST> &pattern, bool deep);

        /** @brief Element access.
         *
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

        /** @brief Iterator designating first element.
         *
         * @return An iterator designating the first element of the
         * vector.  Returns end() if the vector is empty.
         */
        iterator begin() { return m_data; }

        /** @brief Const iterator designating first element.
         *
         * @return A const_iterator designating the first element of
         * the vector.  Returns end() if the vector is empty.
         */
        const_iterator begin() const { return m_data; }

        /** @brief One-past-the-end iterator.
         *
         * @return An iterator designating a position 'one past the
         * end' of the vector.
         */
        iterator end() { return begin() + size(); }

        /** @brief One-past-the-end const_iterator.
         *
         * @return A const_iterator designating a position 'one past
         * the end' of the vector.
         */
        const_iterator end() const { return begin() + size(); }

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
        FixedVector<T, ST> *clone(bool deep) const override;
        const char *typeName() const override;

    protected:
        /**
         * Declared protected to ensure that FixedVector objects are
         * allocated only using 'new'.
         */
        ~FixedVector()
        {
            if (m_data != &m_singleton)
                MemoryBank::deallocate(m_data, size() * sizeof(T), m_allocator);
            // GCNode::~GCNode doesn't know about the string storage space in
            // this object, so account for it here.
            // size_t bytes = (size()) * sizeof(T);
            // if (bytes != 0)
            // {
            //     MemoryBank::adjustFreedSize(
            //         sizeof(FixedVector), sizeof(FixedVector) + bytes);
            // }
        }

    private:
        T *m_data; // pointer to the vector's data block.

        // If there is only one element, it is stored here, internally
        // to the FixedVector object, rather than via a separate
        // allocation from CXXR::MemoryBank.  We put this last, so that it
        // will be adjacent to any trailing redzone.
        T m_singleton;
        bool m_allocator; // indicator whether external allocator was used

        // Not implemented yet.  Declared to prevent
        // compiler-generated versions:
        FixedVector &operator=(const FixedVector &);

        // If there is more than one element, this function is used to
        // allocate the required memory block from CXXR::MemoryBank :
        void allocData(R_xlen_t sz, bool initialize = false, R_allocator_t *allocator = nullptr);
    };

    template <typename T, SEXPTYPE ST>
    FixedVector<T, ST>::FixedVector(const FixedVector<T, ST> &pattern, bool deep)
        : VectorBase(pattern, deep), m_data(&m_singleton),
          m_singleton(pattern.m_singleton)
    {
        R_xlen_t sz = size();
#ifdef R_MEMORY_PROFILING
        MemoryBank::R_ReportAllocation(convert2VEC<T>(sz) * sizeof(VECREC));
#endif
        if (sz > 1)
        {
            allocData(sz);
            memcpy(m_data, pattern.m_data, sizeof(T) * sz);
        }
    }

    template <typename T, SEXPTYPE ST>
    void FixedVector<T, ST>::allocData(R_xlen_t sz, bool initialize, R_allocator_t *allocator)
    {
        R_xlen_t bytes = sz * sizeof(T);
        // Check for integer overflow:
        if (R_xlen_t(bytes / sizeof(T)) != sz)
            Rf_error(_("Request to create impossibly large vector."));
        GCStackRoot<> thisroot(this);
        try
        {
            m_data = static_cast<T *>(MemoryBank::allocate(bytes, true, allocator));
        }
        catch (std::bad_alloc &e)
        {
            m_data = nullptr;
            expose();
            tooBig(bytes);
            return;
        }
        if (initialize)
        {
            for (R_xlen_t i = 0; i < sz; ++i)
                m_data[i] = m_singleton;
        }
    }

    template <typename T, SEXPTYPE ST>
    FixedVector<T, ST> *FixedVector<T, ST>::create(size_type sz, R_allocator_t *allocator)
    {
        return GCNode::expose(new FixedVector(sz, allocator));
    }

    template <typename T, SEXPTYPE ST>
    FixedVector<T, ST> *FixedVector<T, ST>::clone(bool deep) const
    {
        // return GCNode::expose(new FixedVector<T, ST>(*this, deep));
        return new FixedVector<T, ST>(*this, deep);
    }

    template <typename T, SEXPTYPE ST>
    const char *FixedVector<T, ST>::typeName() const
    {
        return FixedVector<T, ST>::staticTypeName();
    }

} // namespace CXXR

extern "C"
{
    /** @brief Is an object of numeric type.
     *
     * @param s Pointer to an CXXR::RObject.
     *
     * @return \c true if CXXR::RObject is integer, logical or real.
     *
     * @todo the LGLSXP case should be excluded here
     *       (really? in many places we affirm they are treated like INTs)
     */
    Rboolean Rf_isNumeric(SEXP s);

    /** @brief Is an object "Numeric" or complex?
     *
     * @param s Pointer to an CXXR::RObject.
     *
     * @return \c true if CXXR::RObject is integer, logical, real or complex.
     */
    Rboolean Rf_isNumber(SEXP s);
} // extern "C"

#if (defined(R_NO_REMAP) && defined(COMPILING_IVORY)) && defined(__cplusplus)
const auto isNumeric = Rf_isNumeric;
const auto isNumber = Rf_isNumber;
#endif

#ifdef STRICT_TYPECHECK
#define CHECK_BOUNDS_ELT(x, i)                      \
    do                                              \
    {                                               \
        if (i < 0 || i > XLENGTH(x))                \
            Rf_error(_("subscript out of bounds")); \
    } while (0)

#else
#define CHECK_BOUNDS_ELT(x, i) \
    do                         \
    {                          \
    } while (0)
#endif

#endif // FIXEDVECTOR_HPP
