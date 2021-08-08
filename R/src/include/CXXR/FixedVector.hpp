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
     * Having said that, the template \e does implement decreaseSizeInPlace(),
     * primarily to service CR code's occasional use of SETLENGTH().
     *
     * CXXR implements all of CR's built-in vector types using this
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

        /** @brief Create a vector from a range.
         *
         * @tparam An iterator type, at least a forward iterator.
         *
         * @param from Iterator designating the start of the range
         *          from which the FixedVector is to be constructed.
         *
         * @param to Iterator designating 'one past the end' of the
         *          range from which the FixedVector is to be
         *          constructed.
         */
        template <typename FwdIter>
        static FixedVector *create(FwdIter from, FwdIter to)
        {
            FixedVector *result = create(std::distance(from, to));
            iterator out = result->begin();
            for (FwdIter in = from; in != to; ++in, ++out)
            {
                *out = ElementTraits::duplicate_element(*in);
            }
            return result;
        }

        /** @brief Create a vector from an initializer list.
         *
         * @param An initializer list containing the values to store in the
         *          FixedVector.
         */
        static FixedVector *create(std::initializer_list<T> items)
        {
            return create(items.begin(), items.end());
        }

        /** @brief Create a vector containing a single value.
         *
         * @param value The value to store in the vector.
         */
        template <typename U>
        static FixedVector *createScalar(const U &value)
        {
            FixedVector *result = create(1);
            (*result)[0] = value;
            return result;
        }

        /** @brief Element access.
         *
         * @param index Index of required element (counting from
         *          zero).  No bounds checking is applied.
         *
         * @return Reference to the specified element.
         */
        T &operator[](size_type index)
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
        const T &operator[](size_type index) const
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

        // Virtual functions of VectorBase:
        void decreaseSizeInPlace(size_type new_size) override;

        // Virtual functions of RObject:
        FixedVector<T, ST> *clone(Duplicate deep) const override;
        const char *typeName() const override;

        // Virtual function of GCNode:
        void visitReferents(const_visitor *v) const override;

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

        /** @brief Create a vector, leaving its contents
         *         uninitialized (for POD types) or default
         *         constructed.
         *
         * @param sz Number of elements required.  Zero is
         *          permissible.
         *
         * @param allocator Custom allocator.
         */
        FixedVector(size_type sz, R_allocator_t *allocator = nullptr)
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
        FixedVector(size_type sz, const T &initializer, R_allocator_t *allocator = nullptr)
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
        FixedVector(const FixedVector<T, ST> &pattern, Duplicate deep);

        FixedVector &operator=(const FixedVector &) = delete;

        // If there is more than one element, this function is used to
        // allocate the required memory block from CXXR::MemoryBank :
        void allocData(size_type sz, bool initialize = false, R_allocator_t *allocator = nullptr);

        static void constructElements(iterator from, iterator to);
        static void constructElementsIfNeeded(iterator from, iterator to)
        {
            // This is essential for e.g. GCEdges, otherwise they
            // may contain junk pointers.
            if (ElementTraits::MustConstruct<T>::value) // compile-time constant
                constructElements(from, to);
        }
        void constructElementsIfNeeded()
        {
            constructElementsIfNeeded(begin(), end());
        }

        void destructElementsIfNeeded(iterator from, iterator to)
        {
            if (ElementTraits::MustDestruct<T>::value) // compile-time constant
                destructElements(from, to);
        }
        void destructElementsIfNeeded()
        {
            destructElementsIfNeeded(begin(), end());
        }
        void destructElements(iterator from, iterator to);

        // Helper functions for visitReferents():
        void visitElements(const_visitor *v, std::true_type) const;
        void visitElements(const_visitor *v, std::false_type) const {}
    };

    // VectorTypeFor<T>::type is the type of vector that can hold elements of
    // type T.
    template <class T>
    struct VectorTypeFor
    {
    };

    template <typename T, SEXPTYPE ST>
    FixedVector<T, ST>::FixedVector(const FixedVector<T, ST> &pattern, Duplicate deep)
        : VectorBase(pattern, deep), m_data(&m_singleton),
          m_singleton(pattern.m_singleton)
    {
#if CXXR_TRUE
        size_type sz = size();
#ifdef R_MEMORY_PROFILING
        MemoryBank::R_ReportAllocation(convert2VEC<T>(sz) * sizeof(VECREC));
#endif
        if (sz > 1)
        {
            allocData(sz);
            memcpy(m_data, pattern.m_data, sizeof(T) * sz);
        }
#else
        constructElementsIfNeeded();

        const_iterator to = pattern.end();
        iterator out = begin();
        for (const_iterator in = pattern.begin(); in != to; ++in)
        {
            *out = ElementTraits::duplicate_element(*in);
            ++out;
        }
#endif
    }

    template <typename T, SEXPTYPE ST>
    void FixedVector<T, ST>::allocData(size_type sz, bool initialize, R_allocator_t *allocator)
    {
        size_type bytes = sz * sizeof(T);
        // Check for integer overflow:
        if (size_type(bytes / sizeof(T)) != sz)
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
            for (size_type i = 0; i < sz; ++i)
                m_data[i] = m_singleton;
        }
    }

    template <typename T, SEXPTYPE ST>
    FixedVector<T, ST> *FixedVector<T, ST>::create(size_type sz, R_allocator_t *allocator)
    {
        return GCNode::expose(new FixedVector(sz, allocator));
    }

    template <typename T, SEXPTYPE ST>
    FixedVector<T, ST> *FixedVector<T, ST>::clone(Duplicate deep) const
    {
        return new FixedVector<T, ST>(*this, deep);
    }

    template <typename T, SEXPTYPE ST>
    void FixedVector<T, ST>::constructElements(iterator from, iterator to)
    {
        for (iterator p = from; p != to; ++p)
            new (p) T;
    }

    template <typename T, SEXPTYPE ST>
    void FixedVector<T, ST>::destructElements(iterator from, iterator to)
    {
        for (iterator p = from; p != to; ++p)
            p->~T();
    }

    template <typename T, SEXPTYPE ST>
    void FixedVector<T, ST>::decreaseSizeInPlace(size_type new_size)
    {
        if (new_size > size())
        {
            Rf_error("Increasing vector length in place not allowed.");
        }
        size_t bytes = (size() - new_size) * sizeof(T);
        MemoryBank::adjustBytesAllocated(-bytes);

        destructElementsIfNeeded(begin() + new_size, end());
        adjustSize(new_size);
    }

    template <typename T, SEXPTYPE ST>
    const char *FixedVector<T, ST>::typeName() const
    {
        return FixedVector<T, ST>::staticTypeName();
    }

    template <typename T, SEXPTYPE ST>
    void FixedVector<T, ST>::visitElements(
        const_visitor *v, std::true_type) const
    {
        std::for_each(begin(), end(), [=](GCNode *n)
                      {
                          if (n)
                              (*v)(n);
                      });
    }

    template <typename T, SEXPTYPE ST>
    void FixedVector<T, ST>::visitReferents(const_visitor *v) const
    {
        visitElements(v, typename ElementTraits::IsGCEdge<T>());
        VectorBase::visitReferents(v);
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
