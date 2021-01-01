/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999--2020  The R Core Team.
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
 *
 *  Rho is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the Rho website.
 *
 *  This header file is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
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
 *  https://www.R-project.org/Licenses/
 */

/** @file ConsCell.hpp
 * @brief Class CXXR::ConsCell and associated C interface.
 *
 * To facilitate inlining of various ConsCell member functions, this
 * file also includes the definition of class CXXR::PairList.
 *
 * This file includes C functions for examining and setting the CAR
 * and TAG of a CXXR::ConsCell; functions for examining and setting
 * the CDR, and other operations accessing the tail of the list, are
 * to be found in PairList.hpp.
 */

#ifndef CONSCELL_HPP
#define CONSCELL_HPP

#include <stdexcept>
#include <CXXR/RObject.hpp>
#include <CXXR/GCManager.hpp>
#include <CXXR/GCRoot.hpp>
#include <CXXR/SEXP_downcast.hpp>

#if (SIZEOF_SIZE_T < SIZEOF_DOUBLE)
#define BOXED_BINDING_CELLS 1
#else
#define BOXED_BINDING_CELLS 0
#endif
#if BOXED_BINDING_CELLS
/* Use allocated scalars to hold immediate binding values. A little
   less efficient but does not change memory layout or use. These
   allocated scalars must not escape their bindings. */
#define BNDCELL_DVAL(v) SCALAR_DVAL(CAR0(v))
#define BNDCELL_IVAL(v) SCALAR_IVAL(CAR0(v))
#define BNDCELL_LVAL(v) SCALAR_LVAL(CAR0(v))

#define SET_BNDCELL_DVAL(cell, dval) SET_SCALAR_DVAL(CAR0(cell), dval)
#define SET_BNDCELL_IVAL(cell, ival) SET_SCALAR_IVAL(CAR0(cell), ival)
#define SET_BNDCELL_LVAL(cell, lval) SET_SCALAR_LVAL(CAR0(cell), lval)

#define INIT_BNDCELL(cell, type)             \
    do                                       \
    {                                        \
        RObject *val = allocVector(type, 1); \
        SETCAR(cell, val);                   \
        INCREMENT_NAMED(val);                \
        SET_BNDCELL_TAG(cell, type);         \
        SET_MISSING(cell, 0);                \
    } while (0)
#else
/* Use a union in the CAR field to represent an RObject* or an immediate
   value.  More efficient, but changes the memory layout on 32 bit
   platforms since the size of the union is larger than the size of a
   pointer. The layout should not change on 64 bit platforms. */
union R_bndval_t
{
    CXXR::RObject *sxpval;
    double dval;
    int ival;
};

#define BNDCELL_DVAL(v) (CXXR::ConsCell::bndcell_dval(v))
#define BNDCELL_IVAL(v) (CXXR::ConsCell::bndcell_ival(v))
#define BNDCELL_LVAL(v) (CXXR::ConsCell::bndcell_lval(v))

#define SET_BNDCELL_DVAL(cell, dval_) (CXXR::ConsCell::set_bndcell_dval(cell, dval_))
#define SET_BNDCELL_IVAL(cell, ival_) (CXXR::ConsCell::set_bndcell_ival(cell, ival_))
#define SET_BNDCELL_LVAL(cell, lval_) (CXXR::ConsCell::set_bndcell_lval(cell, lval_))

#define INIT_BNDCELL(cell, type)      \
    do                                \
    {                                 \
        if (BNDCELL_TAG(cell) == 0)   \
            SETCAR(cell, R_NilValue); \
        SET_BNDCELL_TAG(cell, type);  \
        SET_MISSING(cell, 0);         \
    } while (0)
#endif

namespace CXXR
{
    class PairList;

    /** @brief Element of a singly linked list.
     *
     * Element of a LISP-like singly-linked list, containing pointers
     * to a 'car' object (this is LISP terminology, and has nothing to
     * do with automobiles) and to a 'tag' object, as well as a
     * pointer to the next element of the list, which must be of the
     * derived type PairList.  (Any of these pointers may be null.)
     *
     * When the object is copied, the copy thus created will contain
     * copies of the 'car' and of subsequent elements of the list.
     * However, the tag is not copied: the copy object will simply
     * contain a pointer to the tag of the original object.  Despite
     * this, the tag is considered to be part of the object.
     *
     * @note This class is used as a base class to implement CR's
     * LISTSXP, LANGSXP, DOTSXP and (for the time being) BCODESXP.
     * Because what these SEXPTYPEs have in common is implementation
     * rather than meaning in the application domain, canons of
     * object-oriented design would argue against their publicly
     * inheriting from a common base class.  Without doing this,
     * however, it would have been difficult efficiently to implement
     * functions such as CAR(), which are ubiquitous in the CR code.
     *
     * @note The semantics of this class are somewhat inconsistent.
     * When a ConsCell is copied, the copy constructor tries to copy
     * the 'car', implying that the car is considered part of the
     * object.  But the const member function car() passes back a
     * non-const pointer to the car.  See the discussion in the
     * documentation of class RObject regarding the handling of const
     * pointers.
     */
    class ConsCell : public RObject
    {
    public:
        /**
         * @return a const pointer to the 'car' of this ConsCell
         * element.
         */
        const RObject *car() const
        {
            return m_car;
        }

        /**
         * @return a pointer to the 'car' of this ConsCell.
         */
        RObject *car()
        {
            return m_car;
        }

        /** @brief Convert a ConsCell to a (possibly) different
         * ConsCell type.
         *
         * @tparam T A (non-abstract) class derived from ConsCell.
         *
         * @param cc Pointer to a ConsCell (possibly null).  The
         *          effect of the method on \a cc is undefined;
         *          consequently \a cc should not be used subsequently to
         *          the method call.
         *
         * @return Pointer to the converted object, or a null pointer
         * if \a cc is null.  If \a cc is already of the desired type,
         * the method simply returns \a cc.
         */
        template <class T>
        static T *convert(ConsCell *cc)
        {
            if (!cc)
                return nullptr;
            if (T *ccc = dynamic_cast<T *>(cc))
                return ccc;
            GCRoot<T> ans(new T(cc->car(), cc->tail(), cc->tag()));
            SET_ATTRIB(ans, ATTRIB(cc));
            return ans;
        }

        /** @brief Set the 'car' value.
         *
         * @param cr Pointer to the new car object (or a null
         *           pointer).
         */
        void setCar(RObject *cr)
        {
            m_car = cr;
            devolveAge(m_car);
        }

        /** @brief Set the 'tag' value.
         *
         * @param tg Pointer to the new tag object (or a null
         *           pointer).
         */
        void setTag(RObject *tg)
        {
            m_tag = tg;
            devolveAge(m_tag);
        }

        /** @brief Set the 'tail' value.
         *
         * @param tl Pointer to the new tail list (or a null
         *           pointer).
         */
        void setTail(PairList *tl);
        // Implemented inline in CXXR/PairList.h

        /** @brief The name by which this type is known in R.
         *
         * @return the name by which this type is known in R.
         */
        static const char *staticTypeName()
        {
            return "(pairlist type)";
        }

        /**
         * @return a const pointer to the 'tag' of this ConsCell
         * element.
         */
        const RObject *tag() const
        {
            return m_tag;
        }

        /**
         * @return a pointer to the 'tag' of this ConsCell.
         */
        RObject *tag()
        {
            return m_tag;
        }

        /**
         * @return a const pointer to the 'tail' of this ConsCell
         * element.
         */
        const PairList *tail() const
        {
            return m_tail;
        }

        /**
         * @return a pointer to the 'tail' of this ConsCell.
         */
        PairList *tail()
        {
            return m_tail;
        }

        // Virtual function of GCNode:
        void visitChildren(const_visitor *v) const;

        static RObject *tag(RObject *x);
        static void set_tag(RObject *x, RObject *v);
        static RObject *car0(RObject *x);
        static void set_car0(RObject *x, RObject *v);
        static RObject *cdr(RObject *x);
        static void set_cdr(RObject *x, RObject *v);
        static double bndcell_dval(RObject *x);
        static int bndcell_ival(RObject *x);
        static int bndcell_lval(RObject *x);
        static void set_bndcell_dval(RObject *x, double v);
        static void set_bndcell_ival(RObject *x, int v);
        static void set_bndcell_lval(RObject *x, int v);

    protected:
        /**
         * @param st The required ::SEXPTYPE of the ConsCell.  Must
         *           be one of LISTSXP, LANGSXP, DOTSXP or BCODESXP (not
         *           normally checked).
         *
         * @param cr Pointer to the 'car' of the element to be
         *           constructed.
         *
         * @param tl Pointer to the 'tail' (LISP cdr) of the element
         *           to be constructed.
         *
         * @param tg Pointer to the 'tag' of the element to be constructed.
         */
        explicit ConsCell(SEXPTYPE st,
                          RObject *cr = nullptr, PairList *tl = nullptr, RObject *tg = nullptr)
            : RObject(st), m_car(cr), m_tail(tl), m_tag(tg)
        {
            checkST(st);
        }

        /** @brief Create a chain of ConsCell objects.
         *
         * This constructor creates a chain of ConsCell objects with a specified number
         * of elements.  On creation, each element has null 'car' and
         * 'tag'.
         *
         * @param st The required ::SEXPTYPE of the PairList.  Must
         *           be one of LISTSXP, LANGSXP, DOTSXP or BCODESXP
         *           (not checked).
         * @param sz Number of elements required in the list.  Must be
         *           strictly positive; the constructor throws
         *           std::out_of_range if \a sz is zero.
         */
        ConsCell(SEXPTYPE st, size_t sz);

        /**
         * Declared protected to ensure that ConsCell objects are
         * allocated only using 'new'.
         */
        ~ConsCell() {}

        /** @brief Set the 'tail' value during construction.
         *
         * This method should only be used during the construction of
         * an object of a class derived from ConsCell, because it
         * skips write-barrier checks.
         *
         * @param tl Pointer to the new tail list (or a null
         *           pointer).
         */
        void constructTail(PairList *tl)
        {
            m_tail = tl;
        }

    private:
        RObject *m_car;
        PairList *m_tail;
        RObject *m_tag;

        // Not implemented yet.  Declared to prevent
        // compiler-generated versions:
        ConsCell(const ConsCell &);
        ConsCell &operator=(const ConsCell &);

        // Check that st is a legal SEXPTYPE for a ConsCell:
        static void checkST(SEXPTYPE st);
    };

    /** @brief (For debugging.)
     *
     * @note The name and interface of this function may well change.
     */
    void ccdump(std::ostream &os, const ConsCell *cc, size_t margin = 0);
} // namespace CXXR

extern "C"
{
    /** @brief Get tag of CXXR::ConsCell.
     *
     * @param e Pointer to a CXXR::ConsCell (checked), or a null pointer.
     * @return Pointer to the tag of the list element, or 0 if \a e is
     * a null pointer.
     */
    SEXP TAG(SEXP e);

    /**
     * @brief Set the tag of a CXXR::ConsCell.
     *
     * @param x Pointer to a CXXR::ConsCell (checked).
     * @param y Pointer a CXXR::RObject representing the new tag of
     *          the CXXR::ConsCell.
     */
    void SET_TAG(SEXP x, SEXP y);

    /** @brief Create an object of a type derived from CXXR::ConsCell.
     *
     * The object is created with null car, tag and tail pointers.
     *
     * @param t The ::SEXPTYPE of the required object. Must be one of
     *          LISTSXP, LANGSXP, DOTSXP or BCODESXP (not checked).
     *
     * @return Pointer to the created object.
     */
    SEXP Rf_allocSExp(SEXPTYPE t);
}

#endif /* CONSCELL_HPP */
