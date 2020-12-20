/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2006   The R Development Core Team.
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

/** @file PairList.hpp
 * @brief Class CXXR::PairList and associated C interface.
 *
 * To facilitate inlining of various ConsCell member functions, the
 * definition of class CXXR::PairList itself is in ConsCell.hpp.
 *
 * This file includes C functions for examining and setting the CDR of
 * a CXXR::ConsCell, and other operations accessing the tail of the
 * list; functions for examining and setting the CAR and TAG of a
 * CXXR:ConsCell are to be found in ConsCell.hpp.
 */

#ifndef PAIRLIST_HPP
#define PAIRLIST_HPP

#include <CXXR/ConsCell.hpp>
#include <CXXR/GCRoot.hpp>
#include <CXXR/SEXP_downcast.hpp>
#include <R_ext/Boolean.h>
#include <stdexcept>

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

#define BNDCELL_DVAL(v) (CXXR::PairList::bndcell_dval(v))
#define BNDCELL_IVAL(v) (CXXR::PairList::bndcell_ival(v))
#define BNDCELL_LVAL(v) (CXXR::PairList::bndcell_lval(v))

#define SET_BNDCELL_DVAL(cell, dval_) (CXXR::PairList::set_bndcell_dval(cell, dval_))
#define SET_BNDCELL_IVAL(cell, ival_) (CXXR::PairList::set_bndcell_ival(cell, ival_))
#define SET_BNDCELL_LVAL(cell, lval_) (CXXR::PairList::set_bndcell_lval(cell, lval_))

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
    /** @brief Singly linked list of pairs.
     *
     * LISP-like singly-linked list, each element containing pointers to a
     * 'car' object (this is LISP terminology, and has nothing to do
     * with automobiles) and to a 'tag' object, as well as a pointer to
     * the next element of the list.  (Any of these pointers may be
     * null.)  A PairList object can be thought of either as
     * representing a single element (link) of such a list, or as
     * representing an entire list: that element and all its
     * successors.
     *
     * @note This class implements CR's LISTSXP, LANGSXP, DOTSXP and
     * (for the time being) BCODESXP.  Arguably these ought to be
     * completely distinct classes, but in that case it would have
     * been difficult efficiently to implement functions such as
     * CAR(), which are ubiquitous in the CR code.
     */
    class PairList : public RObject
    {
    public:
        /**
         * @param st The required ::SEXPTYPE of the PairList.  Must
         *           be one of LISTSXP, LANGSXP, DOTSXP or BCODESXP (not
         *           checked).
         * @param cr Pointer to the 'car' of the element to be
         *           constructed.
         * @param tl Pointer to the 'tail' (LISP cdr) of the element
         *           to be constructed.
         * @param tg Pointer to the 'tag' of the element to be constructed.
         */
        explicit PairList(SEXPTYPE st,
                          RObject *cr = nullptr, PairList *tl = nullptr, RObject *tg = nullptr)
            : RObject(st), m_car(cr), m_tail(tl), m_tag(tg)
        {
            checkST(st);
        }

        /** @brief Create a list of a specified length.
         *
         * This constructor creates a PairList with a specified number
         * of elements.  On creation, each element has null 'car' and
         * 'tag'.
         *
         * @param st The required ::SEXPTYPE of the PairList.  Must
         *           be one of LISTSXP, LANGSXP, DOTSXP or BCODESXP
         *           (not checked).
         * @param sz Number of elements required in the list.  Must be
         *           strictly positive; the constructor throws
         *           std::out_of_range if sz is zero.
         */
        PairList(SEXPTYPE st, size_t sz);

        /**
         * @return a const pointer to the 'car' of this PairList
         * element.
         */
        const RObject *car() const
        {
            return m_car;
        }

        /**
         * @return a pointer to the 'car' of this PairList element. 
         */
        RObject *car()
        {
            return m_car;
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
        void setTail(PairList *tl)
        {
            m_tail = tl;
            devolveAge(m_tail);
        }

        /** @brief The name by which this type is known in R.
         *
         * @return the name by which this type is known in R.
         */
        static const char *staticTypeName()
        {
            return "(pairlist type)";
        }

        /**
         * @return a const pointer to the 'tag' of this PairList
         * element.
         */
        const RObject *tag() const
        {
            return m_tag;
        }

        /**
         * @return a pointer to the 'tag' of this PairList element.
         */
        RObject *tag()
        {
            return m_tag;
        }

        /**
         * @return a const pointer to the 'tail' of this PairList
         * element.
         */
        const PairList *tail() const
        {
            return m_tail;
        }

        /**
         * @return a pointer to the 'tail' of this PairList element.
         */
        PairList *tail()
        {
            return m_tail;
        }

        // Virtual function of RObject:
        const char *typeName() const;

        // Virtual functions of GCNode:
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

    private:
        RObject *m_car;
        PairList *m_tail;
        RObject *m_tag;

        // Declared private to ensure that PairList objects are
        // allocated only using 'new':
        ~PairList() {}

        // Not implemented yet.  Declared to prevent
        // compiler-generated versions:
        PairList(const PairList &);
        PairList &operator=(const PairList &);

        // Check that st is a legal SEXPTYPE for a PairList:
        static void checkST(SEXPTYPE st);
    };

    /** @brief (For debugging.)
     *
     * @note The name and interface of this function may well change.
     */
    void pldump(std::ostream &os, const PairList &pl, size_t margin = 0);
} // namespace CXXR

extern "C"
{
    /* Accessor functions. */

    /** @brief Get car of CXXR::PairList element.
     *
     * @param e Pointer to a CXXR::PairList (checked), or a null pointer.
     * @return Pointer to the value of the list car, or 0 if \a e is
     * a null pointer.
     */
    SEXP CAR(SEXP e);

    /**
     * @param e Pointer to a list.
     * @return Pointer to the value of the list head, or 0 if \a e is
     * a null pointer.
     */
    SEXP CAR0(SEXP e);

    /** @brief Get tail of CXXR::PairList element.
     *
     * @param e Pointer to a CXXR::PairList (checked), or a null pointer.
     * @return Pointer to the tail of the list, or 0 if \a e is
     * a null pointer.
     */
    SEXP CDR(SEXP e);

    /**
     * @brief Equivalent to CAR(CAR(e)).
     */
    SEXP CAAR(SEXP e);

    /**
     * @brief Equivalent to CDR(CAR(e)).
     */
    SEXP CDAR(SEXP e);

    /**
     * @brief Equivalent to CAR(CDR(e)).
     */
    SEXP CADR(SEXP e);

    /**
     * @brief Equivalent to CDR(CDR(e)).
     */
    SEXP CDDR(SEXP e);

    /**
     * @brief Equivalent to CDR(CDR(CDR(e))).
     */
    SEXP CDDDR(SEXP e);

    /**
     * @brief Equivalent to CDR(CDR(CDR(CDR(e)))).
     */
    SEXP CD4R(SEXP e);

    /**
     * @brief Equivalent to CAR(CDR(CDR(e))).
     */
    SEXP CADDR(SEXP e);

    /**
     * @brief Equivalent to CAR(CDR(CDR(CDR(e)))).
     */
    SEXP CADDDR(SEXP e);

    /**
     * @brief Equivalent to CAR(CDR(CDR(CDR(e)))).
     */
    SEXP CAD3R(SEXP e);

    /**
     * @brief Equivalent to CAR(CDR(CDR(CDR(CDR(e))))).
     */
    SEXP CAD4R(SEXP e);

    /**
     * @brief Equivalent to CAR(CDR(CDR(CDR(CDR(CDR(e)))))).
     */
    SEXP CAD5R(SEXP e);

    /** @brief Get tag of CXXR::PairList element.
     *
     * @param e Pointer to a CXXR::PairList (checked), or a null pointer.
     * @return Pointer to the tag of the list element, or 0 if \a e is
     * a null pointer.
     */
    SEXP TAG(SEXP e);

    /**
     * @brief Set the tag of a CXXR::PairList element.
     *
     * @param x Pointer to a CXXR::PairList (checked).
     * @param y Pointer a CXXR::RObject representing the new tag of
     *          the CXXR::PairList element.
     */
    void SET_TAG(SEXP x, SEXP y);

    /**
     * @brief Set the 'car' value of a CXXR::PairList element.
     * @param x Pointer to a CXXR::PairList (checked).
     * @param y Pointer a CXXR::RObject representing the new value of the
     *          list car.
     *
     * @returns \a y.
     */
    SEXP SETCAR(SEXP x, SEXP y);

    /**
     * @brief Replace the tail of a CXXR::PairList element.
     * @param x Pointer to a CXXR::PairList (checked).
     * @param y Pointer a CXXR::RObject representing the new tail of the list.
     *
     * @returns \a y.
     */
    SEXP SETCDR(SEXP x, SEXP y);

    /**
     * @brief Set the 'car' value of the second element of list.
     * @param x Pointer to a CXXR::PairList element with at least one successor
     *          (checked).
     * @param y Pointer a CXXR::RObject representing the new value of the
     *          second element of the list.
     *
     * @returns \a y.
     */
    SEXP SETCADR(SEXP x, SEXP y);

    /**
     * @brief Set the 'car' value of the third element of list.
     * @param x Pointer to a CXXR::PairList element with at least two
     *          successors (checked).
     * @param y Pointer a CXXR::RObject representing the new value of the
     *          third element of the list.
     *
     * @returns \a y.
     */
    SEXP SETCADDR(SEXP x, SEXP y);

    /**
     * @brief Set the 'car' value of the fourth element of list.
     * @param x Pointer to a CXXR::PairList element with at least three
     *          successors (checked).
     * @param y Pointer a CXXR::RObject representing the new value of the
     *          fourth element of the list.
     *
     * @returns \a y.
     */
    SEXP SETCADDDR(SEXP x, SEXP y);

    /**
     * @brief Set the 'car' value of the fifth element of list.
     * @param x Pointer to a CXXR::PairList element with at least four
     *          successors (checked).
     * @param y Pointer a CXXR::RObject representing the new value of the
     *          fifth element of the list.
     *
     * @returns \a y.
     */
    SEXP SETCAD4R(SEXP x, SEXP y);

    /* Bindings */

    /** @brief Is a Binding active?
     *
     * @param b Pointer to a ConsCell object (checked). If \a b points
     *          to any type of ConsCell other than a PairList, the
     *          function returns FALSE.  Otherwise \a b should point
     *          to a PairList object representing a Frame::Binding
     *          (e.g. because it was produced using
     *          Frame::asPairList() ).
     *
     * @return true iff this is an active Binding.
     */
    Rboolean IS_ACTIVE_BINDING(SEXP b);

    /** @brief Is a Binding locked?
     *
     * @param b Pointer to a PairList object (checked) representing a
     *          Frame::Binding (e.g. because it was produced using
     *          Frame::asPairList() ).
     *
     * @return true iff this Binding is locked.
     */
    Rboolean BINDING_IS_LOCKED(SEXP b);

    /** @brief Designate as active the binding represented by a
     * PairList object.
     *
     * @param b Pointer to a PairList object (checked) representing a
     *          Frame::Binding (e.g. because it was produced using
     *          Frame::asPairList() ).
     */
    void SET_ACTIVE_BINDING_BIT(SEXP b);

    /** @brief Lock the binding represented by a PairList object.
     *
     * @param b Pointer to a PairList object (checked) representing a
     *          Frame::Binding (e.g. because it was produced using
     *          Frame::asPairList() ).
     */
    void LOCK_BINDING(SEXP b);

    /** @brief Unlock the binding represented by a PairList object.
     *
     * @param b Pointer to a PairList object (checked) representing a
     *          Frame::Binding (e.g. because it was produced using
     *          Frame::asPairList() ).
     */
    void UNLOCK_BINDING(SEXP b);

    /** @brief Create a LISTSXP CXXR::PairList of a specified length.
     *
     * This constructor creates a CXXR::PairList of ::SEXPTYPE LISTSXP
     * with a specified number of elements.  On creation, each element
     * has null 'car' and 'tag'.
     *
     * @param n Number of elements required in the list.
     *
     * @return The constructed list, or a null pointer if \a n is zero.
     */
    SEXP Rf_allocList(int n);

    /** @brief Create a single CXXR::PairList element.
     *
     * Create a single CXXR::PairList element, with null car and tag
     * pointers.
     *
     * @param t The ::SEXPTYPE of the required object. Must be one of
     *          LISTSXP, LANGSXP, DOTSXP or BCODESXP (not checked).
     *
     * @return Pointer to the created object.
     */
    SEXP Rf_allocSExp(SEXPTYPE t);

    /** @brief Create a CXXR::PairList of ::SEXPTYPE LISTSXP.
     *
     * Creates a CXXR::PairList of ::SEXPTYPE LISTSXP with a specified
     * car and tail.
     *
     * @param cr Pointer to the 'car' of the element to be created.
     *
     * @param tl Pointer to the 'tail' of the element to be created,
     *          which must be of a CXXR::PairList type (checked).
     *
     * @return Pointer to the constructed list.
     */
    SEXP Rf_cons(SEXP cr, SEXP tl);

    /** @brief Create a CXXR::PairList of ::SEXPTYPE LANGSXP.
     *
     * Creates a CXXR::PairList of ::SEXPTYPE LANGSXP with a specified
     * car and tail.
     *
     * @param cr Pointer to the 'car' of the element to be created.
     *
     * @param tl Pointer to the 'tail' of the element to be created,
     *          which must be of a CXXR::PairList type (checked). 
     *
     * @return Pointer to the constructed list.
     */
    SEXP Rf_lcons(SEXP cr, SEXP tl);
} // extern "C"

#endif /* PAIRLIST_HPP */
