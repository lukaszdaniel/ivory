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
#include <CXXR/GCStackRoot.hpp>
#include <CXXR/strutil.hpp>
#include <R_ext/Boolean.h>
#include <Rinternals.h>
#include <Localization.h>

namespace CXXR
{
    /** @brief Singly linked list of pairs.
     *
     * LISP-like singly-linked list, each element containing pointers to a
     * 'car' object (this is LISP terminology, and has nothing to do
     * with automobiles) and to a 'tag' object, as well as a pointer to
     * the next element of the list.  (Any of these pointers may be
     * null.)  A PairList object is considered to 'own' its car, its
     * tag, and all its successors.
     */
    class PairList : public ConsCell
    {
    public:
        /**
         * @param cr Pointer to the 'car' of the element to be
         *           constructed.
         * @param tl Pointer to the 'tail' (LISP cdr) of the element
         *           to be constructed.
         * @param tg Pointer to the 'tag' of the element to be constructed.
         */
        explicit PairList(RObject *cr = nullptr, PairList *tl = nullptr, RObject *tg = nullptr)
            : ConsCell(LISTSXP, cr, tl, tg), m_argused(0)
        {
        }

        /** @brief Copy constructor.
         *
         * @param pattern PairList to be copied.
         *
         * @param deep Indicator whether to perform deep or shallow copy.
         */
        PairList(const PairList &pattern, bool deep);

        /** @brief Create a PairList element on the free store.
         *
         * Unlike the constructor (and contrary to CXXR conventions
         * generally) this function protects its arguments from the
         * garbage collector.
         *
         * @param cr Pointer to the 'car' of the element to be
         *           constructed.
         *
         * @param tl Pointer to the 'tail' (LISP cdr) of the element
         *           to be constructed.
         *
         * @return Pointer to newly created PairList element.
         */
        template <class T = PairList>
        static T *construct(RObject *cr, PairList *tl = nullptr, RObject *tg = nullptr)
        {
            s_cons_car = cr;
            s_cons_cdr = tl;
            T *ans = GCNode::expose(new T(cr, tl, tg));
            s_cons_cdr = nullptr;
            s_cons_car = nullptr;
            return ans;
        }

        /** @brief Create a PairList of a specified length.
         *
         * This constructor creates a chain of PairList nodes with a
         * specified number of elements.  On creation, each element
         * has null 'car' and 'tag'.
         *
         * @param sz Number of elements required in the list.  If
         *           zero, the function returns a null pointer.
         */
        static PairList *makeList(size_t sz);

        unsigned char argUsed() const
        {
            return m_argused;
        }

        void setArgUsed(unsigned char v)
        {
            m_argused = v;
        }

        /** @brief The name by which this type is known in R.
         *
         * @return the name by which this type is known in R.
         */
        static const char *staticTypeName()
        {
            return "pairlist";
        }

        // Virtual functions of RObject:
        PairList *clone(bool deep) const override;
        const char *typeName() const override;

    protected:
        // Declared protected to ensure that PairList objects are
        // allocated only using 'new':
        ~PairList() {}

    private:
        // Permanent GCRoots used to implement construct() without pushing
        // and popping:
        static GCStackRoot<> s_cons_car;
        static GCStackRoot<PairList> s_cons_cdr;

        // Tailless copy constructor.  Copies the node without copying
        // its tail.  Used in implementing the copy constructor
        // proper.  The third parameter is simply to provide a
        // distinct signature, and its value is ignored.
        PairList(const PairList &pattern, bool deep, int);

        // Not implemented yet.  Declared to prevent
        // compiler-generated version:
        PairList &operator=(const PairList &);

        // 'Scratchpad' field used in handling argument lists,
        // formerly hosted in the 'gp' field of sxpinfo_struct.  It
        // would be good to remove this from the class altogether.
        unsigned char m_argused;
    };

    inline void ConsCell::setTail(PairList *tl)
    {
#ifdef TESTING_WRITE_BARRIER
        /* this should not add a non-tracking CDR to a tracking cell */
        if (trackrefs() && tl && !tl->trackrefs())
        {
            Color::Modifier red(Color::Modifier::Code::FG_RED);
            Color::Modifier def(Color::Modifier::Code::FG_DEFAULT);
            std::cerr << red << _("inserting non-tracking CDR in tracking cell") << def << std::endl;
            // Rf_error(_("inserting non-tracking CDR in tracking cell"));
        }
#endif
        // m_tail = tl;
        // m_tail.propagateAge(this);
        m_tail.retarget(this, tl);
    }

    template <class T = PairList>
    T *CXXR_cons(SEXP car, SEXP cdr)
    {
        return PairList::construct<T>(car, SEXP_downcast<PairList *>(cdr));
    }

    // Used in matching formal and actual arguments (within match.cpp
    // and unique.cpp).
    inline unsigned char ARGUSED(SEXP x)
    {
        return SEXP_downcast<PairList *>(x)->argUsed();
    }

    // Used in matching formal and actual arguments (within match.cpp
    // and unique.cpp).
    inline void SET_ARGUSED(SEXP x, unsigned char v)
    {
        SEXP_downcast<PairList *>(x)->setArgUsed(v);
    }

} // namespace CXXR

namespace R
{
    /**
     * @param e Pointer to a list.
     *
     * @return Pointer to the value of the list head, or 0 if \a e is
     * a null pointer.
     */
    SEXP CAR0(SEXP e);

    /** @brief Destructively removes R_NilValue ('NULL') elements from a pairlist.
     *
     * @param s Pointer to a CXXR::ConsCell.
     * 
     * @param keep_initial Indicator whether to keep initial NULL values.
     *
     * @return Pointer to the rearanged CXXR::ConsCell.
     */
    CXXR::PairList *R_listCompact(CXXR::PairList *s, bool keep_initial);

    SEXP Rf_allocFormalsList2(SEXP sym1, SEXP sym2);
    SEXP Rf_allocFormalsList3(SEXP sym1, SEXP sym2, SEXP sym3);
    SEXP Rf_allocFormalsList4(SEXP sym1, SEXP sym2, SEXP sym3, SEXP sym4);
    SEXP Rf_allocFormalsList5(SEXP sym1, SEXP sym2, SEXP sym3, SEXP sym4, SEXP sym5);
    SEXP Rf_allocFormalsList6(SEXP sym1, SEXP sym2, SEXP sym3, SEXP sym4, SEXP sym5, SEXP sym6);
} // namespace R

extern "C"
{
    /** @brief Get car of CXXR::ConsCell.
     *
     * @param e Pointer to a CXXR::ConsCell (checked), or a null pointer.
     *
     * @return Pointer to the value of the list car, or 0 if \a e is
     * a null pointer.
     */
    SEXP CAR(SEXP e);

    /** @brief Get tail of CXXR::ConsCell.
     *
     * @param e Pointer to a CXXR::ConsCell (checked), or a null pointer.
     *
     * @return Pointer to the tail of the list, or 0 if \a e is
     * a null pointer.
     */
    SEXP CDR(SEXP e);

    /** @brief Equivalent to CAR(CAR(e)).
     */
    SEXP CAAR(SEXP e);

    /** @brief Equivalent to CDR(CAR(e)).
     */
    SEXP CDAR(SEXP e);

    /** @brief Equivalent to CAR(CDR(e)).
     */
    SEXP CADR(SEXP e);

    /** @brief Equivalent to CDR(CDR(e)).
     */
    SEXP CDDR(SEXP e);

    /** @brief Equivalent to CDR(CDR(CDR(e))).
     */
    SEXP CDDDR(SEXP e);

    /** @brief Equivalent to CDR(CDR(CDR(CDR(e)))).
     */
    SEXP CD4R(SEXP e);

    /** @brief Equivalent to CAR(CDR(CDR(e))).
     */
    SEXP CADDR(SEXP e);

    /** @brief Equivalent to CAR(CDR(CDR(CDR(e)))).
     */
    SEXP CADDDR(SEXP e);

    /** @brief Equivalent to CAR(CDR(CDR(CDR(e)))).
     */
    SEXP CAD3R(SEXP e);

    /** @brief Equivalent to CAR(CDR(CDR(CDR(CDR(e))))).
     */
    SEXP CAD4R(SEXP e);

    /** @brief Equivalent to CAR(CDR(CDR(CDR(CDR(CDR(e)))))).
     */
    SEXP CAD5R(SEXP e);

    /** @brief Set the 'car' value of a CXXR::ConsCell.
     *
     * @param x Pointer to a CXXR::ConsCell (checked).
     *
     * @param y Pointer to a CXXR::RObject representing the new value of the
     *          list car.
     *
     * @returns \a y.
     */
    SEXP SETCAR(SEXP x, SEXP y);

    /** @brief Replace the tail of a CXXR::ConsCell.
     *
     * @param x Pointer to a CXXR::ConsCell (checked).
     *
     * @param y Pointer to a CXXR::RObject representing the new tail of the list.
     *
     * @returns \a y.
     */
    SEXP SETCDR(SEXP x, SEXP y);

    /** @brief Set the 'car' value of the second element of list.
     *
     * @param x Pointer to a CXXR::ConsCell with at least one successor
     *          (checked).
     *
     * @param y Pointer to a CXXR::RObject representing the new value of the
     *          second element of the list.
     *
     * @returns \a y.
     */
    SEXP SETCADR(SEXP x, SEXP y);

    /** @brief Set the 'car' value of the third element of list.
     *
     * @param x Pointer to a CXXR::ConsCell with at least two
     *          successors (checked).
     *
     * @param y Pointer to a CXXR::RObject representing the new value of the
     *          third element of the list.
     *
     * @returns \a y.
     */
    SEXP SETCADDR(SEXP x, SEXP y);

    /** @brief Set the 'car' value of the fourth element of list.
     *
     * @param x Pointer to a CXXR::ConsCell with at least three
     *          successors (checked).
     *
     * @param y Pointer to a CXXR::RObject representing the new value of the
     *          fourth element of the list.
     *
     * @returns \a y.
     */
    SEXP SETCADDDR(SEXP x, SEXP y);

    /** @brief Set the 'car' value of the fifth element of list.
     *
     * @param x Pointer to a CXXR::ConsCell with at least four
     *          successors (checked).
     *
     * @param y Pointer to a CXXR::RObject representing the new value of the
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
     *          Frame::asPairList()).
     *
     * @return true iff this is an active Binding.
     */
    Rboolean IS_ACTIVE_BINDING(SEXP b);

    /** @brief Is a Binding locked?
     *
     * @param b Pointer to a PairList object (checked) representing a
     *          Frame::Binding (e.g. because it was produced using
     *          Frame::asPairList()).
     *
     * @return true iff this Binding is locked.
     */
    Rboolean BINDING_IS_LOCKED(SEXP b);

    /** @brief Designate as active the binding represented by a
     * PairList object.
     *
     * @param b Pointer to a PairList object (checked) representing a
     *          Frame::Binding (e.g. because it was produced using
     *          Frame::asPairList()).
     */
    void SET_ACTIVE_BINDING_BIT(SEXP b);

    /** @brief Lock the binding represented by a PairList object.
     *
     * @param b Pointer to a PairList object (checked) representing a
     *          Frame::Binding (e.g. because it was produced using
     *          Frame::asPairList()).
     */
    void LOCK_BINDING(SEXP b);

    /** @brief Unlock the binding represented by a PairList object.
     *
     * @param b Pointer to a PairList object (checked) representing a
     *          Frame::Binding (e.g. because it was produced using
     *          Frame::asPairList()).
     */
    void UNLOCK_BINDING(SEXP b);

    /** @brief Create a CXXR::PairList of a specified length.
     *
     * This constructor creates a CXXR::PairList with a specified
     * number of elements.  On creation, each element has null 'car'
     * and 'tag'.
     *
     * @param n Number of elements required in the list.
     *
     * @return The constructed list, or a null pointer if \a n is zero.
     */
    SEXP Rf_allocList(int n);

    /** @brief Creates a CXXR::PairList with a specified car and tail.
     *
     * This function protects its arguments from the garbage collector.
     *
     * @param cr Pointer to the 'car' of the element to be created.
     *
     * @param tl Pointer to the 'tail' of the element to be created,
     *          which must be of a CXXR::PairList type (checked).
     *
     * @return Pointer to the constructed list.
     */
    SEXP Rf_cons(SEXP cr, SEXP tl);

    /** @brief Get the i-th element of a list.
     *
     * Return a dotted pair with the given CAR and CDR.
     * The (R) TAG slot on the cell is set to NULL.
     *
     * @param list SEXP object.
     *
     * @param i i-th element of that object.
     *
     * @return i-th element.
     *
     * @note from list.cpp 
     */
    SEXP Rf_elt(SEXP list, int i);

    /** @brief Return the last element of a list
     */
    SEXP Rf_lastElt(SEXP list);

    /* Shorthands for creating small lists */

    SEXP Rf_list1(SEXP s);
    SEXP Rf_list2(SEXP s, SEXP t);
    SEXP Rf_list3(SEXP s, SEXP t, SEXP u);
    SEXP Rf_list4(SEXP s, SEXP t, SEXP u, SEXP v);
    SEXP Rf_list5(SEXP s, SEXP t, SEXP u, SEXP v, SEXP w);
    SEXP Rf_list6(SEXP s, SEXP t, SEXP u, SEXP v, SEXP w, SEXP x);

    /** @brief Destructive list append
     *
     * @note See also ``append''
     */
    SEXP Rf_listAppend(SEXP s, SEXP t);

    /** @brief Check to see if a list can be made into a vector.
     *
     * @note it must have every element being a vector of length 1.
     *       BUT it does not exclude 0!
     *
     * @return true if list can be made into a vector
     */
    Rboolean Rf_isVectorizable(SEXP s);
    Rboolean Rf_isList(SEXP s);
    Rboolean Rf_isPairList(SEXP s);
} // extern "C"

#if (defined(R_NO_REMAP) && defined(COMPILING_IVORY)) && defined(__cplusplus)
const auto elt = Rf_elt;
const auto lastElt = Rf_lastElt;
const auto list1 = Rf_list1;
const auto list2 = Rf_list2;
const auto list3 = Rf_list3;
const auto list4 = Rf_list4;
const auto list5 = Rf_list5;
const auto list6 = Rf_list6;
const auto listAppend = Rf_listAppend;
const auto isVectorizable = Rf_isVectorizable;
const auto isList = Rf_isList;
const auto isPairList = Rf_isPairList;
#endif

#endif /* PAIRLIST_HPP */
