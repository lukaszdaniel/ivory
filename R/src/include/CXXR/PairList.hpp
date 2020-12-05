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
 * @brief C interface of class PairList.
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
#include <R_ext/Boolean.h>

extern "C"
{
    /* Accessor functions. */

    /**
     * @param e Pointer to a list.
     * @return Pointer to the value of the list head, or 0 if \a e is
     * a null pointer.
     */
    SEXP CAR(SEXP e);

    /**
     * @param e Pointer to a list.
     * @return Pointer to the value of the list head, or 0 if \a e is
     * a null pointer.
     */
    SEXP CAR0(SEXP e);

    /** @brief Get tail of CXXR::ConsCell.
     *
     * @param e Pointer to a CXXR::ConsCell (checked), or a null pointer.
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

    int MISSING(SEXP x);

    /**
     * @param e Pointer to a list.
     * @return Pointer to the tag (key) of the list head, or 0 if \a e is
     * a null pointer.
     */
    SEXP TAG(SEXP e);

    void SET_MISSING(SEXP x, int v);

    /**
     * @brief Set the tag of a list element.
     * @param x Pointer to a list.
     * @param y Pointer a CXXR::RObject representing the new tag of the list head..
     */
    void SET_TAG(SEXP x, SEXP y);

    /**
     * @brief Set the value of the first element of list.
     * @param x Pointer to a list.
     * @param y Pointer a CXXR::RObject representing the new value of the
     *          list head.
     */
    SEXP SETCAR(SEXP x, SEXP y);

    /**
     * @brief Replace the tail of a CXXR::ConsCell.
     * @param x Pointer to a CXXR::ConsCell (checked).
     * @param y Pointer a CXXR::RObject representing the new tail of the list.
     *
     * @returns \a y.
     */
    SEXP SETCDR(SEXP x, SEXP y);

    /**
     * @brief Set the 'car' value of the second element of list.
     * @param x Pointer to a CXXR::ConsCell with at least one successor
     *          (checked).
     * @param y Pointer a CXXR::RObject representing the new value of the
     *          second element of the list.
     *
     * @returns \a y.
     */
    SEXP SETCADR(SEXP x, SEXP y);

    /**
     * @brief Set the 'car' value of the third element of list.
     * @param x Pointer to a CXXR::ConsCell with at least two
     *          successors (checked).
     * @param y Pointer a CXXR::RObject representing the new value of the
     *          third element of the list.
     *
     * @returns \a y.
     */
    SEXP SETCADDR(SEXP x, SEXP y);

    /**
     * @brief Set the 'car' value of the fourth element of list.
     * @param x Pointer to a CXXR::ConsCell with at least three
     *          successors (checked).
     * @param y Pointer a CXXR::RObject representing the new value of the
     *          fourth element of the list.
     *
     * @returns \a y.
     */
    SEXP SETCADDDR(SEXP x, SEXP y);

    /**
     * @brief Set the 'car' value of the fifth element of list.
     * @param x Pointer to a CXXR::ConsCell with at least four
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

    /**
     * @brief Create a CXXR::RObject of listlike type
     *
     * Despite the general name, the code (in memory.cpp) seems to assume
     * that the resulting object with have CAR, CDR, TAG etc.
     *
     * @param t The stype of the required object.
     * @return Pointer to the created object.
     */
    SEXP Rf_allocSExp(SEXPTYPE t);

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
} // extern "C"

#endif /* PAIRLIST_HPP */
