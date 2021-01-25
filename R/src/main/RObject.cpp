/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999--2020  The R Core Team.
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

/** @file RObject.cpp
 *
 * Class RObject and associated C interface functions.
 */

#include <CXXR/RObject.hpp>
#include <CXXR/Symbol.hpp>
#include <CXXR/PairList.hpp>
#include <R_ext/Boolean.h>

namespace CXXR
{
    // Force the creation of non-inline embodiments of functions callable
    // from C:
    namespace ForceNonInline
    {
        const auto &DUPLICATE_ATTRIBptr = DUPLICATE_ATTRIB;
        const auto &SHALLOW_DUPLICATE_ATTRIBptr = SHALLOW_DUPLICATE_ATTRIB;
        const auto &isNullptr = Rf_isNull;
        const auto &isObjectptr = Rf_isObject;
        const auto &NAMEDptr = NAMED;
        const auto &OBJECTptr = OBJECT;
        const auto &SET_NAMEDptr = SET_NAMED;
        const auto &ENSURE_NAMEDMAXptr = ENSURE_NAMEDMAX;
        const auto &ENSURE_NAMEDptr = ENSURE_NAMED;
        const auto &SETTER_CLEAR_NAMEDptr = SETTER_CLEAR_NAMED;
        const auto &RAISE_NAMEDptr = RAISE_NAMED;
        const auto &TYPEOFptr = TYPEOF;
        const auto &LEVELSptr = LEVELS;
        const auto &SETLEVELSptr = SETLEVELS;
        const auto &ALTREPptr = ALTREP;
        const auto &SETALTREPptr = SETALTREP;
    } // namespace ForceNonInline

    // In CXXR R_NilValue is simply a null pointer:
    SEXP R_NilValue = nullptr;

    RObject::~RObject()
    {
    }

    void RObject::visitChildren(const_visitor *v) const
    {
        if (m_attrib && (m_type != CHARSXP || m_attrib->m_type != CHARSXP))
            m_attrib->conductVisitor(v);
    }

    RObject::RObject(const RObject &pattern)
        : m_type(pattern.m_type), m_scalar(pattern.m_scalar), m_has_class(pattern.m_has_class), m_alt(pattern.m_alt), m_gpbits(pattern.m_gpbits),
          m_trace(pattern.m_trace), m_spare(pattern.m_spare), m_named(pattern.m_named), m_extra(pattern.m_extra), m_attrib(pattern.m_attrib)
    {
    }

    void RObject::setS4Object(bool on)
    {
        // if (!on && sexptype() == S4SXP)
        //     Rf_error("CXXR: S4 object (S4SXP) cannot cease to be an S4 object.");
        if (on)
            m_gpbits |= S4_OBJECT_MASK;
        else
            m_gpbits &= ~S4_OBJECT_MASK;
    }

    const char *RObject::typeName() const
    {
        return Rf_type2char(sexptype());
    }

    RObject *RObject::getAttribute(const Symbol *name)
    {
        for (PairList *node = m_attrib; node; node = node->tail())
            if (node->tag() == name)
                return node->car();
        return nullptr;
    }

    const RObject *RObject::getAttribute(const Symbol *name) const
    {
        for (PairList *node = m_attrib; node; node = node->tail())
            if (node->tag() == name)
                return node->car();
        return nullptr;
    }

    // This follows CR in adding new attributes at the end of the list,
    // though it would be easier to add them at the beginning.
    void RObject::setAttribute(Symbol *name, RObject *value)
    {
        if (!name)
            Rf_error(_("attempt to set an attribute on NULL"));
        // Update m_has_class if necessary:
        if (name == R_ClassSymbol)
            m_has_class = (value != nullptr);
        // Find attribute:
        PairList *prev = nullptr;
        PairList *node = m_attrib;
        while (node && node->tag() != name)
        {
            prev = node;
            node = node->tail();
        }

        if (node)
        { // Attribute already present
            // Update existing attribute:
            if (value)
                node->setCar(value);
            // Delete existing attribute:
            else if (prev)
                prev->setTail(node->tail());
            else
                m_attrib = node->tail();
        }
        else if (value)
        {
            // Create new node:
            PairList *newnode = new PairList(value, nullptr, name);
            newnode->expose();
            if (prev)
                prev->setTail(newnode);
            else
            { // No preexisting attributes at all:
                m_attrib = newnode;
                propagateAge(m_attrib);
            }
        }
    }

    // This has complexity O(n^2) where n is the number of attributes, but
    // we assume n is very small.
    void RObject::setAttributes(PairList *new_attributes)
    {
        clearAttributes();
        while (new_attributes)
        {
            Symbol *name = SEXP_downcast<Symbol *>(new_attributes->tag());
            setAttribute(name, new_attributes->car());
            new_attributes = new_attributes->tail();
        }
    }

    /**
     * Replace x's attributes by \a v.
     * @param x Pointer to \c RObject.
     * @param v Pointer to attributes \c RObject.
     * @todo Could \a v be \c const ?
     */
    void RObject::set_attrib(RObject *x, RObject *v)
    {
        if (!x)
            return;
        GCRoot<PairList> pl(SEXP_downcast<PairList *>(v));
        x->setAttributes(pl);
    }

    /**
     * Return the attributes of an \c RObject.
     * @param x Pointer to the \c RObject whose attributes are required.
     * @return Pointer to the attributes object of \a x , or 0 if \a x is
     * a null pointer.
     */
    RObject *RObject::attrib(RObject *x) { return x ? x->m_attrib : nullptr; }

    /**
     * Object copying status.
     * @param x Pointer to \c RObject.
     * @return Refer to 'R Internals' document.  Returns 0 if \a x is a
     * null pointer.
     */
    unsigned int RObject::named(RObject *x) { return x ? x->m_named : 0; }

    /**
     * Set object copying status.  Does nothing if \a x is a null pointer.
     * @param x Pointer to \c RObject.
     * @param v Refer to 'R Internals' document.
     * @deprecated Ought to be private.
     */
    void RObject::set_named(RObject *x, unsigned int v)
    {
        if (!x)
            return;
        x->m_named = v;
    }

    /**
     * @deprecated Ought to be private.
     */
    void RObject::set_typeof(RObject *x, SEXPTYPE v)
    {
        if (!x)
            return;
        x->m_type = v;
    }

    /**
     * Object type.
     * @param x Pointer to \c RObject.
     * @return \c SEXPTYPE of \a x, or NILSXP if x is a null pointer.
     */
    SEXPTYPE RObject::typeof_(const RObject *x) { return x ? x->m_type : NILSXP; }

    /**
     * @deprecated
     */
    unsigned int RObject::levels(RObject *x) { return x ? x->m_gpbits : 0; }

    /**
     * Does \c RObject have a class attribute?.
     * @param x Pointer to an \c RObject.
     * @return true iff \a x has a class attribute.  Returns false if \a x
     * is 0.
     */
    bool RObject::object(RObject *x)
    {
        return x && x->hasClass();
    }

    /**
     * @deprecated This has no effect in CXXR.
     * Object status is determined in set_attrib().
     */
    void RObject::set_object(RObject *x, bool v)
    {
        if (!x)
            return;
        // x->m_has_class = v;
    }

    bool RObject::altrep(RObject *x) { return x && x->m_alt; }

    void RObject::set_altrep(RObject *x, bool v)
    {
        if (!x)
            return;
        x->m_alt = v;
    }

    /**
     * @deprecated
     */
    void RObject::setlevels(RObject *x, unsigned short int v)
    {
        if (!x)
            return;
        x->m_gpbits = (unsigned short)v;
    }

    bool RObject::scalar(RObject *x) { return x && x->m_scalar; }

    void RObject::setscalar(RObject *x, bool v)
    {
        if (!x)
            return;
        x->m_scalar = v;
    }

    bool RObject::is_scalar(RObject *x, SEXPTYPE t) { return x && (x->m_type == t) && x->m_scalar; }

    unsigned int RObject::refcnt(RObject *x) { return x ? x->m_named : 0; }

    void RObject::set_refcnt(RObject *x, unsigned int v)
    {
        if (!x)
            return;
        x->m_named = v;
    }

    bool RObject::trackrefs(RObject *x) { return x && (typeof_(x) == CLOSXP ? TRUE : !x->m_spare); }

    void RObject::set_trackrefs(RObject *x, bool v)
    {
        if (!x)
            return;
        x->m_spare = v;
    }

    unsigned int RObject::assignment_pending(RObject *x) { return x ? (x->m_gpbits & ASSIGNMENT_PENDING_MASK) : 0; }

    void RObject::set_assignment_pending(RObject *x, bool v)
    {
        if (!x)
            return;
        if (v)
            (((x)->m_gpbits) |= ASSIGNMENT_PENDING_MASK);
        else
            (((x)->m_gpbits) &= ~ASSIGNMENT_PENDING_MASK);
    }

    unsigned int RObject::missing(RObject *x) { return x ? (x->m_gpbits & MISSING_MASK) : 0; } /* for closure calls */

    void RObject::set_missing(RObject *x, int v)
    {
        if (!x)
            return;
        int __other_flags__ = x->m_gpbits & ~MISSING_MASK;
        x->m_gpbits = __other_flags__ | v;
    }

    unsigned int RObject::bndcell_tag(const RObject *e) { return e ? e->m_extra : 0; }

    void RObject::set_bndcell_tag(RObject *e, unsigned int v)
    {
        if (!e)
            return;
        e->m_extra = v;
    }

    /**
     * An S4 object?
     * @param x Pointer to \c RObject.
     * @return true iff \a x is an S4 object.  Returns false if \a x
     * is 0.
     * @note S4 object bit, set by R_do_new_object for all new() calls
     */
    bool RObject::is_s4_object(RObject *x) { return x && (x->m_gpbits & S4_OBJECT_MASK); }

    /**
     * @deprecated Ought to be private.
     */
    void RObject::set_s4_object(RObject *x)
    {
        if (!x)
            return;
        x->setS4Object(true);
    }

    /**
     * @deprecated Ought to be private.
     */
    void RObject::unset_s4_object(RObject *x)
    {
        if (!x)
            return;
        x->setS4Object(false);
    }

    /* JIT optimization support */

    unsigned int RObject::nojit(RObject *x) { return x ? (x->m_gpbits & NOJIT_MASK) : 0; }

    void RObject::set_nojit(RObject *x)
    {
        if (!x)
            return;
        x->m_gpbits |= NOJIT_MASK;
    }

    unsigned int RObject::maybejit(RObject *x) { return x ? (x->m_gpbits & MAYBEJIT_MASK) : 0; }

    void RObject::set_maybejit(RObject *x)
    {
        if (!x)
            return;
        x->m_gpbits |= MAYBEJIT_MASK;
    }

    void RObject::unset_maybejit(RObject *x)
    {
        if (!x)
            return;
        x->m_gpbits &= ~MAYBEJIT_MASK;
    }

    /* Growable vector support */
    unsigned int RObject::growable_bit_set(RObject *x) { return x ? (x->m_gpbits & GROWABLE_MASK) : 0; }

    void RObject::set_growable_bit(RObject *x)
    {
        if (!x)
            return;
        x->m_gpbits |= GROWABLE_MASK;
    }

    unsigned int RObject::is_active_binding(RObject *x) { return x ? (x->m_gpbits & ACTIVE_BINDING_MASK) : 0; }

    unsigned int RObject::binding_is_locked(RObject *x) { return x ? (x->m_gpbits & BINDING_LOCK_MASK) : 0; }

    void RObject::lock_binding(RObject *x)
    {
        if (!RObject::is_active_binding(x))
        {
            if (RObject::typeof_(x) == SYMSXP)
                MARK_NOT_MUTABLE(Symbol::symvalue(x));
            else
                MARK_NOT_MUTABLE(ConsCell::car0(x));
        }
        ((x))->m_gpbits |= BINDING_LOCK_MASK;
    }

    void RObject::unlock_binding(RObject *x)
    {
        if (!x)
            return;
        x->m_gpbits &= (~BINDING_LOCK_MASK);
    }

    void RObject::set_active_binding_bit(RObject *x)
    {
        if (!x)
            return;
        x->m_gpbits |= ACTIVE_BINDING_MASK;
    }
} // namespace CXXR