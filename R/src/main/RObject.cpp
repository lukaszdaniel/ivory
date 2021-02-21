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
#include <CXXR/StringVector.hpp>
#include <Rinternals.h>
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

    RObject::RObject(const RObject &pattern, bool deep)
        : m_type(pattern.m_type), m_scalar(pattern.m_scalar), m_has_class(pattern.m_has_class), m_alt(pattern.m_alt), /*m_gpbits(pattern.m_gpbits),*/
          m_trace(false), m_spare(0), m_named(0), m_extra(0), m_s4_object(pattern.m_s4_object),
          m_active_binding(false),
          m_binding_locked(false), m_assignment_pending(false), m_attrib(clone(pattern.m_attrib, deep))
    {
    }

    RObject *RObject::clone(bool deep) const
    {
        std::cerr << "clone() not yet implemented for this type.\n";
        abort();
    }

    namespace
    {
        // Used in {,un}packGPBits():
        constexpr unsigned int S4_OBJECT_MASK = 1 << 4;
        constexpr unsigned int BINDING_LOCK_MASK = 1 << 14;
        constexpr unsigned int ACTIVE_BINDING_MASK = 1 << 15;
        constexpr unsigned int ASSIGNMENT_PENDING_MASK = 1 << 11;
        // constexpr unsigned int SPECIAL_BINDING_MASK = (ACTIVE_BINDING_MASK | BINDING_LOCK_MASK);
    } // namespace

    unsigned int RObject::packGPBits() const
    {
        unsigned int ans = 0;
        if (m_s4_object)
            ans |= S4_OBJECT_MASK;
        if (m_binding_locked)
            ans |= BINDING_LOCK_MASK;
        if (m_active_binding)
            ans |= ACTIVE_BINDING_MASK;
        if (m_assignment_pending)
            ans |= ASSIGNMENT_PENDING_MASK;
        return ans;
    }

    void RObject::unpackGPBits(unsigned int gpbits)
    {
        // Be careful with precedence!
        setS4Object((gpbits & S4_OBJECT_MASK) != 0);
        m_binding_locked = ((gpbits & BINDING_LOCK_MASK) != 0);
        m_active_binding = ((gpbits & ACTIVE_BINDING_MASK) != 0);
        m_assignment_pending = ((gpbits & ASSIGNMENT_PENDING_MASK) != 0);
    }

    void RObject::setS4Object(bool on)
    {
        // if (!on && sexptype() == S4SXP)
        //     Rf_error(_("CXXR: S4 object (S4SXP) cannot cease to be an S4 object."));
#if CXXR_FALSE
        if (on)
        {
            m_gpbits |= S4_OBJECT_MASK;
        }
        else
        {
            m_gpbits &= ~S4_OBJECT_MASK;
        }
#endif
        m_s4_object = on;
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

    /* Tweaks here based in part on PR#14934 */
    // This follows CR in adding new attributes at the end of the list,
    // though it would be easier to add them at the beginning.
    RObject *RObject::setAttribute(Symbol *name, RObject *value)
    {
        if (!name)
            Rf_error(_("attempt to set an attribute on NULL"));
        if (sexptype() == CHARSXP)
            Rf_error(_("cannot set attribute on a 'CHARSXP'"));
        if (sexptype() == SYMSXP)
            Rf_error(_("cannot set attribute on a symbol"));
        // Update m_has_class if necessary:
        if (name == R_ClassSymbol)
            m_has_class = (value != nullptr);
        // Find attribute:
        /* this does no allocation */
        PairList *prev = nullptr;
        PairList *node = m_attrib;
        while (node && node->tag() != name)
        {
            prev = node; // record last attribute, if any
            node = node->tail();
        }

        if (node)
        { // Attribute already present
            // Update existing attribute:
            if (value)
            {
                if (MAYBE_REFERENCED(value) && value != node->car())
                    value = R_FixupRHS(this, value);
                node->setCar(value);
                return value;
            }
            else if (prev)
            { // Delete existing attribute:
                prev->setTail(node->tail());
            }
            else
                m_attrib = node->tail();
        }
        else if (value)
        {
            // Create new node:
            /* The usual convention is that the caller protects,
               but a lot of existing code depends assume that
               setAttrib/installAttrib protects its arguments */
            GCRoot<Symbol> namer(name);
            GCRoot<> valuer(value);
            if (MAYBE_REFERENCED(value))
                ENSURE_NAMEDMAX(value);
            PairList *newnode = PairList::construct(value, nullptr, name);
            if (prev)
                prev->setTail(newnode);
            else
            { // No preexisting attributes at all:
                m_attrib = newnode;
                propagateAge(m_attrib);
            }
        }
        return value;
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

    void RObject::set_attrib(RObject *x, RObject *v)
    {
        if (!x)
            return;
        GCRoot<PairList> pl(SEXP_downcast<PairList *>(v));
        x->setAttributes(pl);
    }

    RObject *RObject::attrib(RObject *x)
    {
        return x ? x->m_attrib : nullptr;
    }

    unsigned int RObject::named(RObject *x)
    {
        return x ? x->m_named : 0;
    }

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

    /** @brief Object type.
     *
     * @param x Pointer to \c RObject.
     * @return \c SEXPTYPE of \a x, or NILSXP if x is a null pointer.
     */
    SEXPTYPE RObject::typeof_(const RObject *x)
    {
        return x ? x->m_type : NILSXP;
    }

    /**
     * @deprecated
     */
    unsigned int RObject::levels(RObject *x)
    {
        if (!x)
            return 0;

        return x->packGPBits();
    }

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

    bool RObject::altrep(RObject *x)
    {
        return x && x->m_alt;
    }

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
        x->unpackGPBits(v);
        // x->m_gpbits = v;
    }

    bool RObject::scalar(RObject *x)
    {
        return x && x->m_scalar;
    }

    void RObject::setscalar(RObject *x, bool v)
    {
        if (!x)
            return;
        x->m_scalar = v;
    }

    bool RObject::is_scalar(RObject *x, SEXPTYPE t)
    {
        return x && (x->m_type == t) && x->m_scalar;
    }

    unsigned int RObject::refcnt(RObject *x)
    {
        return x ? x->m_named : 0;
    }

    void RObject::set_refcnt(RObject *x, unsigned int v)
    {
        if (!x)
            return;
        x->m_named = v;
    }

    bool RObject::trackrefs(RObject *x)
    {
        return x && (typeof_(x) == CLOSXP ? TRUE : !x->m_spare);
    }

    void RObject::set_trackrefs(RObject *x, bool v)
    {
        if (!x)
            return;
        x->m_spare = v;
    }

    unsigned int RObject::assignment_pending(RObject *x)
    {
        return x && x->m_assignment_pending;
    }

    void RObject::set_assignment_pending(RObject *x, bool v)
    {
        if (!x)
            return;
#if CXXR_FALSE
        if (v)
        {
            x->m_gpbits |= ASSIGNMENT_PENDING_MASK;
        }
        else
        {
            x->m_gpbits &= ~ASSIGNMENT_PENDING_MASK;
        }
#endif
        x->m_assignment_pending = v;
    }

    unsigned int RObject::bndcell_tag(const RObject *e)
    {
        return e ? e->m_extra : 0;
    }

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
    bool RObject::is_s4_object(RObject *x)
    {
        return x && x->m_s4_object;
    }

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

    unsigned int RObject::is_active_binding(RObject *x)
    {
        if (!x)
            return 0;
        return x->m_active_binding;
    }

    unsigned int RObject::binding_is_locked(RObject *x)
    {
        if (!x)
            return 0;
        return x->m_binding_locked;
    }

    void RObject::lock_binding(RObject *x)
    {
        if (!RObject::is_active_binding(x))
        {
            if (RObject::typeof_(x) == SYMSXP)
                MARK_NOT_MUTABLE(Symbol::symvalue(x));
            else
                MARK_NOT_MUTABLE(ConsCell::car0(x));
        }
        // x->m_gpbits |= BINDING_LOCK_MASK;
        x->m_binding_locked = true;
    }

    void RObject::unlock_binding(RObject *x)
    {
        if (!x)
            return;
        // x->m_gpbits &= (~BINDING_LOCK_MASK);
        x->m_binding_locked = false;
    }

    void RObject::set_active_binding_bit(RObject *x)
    {
        if (!x)
            return;
        // x->m_gpbits |= ACTIVE_BINDING_MASK;
        x->m_active_binding = true;
    }

    void RObject::fix_refcnt(RObject *x, RObject *old, RObject *new_)
    {
#ifdef COMPUTE_REFCNT_VALUES
        if (!TRACKREFS(x))
            return;
        if (old == new_)
            return;

        if (old)
            DECREMENT_REFCNT(old);
        if (new_)
            INCREMENT_REFCNT(new_);
#endif
    }

    void RObject::fix_binding_refcnt(RObject *x, RObject *old, RObject *new_)
    {
#ifdef COMPUTE_REFCNT_VALUES
        if (!TRACKREFS(x))
            return;
        if (old == new_)
            return;

        if (old)
        {
            if (ASSIGNMENT_PENDING(x))
                SET_ASSIGNMENT_PENDING(x, FALSE);
            else
                DECREMENT_REFCNT(old);
        }
        if (new_)
            INCREMENT_REFCNT(new_);
#else
        if (ASSIGNMENT_PENDING(x) && old && old != new_)
            SET_ASSIGNMENT_PENDING(x, FALSE);
#endif
    }
} // namespace CXXR

// ***** C interface *****

R_len_t Rf_length(SEXP s)
{
    switch (TYPEOF(s))
    {
    case NILSXP:
        return 0;
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
    case CHARSXP:
    case VECSXP:
    case EXPRSXP:
    case RAWSXP:
        return LENGTH(s);
    case LISTSXP:
    case LANGSXP:
    case DOTSXP:
    {
        R_len_t i = 0;
        while (s && s != R_NilValue)
        {
            ++i;
            s = CDR(s);
        }
        return i;
    }
    case ENVSXP:
        return Rf_envlength(s);
    default:
        return 1;
    }
}

R_xlen_t Rf_xlength(SEXP s)
{
    switch (TYPEOF(s))
    {
    case NILSXP:
        return 0;
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
    case CHARSXP:
    case VECSXP:
    case EXPRSXP:
    case RAWSXP:
        return XLENGTH(s);
    case LISTSXP:
    case LANGSXP:
    case DOTSXP:
    {
        // it is implausible this would be >= 2^31 elements, but allow it
        R_xlen_t i = 0;
        while (s && s != R_NilValue)
        {
            ++i;
            s = CDR(s);
        }
        return i;
    }
    case ENVSXP:
        return Rf_envxlength(s);
    default:
        return 1;
    }
}

SEXP R_FixupRHS(SEXP x, SEXP y)
{
    if (y != R_NilValue && MAYBE_REFERENCED(y))
    {
        if (R_cycle_detected(x, y))
        {
#ifdef WARNING_ON_CYCLE_DETECT
            warning(_("cycle detected"));
            R_cycle_detected(x, y);
#endif
            y = Rf_duplicate(y);
        }
        else
            ENSURE_NAMEDMAX(y);
    }
    return y;
}

Rboolean Rf_isFrame(SEXP s)
{
    return Rf_inherits(s, "data.frame");
}

SEXP Rf_ScalarLogical(int x)
{
    extern SEXP R_LogicalNAValue, R_TrueValue, R_FalseValue;
    if (x == NA_LOGICAL)
        return R_LogicalNAValue;
    else if (x != 0)
        return R_TrueValue;
    else
        return R_FalseValue;
}

Rboolean Rf_conformable(SEXP x_, SEXP y)
{
    int n;
    CXXR::GCRoot<> x(Rf_getAttrib(x_, R_DimSymbol));
    y = Rf_getAttrib(y, R_DimSymbol);
    if ((n = Rf_length(x)) != Rf_length(y))
        return FALSE;
    for (int i = 0; i < n; ++i)
        if (INTEGER(x)[i] != INTEGER(y)[i])
            return FALSE;
    return TRUE;
}

Rboolean Rf_inherits(SEXP s, const char *name)
{
    if (OBJECT(s))
    {
        SEXP klass = Rf_getAttrib(s, R_ClassSymbol);
        int nclass = Rf_length(klass);
        for (int i = 0; i < nclass; ++i)
        {
            if (strcmp(R_CHAR(STRING_ELT(klass, i)), name) == 0)
                return TRUE;
        }
    }
    return FALSE;
}
