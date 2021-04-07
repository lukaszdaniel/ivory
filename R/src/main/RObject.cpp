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
#include <CXXR/RAltRep.hpp>
#include <CXXR/StringVector.hpp>
#include <Rinternals.h>
#include <R_ext/Boolean.h>

using namespace CXXR;

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

    RObject::~RObject()
    {
    }

    void RObject::visitChildren(const_visitor *v) const
    {
        if (m_attrib && (m_type != CHARSXP || m_attrib->m_type != CHARSXP))
            m_attrib->conductVisitor(v);
    }

    RObject::RObject(SEXPTYPE stype) : GCNode(), m_type(stype), m_scalar(false), m_has_class(false), m_alt(false), /*m_gpbits(0),*/
                                       m_trace(false), m_spare(false), m_named(0), m_extra(0), m_s4_object(stype == S4SXP),
                                       m_active_binding(false), m_binding_locked(false), m_assignment_pending(false), m_attrib(nullptr)
    {
    }

    RObject::RObject(const RObject &pattern, bool deep)
        : GCNode(), m_type(pattern.m_type), m_scalar(pattern.m_scalar), m_has_class(pattern.m_has_class), m_alt(pattern.m_alt), /*m_gpbits(pattern.m_gpbits),*/
          m_trace(false), m_spare(false), m_named(0), m_extra(0), m_s4_object(pattern.m_s4_object),
          m_active_binding(false),
          m_binding_locked(false), m_assignment_pending(false), m_attrib(clone(pattern.m_attrib, deep))
    {
        if (m_attrib)
            m_attrib->incrementRefCount();
    }

    void RObject::cloneAttributes(const RObject &source, bool deep)
    {
        m_attrib = RObject::clone(source.m_attrib, deep);
        if (m_attrib)
            m_attrib->incrementRefCount();
        m_has_class = source.m_has_class;
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

    void RObject::clearAttributes()
    {
        if (m_attrib)
        {
            m_attrib->decrementRefCount();
            m_attrib = nullptr;
            m_has_class = false;
        }
    }

    RObject *RObject::evaluate(Environment *env)
    {
        /* Make sure constants in expressions are NAMED before being
           used as values.  Setting NAMED to NAMEDMAX makes sure weird calls
           to replacement functions won't modify constants in
           expressions.  */
        ENSURE_NAMEDMAX(this);
        return this;
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
            {
                xfix_refcnt(m_attrib, node->tail());
                m_attrib = node->tail();
                propagateAge(m_attrib);
            }
        }
        else if (value)
        {
            // Create new node:
            /* The usual convention is that the caller protects,
               but a lot of existing code depends assume that
               setAttrib/installAttrib protects its arguments */
            GCStackRoot<Symbol> namer(name);
            GCStackRoot<> valuer(value);
            if (MAYBE_REFERENCED(value))
                ENSURE_NAMEDMAX(value);
            PairList *newnode = PairList::construct(value, nullptr, name);
            if (prev)
                prev->setTail(newnode);
            else
            { // No preexisting attributes at all:
                xfix_refcnt(m_attrib, newnode);
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
        xfix_refcnt(m_attrib, new_attributes);
#if CXXR_TRUE // temporarily
        m_attrib = new_attributes;
        while (new_attributes)
        {
            Symbol *name = SEXP_downcast<Symbol *>(new_attributes->tag());
            if (name == R_ClassSymbol)
            {
                m_has_class = (new_attributes->car() != nullptr);
                break;
            }
            new_attributes = new_attributes->tail();
        }
        propagateAge(m_attrib);
#else
        // Below code results in installation error for package "vctrs".
        // Error: Can't bind data because some elements are not named.
        // Error: unable to load R code in package ‘vctrs’

        // clearAttributes();
        m_attrib = nullptr;
        m_has_class = false;
        while (new_attributes)
        {
            Symbol *name = SEXP_downcast<Symbol *>(new_attributes->tag());
            setAttribute(name, new_attributes->car());
            new_attributes = new_attributes->tail();
        }
#endif
    }

    void RObject::lockBinding()
    {
        if (!m_active_binding)
        {
            if (m_type == SYMSXP)
                MARK_NOT_MUTABLE(SYMVALUE(this));
            else
                MARK_NOT_MUTABLE(SEXP_downcast<ConsCell *>(this)->car());
        }
        // m_gpbits |= BINDING_LOCK_MASK;
        m_binding_locked = true;
    }

    void RObject::xfix_refcnt(RObject *old, RObject *new_)
    {
        if (!trackrefs())
            return;
        if (old == new_)
            return;

        if (old)
            old->decrementRefCount();
        if (new_)
            new_->incrementRefCount();
    }

    void RObject::xfix_binding_refcnt(RObject *old, RObject *new_)
    {
        if (!trackrefs())
            return;
        if (old == new_)
            return;

        if (old)
        {
            if (assignmentPending())
                setAssignmentPending(false);
            else
                old->decrementRefCount();
        }
        if (new_)
            new_->incrementRefCount();
    }
} // namespace CXXR

// ***** C interface *****

// In CXXR R_NilValue is simply a null pointer:
SEXP R_NilValue = nullptr;

SEXP ATTRIB(SEXP x)
{
    return x ? x->attributes() : nullptr;
}
int OBJECT(SEXP x)
{
    return x && x->hasClass();
}

SEXPTYPE TYPEOF(SEXP x)
{
    if (!x)
        return NILSXP;
    else if (x->altrep())
        return SEXP_downcast<AltRep *>(x)->altsexptype();
    else
        return x->sexptype();
}

int ALTREP(SEXP x)
{
    return x ? x->altrep() : 0;
}

void SETALTREP(SEXP x, int v)
{
}

int NAMED(SEXP x)
{
    return REFCNT(x);
}

int LEVELS(SEXP x)
{
    return x ? x->packGPBits() : 0;
}

int IS_SCALAR(SEXP x, SEXPTYPE type)
{
    return x ? x->isScalarOfType(type) : false;
}

int SIMPLE_SCALAR_TYPE(SEXP x)
{
    return (x && x->isScalar() && ATTRIB(x) == nullptr) ? x->sexptype() : 0;
}

void DECREMENT_REFCNT(SEXP x)
{
    if (x)
        x->decrementRefCount();
}

void INCREMENT_REFCNT(SEXP x)
{
    if (x)
        x->incrementRefCount();
}

void DISABLE_REFCNT(SEXP x)
{
    SET_TRACKREFS(x, false);
}

void ENABLE_REFCNT(SEXP x)
{
    SET_TRACKREFS(x, true);
}

void SET_ATTRIB(SEXP x, SEXP v)
{
    if (!x)
        Rf_error(_("null value provided to SET_ATTRIB"));
    if (v && v->sexptype() != LISTSXP)
        Rf_error(_("value of 'SET_ATTRIB' must be a pairlist or nullptr, not a '%s'"),
                 Rf_type2char(v->sexptype()));
    GCStackRoot<PairList> pl(SEXP_downcast<PairList *>(v));
    x->setAttributes(pl);
}

void SET_OBJECT(SEXP x, int v)
{
}

void SET_TYPEOF(SEXP x, SEXPTYPE v)
{
    if (x)
        x->setSexpType(v);
}

void SET_NAMED(SEXP x, int v)
{
    if (x)
        x->setNamed(v);
}

void SETLEVELS(SEXP x, int v)
{
    if (!x)
        return;
    x->unpackGPBits(v);
    // x->m_gpbits = v;
}

void DUPLICATE_ATTRIB(SEXP to, SEXP from)
{
    SET_ATTRIB(to, Rf_duplicate(ATTRIB(from)));
    if (to && from)
        to->setS4Object(from->isS4Object());
}

void SHALLOW_DUPLICATE_ATTRIB(SEXP to, SEXP from)
{
    SET_ATTRIB(to, Rf_shallow_duplicate(ATTRIB(from)));
    if (to && from)
        to->setS4Object(from->isS4Object());
}

int ASSIGNMENT_PENDING(SEXP x)
{
    if (!x)
        return 0;
    return x->assignmentPending();
}

void SET_ASSIGNMENT_PENDING(SEXP x, int v)
{
    if (x)
        x->setAssignmentPending(v);
}

void(MARK_NOT_MUTABLE)(SEXP x)
{
    MARK_NOT_MUTABLE(x);
}

int IS_ASSIGNMENT_CALL(SEXP x)
{
    return IS_ASSIGNMENT_CALL_MACRO(x);
}

void MARK_ASSIGNMENT_CALL(SEXP x)
{
    MARK_ASSIGNMENT_CALL_MACRO(x);
}

void ENSURE_NAMEDMAX(SEXP x)
{
    ENSURE_NAMEDMAX_MACRO(x);
}

void ENSURE_NAMED(SEXP x)
{
    ENSURE_NAMED_MACRO(x);
}

void SETTER_CLEAR_NAMED(SEXP x)
{
    SETTER_CLEAR_NAMED_MACRO(x);
}

void RAISE_NAMED(SEXP x, int n)
{
    RAISE_NAMED_MACRO(x, n);
}

Rboolean Rf_isNull(SEXP s)
{
    return Rboolean(TYPEOF(s) == NILSXP);
}

Rboolean Rf_isSymbol(SEXP s)
{
    return Rboolean(TYPEOF(s) == SYMSXP);
}

Rboolean Rf_isLogical(SEXP s)
{
    return Rboolean(TYPEOF(s) == LGLSXP);
}

Rboolean Rf_isReal(SEXP s)
{
    return Rboolean(TYPEOF(s) == REALSXP);
}

Rboolean Rf_isComplex(SEXP s)
{
    return Rboolean(TYPEOF(s) == CPLXSXP);
}

Rboolean Rf_isExpression(SEXP s)
{
    return Rboolean(TYPEOF(s) == EXPRSXP);
}

Rboolean Rf_isEnvironment(SEXP s)
{
    return Rboolean(TYPEOF(s) == ENVSXP);
}

Rboolean Rf_isString(SEXP s)
{
    return Rboolean(TYPEOF(s) == STRSXP);
}

Rboolean Rf_isObject(SEXP s)
{
    return Rboolean(OBJECT(s) != 0);
}

Rboolean Rf_isRaw(SEXP s)
{
    return Rboolean(TYPEOF(s) == RAWSXP);
}

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
        while (s)
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
        while (s)
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
    if (y && MAYBE_REFERENCED(y))
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

Rboolean Rf_conformable(SEXP x_, SEXP y)
{
    int n;
    GCStackRoot<> x(Rf_getAttrib(x_, R_DimSymbol));
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
