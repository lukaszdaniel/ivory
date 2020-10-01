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
#include <R_ext/Boolean.h>
#include <Rinternals.h>

namespace R
{
    // Force the creation of non-inline embodiments of functions callable
    // from C:
    namespace ForceNonInline
    {
        const auto &DUPLICATE_ATTRIBptr = DUPLICATE_ATTRIB;
        const auto &SHALLOW_DUPLICATE_ATTRIBptr = SHALLOW_DUPLICATE_ATTRIB;
        const auto &isNullptr = Rf_isNull;
        const auto &isObjectptr = Rf_isObject;
        const auto &IS_S4_OBJECTptr = IS_S4_OBJECT;
        const auto &NAMEDptr = NAMED;
        const auto &OBJECTptr = OBJECT;
        const auto &SET_NAMEDptr = SET_NAMED;
        const auto &ENSURE_NAMEDMAXptr = ENSURE_NAMEDMAX;
        const auto &ENSURE_NAMEDptr = ENSURE_NAMED;
        const auto &SETTER_CLEAR_NAMEDptr = SETTER_CLEAR_NAMED;
        const auto &RAISE_NAMEDptr = RAISE_NAMED;
        const auto &SET_S4_OBJECTptr = SET_S4_OBJECT;
        const auto &TYPEOFptr = TYPEOF;
        const auto &UNSET_S4_OBJECTptr = UNSET_S4_OBJECT;
        const auto &LEVELSptr = LEVELS;
        const auto &SETLEVELSptr = SETLEVELS;
        const auto &ALTREPptr = ALTREP;
        const auto &SETALTREPptr = SETALTREP;
    } // namespace ForceNonInline

    RObject::~RObject()
    {
        if (m_data)
            MemoryBank::deallocate(m_data, m_databytes);
    }

    void RObject::set_ready_to_finalize(RObject *x)
    {
        if (!x)
            return;
        x->m_gpbits |= READY_TO_FINALIZE_MASK;
    }

    void RObject::clear_ready_to_finalize(RObject *x)
    {
        if (!x)
            return;
        x->m_gpbits &= ~READY_TO_FINALIZE_MASK;
    }

    unsigned int RObject::is_ready_to_finalize(RObject *x) { return x ? x->m_gpbits & READY_TO_FINALIZE_MASK : 0; }

    void RObject::set_finalize_on_exit(RObject *x)
    {
        if (!x)
            return;
        x->m_gpbits |= FINALIZE_ON_EXIT_MASK;
    }

    void RObject::clear_finalize_on_exit(RObject *x)
    {
        if (!x)
            return;
        x->m_gpbits &= ~FINALIZE_ON_EXIT_MASK;
    }

    unsigned int RObject::finalize_on_exit(RObject *x) { return x ? (x->m_gpbits & FINALIZE_ON_EXIT_MASK) : 0; }

    void RObject::set_attrib(RObject *x, RObject *v)
    {
        if (!x)
            return;
        x->m_attrib = v;
    }

    RObject *RObject::attrib(RObject *x) { return x ? x->m_attrib : nullptr; }

    unsigned int RObject::named(RObject *x) { return x ? x->m_named : 0; }

    void RObject::set_named(RObject *x, unsigned int v)
    {
        if (!x)
            return;
        x->m_named = v;
    }

    void RObject::set_typeof(RObject *x, SEXPTYPE v)
    {
        if (!x)
            return;
        x->m_type = v;
    }

    SEXPTYPE RObject::typeof_(RObject *x) { return x ? x->m_type : NILSXP; }

    unsigned int RObject::levels(RObject *x) { return x ? x->m_gpbits : 0; }

    bool RObject::object(RObject *x) { return x && x->m_has_class; }

    void RObject::set_object(RObject *x, bool v)
    {
        if (!x)
            return;
        x->m_has_class = v;
    }

    bool RObject::scalar(RObject *x) { return x && x->m_scalar; }

    unsigned int RObject::is_bytes(RObject *x) { return x ? (x->m_gpbits & BYTES_MASK) : 0; }

    void RObject::set_bytes(RObject *x)
    {
        if (!x)
            return;
        x->m_gpbits |= BYTES_MASK;
    }

    unsigned int RObject::is_latin1(RObject *x) { return x ? (x->m_gpbits & LATIN1_MASK) : 0; }

    void RObject::set_latin1(RObject *x)
    {
        if (!x)
            return;
        x->m_gpbits |= LATIN1_MASK;
    }

    unsigned int RObject::is_ascii(RObject *x) { return x ? (x->m_gpbits & ASCII_MASK) : 0; }

    void RObject::set_ascii(RObject *x)
    {
        if (!x)
            return;
        x->m_gpbits |= ASCII_MASK;
    }

    unsigned int RObject::is_utf8(RObject *x) { return x ? (x->m_gpbits & UTF8_MASK) : 0; }

    void RObject::set_utf8(RObject *x)
    {
        if (!x)
            return;
        x->m_gpbits |= UTF8_MASK;
    }

    unsigned int RObject::enc_known(RObject *x) { return x ? (x->m_gpbits & (LATIN1_MASK | UTF8_MASK)) : 0; }

    void RObject::set_cached(RObject *x)
    {
        if (!x)
            return;
        x->m_gpbits |= CACHED_MASK;
    }

    unsigned int RObject::is_cached(RObject *x) { return x ? (x->m_gpbits & CACHED_MASK) : 0; }

    bool RObject::altrep(RObject *x) { return x && x->m_alt; }

    void RObject::set_altrep(RObject *x, bool v)
    {
        if (!x)
            return;
        x->m_alt = v;
    }

    void RObject::setlevels(RObject *x, unsigned short int v)
    {
        if (!x)
            return;
        x->m_gpbits = (unsigned short)v;
    }

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

    bool RObject::rtrace(RObject *x) { return x && x->m_trace; }

    void RObject::set_rtrace(RObject *x, bool v)
    {
        if (!x)
            return;
        x->m_trace = v;
    }

    unsigned int RObject::missing(RObject *x) { return x ? (x->m_gpbits & MISSING_MASK) : 0; } /* for closure calls */

    void RObject::set_missing(RObject *x, int v)
    {
        if (!x)
            return;
        int __other_flags__ = x->m_gpbits & ~MISSING_MASK;
        x->m_gpbits = __other_flags__ | v;
    }

    unsigned int RObject::bndcell_tag(RObject *e) { return e ? e->m_extra : 0; }

    void RObject::set_bndcell_tag(RObject *e, unsigned int v)
    {
        if (!e)
            return;
        e->m_extra = v;
    }

    /* External pointer access methods */
    RObject *RObject::extptr_prot(RObject *x) { return RObject::cdr(x); }

    RObject *RObject::extptr_tag(RObject *x) { return RObject::tag(x); }

    void RObject::set_extptr_tag(RObject *x, RObject *v) { RObject::set_tag(x, v); }

    void RObject::set_extptr_prot(RObject *x, RObject *v) { RObject::set_cdr(x, v); }

    /* S4 object bit, set by R_do_new_object for all new() calls */
    bool RObject::is_s4_object(RObject *x) { return x && (x->m_gpbits & S4_OBJECT_MASK); }

    void RObject::set_s4_object(RObject *x)
    {
        if (!x)
            return;
        x->m_gpbits |= S4_OBJECT_MASK;
    }

    void RObject::unset_s4_object(RObject *x)
    {
        if (!x)
            return;
        x->m_gpbits &= ~S4_OBJECT_MASK;
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

    /* Hashing Methods */
    unsigned int RObject::hashash(RObject *x) { return x ? (x->m_gpbits & HASHASH_MASK) : 0; }

    void RObject::set_hashash(RObject *x, bool v)
    {
        if (v)
        {
            (x->m_gpbits |= HASHASH_MASK);
        }
        else
        {
            (x->m_gpbits &= (~HASHASH_MASK));
        }
    }

    void RObject::set_base_sym_cached(RObject *x)
    {
        if (!x)
            return;
        x->m_gpbits |= BASE_SYM_CACHED_MASK;
    }
    void RObject::unset_base_sym_cached(RObject *x)
    {
        if (!x)
            return;
        x->m_gpbits &= (~BASE_SYM_CACHED_MASK);
    }

    unsigned int RObject::base_sym_cached(RObject *x) { return x ? (x->m_gpbits & BASE_SYM_CACHED_MASK) : 0; }

    unsigned int RObject::no_special_symbols(RObject *x) { return x ? (x->m_gpbits & SPECIAL_SYMBOL_MASK) : 0; }

    void RObject::set_no_special_symbols(RObject *x)
    {
        if (!x)
            return;
        x->m_gpbits |= SPECIAL_SYMBOL_MASK;
    }

    unsigned int RObject::is_special_symbol(RObject *x) { return x ? (x->m_gpbits & SPECIAL_SYMBOL_MASK) : 0; }

    void RObject::set_special_symbol(RObject *x)
    {
        if (!x)
            return;
        x->m_gpbits |= SPECIAL_SYMBOL_MASK;
    }

    void RObject::unset_no_special_symbols(RObject *x)
    {
        if (!x)
            return;
        x->m_gpbits &= (~SPECIAL_SYMBOL_MASK);
    }

    void RObject::unset_special_symbol(RObject *x)
    {
        if (!x)
            return;
        x->m_gpbits &= (~SPECIAL_SYMBOL_MASK);
    }

    unsigned int RObject::is_active_binding(RObject *x) { return x ? (x->m_gpbits & ACTIVE_BINDING_MASK) : 0; }

    unsigned int RObject::binding_is_locked(RObject *x) { return x ? (x->m_gpbits & BINDING_LOCK_MASK) : 0; }

    void RObject::lock_binding(RObject *x)
    {
        if (!RObject::is_active_binding(x))
        {
            if (RObject::typeof_(x) == SYMSXP)
                MARK_NOT_MUTABLE(RObject::symvalue(x));
            else
                MARK_NOT_MUTABLE(RObject::car0(x));
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

    double RObject::bndcell_dval(RObject *x) { return x ? ((R_bndval_t *)&(x->u.listsxp.m_carval))->dval : 0; }

    int RObject::bndcell_ival(RObject *x) { return x ? ((R_bndval_t *)&(x->u.listsxp.m_carval))->ival : 0; }

    int RObject::bndcell_lval(RObject *x) { return x ? ((R_bndval_t *)&(x->u.listsxp.m_carval))->ival : 0; }

    void RObject::set_bndcell_dval(RObject *x, double v)
    {
        if (!x)
            return;
        ((R_bndval_t *)&(x->u.listsxp.m_carval))->dval = v;
    }

    void RObject::set_bndcell_ival(RObject *x, int v)
    {
        if (!x)
            return;
        ((R_bndval_t *)&(x->u.listsxp.m_carval))->ival = v;
    }

    void RObject::set_bndcell_lval(RObject *x, int v)
    {
        if (!x)
            return;
        ((R_bndval_t *)&(x->u.listsxp.m_carval))->ival = v;
    }

} // namespace R
