/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999--2020  The R Core Team.
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *
 *  This header file is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This file is part of R. R is distributed under the terms of the
 *  GNU General Public License, either Version 2, June 1991 or Version 3,
 *  June 2007. See doc/COPYRIGHTS for details of the copyright status of R.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

#include <RObject.hpp>

namespace R
{

    /* General Cons Cell Attributes */
    bool RObject::gcgen(RObject *v) { return v && v->m_gcgen; }

    void RObject::set_gcgen(RObject *v, bool x)
    {
        if (!v)
            return;
        v->m_gcgen = x;
    }
    unsigned int RObject::gccls(RObject *v) { return v ? v->m_gccls : 0; }

    void RObject::set_gccls(RObject *v, unsigned int x)
    {
        if (!v)
            return;
        v->m_gccls = x;
    }

    RObject *RObject::next_node(RObject *s) { return s ? s->gengc_next_node : nullptr; }

    RObject *RObject::prev_node(RObject *s) { return s ? s->gengc_prev_node : nullptr; }

    void RObject::set_next_node(RObject *s, RObject *t)
    {
        if (!s)
            return;
        s->gengc_next_node = t;
    }

    void RObject::set_prev_node(RObject *s, RObject *t)
    {
        if (!s)
            return;
        s->gengc_prev_node = t;
    }

    void RObject::copy_sxpinfo(RObject *x, RObject &y)
    {
        if (!x)
            return;

        x->m_type = y.m_type;
        x->m_scalar = y.m_scalar;
        x->m_has_class = y.m_has_class;
        x->m_alt = y.m_alt;
        x->m_gpbits = y.m_gpbits;
        x->m_marked = y.m_marked;
        x->m_debug = y.m_debug;
        x->m_trace = y.m_trace;
        x->m_spare = y.m_spare;
        x->m_gcgen = y.m_gcgen;
        x->m_gccls = y.m_gccls;
        x->m_named = y.m_named;
        x->m_extra = y.m_extra;
    }

    void RObject::set_ready_to_finalize(RObject *s)
    {
        if (!s)
            return;
        s->m_gpbits |= READY_TO_FINALIZE_MASK;
    }

    void RObject::clear_ready_to_finalize(RObject *s)
    {
        if (!s)
            return;
        s->m_gpbits &= ~READY_TO_FINALIZE_MASK;
    }

    unsigned int RObject::is_ready_to_finalize(RObject *s) { return s ? s->m_gpbits & READY_TO_FINALIZE_MASK : 0; }

    void RObject::set_finalize_on_exit(RObject *s)
    {
        if (!s)
            return;
        s->m_gpbits |= FINALIZE_ON_EXIT_MASK;
    }

    void RObject::clear_finalize_on_exit(RObject *s)
    {
        if (!s)
            return;
        s->m_gpbits &= ~FINALIZE_ON_EXIT_MASK;
    }

    unsigned int RObject::finalize_on_exit(RObject *s) { return s ? (s->m_gpbits & FINALIZE_ON_EXIT_MASK) : 0; }

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

    bool RObject::mark(RObject *x) { return x && x->m_marked; }

    void RObject::set_mark(RObject *x, int v)
    {
        if (!x)
            return;
        x->m_marked = v;
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

    void RObject::set_base_sym_cached(RObject *b)
    {
        if (!b)
            return;
        b->m_gpbits |= BASE_SYM_CACHED_MASK;
    }
    void RObject::unset_base_sym_cached(RObject *b)
    {
        if (!b)
            return;
        b->m_gpbits &= (~BASE_SYM_CACHED_MASK);
    }

    unsigned int RObject::base_sym_cached(RObject *b) { return b ? (b->m_gpbits & BASE_SYM_CACHED_MASK) : 0; }

    unsigned int RObject::no_special_symbols(RObject *b) { return b ? (b->m_gpbits & SPECIAL_SYMBOL_MASK) : 0; }

    void RObject::set_no_special_symbols(RObject *b)
    {
        if (!b)
            return;
        b->m_gpbits |= SPECIAL_SYMBOL_MASK;
    }

    unsigned int RObject::is_special_symbol(RObject *b) { return b ? (b->m_gpbits & SPECIAL_SYMBOL_MASK) : 0; }

    void RObject::set_special_symbol(RObject *b)
    {
        if (!b)
            return;
        b->m_gpbits |= SPECIAL_SYMBOL_MASK;
    }

    void RObject::unset_no_special_symbols(RObject *b)
    {
        if (!b)
            return;
        b->m_gpbits &= (~SPECIAL_SYMBOL_MASK);
    }

    void RObject::unset_special_symbol(RObject *b)
    {
        if (!b)
            return;
        b->m_gpbits &= (~SPECIAL_SYMBOL_MASK);
    }

    unsigned int RObject::is_active_binding(RObject *b) { return b ? (b->m_gpbits & ACTIVE_BINDING_MASK) : 0; }

    unsigned int RObject::binding_is_locked(RObject *b) { return b ? (b->m_gpbits & BINDING_LOCK_MASK) : 0; }

    void RObject::lock_binding(RObject *b)
    {
        if (!RObject::is_active_binding(b))
        {
            if (RObject::typeof_(b) == SYMSXP)
                MARK_NOT_MUTABLE(RObject::symvalue(b));
            else
                MARK_NOT_MUTABLE(RObject::car0(b));
        }
        ((b))->m_gpbits |= BINDING_LOCK_MASK;
    }

    void RObject::unlock_binding(RObject *b)
    {
        if (!b)
            return;
        b->m_gpbits &= (~BINDING_LOCK_MASK);
    }

    void RObject::set_active_binding_bit(RObject *b)
    {
        if (!b)
            return;
        b->m_gpbits |= ACTIVE_BINDING_MASK;
    }

    double RObject::bndcell_dval(RObject *v) { return v ? ((R_bndval_t *)&(v->u.listsxp.carval))->dval : 0; }

    int RObject::bndcell_ival(RObject *v) { return v ? ((R_bndval_t *)&(v->u.listsxp.carval))->ival : 0; }

    int RObject::bndcell_lval(RObject *v) { return v ? ((R_bndval_t *)&(v->u.listsxp.carval))->ival : 0; }

    void RObject::set_bndcell_dval(RObject *v, double x)
    {
        if (!v)
            return;
        ((R_bndval_t *)&(v->u.listsxp.carval))->dval = x;
    }

    void RObject::set_bndcell_ival(RObject *v, int x)
    {
        if (!v)
            return;
        ((R_bndval_t *)&(v->u.listsxp.carval))->ival = x;
    }

    void RObject::set_bndcell_lval(RObject *v, int x)
    {
        if (!v)
            return;
        ((R_bndval_t *)&(v->u.listsxp.carval))->ival = x;
    }

} // namespace R
