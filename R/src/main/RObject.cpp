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
    bool RObject::gcgen(SEXP v) { return v && v->sxpinfo.m_gcgen; }

    void RObject::set_gcgen(SEXP v, bool x)
    {
        if (!v)
            return;
        v->sxpinfo.m_gcgen = x;
    }
    unsigned int RObject::gccls(SEXP v) { return v ? v->sxpinfo.m_gccls : 0; }

    void RObject::set_gccls(SEXP v, unsigned int x)
    {
        if (!v)
            return;
        v->sxpinfo.m_gccls = x;
    }

    RObject *RObject::next_node(SEXP s) { return s ? s->gengc_next_node : nullptr; }

    RObject *RObject::prev_node(SEXP s) { return s ? s->gengc_prev_node : nullptr; }

    void RObject::set_next_node(SEXP s, SEXP t)
    {
        if (!s)
            return;
        s->gengc_next_node = t;
    }

    void RObject::set_prev_node(SEXP s, SEXP t)
    {
        if (!s)
            return;
        s->gengc_prev_node = t;
    }

    void RObject::copy_sxpinfo(SEXP x, RObject &y)
    {
        if (!x)
            return;
        x->sxpinfo = y.sxpinfo;
    }

    void RObject::set_ready_to_finalize(SEXP s)
    {
        if (!s)
            return;
        s->sxpinfo.m_gp |= READY_TO_FINALIZE_MASK;
    }

    void RObject::clear_ready_to_finalize(SEXP s)
    {
        if (!s)
            return;
        s->sxpinfo.m_gp &= ~READY_TO_FINALIZE_MASK;
    }

    unsigned int RObject::is_ready_to_finalize(SEXP s) { return s ? s->sxpinfo.m_gp & READY_TO_FINALIZE_MASK : 0; }

    void RObject::set_finalize_on_exit(SEXP s)
    {
        if (!s)
            return;
        s->sxpinfo.m_gp |= FINALIZE_ON_EXIT_MASK;
    }

    void RObject::clear_finalize_on_exit(SEXP s)
    {
        if (!s)
            return;
        s->sxpinfo.m_gp &= ~FINALIZE_ON_EXIT_MASK;
    }

    unsigned int RObject::finalize_on_exit(SEXP s) { return s ? (s->sxpinfo.m_gp & FINALIZE_ON_EXIT_MASK) : 0; }

    void RObject::set_attrib(SEXP x, SEXP v)
    {
        if (!x)
            return;
        x->m_attrib = v;
    }

    RObject *RObject::attrib(SEXP x) { return x ? x->m_attrib : nullptr; }

    unsigned int RObject::named(SEXP x) { return x ? x->sxpinfo.m_named : 0; }

    void RObject::set_named(SEXP x, unsigned int v)
    {
        if (!x)
            return;
        x->sxpinfo.m_named = v;
    }

    void RObject::set_typeof(SEXP x, SEXPTYPE v)
    {
        if (!x)
            return;
        x->sxpinfo.m_type = v;
    }

    SEXPTYPE RObject::typeof_(SEXP x) { return x ? x->sxpinfo.m_type : NILSXP; }

    unsigned int RObject::levels(SEXP x) { return x ? x->sxpinfo.m_gp : 0; }

    bool RObject::object(SEXP x) { return x && x->sxpinfo.m_obj; }

    void RObject::set_object(SEXP x, bool v)
    {
        if (!x)
            return;
        x->sxpinfo.m_obj = v;
    }

    bool RObject::mark(SEXP x) { return x && x->sxpinfo.m_mark; }

    void RObject::set_mark(SEXP x, int v)
    {
        if (!x)
            return;
        x->sxpinfo.m_mark = v;
    }

    bool RObject::scalar(SEXP x) { return x && x->sxpinfo.m_scalar; }

    unsigned int RObject::is_bytes(SEXP x) { return x ? (x->sxpinfo.m_gp & BYTES_MASK) : 0; }

    void RObject::set_bytes(SEXP x)
    {
        if (!x)
            return;
        x->sxpinfo.m_gp |= BYTES_MASK;
    }

    unsigned int RObject::is_latin1(SEXP x) { return x ? (x->sxpinfo.m_gp & LATIN1_MASK) : 0; }

    void RObject::set_latin1(SEXP x)
    {
        if (!x)
            return;
        x->sxpinfo.m_gp |= LATIN1_MASK;
    }

    unsigned int RObject::is_ascii(SEXP x) { return x ? (x->sxpinfo.m_gp & ASCII_MASK) : 0; }

    void RObject::set_ascii(SEXP x)
    {
        if (!x)
            return;
        x->sxpinfo.m_gp |= ASCII_MASK;
    }

    unsigned int RObject::is_utf8(SEXP x) { return x ? (x->sxpinfo.m_gp & UTF8_MASK) : 0; }

    void RObject::set_utf8(SEXP x)
    {
        if (!x)
            return;
        x->sxpinfo.m_gp |= UTF8_MASK;
    }

    unsigned int RObject::enc_known(SEXP x) { return x ? (x->sxpinfo.m_gp & (LATIN1_MASK | UTF8_MASK)) : 0; }

    void RObject::set_cached(SEXP x)
    {
        if (!x)
            return;
        x->sxpinfo.m_gp |= CACHED_MASK;
    }

    unsigned int RObject::is_cached(SEXP x) { return x ? (x->sxpinfo.m_gp & CACHED_MASK) : 0; }

    bool RObject::altrep(SEXP x) { return x && x->sxpinfo.m_alt; }

    void RObject::set_altrep(SEXP x, bool v)
    {
        if (!x)
            return;
        x->sxpinfo.m_alt = v;
    }

    void RObject::setlevels(SEXP x, unsigned short int v)
    {
        if (!x)
            return;
        x->sxpinfo.m_gp = (unsigned short)v;
    }

    void RObject::setscalar(SEXP x, bool v)
    {
        if (!x)
            return;
        x->sxpinfo.m_scalar = v;
    }

    bool RObject::is_scalar(SEXP x, SEXPTYPE t) { return x && (x->sxpinfo.m_type == t) && x->sxpinfo.m_scalar; }

    unsigned int RObject::refcnt(SEXP x) { return x ? x->sxpinfo.m_named : 0; }

    void RObject::set_refcnt(SEXP x, unsigned int v)
    {
        if (!x)
            return;
        x->sxpinfo.m_named = v;
    }

    bool RObject::trackrefs(SEXP x) { return x && (typeof_(x) == CLOSXP ? TRUE : !x->sxpinfo.m_spare); }

    void RObject::set_trackrefs(SEXP x, bool v)
    {
        if (!x)
            return;
        x->sxpinfo.m_spare = v;
    }

    unsigned int RObject::assignment_pending(SEXP x) { return x ? (x->sxpinfo.m_gp & ASSIGNMENT_PENDING_MASK) : 0; }

    void RObject::set_assignment_pending(SEXP x, bool v)
    {
        if (!x)
            return;
        if (v)
            (((x)->sxpinfo.m_gp) |= ASSIGNMENT_PENDING_MASK);
        else
            (((x)->sxpinfo.m_gp) &= ~ASSIGNMENT_PENDING_MASK);
    }

    bool RObject::rtrace(SEXP x) { return x && x->sxpinfo.m_trace; }

    void RObject::set_rtrace(SEXP x, bool v)
    {
        if (!x)
            return;
        x->sxpinfo.m_trace = v;
    }

    /* List Access Methods */
    RObject *RObject::tag(SEXP e) { return e ? e->u.listsxp.tagval : nullptr; }

    void RObject::set_tag(SEXP x, SEXP v)
    {
        if (!x)
            return;
        x->u.listsxp.tagval = v;
    }

    RObject *RObject::car0(SEXP e) { return e ? e->u.listsxp.carval : nullptr; }

    void RObject::set_car0(SEXP x, SEXP v)
    {
        if (!x)
            return;
        x->u.listsxp.carval = v;
    }

    RObject *RObject::extptr_ptr(SEXP e) { return e ? e->u.listsxp.carval : nullptr; }

    void RObject::set_extptr_ptr(SEXP x, SEXP v)
    {
        if (!x)
            return;
        x->u.listsxp.carval = v;
    }

    RObject *RObject::cdr(SEXP e) { return e ? e->u.listsxp.cdrval : nullptr; }

    void RObject::set_cdr(SEXP x, SEXP v)
    {
        if (!x)
            return;
        x->u.listsxp.cdrval = v;
    }

    unsigned int RObject::missing(SEXP x) { return x ? (x->sxpinfo.m_gp & MISSING_MASK) : 0; } /* for closure calls */

    void RObject::set_missing(SEXP x, int v)
    {
        if (!x)
            return;
        int __other_flags__ = x->sxpinfo.m_gp & ~MISSING_MASK;
        x->sxpinfo.m_gp = __other_flags__ | v;
    }

    unsigned int RObject::bndcell_tag(SEXP e) { return e ? e->sxpinfo.m_extra : 0; }

    void RObject::set_bndcell_tag(SEXP e, unsigned int v)
    {
        if (!e)
            return;
        e->sxpinfo.m_extra = v;
    }

    /* External pointer access methods */
    RObject *RObject::extptr_prot(SEXP x) { return RObject::cdr(x); }

    RObject *RObject::extptr_tag(SEXP x) { return RObject::tag(x); }

    void RObject::set_extptr_tag(SEXP x, SEXP v) { RObject::set_tag(x, v); }

    void RObject::set_extptr_prot(SEXP x, SEXP v) { RObject::set_cdr(x, v); }

    /* S4 object bit, set by R_do_new_object for all new() calls */
    bool RObject::is_s4_object(SEXP x) { return x && (x->sxpinfo.m_gp & S4_OBJECT_MASK); }

    void RObject::set_s4_object(SEXP x)
    {
        if (!x)
            return;
        x->sxpinfo.m_gp |= S4_OBJECT_MASK;
    }

    void RObject::unset_s4_object(SEXP x)
    {
        if (!x)
            return;
        x->sxpinfo.m_gp &= ~S4_OBJECT_MASK;
    }

    /* JIT optimization support */

    unsigned int RObject::nojit(SEXP x) { return x ? (x->sxpinfo.m_gp & NOJIT_MASK) : 0; }

    void RObject::set_nojit(SEXP x)
    {
        if (!x)
            return;
        x->sxpinfo.m_gp |= NOJIT_MASK;
    }

    unsigned int RObject::maybejit(SEXP x) { return x ? (x->sxpinfo.m_gp & MAYBEJIT_MASK) : 0; }

    void RObject::set_maybejit(SEXP x)
    {
        if (!x)
            return;
        x->sxpinfo.m_gp |= MAYBEJIT_MASK;
    }

    void RObject::unset_maybejit(SEXP x)
    {
        if (!x)
            return;
        x->sxpinfo.m_gp &= ~MAYBEJIT_MASK;
    }

    /* Growable vector support */
    unsigned int RObject::growable_bit_set(SEXP x) { return x ? (x->sxpinfo.m_gp & GROWABLE_MASK) : 0; }

    void RObject::set_growable_bit(SEXP x)
    {
        if (!x)
            return;
        x->sxpinfo.m_gp |= GROWABLE_MASK;
    }

    /* Hashing Methods */
    unsigned int RObject::hashash(SEXP x) { return x ? (x->sxpinfo.m_gp & HASHASH_MASK) : 0; }

    void RObject::set_hashash(SEXP x, bool v)
    {
        if (v)
        {
            (x->sxpinfo.m_gp |= HASHASH_MASK);
        }
        else
        {
            (x->sxpinfo.m_gp &= (~HASHASH_MASK));
        }
    }

    void RObject::set_base_sym_cached(SEXP b)
    {
        if (!b)
            return;
        b->sxpinfo.m_gp |= BASE_SYM_CACHED_MASK;
    }
    void RObject::unset_base_sym_cached(SEXP b)
    {
        if (!b)
            return;
        b->sxpinfo.m_gp &= (~BASE_SYM_CACHED_MASK);
    }

    unsigned int RObject::base_sym_cached(SEXP b) { return b ? (b->sxpinfo.m_gp & BASE_SYM_CACHED_MASK) : 0; }

    unsigned int RObject::no_special_symbols(SEXP b) { return b ? (b->sxpinfo.m_gp & SPECIAL_SYMBOL_MASK) : 0; }

    void RObject::set_no_special_symbols(SEXP b)
    {
        if (!b)
            return;
        b->sxpinfo.m_gp |= SPECIAL_SYMBOL_MASK;
    }

    unsigned int RObject::is_special_symbol(SEXP b) { return b ? (b->sxpinfo.m_gp & SPECIAL_SYMBOL_MASK) : 0; }

    void RObject::set_special_symbol(SEXP b)
    {
        if (!b)
            return;
        b->sxpinfo.m_gp |= SPECIAL_SYMBOL_MASK;
    }

    void RObject::unset_no_special_symbols(SEXP b)
    {
        if (!b)
            return;
        b->sxpinfo.m_gp &= (~SPECIAL_SYMBOL_MASK);
    }

    void RObject::unset_special_symbol(SEXP b)
    {
        if (!b)
            return;
        b->sxpinfo.m_gp &= (~SPECIAL_SYMBOL_MASK);
    }

    unsigned int RObject::is_active_binding(SEXP b) { return b ? (b->sxpinfo.m_gp & ACTIVE_BINDING_MASK) : 0; }

    unsigned int RObject::binding_is_locked(SEXP b) { return b ? (b->sxpinfo.m_gp & BINDING_LOCK_MASK) : 0; }

    void RObject::lock_binding(SEXP b)
    {
        if (!RObject::is_active_binding(b))
        {
            if (RObject::typeof_(b) == SYMSXP)
                MARK_NOT_MUTABLE(RObject::symvalue(b));
            else
                MARK_NOT_MUTABLE(RObject::car0(b));
        }
        ((b))->sxpinfo.m_gp |= BINDING_LOCK_MASK;
    }

    void RObject::unlock_binding(SEXP b)
    {
        if (!b)
            return;
        b->sxpinfo.m_gp &= (~BINDING_LOCK_MASK);
    }

    void RObject::set_active_binding_bit(SEXP b)
    {
        if (!b)
            return;
        b->sxpinfo.m_gp |= ACTIVE_BINDING_MASK;
    }

    double RObject::bndcell_dval(SEXP v) { return v ? ((R_bndval_t *)&(v->u.listsxp.carval))->dval : 0; }

    int RObject::bndcell_ival(SEXP v) { return v ? ((R_bndval_t *)&(v->u.listsxp.carval))->ival : 0; }

    int RObject::bndcell_lval(SEXP v) { return v ? ((R_bndval_t *)&(v->u.listsxp.carval))->ival : 0; }

    void RObject::set_bndcell_dval(SEXP v, double x)
    {
        if (!v)
            return;
        ((R_bndval_t *)&(v->u.listsxp.carval))->dval = x;
    }

    void RObject::set_bndcell_ival(SEXP v, int x)
    {
        if (!v)
            return;
        ((R_bndval_t *)&(v->u.listsxp.carval))->ival = x;
    }

    void RObject::set_bndcell_lval(SEXP v, int x)
    {
        if (!v)
            return;
        ((R_bndval_t *)&(v->u.listsxp.carval))->ival = x;
    }

} // namespace R
