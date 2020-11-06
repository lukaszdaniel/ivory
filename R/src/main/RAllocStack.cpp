/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
 *
 *  Rho is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the Rho website.
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

/** @file RAllocStack.cpp
 *
 * Implementation of class RAllocStack and related functions.
 */

#include <CXXR/RAllocStack.hpp>
#include <CXXR/RTypes.hpp>
#include <cstring>
#include <stdexcept>
#include <R_ext/Error.h>
#include <Localization.h>
#include <CXXR/MemoryBank.hpp>
#include <Rmath.h>
#ifdef HAVE_STDALIGN_H
#include <stdalign.h>
#endif

#include <cstdint>

using namespace std;
using namespace R;

// Force the creation of non-inline embodiments of functions callable
// from C:
namespace R
{
    // Force the creation of non-inline embodiments of functions callable
    // from C:
    namespace ForceNonInline
    {
        const auto &vmaxgetp = vmaxget;
        const auto &vmaxsetp = vmaxset;
    } // namespace ForceNonInline
} // namespace R

RAllocStack::Stack *RAllocStack::s_stack = nullptr;
RAllocStack::Scope *RAllocStack::s_innermost_scope = 0;

void *RAllocStack::allocate(size_t sz)
{
    Pair pr(sz, MemoryBank::allocate(sz));
    s_stack->push(pr);
    return s_stack->top().second;
}

void RAllocStack::initialize()
{
    s_stack = new Stack();
}

void RAllocStack::restoreSize(size_t new_size)
{
    if (new_size > s_stack->size())
        throw out_of_range("RAllocStack::restoreSize: requested size greater than current size.");
#ifndef NDEBUG
    if (s_innermost_scope && new_size < s_innermost_scope->startSize())
        throw out_of_range("RAllocStack::restoreSize: requested size too small for current scope.");
#endif
    trim(new_size);
}

void RAllocStack::trim(size_t new_size)
{
    while (s_stack->size() > new_size)
    {
        Pair &top = s_stack->top();
        MemoryBank::deallocate(top.second, top.first);
        s_stack->pop();
    }
}

// ***** C interface *****

char *R_alloc(size_t num_elts, int elt_size)
{
    R_size_t size = num_elts * elt_size;
    /* doubles are a precaution against integer overflow on 32-bit */
    double dsize = (double)num_elts * elt_size;
    if (dsize > 0)
    {
#ifdef LONG_VECTOR_SUPPORT
        /* 64-bit platform: previous version used REALSXPs */
        if (dsize > (double)R_XLEN_T_MAX) /* currently 4096 TB */
            error(_("cannot allocate memory block of size %0.f TB"),
                  dsize / R_pow_di(1024.0, 4));
#else
        if (dsize > R_LEN_T_MAX) /* must be in the Gb range */
            error(_("cannot allocate memory block of size %0.1f GB"),
                  dsize / R_pow_di(1024.0, 3));
#endif
        return static_cast<char *>(RAllocStack::allocate(size));
    }
    /* One programmer has relied on this, but it is undocumented! */
    else
        return nullptr;
}

long double *R_allocLD(size_t num_elts)
{
#if __cplusplus || __alignof_is_defined
    // This is C11: picky compilers may warn.
    size_t ld_align = alignof(long double);
#elif __GNUC__
    // This is C99, but do not rely on it.
    size_t ld_align = offsetof(
        struct { char __a; long double __b; }, __b);
#else
    size_t ld_align = 0x0F; // value of x86_64, known others are 4 or 8
#endif
    if (ld_align > 8)
    {
        uintptr_t tmp = (uintptr_t)R_alloc(num_elts + 1, sizeof(long double));
        tmp = (tmp + ld_align - 1) & ~((uintptr_t)ld_align - 1);
        return (long double *)tmp;
    }
    else
    {
        return (long double *)R_alloc(num_elts, sizeof(long double));
    }
}

char *S_alloc(long num_elts, int elt_size)
{
    R_size_t size = num_elts * elt_size;
    char *p = R_alloc(num_elts, elt_size);

    if (p)
        memset(p, 0, size);
    return p;
}

/* S COMPATIBILITY */

char *S_realloc(char *prev_block, long new_sz, long old_sz, int elt_size)
{
    size_t old_bytes;
    char *q;
    /* shrinking is a no-op */
    if (new_sz <= old_sz)
        return prev_block; // so nnew > 0 below
    q = R_alloc((size_t)new_sz, elt_size);
    old_bytes = (size_t)old_sz * elt_size;
    memcpy(q, prev_block, old_bytes);
    memset(q + old_bytes, 0, (size_t)new_sz * elt_size - old_bytes);
    return q;
}
