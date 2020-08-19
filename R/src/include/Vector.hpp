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

#ifndef VECTOR_HPP
#define VECTOR_HPP

#include <RObject.hpp>

namespace R
{
    /* The generational collector uses a reduced version of RObject as a
   header in vector nodes.  The layout MUST be kept consistent with
   the RObject definition. The standard RObject takes up 7 words
   and the reduced version takes 6 words on most 64-bit systems. On most
   32-bit systems, RObject takes 8 words and the reduced version 7 words. */
    class VECTOR
    {
    private:
        [[maybe_unused]] SEXPTYPE m_type : FULL_TYPE_BITS;
        [[maybe_unused]] bool m_scalar;
        [[maybe_unused]] bool m_has_class;
        [[maybe_unused]] bool m_alt;
        [[maybe_unused]] unsigned int m_gpbits : 16;
        [[maybe_unused]] bool m_marked;
        [[maybe_unused]] bool m_debug;
        [[maybe_unused]] bool m_trace;             /* functions and memory tracing */
        [[maybe_unused]] bool m_spare;             /* used on closures and when REFCNT is defined */
        [[maybe_unused]] bool m_gcgen;             /* old generation number */
        [[maybe_unused]] unsigned int m_gccls : 3; /* node class */
        [[maybe_unused]] unsigned int m_named : NAMED_BITS;
        [[maybe_unused]] unsigned int m_extra : 29 - NAMED_BITS; /* used for immediate bindings */
        [[maybe_unused]] RObject *m_attrib;
        [[maybe_unused]] RObject *gengc_next_node;
        [[maybe_unused]] RObject *gengc_prev_node;
        R_xlen_t m_length;
        R_xlen_t m_truelength;

    public:
        static inline R_xlen_t stdvec_length(RObject *x) { return x ? reinterpret_cast<VECTOR *>(x)->m_length : 0; }
        static inline R_xlen_t stdvec_truelength(RObject *x) { return x ? reinterpret_cast<VECTOR *>(x)->m_truelength : 0; }
        static inline void set_stdvec_truelength(RObject *x, R_xlen_t v)
        {
            if (!x)
                return;
            reinterpret_cast<VECTOR *>(x)->m_truelength = v;
        }
        static inline void set_stdvec_length(RObject *x, R_xlen_t v)
        {
            if (!x)
                return;
            reinterpret_cast<VECTOR *>(x)->m_length = v;
            RObject::setscalar(x, v == 1);
        }
    };

    using VECSEXP = class R::VECTOR *;

    union SEXPREC_ALIGN
    {
        VECTOR s;
        double align;
    };
} // namespace R

/* Vector Access Macros */
#ifdef LONG_VECTOR_SUPPORT
#define IS_LONG_VEC(x) (XLENGTH(x) > R_SHORT_LEN_MAX)
#else
#define IS_LONG_VEC(x) false
#endif
#define STDVEC_LENGTH(x) (R::VECTOR::stdvec_length(x))
#define STDVEC_TRUELENGTH(x) (R::VECTOR::stdvec_truelength(x))
#define SET_STDVEC_TRUELENGTH(x, v) (R::VECTOR::set_stdvec_truelength(x, v))
#define SET_TRUELENGTH(x, v)                      \
    do                                            \
    {                                             \
        R::RObject *sl__x__ = (x);                \
        R_xlen_t sl__v__ = (v);                   \
        if (ALTREP(x))                            \
            error("can't set ALTREP truelength"); \
        SET_STDVEC_TRUELENGTH(sl__x__, sl__v__);  \
    } while (0)

#define IS_SCALAR(x, t) (R::RObject::is_scalar(x, t))
#define LENGTH(x) LENGTH_EX(x, __FILE__, __LINE__)
#define TRUELENGTH(x) XTRUELENGTH(x)

/* defined as a macro since fastmatch packages tests for it */
#define XLENGTH(x) XLENGTH_EX(x)

/* THIS ABSOLUTELY MUST NOT BE USED IN PACKAGES !!! */
#define SET_STDVEC_LENGTH(x, v) (R::VECTOR::set_stdvec_length(x, v))

/* Under the generational allocator the data for vector nodes comes
   immediately after the node structure, so the data address is a
   known offset from the node SEXP. */
#define STDVEC_DATAPTR(x) (reinterpret_cast<void *>(reinterpret_cast<R::SEXPREC_ALIGN *>(x) + 1))
#define CHAR(x) ((const char *)STDVEC_DATAPTR(x))
#define LOGICAL(x) ((int *)DATAPTR(x))
#define INTEGER(x) ((int *)DATAPTR(x))
#define RAW(x) ((Rbyte *)DATAPTR(x))
#define COMPLEX(x) ((Rcomplex *)DATAPTR(x))
#define REAL(x) ((double *)DATAPTR(x))
#define VECTOR_ELT(x, i) ((SEXP *)DATAPTR(x))[i]
#define STRING_PTR(x) ((SEXP *)DATAPTR(x))
#define VECTOR_PTR(x) ((SEXP *)DATAPTR(x))
#define LOGICAL_RO(x) ((const int *)DATAPTR_RO(x))
#define INTEGER_RO(x) ((const int *)DATAPTR_RO(x))
#define RAW_RO(x) ((const Rbyte *)DATAPTR_RO(x))
#define COMPLEX_RO(x) ((const Rcomplex *)DATAPTR_RO(x))
#define REAL_RO(x) ((const double *)DATAPTR_RO(x))
#define STRING_PTR_RO(x) ((const SEXP *)DATAPTR_RO(x))

#endif