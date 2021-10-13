/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998--2020  The R Core Team.
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
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

/** @file BCNodeStack.h
 *
 * Byte Code Node Stack and related functions.
 */

#ifndef BCNODESTACK_H
#define BCNODESTACK_H

#ifndef __cplusplus
#error BCNodeStack.h can only be included in C++ files
#endif

#include <CXXR/RTypes.hpp>

/* The byte code engine uses a typed stack. The typed stack's entries
   consist of a tag and a union. An entry can represent a standard
   SEXP value (tag = 0) or an unboxed scalar value.  For now real,
   integer, and logical values are supported. It would in principle be
   possible to support complex scalars and short scalar strings, but
   it isn't clear if this is worth while.

   In addition to unboxed values the typed stack can hold partially
   evaluated or incomplete allocated values. For now this is only used
   for holding a short representation of an integer sequence as produce
   by the colon operator, seq_len, or seq_along, and as consumed by
   compiled 'for' loops. This could be used more extensively in the
   future, though the ALTREP framework may be a better choice.

   Allocating on the stack memory is also supported; this is currently
   used for jump buffers.
*/

enum BCStack_enum
{
    PARTIALSXP_MASK = (~255),
    CACHESZ_TAG = 253,
    RAWMEM_TAG = 254
};

struct R_bcstack_t
{
    int tag;
    int flags;
    union
    {
        int ival;
        double dval;
        SEXP sxpval;
    } u;
    static int IS_PARTIAL_SXP_TAG(int x) { return ((x)&PARTIALSXP_MASK); }
};

constexpr size_t R_BCNODESTACKSIZE = 200000;

#include <R_ext/libextern.h>

#ifdef __MAIN__
#define extern0 HIDDEN
#else
#define extern0 extern
#endif

LibExtern R_bcstack_t *R_BCNodeStackTop, *R_BCNodeStackEnd;
extern0 R_bcstack_t *R_BCNodeStackBase;
extern0 R_bcstack_t *R_BCProtTop;

void R_BCProtReset(R_bcstack_t *);

#ifdef __MAIN__
#undef extern1
#undef extern0
#undef LibExtern
#endif

#endif // BCNODESTACK_H