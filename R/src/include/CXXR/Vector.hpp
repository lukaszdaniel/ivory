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

#include <CXXR/RObject.hpp>

namespace R
{
  /* The generational collector uses a reduced version of RObject as a
   header in vector nodes.  The layout MUST be kept consistent with
   the RObject definition. The standard RObject takes up 7 words
   and the reduced version takes 6 words on most 64-bit systems. On most
   32-bit systems, RObject takes 8 words and the reduced version 7 words. */
  class VECTOR : public RObject
  {
  private:
  public:
  };

  using VECSEXP = class R::RObject *;

/* Vector Access Macros */
#ifdef LONG_VECTOR_SUPPORT
#define IS_LONG_VEC(x) (XLENGTH(x) > R_SHORT_LEN_MAX)
#else
#define IS_LONG_VEC(x) false
#endif

#define LENGTH(x) LENGTH_EX(x, __FILE__, __LINE__)
#define TRUELENGTH(x) XTRUELENGTH(x)

/* defined as a macro since fastmatch packages tests for it */
#define XLENGTH(x) XLENGTH_EX(x)

  /* Under the generational allocator the data for vector nodes comes
   immediately after the node structure, so the data address is a
   known offset from the node SEXP. */
  inline void *stdvec_dataptr(RObject *x) { return x ? x->m_data : nullptr; }
#define STDVEC_DATAPTR(x) (x->m_data)

  inline const char *r_char(RObject *x) { return static_cast<const char *>(stdvec_dataptr(x)); }

  /* writable char access for R internal use only */
#define CHAR_RW(x) ((char *)x->m_data)
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

} // namespace R

#endif /* VECTOR_HPP */