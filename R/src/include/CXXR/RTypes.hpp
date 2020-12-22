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

#ifndef RTYPES_HPP
#define RTYPES_HPP

#ifdef __cplusplus
#include <cstdio>
#include <climits>
#include <limits>
#include <cstddef>
#include <string>
#else
#include <stdio.h>
#include <limits.h> /* for INT_MAX */
#include <stddef.h> /* for ptrdiff_t, which is required by C99 */
#endif

#if defined(COMPILING_IVORY) && defined(__cplusplus)

namespace CXXR
{
    class RObject;
}
using SEXP = CXXR::RObject *;
#else
#define RObject SEXPREC
typedef struct RObject *SEXP;
#endif

#define CXXR_TRUE 1
#define CXXR_FALSE 0
#define LOCATION __FILE__ << ":" << __LINE__ << " "

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
/* both config.h and Rconfig.h set SIZEOF_SIZE_T, but Rconfig.h is
   skipped if config.h has already been included. */
// #ifndef R_CONFIG_H
// #include <Rconfig.h>
// #endif

#ifdef __cplusplus
using Rbyte = unsigned char;
#else
typedef unsigned char Rbyte;
#endif

/* type for length of (standard, not long) vectors etc */
#ifdef __cplusplus
using R_len_t = int;
constexpr R_len_t R_LEN_T_MAX = std::numeric_limits<R_len_t>::max();
constexpr int R_INT_MAX = std::numeric_limits<int>::max();
constexpr int R_INT_MIN = std::numeric_limits<int>::min() + 1;
#else
typedef int R_len_t;
#define R_LEN_T_MAX INT_MAX
#define R_INT_MAX INT_MAX
#define R_INT_MIN -INT_MAX
#endif

#if (SIZEOF_SIZE_T > 4)
#define LONG_VECTOR_SUPPORT
#endif

#ifdef __cplusplus
#ifdef LONG_VECTOR_SUPPORT
using R_xlen_t = ptrdiff_t;
constexpr R_xlen_t R_XLEN_T_MAX = std::numeric_limits<R_xlen_t>::max();
#else
using R_xlen_t = int;
constexpr R_xlen_t R_XLEN_T_MAX = std::numeric_limits<R_xlen_t>::max();
#endif
constexpr int R_SHORT_LEN_MAX = std::numeric_limits<int>::max();
#else // not __cplusplus
#ifdef LONG_VECTOR_SUPPORT
#include <stdint.h>
typedef ptrdiff_t R_xlen_t;
#define R_XLEN_T_MAX PTRDIFF_MAX
#define R_SHORT_LEN_MAX INT_MAX
#else
typedef int R_xlen_t;
#define R_XLEN_T_MAX R_LEN_T_MAX
#endif
#endif

#ifdef __cplusplus
using R_size_t = size_t;
constexpr R_size_t R_SIZE_T_MAX = std::numeric_limits<R_size_t>::max();
#endif

#endif /* RTYPES_HPP */
