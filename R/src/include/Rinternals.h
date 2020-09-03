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

/* This file is installed and available to packages, but only a small
   part of the contents is within the API.  See chapter 6 of 'Writing
   R Extensions'.
*/

/** @file Rinternals.h
 * @brief (As described in 'Writing R Extensions'.)
 */

#ifndef R_INTERNALS_H_
#define R_INTERNALS_H_

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

#ifdef __cplusplus
extern "C" {
#endif

#include <R_ext/Arith.h>
#include <R_ext/Boolean.h>
#include <R_ext/Complex.h>
#include <R_ext/Error.h>
#include <R_ext/Visibility.h>  // includes NORET macro
#include <R_ext/Memory.h>
#include <R_ext/Utils.h>
#include <R_ext/Print.h>
#include <R_ext/Rdynload.h> // for DL_FUNC

#include <R_ext/libextern.h>

#ifdef __cplusplus
using Rbyte = unsigned char;
#else
typedef unsigned char Rbyte;
#endif

#define DO_NOTHING do {} while(0)
/* type for length of (standard, not long) vectors etc */
#ifdef __cplusplus
using R_len_t = int;
constexpr R_len_t R_LEN_T_MAX = std::numeric_limits<R_len_t>::max();
constexpr int R_INT_MAX = std::numeric_limits<int>::max();
constexpr int R_INT_MIN = std::numeric_limits<int>::min() + 1;
#else
typedef int R_len_t;
#define R_LEN_T_MAX INT_MAX
#define R_INT_MAX  INT_MAX
#define R_INT_MIN -INT_MAX
#endif

/* both config.h and Rconfig.h set SIZEOF_SIZE_T, but Rconfig.h is
   skipped if config.h has already been included. */
#ifndef R_CONFIG_H
#include <Rconfig.h>
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

#ifndef TESTING_WRITE_BARRIER
#define INLINE_PROTECT
#endif

/* Fundamental Data Types:  These are largely Lisp
 * influenced structures, with the exception of LGLSXP,
 * INTSXP, REALSXP, CPLXSXP and STRSXP which are the
 * element types for S-like data objects.
 *
 *			--> TypeTable[] in ../main/util.cpp for  typeof()
 */

/* UUID identifying the internals version -- packages using compiled
   code should be re-installed when this changes */
#define R_INTERNALS_UUID "2fdf6c18-697a-4ba7-b8ef-11c0d92f1327"

/*  These exact numeric values are seldom used, but they are, e.g., in
 *  ../main/subassign.cpp, and they are serialized.
*/

    /** @enum SEXPTYPE
     *
     * @brief CR's object type identification.
     *
     * This enumeration is used within CR to identify different types
     * of R object.
     * 
     * Note: when not compiling ivory, SEXPTYPE is a typedef for unsigned int.
     * This is done to support C++ packages that expect implicit int to
     * SEXPTYPE conversions.
     */
#ifndef COMPILING_IVORY
typedef unsigned int SEXPTYPE;
#else
typedef
#endif
enum
{
    NILSXP = 0,      /* nil = NULL */
    SYMSXP = 1,      /* symbols */
    LISTSXP = 2,     /* lists of dotted pairs */
    CLOSXP = 3,      /* closures */
    ENVSXP = 4,      /* environments */
    PROMSXP = 5,     /* promises: [un]evaluated closure arguments */
    LANGSXP = 6,     /* language constructs (special lists) */
    SPECIALSXP = 7,  /* special forms */
    BUILTINSXP = 8,  /* builtin non-special forms */
    CHARSXP = 9,     /* "scalar" string type (internal only)*/
    LGLSXP = 10,     /* logical vectors */
    INTSXP = 13,     /* integer vectors */
    REALSXP = 14,    /* real variables */
    CPLXSXP = 15,    /* complex variables */
    STRSXP = 16,     /* string vectors */
    DOTSXP = 17,     /* dot-dot-dot object */
    ANYSXP = 18,     /* make "any" args work */
    VECSXP = 19,     /* generic vectors */
    EXPRSXP = 20,    /* expressions vectors */
    BCODESXP = 21,   /* byte code */
    EXTPTRSXP = 22,  /* external pointer */
    WEAKREFSXP = 23, /* weak reference */
    RAWSXP = 24,     /* raw bytes */
    S4SXP = 25,      /* S4 non-vector */

    NEWSXP = 30,    /* fresh node created in new page */
    FREESXP = 31,   /* node released by GC */
    SINGLESXP = 47, /* For interfaces to objects created with as.single */
    intCHARSXP = 73,
    FUNSXP = 99, /* Closure or Builtin */
    ALTREP_SXP = 238,
    ATTRLISTSXP = 239,
    ATTRLANGSXP = 240,
    /* the following (241 - 246) are speculative--we may or may not need them soon */
    BASEENV_SXP = 241,
    EMPTYENV_SXP = 242,
    BCREPREF = 243,
    BCREPDEF = 244,
    GENERICREFSXP = 245,
    CLASSREFSXP = 246,
    PERSISTSXP = 247,
    PACKAGESXP = 248,
    NAMESPACESXP = 249,
    BASENAMESPACE_SXP = 250,
    MISSINGARG_SXP = 251,
    UNBOUNDVALUE_SXP = 252,
    GLOBALENV_SXP = 253,
    NILVALUE_SXP = 254,
    REFSXP = 255
}
#ifdef COMPILING_IVORY
SEXPTYPE
#endif
    ;

#if defined(COMPILING_IVORY) && defined(__cplusplus)

    namespace R
    {
        class RObject;
    }
    using SEXP = R::RObject *;
#if 0
    class R::GCNode;
    class R::Symbol;
    class R::BuiltInFunction;
    class R::Environment;
    class R::Closure;
    class R::Promise;
    class R::List;
#else
    typedef class R::RObject GCNode;
    typedef class R::RObject Symbol;
    typedef class R::RObject BuiltInFunction;
    typedef class R::RObject Environment;
    typedef class R::RObject Closure;
    typedef class R::RObject Promise;
    typedef class R::RObject RList;
#endif
#else
    #define RObject SEXPREC
    typedef struct RObject *SEXP;
#endif

/* Define SWITCH_TO_NAMED to use the 'NAMED' mechanism instead of
   reference counting. */
#if (!defined(SWITCH_TO_NAMED) && !defined(SWITCH_TO_REFCNT)) || defined(COMPILING_IVORY)
#define SWITCH_TO_REFCNT
#endif

#if defined(SWITCH_TO_REFCNT) && !defined(COMPUTE_REFCNT_VALUES)
#define COMPUTE_REFCNT_VALUES
#endif
#if defined(SWITCH_TO_REFCNT) && !defined(ADJUST_ENVIR_REFCNTS)
#define ADJUST_ENVIR_REFCNTS
#endif


// ======================= USE_RINTERNALS section
#if defined(USE_RINTERNALS) && defined(COMPILING_IVORY) && defined(__cplusplus)

// Content moved to RObject.hpp

#else /* not USE_RINTERNALS */
// ======================= not USE_RINTERNALS section

#define CHAR(x)		R_CHAR(x)
const char *(R_CHAR)(SEXP x);

/* Various tests with macro versions in the second USE_RINTERNALS section */
Rboolean (Rf_isNull)(SEXP s);
Rboolean (Rf_isSymbol)(SEXP s);
Rboolean (Rf_isLogical)(SEXP s);
Rboolean (Rf_isReal)(SEXP s);
Rboolean (Rf_isComplex)(SEXP s);
Rboolean (Rf_isExpression)(SEXP s);
Rboolean (Rf_isEnvironment)(SEXP s);
Rboolean (Rf_isString)(SEXP s);
Rboolean (Rf_isObject)(SEXP s);

#endif /* USE_RINTERNALS */

// =====
// These are required by stringi and data.table packages
// if we're disabling USE_RINTERNALS for them.
#ifndef TESTING_WRITE_BARRIER
#ifndef IS_BYTES
#define IS_BYTES(x) (LEVELS(x) & 2)
#endif
#ifndef IS_LATIN1
#define IS_LATIN1(x) (LEVELS(x) & 4)
#endif
#ifndef IS_ASCII
#define IS_ASCII(x) (LEVELS(x) & 64)
#endif
#ifndef IS_UTF8
#define IS_UTF8(x) (LEVELS(x) & 8)
#endif
#ifndef IS_CACHED
#define IS_CACHED(x) (LEVELS(x) & 32)
#endif
#ifndef ENC_KNOWN
#define ENC_KNOWN(x) (LEVELS(x) & 12)
#endif
#endif
// =====

#define IS_SIMPLE_SCALAR(x, type) \
    (IS_SCALAR(x, type) && ATTRIB(x) == R_NilValue)
int (SIMPLE_SCALAR_TYPE)(SEXP x);

#define NAMEDMAX 7
#ifdef SWITCH_TO_REFCNT
# define INCREMENT_NAMED(x) do { } while (0)
# define DECREMENT_NAMED(x) do { } while (0)
#else
#define INCREMENT_NAMED(x)                      \
    do                                          \
    {                                           \
        SEXP __x__ = (x);                       \
        if (NAMED(__x__) != NAMEDMAX)           \
            SET_NAMED(__x__, NAMED(__x__) + 1); \
    } while (0)
#define DECREMENT_NAMED(x)                 \
    do                                     \
    {                                      \
        SEXP __x__ = (x);                  \
        int __n__ = NAMED(__x__);          \
        if (__n__ > 0 && __n__ < NAMEDMAX) \
            SET_NAMED(__x__, __n__ - 1);   \
    } while (0)
#endif

#define INCREMENT_LINKS(x)         \
    do                             \
    {                              \
        SEXP il__x__ = (x);        \
        INCREMENT_NAMED(il__x__);  \
        INCREMENT_REFCNT(il__x__); \
    } while (0)
#define DECREMENT_LINKS(x)         \
    do                             \
    {                              \
        SEXP dl__x__ = (x);        \
        DECREMENT_NAMED(dl__x__);  \
        DECREMENT_REFCNT(dl__x__); \
    } while (0)

/* Macros for some common idioms. */
#ifdef SWITCH_TO_REFCNT
# define MAYBE_SHARED(x) (REFCNT(x) > 1)
# define NO_REFERENCES(x) (REFCNT(x) == 0)
# ifdef USE_RINTERNALS
#  define MARK_NOT_MUTABLE(x) SET_REFCNT(x, REFCNTMAX)
# endif
#else
# define MAYBE_SHARED(x) (NAMED(x) > 1)
# define NO_REFERENCES(x) (NAMED(x) == 0)
# ifdef USE_RINTERNALS
#  define MARK_NOT_MUTABLE(x) SET_NAMED(x, NAMEDMAX)
# endif
#endif
#define MAYBE_REFERENCED(x) (! NO_REFERENCES(x))
#define NOT_SHARED(x) (! MAYBE_SHARED(x))

/* ALTREP sorting support */
typedef enum Sortness
{
    SORTED_DECR_NA_1ST = -2,
    SORTED_DECR = -1,
    UNKNOWN_SORTEDNESS = INT_MIN, /*INT_MIN is NA_INTEGER! */
    SORTED_INCR = 1,
    SORTED_INCR_NA_1ST = 2,
    KNOWN_UNSORTED = 0
} Sortness;

#define KNOWN_SORTED(sorted) (sorted == SORTED_DECR ||        \
                              sorted == SORTED_INCR ||        \
                              sorted == SORTED_DECR_NA_1ST || \
                              sorted == SORTED_INCR_NA_1ST)

#define KNOWN_NA_1ST(sorted) (sorted == SORTED_INCR_NA_1ST || \
                              sorted == SORTED_DECR_NA_1ST)

#define KNOWN_INCR(sorted) (sorted == SORTED_INCR || \
                            sorted == SORTED_INCR_NA_1ST)

#define KNOWN_DECR(sorted) (sorted == SORTED_DECR || \
                            sorted == SORTED_DECR_NA_1ST)

/* Complex assignment support */
/* temporary definition that will need to be refined to distinguish
   getter from setter calls */
#define IS_GETTER_CALL(call) (CADR(call) == R_TmpvalSymbol)

/* Accessor functions.  Many are declared using () to avoid the macro
   definitions in the USE_RINTERNALS section.
   The function STRING_ELT is used as an argument to arrayAssign even
   if the macro version is in use.
*/

/* General Cons Cell Attributes */
SEXP (ATTRIB)(SEXP x);
int  (OBJECT)(SEXP x);
int  (MARK)(SEXP x);
SEXPTYPE (TYPEOF)(SEXP x);
int  (NAMED)(SEXP x);
int  (REFCNT)(SEXP x);
int  (TRACKREFS)(SEXP x);
void (SET_OBJECT)(SEXP x, int v);
void (SET_TYPEOF)(SEXP x, SEXPTYPE v);
void (SET_NAMED)(SEXP x, int v);
void SET_ATTRIB(SEXP x, SEXP v);
void DUPLICATE_ATTRIB(SEXP to, SEXP from);
void SHALLOW_DUPLICATE_ATTRIB(SEXP to, SEXP from);
void (ENSURE_NAMEDMAX)(SEXP x);
void (ENSURE_NAMED)(SEXP x);
void (SETTER_CLEAR_NAMED)(SEXP x);
void (RAISE_NAMED)(SEXP x, int n);
void (DECREMENT_REFCNT)(SEXP x);
void (INCREMENT_REFCNT)(SEXP x);
void (DISABLE_REFCNT)(SEXP x);
void (ENABLE_REFCNT)(SEXP x);
void (MARK_NOT_MUTABLE)(SEXP x);

int (ASSIGNMENT_PENDING)(SEXP x);
void (SET_ASSIGNMENT_PENDING)(SEXP x, int v);
int (IS_ASSIGNMENT_CALL)(SEXP x);
void (MARK_ASSIGNMENT_CALL)(SEXP x);

/* S4 object testing */
int (IS_S4_OBJECT)(SEXP x);
void (SET_S4_OBJECT)(SEXP x);
void (UNSET_S4_OBJECT)(SEXP x);

/* JIT optimization support */
int (NOJIT)(SEXP x);
int (MAYBEJIT)(SEXP x);
void (SET_NOJIT)(SEXP x);
void (SET_MAYBEJIT)(SEXP x);
void (UNSET_MAYBEJIT)(SEXP x);

/* Growable vector support */
int (IS_GROWABLE)(SEXP x);
void (SET_GROWABLE_BIT)(SEXP x);

/* Vector Access Functions */
int  (LENGTH)(SEXP x);
R_xlen_t (XLENGTH)(SEXP x);
R_xlen_t  (TRUELENGTH)(SEXP x);
void (SETLENGTH)(SEXP x, R_xlen_t v);
void (SET_TRUELENGTH)(SEXP x, R_xlen_t v);
int  (IS_LONG_VEC)(SEXP x);
int  (LEVELS)(SEXP x);
void  (SETLEVELS)(SEXP x, int v);
#if defined(TESTING_WRITE_BARRIER) || defined(COMPILING_IVORY)
R_xlen_t (STDVEC_LENGTH)(SEXP);
R_xlen_t (STDVEC_TRUELENGTH)(SEXP);
void (SETALTREP)(SEXP, int);
#endif

int  *(LOGICAL)(SEXP x);
int  *(INTEGER)(SEXP x);
Rbyte *(RAW)(SEXP x);
double *(REAL)(SEXP x);
Rcomplex *(COMPLEX)(SEXP x);
const int  *(LOGICAL_RO)(SEXP x);
const int  *(INTEGER_RO)(SEXP x);
const Rbyte *(RAW_RO)(SEXP x);
const double *(REAL_RO)(SEXP x);
const Rcomplex *(COMPLEX_RO)(SEXP x);
//SEXP (STRING_ELT)(SEXP x, R_xlen_t i);
SEXP (VECTOR_ELT)(SEXP x, R_xlen_t i);
void SET_STRING_ELT(SEXP x, R_xlen_t i, SEXP v);
SEXP SET_VECTOR_ELT(SEXP x, R_xlen_t i, SEXP v);
SEXP *(STRING_PTR)(SEXP x);
const SEXP *(STRING_PTR_RO)(SEXP x);
NORET SEXP * (VECTOR_PTR)(SEXP x);

/* ALTREP support */
void *(STDVEC_DATAPTR)(SEXP x);
int (IS_SCALAR)(SEXP x, SEXPTYPE type);
int (ALTREP)(SEXP x);
SEXP ALTREP_DUPLICATE_EX(SEXP x, Rboolean deep);
SEXP ALTREP_COERCE(SEXP x, int type);
Rboolean ALTREP_INSPECT(SEXP, int, int, int, void (*)(SEXP, int, int, int));
SEXP ALTREP_SERIALIZED_CLASS(SEXP);
SEXP ALTREP_SERIALIZED_STATE(SEXP);
SEXP ALTREP_UNSERIALIZE_EX(SEXP, SEXP, SEXP, int, int);
R_xlen_t ALTREP_LENGTH(SEXP x);
R_xlen_t ALTREP_TRUELENGTH(SEXP x);
void *ALTVEC_DATAPTR(SEXP x);
const void *ALTVEC_DATAPTR_RO(SEXP x);
const void *ALTVEC_DATAPTR_OR_NULL(SEXP x);
SEXP ALTVEC_EXTRACT_SUBSET(SEXP x, SEXP indx, SEXP call);

/* data access */
int ALTINTEGER_ELT(SEXP x, R_xlen_t i);
void ALTINTEGER_SET_ELT(SEXP x, R_xlen_t i, int v);
int ALTLOGICAL_ELT(SEXP x, R_xlen_t i);
void ALTLOGICAL_SET_ELT(SEXP x, R_xlen_t i, int v);
double ALTREAL_ELT(SEXP x, R_xlen_t i);
void ALTREAL_SET_ELT(SEXP x, R_xlen_t i, double v);
SEXP ALTSTRING_ELT(SEXP, R_xlen_t);
void ALTSTRING_SET_ELT(SEXP, R_xlen_t, SEXP);
Rcomplex ALTCOMPLEX_ELT(SEXP x, R_xlen_t i);
void ALTCOMPLEX_SET_ELT(SEXP x, R_xlen_t i, Rcomplex v);
Rbyte ALTRAW_ELT(SEXP x, R_xlen_t i);
void ALTRAW_SET_ELT(SEXP x, R_xlen_t i, Rbyte v);

R_xlen_t INTEGER_GET_REGION(SEXP sx, R_xlen_t i, R_xlen_t n, int *buf);
R_xlen_t REAL_GET_REGION(SEXP sx, R_xlen_t i, R_xlen_t n, double *buf);
R_xlen_t LOGICAL_GET_REGION(SEXP sx, R_xlen_t i, R_xlen_t n, int *buf);
R_xlen_t COMPLEX_GET_REGION(SEXP sx, R_xlen_t i, R_xlen_t n, Rcomplex *buf);
R_xlen_t RAW_GET_REGION(SEXP sx, R_xlen_t i, R_xlen_t n, Rbyte *buf);

/* metadata access */
int INTEGER_IS_SORTED(SEXP x);
int INTEGER_NO_NA(SEXP x);
int REAL_IS_SORTED(SEXP x);
int REAL_NO_NA(SEXP x);
int LOGICAL_IS_SORTED(SEXP x);
int LOGICAL_NO_NA(SEXP x);
int STRING_IS_SORTED(SEXP x);
int STRING_NO_NA(SEXP x);

/* invoking ALTREP class methods */
SEXP ALTINTEGER_SUM(SEXP x, Rboolean narm);
SEXP ALTINTEGER_MIN(SEXP x, Rboolean narm);
SEXP ALTINTEGER_MAX(SEXP x, Rboolean narm);
SEXP INTEGER_MATCH(SEXP, SEXP, int, SEXP, SEXP, Rboolean);
SEXP INTEGER_IS_NA(SEXP x);
SEXP ALTREAL_SUM(SEXP x, Rboolean narm);
SEXP ALTREAL_MIN(SEXP x, Rboolean narm);
SEXP ALTREAL_MAX(SEXP x, Rboolean narm);
SEXP REAL_MATCH(SEXP, SEXP, int, SEXP, SEXP, Rboolean);
SEXP REAL_IS_NA(SEXP x);
SEXP ALTLOGICAL_SUM(SEXP x, Rboolean narm);

/* constructors for internal ALTREP classes */
SEXP R_compact_intrange(R_xlen_t n1, R_xlen_t n2);
SEXP R_deferred_coerceToString(SEXP v, SEXP info);
SEXP R_virtrep_vec(SEXP, SEXP);
SEXP R_tryWrap(SEXP);
SEXP R_tryUnwrap(SEXP);

#ifdef LONG_VECTOR_SUPPORT
    NORET R_len_t R_BadLongVector(SEXP, const char *, int);
#endif

/* checking for mis-use of multi-threading */
#ifdef TESTING_WRITE_BARRIER
# define THREADCHECK
#endif
#ifdef THREADCHECK
void R_check_thread(const char *s);
# define R_CHECK_THREAD R_check_thread(__func__)
#else
# define R_CHECK_THREAD do {} while (0)
#endif

/* List Access Functions */
/* These also work for ... objects */
#define CONS(a, b)	Rf_cons((a), (b))		/* data lists */
#define LCONS(a, b)	Rf_lcons((a), (b))		/* language lists */
int (BNDCELL_TAG)(SEXP e);
void (SET_BNDCELL_TAG)(SEXP e, int v);
double (BNDCELL_DVAL)(SEXP cell);
int (BNDCELL_IVAL)(SEXP cell);
int (BNDCELL_LVAL)(SEXP cell);
void (SET_BNDCELL_DVAL)(SEXP cell, double v);
void (SET_BNDCELL_IVAL)(SEXP cell, int v);
void (SET_BNDCELL_LVAL)(SEXP cell, int v);
void (INIT_BNDCELL)(SEXP cell, int type);
void (SET_BNDCELL)(SEXP cell, SEXP val);

SEXP (TAG)(SEXP e);
SEXP (CAR0)(SEXP e);
SEXP (CDR)(SEXP e);
SEXP (CAAR)(SEXP e);
SEXP (CDAR)(SEXP e);
SEXP (CADR)(SEXP e);
SEXP (CDDR)(SEXP e);
SEXP (CDDDR)(SEXP e);
SEXP (CADDR)(SEXP e);
SEXP (CD4R)(SEXP e);
SEXP (CADDDR)(SEXP e);
SEXP (CAD3R)(SEXP e);
SEXP (CAD4R)(SEXP e);
SEXP (CAD5R)(SEXP e);
int  (MISSING)(SEXP x);
void (SET_MISSING)(SEXP x, int v);
void (SET_TAG)(SEXP x, SEXP y);
SEXP SETCAR(SEXP x, SEXP y);
SEXP SETCDR(SEXP x, SEXP y);
SEXP SETCADR(SEXP x, SEXP y);
SEXP SETCADDR(SEXP x, SEXP y);
SEXP SETCADDDR(SEXP x, SEXP y);
SEXP SETCAD4R(SEXP e, SEXP y);
void *(EXTPTR_PTR)(SEXP);

SEXP CONS_NR(SEXP a, SEXP b);

/* Closure Access Functions */
SEXP (FORMALS)(SEXP x);
SEXP (BODY)(SEXP x);
SEXP (CLOENV)(SEXP x);
int  (RDEBUG)(SEXP x);
int  (RSTEP)(SEXP x);
int  (RTRACE)(SEXP x);
void (SET_RDEBUG)(SEXP x, int v);
void (SET_RSTEP)(SEXP x, int v);
void (SET_RTRACE)(SEXP x, int v);
void (SET_FORMALS)(SEXP x, SEXP v);
void (SET_BODY)(SEXP x, SEXP v);
void (SET_CLOENV)(SEXP x, SEXP v);

/* Symbol Access Functions */
SEXP (PRINTNAME)(SEXP x);
SEXP (SYMVALUE)(SEXP x);
SEXP (INTERNAL)(SEXP x);
int  (DDVAL)(SEXP x);
void (SET_DDVAL)(SEXP x, int v);
void (SET_PRINTNAME)(SEXP x, SEXP v);
void (SET_SYMVALUE)(SEXP x, SEXP v);
void (SET_INTERNAL)(SEXP x, SEXP v);

/* Environment Access Functions */
SEXP (FRAME)(SEXP x);
SEXP (ENCLOS)(SEXP x);
SEXP (HASHTAB)(SEXP x);
int  (ENVFLAGS)(SEXP x);
void (SET_ENVFLAGS)(SEXP x, int v);
void (SET_FRAME)(SEXP x, SEXP v);
void (SET_ENCLOS)(SEXP x, SEXP v);
void (SET_HASHTAB)(SEXP x, SEXP v);

/* Promise Access Functions */
/* First five have macro versions in Defn.h */
SEXP (PRCODE)(SEXP x);
SEXP (PRENV)(SEXP x);
SEXP (PRVALUE)(SEXP x);
int  (PRSEEN)(SEXP x);
void (SET_PRSEEN)(SEXP x, int v);
void (SET_PRENV)(SEXP x, SEXP v);
void (SET_PRVALUE)(SEXP x, SEXP v);
void (SET_PRCODE)(SEXP x, SEXP v);

/* Hashing Functions */
/* There are macro versions in Defn.h */
int  (HASHASH)(SEXP x);
int  (HASHVALUE)(SEXP x);
void (SET_HASHASH)(SEXP x, int v);
void (SET_HASHVALUE)(SEXP x, int v);


/* External pointer access macros */
#define EXTPTR_PROT(x)	CDR(x)
#define EXTPTR_TAG(x)	TAG(x)
/* EXTPTR_PTR is defined above within USE_RINTERNALS */

/* Bytecode access macros */
#define BCODE_CODE(x)	CAR(x)
#define BCODE_CONSTS(x) CDR(x)
#define BCODE_EXPR(x)	TAG(x)
#define isByteCode(x)	(TYPEOF(x)==BCODESXP)

/* Pointer Protection and Unprotection */
#define PROTECT(s)	Rf_protect(s)
#define UNPROTECT(n)	Rf_unprotect(n)
#define UNPROTECT_PTR(s)	Rf_unprotect_ptr(s)

/* We sometimes need to coerce a protected value and place the new
   coerced value under protection.  For these cases PROTECT_WITH_INDEX
   saves an index of the protection location that can be used to
   replace the protected value using REPROTECT. */
typedef int PROTECT_INDEX;
#define PROTECT_WITH_INDEX(x,i) R_ProtectWithIndex(x,i)
#define REPROTECT(x,i) R_Reprotect(x,i)

/* Evaluation Environment */
LibExtern SEXP R_GlobalEnv;	    /* The "global" environment */

LibExtern SEXP R_EmptyEnv;	    /* An empty environment at the root of the
				    	environment tree */
LibExtern SEXP R_BaseEnv;	    /* The base environment; formerly R_NilValue */
LibExtern SEXP R_BaseNamespace;    /* The (fake) namespace for base */
LibExtern SEXP	R_NamespaceRegistry;/* Registry for registered namespaces */

LibExtern SEXP	R_Srcref;           /* Current srcref, for debuggers */

/* Special Values */
#if 0
LibExtern SEXP	R_NilValue;	    /* The nil object */
#else
#ifdef __cplusplus
#define R_NilValue ((SEXP)nullptr)
#else
#define R_NilValue ((SEXP)NULL)
#endif
#endif
LibExtern SEXP R_UnboundValue;	    /* Unbound marker */
LibExtern SEXP R_MissingArg;	    /* Missing argument marker */
LibExtern SEXP	R_InBCInterpreter;  /* To be found in BC interp. state
				       (marker) */
LibExtern SEXP	R_CurrentExpression; /* Use current expression (marker) */
#ifdef __MAIN__
HIDDEN
#else
extern
#endif
SEXP	R_RestartToken;     /* Marker for restarted function calls */

/* Symbol Table Shortcuts */
LibExtern SEXP R_AsCharacterSymbol;/* "as.character" */
LibExtern SEXP R_baseSymbol; // <-- backcompatible version of:
LibExtern SEXP R_BaseSymbol;	// "base"
LibExtern SEXP R_BraceSymbol;	    /* "{" */
LibExtern SEXP R_Bracket2Symbol;   /* "[[" */
LibExtern SEXP R_BracketSymbol;    /* "[" */
LibExtern SEXP R_ClassSymbol;	    /* "class" */
LibExtern SEXP R_DeviceSymbol;	    /* ".Device" */
LibExtern SEXP R_DimNamesSymbol;   /* "dimnames" */
LibExtern SEXP R_DimSymbol;	    /* "dim" */
LibExtern SEXP R_DollarSymbol;	    /* "$" */
LibExtern SEXP R_DotsSymbol;	    /* "..." */
LibExtern SEXP R_DoubleColonSymbol;// "::"
LibExtern SEXP R_DropSymbol;	    /* "drop" */
LibExtern SEXP R_EvalSymbol;	    /* "eval" */
LibExtern SEXP R_LastvalueSymbol;  /* ".Last.value" */
LibExtern SEXP R_LevelsSymbol;	    /* "levels" */
LibExtern SEXP R_ModeSymbol;	    /* "mode" */
LibExtern SEXP R_NaRmSymbol;	    /* "na.rm" */
LibExtern SEXP R_NameSymbol;	    /* "name" */
LibExtern SEXP R_NamesSymbol;	    /* "names" */
LibExtern SEXP R_NamespaceEnvSymbol;// ".__NAMESPACE__."
LibExtern SEXP R_PackageSymbol;    /* "package" */
LibExtern SEXP R_PreviousSymbol;   /* "previous" */
LibExtern SEXP R_QuoteSymbol;	    /* "quote" */
LibExtern SEXP R_RowNamesSymbol;   /* "row.names" */
LibExtern SEXP R_SeedsSymbol;	    /* ".Random.seed" */
LibExtern SEXP R_SortListSymbol;   /* "sort.list" */
LibExtern SEXP R_SourceSymbol;	    /* "source" */
LibExtern SEXP R_SpecSymbol;	// "spec"
LibExtern SEXP R_TripleColonSymbol;// ":::"
LibExtern SEXP R_TspSymbol;	    /* "tsp" */

LibExtern SEXP R_dot_defined;      /* ".defined" */
LibExtern SEXP R_dot_Method;       /* ".Method" */
LibExtern SEXP R_dot_packageName;// ".packageName"
LibExtern SEXP R_dot_target;       /* ".target" */
LibExtern SEXP R_dot_Generic;      /* ".Generic" */

/* Missing Values - others from Arith.h */
#define NA_STRING	R_NaString
LibExtern SEXP	R_NaString;	    /* NA_STRING as a CHARSXP */
LibExtern SEXP	R_BlankString;	    /* "" as a CHARSXP */
LibExtern SEXP	R_BlankScalarString;/* "" as a STRSXP */

/* srcref related functions */
SEXP R_GetCurrentSrcref(int);
SEXP R_GetSrcFilename(SEXP);

/*--- FUNCTIONS ------------------------------------------------------ */

/* Type Coercions of all kinds */

SEXP Rf_asChar(SEXP);
SEXP Rf_coerceVector(SEXP, SEXPTYPE);
SEXP Rf_PairToVectorList(SEXP x);
SEXP Rf_VectorToPairList(SEXP x);
SEXP Rf_asCharacterFactor(SEXP x);
int Rf_asLogical(SEXP x);
int Rf_asLogical2(SEXP x, int checking, SEXP call, SEXP rho);
int Rf_asInteger(SEXP x);
double Rf_asReal(SEXP x);
Rcomplex Rf_asComplex(SEXP x);


#ifndef R_ALLOCATOR_TYPE
#define R_ALLOCATOR_TYPE
typedef struct R_allocator R_allocator_t;
#endif

typedef enum
{
    iSILENT,
    iWARN,
    iERROR
} warn_type;

/* Other Internally Used Functions, excluding those which are inline-able*/

char *Rf_acopy_string(const char *);
void Rf_addMissingVarsToNewEnv(SEXP, SEXP);
SEXP Rf_alloc3DArray(SEXPTYPE, int, int, int);
SEXP Rf_allocArray(SEXPTYPE, SEXP);
SEXP Rf_allocFormalsList2(SEXP sym1, SEXP sym2);
SEXP Rf_allocFormalsList3(SEXP sym1, SEXP sym2, SEXP sym3);
SEXP Rf_allocFormalsList4(SEXP sym1, SEXP sym2, SEXP sym3, SEXP sym4);
SEXP Rf_allocFormalsList5(SEXP sym1, SEXP sym2, SEXP sym3, SEXP sym4, SEXP sym5);
SEXP Rf_allocFormalsList6(SEXP sym1, SEXP sym2, SEXP sym3, SEXP sym4, SEXP sym5, SEXP sym6);
SEXP Rf_allocMatrix(SEXPTYPE, int, int);
SEXP Rf_allocList(int);
SEXP Rf_allocS4Object(void);
SEXP Rf_allocSExp(SEXPTYPE);
SEXP Rf_allocVector3(SEXPTYPE, R_xlen_t, R_allocator_t *);
R_xlen_t Rf_any_duplicated(SEXP x, Rboolean from_last);
R_xlen_t Rf_any_duplicated3(SEXP x, SEXP incomp, Rboolean from_last);
SEXP Rf_applyClosure(SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP Rf_arraySubscript(int, SEXP, SEXP, SEXP (*)(SEXP, SEXP), SEXP (*)(SEXP, int), SEXP);
SEXP Rf_classgets(SEXP, SEXP);
SEXP Rf_cons(SEXP, SEXP);
SEXP Rf_fixSubset3Args(SEXP, SEXP, SEXP, SEXP *);
void Rf_copyMatrix(SEXP, SEXP, Rboolean);
void Rf_copyListMatrix(SEXP, SEXP, Rboolean);
void Rf_copyMostAttrib(SEXP, SEXP);
void Rf_copyVector(SEXP, SEXP);
int Rf_countContexts(int, int);
SEXP Rf_CreateTag(SEXP);
void Rf_defineVar(SEXP, SEXP, SEXP);
SEXP Rf_dimgets(SEXP, SEXP);
SEXP Rf_dimnamesgets(SEXP, SEXP);
SEXP Rf_DropDims(SEXP);
SEXP Rf_duplicate(SEXP);
SEXP Rf_shallow_duplicate(SEXP);
SEXP R_duplicate_attr(SEXP);
SEXP R_shallow_duplicate_attr(SEXP);
SEXP Rf_lazy_duplicate(SEXP);
/* the next really should not be here and is also in Defn.h */
SEXP Rf_duplicated(SEXP x, Rboolean from_last);
Rboolean R_envHasNoSpecialSymbols(SEXP);
SEXP Rf_eval(SEXP e, SEXP rho);
SEXP Rf_ExtractSubset(SEXP x, SEXP indx, SEXP call);
SEXP Rf_findFun(SEXP symbol, SEXP rho);
SEXP Rf_findFun3(SEXP symbol, SEXP rho, SEXP call);
void Rf_findFunctionForBody(SEXP);
SEXP Rf_findVar(SEXP symbol, SEXP rho);
SEXP Rf_findVarInFrame(SEXP, SEXP);
SEXP Rf_findVarInFrame3(SEXP, SEXP, Rboolean);
void R_removeVarFromFrame(SEXP name, SEXP env);
SEXP Rf_getAttrib(SEXP vec, SEXP name);
SEXP Rf_GetArrayDimnames(SEXP);
SEXP Rf_GetColNames(SEXP);
void Rf_GetMatrixDimnames(SEXP x, SEXP *rl, SEXP *cl, const char **rn, const char **cn);
SEXP Rf_GetOption(SEXP tag, SEXP rho); /* pre-2.13.0 compatibility */
SEXP Rf_GetOption1(SEXP tag);
int Rf_FixupDigits(SEXP, warn_type);
size_t Rf_FixupWidth(SEXP, warn_type);
int Rf_GetOptionDigits(void);
size_t Rf_GetOptionWidth(void);
SEXP Rf_GetRowNames(SEXP);
void Rf_gsetVar(SEXP, SEXP, SEXP);
SEXP Rf_install(const char *name);
#ifdef __cplusplus
namespace R
{
    SEXP install_(const std::string &name);
}
#endif
SEXP Rf_installChar(SEXP x);
SEXP Rf_installNoTrChar(SEXP charSXP);
SEXP Rf_installTrChar(SEXP x);
SEXP Rf_installDDVAL(int i);
SEXP Rf_installS3Signature(const char *className, const char *methodName);
Rboolean Rf_isFree(SEXP val);
Rboolean Rf_isOrdered(SEXP s);
Rboolean Rf_isUnmodifiedSpecSym(SEXP sym, SEXP env);
Rboolean Rf_isUnordered(SEXP s);
Rboolean Rf_isUnsorted(SEXP x, Rboolean strictly);
SEXP Rf_lengthgets(SEXP x, R_len_t len);
SEXP Rf_xlengthgets(SEXP x, R_xlen_t len);
SEXP R_lsInternal(SEXP, Rboolean);
SEXP R_lsInternal3(SEXP env, Rboolean all, Rboolean sorted);
SEXP Rf_match(SEXP, SEXP, int);
SEXP Rf_matchE(SEXP itable, SEXP ix, int nmatch, SEXP env);
SEXP Rf_namesgets(SEXP vec, SEXP val);
SEXP Rf_mkChar(const char *const name);
SEXP Rf_mkCharLen(const char *name, int len);
Rboolean Rf_NonNullStringMatch(SEXP, SEXP);
int Rf_ncols(SEXP);
int Rf_nrows(SEXP);
SEXP Rf_nthcdr(SEXP, int);

// ../main/character.cpp :
typedef enum
{
    Bytes,
    Chars,
    Width
} nchar_type;
int R_nchar(SEXP string, nchar_type type_, Rboolean allowNA, Rboolean keepNA, const char* msg_name);
Rboolean Rf_pmatch(SEXP formal, SEXP tag, Rboolean exact);
Rboolean Rf_psmatch(const char *f, const char *t, Rboolean exact);
SEXP R_ParseEvalString(const char *, SEXP);
void Rf_PrintValue(SEXP);
void Rf_printwhere(void);
#ifndef INLINE_PROTECT
SEXP Rf_protect(SEXP);
#endif
void Rf_readS3VarsFromFrame(SEXP rho, SEXP *dotGeneric, SEXP *dotGroup, SEXP *dotClass, SEXP *dotMethod, SEXP *dotGenericCallEnv, SEXP *dotGenericDefEnv);
SEXP Rf_setAttrib(SEXP vec, SEXP name, SEXP val);
void Rf_setSVector(SEXP *vec, int len, SEXP val);
void Rf_setVar(SEXP symbol, SEXP value, SEXP rho);
SEXP Rf_stringSuffix(SEXP string, int fromIndex);
SEXPTYPE Rf_str2type(const char *const s);
Rboolean Rf_StringBlank(SEXP x);
SEXP Rf_substitute(SEXP lang, SEXP rho);
SEXP Rf_topenv(SEXP target, SEXP envir);
const char *Rf_translateChar(SEXP);
const char *Rf_translateChar0(SEXP);
const char *Rf_translateCharUTF8(SEXP);
const char *Rf_type2char(SEXPTYPE);
SEXP Rf_type2rstr(SEXPTYPE);
SEXP Rf_type2str(SEXPTYPE);
SEXP Rf_type2str_nowarn(SEXPTYPE);
#ifndef INLINE_PROTECT
void Rf_unprotect(int);
#endif
void Rf_unprotect_ptr(SEXP);

NORET void R_signal_protect_error(void);
NORET void R_signal_unprotect_error(void);
NORET void R_signal_reprotect_error(PROTECT_INDEX i);

#ifndef INLINE_PROTECT
void R_ProtectWithIndex(SEXP, PROTECT_INDEX *);
void R_Reprotect(SEXP, PROTECT_INDEX);
#endif
SEXP R_tryEval(SEXP e, SEXP env, int *ErrorOccurred);
SEXP R_tryEvalSilent(SEXP e, SEXP env, int *ErrorOccurred);
SEXP R_GetCurrentEnv();
const char *R_curErrorBuf();

Rboolean Rf_isS4(SEXP);
SEXP Rf_asS4(SEXP, Rboolean, int);
SEXP Rf_S3Class(SEXP);
int Rf_isBasicClass(const char *);

Rboolean R_cycle_detected(SEXP s, SEXP child);

/* cetype_t is an identifier reseved by POSIX, but it is
   well established as public.  Could remap by a #define though */
typedef enum
{
    CE_NATIVE = 0,
    CE_UTF8 = 1,
    CE_LATIN1 = 2,
    CE_BYTES = 3,
    CE_SYMBOL = 5,
    CE_ANY = 99
} cetype_t;

cetype_t Rf_getCharCE(SEXP);
SEXP Rf_mkCharCE(const char *, cetype_t);
SEXP Rf_mkCharLenCE(const char *, int, cetype_t);
const char *Rf_reEnc(const char *x, cetype_t ce_in, cetype_t ce_out, int subst);

				/* match(.) NOT reached : for -Wall */
#define error_return(msg)	{ Rf_error(msg);	   return R_NilValue; }
#define errorcall_return(cl,msg){ Rf_errorcall(cl, msg);   return R_NilValue; }

#ifdef __MAIN__
#undef extern
#undef LibExtern
#endif

/* Calling a function with arguments evaluated */
SEXP R_forceAndCall(SEXP e, int n, SEXP rho);

/* External pointer interface */
SEXP R_MakeExternalPtr(void *p, SEXP tag, SEXP prot);
void *R_ExternalPtrAddr(SEXP s);
SEXP R_ExternalPtrTag(SEXP s);
SEXP R_ExternalPtrProtected(SEXP s);
void R_ClearExternalPtr(SEXP s);
void R_SetExternalPtrAddr(SEXP s, void *p);
void R_SetExternalPtrTag(SEXP s, SEXP tag);
void R_SetExternalPtrProtected(SEXP s, SEXP p);
// Added in R 3.4.0
SEXP R_MakeExternalPtrFn(DL_FUNC p, SEXP tag, SEXP prot);
DL_FUNC R_ExternalPtrAddrFn(SEXP s);

/* Finalization interface */
typedef void (*R_CFinalizer_t)(SEXP);
void R_RegisterFinalizer(SEXP s, SEXP fun);
void R_RegisterCFinalizer(SEXP s, R_CFinalizer_t fun);
void R_RegisterFinalizerEx(SEXP s, SEXP fun, Rboolean onexit);
void R_RegisterCFinalizerEx(SEXP s, R_CFinalizer_t fun, Rboolean onexit);
void R_RunPendingFinalizers(void);

/* Weak reference interface */
SEXP R_MakeWeakRef(SEXP key, SEXP val, SEXP fin, Rboolean onexit);
SEXP R_MakeWeakRefC(SEXP key, SEXP val, R_CFinalizer_t fin, Rboolean onexit);
SEXP R_WeakRefKey(SEXP w);
SEXP R_WeakRefValue(SEXP w);
void R_RunWeakRefFinalizer(SEXP w);

SEXP R_PromiseExpr(SEXP);
SEXP R_ClosureExpr(SEXP);
SEXP R_BytecodeExpr(SEXP e);
void R_initialize_bcode(void);
SEXP R_bcEncode(SEXP);
SEXP R_bcDecode(SEXP);
void R_registerBC(SEXP, SEXP);
Rboolean R_checkConstants(Rboolean);
Rboolean R_BCVersionOK(SEXP);
#define PREXPR(e) R_PromiseExpr(e)
#define BODY_EXPR(e) R_ClosureExpr(e)

void R_init_altrep();
void R_reinit_altrep_classes(DllInfo *);

/* Protected evaluation */
Rboolean R_ToplevelExec(void (*fun)(void *), void *data);
SEXP R_ExecWithCleanup(SEXP (*fun)(void *), void *data, void (*cleanfun)(void *), void *cleandata);
SEXP R_tryCatch(SEXP (*)(void *), void *,       /* body closure*/
		SEXP,                           /* condition classes (STRSXP) */
		SEXP (*)(SEXP, void *), void *, /* handler closure */
		void (*)(void *), void *);      /* finally closure */
SEXP R_tryCatchError(SEXP (*)(void *), void *,        /* body closure*/
		     SEXP (*)(SEXP, void *), void *); /* handler closure */
SEXP R_withCallingErrorHandler(SEXP (*)(void *), void *, /* body closure*/
			       SEXP (*)(SEXP, void *), void *); /* handler closure */
SEXP R_MakeUnwindCont();
NORET void R_ContinueUnwind(SEXP cont);
SEXP R_UnwindProtect(SEXP (*fun)(void *data), void *data,
                     void (*cleanfun)(void *data, Rboolean jump),
                     void *cleandata, SEXP cont);

/* Environment and Binding Features */
void R_RestoreHashCount(SEXP rho);
Rboolean R_IsPackageEnv(SEXP rho);
SEXP R_PackageEnvName(SEXP rho);
SEXP R_FindPackageEnv(SEXP info);
Rboolean R_IsNamespaceEnv(SEXP rho);
SEXP R_NamespaceEnvSpec(SEXP rho);
SEXP R_FindNamespace(SEXP info);
void R_LockEnvironment(SEXP env, Rboolean bindings);
Rboolean R_EnvironmentIsLocked(SEXP env);
void R_LockBinding(SEXP sym, SEXP env);
void R_unLockBinding(SEXP sym, SEXP env);
void R_MakeActiveBinding(SEXP sym, SEXP fun, SEXP env);
Rboolean R_BindingIsLocked(SEXP sym, SEXP env);
Rboolean R_BindingIsActive(SEXP sym, SEXP env);
SEXP R_ActiveBindingFunction(SEXP sym, SEXP env);
Rboolean R_HasFancyBindings(SEXP rho);


/* ../main/errors.cpp : */
/* needed for R_load/savehistory handling in front ends */
#if defined(__GNUC__) && __GNUC__ >= 3
void Rf_errorcall(SEXP, const char *, ...) __attribute__((noreturn));
#else
void Rf_errorcall(SEXP, const char *, ...);
#endif
void warningcall(SEXP, const char *, ...);
void Rf_warningcall_immediate(SEXP, const char *, ...);

/* Save/Load Interface */
#define R_XDR_DOUBLE_SIZE 8
#define R_XDR_INTEGER_SIZE 4

void R_XDREncodeDouble(double d, void *buf);
double R_XDRDecodeDouble(void *buf);
void R_XDREncodeInteger(int i, void *buf);
int R_XDRDecodeInteger(void *buf);

typedef void *R_pstream_data_t;

typedef enum
{
    R_pstream_any_format,
    R_pstream_ascii_format,
    R_pstream_binary_format,
    R_pstream_xdr_format,
    R_pstream_asciihex_format
} R_pstream_format_t;

typedef struct R_outpstream_st *R_outpstream_t;
struct R_outpstream_st
{
    R_pstream_data_t data;
    R_pstream_format_t type;
    int version;
    void (*OutChar)(R_outpstream_t, int);
    void (*OutBytes)(R_outpstream_t, /*const*/ void *, int);
    SEXP(*OutPersistHookFunc)
    (SEXP, SEXP);
    SEXP OutPersistHookData;
};

typedef struct R_inpstream_st *R_inpstream_t;
#define R_CODESET_MAX 63
struct R_inpstream_st
{
    R_pstream_data_t data;
    R_pstream_format_t type;
    int (*InChar)(R_inpstream_t);
    void (*InBytes)(R_inpstream_t, void *, int);
    SEXP (*InPersistHookFunc)(SEXP, SEXP);
    SEXP InPersistHookData;
    char native_encoding[R_CODESET_MAX + 1];
    void *nat2nat_obj;
    void *nat2utf8_obj;
};

void R_InitInPStream(R_inpstream_t stream, R_pstream_data_t data,
		     R_pstream_format_t type,
		     int (*inchar)(R_inpstream_t),
		     void (*inbytes)(R_inpstream_t, void *, int),
		     SEXP (*phook)(SEXP, SEXP), SEXP pdata);
void R_InitOutPStream(R_outpstream_t stream, R_pstream_data_t data,
		      R_pstream_format_t type, int version,
		      void (*outchar)(R_outpstream_t, int),
		      void (*outbytes)(R_outpstream_t, /*const*/ void *, int),
		      SEXP (*phook)(SEXP, SEXP), SEXP pdata);

#ifdef __cplusplus
void R_InitFileInPStream(R_inpstream_t stream, std::FILE *fp,
			 R_pstream_format_t type,
			 SEXP (*phook)(SEXP, SEXP), SEXP pdata);
void R_InitFileOutPStream(R_outpstream_t stream, std::FILE *fp,
			  R_pstream_format_t type, int version,
			  SEXP (*phook)(SEXP, SEXP), SEXP pdata);
#else
void R_InitFileInPStream(R_inpstream_t stream, FILE *fp,
			 R_pstream_format_t type,
			 SEXP (*phook)(SEXP, SEXP), SEXP pdata);
void R_InitFileOutPStream(R_outpstream_t stream, FILE *fp,
			  R_pstream_format_t type, int version,
			  SEXP (*phook)(SEXP, SEXP), SEXP pdata);
#endif

#ifdef NEED_CONNECTION_PSTREAMS
/* The connection interface is not available to packages.  To
   allow limited use of connection pointers this defines the opaque
   pointer type. */
#ifndef HAVE_RCONNECTION_TYPEDEF
typedef struct Rconn  *Rconnection;
#define HAVE_RCONNECTION_TYPEDEF
#endif
void R_InitConnOutPStream(R_outpstream_t stream, Rconnection con,
			  R_pstream_format_t type, int version,
			  SEXP (*phook)(SEXP, SEXP), SEXP pdata);
void R_InitConnInPStream(R_inpstream_t stream,  Rconnection con,
			 R_pstream_format_t type,
			 SEXP (*phook)(SEXP, SEXP), SEXP pdata);
#endif

void R_Serialize(SEXP s, R_outpstream_t ops);
SEXP R_Unserialize(R_inpstream_t ips);
SEXP R_SerializeInfo(R_inpstream_t ips);

/* slot management (in attrib.cpp) */
SEXP R_do_slot(SEXP obj, SEXP name);
SEXP R_do_slot_assign(SEXP obj, SEXP name, SEXP value);
int R_has_slot(SEXP obj, SEXP name);
/* S3-S4 class (inheritance), attrib.cpp */
SEXP R_S4_extends(SEXP klass, SEXP useTable);

/* class definition, new objects (objects.cpp) */
SEXP R_do_MAKE_CLASS(const char *what);
SEXP R_getClassDef(const char *what);
SEXP R_getClassDef_R(SEXP what);
Rboolean R_has_methods_attached(void);
Rboolean R_isVirtualClass(SEXP class_def, SEXP env);
Rboolean R_extends(SEXP class1, SEXP class2, SEXP env);
SEXP R_do_new_object(SEXP class_def);
/* supporting  a C-level version of  is(., .) : */
int R_check_class_and_super(SEXP x, const char **valid, SEXP rho);
int R_check_class_etc(SEXP x, const char **valid);

/* preserve objects across GCs */
void R_PreserveObject(SEXP);
void R_ReleaseObject(SEXP);

SEXP R_NewPreciousMSet(int);
void R_PreserveInMSet(SEXP x, SEXP mset);
void R_ReleaseFromMSet(SEXP x, SEXP mset);
void R_ReleaseMSet(SEXP mset, int keepSize);

/* Shutdown actions */
void R_dot_Last(void);		/* in main.cpp */
void R_RunExitFinalizers(void);	/* in memory.cpp */

/* Replacements for popen and system */
#ifdef HAVE_POPEN
# ifdef __cplusplus
std::FILE *R_popen(const char *, const char *);
# else
FILE *R_popen(const char *, const char *);
# endif
#endif
int R_system(const char *);

/* R_compute_identical:  C version of identical() function
   The third arg to R_compute_identical() consists of bitmapped flags for non-default options:
   currently the first 4 default to TRUE, so the flag is set for FALSE values:
   1 = !NUM_EQ
   2 = !SINGLE_NA
   4 = !ATTR_AS_SET
   8 = !IGNORE_BYTECODE
  16 = !IGNORE_ENV
  Default from R's default: 16
*/
Rboolean R_compute_identical(SEXP, SEXP, int);

SEXP R_body_no_src(SEXP x); // body(x) without "srcref" etc, ../main/utils.cpp

/* C version of R's  indx <- order(..., na.last, decreasing) :
   e.g.  arglist = Rf_lang2(x,y)  or  Rf_lang3(x,y,z) */
void R_orderVector(int *indx, int n, SEXP arglist, Rboolean nalast, Rboolean decreasing);
// C version of R's  indx <- order(x, na.last, decreasing) :
void R_orderVector1(int *indx, int n, SEXP x, Rboolean nalast, Rboolean decreasing);

#ifdef R_NO_REMAP
#else
#define acopy_string		Rf_acopy_string
#define addMissingVarsToNewEnv	Rf_addMissingVarsToNewEnv
#define alloc3DArray            Rf_alloc3DArray
#define allocArray		Rf_allocArray
#define allocFormalsList2	Rf_allocFormalsList2
#define allocFormalsList3	Rf_allocFormalsList3
#define allocFormalsList4	Rf_allocFormalsList4
#define allocFormalsList5	Rf_allocFormalsList5
#define allocFormalsList6	Rf_allocFormalsList6
#define allocList		Rf_allocList
#define allocMatrix		Rf_allocMatrix
#define allocS4Object		Rf_allocS4Object
#define allocSExp		Rf_allocSExp
#define allocVector		Rf_allocVector
#define allocVector3		Rf_allocVector3
#define any_duplicated		Rf_any_duplicated
#define any_duplicated3		Rf_any_duplicated3
#define applyClosure		Rf_applyClosure
#define arraySubscript		Rf_arraySubscript
#define asChar			Rf_asChar
#define asCharacterFactor	Rf_asCharacterFactor
#define asComplex		Rf_asComplex
#define asInteger		Rf_asInteger
#define asLogical		Rf_asLogical
#define asLogical2		Rf_asLogical2
#define asReal			Rf_asReal
#define asS4			Rf_asS4
#define classgets		Rf_classgets
#define coerceVector		Rf_coerceVector
#define conformable		Rf_conformable
#define cons			Rf_cons
#define fixSubset3Args		Rf_fixSubset3Args
#define copyListMatrix		Rf_copyListMatrix
#define copyMatrix		Rf_copyMatrix
#define copyMostAttrib		Rf_copyMostAttrib
#define copyVector		Rf_copyVector
#define countContexts		Rf_countContexts
#define CreateTag		Rf_CreateTag
#define defineVar		Rf_defineVar
#define dimgets			Rf_dimgets
#define dimnamesgets		Rf_dimnamesgets
#define DropDims                Rf_DropDims
#define duplicate		Rf_duplicate
#define duplicated		Rf_duplicated
#define elt			Rf_elt
#define errorcall		Rf_errorcall
#define eval			Rf_eval
#define ExtractSubset		Rf_ExtractSubset
#define findFun			Rf_findFun
#define findFun3		Rf_findFun3
#define findFunctionForBody	Rf_findFunctionForBody
#define findVar			Rf_findVar
#define findVarInFrame		Rf_findVarInFrame
#define findVarInFrame3		Rf_findVarInFrame3
#define FixupDigits		Rf_FixupDigits
#define FixupWidth		Rf_FixupWidth
#define GetArrayDimnames	Rf_GetArrayDimnames
#define getAttrib		Rf_getAttrib
#define getCharCE		Rf_getCharCE
#define GetColNames		Rf_GetColNames
#define GetMatrixDimnames	Rf_GetMatrixDimnames
#define GetOption1		Rf_GetOption1
#define GetOptionDigits		Rf_GetOptionDigits
#define GetOptionWidth		Rf_GetOptionWidth
#define GetOption		Rf_GetOption
#define GetRowNames		Rf_GetRowNames
#define gsetVar			Rf_gsetVar
#define inherits		Rf_inherits
#define install			Rf_install
#define installChar		Rf_installTrChar
#define installNoTrChar		Rf_installNoTrChar
#define installTrChar		Rf_installTrChar
#define installDDVAL		Rf_installDDVAL
#define installS3Signature	Rf_installS3Signature
#define isArray			Rf_isArray
#define isBasicClass            Rf_isBasicClass
#define isComplex		Rf_isComplex
#define isEnvironment		Rf_isEnvironment
#define isExpression		Rf_isExpression
#define isFactor		Rf_isFactor
#define isFrame			Rf_isFrame
#define isFree			Rf_isFree
#define isFunction		Rf_isFunction
#define isInteger		Rf_isInteger
#define isLanguage		Rf_isLanguage
#define isList			Rf_isList
#define isLogical		Rf_isLogical
#define isSymbol		Rf_isSymbol
#define isMatrix		Rf_isMatrix
#define isNewList		Rf_isNewList
#define isNull			Rf_isNull
#define isNumeric		Rf_isNumeric
#define isNumber		Rf_isNumber
#define isObject		Rf_isObject
#define isRaw   		Rf_isRaw
#define isOrdered		Rf_isOrdered
#define isPairList		Rf_isPairList
#define isPrimitive		Rf_isPrimitive
#define isReal			Rf_isReal
#define isS4			Rf_isS4
#define isString		Rf_isString
#define isTs			Rf_isTs
#define isUnmodifiedSpecSym	Rf_isUnmodifiedSpecSym
#define isUnordered		Rf_isUnordered
#define isUnsorted		Rf_isUnsorted
#define isUserBinop		Rf_isUserBinop
#define isValidString		Rf_isValidString
#define isValidStringF		Rf_isValidStringF
#define isVector		Rf_isVector
#define isVectorAtomic		Rf_isVectorAtomic
#define isVectorizable		Rf_isVectorizable
#define isVectorList		Rf_isVectorList
#define lang1			Rf_lang1
#define lang2			Rf_lang2
#define lang3			Rf_lang3
#define lang4			Rf_lang4
#define lang5			Rf_lang5
#define lang6			Rf_lang6
#define lastElt			Rf_lastElt
#define lazy_duplicate		Rf_lazy_duplicate
#define lcons			Rf_lcons
#ifndef __cplusplus
#define length(x)		Rf_length(x)
#endif
#define lengthgets		Rf_lengthgets
#define list1			Rf_list1
#define list2			Rf_list2
#define list3			Rf_list3
#define list4			Rf_list4
#define list5			Rf_list5
#define list6			Rf_list6
#define listAppend		Rf_listAppend
#define match			Rf_match
#define matchE			Rf_matchE
#define mkChar			Rf_mkChar
#define mkCharCE		Rf_mkCharCE
#define mkCharLen		Rf_mkCharLen
#define mkCharLenCE		Rf_mkCharLenCE
#define mkNamed			Rf_mkNamed
#define mkString		Rf_mkString
#define namesgets		Rf_namesgets
#define ncols			Rf_ncols
#define nlevels			Rf_nlevels
#define NonNullStringMatch	Rf_NonNullStringMatch
#define nrows			Rf_nrows
#define nthcdr			Rf_nthcdr
#define PairToVectorList	Rf_PairToVectorList
#define pmatch			Rf_pmatch
#define psmatch			Rf_psmatch
#define PrintValue		Rf_PrintValue
#define printwhere		Rf_printwhere
#define protect			Rf_protect
#define readS3VarsFromFrame	Rf_readS3VarsFromFrame
#define reEnc			Rf_reEnc
#define S3Class                 Rf_S3Class
#define ScalarComplex		Rf_ScalarComplex
#define ScalarInteger		Rf_ScalarInteger
#define ScalarLogical		Rf_ScalarLogical
#define ScalarReal		Rf_ScalarReal
#define ScalarString		Rf_ScalarString
#define ScalarRaw		Rf_ScalarRaw
#define setAttrib		Rf_setAttrib
#define setSVector		Rf_setSVector
#define setVar			Rf_setVar
#define shallow_duplicate	Rf_shallow_duplicate
#define str2type		Rf_str2type
#define stringSuffix		Rf_stringSuffix
#define stringPositionTr	Rf_stringPositionTr
#define StringBlank		Rf_StringBlank
#define substitute		Rf_substitute
#define topenv		        Rf_topenv
#define translateChar		Rf_translateChar
#define translateChar0		Rf_translateChar0
#define translateCharUTF8      	Rf_translateCharUTF8
#define type2char		Rf_type2char
#define type2rstr		Rf_type2rstr
#define type2str		Rf_type2str
#define type2str_nowarn		Rf_type2str_nowarn
#define unprotect		Rf_unprotect
#define unprotect_ptr		Rf_unprotect_ptr
#define VectorToPairList	Rf_VectorToPairList
#define warningcall_immediate	Rf_warningcall_immediate
#define xlength(x)		Rf_xlength(x)
#define xlengthgets		Rf_xlengthgets
#endif
#define Rf_warningcall		warningcall

/* Defining NO_RINLINEDFUNS disables use to simulate platforms where
   this is not available */
#if defined(CALLED_FROM_DEFN_H) && !defined(__MAIN__) && \
   (defined(COMPILING_R) || (__GNUC__ && !defined(__INTEL_COMPILER))) && \
   (defined(COMPILING_R) || !defined(NO_RINLINEDFUNS))
#include "Rinlinedfuns.h"
#else
/* need remapped names here for use with R_NO_REMAP */

/*
   These are the inlinable functions that are provided in Rinlinedfuns.h
   It is *essential* that these do not appear in any other header file,
   with or without the Rf_ prefix.
*/
SEXP     Rf_allocVector(SEXPTYPE, R_xlen_t);
Rboolean Rf_conformable(SEXP, SEXP);
SEXP	 Rf_elt(SEXP, int);
Rboolean Rf_inherits(SEXP, const char *);
Rboolean Rf_isArray(SEXP);
Rboolean Rf_isFactor(SEXP);
Rboolean Rf_isFrame(SEXP);
Rboolean Rf_isFunction(SEXP);
Rboolean Rf_isInteger(SEXP);
Rboolean Rf_isLanguage(SEXP);
Rboolean Rf_isList(SEXP);
Rboolean Rf_isMatrix(SEXP);
Rboolean Rf_isNewList(SEXP);
Rboolean Rf_isNumber(SEXP);
Rboolean Rf_isNumeric(SEXP);
Rboolean Rf_isPairList(SEXP);
Rboolean Rf_isPrimitive(SEXP);
Rboolean Rf_isTs(SEXP);
Rboolean Rf_isUserBinop(SEXP);
Rboolean Rf_isValidString(SEXP);
Rboolean Rf_isValidStringF(SEXP);
Rboolean Rf_isVector(SEXP);
Rboolean Rf_isVectorAtomic(SEXP);
Rboolean Rf_isVectorList(SEXP);
Rboolean Rf_isVectorizable(SEXP);
SEXP	 Rf_lang1(SEXP);
SEXP	 Rf_lang2(SEXP, SEXP);
SEXP	 Rf_lang3(SEXP, SEXP, SEXP);
SEXP	 Rf_lang4(SEXP, SEXP, SEXP, SEXP);
SEXP	 Rf_lang5(SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP	 Rf_lang6(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP	 Rf_lastElt(SEXP);
SEXP	 Rf_lcons(SEXP, SEXP);
R_len_t  Rf_length(SEXP);
SEXP	 Rf_list1(SEXP);
SEXP	 Rf_list2(SEXP, SEXP);
SEXP	 Rf_list3(SEXP, SEXP, SEXP);
SEXP	 Rf_list4(SEXP, SEXP, SEXP, SEXP);
SEXP	 Rf_list5(SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP	 Rf_list6(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP	 Rf_listAppend(SEXP, SEXP);
SEXP	 Rf_mkNamed(SEXPTYPE, const char **);
SEXP	 Rf_mkString(const char *);
int	 Rf_nlevels(SEXP);
int	 Rf_stringPositionTr(SEXP, const char *);
SEXP	 Rf_ScalarComplex(Rcomplex);
SEXP	 Rf_ScalarInteger(int);
SEXP	 Rf_ScalarLogical(int);
SEXP	 Rf_ScalarRaw(Rbyte);
SEXP	 Rf_ScalarReal(double);
SEXP	 Rf_ScalarString(SEXP);
R_xlen_t  Rf_xlength(SEXP);
R_xlen_t  (XLENGTH)(SEXP x);
R_xlen_t  (XTRUELENGTH)(SEXP x);
int LENGTH_EX(SEXP x, const char *file, int line);
R_xlen_t XLENGTH_EX(SEXP x);
# ifdef INLINE_PROTECT
SEXP Rf_protect(SEXP);
void Rf_unprotect(int);
void R_ProtectWithIndex(SEXP, PROTECT_INDEX *);
void R_Reprotect(SEXP, PROTECT_INDEX);
# endif
SEXP R_FixupRHS(SEXP x, SEXP y);
SEXP (CAR)(SEXP e);
void *(DATAPTR)(SEXP x);
const void *(DATAPTR_RO)(SEXP x);
const void *(DATAPTR_OR_NULL)(SEXP x);
const int *(LOGICAL_OR_NULL)(SEXP x);
const int *(INTEGER_OR_NULL)(SEXP x);
const double *(REAL_OR_NULL)(SEXP x);
const Rcomplex *(COMPLEX_OR_NULL)(SEXP x);
const Rbyte *(RAW_OR_NULL)(SEXP x);
void *(STDVEC_DATAPTR)(SEXP x);
int (INTEGER_ELT)(SEXP x, R_xlen_t i);
double (REAL_ELT)(SEXP x, R_xlen_t i);
int (LOGICAL_ELT)(SEXP x, R_xlen_t i);
Rcomplex (COMPLEX_ELT)(SEXP x, R_xlen_t i);
Rbyte (RAW_ELT)(SEXP x, R_xlen_t i);
SEXP (STRING_ELT)(SEXP x, R_xlen_t i);
double SCALAR_DVAL(SEXP x);
int SCALAR_LVAL(SEXP x);
int SCALAR_IVAL(SEXP x);
void SET_SCALAR_DVAL(SEXP x, double v);
void SET_SCALAR_LVAL(SEXP x, int v);
void SET_SCALAR_IVAL(SEXP x, int v);
void SET_SCALAR_CVAL(SEXP x, Rcomplex v);
void SET_SCALAR_BVAL(SEXP x, Rbyte v);
SEXP R_altrep_data1(SEXP x);
SEXP R_altrep_data2(SEXP x);
void R_set_altrep_data1(SEXP x, SEXP v);
void R_set_altrep_data2(SEXP x, SEXP v);
SEXP ALTREP_CLASS(SEXP x);
int *LOGICAL0(SEXP x);
int *INTEGER0(SEXP x);
double *REAL0(SEXP x);
Rcomplex *COMPLEX0(SEXP x);
Rbyte *RAW0(SEXP x);
void SET_LOGICAL_ELT(SEXP x, R_xlen_t i, int v);
void SET_INTEGER_ELT(SEXP x, R_xlen_t i, int v);
void SET_REAL_ELT(SEXP x, R_xlen_t i, double v);
#endif

#if defined(USE_RINTERNALS) || defined(TESTING_WRITE_BARRIER) || defined(COMPILING_IVORY)

/* Test macros with function versions above */
// These macros are required by stringi package.

#undef isNull
#define isNull(s) (TYPEOF(s) == NILSXP)
#undef isSymbol
#define isSymbol(s) (TYPEOF(s) == SYMSXP)
#undef isLogical
#define isLogical(s) (TYPEOF(s) == LGLSXP)
#undef isReal
#define isReal(s) (TYPEOF(s) == REALSXP)
#undef isComplex
#define isComplex(s) (TYPEOF(s) == CPLXSXP)
#undef isExpression
#define isExpression(s) (TYPEOF(s) == EXPRSXP)
#undef isEnvironment
#define isEnvironment(s) (TYPEOF(s) == ENVSXP)
#undef isString
#define isString(s) (TYPEOF(s) == STRSXP)
#undef isObject
#define isObject(s) (OBJECT(s) != 0)
#undef isRaw
#define isRaw(s) (TYPEOF(s) == RAWSXP)

#endif

void R_BadValueInRCode(SEXP value, SEXP call, SEXP rho, const char *rawmsg,
        const char *errmsg, const char *warnmsg, const char *varname,
        Rboolean warnByDefault);

#ifdef __cplusplus
}
#endif

#if defined(R_NO_REMAP) && defined(COMPILING_IVORY) && defined(__cplusplus)
const auto acopy_string = Rf_acopy_string;
const auto addMissingVarsToNewEnv = Rf_addMissingVarsToNewEnv;
const auto alloc3DArray = Rf_alloc3DArray;
const auto allocArray = Rf_allocArray;
const auto allocFormalsList2 = Rf_allocFormalsList2;
const auto allocFormalsList3 = Rf_allocFormalsList3;
const auto allocFormalsList4 = Rf_allocFormalsList4;
const auto allocFormalsList5 = Rf_allocFormalsList5;
const auto allocFormalsList6 = Rf_allocFormalsList6;
const auto allocList = Rf_allocList;
const auto allocMatrix = Rf_allocMatrix;
const auto allocS4Object = Rf_allocS4Object;
const auto allocSExp = Rf_allocSExp;
const auto allocVector = Rf_allocVector;
const auto allocVector3 = Rf_allocVector3;
const auto any_duplicated = Rf_any_duplicated;
const auto any_duplicated3 = Rf_any_duplicated3;
const auto applyClosure = Rf_applyClosure;
const auto arraySubscript = Rf_arraySubscript;
const auto asChar = Rf_asChar;
const auto asCharacterFactor = Rf_asCharacterFactor;
const auto asComplex = Rf_asComplex;
const auto asInteger = Rf_asInteger;
const auto asLogical = Rf_asLogical;
const auto asLogical2 = Rf_asLogical2;
const auto asReal = Rf_asReal;
const auto asS4	= Rf_asS4;
const auto classgets = Rf_classgets;
const auto coerceVector = Rf_coerceVector;
const auto conformable = Rf_conformable;
const auto cons = Rf_cons;
const auto error = Rf_error;
const auto warning = Rf_warning;
const auto fixSubset3Args = Rf_fixSubset3Args;
const auto copyListMatrix = Rf_copyListMatrix;
const auto copyMatrix = Rf_copyMatrix;
const auto copyMostAttrib = Rf_copyMostAttrib;
const auto copyVector = Rf_copyVector;
const auto countContexts = Rf_countContexts;
const auto CreateTag = Rf_CreateTag;
const auto defineVar = Rf_defineVar;
const auto dimgets = Rf_dimgets;
const auto dimnamesgets = Rf_dimnamesgets;
const auto DropDims = Rf_DropDims;
const auto duplicate = Rf_duplicate;
const auto duplicated = Rf_duplicated;
const auto elt = Rf_elt;
const auto errorcall = Rf_errorcall;
const auto eval = Rf_eval;
const auto ExtractSubset = Rf_ExtractSubset;
const auto findFun = Rf_findFun;
const auto findFun3 = Rf_findFun3;
const auto findFunctionForBody = Rf_findFunctionForBody;
const auto findVar = Rf_findVar;
const auto findVarInFrame = Rf_findVarInFrame;
const auto findVarInFrame3 = Rf_findVarInFrame3;
const auto FixupDigits = Rf_FixupDigits;
const auto FixupWidth = Rf_FixupWidth;
const auto GetArrayDimnames = Rf_GetArrayDimnames;
const auto getAttrib = Rf_getAttrib;
const auto getCharCE = Rf_getCharCE;
const auto GetColNames = Rf_GetColNames;
const auto GetMatrixDimnames = Rf_GetMatrixDimnames;
const auto GetOption1 = Rf_GetOption1;
const auto GetOptionDigits = Rf_GetOptionDigits;
const auto GetOptionWidth = Rf_GetOptionWidth;
const auto GetOption = Rf_GetOption;
const auto GetRowNames = Rf_GetRowNames;
const auto gsetVar = Rf_gsetVar;
const auto inherits = Rf_inherits;
const auto install = Rf_install;
const auto installChar = Rf_installTrChar;
const auto installNoTrChar = Rf_installNoTrChar;
const auto installTrChar = Rf_installTrChar;
const auto installDDVAL = Rf_installDDVAL;
const auto installS3Signature = Rf_installS3Signature;
const auto isArray = Rf_isArray;
const auto isBasicClass = Rf_isBasicClass;
const auto isFactor = Rf_isFactor;
const auto isFrame = Rf_isFrame;
const auto isFree = Rf_isFree;
const auto isFunction = Rf_isFunction;
const auto isInteger = Rf_isInteger;
const auto isLanguage = Rf_isLanguage;
const auto isList = Rf_isList;
const auto isMatrix = Rf_isMatrix;
const auto isNewList = Rf_isNewList;
const auto isNumeric = Rf_isNumeric;
const auto isNumber = Rf_isNumber;
const auto isOrdered = Rf_isOrdered;
const auto isPairList = Rf_isPairList;
const auto isPrimitive = Rf_isPrimitive;
const auto isS4 = Rf_isS4;
const auto isTs = Rf_isTs;
const auto isUnmodifiedSpecSym = Rf_isUnmodifiedSpecSym;
const auto isUnordered = Rf_isUnordered;
const auto isUnsorted = Rf_isUnsorted;
const auto isUserBinop = Rf_isUserBinop;
const auto isValidString = Rf_isValidString;
const auto isValidStringF = Rf_isValidStringF;
const auto isVector = Rf_isVector;
const auto isVectorAtomic = Rf_isVectorAtomic;
const auto isVectorizable = Rf_isVectorizable;
const auto isVectorList = Rf_isVectorList;
const auto lang1 = Rf_lang1;
const auto lang2 = Rf_lang2;
const auto lang3 = Rf_lang3;
const auto lang4 = Rf_lang4;
const auto lang5 = Rf_lang5;
const auto lang6 = Rf_lang6;
const auto lastElt = Rf_lastElt;
const auto lazy_duplicate = Rf_lazy_duplicate;
const auto lcons = Rf_lcons;
const auto lengthgets = Rf_lengthgets;
const auto list1 = Rf_list1;
const auto list2 = Rf_list2;
const auto list3 = Rf_list3;
const auto list4 = Rf_list4;
const auto list5 = Rf_list5;
const auto list6 = Rf_list6;
const auto listAppend = Rf_listAppend;
const auto match = Rf_match;
const auto matchE = Rf_matchE;
const auto mkChar = Rf_mkChar;
const auto mkCharCE = Rf_mkCharCE;
const auto mkCharLen = Rf_mkCharLen;
const auto mkCharLenCE = Rf_mkCharLenCE;
const auto mkNamed = Rf_mkNamed;
const auto mkString = Rf_mkString;
const auto namesgets = Rf_namesgets;
const auto ncols = Rf_ncols;
const auto nlevels = Rf_nlevels;
const auto NonNullStringMatch = Rf_NonNullStringMatch;
const auto nrows = Rf_nrows;
const auto nthcdr = Rf_nthcdr;
const auto PairToVectorList = Rf_PairToVectorList;
const auto pmatch = Rf_pmatch;
const auto psmatch = Rf_psmatch;
const auto PrintValue = Rf_PrintValue;
const auto printwhere = Rf_printwhere;
const auto protect = Rf_protect;
const auto readS3VarsFromFrame = Rf_readS3VarsFromFrame;
const auto reEnc = Rf_reEnc;
const auto S3Class = Rf_S3Class;
const auto ScalarComplex = Rf_ScalarComplex;
const auto ScalarInteger = Rf_ScalarInteger;
const auto ScalarLogical = Rf_ScalarLogical;
const auto ScalarReal = Rf_ScalarReal;
const auto ScalarString = Rf_ScalarString;
const auto ScalarRaw = Rf_ScalarRaw;
const auto setAttrib = Rf_setAttrib;
const auto setSVector = Rf_setSVector;
const auto setVar = Rf_setVar;
const auto shallow_duplicate = Rf_shallow_duplicate;
const auto str2type = Rf_str2type;
const auto stringSuffix = Rf_stringSuffix;
const auto stringPositionTr = Rf_stringPositionTr;
const auto StringBlank = Rf_StringBlank;
const auto substitute = Rf_substitute;
const auto topenv = Rf_topenv;
const auto translateChar = Rf_translateChar;
const auto translateChar0 = Rf_translateChar0;
const auto translateCharUTF8 = Rf_translateCharUTF8;
const auto type2char = Rf_type2char;
const auto type2rstr = Rf_type2rstr;
const auto type2str = Rf_type2str;
const auto type2str_nowarn = Rf_type2str_nowarn;
const auto unprotect = Rf_unprotect;
const auto unprotect_ptr = Rf_unprotect_ptr;
const auto VectorToPairList = Rf_VectorToPairList;
const auto warningcall_immediate = Rf_warningcall_immediate;
const auto xlength = Rf_xlength;
const auto xlengthgets = Rf_xlengthgets;
#endif

#ifdef __cplusplus

/** @brief Shorthand for Rf_length().
 */
inline R_len_t length(SEXP s)
{
    return Rf_length(s);
}
#endif

#endif /* R_INTERNALS_H_ */
