/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2017  The R Core Team.
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

/* Internal header, not installed */

/* this header is always to be included from others.
   It is only called if COMPILING_R is defined (in util.cpp) or
   from GNU C systems.

   There are different conventions for inlining across compilation units.
   See http://www.greenend.org.uk/rjk/2003/03/inline.html
 */
#ifndef R_INLINES_H_
#define R_INLINES_H_

#ifndef __cplusplus
#error Rinlinedfuns.h can only be included in C++ files
#endif

#include <Localization.h>
#include <R_ext/Error.h>

#include <string.h> /* for strlen, strcmp */

/* define inline-able functions */
#ifdef TESTING_WRITE_BARRIER
# define STRICT_TYPECHECK
# define CATCH_ZERO_LENGTH_ACCESS
#endif


#if defined(USE_RINTERNALS) || defined(COMPILING_R)
/* inline version of CAR to support immediate bindings */
extern inline SEXP CAR(SEXP e)
{
    if (BNDCELL_TAG(e))
        Rf_error(_("bad binding access"));
    return CAR0(e);
}
#else
SEXP CAR(SEXP e);
#endif

#ifdef STRICT_TYPECHECK
extern inline void CHKVEC(SEXP x)
{
    switch (TYPEOF(x))
    {
    case CHARSXP:
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
    case VECSXP:
    case EXPRSXP:
    case RAWSXP:
    case WEAKREFSXP:
        break;
    default:
        Rf_error(_("cannot get data pointer of '%s' objects"), Rf_type2char(TYPEOF(x)));
    }
}
#else
# define CHKVEC(x) do {} while(0)
#endif

/**
 * @brief The general data pointer function
 * 
 * Function works as a dispatcher between ALTREP
 * or STDVEC representation of data.
 * 
 * @return pointer to the data block
 */
extern inline void *CXXR::DATAPTR(SEXP x)
{
    CHKVEC(x);
    if (ALTREP(x))
        return ALTVEC_DATAPTR(x);
#ifdef CATCH_ZERO_LENGTH_ACCESS
    /* Attempts to read or write elements of a zero length vector will
       result in a segfault, rather than read and write random memory.
       Returning NULL would be more natural, but Matrix seems to assume
       that even zero-length vectors have non-NULL data pointers, so
       return (void *) 1 instead. Zero-length CHARSXP objects still
       have a trailing zero byte so they are not handled. */
    else if (STDVEC_LENGTH(x) == 0 && TYPEOF(x) != CHARSXP)
        return (void *)1;
#endif
    else
        return CXXR::stdvec_dataptr(x);
}

/**
 * @brief The general (read only) data pointer function
 * 
 * Function works as a dispatcher between ALTREP
 * or STDVEC representation of data.
 * 
 * @return pointer to the (read only) data block
 */
extern inline const void *CXXR::DATAPTR_RO(SEXP x)
{
    CHKVEC(x);
    if (ALTREP(x))
        return ALTVEC_DATAPTR_RO(x);
    else
        return CXXR::stdvec_dataptr(x);
}

extern inline const void *DATAPTR_OR_NULL(SEXP x)
{
    CHKVEC(x);
    if (ALTREP(x))
        return ALTVEC_DATAPTR_OR_NULL(x);
    else
        return CXXR::stdvec_dataptr(x);
}

#ifdef STRICT_TYPECHECK
#define CHECK_VECTOR_LGL(x)                         \
    do                                              \
    {                                               \
        if (TYPEOF(x) != LGLSXP)                    \
            Rf_error(_("bad %s vector"), "LGLSXP"); \
    } while (0)
#define CHECK_VECTOR_INT(x)                                \
    do                                                     \
    {                                                      \
        if (!(TYPEOF(x) == INTSXP || TYPEOF(x) == LGLSXP)) \
            Rf_error(_("bad %s vector"), "INTSXP");        \
    } while (0)
#define CHECK_VECTOR_REAL(x)                         \
    do                                               \
    {                                                \
        if (TYPEOF(x) != REALSXP)                    \
            Rf_error(_("bad %s vector"), "REALSXP"); \
    } while (0)
#define CHECK_VECTOR_CPLX(x)                         \
    do                                               \
    {                                                \
        if (TYPEOF(x) != CPLXSXP)                    \
            Rf_error(_("bad %s vector"), "CPLXSXP"); \
    } while (0)
#define CHECK_VECTOR_RAW(x)                         \
    do                                              \
    {                                               \
        if (TYPEOF(x) != RAWSXP)                    \
            Rf_error(_("bad %s vector"), "RAWSXP"); \
    } while (0)
#else
#define CHECK_VECTOR_LGL(x) do { } while(0)
#define CHECK_VECTOR_INT(x) do { } while(0)
#define CHECK_VECTOR_REAL(x) do { } while(0)
#define CHECK_VECTOR_CPLX(x) do { } while(0)
#define CHECK_VECTOR_RAW(x) do { } while(0)
#endif

extern inline const int *LOGICAL_OR_NULL(SEXP x)
{
    CHECK_VECTOR_LGL(x);
    return (int *)(ALTREP(x) ? ALTVEC_DATAPTR_OR_NULL(x) : CXXR::stdvec_dataptr(x));
}

extern inline const int *INTEGER_OR_NULL(SEXP x)
{
    CHECK_VECTOR_INT(x);
    return (int *)(ALTREP(x) ? ALTVEC_DATAPTR_OR_NULL(x) : CXXR::stdvec_dataptr(x));
}

extern inline const double *REAL_OR_NULL(SEXP x)
{
    CHECK_VECTOR_REAL(x);
    return (double *)(ALTREP(x) ? ALTVEC_DATAPTR_OR_NULL(x) : CXXR::stdvec_dataptr(x));
}

extern inline const Rcomplex *COMPLEX_OR_NULL(SEXP x)
{
    CHECK_VECTOR_CPLX(x);
    return (Rcomplex *)(ALTREP(x) ? ALTVEC_DATAPTR_OR_NULL(x) : CXXR::stdvec_dataptr(x));
}

extern inline const Rbyte *RAW_OR_NULL(SEXP x)
{
    CHECK_VECTOR_RAW(x);
    return (Rbyte *)(ALTREP(x) ? ALTVEC_DATAPTR_OR_NULL(x) : CXXR::stdvec_dataptr(x));
}

extern inline R_xlen_t XLENGTH_EX(SEXP x)
{
    return ALTREP(x) ? ALTREP_LENGTH(x) : CXXR::VectorBase::stdvec_length(x);
}

extern inline R_xlen_t XTRUELENGTH(SEXP x)
{
    return ALTREP(x) ? ALTREP_TRUELENGTH(x) : CXXR::VectorBase::stdvec_truelength(x);
}

extern inline int LENGTH_EX(SEXP x, const char *file, int line)
{
    if (!x || x == R_NilValue)
        return 0;
    R_xlen_t len = XLENGTH(x);
#ifdef LONG_VECTOR_SUPPORT
    if (len > R_SHORT_LEN_MAX)
        R_BadLongVector(x, file, line);
#endif
    return (int)len;
}

#ifdef STRICT_TYPECHECK
#define CHECK_STDVEC_LGL(x)                                  \
    do                                                       \
    {                                                        \
        CHECK_VECTOR_LGL(x);                                 \
        if (ALTREP(x))                                       \
            Rf_error(_("bad standard %s vector"), "LGLSXP"); \
    } while (0)
#define CHECK_STDVEC_INT(x)                                  \
    do                                                       \
    {                                                        \
        CHECK_VECTOR_INT(x);                                 \
        if (ALTREP(x))                                       \
            Rf_error(_("bad standard %s vector"), "INTSXP"); \
    } while (0)
#define CHECK_STDVEC_REAL(x)                                  \
    do                                                        \
    {                                                         \
        CHECK_VECTOR_REAL(x);                                 \
        if (ALTREP(x))                                        \
            Rf_error(_("bad standard %s vector"), "REALSXP"); \
    } while (0)
#define CHECK_STDVEC_CPLX(x)                                  \
    do                                                        \
    {                                                         \
        CHECK_VECTOR_CPLX(x);                                 \
        if (ALTREP(x))                                        \
            Rf_error(_("bad standard %s vector"), "CPLXSXP"); \
    } while (0)
#define CHECK_STDVEC_RAW(x)                                  \
    do                                                       \
    {                                                        \
        CHECK_VECTOR_RAW(x);                                 \
        if (ALTREP(x))                                       \
            Rf_error(_("bad standard %s vector"), "RAWSXP"); \
    } while (0)

#define CHECK_SCALAR_LGL(x)                         \
    do                                              \
    {                                               \
        CHECK_STDVEC_LGL(x);                        \
        if (XLENGTH(x) != 1)                        \
            Rf_error(_("bad %s scalar"), "LGLSXP"); \
    } while (0)
#define CHECK_SCALAR_INT(x)                         \
    do                                              \
    {                                               \
        CHECK_STDVEC_INT(x);                        \
        if (XLENGTH(x) != 1)                        \
            Rf_error(_("bad %s scalar"), "INTSXP"); \
    } while (0)
#define CHECK_SCALAR_REAL(x)                         \
    do                                               \
    {                                                \
        CHECK_STDVEC_REAL(x);                        \
        if (XLENGTH(x) != 1)                         \
            Rf_error(_("bad %s scalar"), "REALSXP"); \
    } while (0)
#define CHECK_SCALAR_CPLX(x)                         \
    do                                               \
    {                                                \
        CHECK_STDVEC_CPLX(x);                        \
        if (XLENGTH(x) != 1)                         \
            Rf_error(_("bad %s scalar"), "CPLXSXP"); \
    } while (0)
#define CHECK_SCALAR_RAW(x)                         \
    do                                              \
    {                                               \
        CHECK_STDVEC_RAW(x);                        \
        if (XLENGTH(x) != 1)                        \
            Rf_error(_("bad %s scalar"), "RAWSXP"); \
    } while (0)

#define CHECK_BOUNDS_ELT(x, i)                      \
    do                                              \
    {                                               \
        if (i < 0 || i > XLENGTH(x))                \
            Rf_error(_("subscript out of bounds")); \
    } while (0)

#define CHECK_VECTOR_LGL_ELT(x, i)          \
    do                                      \
    {                                       \
        SEXP ce__x__ = (x);                 \
        R_xlen_t ce__i__ = (i);             \
        CHECK_VECTOR_LGL(ce__x__);          \
        CHECK_BOUNDS_ELT(ce__x__, ce__i__); \
    } while (0)
#define CHECK_VECTOR_INT_ELT(x, i)          \
    do                                      \
    {                                       \
        SEXP ce__x__ = (x);                 \
        R_xlen_t ce__i__ = (i);             \
        CHECK_VECTOR_INT(ce__x__);          \
        CHECK_BOUNDS_ELT(ce__x__, ce__i__); \
    } while (0)
#define CHECK_VECTOR_REAL_ELT(x, i)         \
    do                                      \
    {                                       \
        SEXP ce__x__ = (x);                 \
        R_xlen_t ce__i__ = (i);             \
        CHECK_VECTOR_REAL(ce__x__);         \
        CHECK_BOUNDS_ELT(ce__x__, ce__i__); \
    } while (0)
#define CHECK_VECTOR_CPLX_ELT(x, i)         \
    do                                      \
    {                                       \
        SEXP ce__x__ = (x);                 \
        R_xlen_t ce__i__ = (i);             \
        CHECK_VECTOR_CPLX(ce__x__);         \
        CHECK_BOUNDS_ELT(ce__x__, ce__i__); \
    } while (0)
#define CHECK_VECTOR_RAW_ELT(x, i)          \
    do                                      \
    {                                       \
        SEXP ce__x__ = (x);                 \
        R_xlen_t ce__i__ = (i);             \
        CHECK_VECTOR_RAW(ce__x__);          \
        CHECK_BOUNDS_ELT(ce__x__, ce__i__); \
    } while (0)
#else
#define CHECK_STDVEC_LGL(x) do { } while(0)
#define CHECK_STDVEC_INT(x) do { } while(0)
#define CHECK_STDVEC_REAL(x) do { } while(0)
#define CHECK_STDVEC_CPLX(x) do { } while(0)
#define CHECK_STDVEC_RAW(x) do { } while(0)

#define CHECK_SCALAR_LGL(x) do { } while(0)
#define CHECK_SCALAR_INT(x) do { } while(0)
#define CHECK_SCALAR_REAL(x) do { } while(0)
#define CHECK_SCALAR_CPLX(x) do { } while(0)
#define CHECK_SCALAR_RAW(x) do { } while(0)

#define CHECK_VECTOR_LGL_ELT(x, i) do { } while(0)
#define CHECK_VECTOR_INT_ELT(x, i) do { } while(0)
#define CHECK_VECTOR_REAL_ELT(x, i) do { } while(0)
#define CHECK_VECTOR_CPLX_ELT(x, i) do { } while(0)
#define CHECK_VECTOR_RAW_ELT(x, i) do { } while(0)
#endif

extern inline int *LOGICAL0(SEXP x)
{
    CHECK_STDVEC_LGL(x);
    return (int *)STDVEC_DATAPTR(x);
}
extern inline int SCALAR_LVAL(SEXP x)
{
    CHECK_SCALAR_LGL(x);
    return (Rboolean)LOGICAL0(x)[0];
}
extern inline void SET_SCALAR_LVAL(SEXP x, Rboolean v)
{
    CHECK_SCALAR_LGL(x);
    LOGICAL0(x)[0] = v;
}

extern inline int *INTEGER0(SEXP x)
{
    CHECK_STDVEC_INT(x);
    return (int *)STDVEC_DATAPTR(x);
}
extern inline int SCALAR_IVAL(SEXP x)
{
    CHECK_SCALAR_INT(x);
    return INTEGER0(x)[0];
}
extern inline void SET_SCALAR_IVAL(SEXP x, int v)
{
    CHECK_SCALAR_INT(x);
    INTEGER0(x)[0] = v;
}

extern inline double *REAL0(SEXP x)
{
    CHECK_STDVEC_REAL(x);
    return (double *)STDVEC_DATAPTR(x);
}
extern inline double SCALAR_DVAL(SEXP x)
{
    CHECK_SCALAR_REAL(x);
    return REAL0(x)[0];
}
extern inline void SET_SCALAR_DVAL(SEXP x, double v)
{
    CHECK_SCALAR_REAL(x);
    REAL0(x)[0] = v;
}

extern inline Rcomplex *COMPLEX0(SEXP x)
{
    CHECK_STDVEC_CPLX(x);
    return (Rcomplex *)STDVEC_DATAPTR(x);
}
extern inline Rcomplex SCALAR_CVAL(SEXP x)
{
    CHECK_SCALAR_CPLX(x);
    return COMPLEX0(x)[0];
}
extern inline void SET_SCALAR_CVAL(SEXP x, Rcomplex v)
{
    CHECK_SCALAR_CPLX(x);
    COMPLEX0(x)[0] = v;
}

extern inline Rbyte *RAW0(SEXP x)
{
    CHECK_STDVEC_RAW(x);
    return (Rbyte *)STDVEC_DATAPTR(x);
}
extern inline Rbyte SCALAR_BVAL(SEXP x)
{
    CHECK_SCALAR_RAW(x);
    return RAW0(x)[0];
}
extern inline void SET_SCALAR_BVAL(SEXP x, Rbyte v)
{
    CHECK_SCALAR_RAW(x);
    RAW0(x)[0] = v;
}

extern inline SEXP ALTREP_CLASS(SEXP x) { return TAG(x); }

extern inline SEXP R_altrep_data1(SEXP x) { return CAR(x); }
extern inline SEXP R_altrep_data2(SEXP x) { return CDR(x); }
extern inline void R_set_altrep_data1(SEXP x, SEXP v) { SETCAR(x, v); }
extern inline void R_set_altrep_data2(SEXP x, SEXP v) { SETCDR(x, v); }

extern inline int INTEGER_ELT(SEXP x, R_xlen_t i)
{
    CHECK_VECTOR_INT_ELT(x, i);
    return ALTREP(x) ? ALTINTEGER_ELT(x, i) : INTEGER0(x)[i];
}

extern inline void SET_INTEGER_ELT(SEXP x, R_xlen_t i, int v)
{
    CHECK_VECTOR_INT_ELT(x, i);
    if (ALTREP(x))
        ALTINTEGER_SET_ELT(x, i, v);
    else
        INTEGER0(x)[i] = v;
}

extern inline int LOGICAL_ELT(SEXP x, R_xlen_t i)
{
    CHECK_VECTOR_LGL_ELT(x, i);
    return ALTREP(x) ? ALTLOGICAL_ELT(x, i) : LOGICAL0(x)[i];
}

extern inline void SET_LOGICAL_ELT(SEXP x, R_xlen_t i, int v)
{
    CHECK_VECTOR_LGL_ELT(x, i);
    if (ALTREP(x))
        ALTLOGICAL_SET_ELT(x, i, v);
    else
        LOGICAL0(x)[i] = v;
}

extern inline double REAL_ELT(SEXP x, R_xlen_t i)
{
    CHECK_VECTOR_REAL_ELT(x, i);
    return ALTREP(x) ? ALTREAL_ELT(x, i) : REAL0(x)[i];
}

extern inline void SET_REAL_ELT(SEXP x, R_xlen_t i, double v)
{
    CHECK_VECTOR_REAL_ELT(x, i);
    if (ALTREP(x))
        ALTREAL_SET_ELT(x, i, v);
    else
        REAL0(x)[i] = v;
}

extern inline Rcomplex COMPLEX_ELT(SEXP x, R_xlen_t i)
{
    CHECK_VECTOR_CPLX_ELT(x, i);
    return ALTREP(x) ? ALTCOMPLEX_ELT(x, i) : COMPLEX0(x)[i];
}

extern inline void SET_COMPLEX_ELT(SEXP x, R_xlen_t i, Rcomplex v)
{
    CHECK_VECTOR_CPLX_ELT(x, i);
    if (ALTREP(x))
        ALTCOMPLEX_SET_ELT(x, i, v);
    else
        COMPLEX0(x)[i] = v;
}

extern inline Rbyte RAW_ELT(SEXP x, R_xlen_t i)
{
    CHECK_VECTOR_RAW_ELT(x, i);
    return ALTREP(x) ? ALTRAW_ELT(x, i) : RAW0(x)[i];
}

extern inline void SET_RAW_ELT(SEXP x, R_xlen_t i, Rbyte v)
{
    CHECK_VECTOR_RAW_ELT(x, i);
    if (ALTREP(x))
        ALTRAW_SET_ELT(x, i, v);
    else
        RAW0(x)[i] = v;
}

#if !defined(COMPILING_R) && !defined(COMPILING_MEMORY_C) && \
    !defined(TESTING_WRITE_BARRIER)
/* if not inlining use version in memory.cpp with more error checking */
extern inline SEXP STRING_ELT(SEXP x, R_xlen_t i)
{
    if (ALTREP(x))
        return ALTSTRING_ELT(x, i);
    else
    {
        SEXP *ps = (SEXP *)STDVEC_DATAPTR(x);
        return ps[i];
    }
}
#else
SEXP STRING_ELT(SEXP x, R_xlen_t i);
#endif

/* from dstruct.cpp */

/*  length - length of objects  */

int Rf_envlength(SEXP rho);

/* TODO: a  Length(.) {say} which is length() + dispatch (S3 + S4) if needed
         for one approach, see do_seq_along() in ../main/seq.cpp
*/
extern inline R_len_t Rf_length(SEXP s)
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
        while (s && s != R_NilValue)
        {
            i++;
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

R_xlen_t Rf_envxlength(SEXP rho);

extern inline R_xlen_t Rf_xlength(SEXP s)
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
        while (s && s != R_NilValue)
        {
            i++;
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

/* regular Rf_allocVector() as a special case of allocVector3() with no custom allocator */
extern inline SEXP Rf_allocVector(SEXPTYPE type, R_xlen_t length = 1)
{
    return Rf_allocVector3(type, length, nullptr);
}

/* from list.cpp */
/* Return a dotted pair with the given CAR and CDR. */
/* The (R) TAG slot on the cell is set to NULL. */

/** @brief Get the i-th element of a list.
 *
 * @param list SEXP object.
 * @param i i-th element of that object.
 *
 * @return i-th element.
 */
extern inline SEXP Rf_elt(SEXP list, int i)
{
    SEXP result = list;

    if ((i < 0) || (i > Rf_length(list)))
        return R_NilValue;

    for (int j = 0; j < i; j++)
        result = CDR(result);

    return CAR(result);
}

/**
 * @brief Return the last element of a list
 */
extern inline SEXP Rf_lastElt(SEXP list)
{
    SEXP result = R_NilValue;
    while (list != R_NilValue)
    {
        result = list;
        list = CDR(list);
    }
    return result;
}

/* Shorthands for creating small lists */

extern inline SEXP Rf_list1(SEXP s)
{
    return CONS(s, R_NilValue);
}

extern inline SEXP Rf_list2(SEXP s, SEXP t)
{
    PROTECT(s);
    s = CONS(s, Rf_list1(t));
    UNPROTECT(1);
    return s;
}

extern inline SEXP Rf_list3(SEXP s, SEXP t, SEXP u)
{
    PROTECT(s);
    s = CONS(s, Rf_list2(t, u));
    UNPROTECT(1);
    return s;
}

extern inline SEXP Rf_list4(SEXP s, SEXP t, SEXP u, SEXP v)
{
    PROTECT(s);
    s = CONS(s, Rf_list3(t, u, v));
    UNPROTECT(1);
    return s;
}

extern inline SEXP Rf_list5(SEXP s, SEXP t, SEXP u, SEXP v, SEXP w)
{
    PROTECT(s);
    s = CONS(s, Rf_list4(t, u, v, w));
    UNPROTECT(1);
    return s;
}

extern inline SEXP Rf_list6(SEXP s, SEXP t, SEXP u, SEXP v, SEXP w, SEXP x)
{
    PROTECT(s);
    s = CONS(s, Rf_list5(t, u, v, w, x));
    UNPROTECT(1);
    return s;
}

/* Destructive list append : See also ``append'' */

extern inline SEXP Rf_listAppend(SEXP s, SEXP t)
{
    SEXP r;
    if (s == R_NilValue)
        return t;
    r = s;
    while (CDR(r) != R_NilValue)
        r = CDR(r);
    SETCDR(r, t);
    return s;
}

/* Language based list constructs.  These are identical to the list */
/* constructs, but the results can be evaluated. */

/**
 * @brief Return a (language) dotted pair with the given car and cdr
 */

extern inline SEXP Rf_lcons(SEXP car, SEXP cdr)
{
    SEXP e = Rf_cons(car, cdr);
    SET_TYPEOF(e, LANGSXP);
    return e;
}

extern inline SEXP Rf_lang1(SEXP s)
{
    return LCONS(s, R_NilValue);
}

extern inline SEXP Rf_lang2(SEXP s, SEXP t)
{
    PROTECT(s);
    s = LCONS(s, Rf_list1(t));
    UNPROTECT(1);
    return s;
}

extern inline SEXP Rf_lang3(SEXP s, SEXP t, SEXP u)
{
    PROTECT(s);
    s = LCONS(s, Rf_list2(t, u));
    UNPROTECT(1);
    return s;
}

extern inline SEXP Rf_lang4(SEXP s, SEXP t, SEXP u, SEXP v)
{
    PROTECT(s);
    s = LCONS(s, Rf_list3(t, u, v));
    UNPROTECT(1);
    return s;
}

extern inline SEXP Rf_lang5(SEXP s, SEXP t, SEXP u, SEXP v, SEXP w)
{
    PROTECT(s);
    s = LCONS(s, Rf_list4(t, u, v, w));
    UNPROTECT(1);
    return s;
}

extern inline SEXP Rf_lang6(SEXP s, SEXP t, SEXP u, SEXP v, SEXP w, SEXP x)
{
    PROTECT(s);
    s = LCONS(s, Rf_list5(t, u, v, w, x));
    UNPROTECT(1);
    return s;
}

/* from util.cpp */

/**
 * @brief Check to see if the arrays "x" and "y" have the identical extents
 */

extern inline Rboolean Rf_conformable(SEXP x, SEXP y)
{
    int i, n;
    PROTECT(x = Rf_getAttrib(x, R_DimSymbol));
    y = Rf_getAttrib(y, R_DimSymbol);
    UNPROTECT(1);
    if ((n = Rf_length(x)) != Rf_length(y))
        return FALSE;
    for (i = 0; i < n; i++)
        if (INTEGER(x)[i] != INTEGER(y)[i])
            return FALSE;
    return TRUE;
}

/**
 * @note R's Rf_inherits() is based on inherits3() in ../main/objects.cpp
 * Here, use char / CHAR() instead of the slower more general Rf_translateChar()
 */
extern inline Rboolean Rf_inherits(SEXP s, const char *name)
{
    SEXP klass;
    int i, nclass;
    if (OBJECT(s))
    {
        klass = Rf_getAttrib(s, R_ClassSymbol);
        nclass = Rf_length(klass);
        for (i = 0; i < nclass; i++)
        {
            if (!strcmp(CXXR::r_char(STRING_ELT(klass, i)), name))
                return TRUE;
        }
    }
    return FALSE;
}

extern inline Rboolean Rf_isValidString(SEXP x)
{
    return Rboolean(TYPEOF(x) == STRSXP && LENGTH(x) > 0 && TYPEOF(STRING_ELT(x, 0)) != NILSXP);
}

/* non-empty ("") valid string :*/
extern inline Rboolean Rf_isValidStringF(SEXP x)
{
    return Rboolean(Rf_isValidString(x) && CXXR::r_char(STRING_ELT(x, 0))[0]);
}

extern inline Rboolean Rf_isUserBinop(SEXP s)
{
    if (TYPEOF(s) == SYMSXP)
    {
        const char *str = CXXR::r_char(PRINTNAME(s));
        if (strlen(str) >= 2 && str[0] == '%' && str[strlen(str) - 1] == '%')
            return TRUE;
    }
    return FALSE;
}

extern inline Rboolean Rf_isPrimitive(SEXP s)
{
    return Rboolean(TYPEOF(s) == BUILTINSXP ||
                    TYPEOF(s) == SPECIALSXP);
}

extern inline Rboolean Rf_isFunction(SEXP s)
{
    return Rboolean(TYPEOF(s) == CLOSXP ||
                    Rf_isPrimitive(s));
}

extern inline Rboolean Rf_isList(SEXP s)
{
    return Rboolean(s == R_NilValue || TYPEOF(s) == LISTSXP);
}

extern inline Rboolean Rf_isNewList(SEXP s)
{
    return Rboolean(s == R_NilValue || TYPEOF(s) == VECSXP);
}

extern inline Rboolean Rf_isPairList(SEXP s)
{
    switch (TYPEOF(s))
    {
    case NILSXP:
    case LISTSXP:
    case LANGSXP:
    case DOTSXP:
        return TRUE;
    default:
        return FALSE;
    }
}

extern inline Rboolean Rf_isVectorList(SEXP s)
{
    switch (TYPEOF(s))
    {
    case VECSXP:
    case EXPRSXP:
        return TRUE;
    default:
        return FALSE;
    }
}

extern inline Rboolean Rf_isVectorAtomic(SEXP s)
{
    switch (TYPEOF(s))
    {
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
    case RAWSXP:
        return TRUE;
    default: /* including NULL */
        return FALSE;
    }
}

extern inline Rboolean Rf_isVector(SEXP s) /* === isVectorList() or isVectorAtomic() */
{
    switch (TYPEOF(s))
    {
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
    case RAWSXP:

    case VECSXP:
    case EXPRSXP:
        return TRUE;
    default:
        return FALSE;
    }
}

extern inline Rboolean Rf_isFrame(SEXP s)
{
    SEXP klass;
    int i;
    if (OBJECT(s))
    {
        klass = Rf_getAttrib(s, R_ClassSymbol);
        for (i = 0; i < Rf_length(klass); i++)
            if (!strcmp(CXXR::r_char(STRING_ELT(klass, i)), "data.frame"))
                return TRUE;
    }
    return FALSE;
}

/* DIFFERENT than R's  is.language(.) in ../main/coerce.cpp [do_is(), case 301:]
 *                                    which is   <=>  SYMSXP || LANGSXP || EXPRSXP */
extern inline Rboolean Rf_isLanguage(SEXP s)
{
    return Rboolean(s == R_NilValue || TYPEOF(s) == LANGSXP);
}

extern inline Rboolean Rf_isMatrix(SEXP s)
{
    SEXP t;
    if (Rf_isVector(s))
    {
        t = Rf_getAttrib(s, R_DimSymbol);
        /* You are not supposed to be able to assign a non-integer dim,
	   although this might be possible by misuse of ATTRIB. */
        if (TYPEOF(t) == INTSXP && LENGTH(t) == 2)
            return TRUE;
    }
    return FALSE;
}

extern inline Rboolean Rf_isArray(SEXP s)
{
    SEXP t;
    if (Rf_isVector(s))
    {
        t = Rf_getAttrib(s, R_DimSymbol);
        /* You are not supposed to be able to assign a 0-length dim,
	 nor a non-integer dim */
        if ((TYPEOF(t) == INTSXP) && LENGTH(t) > 0)
            return TRUE;
    }
    return FALSE;
}

extern inline Rboolean Rf_isTs(SEXP s)
{
    return Rboolean(Rf_isVector(s) && Rf_getAttrib(s, R_TspSymbol) != R_NilValue);
}

extern inline Rboolean Rf_isInteger(SEXP s)
{
    return Rboolean(s && TYPEOF(s) == INTSXP && !Rf_inherits(s, "factor"));
}

extern inline Rboolean Rf_isFactor(SEXP s)
{
    return Rboolean(s && TYPEOF(s) == INTSXP && Rf_inherits(s, "factor"));
}

extern inline int Rf_nlevels(SEXP f)
{
    if (!Rf_isFactor(f))
        return 0;
    return LENGTH(Rf_getAttrib(f, R_LevelsSymbol));
}

/**
 * @brief Is an object of numeric type.
 * 
 * @todo the LGLSXP case should be excluded here
 *       (really? in many places we affirm they are treated like INTs)
 */

extern inline Rboolean Rf_isNumeric(SEXP s)
{
    switch (TYPEOF(s))
    {
    case INTSXP:
        if (Rf_inherits(s, "factor"))
            return FALSE;
    case LGLSXP:
    case REALSXP:
        return TRUE;
    default:
        return FALSE;
    }
}

/**
 *  @brief Is an object "Numeric" or  complex
*/
extern inline Rboolean Rf_isNumber(SEXP s)
{
    switch (TYPEOF(s))
    {
    case INTSXP:
        if (Rf_inherits(s, "factor"))
            return FALSE;
    case LGLSXP:
    case REALSXP:
    case CPLXSXP:
        return TRUE;
    default:
        return FALSE;
    }
}

/* As from R 2.4.0 we check that the value is allowed. */
extern inline SEXP Rf_ScalarLogical(int x)
{
    extern SEXP R_LogicalNAValue, R_TrueValue, R_FalseValue;
    if (x == NA_LOGICAL)
        return R_LogicalNAValue;
    else if (x != 0)
        return R_TrueValue;
    else
        return R_FalseValue;
}

extern inline SEXP Rf_ScalarInteger(int x)
{
    SEXP ans = Rf_allocVector(INTSXP, 1);
    SET_SCALAR_IVAL(ans, x);
    return ans;
}

extern inline SEXP Rf_ScalarReal(double x)
{
    SEXP ans = Rf_allocVector(REALSXP, 1);
    SET_SCALAR_DVAL(ans, x);
    return ans;
}

extern inline SEXP Rf_ScalarComplex(Rcomplex x)
{
    SEXP ans = Rf_allocVector(CPLXSXP, 1);
    SET_SCALAR_CVAL(ans, x);
    return ans;
}

extern inline SEXP Rf_ScalarString(SEXP x)
{
    SEXP ans;
    PROTECT(x);
    ans = Rf_allocVector(STRSXP, (R_xlen_t)1);
    SET_STRING_ELT(ans, (R_xlen_t)0, x);
    UNPROTECT(1);
    return ans;
}

extern inline SEXP Rf_ScalarRaw(Rbyte x)
{
    SEXP ans = Rf_allocVector(RAWSXP, 1);
    SET_SCALAR_BVAL(ans, x);
    return ans;
}

/**
 * @brief Check to see if a list can be made into a vector.
 * 
 * @note it must have every element being a vector of length 1.
 *       BUT it does not exclude 0!
 * 
 * @return true if list can be made into a vector
 */

extern inline Rboolean Rf_isVectorizable(SEXP s)
{
    if (s == R_NilValue)
        return TRUE;
    else if (Rf_isNewList(s))
    {
        R_xlen_t i, n;

        n = XLENGTH(s);
        for (i = 0; i < n; i++)
            if (!Rf_isVector(VECTOR_ELT(s, i)) || XLENGTH(VECTOR_ELT(s, i)) > 1)
                return FALSE;
        return TRUE;
    }
    else if (Rf_isList(s))
    {
        for (; s != R_NilValue; s = CDR(s))
            if (!Rf_isVector(CAR(s)) || LENGTH(CAR(s)) > 1)
                return FALSE;
        return TRUE;
    }
    else
        return FALSE;
}

/** @fn SEXP Rf_mkNamed(SEXPTYPE TYP, const char **names)
 *
 * @brief Create a named vector of type TYP
 *
 * @example const char *nms[] = {"xi", "yi", "zi", ""};
 *          mkNamed(VECSXP, nms);  =~= R  list(xi=, yi=, zi=)
 *
 * @param TYP a vector SEXP type (e.g. REALSXP)
 * @param names names of list elements with null string appended
 *
 * @return (pointer to a) named vector of type TYP
 */
extern inline SEXP Rf_mkNamed(SEXPTYPE TYP, const char **names)
{
    SEXP ans, nms;
    R_xlen_t i, n;

    for (n = 0; strlen(names[n]) > 0; n++) {}
    ans = PROTECT(Rf_allocVector(TYP, n));
    nms = PROTECT(Rf_allocVector(STRSXP, n));
    for (i = 0; i < n; i++)
        SET_STRING_ELT(nms, i, Rf_mkChar(names[i]));
    Rf_setAttrib(ans, R_NamesSymbol, nms);
    UNPROTECT(2);
    return ans;
}

/* from gram.y */

/**
 * @brief shortcut for ScalarString(Rf_mkChar(s))
 * 
 * @return string scalar
 */
extern inline SEXP Rf_mkString(const char *s)
{
    SEXP t;

    PROTECT(t = Rf_allocVector(STRSXP, (R_xlen_t)1));
    SET_STRING_ELT(t, (R_xlen_t)0, Rf_mkChar(s));
    UNPROTECT(1);
    return t;
}

/**
 * @brief Obtaing index of a string vector
 * 
 * @return index of a given C string in (translated) R string vector
 */
extern inline int Rf_stringPositionTr(SEXP string, const char *translatedElement)
{

    int slen = LENGTH(string);
    int i;

    const void *vmax = vmaxget();
    for (i = 0; i < slen; i++)
    {
        Rboolean found = (Rboolean)(!strcmp(Rf_translateChar(STRING_ELT(string, i)),
                                            translatedElement));
        vmaxset(vmax);
        if (found)
            return i;
    }
    return -1; /* not found */
}

/* duplicate RHS value of complex assignment if necessary to prevent cycles */
extern inline SEXP R_FixupRHS(SEXP x, SEXP y)
{
    if (y != R_NilValue && MAYBE_REFERENCED(y))
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

#endif /* R_INLINES_H_ */
