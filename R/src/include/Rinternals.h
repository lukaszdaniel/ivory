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
     * of R object.  In rho the same purpose could be (and sometimes
     * is) achieved by C++ run-time type information (RTTI), virtual
     * function despatch etc.  However, a ::SEXPTYPE field is retained
     * within each rho::RObject for backwards compatibility, and indeed
     * efficiency.
     *
     * Note: when not compiling rho, SEXPTYPE is a typedef for unsigned int.
     * This is done to support C++ packages that expect implicit int to
     * SEXPTYPE conversions.
     */
#ifndef COMPILING_IVORY
    typedef unsigned int SEXPTYPE;
#else
    typedef
#endif
    enum {
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

    NEWSXP = 30,  /* fresh node created in new page */
    FREESXP = 31, /* node released by GC */
    SINGLESXP = 47, /* For interfaces to objects created with as.single */
    intCHARSXP = 73,
    FUNSXP = 99,    /* Closure or Builtin */
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

/* These are also used with the write barrier on, in attrib.cpp and util.cpp */
#define BASIC_TYPE_BITS 5
#define FULL_TYPE_BITS 8
#define MAX_NUM_BASIC_SEXPTYPE (1 << BASIC_TYPE_BITS)
#define MAX_NUM_SEXPTYPE (1 << FULL_TYPE_BITS)

#if defined(COMPILING_IVORY) && defined(__cplusplus)
    namespace R
    {
        class SEXPREC;
    }
    using SEXP = R::SEXPREC *;
#if 0
struct Symbol;
struct BuiltInFunction;
struct Environment;
struct Closure;
struct Promise;
struct RList;
#else
    typedef struct R::SEXPREC Symbol;
    typedef struct R::SEXPREC BuiltInFunction;
    typedef struct R::SEXPREC Environment;
    typedef struct R::SEXPREC Closure;
    typedef struct R::SEXPREC Promise;
    typedef struct R::SEXPREC RList;
#endif
#else
    typedef struct SEXPREC *SEXP;
#endif

/* Define SWITCH_TO_NAMED to use the 'NAMED' mechanism instead of
   reference counting. */
#if !defined(SWITCH_TO_NAMED) && !defined(SWITCH_TO_REFCNT)
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
/* This is intended for use only within R itself.
 * It defines internal structures that are otherwise only accessible
 * via SEXP, and macros to replace many (but not all) of accessor functions
 * (which are always defined).
 */

constexpr int NAMED_BITS = 16;

/* Flags */
namespace R {
struct sxpinfo_struct
{
    SEXPTYPE type : FULL_TYPE_BITS;
    bool scalar;
    bool obj;
    bool alt;
    unsigned int gp : 16;
    bool mark;
    bool debug;
    bool trace; /* functions and memory tracing */
    bool spare; /* used on closures and when REFCNT is defined */
    bool gcgen; /* old generation number */
    unsigned int gccls : 3; /* node class */
    unsigned int named : NAMED_BITS;
    unsigned int extra : 29 - NAMED_BITS; /* used for immediate bindings */
};                                        /*		    Tot: 64 */

struct vecsxp_struct
{
    R_xlen_t length;
    R_xlen_t truelength;
};

struct primsxp_struct
{
    int offset;
};

struct symsxp_struct
{
    SEXPREC *pname;
    SEXPREC *value;
    SEXPREC *internal;
};

struct listsxp_struct
{
    SEXPREC *carval;
    SEXPREC *cdrval;
    SEXPREC *tagval;
};

struct envsxp_struct
{
    SEXPREC *frame;
    SEXPREC *enclos;
    SEXPREC *hashtab;
};

struct closxp_struct
{
    SEXPREC *formals;
    SEXPREC *body;
    SEXPREC *env;
};

struct promsxp_struct
{
    SEXPREC *value;
    SEXPREC *expr;
    SEXPREC *env;
};

/* Every node must start with a set of sxpinfo flags and an attribute
   field. Under the generational collector these are followed by the
   fields used to maintain the collector's linked list structures. */
} //namespace
#ifdef SWITCH_TO_REFCNT
constexpr int REFCNTMAX = ((1 << NAMED_BITS) - 1);
#endif
namespace R {
/* The standard node structure consists of a header followed by the
   node data. */
class SEXPREC
{
    private:
    sxpinfo_struct sxpinfo;
    SEXPREC *attrib;
    SEXPREC *gengc_next_node;
    SEXPREC *gengc_prev_node;
    union
    {
        primsxp_struct primsxp;
        symsxp_struct symsxp;
        listsxp_struct listsxp;
        envsxp_struct envsxp;
        closxp_struct closxp;
        promsxp_struct promsxp;
    } u;
    // virtual ~SEXPREC() = default;
#if 0
    SEXPTYPE sexptype() const { return this->sxpinfo.type; }
    void setsexptype(const SEXPTYPE& type) { this->sxpinfo.type = type; }
    bool sexptypeEqual(const SEXPTYPE& type) const { return this->sxpinfo.type == type; }
    bool sexptypeNotEqual(const SEXPTYPE& type) const { return !this->sexptypeEqual(type); }
    auto altrep() const { return this->sxpinfo.alt; }
    void setaltrep() { this->sxpinfo.alt = true; }
    void unsetaltrep() { this->sxpinfo.alt = false; }
    bool isPrimitive_() const { return this->sexptypeEqual(BUILTINSXP) || this->sexptypeEqual(SPECIALSXP); }
    bool isFunction_() const { return this->sexptypeEqual(CLOSXP) || this->isPrimitive_(); }
    bool isPairList_() const
    {
        switch (this->sexptype())
        {
        case NILSXP:
        case LISTSXP:
        case LANGSXP:
        case DOTSXP:
            return true;
        default:
            return false;
        }
    }
    // bool isLanguage_() const { return this->sexptypeEqual(LANGSXP); }
    // R_len_t length_() const { return this->sexptypeEqual(NILSXP) ? 0 : 1; }
    bool isNull_() const { return this->sexptype() == SEXPTYPE::NILSXP; }
    bool isSymbol_() const { return this->sexptype() == SEXPTYPE::SYMSXP; }
    bool isLogical_() const { return this->sexptype() == SEXPTYPE::LGLSXP; }
    bool isReal_() const { return this->sexptype() == SEXPTYPE::REALSXP; }
    bool isComplex_() const { return this->sexptype() == SEXPTYPE::CPLXSXP; }
    bool isExpression_() const { return this->sexptype() == SEXPTYPE::EXPRSXP; }
    bool isEnvironment_() const { return this->sexptype() == SEXPTYPE::ENVSXP; }
    bool isString_() const { return this->sexptype() == SEXPTYPE::STRSXP; }
    bool isRaw_() const { return this->sexptypeEqual(RAWSXP); }
    bool isScalar(const SEXPTYPE &t) const { return this->sexptypeEqual(t) && this->sxpinfo.scalar; }
    bool isVector_() const
    {
        switch (this->sexptype())
        {
        case LGLSXP:
        case INTSXP:
        case REALSXP:
        case CPLXSXP:
        case STRSXP:
        case RAWSXP:

        case VECSXP:
        case EXPRSXP:
            return true;
        default:
            return false;
        }
    }
    auto attrib_() const { return this->attrib; }
    inline auto isBytes() const;
    inline void setBytes();
    inline auto isLatin1() const;
    inline void setLatin1();
    inline auto isAscii() const;
    inline void setAscii();
    inline auto isUTF8() const;
    inline void setUTF8();
    inline auto encKnown() const;
    inline auto isCached() const;
    inline void setCached();
    auto printname() const { return this->u.symsxp.pname; }
    auto symvalue() const { return this->u.symsxp.value; }
    auto internal() const { return this->u.symsxp.internal; }
    auto tag() const { return this->u.listsxp.tagval; }
    auto car() const { return this->u.listsxp.carval; }
    auto cdr() const { return this->u.listsxp.cdrval; }
    const char *translateCharUTF8_() const;
#endif
    public:
    /* General Cons Cell Attributes */
    static inline bool GCGEN(SEXP v) { return v ? v->sxpinfo.gcgen : false; }
    static inline void SET_GCGEN(SEXP v, bool x) { if(!v) return; v->sxpinfo.gcgen = x; }
    static inline unsigned int GCCLS(SEXP v) { return v ? v->sxpinfo.gccls : 0; }
    static inline void SET_GCCLS(SEXP v, unsigned int x) { if(!v) return; v->sxpinfo.gccls = x; }
    static inline auto NEXT_NODE(SEXP s) { return s ? s->gengc_next_node : nullptr; }
    static inline auto PREV_NODE(SEXP s) { return s ? s->gengc_prev_node : nullptr; }
    static inline void SET_NEXT_NODE(SEXP s, SEXP t) { if(!s) return; s->gengc_next_node = t; }
    static inline void SET_PREV_NODE(SEXP s, SEXP t) { if(!s) return; s->gengc_prev_node = t; }
    static inline void COPY_SXPINFO(SEXP x, SEXPREC &y) { if(!x) return; x->sxpinfo = y.sxpinfo; }
    static constexpr int READY_TO_FINALIZE_MASK = 1;
    static constexpr int FINALIZE_ON_EXIT_MASK = 2;
    static constexpr int WEAKREF_SIZE = 4;
    static inline void SET_READY_TO_FINALIZE(SEXP s) { if(!s) return; s->sxpinfo.gp |= READY_TO_FINALIZE_MASK; }
    static inline void CLEAR_READY_TO_FINALIZE(SEXP s) { if(!s) return; s->sxpinfo.gp &= ~READY_TO_FINALIZE_MASK; }
    static inline auto IS_READY_TO_FINALIZE(SEXP s) { return s ? s->sxpinfo.gp & READY_TO_FINALIZE_MASK : 0; }
    static inline void SET_FINALIZE_ON_EXIT(SEXP s) { if(!s) return; s->sxpinfo.gp |= FINALIZE_ON_EXIT_MASK; }
    static inline void CLEAR_FINALIZE_ON_EXIT(SEXP s) { if(!s) return; s->sxpinfo.gp &= ~FINALIZE_ON_EXIT_MASK; }
    static inline auto FINALIZE_ON_EXIT(SEXP s) { return s ? (s->sxpinfo.gp & FINALIZE_ON_EXIT_MASK) : 0; }
    static inline void SET_ATTRIB(SEXP x, SEXP v) { if(!x) return; x->attrib = v; }
    static inline SEXP ATTRIB(SEXP x) { return x ? x->attrib : nullptr; }
    static inline auto NAMED(SEXP x) { return x ? x->sxpinfo.named : 0; }
    static inline void SET_NAMED(SEXP x, unsigned int v) { if(!x) return; x->sxpinfo.named = v; }
    static inline void SET_TYPEOF(SEXP x, SEXPTYPE v) { if(!x) return; x->sxpinfo.type = v; }
    static inline SEXPTYPE TYPEOF(SEXP x) { return x ? x->sxpinfo.type : NILSXP; }
    static inline auto LEVELS(SEXP x) { return x ? x->sxpinfo.gp : 0; }
    static inline auto OBJECT(SEXP x) { return x && x->sxpinfo.obj; }
    static inline void SET_OBJECT(SEXP x, bool v) { if(!x) return; x->sxpinfo.obj = v; }
    static inline auto MARK(SEXP x) { return x ? x->sxpinfo.mark : false; }
    static inline void SET_MARK(SEXP x, int v) { if(!x) return; x->sxpinfo.mark = v; }
    enum CharsetBit
    {
        NATIVE_MASK = 0,
        BYTES_MASK = (1 << 1),
        LATIN1_MASK = (1 << 2),
        UTF8_MASK = (1 << 3),
        /* (1 << 4) is taken by S4_OBJECT_MASK */
        CACHED_MASK = (1 << 5),
        ASCII_MASK = (1 << 6)
    };
    static inline bool SCALAR(SEXP x) { return x ? x->sxpinfo.scalar : false; }
    static inline auto IS_BYTES(SEXP x) { return x ? (x->sxpinfo.gp & BYTES_MASK) : 0; }
    static inline void SET_BYTES(SEXP x) { if(!x) return; x->sxpinfo.gp |= BYTES_MASK; }
    static inline auto IS_LATIN1(SEXP x) { return x ? (x->sxpinfo.gp & LATIN1_MASK) : 0; }
    static inline void SET_LATIN1(SEXP x) { if(!x) return; x->sxpinfo.gp |= LATIN1_MASK; }
    static inline auto IS_ASCII(SEXP x) { return x ? (x->sxpinfo.gp & ASCII_MASK) : 0; }
    static inline void SET_ASCII(SEXP x) { if(!x) return; x->sxpinfo.gp |= ASCII_MASK; }
    static inline auto IS_UTF8(SEXP x) { return x ? (x->sxpinfo.gp & UTF8_MASK) : 0; }
    static inline void SET_UTF8(SEXP x) { if(!x) return; x->sxpinfo.gp |= UTF8_MASK; }
    static inline auto ENC_KNOWN(SEXP x) { return x ? (x->sxpinfo.gp & (LATIN1_MASK | UTF8_MASK)) : 0; }
    static inline void SET_CACHED(SEXP x) { if(!x) return; x->sxpinfo.gp |= CACHED_MASK; }
    static inline auto IS_CACHED(SEXP x) { return x ? (x->sxpinfo.gp & CACHED_MASK) : 0; }
    static inline auto ALTREP(SEXP x) { return x ? x->sxpinfo.alt : 0; }
    static inline void SET_ALTREP(SEXP x, bool v) { if(!x) return; x->sxpinfo.alt = v; }
    static inline void SETLEVELS(SEXP x, unsigned short int v) { if(!x) return; x->sxpinfo.gp = (unsigned short) v; }
    static inline void SETSCALAR(SEXP x, bool v) { if(!x) return; x->sxpinfo.scalar = v; }
    static inline bool IS_SCALAR(SEXP x, SEXPTYPE t) { return x && (x->sxpinfo.type == t) && x->sxpinfo.scalar; }
    static inline auto REFCNT(SEXP x) { return x ? x->sxpinfo.named : 0; }
    static inline void SET_REFCNT(SEXP x, unsigned int v) { if(!x) return; x->sxpinfo.named = v; }
    static inline bool TRACKREFS(SEXP x) { return x && (TYPEOF(x) == CLOSXP ? TRUE : ! x->sxpinfo.spare); }
    static inline void SET_TRACKREFS(SEXP x, bool v) {  if(!x) return; x->sxpinfo.spare = v; };
    static constexpr int ASSIGNMENT_PENDING_MASK = (1 << 11);
    static inline auto ASSIGNMENT_PENDING(SEXP x) { return x ?  (x->sxpinfo.gp & ASSIGNMENT_PENDING_MASK) : 0; };
    static inline void SET_ASSIGNMENT_PENDING(SEXP x, bool v)
    {
        if(!x) return;
        if (v)
            (((x)->sxpinfo.gp) |= ASSIGNMENT_PENDING_MASK);
        else
            (((x)->sxpinfo.gp) &= ~ASSIGNMENT_PENDING_MASK);
    }
    static inline auto RTRACE(SEXP x) { return x ? x->sxpinfo.trace : false; }
    static inline void SET_RTRACE(SEXP x, bool v) { if(!x) return; x->sxpinfo.trace = v; }
    /* Primitive Access Methods */
    static inline auto PRIMOFFSET(SEXP x) { return x ? x->u.primsxp.offset : 0; }
    static inline void SET_PRIMOFFSET(SEXP x, int v) { if(!x) return; x->u.primsxp.offset = v; }
    /* Closure Access Methods */
    static inline auto FORMALS(SEXP x) { return x ? x->u.closxp.formals : nullptr; }
    static inline void SET_FORMALS(SEXP x, SEXP v) { if(!x) return; x->u.closxp.formals = v; }
    static inline auto BODY(SEXP x) { return x ? x->u.closxp.body : nullptr; }
    static inline void SET_BODY(SEXP x, SEXP v) { if(!x) return; x->u.closxp.body = v; }
    static inline auto CLOENV(SEXP x) { return x ? x->u.closxp.env : nullptr; }
    static inline void SET_CLOENV(SEXP x, SEXP v) { if(!x) return; x->u.closxp.env = v; }
    static inline auto RDEBUG(SEXP x) { return x && x->sxpinfo.debug; }
    static inline void SET_RDEBUG(SEXP x, int v) { if(!x) return; x->sxpinfo.debug = v; }
    static inline auto RSTEP(SEXP x) { return x && x->sxpinfo.spare; }
    static inline void SET_RSTEP(SEXP x, int v) { if(!x) return; x->sxpinfo.spare = v; }
    /* Symbol Access Methods */
    static constexpr int DDVAL_MASK = 1;
    static inline auto PRINTNAME(SEXP x) { return x ? x->u.symsxp.pname : nullptr; }
    static inline auto SYMVALUE(SEXP x) { return x ? x->u.symsxp.value : nullptr; }
    static inline auto INTERNAL(SEXP x) { return x ? x->u.symsxp.internal : nullptr; }
    static inline auto DDVAL(SEXP x) { return x ? (x->sxpinfo.gp & DDVAL_MASK) : 0; } /* for ..1, ..2 etc */
    static inline void SET_DDVAL_BIT(SEXP x) { if(!x) return; x->sxpinfo.gp |= DDVAL_MASK; }
    static inline void UNSET_DDVAL_BIT(SEXP x) { if(!x) return; x->sxpinfo.gp &= ~DDVAL_MASK; }
    static inline void SET_DDVAL(SEXP x, int v) { if(v) { SET_DDVAL_BIT(x); } else { UNSET_DDVAL_BIT(x); } } /* for ..1, ..2 etc */
    static inline void SET_PRINTNAME(SEXP x, SEXP v) { if(!x) return; x->u.symsxp.pname = v; }
    static inline void SET_SYMVALUE(SEXP x, SEXP v) { if(!x) return; x->u.symsxp.value = v; }
    static inline void SET_INTERNAL(SEXP x, SEXP v) { if(!x) return; x->u.symsxp.internal = v; }
    /* Environment Access Methods */
    static constexpr int FRAME_LOCK_MASK = (1 << 14);
    static constexpr int GLOBAL_FRAME_MASK = (1 << 15);
    static inline auto FRAME(SEXP x) { return x ? x->u.envsxp.frame : nullptr; }
    static inline auto ENCLOS(SEXP x) { return x ? x->u.envsxp.enclos : nullptr; }
    static inline auto HASHTAB(SEXP x) { return x ? x->u.envsxp.hashtab : nullptr; }
    static inline auto ENVFLAGS(SEXP x) { return x ? x->sxpinfo.gp : 0; }	/* for environments */
    static inline void SET_ENVFLAGS(SEXP x, int v) { if(!x) return; x->sxpinfo.gp = v; }
    static inline void SET_FRAME(SEXP x, SEXP v) { if(!x) return; x->u.envsxp.frame = v; }
    static inline void SET_ENCLOS(SEXP x, SEXP v) { if(!x) return; x->u.envsxp.enclos = v; }
    static inline void SET_HASHTAB(SEXP x, SEXP v) { if(!x) return; x->u.envsxp.hashtab = v; }
    static inline auto FRAME_IS_LOCKED(SEXP e) { return e ? (ENVFLAGS(e) & FRAME_LOCK_MASK) : 0; }
    static inline auto IS_GLOBAL_FRAME(SEXP e) { return e ? (ENVFLAGS(e) & GLOBAL_FRAME_MASK) : 0; }
    /* Promise Access Methods */
    static inline auto PRCODE(SEXP x) { return x ? x->u.promsxp.expr : nullptr; }
    static inline void SET_PRCODE(SEXP x, SEXP v) { if(!x) return; x->u.promsxp.expr = v; }
    static inline auto PRENV(SEXP x) { return x ? x->u.promsxp.env : nullptr; }
    static inline auto PRVALUE(SEXP x) { return x ? x->u.promsxp.value : nullptr; }
    static inline void SET_PRVALUE(SEXP x, SEXP v) { if(!x) return; x->u.promsxp.value = v; }
    static inline auto PRSEEN(SEXP x) { return x ? x->sxpinfo.gp : 0; }
    static inline void SET_PRENV(SEXP x, SEXP v) { if(!x) return; x->u.promsxp.env = v; }
    static inline void SET_PRSEEN(SEXP x, int v) { if(!x) return; x->sxpinfo.gp = v; }
    /* List Access Methods */
    static inline auto TAG(SEXP e) { return e ? e->u.listsxp.tagval : nullptr; }
    static inline void SET_TAG(SEXP x, SEXP v) { if(!x) return; x->u.listsxp.tagval = v; }
    static inline auto CAR0(SEXP e) { return e ? e->u.listsxp.carval : nullptr; }
    static inline void SET_CAR0(SEXP x, SEXP v) { if(!x) return; x->u.listsxp.carval = v; }
    static inline auto EXTPTR_PTR(SEXP e) { return e ? e->u.listsxp.carval : nullptr; }
    static inline void SET_EXTPTR_PTR(SEXP x, SEXP v) { if(!x) return; x->u.listsxp.carval = v; }
    static inline auto CDR(SEXP e) { return e ? e->u.listsxp.cdrval : nullptr; }
    static inline void SET_CDR(SEXP x, SEXP v) { if(!x) return; x->u.listsxp.cdrval = v; }
    static constexpr int MISSING_MASK = ((1 << 4) - 1); // = 15 /* reserve 4 bits--only 2 uses now */
    static inline auto MISSING(SEXP x) { return x ? (x->sxpinfo.gp & MISSING_MASK) : 0; }/* for closure calls */
    static inline void SET_MISSING(SEXP x, int v)
    {
        if (!x)
            return;
        int __other_flags__ = x->sxpinfo.gp & ~MISSING_MASK;
        x->sxpinfo.gp = __other_flags__ | v;
    }
    static inline auto BNDCELL_TAG(SEXP e) { return e ? e->sxpinfo.extra : 0; }
    static inline void SET_BNDCELL_TAG(SEXP e, unsigned int v) { if(!e) return; e->sxpinfo.extra = v; }
    /* External pointer access methods */
    static inline auto EXTPTR_PROT(SEXP x) { return SEXPREC::CDR(x); }
    static inline auto EXTPTR_TAG(SEXP x) { return SEXPREC::TAG(x); }
    static inline void SET_EXTPTR_TAG(SEXP x, SEXP v) { SEXPREC::SET_TAG(x, v); }
    static inline void SET_EXTPTR_PROT(SEXP x, SEXP v) { SEXPREC::SET_CDR(x, v); }
    /* S4 object bit, set by R_do_new_object for all new() calls */
    static constexpr int S4_OBJECT_MASK = ((unsigned short)(1 << 4));
    static inline auto IS_S4_OBJECT(SEXP x) { return x && (x->sxpinfo.gp & S4_OBJECT_MASK); }
    static inline void SET_S4_OBJECT(SEXP x) { if(!x) return; x->sxpinfo.gp |= S4_OBJECT_MASK; }
    static inline void UNSET_S4_OBJECT(SEXP x) { if(!x) return; x->sxpinfo.gp &= ~S4_OBJECT_MASK; }
    /* JIT optimization support */
    static constexpr int NOJIT_MASK = ((unsigned short)(1 << 5));
    static constexpr int MAYBEJIT_MASK = ((unsigned short)(1 << 6));
    static inline auto NOJIT(SEXP x) { return x ? (x->sxpinfo.gp & NOJIT_MASK) : 0; }
    static inline void SET_NOJIT(SEXP x) { if(!x) return; x->sxpinfo.gp |= NOJIT_MASK; }
    static inline auto MAYBEJIT(SEXP x) { return x ? (x->sxpinfo.gp & MAYBEJIT_MASK) : 0; }
    static inline void SET_MAYBEJIT(SEXP x) { if(!x) return; x->sxpinfo.gp |= MAYBEJIT_MASK; }
    static inline void UNSET_MAYBEJIT(SEXP x) { if(!x) return; x->sxpinfo.gp &= ~MAYBEJIT_MASK; }
    /* Growable vector support */
    static constexpr int GROWABLE_MASK = ((unsigned short)(1 << 5));
    static inline auto GROWABLE_BIT_SET(SEXP x) { return x ? (x->sxpinfo.gp & GROWABLE_MASK) : 0; }
    static inline void SET_GROWABLE_BIT(SEXP x) { if(!x) return; x->sxpinfo.gp |= GROWABLE_MASK; }
    /* Hashing Methods */
    static constexpr int HASHASH_MASK = 1;
    static inline auto HASHASH(SEXP x) { return x ? (x->sxpinfo.gp & HASHASH_MASK) : 0; }
    static inline void SET_HASHASH(SEXP x, bool v) { v ? (x->sxpinfo.gp |= HASHASH_MASK) : (x->sxpinfo.gp &= (~HASHASH_MASK)); }

    static constexpr int SPECIAL_SYMBOL_MASK = (1 << 12);
    static constexpr int BASE_SYM_CACHED_MASK = (1 << 13);
    static inline void SET_BASE_SYM_CACHED(SEXP b) { if(!b) return; b->sxpinfo.gp |= BASE_SYM_CACHED_MASK; }
    static inline void UNSET_BASE_SYM_CACHED(SEXP b) { if(!b) return; b->sxpinfo.gp &= (~BASE_SYM_CACHED_MASK); }
    static inline auto BASE_SYM_CACHED(SEXP b) { return b ? (b->sxpinfo.gp & BASE_SYM_CACHED_MASK) : 0; }
    static inline auto NO_SPECIAL_SYMBOLS(SEXP b) { return b ? (b->sxpinfo.gp & SPECIAL_SYMBOL_MASK) : 0; }
    static inline void SET_NO_SPECIAL_SYMBOLS(SEXP b) { if(!b) return; b->sxpinfo.gp |= SPECIAL_SYMBOL_MASK; }
    static inline auto IS_SPECIAL_SYMBOL(SEXP b) { return b ? (b->sxpinfo.gp & SPECIAL_SYMBOL_MASK) : 0; }
    static inline void SET_SPECIAL_SYMBOL(SEXP b) { if(!b) return; b->sxpinfo.gp |= SPECIAL_SYMBOL_MASK; }
    static inline void UNSET_NO_SPECIAL_SYMBOLS(SEXP b) { if(!b) return; b->sxpinfo.gp &= (~SPECIAL_SYMBOL_MASK); }
    static inline void UNSET_SPECIAL_SYMBOL(SEXP b) { if(!b) return; b->sxpinfo.gp &= (~SPECIAL_SYMBOL_MASK); }
    static constexpr int ACTIVE_BINDING_MASK = (1 << 15);
    static constexpr int BINDING_LOCK_MASK = (1 << 14);
    static constexpr int SPECIAL_BINDING_MASK = (ACTIVE_BINDING_MASK | BINDING_LOCK_MASK);
    static inline auto IS_ACTIVE_BINDING(SEXP b) { return b ? (b->sxpinfo.gp & ACTIVE_BINDING_MASK) : 0; }
    static inline auto BINDING_IS_LOCKED(SEXP b) { return b ? (b->sxpinfo.gp & BINDING_LOCK_MASK) : 0; }
    static inline void LOCK_BINDING_(SEXP b);
    static inline void UNLOCK_BINDING(SEXP b) { if(!b) return; b->sxpinfo.gp &= (~BINDING_LOCK_MASK); }
    static inline void SET_ACTIVE_BINDING_BIT(SEXP b) { if(!b) return; b->sxpinfo.gp |= ACTIVE_BINDING_MASK; }
    static inline auto BNDCELL_DVAL(SEXP v);
    static inline auto BNDCELL_IVAL(SEXP v);
    static inline auto BNDCELL_LVAL(SEXP v);
    static inline void SET_BNDCELL_DVAL(SEXP v, double x);
    static inline void SET_BNDCELL_IVAL(SEXP v, int x);
    static inline void SET_BNDCELL_LVAL(SEXP v, int x);
};
#if 0
class Symbol : public SEXPREC {
    struct {
        struct symsxp_struct symsxp;
    } u;
    auto printname() const {
        return this->u.symsxp.pname;
    }
    auto symvalue() const {
        return this->u.symsxp.value;
    }
    auto internal() const {
        return this->u.symsxp.internal;
    }
};

class Environment : public SEXPREC {
    struct {
        struct envsxp_struct envsxp;
    } u;
};

class BuiltInFunction : public SEXPREC {
    struct {
        struct primsxp_struct primsxp;
    } u;
};

class Closure : public SEXPREC {
    struct {
        struct closxp_struct closxp;
    } u;
};

class Promise : public SEXPREC {
    struct {
        struct promsxp_struct promsxp;
    } u;
};

class RList : public SEXPREC {
    struct {
        struct listsxp_struct listsxp;
    } u;
    auto tag() const {
        return this->u.listsxp.tagval;
    }
    auto car() const {
        return this->u.listsxp.carval;
    }
    auto cdr() const {
        return this->u.listsxp.cdrval;
    }
};
#endif

/* The generational collector uses a reduced version of SEXPREC as a
   header in vector nodes.  The layout MUST be kept consistent with
   the SEXPREC definition. The standard SEXPREC takes up 7 words
   and the reduced version takes 6 words on most 64-bit systems. On most
   32-bit systems, SEXPREC takes 8 words and the reduced version 7 words. */
struct VECTOR_SEXPREC
{
    sxpinfo_struct sxpinfo;
    SEXPREC *attrib;
    SEXPREC *gengc_next_node;
    SEXPREC *gengc_prev_node;
    vecsxp_struct vecsxp;
};
} //namespace
using VECSEXP = struct R::VECTOR_SEXPREC *;

union SEXPREC_ALIGN
{
    R::VECTOR_SEXPREC s;
    double align;
};

/* General Cons Cell Attributes */
#define ATTRIB(x)	(R::SEXPREC::ATTRIB(x))
#define OBJECT(x)	(R::SEXPREC::OBJECT(x))
#define MARK(x)		(R::SEXPREC::MARK(x))
#define TYPEOF(x)	(R::SEXPREC::TYPEOF(x))
#define NAMED(x)	(R::SEXPREC::NAMED(x))
#define RTRACE(x)	(R::SEXPREC::RTRACE(x))
#define LEVELS(x)	(R::SEXPREC::LEVELS(x))
#define SET_OBJECT(x,v)	(R::SEXPREC::SET_OBJECT(x, v))
#define SET_TYPEOF(x,v)	(R::SEXPREC::SET_TYPEOF(x, v))
#define SET_NAMED(x,v) (R::SEXPREC::SET_NAMED(x, v))
#define SET_RTRACE(x,v)	(R::SEXPREC::SET_RTRACE(x, v))
#define SETLEVELS(x,v)	(R::SEXPREC::SETLEVELS(x, v))
#define ALTREP(x)       (R::SEXPREC::ALTREP(x))
#define SETALTREP(x, v) (R::SEXPREC::SET_ALTREP(x, v))
#define SETSCALAR(x, v) (R::SEXPREC::SETSCALAR(x, v))

#if defined(COMPUTE_REFCNT_VALUES)
# define REFCNT(x) (R::SEXPREC::REFCNT(x))
# define TRACKREFS(x) (R::SEXPREC::TRACKREFS(x))
#else
# define REFCNT(x) 0
# define TRACKREFS(x) FALSE
#endif

#if defined(COMPUTE_REFCNT_VALUES)
# define SET_REFCNT(x,v) (R::SEXPREC::SET_REFCNT(x, v))
# if defined(EXTRA_REFCNT_FIELDS)
#  define SET_TRACKREFS(x,v) (R::SEXPREC::SET_TRACKREFS(x, v))
# else
#  define SET_TRACKREFS(x,v) (R::SEXPREC::SET_TRACKREFS(x, !v))
# endif
#define DECREMENT_REFCNT(x)                                       \
    do                                                            \
    {                                                             \
        SEXP drc__x__ = (x);                                      \
        if (REFCNT(drc__x__) > 0 && REFCNT(drc__x__) < REFCNTMAX) \
            SET_REFCNT(drc__x__, REFCNT(drc__x__) - 1);           \
    } while (0)
#define INCREMENT_REFCNT(x)                             \
    do                                                  \
    {                                                   \
        SEXP irc__x__ = (x);                            \
        if (REFCNT(irc__x__) < REFCNTMAX)               \
            SET_REFCNT(irc__x__, REFCNT(irc__x__) + 1); \
    } while (0)
#else
# define SET_REFCNT(x,v) do {} while(0)
# define SET_TRACKREFS(x,v) do {} while(0)
# define DECREMENT_REFCNT(x) do {} while(0)
# define INCREMENT_REFCNT(x) do {} while(0)
#endif

#define ENABLE_REFCNT(x) SET_TRACKREFS(x, TRUE)
#define DISABLE_REFCNT(x) SET_TRACKREFS(x, FALSE)

/* To make complex assignments a bit safer, in particular with
   reference counting, a bit is set on the LHS binding cell or symbol
   at the beginning of the complex assignment process and unset at the
   end.

   - When the assignment bit is set and a new value is assigned to the
     binding then the reference count on the old value is not
     decremented. This prevents moving a single binding from the LHS
     variable of the assignment to another variable during the
     assignment process.

  - If a complex assignment tries to update a binding that already has
    its bit set then, the value of the binding is shallow-duplicated
    before proceeding. This ensures that the structure involved in the
    original complex assignment will not be mutated by further R level
    assignments during the original assignment process.

  For now, no attempt is made to unset the bit if the end of an
  assignment is not reached because of a jump. This may result in some
  unnecessary duplications. This could be prevented by maintaining a
  stack of pending assignments to resent the bits on jump, but that
  seems like overkill.

  It might also be useful to use this bit to communicate to functions
  when they are used in a getter/setter context.

  The bit used is bit 11 in the 'gp' field. An alternative would be to
  take a bit from the 'extra' field.

  LT
*/

#define ASSIGNMENT_PENDING(x) (R::SEXPREC::ASSIGNMENT_PENDING(x))
#define SET_ASSIGNMENT_PENDING(x, v) (R::SEXPREC::SET_ASSIGNMENT_PENDING(x, v))

/* The same bit can be used to mark calls used in complex assignments
   to allow replacement functions to determine when they are being
   called in an assignment context and can modify an object with one
   refrence */
#define MARK_ASSIGNMENT_CALL(call) SET_ASSIGNMENT_PENDING(call, TRUE)
#define IS_ASSIGNMENT_CALL(call) ASSIGNMENT_PENDING(call)

#ifdef SWITCH_TO_REFCNT
# undef NAMED
# undef SET_NAMED
# define NAMED(x) REFCNT(x)
/* no definition for SET_NAMED; any calls will use the one in memory.cpp */
# define ENSURE_NAMEDMAX(v) do { } while (0)
# define ENSURE_NAMED(v) do { } while (0)
#else
#define ENSURE_NAMEDMAX(v)                  \
    do                                      \
    {                                       \
        SEXP __enm_v__ = (v);               \
        if (NAMED(__enm_v__) < NAMEDMAX)    \
            SET_NAMED(__enm_v__, NAMEDMAX); \
    } while (0)
# define ENSURE_NAMED(v) do { if (NAMED(v) == 0) SET_NAMED(v, 1); } while (0)
#endif

#ifdef SWITCH_TO_REFCNT
# define SETTER_CLEAR_NAMED(x) do { } while (0)
# define RAISE_NAMED(x, n) do { } while (0)
#else
#define SETTER_CLEAR_NAMED(x)    \
    do                           \
    {                            \
        SEXP __x__ = (x);        \
        if (NAMED(__x__) == 1)   \
            SET_NAMED(__x__, 0); \
    } while (0)
#define RAISE_NAMED(x, n)            \
    do                               \
    {                                \
        SEXP __x__ = (x);            \
        int __n__ = (n);             \
        if (NAMED(__x__) < __n__)    \
            SET_NAMED(__x__, __n__); \
    } while (0)
#endif

/* S4 object bit, set by R_do_new_object for all new() calls */
#define IS_S4_OBJECT(x) (R::SEXPREC::IS_S4_OBJECT(x))
#define SET_S4_OBJECT(x) (R::SEXPREC::SET_S4_OBJECT(x))
#define UNSET_S4_OBJECT(x) (R::SEXPREC::UNSET_S4_OBJECT(x))

/* JIT optimization support */
#define NOJIT(x) (R::SEXPREC::NOJIT(x))
#define SET_NOJIT(x) (R::SEXPREC::SET_NOJIT(x))
#define MAYBEJIT(x) (R::SEXPREC::MAYBEJIT(x))
#define SET_MAYBEJIT(x) (R::SEXPREC::SET_MAYBEJIT(x))
#define UNSET_MAYBEJIT(x) (R::SEXPREC::UNSET_MAYBEJIT(x))

/* Growable vector support */
#define GROWABLE_BIT_SET(x) (R::SEXPREC::GROWABLE_BIT_SET(x))
#define SET_GROWABLE_BIT(x) (R::SEXPREC::SET_GROWABLE_BIT(x))
#define IS_GROWABLE(x) (GROWABLE_BIT_SET(x) && XLENGTH(x) < XTRUELENGTH(x))

/* Vector Access Macros */
#ifdef LONG_VECTOR_SUPPORT
# define IS_LONG_VEC(x) (XLENGTH(x) > R_SHORT_LEN_MAX)
#else
# define IS_LONG_VEC(x) false
#endif
#define STDVEC_LENGTH(x) (((VECSEXP) (x))->vecsxp.length)
#define STDVEC_TRUELENGTH(x) (((VECSEXP) (x))->vecsxp.truelength)
#define SET_STDVEC_TRUELENGTH(x, v) (STDVEC_TRUELENGTH(x)=(v))
#define SET_TRUELENGTH(x, v)                      \
    do                                            \
    {                                             \
        SEXP sl__x__ = (x);                       \
        R_xlen_t sl__v__ = (v);                   \
        if (ALTREP(x))                            \
            error("can't set ALTREP truelength"); \
        SET_STDVEC_TRUELENGTH(sl__x__, sl__v__);  \
    } while (0)

#define IS_SCALAR(x, t) (R::SEXPREC::IS_SCALAR(x, t))
#define LENGTH(x) LENGTH_EX(x, __FILE__, __LINE__)
#define TRUELENGTH(x) XTRUELENGTH(x)

/* defined as a macro since fastmatch packages tests for it */
#define XLENGTH(x) XLENGTH_EX(x)

/* THIS ABSOLUTELY MUST NOT BE USED IN PACKAGES !!! */
#define SET_STDVEC_LENGTH(x, v)               \
    do                                        \
    {                                         \
        SEXP __x__ = (x);                     \
        R_xlen_t __v__ = (v);                 \
        STDVEC_LENGTH(__x__) = __v__;         \
        SETSCALAR(__x__, __v__ == 1 ? 1 : 0); \
    } while (0)

/* Under the generational allocator the data for vector nodes comes
   immediately after the node structure, so the data address is a
   known offset from the node SEXP. */
#define STDVEC_DATAPTR(x) ((void *) (((SEXPREC_ALIGN *) (x)) + 1))
#define CHAR(x)		((const char *) STDVEC_DATAPTR(x))
#define LOGICAL(x)	((int *) DATAPTR(x))
#define INTEGER(x)	((int *) DATAPTR(x))
#define RAW(x)		((Rbyte *) DATAPTR(x))
#define COMPLEX(x)	((Rcomplex *) DATAPTR(x))
#define REAL(x)		((double *) DATAPTR(x))
#define VECTOR_ELT(x,i)	((SEXP *) DATAPTR(x))[i]
#define STRING_PTR(x)	((SEXP *) DATAPTR(x))
#define VECTOR_PTR(x)	((SEXP *) DATAPTR(x))
#define LOGICAL_RO(x)	((const int *) DATAPTR_RO(x))
#define INTEGER_RO(x)	((const int *) DATAPTR_RO(x))
#define RAW_RO(x)	((const Rbyte *) DATAPTR_RO(x))
#define COMPLEX_RO(x)	((const Rcomplex *) DATAPTR_RO(x))
#define REAL_RO(x)	((const double *) DATAPTR_RO(x))
#define STRING_PTR_RO(x)((const SEXP *) DATAPTR_RO(x))

/* List Access Macros */
/* These also work for ... objects */
#define TAG(e)		(R::SEXPREC::TAG(e))
#define CAR0(e)		(R::SEXPREC::CAR0(e))
#define EXTPTR_PTR(e)	(R::SEXPREC::EXTPTR_PTR(e))
#define CDR(e)		(R::SEXPREC::CDR(e))
#define CAAR(e)		CAR(CAR(e))
#define CDAR(e)		CDR(CAR(e))
#define CADR(e)		CAR(CDR(e))
#define CDDR(e)		CDR(CDR(e))
#define CDDDR(e)	CDR(CDDR(e))
#define CD4R(x)		CDR(CDR(CDR(CDR(x))))
#define CADDR(e)	CAR(CDDR(e))
#define CADDDR(e)	CAR(CDR(CDDR(e)))
#define CAD3R(e)	CAR(CDR(CDDR(e)))
#define CAD4R(e)	CAR(CDDR(CDDR(e)))
#define CAD5R(e)	CAR(CDR(CDR(CDR(CDR(CDR(e))))))
#define MISSING(x)	(R::SEXPREC::MISSING(x))/* for closure calls */
#define SET_MISSING(x,v) (R::SEXPREC::SET_MISSING(x, v))
#define BNDCELL_TAG(e)	(R::SEXPREC::BNDCELL_TAG(e))
#define SET_BNDCELL_TAG(e, v) (R::SEXPREC::SET_BNDCELL_TAG(e, v))

#if ( SIZEOF_SIZE_T < SIZEOF_DOUBLE )
# define BOXED_BINDING_CELLS 1
#else
# define BOXED_BINDING_CELLS 0
#endif
#if BOXED_BINDING_CELLS
/* Use allocated scalars to hold immediate binding values. A little
   less efficient but does not change memory layout or use. These
   allocated scalars must not escape their bindings. */
#define BNDCELL_DVAL(v) SCALAR_DVAL(CAR0(v))
#define BNDCELL_IVAL(v) SCALAR_IVAL(CAR0(v))
#define BNDCELL_LVAL(v) SCALAR_LVAL(CAR0(v))

#define SET_BNDCELL_DVAL(cell, dval) SET_SCALAR_DVAL(CAR0(cell), dval)
#define SET_BNDCELL_IVAL(cell, ival) SET_SCALAR_IVAL(CAR0(cell), ival)
#define SET_BNDCELL_LVAL(cell, lval) SET_SCALAR_LVAL(CAR0(cell), lval)

#define INIT_BNDCELL(cell, type)         \
    do                                   \
    {                                    \
        SEXP val = allocVector(type, 1); \
        SETCAR(cell, val);               \
        INCREMENT_NAMED(val);            \
        SET_BNDCELL_TAG(cell, type);     \
        SET_MISSING(cell, 0);            \
    } while (0)
#else
/* Use a union in the CAR field to represent an SEXP or an immediate
   value.  More efficient, but changes the memory layout on 32 bit
   platforms since the size of the union is larger than the size of a
   pointer. The layout should not change on 64 bit platforms. */
union R_bndval_t
{
    SEXP sxpval;
    double dval;
    int ival;
};
auto R::SEXPREC::BNDCELL_DVAL(SEXP v) { return v ? ((R_bndval_t *) &(v->u.listsxp.carval))->dval: 0; }
auto R::SEXPREC::BNDCELL_IVAL(SEXP v) { return v ? ((R_bndval_t *) &(v->u.listsxp.carval))->ival: 0; }
auto R::SEXPREC::BNDCELL_LVAL(SEXP v) { return v ? ((R_bndval_t *) &(v->u.listsxp.carval))->ival: 0; }
#define BNDCELL_DVAL(v) (R::SEXPREC::BNDCELL_DVAL(v))
#define BNDCELL_IVAL(v) (R::SEXPREC::BNDCELL_IVAL(v))
#define BNDCELL_LVAL(v) (R::SEXPREC::BNDCELL_LVAL(v))

void R::SEXPREC::SET_BNDCELL_DVAL(SEXP v, double x) { if(!v) return; ((R_bndval_t *) &(v->u.listsxp.carval))->dval = x; }
void R::SEXPREC::SET_BNDCELL_IVAL(SEXP v, int x) { if(!v) return; ((R_bndval_t *) &(v->u.listsxp.carval))->ival = x; }
void R::SEXPREC::SET_BNDCELL_LVAL(SEXP v, int x) { if(!v) return; ((R_bndval_t *) &(v->u.listsxp.carval))->ival = x; }
#define SET_BNDCELL_DVAL(cell, dval_) (R::SEXPREC::SET_BNDCELL_DVAL(cell, dval_))
#define SET_BNDCELL_IVAL(cell, ival_) (R::SEXPREC::SET_BNDCELL_IVAL(cell, ival_))
#define SET_BNDCELL_LVAL(cell, lval_) (R::SEXPREC::SET_BNDCELL_LVAL(cell, lval_))

#define INIT_BNDCELL(cell, type)      \
    do                                \
    {                                 \
        if (BNDCELL_TAG(cell) == 0)   \
            SETCAR(cell, R_NilValue); \
        SET_BNDCELL_TAG(cell, type);  \
        SET_MISSING(cell, 0);         \
    } while (0)
#endif

/* Closure Access Macros */
#define FORMALS(x)	(R::SEXPREC::FORMALS(x))
#define BODY(x)		(R::SEXPREC::BODY(x))
#define CLOENV(x)	(R::SEXPREC::CLOENV(x))
#define RDEBUG(x)	(R::SEXPREC::RDEBUG(x))
#define SET_RDEBUG(x,v)	(R::SEXPREC::SET_RDEBUG(x, v))
#define RSTEP(x)	(R::SEXPREC::RSTEP(x))
#define SET_RSTEP(x,v)	(R::SEXPREC::SET_RSTEP(x, v))

/* Symbol Access Macros */
#define PRINTNAME(x)	(R::SEXPREC::PRINTNAME(x))
#define SYMVALUE(x)	(R::SEXPREC::SYMVALUE(x))
#define INTERNAL(x)	(R::SEXPREC::INTERNAL(x))
#define DDVAL(x)	(R::SEXPREC::DDVAL(x))
#define SET_DDVAL_BIT(x)	(R::SEXPREC::SET_DDVAL_BIT(x))
#define UNSET_DDVAL_BIT(x)	(R::SEXPREC::UNSET_DDVAL_BIT(x))
#define SET_DDVAL(x,v)	(R::SEXPREC::SET_DDVAL(x, v))

/* Environment Access Macros */
#define FRAME(x)	(R::SEXPREC::FRAME(x))
#define ENCLOS(x)	(R::SEXPREC::ENCLOS(x))
#define HASHTAB(x)	(R::SEXPREC::HASHTAB(x))
#define ENVFLAGS(x)	(R::SEXPREC::ENVFLAGS(x))	/* for environments */
#define SET_ENVFLAGS(x,v)	(R::SEXPREC::SET_ENVFLAGS(x, v))

#else /* not USE_RINTERNALS */
// ======================= not USE_RINTERNALS section

// =====
// These are required by stringi and data.table packages
// if we're disabling USE_RINTERNALS for them.
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
#ifndef ENC_KNOWN
#define ENC_KNOWN(x) (LEVELS(x) & 12)
#endif
// =====

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
#ifdef TESTING_WRITE_BARRIER
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
#define CONS(a, b)	cons((a), (b))		/* data lists */
#define LCONS(a, b)	lcons((a), (b))		/* language lists */
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
SEXP (CADDDR)(SEXP e);
SEXP (CAD4R)(SEXP e);
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
LibExtern SEXP	R_NilValue;	    /* The nil object */
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
SEXP Rf_arraySubscript(int, SEXP, SEXP, SEXP (*)(SEXP, SEXP),
                       SEXP (*)(SEXP, int), SEXP);
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
SEXP Rf_mkChar(const char *);
SEXP Rf_mkCharLen(const char *, int);
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
int R_nchar(SEXP string, nchar_type type_,
	    Rboolean allowNA, Rboolean keepNA, const char* msg_name);

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
void Rf_setSVector(SEXP*, int, SEXP);
void Rf_setVar(SEXP, SEXP, SEXP);
SEXP Rf_stringSuffix(SEXP, int);
SEXPTYPE Rf_str2type(const char *);
Rboolean Rf_StringBlank(SEXP);
SEXP Rf_substitute(SEXP,SEXP);
SEXP Rf_topenv(SEXP, SEXP);
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
SEXP R_tryEval(SEXP, SEXP, int *);
SEXP R_tryEvalSilent(SEXP, SEXP, int *);
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
SEXP R_ExecWithCleanup(SEXP (*fun)(void *), void *data,
		       void (*cleanfun)(void *), void *cleandata);
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
void Rf_warningcall(SEXP, const char *, ...);
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
    SEXP (*OutPersistHookFunc)
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
SEXP R_getClassDef  (const char *what);
SEXP R_getClassDef_R(SEXP what);
Rboolean R_has_methods_attached(void);
Rboolean R_isVirtualClass(SEXP class_def, SEXP env);
Rboolean R_extends  (SEXP class1, SEXP class2, SEXP env);
SEXP R_do_new_object(SEXP class_def);
/* supporting  a C-level version of  is(., .) : */
int R_check_class_and_super(SEXP x, const char **valid, SEXP rho);
int R_check_class_etc      (SEXP x, const char **valid);

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
void R_orderVector (int *indx, int n, SEXP arglist, Rboolean nalast, Rboolean decreasing);
// C version of R's  indx <- order(x, na.last, decreasing) :
void R_orderVector1(int *indx, int n, SEXP x,       Rboolean nalast, Rboolean decreasing);

#ifndef R_NO_REMAP
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
//#define rownamesgets		Rf_rownamesgets
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
#define warningcall		Rf_warningcall
#define warningcall_immediate	Rf_warningcall_immediate
#define xlength(x)		Rf_xlength(x)
#define xlengthgets		Rf_xlengthgets

#endif

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

#ifdef USE_RINTERNALS

/* Test macros with function versions above */
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

#ifdef __cplusplus

/** @brief Shorthand for Rf_length().
 */
inline auto length(SEXP s)
{
    return Rf_length(s);
}
#endif

#endif /* R_INTERNALS_H_ */
