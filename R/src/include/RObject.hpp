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

#ifndef ROBJECT_HPP
#define ROBJECT_HPP

#define USE_RINTERNALS
#include <Rinternals.h>

/* This is intended for use only within R itself.
 * It defines internal structures that are otherwise only accessible
 * via SEXP, and macros to replace many (but not all) of accessor functions
 * (which are always defined).
 */

/* These are also used with the write barrier on, in attrib.cpp and util.cpp */
constexpr int BASIC_TYPE_BITS = 5;
constexpr int FULL_TYPE_BITS = 8;
constexpr int MAX_NUM_BASIC_SEXPTYPE = (1 << BASIC_TYPE_BITS);
constexpr int MAX_NUM_SEXPTYPE = (1 << FULL_TYPE_BITS);
constexpr int NAMED_BITS = 16;

/* Flags */
namespace R
{
    struct sxpinfo_struct
    {
        SEXPTYPE m_type : FULL_TYPE_BITS;
        bool m_scalar;
        bool m_obj;
        bool m_alt;
        unsigned int m_gp : 16;
        bool m_mark;
        bool m_debug;
        bool m_trace;             /* functions and memory tracing */
        bool m_spare;             /* used on closures and when REFCNT is defined */
        bool m_gcgen;             /* old generation number */
        unsigned int m_gccls : 3; /* node class */
        unsigned int m_named : NAMED_BITS;
        unsigned int m_extra : 29 - NAMED_BITS; /* used for immediate bindings */
    };                                          /*		    Tot: 64 */

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
        RObject *pname;
        RObject *value;
        RObject *internal;
    };

    struct listsxp_struct
    {
        RObject *carval;
        RObject *cdrval;
        RObject *tagval;
    };

    struct envsxp_struct
    {
        RObject *frame;
        RObject *enclos;
        RObject *hashtab;
    };

    struct closxp_struct
    {
        RObject *formals;
        RObject *body;
        RObject *env;
    };

    struct promsxp_struct
    {
        RObject *value;
        RObject *expr;
        RObject *env;
    };

    /* Every node must start with a set of sxpinfo flags and an attribute
   field. Under the generational collector these are followed by the
   fields used to maintain the collector's linked list structures. */
} // namespace R
#ifdef SWITCH_TO_REFCNT
constexpr int REFCNTMAX = ((1 << NAMED_BITS) - 1);
#endif
namespace R
{

    /* The standard node structure consists of a header followed by the
   node data. */
    class RObject
    {
    private:
        sxpinfo_struct sxpinfo;
        RObject *m_attrib;
        RObject *gengc_next_node;
        RObject *gengc_prev_node;
        union
        {
            primsxp_struct primsxp;
            symsxp_struct symsxp;
            listsxp_struct listsxp;
            envsxp_struct envsxp;
            closxp_struct closxp;
            promsxp_struct promsxp;
        } u;
        // virtual ~RObject() = default;
#if 0
    SEXPTYPE sexptype() const { return this->sxpinfo.m_type; }
    void setsexptype(const SEXPTYPE& type) { this->sxpinfo.m_type = type; }
    bool sexptypeEqual(const SEXPTYPE& type) const { return this->sxpinfo.m_type == type; }
    bool sexptypeNotEqual(const SEXPTYPE& type) const { return !this->sexptypeEqual(type); }
    bool altrep() const { return this->sxpinfo.m_alt; }
    void setaltrep() { this->sxpinfo.m_alt = true; }
    void unsetaltrep() { this->sxpinfo.m_alt = false; }
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
    bool isScalar(const SEXPTYPE &t) const { return this->sexptypeEqual(t) && this->sxpinfo.m_scalar; }
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
    RObject *attrib_() const { return this->m_attrib; }
    inline unsigned int isBytes() const;
    inline void setBytes();
    inline unsigned int isLatin1() const;
    inline void setLatin1();
    inline unsigned int isAscii() const;
    inline void setAscii();
    inline unsigned int isUTF8() const;
    inline void setUTF8();
    inline aunsigned intto encKnown() const;
    inline unsigned int isCached() const;
    inline void setCached();
    RObject *printname() const { return this->u.symsxp.pname; }
    RObject *symvalue() const { return this->u.symsxp.value; }
    RObject *internal() const { return this->u.symsxp.internal; }
    RObject *tag() const { return this->u.listsxp.tagval; }
    RObject *car() const { return this->u.listsxp.carval; }
    RObject *cdr() const { return this->u.listsxp.cdrval; }
    const char *translateCharUTF8_() const;
#endif
    public:
        /* General Cons Cell Attributes */
        static bool gcgen(SEXP v);
        static void set_gcgen(SEXP v, bool x);
        static unsigned int gccls(SEXP v);
        static void set_gccls(SEXP v, unsigned int x);
        static RObject *next_node(SEXP s);
        static RObject *prev_node(SEXP s);
        static void set_next_node(SEXP s, SEXP t);
        static void set_prev_node(SEXP s, SEXP t);
        static void copy_sxpinfo(SEXP x, RObject &y);
        static constexpr int READY_TO_FINALIZE_MASK = 1;
        static constexpr int FINALIZE_ON_EXIT_MASK = 2;
        static constexpr int WEAKREF_SIZE = 4;
        static void set_ready_to_finalize(SEXP s);
        static void clear_ready_to_finalize(SEXP s);
        static unsigned int is_ready_to_finalize(SEXP s);
        static void set_finalize_on_exit(SEXP s);
        static void clear_finalize_on_exit(SEXP s);
        static unsigned int finalize_on_exit(SEXP s);
        static void set_attrib(SEXP x, SEXP v);
        static SEXP attrib(SEXP x);
        static unsigned int named(SEXP x);
        static void set_named(SEXP x, unsigned int v);
        static void set_typeof(SEXP x, SEXPTYPE v);
        static SEXPTYPE typeof_(SEXP x);
        static unsigned int levels(SEXP x);
        static bool object(SEXP x);
        static void set_object(SEXP x, bool v);
        static bool mark(SEXP x);
        static void set_mark(SEXP x, int v);
        /* CHARSXP charset bits */
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
        static bool scalar(SEXP x);
        static unsigned int is_bytes(SEXP x);
        static void set_bytes(SEXP x);
        static unsigned int is_latin1(SEXP x);
        static void set_latin1(SEXP x);
        static unsigned int is_ascii(SEXP x);
        static void set_ascii(SEXP x);
        static unsigned int is_utf8(SEXP x);
        static void set_utf8(SEXP x);
        static unsigned int enc_known(SEXP x);
        static void set_cached(SEXP x);
        static unsigned int is_cached(SEXP x);
        static bool altrep(SEXP x);
        static void set_altrep(SEXP x, bool v);
        static void setlevels(SEXP x, unsigned short int v);
        static void setscalar(SEXP x, bool v);
        static bool is_scalar(SEXP x, SEXPTYPE t);
        static unsigned int refcnt(SEXP x);
        static void set_refcnt(SEXP x, unsigned int v);
        static bool trackrefs(SEXP x);
        static void set_trackrefs(SEXP x, bool v);
        ;
        static constexpr int ASSIGNMENT_PENDING_MASK = (1 << 11);
        static unsigned int assignment_pending(SEXP x);
        ;
        static void set_assignment_pending(SEXP x, bool v);
        static bool rtrace(SEXP x);
        static void set_rtrace(SEXP x, bool v);
        /* Primitive Access Methods */
        static int primoffset(SEXP x);
        static void set_primoffset(SEXP x, int v);
        /* Closure Access Methods */
        static RObject *formals(SEXP x);
        static void set_formals(SEXP x, SEXP v);
        static RObject *body(SEXP x);
        static void set_body(SEXP x, SEXP v);
        static RObject *cloenv(SEXP x);
        static void set_cloenv(SEXP x, SEXP v);
        static bool rdebug(SEXP x);
        static void set_rdebug(SEXP x, int v);
        static bool rstep(SEXP x);
        static void set_rstep(SEXP x, int v);
        /* Symbol Access Methods */
        static constexpr int DDVAL_MASK = 1;
        static RObject *printname(SEXP x);
        static RObject *symvalue(SEXP x);
        static RObject *internal(SEXP x);
        static unsigned int ddval(SEXP x); /* for ..1, ..2 etc */
        static void set_ddval_bit(SEXP x);
        static void unset_ddval_bit(SEXP x);
        static void set_ddval(SEXP x, int v); /* for ..1, ..2 etc */
        static void set_printname(SEXP x, SEXP v);
        static void set_symvalue(SEXP x, SEXP v);
        static void set_internal(SEXP x, SEXP v);
        /* Environment Access Methods */
        static constexpr int FRAME_LOCK_MASK = (1 << 14);
        static constexpr int GLOBAL_FRAME_MASK = (1 << 15);
        static RObject *frame(SEXP x);
        static RObject *enclos(SEXP x);
        static RObject *hashtab(SEXP x);
        static unsigned int envflags(SEXP x); /* for environments */
        static void set_envflags(SEXP x, int v);
        static void set_frame(SEXP x, SEXP v);
        static void set_enclos(SEXP x, SEXP v);
        static void set_hashtab(SEXP x, SEXP v);
        static unsigned int frame_is_locked(SEXP e);
        static unsigned int is_global_frame(SEXP e);
        /* Promise Access Methods */
        static RObject *prcode(SEXP x);
        static void set_prcode(SEXP x, SEXP v);
        static RObject *prenv(SEXP x);
        static RObject *prvalue(SEXP x);
        static void set_prvalue(SEXP x, SEXP v);
        static unsigned int prseen(SEXP x);
        static void set_prenv(SEXP x, SEXP v);
        static void set_prseen(SEXP x, int v);
        /* List Access Methods */
        static RObject *tag(SEXP e);
        static void set_tag(SEXP x, SEXP v);
        static RObject *car0(SEXP e);
        static void set_car0(SEXP x, SEXP v);
        static RObject *extptr_ptr(SEXP e);
        static void set_extptr_ptr(SEXP x, SEXP v);
        static RObject *cdr(SEXP e);
        static void set_cdr(SEXP x, SEXP v);
        static constexpr int MISSING_MASK = ((1 << 4) - 1); // = 15 /* reserve 4 bits--only 2 uses now */
        static unsigned int missing(SEXP x);                /* for closure calls */
        static void set_missing(SEXP x, int v);
        static unsigned int bndcell_tag(SEXP e);
        static void set_bndcell_tag(SEXP e, unsigned int v);
        /* External pointer access methods */
        static RObject *extptr_prot(SEXP x);
        static RObject *extptr_tag(SEXP x);
        static void set_extptr_tag(SEXP x, SEXP v);
        static void set_extptr_prot(SEXP x, SEXP v);
        /* S4 object bit, set by R_do_new_object for all new() calls */
        static constexpr int S4_OBJECT_MASK = ((unsigned short)(1 << 4));
        static bool is_s4_object(SEXP x);
        static void set_s4_object(SEXP x);
        static void unset_s4_object(SEXP x);
        /* JIT optimization support */
        enum JITBit
        {
            NOJIT_MASK = (1 << 5),
            MAYBEJIT_MASK = (1 << 6)
        };
        static unsigned int nojit(SEXP x);
        static void set_nojit(SEXP x);
        static unsigned int maybejit(SEXP x);
        static void set_maybejit(SEXP x);
        static void unset_maybejit(SEXP x);
        /* Growable vector support */
        static constexpr int GROWABLE_MASK = ((unsigned short)(1 << 5));
        static unsigned int growable_bit_set(SEXP x);
        static void set_growable_bit(SEXP x);
        /* Hashing Methods */
        static constexpr int HASHASH_MASK = 1;
        static unsigned int hashash(SEXP x);
        static void set_hashash(SEXP x, bool v);

        static constexpr int SPECIAL_SYMBOL_MASK = (1 << 12);
        static constexpr int BASE_SYM_CACHED_MASK = (1 << 13);
        static void set_base_sym_cached(SEXP b);
        static void unset_base_sym_cached(SEXP b);
        static unsigned int base_sym_cached(SEXP b);
        static unsigned int no_special_symbols(SEXP b);
        static void set_no_special_symbols(SEXP b);
        static unsigned int is_special_symbol(SEXP b);
        static void set_special_symbol(SEXP b);
        static void unset_no_special_symbols(SEXP b);
        static void unset_special_symbol(SEXP b);
        static constexpr int ACTIVE_BINDING_MASK = (1 << 15);
        static constexpr int BINDING_LOCK_MASK = (1 << 14);
        static constexpr int SPECIAL_BINDING_MASK = (ACTIVE_BINDING_MASK | BINDING_LOCK_MASK);
        static unsigned int is_active_binding(SEXP b);
        static unsigned int binding_is_locked(SEXP b);
        static void lock_binding(SEXP b);
        static void unlock_binding(SEXP b);
        static void set_active_binding_bit(SEXP b);
        static double bndcell_dval(SEXP v);
        static int bndcell_ival(SEXP v);
        static int bndcell_lval(SEXP v);
        static void set_bndcell_dval(SEXP v, double x);
        static void set_bndcell_ival(SEXP v, int x);
        static void set_bndcell_lval(SEXP v, int x);
    };
#if 0
class Symbol : public RObject {
    struct {
        struct symsxp_struct symsxp;
    } u;
    RObject *printname() const {
        return this->u.symsxp.pname;
    }
    RObject *symvalue() const {
        return this->u.symsxp.value;
    }
    RObject *internal() const {
        return this->u.symsxp.internal;
    }
};

class Environment : public RObject {
    struct {
        struct envsxp_struct envsxp;
    } u;
};

class BuiltInFunction : public RObject {
    struct {
        struct primsxp_struct primsxp;
    } u;
};

class Closure : public RObject {
    struct {
        struct closxp_struct closxp;
    } u;
};

class Promise : public RObject {
    struct {
        struct promsxp_struct promsxp;
    } u;
};

class RList : public RObject {
    struct {
        struct listsxp_struct listsxp;
    } u;
    RObject *tag() const {
        return this->u.listsxp.tagval;
    }
    RObject *car() const {
        return this->u.listsxp.carval;
    }
    RObject *cdr() const {
        return this->u.listsxp.cdrval;
    }
};
#endif

    /* The generational collector uses a reduced version of RObject as a
   header in vector nodes.  The layout MUST be kept consistent with
   the RObject definition. The standard RObject takes up 7 words
   and the reduced version takes 6 words on most 64-bit systems. On most
   32-bit systems, RObject takes 8 words and the reduced version 7 words. */
    struct VECTOR_SEXPREC
    {
        sxpinfo_struct sxpinfo;
        RObject *attrib;
        RObject *gengc_next_node;
        RObject *gengc_prev_node;
        vecsxp_struct vecsxp;
    };

    using VECSEXP = struct R::VECTOR_SEXPREC *;

    union SEXPREC_ALIGN
    {
        VECTOR_SEXPREC s;
        double align;
    };
} // namespace R

/* General Cons Cell Attributes */
#define ATTRIB(x) (R::RObject::attrib(x))
#define OBJECT(x) (R::RObject::object(x))
#define MARK(x) (R::RObject::mark(x))
#define TYPEOF(x) (R::RObject::typeof_(x))
#define NAMED(x) (R::RObject::named(x))
#define RTRACE(x) (R::RObject::rtrace(x))
#define LEVELS(x) (R::RObject::levels(x))
#define SET_OBJECT(x, v) (R::RObject::set_object(x, v))
#define SET_TYPEOF(x, v) (R::RObject::set_typeof(x, v))
#define SET_NAMED(x, v) (R::RObject::set_named(x, v))
#define SET_RTRACE(x, v) (R::RObject::set_rtrace(x, v))
#define SETLEVELS(x, v) (R::RObject::setlevels(x, v))
#define ALTREP(x) (R::RObject::altrep(x))
#define SETALTREP(x, v) (R::RObject::set_altrep(x, v))
#define SETSCALAR(x, v) (R::RObject::setscalar(x, v))

#define IS_BYTES(x) (R::RObject::is_bytes(x))
#define SET_BYTES(x) (R::RObject::set_bytes(x))
#define IS_LATIN1(x) (R::RObject::is_latin1(x))
#define SET_LATIN1(x) (R::RObject::set_latin1(x))
#define IS_ASCII(x) (R::RObject::is_ascii(x))
#define SET_ASCII(x) (R::RObject::set_ascii(x))
#define IS_UTF8(x) (R::RObject::is_utf8(x))
#define SET_UTF8(x) (R::RObject::set_utf8(x))
#define ENC_KNOWN(x) (R::RObject::enc_known(x))
#define SET_CACHED(x) (R::RObject::set_cached(x))
#define IS_CACHED(x) (R::RObject::is_cached(x))

#if defined(COMPUTE_REFCNT_VALUES)
#define REFCNT(x) (R::RObject::refcnt(x))
#define TRACKREFS(x) (R::RObject::trackrefs(x))
#else
#define REFCNT(x) 0
#define TRACKREFS(x) false
#endif

#if defined(COMPUTE_REFCNT_VALUES)
#define SET_REFCNT(x, v) (R::RObject::set_refcnt(x, v))
#if defined(EXTRA_REFCNT_FIELDS)
#define SET_TRACKREFS(x, v) (R::RObject::set_trackrefs(x, v))
#else
#define SET_TRACKREFS(x, v) (R::RObject::set_trackrefs(x, !v))
#endif
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
#define SET_REFCNT(x, v) \
    do                   \
    {                    \
    } while (0)
#define SET_TRACKREFS(x, v) \
    do                      \
    {                       \
    } while (0)
#define DECREMENT_REFCNT(x) \
    do                      \
    {                       \
    } while (0)
#define INCREMENT_REFCNT(x) \
    do                      \
    {                       \
    } while (0)
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

#define ASSIGNMENT_PENDING(x) (R::RObject::assignment_pending(x))
#define SET_ASSIGNMENT_PENDING(x, v) (R::RObject::set_assignment_pending(x, v))

/* The same bit can be used to mark calls used in complex assignments
   to allow replacement functions to determine when they are being
   called in an assignment context and can modify an object with one
   refrence */
#define MARK_ASSIGNMENT_CALL(call) SET_ASSIGNMENT_PENDING(call, TRUE)
#define IS_ASSIGNMENT_CALL(call) ASSIGNMENT_PENDING(call)

#ifdef SWITCH_TO_REFCNT
#undef NAMED
#undef SET_NAMED
#define NAMED(x) REFCNT(x)
/* no definition for SET_NAMED; any calls will use the one in memory.cpp */
#define ENSURE_NAMEDMAX(v) \
    do                     \
    {                      \
    } while (0)
#define ENSURE_NAMED(v) \
    do                  \
    {                   \
    } while (0)
#else
#define ENSURE_NAMEDMAX(v)                  \
    do                                      \
    {                                       \
        SEXP __enm_v__ = (v);               \
        if (NAMED(__enm_v__) < NAMEDMAX)    \
            SET_NAMED(__enm_v__, NAMEDMAX); \
    } while (0)
#define ENSURE_NAMED(v)      \
    do                       \
    {                        \
        if (NAMED(v) == 0)   \
            SET_NAMED(v, 1); \
    } while (0)
#endif

#ifdef SWITCH_TO_REFCNT
#define SETTER_CLEAR_NAMED(x) \
    do                        \
    {                         \
    } while (0)
#define RAISE_NAMED(x, n) \
    do                    \
    {                     \
    } while (0)
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
#define IS_S4_OBJECT(x) (R::RObject::is_s4_object(x))
#define SET_S4_OBJECT(x) (R::RObject::set_s4_object(x))
#define UNSET_S4_OBJECT(x) (R::RObject::unset_s4_object(x))

/* JIT optimization support */
#define NOJIT(x) (R::RObject::nojit(x))
#define SET_NOJIT(x) (R::RObject::set_nojit(x))
#define MAYBEJIT(x) (R::RObject::maybejit(x))
#define SET_MAYBEJIT(x) (R::RObject::set_maybejit(x))
#define UNSET_MAYBEJIT(x) (R::RObject::unset_maybejit(x))

/* Growable vector support */
#define GROWABLE_BIT_SET(x) (R::RObject::growable_bit_set(x))
#define SET_GROWABLE_BIT(x) (R::RObject::set_growable_bit(x))
#define IS_GROWABLE(x) (GROWABLE_BIT_SET(x) && XLENGTH(x) < XTRUELENGTH(x))

/* Vector Access Macros */
#ifdef LONG_VECTOR_SUPPORT
#define IS_LONG_VEC(x) (XLENGTH(x) > R_SHORT_LEN_MAX)
#else
#define IS_LONG_VEC(x) false
#endif
#define STDVEC_LENGTH(x) (((R::VECSEXP)(x))->vecsxp.length)
#define STDVEC_TRUELENGTH(x) (((R::VECSEXP)(x))->vecsxp.truelength)
#define SET_STDVEC_TRUELENGTH(x, v) (STDVEC_TRUELENGTH(x) = (v))
#define SET_TRUELENGTH(x, v)                      \
    do                                            \
    {                                             \
        SEXP sl__x__ = (x);                       \
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
#define STDVEC_DATAPTR(x) ((void *)(((R::SEXPREC_ALIGN *)(x)) + 1))
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

/* List Access Macros */
/* These also work for ... objects */
#define TAG(e) (R::RObject::tag(e))
#define CAR0(e) (R::RObject::car0(e))
#define EXTPTR_PTR(e) (R::RObject::extptr_ptr(e))
#define CDR(e) (R::RObject::cdr(e))
#define CAAR(e) CAR(CAR(e))
#define CDAR(e) CDR(CAR(e))
#define CADR(e) CAR(CDR(e))
#define CDDR(e) CDR(CDR(e))
#define CDDDR(e) CDR(CDDR(e))
#define CD4R(x) CDR(CDR(CDR(CDR(x))))
#define CADDR(e) CAR(CDDR(e))
#define CADDDR(e) CAR(CDR(CDDR(e)))
#define CAD3R(e) CAR(CDR(CDDR(e)))
#define CAD4R(e) CAR(CDDR(CDDR(e)))
#define CAD5R(e) CAR(CDR(CDR(CDR(CDR(CDR(e))))))
#define MISSING(x) (R::RObject::missing(x)) /* for closure calls */
#define SET_MISSING(x, v) (R::RObject::set_missing(x, v))
#define BNDCELL_TAG(e) (R::RObject::bndcell_tag(e))
#define SET_BNDCELL_TAG(e, v) (R::RObject::set_bndcell_tag(e, v))

#if (SIZEOF_SIZE_T < SIZEOF_DOUBLE)
#define BOXED_BINDING_CELLS 1
#else
#define BOXED_BINDING_CELLS 0
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

#define BNDCELL_DVAL(v) (R::RObject::bndcell_dval(v))
#define BNDCELL_IVAL(v) (R::RObject::bndcell_ival(v))
#define BNDCELL_LVAL(v) (R::RObject::bndcell_lval(v))

#define SET_BNDCELL_DVAL(cell, dval_) (R::RObject::set_bndcell_dval(cell, dval_))
#define SET_BNDCELL_IVAL(cell, ival_) (R::RObject::set_bndcell_ival(cell, ival_))
#define SET_BNDCELL_LVAL(cell, lval_) (R::RObject::set_bndcell_lval(cell, lval_))

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
#define FORMALS(x) (R::RObject::formals(x))
#define BODY(x) (R::RObject::body(x))
#define CLOENV(x) (R::RObject::cloenv(x))
#define RDEBUG(x) (R::RObject::rdebug(x))
#define SET_RDEBUG(x, v) (R::RObject::set_rdebug(x, v))
#define RSTEP(x) (R::RObject::rstep(x))
#define SET_RSTEP(x, v) (R::RObject::set_rstep(x, v))

/* Symbol Access Macros */
#define PRINTNAME(x) (R::RObject::printname(x))
#define SYMVALUE(x) (R::RObject::symvalue(x))
#define INTERNAL(x) (R::RObject::internal(x))
#define DDVAL(x) (R::RObject::ddval(x))
#define SET_DDVAL_BIT(x) (R::RObject::set_ddval_bit(x))
#define UNSET_DDVAL_BIT(x) (R::RObject::unset_ddval_bit(x))
#define SET_DDVAL(x, v) (R::RObject::set_ddval(x, v))

/* Environment Access Macros */
#define FRAME(x) (R::RObject::frame(x))
#define ENCLOS(x) (R::RObject::enclos(x))
#define HASHTAB(x) (R::RObject::hashtab(x))
#define ENVFLAGS(x) (R::RObject::envflags(x)) /* for environments */
#define SET_ENVFLAGS(x, v) (R::RObject::set_envflags(x, v))

/* There is much more in Rinternals.h, including function versions
 * of the Promise and Hashing groups.
 */

/* Primitive Access Macros */
#define PRIMOFFSET(x) (R::RObject::primoffset(x))
#define SET_PRIMOFFSET(x, v) (R::RObject::set_primoffset(x, v))
#define PRIMFUN(x) (R_FunTab[PRIMOFFSET(x)].cfun)
#define PRIMNAME(x) (R_FunTab[PRIMOFFSET(x)].name)
#define PRIMVAL(x) (R_FunTab[PRIMOFFSET(x)].code)
#define PRIMARITY(x) (R_FunTab[PRIMOFFSET(x)].arity)
#define PPINFO(x) (R_FunTab[PRIMOFFSET(x)].gram)
#define PRIMPRINT(x) (((R_FunTab[PRIMOFFSET(x)].eval) / 100) % 10)
#define PRIMINTERNAL(x) (((R_FunTab[PRIMOFFSET(x)].eval) % 100) / 10)

/* Promise Access Macros */
#define PRCODE(x) (R::RObject::prcode(x))
#define PRENV(x) (R::RObject::prenv(x))
#define PRVALUE(x) (R::RObject::prvalue(x))
#define PRSEEN(x) (R::RObject::prseen(x))
#define SET_PRSEEN(x, v) (R::RObject::set_prseen(x, v))

/* Hashing Macros */
#define HASHASH(x) (R::RObject::hashash(x))
#define HASHVALUE(x) ((int)TRUELENGTH(x))
#define SET_HASHASH(x, v) (R::RObject::set_hashash(x, v))
#define SET_HASHVALUE(x, v) SET_TRUELENGTH(x, ((int)(v)))

/* Vector Heap Structure */
struct VECREC
{
    union
    {
        SEXP backpointer;
        double align;
    } u;
};

/* Vector Heap Macros */
// #define BACKPOINTER(v) ((v).u.backpointer)
inline size_t BYTE2VEC(int n) { return (n > 0) ? (std::size_t(n) - 1) / sizeof(VECREC) + 1 : 0; }
inline size_t INT2VEC(int n) { return (n > 0) ? (std::size_t(n) * sizeof(int) - 1) / sizeof(VECREC) + 1 : 0; }
inline size_t FLOAT2VEC(int n) { return (n > 0) ? (std::size_t(n) * sizeof(double) - 1) / sizeof(VECREC) + 1 : 0; }
inline size_t COMPLEX2VEC(int n) { return (n > 0) ? (std::size_t(n) * sizeof(Rcomplex) - 1) / sizeof(VECREC) + 1 : 0; }
inline size_t PTR2VEC(int n) { return (n > 0) ? (std::size_t(n) * sizeof(SEXP) - 1) / sizeof(VECREC) + 1 : 0; }

/* Bindings */
/* use the same bits (15 and 14) in symbols and bindings */
#define IS_ACTIVE_BINDING(b) (R::RObject::is_active_binding(b))
#define BINDING_IS_LOCKED(b) (R::RObject::binding_is_locked(b))
#define SET_ACTIVE_BINDING_BIT(b) (R::RObject::set_active_binding_bit(b))
#define LOCK_BINDING(b) (R::RObject::lock_binding(b))

#define UNLOCK_BINDING(b) (R::RObject::unlock_binding(b))

#define SET_BASE_SYM_CACHED(b) (R::RObject::set_base_sym_cached(b))
#define UNSET_BASE_SYM_CACHED(b) (R::RObject::unset_base_sym_cached(b))
#define BASE_SYM_CACHED(b) (R::RObject::base_sym_cached(b))

#define SET_SPECIAL_SYMBOL(b) (R::RObject::set_special_symbol(b))
#define UNSET_SPECIAL_SYMBOL(b) (R::RObject::unset_special_symbol(b))
#define IS_SPECIAL_SYMBOL(b) (R::RObject::is_special_symbol(b))
#define SET_NO_SPECIAL_SYMBOLS(b) (R::RObject::set_no_special_symbols(b))
#define UNSET_NO_SPECIAL_SYMBOLS(b) (R::RObject::unset_no_special_symbols(b))
#define NO_SPECIAL_SYMBOLS(b) (R::RObject::no_special_symbols(b))


#endif /* ROBJECT_HPP */