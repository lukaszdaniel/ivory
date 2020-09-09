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

#define SWITCH_TO_REFCNT
#define COMPUTE_REFCNT_VALUES
#include <cstddef>
#include <CXXR/SEXPTYPE.hpp>
#include <CXXR/RTypes.hpp>
#include <R_ext/Error.h>
#include <R_ext/Complex.h>

/* This is intended for use only within R itself.
 * It defines internal structures that are otherwise only accessible
 * via RObject*, and macros to replace many (but not all) of accessor functions
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
    class RObject;

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

    struct vecsxp_struct
    {
        R_len_t m_length;
        R_len_t m_truelength;
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
        SEXPTYPE m_type : FULL_TYPE_BITS;
        bool m_scalar;
        bool m_has_class;
        bool m_alt;
        unsigned int m_gpbits : 16;
        bool m_marked;
        bool m_debug;
        bool m_trace;             /* functions and memory tracing */
        bool m_spare;             /* used on closures and when REFCNT is defined */
        bool m_gcgen;             /* old generation number */
        unsigned int m_gcclass : 3; /* node class */
        unsigned int m_named : NAMED_BITS;
        unsigned int m_extra : 29 - NAMED_BITS; /* used for immediate bindings */
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
            vecsxp_struct vecsxp;
        } u;
    public:
    void* m_data;
    size_t m_databytes;
        // virtual ~RObject() = default;
#if 0
    SEXPTYPE sexptype() const { return this->m_type; }
    void setsexptype(const SEXPTYPE& type) { this->m_type = type; }
    bool sexptypeEqual(const SEXPTYPE& type) const { return this->m_type == type; }
    bool sexptypeNotEqual(const SEXPTYPE& type) const { return !this->sexptypeEqual(type); }
    bool altrep() const { return this->m_alt; }
    void setaltrep() { this->m_alt = true; }
    void unsetaltrep() { this->m_alt = false; }
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
    bool isScalar(const SEXPTYPE &t) const { return this->sexptypeEqual(t) && this->m_scalar; }
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
    // virtual ~RObject() {}
        /* General Cons Cell Attributes */
        static bool gcgen(RObject *v);
        static void set_gcgen(RObject *v, bool x);
        static unsigned int gccls(RObject *x);
        static void set_gccls(RObject *x, unsigned int v);
        static RObject *next_node(RObject *s);
        static RObject *prev_node(RObject *s);
        static void set_next_node(RObject *s, RObject *t);
        static void set_prev_node(RObject *s, RObject *t);
        static void copy_sxpinfo(RObject *x, RObject &y);
        // Make t the successor of s:
        static inline void link(RObject *s, RObject *t)
        {
            s->gengc_next_node = t;
            t->gengc_prev_node = s;
        }
        static constexpr int READY_TO_FINALIZE_MASK = 1;
        static constexpr int FINALIZE_ON_EXIT_MASK = 2;
        static constexpr int WEAKREF_SIZE = 4;
        static void set_ready_to_finalize(RObject *x);
        static void clear_ready_to_finalize(RObject *x);
        static unsigned int is_ready_to_finalize(RObject *x);
        static void set_finalize_on_exit(RObject *x);
        static void clear_finalize_on_exit(RObject *x);
        static unsigned int finalize_on_exit(RObject *x);
        static void set_attrib(RObject *x, RObject *v);
        static RObject *attrib(RObject *x);
        static unsigned int named(RObject *x);
        static void set_named(RObject *x, unsigned int v);
        static void set_typeof(RObject *x, SEXPTYPE v);
        static SEXPTYPE typeof_(RObject *x);
        static unsigned int levels(RObject *x);
        static bool object(RObject *x);
        static void set_object(RObject *x, bool v);
        static bool mark(RObject *x);
        static void set_mark(RObject *x, int v);
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
        static bool scalar(RObject *x);
        static unsigned int is_bytes(RObject *x);
        static void set_bytes(RObject *x);
        static unsigned int is_latin1(RObject *x);
        static void set_latin1(RObject *x);
        static unsigned int is_ascii(RObject *x);
        static void set_ascii(RObject *x);
        static unsigned int is_utf8(RObject *x);
        static void set_utf8(RObject *x);
        static unsigned int enc_known(RObject *x);
        static void set_cached(RObject *x);
        static unsigned int is_cached(RObject *x);
        static bool altrep(RObject *x);
        static void set_altrep(RObject *x, bool v);
        static void setlevels(RObject *x, unsigned short int v);
        static void setscalar(RObject *x, bool v);
        static bool is_scalar(RObject *x, SEXPTYPE t);
        static unsigned int refcnt(RObject *x);
        static void set_refcnt(RObject *x, unsigned int v);
        static bool trackrefs(RObject *x);
        static void set_trackrefs(RObject *x, bool v);
        static constexpr int ASSIGNMENT_PENDING_MASK = (1 << 11);
        static unsigned int assignment_pending(RObject *x);
        static void set_assignment_pending(RObject *x, bool v);
        static bool rtrace(RObject *x);
        static void set_rtrace(RObject *x, bool v);
        /* Primitive Access Methods */
        static int primoffset(RObject *x);
        static void set_primoffset(RObject *x, int v);
        /* Closure Access Methods */
        static RObject *formals(RObject *x);
        static void set_formals(RObject *x, RObject *v);
        static RObject *body(RObject *x);
        static void set_body(RObject *x, RObject *v);
        static RObject *cloenv(RObject *x);
        static void set_cloenv(RObject *x, RObject *v);
        static bool rdebug(RObject *x);
        static void set_rdebug(RObject *x, bool v);
        static bool rstep(RObject *x);
        static void set_rstep(RObject *x, bool v);
        /* Symbol Access Methods */
        static constexpr int DDVAL_MASK = 1;
        static RObject *printname(RObject *x);
        static RObject *symvalue(RObject *x);
        static RObject *internal(RObject *x);
        static unsigned int ddval(RObject *x); /* for ..1, ..2 etc */
        static void set_ddval_bit(RObject *x);
        static void unset_ddval_bit(RObject *x);
        static void set_ddval(RObject *x, bool v); /* for ..1, ..2 etc */
        static void set_printname(RObject *x, RObject *v);
        static void set_symvalue(RObject *x, RObject *v);
        static void set_internal(RObject *x, RObject *v);
        /* Environment Access Methods */
        static constexpr int FRAME_LOCK_MASK = (1 << 14);
        static constexpr int GLOBAL_FRAME_MASK = (1 << 15);
        static RObject *frame(RObject *x);
        static RObject *enclos(RObject *x);
        static RObject *hashtab(RObject *x);
        static unsigned int envflags(RObject *x); /* for environments */
        static void set_envflags(RObject *x, unsigned int v);
        static void set_frame(RObject *x, RObject *v);
        static void set_enclos(RObject *x, RObject *v);
        static void set_hashtab(RObject *x, RObject *v);
        static unsigned int frame_is_locked(RObject *x);
        static unsigned int is_global_frame(RObject *x);
        /* Promise Access Methods */
        static RObject *prcode(RObject *x);
        static void set_prcode(RObject *x, RObject *v);
        static RObject *prenv(RObject *x);
        static RObject *prvalue(RObject *x);
        static void set_prvalue(RObject *x, RObject *v);
        static unsigned int prseen(RObject *x);
        static void set_prenv(RObject *x, RObject *v);
        static void set_prseen(RObject *x, unsigned int v);
        /* List Access Methods */
        static RObject *tag(RObject *x);
        static void set_tag(RObject *x, RObject *v);
        static RObject *car0(RObject *x);
        static void set_car0(RObject *x, RObject *v);
        static RObject *extptr_ptr(RObject *x);
        static void set_extptr_ptr(RObject *x, RObject *v);
        static RObject *cdr(RObject *x);
        static void set_cdr(RObject *x, RObject *v);
        static constexpr int MISSING_MASK = ((1 << 4) - 1); // = 15 /* reserve 4 bits--only 2 uses now */
        static unsigned int missing(RObject *x);                /* for closure calls */
        static void set_missing(RObject *x, int v);
        static unsigned int bndcell_tag(RObject *x);
        static void set_bndcell_tag(RObject *e, unsigned int v);
        /* External pointer access methods */
        static RObject *extptr_prot(RObject *x);
        static RObject *extptr_tag(RObject *x);
        static void set_extptr_tag(RObject *x, RObject *v);
        static void set_extptr_prot(RObject *x, RObject *v);
        /* S4 object bit, set by R_do_new_object for all new() calls */
        static constexpr int S4_OBJECT_MASK = ((unsigned short)(1 << 4));
        static bool is_s4_object(RObject *x);
        static void set_s4_object(RObject *x);
        static void unset_s4_object(RObject *x);
        /* JIT optimization support */
        enum JITBit
        {
            NOJIT_MASK = (1 << 5),
            MAYBEJIT_MASK = (1 << 6)
        };
        static unsigned int nojit(RObject *x);
        static void set_nojit(RObject *x);
        static unsigned int maybejit(RObject *x);
        static void set_maybejit(RObject *x);
        static void unset_maybejit(RObject *x);
        /* Growable vector support */
        static constexpr int GROWABLE_MASK = ((unsigned short)(1 << 5));
        static unsigned int growable_bit_set(RObject *x);
        static void set_growable_bit(RObject *x);
        /* Hashing Methods */
        static constexpr int HASHASH_MASK = 1;
        static unsigned int hashash(RObject *x);
        static void set_hashash(RObject *x, bool v);

        static constexpr int SPECIAL_SYMBOL_MASK = (1 << 12);
        static constexpr int BASE_SYM_CACHED_MASK = (1 << 13);
        static void set_base_sym_cached(RObject *x);
        static void unset_base_sym_cached(RObject *x);
        static unsigned int base_sym_cached(RObject *x);
        static unsigned int no_special_symbols(RObject *x);
        static void set_no_special_symbols(RObject *x);
        static unsigned int is_special_symbol(RObject *x);
        static void set_special_symbol(RObject *x);
        static void unset_no_special_symbols(RObject *x);
        static void unset_special_symbol(RObject *x);
        static constexpr int ACTIVE_BINDING_MASK = (1 << 15);
        static constexpr int BINDING_LOCK_MASK = (1 << 14);
        static constexpr int SPECIAL_BINDING_MASK = (ACTIVE_BINDING_MASK | BINDING_LOCK_MASK);
        static unsigned int is_active_binding(RObject *x);
        static unsigned int binding_is_locked(RObject *x);
        static void lock_binding(RObject *x);
        static void unlock_binding(RObject *x);
        static void set_active_binding_bit(RObject *x);
        static double bndcell_dval(RObject *x);
        static int bndcell_ival(RObject *x);
        static int bndcell_lval(RObject *x);
        static void set_bndcell_dval(RObject *x, double v);
        static void set_bndcell_ival(RObject *x, int v);
        static void set_bndcell_lval(RObject *x, int v);

        static inline R_xlen_t stdvec_length(RObject *x) { return x ? x->u.vecsxp.m_length : 0; }
        static inline R_xlen_t stdvec_truelength(RObject *x) { return x ? x->u.vecsxp.m_truelength : 0; }
        static inline void set_stdvec_truelength(RObject *x, R_xlen_t v)
        {
            if (!x)
                return;
            x->u.vecsxp.m_truelength = v;
        }
        static inline void set_stdvec_length(RObject *x, R_xlen_t v)
        {
            if (!x)
                return;
            x->u.vecsxp.m_length = v;
            RObject::setscalar(x, v == 1);
        }
        static inline void set_truelength(RObject *x, R_xlen_t v)
        {
            if (R::RObject::altrep(x))
                Rf_error("can't set ALTREP truelength");
            R::RObject::set_stdvec_truelength(x, v);
        }
    };
#if 0
    class Symbol : public RObject
    {
    private:
        RObject *pname;
        RObject *value;
        RObject *internal;

    public:
        RObject *printname() const
        {
            return this->pname;
        }
        RObject *symvalue() const
        {
            return this->value;
        }
        RObject *internal() const
        {
            return this->internal;
        }
    };

    class Environment : public RObject
    {
    private:
        RObject *frame;
        RObject *enclos;
        RObject *hashtab;

    public:
    };

    class BuiltInFunction : public RObject
    {
    private:
        int offset;

    public:
    };

    class Closure : public RObject
    {
    private:
        RObject *formals;
        RObject *body;
        RObject *env;

    public:
    };

    class Promise : public RObject
    {
    private:
        RObject *value;
        RObject *expr;
        RObject *env;

    public:
    };

    class RList : public RObject
    {
    private:
        RObject *carval;
        RObject *cdrval;
        RObject *tagval;

    public:
        RObject *tag() const
        {
            return this->tagval;
        }
        RObject *car() const
        {
            return this->carval;
        }
        RObject *cdr() const
        {
            return this->cdrval;
        }
    };
#endif


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
        R::RObject *drc__x__ = (x);                               \
        if (REFCNT(drc__x__) > 0 && REFCNT(drc__x__) < REFCNTMAX) \
            SET_REFCNT(drc__x__, REFCNT(drc__x__) - 1);           \
    } while (0)
#define INCREMENT_REFCNT(x)                             \
    do                                                  \
    {                                                   \
        R::RObject *irc__x__ = (x);                     \
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
        R::RObject *__enm_v__ = (v);        \
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
        RObject *__x__ = (x);    \
        if (NAMED(__x__) == 1)   \
            SET_NAMED(__x__, 0); \
    } while (0)
#define RAISE_NAMED(x, n)            \
    do                               \
    {                                \
        RObject *__x__ = (x);        \
        int __n__ = (n);             \
        if (NAMED(__x__) < __n__)    \
            SET_NAMED(__x__, __n__); \
    } while (0)
#endif

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

#define INIT_BNDCELL(cell, type)             \
    do                                       \
    {                                        \
        RObject *val = allocVector(type, 1); \
        SETCAR(cell, val);                   \
        INCREMENT_NAMED(val);                \
        SET_BNDCELL_TAG(cell, type);         \
        SET_MISSING(cell, 0);                \
    } while (0)
#else
/* Use a union in the CAR field to represent an RObject* or an immediate
   value.  More efficient, but changes the memory layout on 32 bit
   platforms since the size of the union is larger than the size of a
   pointer. The layout should not change on 64 bit platforms. */
union R_bndval_t
{
    R::RObject *sxpval;
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

/* There is much more in Rinternals.h, including function versions
 * of the Promise and Hashing groups.
 */

/* Vector Heap Structure */
struct VECREC
{
    union
    {
        R::RObject *backpointer;
        double align;
    } u;
};

using VECP = VECREC *;

/* Vector Heap Macros */
// #define BACKPOINTER(v) ((v).u.backpointer)
inline size_t BYTE2VEC(int n) { return (n > 0) ? (std::size_t(n) - 1) / sizeof(VECREC) + 1 : 0; }
inline size_t INT2VEC(int n) { return (n > 0) ? (std::size_t(n) * sizeof(int) - 1) / sizeof(VECREC) + 1 : 0; }
inline size_t FLOAT2VEC(int n) { return (n > 0) ? (std::size_t(n) * sizeof(double) - 1) / sizeof(VECREC) + 1 : 0; }
inline size_t COMPLEX2VEC(int n) { return (n > 0) ? (std::size_t(n) * sizeof(Rcomplex) - 1) / sizeof(VECREC) + 1 : 0; }
inline size_t PTR2VEC(int n) { return (n > 0) ? (std::size_t(n) * sizeof(RObject) - 1) / sizeof(VECREC) + 1 : 0; }

} // namespace R

#endif /* ROBJECT_HPP */
