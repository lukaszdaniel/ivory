/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999--2020  The R Core Team.
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
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

/** @file RObject.hpp
 *
 * @brief Class CXXR::RObject and associated C interface functions.
 */

#ifndef ROBJECT_HPP
#define ROBJECT_HPP

#define SWITCH_TO_REFCNT
#define COMPUTE_REFCNT_VALUES

#include <cstddef>
#include <cstring>
#include <CXXR/SEXPTYPE.hpp>
#include <CXXR/RTypes.hpp>
#include <CXXR/GCNode.hpp>
#include <R_ext/Error.h>
#include <R_ext/Complex.h>

#if defined(COMPUTE_REFCNT_VALUES)
#define REFCNT(x) (CXXR::RObject::refcnt(x))
#define TRACKREFS(x) (CXXR::RObject::trackrefs(x))
#else
#define REFCNT(x) 0
#define TRACKREFS(x) false
#endif

#if defined(COMPUTE_REFCNT_VALUES)
#define SET_REFCNT(x, v) (CXXR::RObject::set_refcnt(x, v))
#if defined(EXTRA_REFCNT_FIELDS)
#define SET_TRACKREFS(x, v) (CXXR::RObject::set_trackrefs(x, v))
#else
#define SET_TRACKREFS(x, v) (CXXR::RObject::set_trackrefs(x, !v))
#endif
#define DECREMENT_REFCNT(x)                                       \
    do                                                            \
    {                                                             \
        CXXR::RObject *drc__x__ = (x);                            \
        if (REFCNT(drc__x__) > 0 && REFCNT(drc__x__) < REFCNTMAX) \
            SET_REFCNT(drc__x__, REFCNT(drc__x__) - 1);           \
    } while (0)
#define INCREMENT_REFCNT(x)                             \
    do                                                  \
    {                                                   \
        CXXR::RObject *irc__x__ = (x);                  \
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
        CXXR::RObject *__enm_v__ = (v);     \
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
    CXXR::RObject *sxpval;
    double dval;
    int ival;
};

#define BNDCELL_DVAL(v) (CXXR::RObject::bndcell_dval(v))
#define BNDCELL_IVAL(v) (CXXR::RObject::bndcell_ival(v))
#define BNDCELL_LVAL(v) (CXXR::RObject::bndcell_lval(v))

#define SET_BNDCELL_DVAL(cell, dval_) (CXXR::RObject::set_bndcell_dval(cell, dval_))
#define SET_BNDCELL_IVAL(cell, ival_) (CXXR::RObject::set_bndcell_ival(cell, ival_))
#define SET_BNDCELL_LVAL(cell, lval_) (CXXR::RObject::set_bndcell_lval(cell, lval_))

#define INIT_BNDCELL(cell, type)      \
    do                                \
    {                                 \
        if (BNDCELL_TAG(cell) == 0)   \
            SETCAR(cell, R_NilValue); \
        SET_BNDCELL_TAG(cell, type);  \
        SET_MISSING(cell, 0);         \
    } while (0)
#endif

/* This is intended for use only within R itself.
 * It defines internal structures that are otherwise only accessible
 * via RObject*, and macros to replace many (but not all) of accessor functions
 * (which are always defined).
 */

constexpr int NAMED_BITS = 16;

/** @brief Namespace for the CXXR project.
 *
 * CXXR is a project to refactorize the R interpreter into C++.
 */
namespace CXXR
{
    /** @brief Replacement for CR's SEXPREC.
     *
     */
    class RObject;

    struct primsxp_struct
    {
        int m_offset;
    };

    struct symsxp_struct
    {
        RObject *m_pname;
        RObject *m_value;
        RObject *m_internal;
    };

    struct listsxp_struct
    {
        RObject *m_carval;
        RObject *m_cdrval;
        RObject *m_tagval;
    };

    struct envsxp_struct
    {
        RObject *m_frame;
        RObject *m_enclos;
        RObject *m_hashtab;
    };

    struct closxp_struct
    {
        RObject *m_formals;
        RObject *m_body;
        RObject *m_env;
    };

    struct promsxp_struct
    {
        RObject *m_value;
        RObject *m_expr;
        RObject *m_env;
    };

    struct vecsxp_struct
    {
        R_xlen_t m_length;
        R_xlen_t m_truelength;
    };

    /* Every node must start with a set of sxpinfo flags and an attribute
   field. Under the generational collector these are followed by the
   fields used to maintain the collector's linked list structures. */
} // namespace CXXR
#ifdef SWITCH_TO_REFCNT
constexpr int REFCNTMAX = ((1 << NAMED_BITS) - 1);
#endif
namespace CXXR
{

    /** @brief Replacement for CR's SEXPREC.
     *
     * This class is the rough equivalent within CXXR of the SEXPREC
     * union within CR.  However, all functionality relating to
     * garbage collection has been factored out into the base class
     * GCNode, and as CXXR development proceeds other functionality
     * will be factored out into derived classes (corresponding
     * roughly, but not exactly, to different SEXPTYPEs within CR).
     *
     * Eventually this class may end up simply as the home of R
     * attributes.
     *
     * @note The word 'object' in the name of this class is used in
     * the sense in which the 'blue book' (Becker <em>et al.</em>
     * [1988]) uses the phrase 'data object'.  Roughly speaking,
     * CXXR::RObject is a base class for the sorts of data items whose
     * existence would be reported by the R function
     * <tt>objects()</tt>.  In particular, it does not imply that
     * the object belongs to an R class.
     */
    class RObject : public GCNode
    {
    public: // private:
        SEXPTYPE m_type : FULL_TYPE_BITS;
        bool m_scalar;
        bool m_has_class;
        bool m_alt;
        unsigned int m_gpbits : 16;
        bool m_debug;
        bool m_trace; /* functions and memory tracing */
        bool m_spare; /* used on closures and when REFCNT is defined */
        unsigned int m_named : NAMED_BITS;
        unsigned int m_extra : 29 - NAMED_BITS; /* used for immediate bindings */
        RObject *m_attrib;

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
        void *m_data;
        size_t m_databytes;
        bool m_allocator;

        /**
         * @param stype Required type of the RObject.
         */
        explicit RObject(SEXPTYPE stype = ANYSXP) : m_type(stype), m_scalar(false), m_has_class(false), m_alt(false), m_gpbits(0), m_debug(false),
                                                    m_trace(false), m_spare(false), m_named(0), m_extra(0), m_attrib(nullptr), m_data(nullptr), m_databytes(0), m_allocator(false)
        {
#ifdef COMPUTE_REFCNT_VALUES
            SET_REFCNT(this, 0);
            SET_TRACKREFS(this, true);
#endif
        }

        /** @brief Copy constructor.
         *
         * @param pattern Object to be copied.
         */
        RObject(const RObject &pattern);

        /**
         * @return Pointer to the attributes of this object.
         */
        const RObject *attributes() const { return m_attrib; }

        // Virtual methods of GCNode:
        void visitChildren(const_visitor *v) const;
        void visitChildren(visitor *v);

        /**
         * @return pointer to first element (car) of this list.
         */
        const RObject *car() const { return u.listsxp.m_carval; }

        /**
         * @return pointer to tail (cdr) of this list.
         */
        const RObject *cdr() const { return u.listsxp.m_cdrval; }

        /**
         * @return pointer to enclosing environment.
         */
        const RObject *enclosingEnvironment() const { return u.envsxp.m_enclos; }

        /**
         * @return pointer to frame of this environment.
         */
        const RObject *frame() const { return u.envsxp.m_frame; }

        /**
         * @return pointer to hash table of this environment.
         */
        const RObject *hashTable() const { return u.envsxp.m_hashtab; }

        /**
         * @return length of this vector.
         */
        R_xlen_t length() const { return u.vecsxp.m_length; }

        /**
         * @return SEXPTYPE of this object.
         */
        SEXPTYPE sexptype() const { return m_type; }

        /**
         * @return altrep status of this object.
         */
        bool altrep() const { return m_alt; }

        /**
         * @return pointer to tag of this list.
         */
        const RObject *tag() const { return u.listsxp.m_tagval; }

        /** @brief Set the status of this RObject as an S4 object.
         *
         * @param on true iff this is to be considered an S4 object.
         *          CXXR raises an error if an attempt is made to
         *          unset the S4 object status of an S4Object
         *          (::S4SXP), whereas CR permits this.
         */
        void setS4Object(bool on);

        /** @brief Name within R of this type of object.
         *
         * @return the name by which this type of object is known
         *         within R.
         */
        virtual const char *typeName() const;

        /** @brief Return pointer to a copy of this object.
         *
         * This function creates a copy of this object, and returns a
         * pointer to that copy.
         *
         * Generally this function (and the copy constructors it
         * utilises) will attempt to create a 'deep' copy of the
         * object; this follows standard practice within C++, and it
         * is intended to extend this practice as CXXR development
         * continues.
         *
         * However, if the pattern object contains unclonable
         * subobjects, then the created copy will at the relevant
         * places simply contain pointers to those subobjects, i.e. to
         * that extent the copy is 'shallow'.  This is managed using
         * the smart pointers defined by nested class RObject::Handle.
         *
         * @return a pointer to a clone of this object.  Returns the original
         *          object if it cannot be cloned.
         *
         * @note Derived classes should exploit the covariant return
         *          type facility to return a pointer to the type of object
         *          being cloned.
         */
        virtual RObject *clone() const
        {
            return const_cast<RObject *>(this);
        }

        /** @brief Return a pointer to a copy of an object or the object itself
         *          if it isn't cloneable.
         *
         * @tparam T RObject or a type derived from RObject.
         *
         * @param pattern Either a null pointer or a pointer to the
         *          object to be cloned.
         *
         * @return Pointer to a clone of \a pattern, or \a pattern
         * if \a pattern cannot be cloned or is itself a null pointer.
         */
        template <class T>
        static T *clone(const T *pattern)
        {
            return pattern ? pattern->clone() : nullptr;
        }

        // To be protected in future:

        /** Destructor
         *
         * @note The destructor is protected to ensure that RObjects
         * are allocated on the heap.  (See Meyers 'More Effective
         * C++' Item 27.) Derived classes should likewise declare
         * their constructors private or protected.
         */
        virtual ~RObject();

        static void set_attrib(RObject *x, RObject *v);
        static RObject *attrib(RObject *x);
        static unsigned int named(RObject *x);
        static void set_named(RObject *x, unsigned int v);
        static void set_typeof(RObject *x, SEXPTYPE v);
        static SEXPTYPE typeof_(RObject *x);
        static unsigned int levels(RObject *x);
        static bool object(RObject *x);
        static void set_object(RObject *x, bool v);
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

        /* List Access Methods */
        static RObject *tag(RObject *x);
        static void set_tag(RObject *x, RObject *v);
        static RObject *car0(RObject *x);
        static void set_car0(RObject *x, RObject *v);
        static RObject *cdr(RObject *x);
        static void set_cdr(RObject *x, RObject *v);
        static constexpr int MISSING_MASK = ((1 << 4) - 1); // = 15 /* reserve 4 bits--only 2 uses now */
        static unsigned int missing(RObject *x);            /* for closure calls */
        static void set_missing(RObject *x, int v);
        static unsigned int bndcell_tag(const RObject *x);
        static void set_bndcell_tag(RObject *e, unsigned int v);
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

        /** @brief The name by which this type is known in R.
         *
         * @return The name by which this type is known in R.
         */
        static const char *staticTypeName()
        {
            return "RObject";
        }
    };

    /* There is much more in Rinternals.h, including function versions
     * of the Promise and Hashing groups.
     */

    /* Vector Heap Structure */
    // struct alignas(std::max(alignof(double), alignof(RObject*))) VECREC {};
    struct VECREC
    {
        union
        {
            CXXR::RObject *backpointer;
            double align;
        } u;
    };

    using VECP = VECREC *;

    /* Vector Heap Macros */
    inline size_t BYTE2VEC(size_t n) { return (n > 0) ? (n - 1) / sizeof(VECREC) + 1 : 0; }
    inline size_t INT2VEC(size_t n) { return (n > 0) ? (n * sizeof(int) - 1) / sizeof(VECREC) + 1 : 0; }
    inline size_t FLOAT2VEC(size_t n) { return (n > 0) ? (n * sizeof(double) - 1) / sizeof(VECREC) + 1 : 0; }
    inline size_t COMPLEX2VEC(size_t n) { return (n > 0) ? (n * sizeof(Rcomplex) - 1) / sizeof(VECREC) + 1 : 0; }
    inline size_t PTR2VEC(size_t n) { return (n > 0) ? (n * sizeof(RObject) - 1) / sizeof(VECREC) + 1 : 0; }

} // namespace CXXR

#endif /* ROBJECT_HPP */
