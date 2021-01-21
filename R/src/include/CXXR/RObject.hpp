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

#define R_NO_REMAP
#define SWITCH_TO_REFCNT
#define COMPUTE_REFCNT_VALUES
// #define ENABLE_ST_CHECKS
#define CXXR_OLD_ALTREP_IMPL

#define TEST_PL(pp)                                                                                                                   \
    {                                                                                                                                 \
        ConsCell *p = SEXP_downcast<ConsCell *>(pp);                                                                                  \
        do                                                                                                                            \
        {                                                                                                                             \
            if (p && !(p->sexptype() == LISTSXP || p->sexptype() == LANGSXP || p->sexptype() == DOTSXP || p->sexptype() == BCODESXP)) \
            {                                                                                                                         \
                std::cerr << LOCATION << Rf_type2char(p->sexptype()) << "; " << R::typeName(p) << std::endl;                          \
                std::abort();                                                                                                         \
            }                                                                                                                         \
            if (p)                                                                                                                    \
                p = p->tail();                                                                                                        \
        } while (p);                                                                                                                  \
    }

#define PRINT_PL(call)                                                                                                                      \
    if (call && (call->sexptype() == LISTSXP || call->sexptype() == DOTSXP || call->sexptype() == LANGSXP || call->sexptype() == BCODESXP)) \
    {                                                                                                                                       \
        std::cerr << LOCATION << "Begin ccdump for " << Rf_type2char(call->sexptype()) << " ...\n";                                         \
        ccdump(std::cerr, SEXP_downcast<const CXXR::PairList *>(call), 0);                                                                  \
        std::cerr << LOCATION << "Done ccdump ...\n\n";                                                                                     \
    }                                                                                                                                       \
    else if (call)                                                                                                                          \
    {                                                                                                                                       \
        std::cerr << LOCATION << "Not a pairlist but " << Rf_type2char(call->sexptype()) << std::endl;                                      \
    }                                                                                                                                       \
    else                                                                                                                                    \
    {                                                                                                                                       \
        std::cerr << LOCATION << "object = nullptr" << std::endl;                                                                           \
    }

#include <cstddef>
#include <cstring>
#include <CXXR/SEXPTYPE.hpp>
#include <CXXR/RTypes.hpp>
#include <CXXR/GCNode.hpp>
#define CALLED_FROM_DEFN_H
#include <Rinternals.h>
#undef CALLED_FROM_DEFN_H
#include <R_ext/Error.h>
#include <R_ext/Complex.h>
#include <R_ext/Boolean.h>

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

/*
Triplet's translation table:
---- LIST ----- ENV ---------- CLO ---------- PROM --------- SYM
     (SET)CAR   (SET_)FRAME    (SET_)FORMALS  (SET_)PRVALUE  (SET_)PRINTNAME
     (SET)CDR   (SET_)ENCLOS   (SET_)BODY     (SET_)PRCODE   (SET_)SYMVALUE
     (SET_)TAG  (SET_)HASHTAB  (SET_)CLOENV   (SET_)PRENV    (SET_)INTERNAL
*/

#ifdef SWITCH_TO_REFCNT
constexpr int REFCNTMAX = ((1 << NAMED_BITS) - 1);
#endif
namespace CXXR
{
    class PairList;
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
    private:
        SEXPTYPE m_type : FULL_TYPE_BITS;
        bool m_scalar;
        bool m_has_class;
        bool m_alt;

    public:
        unsigned int m_gpbits : 16;
        bool m_debug;
        bool m_trace; /* functions and memory tracing */
        bool m_spare; /* used on closures and when REFCNT is defined */
    private:
        unsigned int m_named : NAMED_BITS;
        unsigned int m_extra : 29 - NAMED_BITS; /* used for immediate bindings */
        PairList *m_attrib;

    public:
        /**
         * @param stype Required type of the RObject.
         */
        explicit RObject(SEXPTYPE stype = CXXSXP) : m_type(stype), m_scalar(false), m_has_class(false), m_alt(false), m_gpbits(0), m_debug(false),
                                                    m_trace(false), m_spare(false), m_named(0), m_extra(0), m_attrib(nullptr)
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

        /** @brief Get object attributes.
         *
         * @return Pointer to the attributes of this object.
         */
        PairList *attributes() { return m_attrib; }

        /** @brief Get object attributes (const variant).
         *
         * @return Pointer to the attributes of this object.
         */
        const PairList *attributes() const { return m_attrib; }

        /** @brief Replace the attributes of an object.
         *
         * @param new_attributes Pointer to the start of the new list
         *          of attributes.  May be a null pointer, in which
         *          case all attributes are removed.
         */
        void setAttributes(PairList *new_attributes);

        // Virtual function of GCNode:
        void visitChildren(const_visitor *v) const override;

        /** @brief Get an object's ::SEXPTYPE.
         *
         * @return ::SEXPTYPE of this object.
         */
        SEXPTYPE sexptype() const { return m_type; }

        /**
         * @return altrep status of this object.
         */
        bool altrep() const
        {
            return m_alt;
        }

        void setAltrep(bool on)
        {
            m_alt = on;
        }

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
         *         if \a pattern cannot be cloned or is itself a null pointer.
         */
        template <class T>
        static T *clone(const T *pattern)
        {
            return pattern ? pattern->clone() : nullptr;
        }

        // To be protected in future:

        virtual ~RObject();

        static void set_attrib(RObject *x, RObject *v);
        static RObject *attrib(RObject *x);
        static unsigned int named(RObject *x);
        static void set_named(RObject *x, unsigned int v);
        static void set_typeof(RObject *x, SEXPTYPE v);
        static SEXPTYPE typeof_(const RObject *x);
        static unsigned int levels(RObject *x);
        static bool object(RObject *x);
        static void set_object(RObject *x, bool v);

        static bool altrep(RObject *x);
        static void set_altrep(RObject *x, bool v);
        static void setlevels(RObject *x, unsigned short int v);

        static bool scalar(RObject *x);
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

        static constexpr int ACTIVE_BINDING_MASK = (1 << 15);
        static constexpr int BINDING_LOCK_MASK = (1 << 14);
        static constexpr int SPECIAL_BINDING_MASK = (ACTIVE_BINDING_MASK | BINDING_LOCK_MASK);
        static unsigned int is_active_binding(RObject *x);
        static unsigned int binding_is_locked(RObject *x);
        static void lock_binding(RObject *x);
        static void unlock_binding(RObject *x);
        static void set_active_binding_bit(RObject *x);

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

    /* Vector Heap Macros */
    // inline size_t BYTE2VEC(size_t n) { return (n > 0) ? (n - 1) / sizeof(VECREC) + 1 : 0; }
    // inline size_t INT2VEC(size_t n) { return (n > 0) ? (n * sizeof(int) - 1) / sizeof(VECREC) + 1 : 0; }
    // inline size_t FLOAT2VEC(size_t n) { return (n > 0) ? (n * sizeof(double) - 1) / sizeof(VECREC) + 1 : 0; }
    // inline size_t COMPLEX2VEC(size_t n) { return (n > 0) ? (n * sizeof(Rcomplex) - 1) / sizeof(VECREC) + 1 : 0; }
    // inline size_t PTR2VEC(size_t n) { return (n > 0) ? (n * sizeof(RObject) - 1) / sizeof(VECREC) + 1 : 0; }
    template <typename T>
    inline size_t convert2VEC(size_t n) { return (n > 0) ? (n * sizeof(T) - 1) / sizeof(VECREC) + 1 : 0; }

} // namespace CXXR

extern "C"
{
    /** @brief Get object's ::SEXPTYPE.
     *
     * @param x Pointer to CXXR::RObject.
     * @return ::SEXPTYPE of \a x, or NILSXP if x is a null pointer.
     */
    SEXPTYPE TYPEOF(SEXP x);

    /** @brief Name of type within R.
     *
     * Translate a ::SEXPTYPE to the name by which it is known within R.
     * @param st The ::SEXPTYPE whose name is required.
     * @return The ::SEXPTYPE's name within R.
     */
    const char *Rf_type2char(SEXPTYPE st);

    /** @brief Copy attributes, with some exceptions.
     *
     * This is called in the case of binary operations to copy most
     * attributes from one of the input arguments to the output.
     * Note that the Dim, Dimnames and Names attributes are not
     * copied: these should have been assigned elsewhere.  The
     * function also copies the S4 object status.
     * @param inp Pointer to the CXXR::RObject from which attributes are to
     *          be copied.
     * @param ans Pointer to the CXXR::RObject to which attributes are to be
     *          copied.
     * @note The above documentation is probably incomplete: refer to the
     *       source code for further details.
     */
    void Rf_copyMostAttrib(SEXP inp, SEXP ans);

    /** @brief Access a named attribute.
     * @param vec Pointer to the CXXR::RObject whose attributes are to be
     *          accessed.
     * @param name Either a pointer to the symbol representing the
     *          required attribute, or a pointer to a CXXR::StringVector
     *          containing the required symbol name as element 0; in
     *          the latter case, as a side effect, the corresponding
     *          symbol is installed if necessary.
     * @return Pointer to the requested attribute, or a null pointer
     *         if there is no such attribute.
     * @note The above documentation is incomplete: refer to the
     *       source code for further details.
     */
    SEXP Rf_getAttrib(SEXP vec, SEXP name);

    /** @brief Set or remove a named attribute.
     * @param vec Pointer to the CXXR::RObject whose attributes are to be
     *          modified.
     * @param name Either a pointer to the symbol representing the
     *          required attribute, or a pointer to a CXXR::StringVector
     *          containing the required symbol name as element 0; in
     *          the latter case, as a side effect, the corresponding
     *          symbol is installed if necessary.
     * @param val Either the value to which the attribute is to be
     *          set, or a null pointer.  In the latter case the
     *          attribute (if present) is removed.
     * @return Refer to source code.  (Sometimes \a vec, sometimes \a
     *         val, sometime a null pointer ...)
     * @note The above documentation is probably incomplete: refer to the
     *       source code for further details.
     */
    SEXP Rf_setAttrib(SEXP vec, SEXP name, SEXP val);

    /** @brief Does an object have a class attribute?
     *
     * @param x Pointer to a CXXR::RObject.
     * @return true iff \a x has a class attribute.  Returns false if \a x
     *         is 0.
     */
    int OBJECT(SEXP x);

    /**
     * @param s Pointer to a CXXR::RObject.
     * @return TRUE iff the CXXR::RObject pointed to by \a s is either a null
     *         pointer (i.e. <tt>== R_NilValue</tt> in CXXR), or is a CXXR::RObject
     *         with SEXPTYPE NILSXP (should not happen in CXXR).
     */
    Rboolean Rf_isNull(SEXP s);

    /** @brief Does an object have a class attribute?
     *
     * @param s Pointer to a CXXR::RObject.
     * @return TRUE iff the CXXR::RObject pointed to by \a s has a class attribute.
     */
    Rboolean Rf_isObject(SEXP s);

    /** @brief Get the attributes of a CXXR::RObject.
     *
     * @param x Pointer to the CXXR::RObject whose attributes are required.
     * @return Pointer to the attributes object of \a x , or 0 if \a x is
     *         a null pointer.
     */
    SEXP ATTRIB(SEXP x);

    /**
     * @deprecated
     */
    int LEVELS(SEXP x);

    /** @brief Get object copying status.
     *
     * @param x Pointer to CXXR::RObject.
     * @return Refer to 'R Internals' document.  Returns 0 if \a x is a
     *         null pointer.
     */
    int(NAMED)(SEXP x);

    /** @brief Get object tracing status.
     *
     * @param x Pointer to CXXR::RObject.
     * @return Refer to 'R Internals' document.  Returns 0 if \a x is a
     *         null pointer.
     */
    int TRACE(SEXP x);

    /**
     * @deprecated
     */
    void SETLEVELS(SEXP x, int v);

    /** @brief Replace an object's attributes.
     *
     * @param x Pointer to a CXXR::RObject.
     * @param v Pointer to the new attributes CXXR::RObject.
     * @todo Could \a v be \c const ?
     */
    void SET_ATTRIB(SEXP x, SEXP v);

    /** @brief Set object copying status.
     *
     * @param x Pointer to CXXR::RObject.  The function does nothing
     *          if \a x is a null pointer.
     * @param v Refer to 'R Internals' document.
     * @deprecated Ought to be private.
     */
    void SET_NAMED(SEXP x, int v);

    /**
     * @deprecated Ought to be private.
     */
    void SET_OBJECT(SEXP x, int v);

    void SET_TRACE(SEXP x, int v);

    /**
     * @deprecated Ought to be private.
     */
    void SET_TYPEOF(SEXP x, SEXPTYPE v);

    /** @brief Replace the attributes of \a to by those of \a from.
     *
     * @param to Pointer to CXXR::RObject.
     * @param from Pointer to another CXXR::RObject.
     */
    void DUPLICATE_ATTRIB(SEXP to, SEXP from);
} // extern "C"

#endif /* ROBJECT_HPP */
