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
// #define ENABLE_ST_CHECKS
#define CXXR_OLD_ALTREP_IMPL

#include <cstddef>
#include <cstring>
#include <bitset>
#include <CXXR/SEXPTYPE.hpp>
#include <CXXR/RTypes.hpp>
#include <CXXR/GCNode.hpp>
#include <R_ext/Error.h>
#include <R_ext/Complex.h>
#include <R_ext/Boolean.h>

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
#define MARK_ASSIGNMENT_CALL_MACRO(call) SET_ASSIGNMENT_PENDING(call, TRUE)
#define IS_ASSIGNMENT_CALL_MACRO(call) ASSIGNMENT_PENDING(call)

#ifdef SWITCH_TO_REFCNT
/* no definition for SET_NAMED; any calls will use the one in memory.cpp */
#define ENSURE_NAMEDMAX_MACRO(v) \
    do                           \
    {                            \
    } while (0)
#define ENSURE_NAMED_MACRO(v) \
    do                        \
    {                         \
    } while (0)
#else
#define ENSURE_NAMEDMAX_MACRO(v)            \
    do                                      \
    {                                       \
        CXXR::RObject *__enm_v__ = (v);     \
        if (NAMED(__enm_v__) < NAMEDMAX)    \
            SET_NAMED(__enm_v__, NAMEDMAX); \
    } while (0)
#define ENSURE_NAMED_MACRO(v) \
    do                        \
    {                         \
        if (NAMED(v) == 0)    \
            SET_NAMED(v, 1);  \
    } while (0)
#endif

#ifdef SWITCH_TO_REFCNT
#define SETTER_CLEAR_NAMED_MACRO(x) \
    do                              \
    {                               \
    } while (0)
#define RAISE_NAMED_MACRO(x, n) \
    do                          \
    {                           \
    } while (0)
#else
#define SETTER_CLEAR_NAMED_MACRO(x) \
    do                              \
    {                               \
        RObject *__x__ = (x);       \
        if (NAMED(__x__) == 1)      \
            SET_NAMED(__x__, 0);    \
    } while (0)
#define RAISE_NAMED_MACRO(x, n)      \
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

// Remove this once cloning is fully rolled out:
namespace CXXR
{
    class RObject;
}

CXXR::RObject *duplicate1(CXXR::RObject *s, bool deep);
CXXR::RObject *duplicate_child(CXXR::RObject *s, bool deep);

namespace CXXR
{
    class PairList;
    class Symbol;
    /** @brief Replacement for CR's SEXPREC.
     *
     * This class is the rough equivalent within CXXR of the SEXPREC
     * union within CR.  However, all functionality relating to
     * garbage collection has been factored out into the base class
     * GCNode, and as CXXR development proceeds other functionality
     * will be factored out into derived classes (corresponding
     * roughly, but not exactly, to different SEXPTYPEs within CR), or
     * outside the RObject hierarchy altogether.
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
     *
     * @invariant The class currently aims to enforce the following
     * invariants in regard to each RObject:
     * <ul>
     *
     * <li><tt>m_has_class</tt> is true iff the object has the class
     * attribute.</li>
     *
     * <li>Each attribute in the list of attributes must have a Symbol
     * as its tag.  Null tags are not allowed.</li>
     *
     * <li>Each attribute must have a distinct tag: no duplicates
     * allowed.</li>
     *
     * <li>No attribute may have a null value: an attempt to set the
     * value of an attribute to null will result in the removal of the
     * attribute altogether.
     *
     * </ul>
     * The CR code in attrib.cpp applies further consistency
     * conditions on attributes, but these are not yet enforced via
     * the class interface.
     *
     * @todo Incorporate further attribute consistency checks within
     * the class interface.  Possibly make setAttribute() virtual so
     * that these consistency checks can be tailored according to the
     * derived class.
     *
     * @todo Possibly key attributes on (cached) strings rather than
     * Symbol objects.
     */
    class RObject : public GCNode
    {
    private:
        SEXPTYPE m_type : FULL_TYPE_BITS;
        bool m_scalar;
        bool m_has_class;
        bool m_alt;
        // unsigned int m_gpbits : 16;
        bool m_trace; /* functions and memory tracing */
        bool m_spare; /* used on closures and when REFCNT is defined */
        unsigned int m_named : NAMED_BITS;
        unsigned int m_extra : 29 - NAMED_BITS; /* used for immediate bindings */
        bool m_s4_object;

        // The following obsolescent fields squeezed
        // in here are used only in connection with objects of class
        // PairList (and only rarely then), so they would more
        // logically be placed in that class (and formerly were within
        // rho).

        // Used when the contents of an Environment are represented as
        // a PairList, for example during serialization and
        // deserialization, and formerly hosted in the gp field of
        // sxpinfo_struct.
        bool m_active_binding : 1;
        bool m_binding_locked : 1;
        bool m_assignment_pending : 1;
        PairList *m_attrib;

    public:
        /** @brief Get object attributes.
         *
         * @return Pointer to the attributes of this object.
         *
         * @deprecated This method allows clients to modify the
         * attribute list directly, and thus bypass attribute
         * consistency checks.
         */
        PairList *attributes() { return m_attrib; }

        /** @brief Get object attributes (const variant).
         *
         * @return const pointer to the attributes of this object.
         */
        const PairList *attributes() const { return m_attrib; }

        /** @brief Remove all attributes.
         */
        void clearAttributes()
        {
            if (m_attrib)
            {
                m_attrib = nullptr;
                m_has_class = false;
            }
        }

        /** @brief Get the value a particular attribute.
         *
         * @param name Reference to a \c Symbol giving the name of the
         *          sought attribute.  Note that this \c Symbol is
         *          identified by its address.
         *
         * @return pointer to the value of the attribute with \a name,
         * or a null pointer if there is no such attribute.
         */
        RObject *getAttribute(const Symbol *name);

        /** @brief Get the value a particular attribute (const variant).
         *
         * @param name Reference to a \c Symbol giving the name of the
         *          sought attribute.  Note that this \c Symbol is
         *          identified by its address.
         *
         * @return const pointer to the value of the attribute with \a
         * name, or a null pointer if there is no such attribute.
         */
        const RObject *getAttribute(const Symbol *name) const;

        /** @brief Has this object any attributes?
         *
         * @return true iff this object has any attributes.
         */
        bool hasAttributes() const
        {
            return m_attrib != nullptr;
        }

        /** @brief Has this object the class attribute?
         *
         * @return true iff this object has the class attribute.
         */
        bool hasClass() const
        {
            return m_has_class;
        }

        /** @brief Reproduce the \c gp (General Purpose) bits field used in CR.
         *
         * This function is used to reproduce the
         * <tt>sxpinfo_struct.gp</tt> field used in CR.  It should be
         * used exclusively for serialization.  Refer to the 'R
         * Internals' document for details of this field.
         *
         * @return the reconstructed \c gp bits field (within the
         * least significant 16 bits).
         *
         * @note If this function is overridden in a derived class,
         * the overriding function should call packGPBits() for its
         * immediate base class, and then 'or' further bits into the
         * result.
         */
        virtual unsigned int packGPBits() const;

        /** @brief Interpret the \c gp bits field used in CR.
         *
         * This function is used to interpret the
         * <tt>sxpinfo_struct.gp</tt> field used in CR in a way
         * appropriate to a particular node class.  It should be
         * used exclusively for deserialization.  Refer to the 'R
         * Internals' document for details of this field.
         *
         * @param gpbits the \c gp bits field (within the
         *          least significant 16 bits).
         *
         * @note If this function is overridden in a derived class,
         * the overriding function should also pass its argument to
         * unpackGPBits() for its immediate base class.
         */
        virtual void unpackGPBits(unsigned int gpbits);

        /** @brief Set or remove an attribute.
         *
         * @param name Pointer to the Symbol naming the attribute to
         *          be set or removed.
         *
         * @param value Pointer to the value to be ascribed to the
         *          attribute, or a null pointer if the attribute is
         *          to be removed.  The object whose attribute is set
         *          (i.e. <tt>this</tt>) should be considered to
         *          assume ownership of \a value, which should
         *          therefore not be subsequently altered externally.
         */
        RObject *setAttribute(Symbol *name, RObject *value);

        /** @brief Replace the attributes of an object.
         *
         * @param new_attributes Pointer to the start of the new list
         *          of attributes.  May be a null pointer, in which
         *          case all attributes are removed.  The object whose
         *          attributes are set (i.e. <tt>this</tt>) should be
         *          considered to assume ownership of the 'car' values
         *          in \a new_attributes ; they should therefore not
         *          be subsequently altered externally.
         *
         * @note The \a new_attributes list should conform to the
         * class invariants.  However, attributes with null values are
         * silently discarded, and if duplicate attributes are
         * present, only the last one is heeded (and if the last
         * setting has a null value, the attribute is removed altogether).
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
         * @deprecated Ought to be private.
         */
        void setSexpType(SEXPTYPE type)
        {
            m_type = type;
        }

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

        bool trackrefs() const
        {
#ifdef COMPUTE_REFCNT_VALUES
            return (sexptype() == CLOSXP ? true : !m_spare);
#else
            return false;
#endif
        }

        void setTrackrefs(bool on)
        {
#ifdef COMPUTE_REFCNT_VALUES
#ifdef EXTRA_REFCNT_FIELDS
            m_spare = on;
#else
            m_spare = !on;
#endif
#endif
        }

        void incrementRefCount()
        {
#ifdef COMPUTE_REFCNT_VALUES
            if (refcnt() < REFCNTMAX)
                setRefCnt(refcnt() + 1);
#endif
        }

        void decrementRefCount()
        {
#ifdef COMPUTE_REFCNT_VALUES
            if (refcnt() > 0 && refcnt() < REFCNTMAX)
                setRefCnt(refcnt() - 1);
#endif
        }

        bool isScalarOfType(SEXPTYPE type) const
        {
            return (m_type == type) && m_scalar;
        }

        bool isScalar() const
        {
            return m_scalar;
        }

        void setScalar(bool on)
        {
            m_scalar = on;
        }

        unsigned int named() const
        {
            return m_named;
        }

        unsigned int refcnt() const
        {
#ifdef COMPUTE_REFCNT_VALUES
            return m_named;
#else
            return 0;
#endif
        }

        void setRefCnt(unsigned int v)
        {
#ifdef COMPUTE_REFCNT_VALUES
            m_named = v;
#endif
        }

        bool trace() const
        {
            return m_trace;
        }

        void setTrace(bool on)
        {
            m_trace = on;
        }

        bool rstep() const
        {
            return m_spare;
        }

        void setRstep(bool on)
        {
            m_spare = on;
        }

        bool bindingIsLocked() const
        {
            return m_binding_locked;
        }

        bool isActiveBinding() const
        {
            return m_active_binding;
        }

        bool assignmentPending() const
        {
            return m_assignment_pending;
        }

        void setAssignmentPending(bool on)
        {
#if CXXR_FALSE
            if (on)
            {
                m_gpbits |= ASSIGNMENT_PENDING_MASK;
            }
            else
            {
                m_gpbits &= ~ASSIGNMENT_PENDING_MASK;
            }
#endif
            m_assignment_pending = on;
        }

        void lockBinding();

        void setActiveBindingBit()
        {
            // m_gpbits |= ACTIVE_BINDING_MASK;
            m_active_binding = true;
        }

        void unlockBinding()
        {
            // m_gpbits &= (~BINDING_LOCK_MASK);
            m_binding_locked = false;
        }

        unsigned int extra() const
        {
            return m_extra;
        }

        void setExtra(unsigned int v)
        {
            m_extra = v;
        }

        bool isS4Object() const
        {
            return m_s4_object;
        }
        /** @brief Set the status of this RObject as an S4 object.
         *
         * @param on true iff this is to be considered an S4 object.
         *          CXXR raises an error if an attempt is made to
         *          unset the S4 object status of an S4Object
         *          (::S4SXP), whereas CR permits this.
         */
        void setS4Object(bool on);

        void setNamed(unsigned int v)
        {
#ifndef SWITCH_TO_REFCNT
            m_named = v;
#endif
        }

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
        virtual RObject *clone(bool deep) const;

        /** @brief Return a pointer to a copy of an object or the object itself
         *          if it isn't cloneable.
         *
         * @tparam T RObject or a type derived from RObject.
         *
         * @param pattern Either a null pointer or a pointer to the
         *          object to be cloned.
         *
         * @param deep Indicator whether to perform deep or shallow copy.
         *
         * @return Pointer to a clone of \a pattern, or \a pattern
         *         if \a pattern cannot be cloned or is itself a null pointer.
         */
        template <class T>
        static T *clone(const T *pattern, bool deep)
        {
            return pattern ? pattern->clone(deep) : nullptr;
        }

        /** @brief Temporary interface to duplicate1().
         *
         * @param T RObject or a type derived from RObject.
         *
         * @param pattern Pointer to object to be duplicated using
         * duplicate1().
         *
         * @param deep Indicator whether to perform deep or shallow copy.
         *
         * @note To be removed once copy constructors and clone() are
         * fully rolled out.
         */
        template <class T>
        static T *dup2(const T *pattern, bool deep)
        {
            return static_cast<T *>(duplicate1(const_cast<T *>(pattern), deep));
        }

        /** @brief Temporary interface to duplicate_child().
         *
         * @param T RObject or a type derived from RObject.
         *
         * @param pattern Pointer to object to be duplicated using
         * duplicate_child().
         *
         * @param deep Indicator whether to perform deep or shallow copy.
         *
         * @note To be removed once copy constructors and clone() are
         * fully rolled out.
         */
        template <class T>
        static T *dup_child2(const T *pattern, bool deep)
        {
            return static_cast<T *>(duplicate_child(const_cast<T *>(pattern), deep));
        }

        void xfix_refcnt(RObject *old, RObject *new_);
        void xfix_binding_refcnt(RObject *old, RObject *new_);

        /** @brief The name by which this type is known in R.
         *
         * @return The name by which this type is known in R.
         */
        static const char *staticTypeName()
        {
            return "RObject";
        }

    protected:
        /**
         * @param stype Required type of the RObject.
         */
        explicit RObject(SEXPTYPE stype = CXXSXP);

        /** @brief Copy constructor.
         *
         * @param pattern Object to be copied.
         *
         * @param deep Indicator whether to perform deep or shallow copy.
         */
        RObject(const RObject &pattern, bool deep);

        virtual ~RObject();
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
    extern SEXP R_NilValue;

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

    /** @brief (For use only in serialization.)
     *
     */
    int LEVELS(SEXP x);

    /** @brief Get object copying status.
     *
     * @param x Pointer to CXXR::RObject.
     * @return Refer to 'R Internals' document.  Returns 0 if \a x is a
     *         null pointer.
     */
    int NAMED(SEXP x);

    /** @brief (For use only in deserialization.)
     *
     */
    void SETLEVELS(SEXP x, int v);

    /** @brief Replace an object's attributes.
     *
     * @param x Pointer to a CXXR::RObject.
     *
     * @param v Pointer to a PairList giving the new attributes of \a
     *          x.  \a x should be considered to assume ownership of
     *          the 'car' values in \a v ; they should therefore not
     *          be subsequently altered externally.
     *
     * @note Unlike CR, \a v isn't simply plugged into the attributes
     * field of \x : refer to the documentation for \c
     * RObject::setAttributes() .  In particular, do not attempt to
     * modify the attributes by changing \a v \e after SET_ATTRIB
     * has been called.
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

    /* from dstruct.cpp */
    /*  length - length of objects  */
    int Rf_envlength(SEXP rho);

    /* TODO: a  Length(.) {say} which is length() + dispatch (S3 + S4) if needed
         for one approach, see do_seq_along() in ../main/seq.cpp
    */
    R_len_t Rf_length(SEXP s);

    R_xlen_t Rf_envxlength(SEXP rho);

    R_xlen_t Rf_xlength(SEXP s);
    SEXP R_FixupRHS(SEXP x, SEXP y);
    Rboolean Rf_isFrame(SEXP s);

    /* As from R 2.4.0 we check that the value is allowed. */
    SEXP Rf_ScalarLogical(int x);
    /* from util.cpp */

    /**
     * @brief Check to see if the arrays "x" and "y" have the identical extents
     */
    Rboolean Rf_conformable(SEXP x, SEXP y);

    /**
     * @note R's Rf_inherits() is based on inherits3() in ../main/objects.cpp
     * Here, use char / CHAR() instead of the slower more general Rf_translateChar()
     */
    Rboolean Rf_inherits(SEXP s, const char *name);

    Rboolean Rf_isSymbol(SEXP s);
    Rboolean Rf_isLogical(SEXP s);
    Rboolean Rf_isReal(SEXP s);
    Rboolean Rf_isComplex(SEXP s);
    Rboolean Rf_isExpression(SEXP s);
    Rboolean Rf_isEnvironment(SEXP s);
    Rboolean Rf_isString(SEXP s);
    Rboolean Rf_isRaw(SEXP s);
    int ALTREP(SEXP x);
    void SETALTREP(SEXP x, int v);
    int REFCNT(SEXP x);
    void SET_REFCNT(SEXP x, unsigned int v);
    int TRACKREFS(SEXP x);
    void SET_TRACKREFS(SEXP x, bool v);
    int IS_SCALAR(SEXP x, SEXPTYPE type);
    int SIMPLE_SCALAR_TYPE(SEXP x);
    void DECREMENT_REFCNT(SEXP x);
    void INCREMENT_REFCNT(SEXP x);
    void DISABLE_REFCNT(SEXP x);
    void ENABLE_REFCNT(SEXP x);
    void SHALLOW_DUPLICATE_ATTRIB(SEXP to, SEXP from);
    int ASSIGNMENT_PENDING(SEXP x);
    void SET_ASSIGNMENT_PENDING(SEXP x, int v);

    void(MARK_NOT_MUTABLE)(SEXP x);
    int IS_ASSIGNMENT_CALL(SEXP x);
    void MARK_ASSIGNMENT_CALL(SEXP x);
    void ENSURE_NAMEDMAX(SEXP x);
    void ENSURE_NAMED(SEXP x);
    void SETTER_CLEAR_NAMED(SEXP x);
    void RAISE_NAMED(SEXP x, int n);
} // extern "C"

/** @brief Shorthand for Rf_length().
 */
inline R_len_t length(SEXP s)
{
    return Rf_length(s);
}

#if defined(R_NO_REMAP) && defined(COMPILING_IVORY) && defined(__cplusplus)
const auto xlength = Rf_xlength;
const auto isFrame = Rf_isFrame;
const auto conformable = Rf_conformable;
const auto inherits = Rf_inherits;
#endif

#endif /* ROBJECT_HPP */
