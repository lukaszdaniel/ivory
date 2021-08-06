/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999--2020  The R Core Team.
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
 *
 *  Rho is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the Rho website.
 *
 *  This header file is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
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

/** @file VectorBase.hpp
 * @brief Class CXXR::VectorBase and associated C interface.
 */

#ifndef VECTORBASE_HPP
#define VECTORBASE_HPP

#include <cstddef>
#include <CXXR/GCStackRoot.hpp>
#include <CXXR/RObject.hpp>
#include <CXXR/RAltRep.hpp>
#include <CXXR/SEXP_downcast.hpp>
#include <Localization.h>

namespace CXXR
{
  template <typename, SEXPTYPE>
  class FixedVector;
  using IntVector = FixedVector<int, INTSXP>;

  class ListVector;
  class StringVector;

  /** @brief Untemplated base class for R vectors.
   */
  class VectorBase : public RObject
  {
  public:
    using size_type = R_xlen_t;

    /**
     * @param stype The required ::SEXPTYPE.
     *
     * @param sz The required number of elements in the vector.
     */
    VectorBase(SEXPTYPE stype, R_xlen_t sz)
        : RObject(stype), m_size(sz), m_truelength(0), m_growable(false)
    {
      if (sz > R_XLEN_T_MAX)
        Rf_error(_("vector is too large")); /**** put length into message */
      else if (sz < 0)
        Rf_error(_("negative length vectors are not allowed"));
    }

    /** @brief Copy constructor.
     *
     * @param pattern VectorBase to be copied.
     *
     * @param deep Indicator whether to perform deep or shallow copy.
     */
    VectorBase(const VectorBase &pattern, Duplicate deep)
        : RObject(pattern, deep), m_size(pattern.m_size), m_truelength(0), m_growable(pattern.m_growable)
    {
      if (!m_growable)
        m_truelength = pattern.m_truelength;
    }

    /** @brief Names associated with the rows, columns or other
     *  dimensions of an R matrix or array.
     *
     * @return A null pointer signifies that there are no names
     * associated with any of the dimensions of \a *this ; a null
     * pointer will always be returned if \a *this is not an R
     * matrix or array.  Otherwise the return value will be a
     * pointer to ListVector with as many elements as \a *this has
     * dimensions.  Each element of the ListVector is either a
     * null pointer, signifying that there are no names associated
     * with the corresponding dimension, or a pointer to a
     * StringVector with as many elements as the size of the array
     * along the corresponding dimension, giving the names of the
     * 'slices' along that dimension.  For example the zeroth
     * element of the ListVector, if non-null, will give the row
     * names, and the following element will give the column
     * names.
     */
    const ListVector *dimensionNames() const;

    /** @brief Names associated with a particular dimension of an
     *  R matrix or array.
     *
     * @param d Dimension number (counting from 1) for which
     *          dimension names are required.  Must be non-zero (not
     *          checked).
     *
     * @return A null pointer if no names are associated with
     * dimension \a d of \a *this , if \a *this does not have as
     * many as \a d dimensions, or if \a *this is not an R matrix
     * or array.  Otherwise a pointer to a StringVector with as
     * many elements as the size of \a *this along the
     * corresponding dimension, giving the names of the 'slices'
     * along that dimension.
     */
    const StringVector *dimensionNames(unsigned int d) const;

    /** @brief Dimensions of R matrix or array.
     *
     * @return A null pointer if \a *this is not an R matrix or
     * array.  Otherwise a pointer to an IntVector with one or
     * more elements, the product of the elements being equal to
     * the number of elements in \a *this .  The number of
     * elements in the return value is the dimensionality of the
     * array (e.g. 2 for a matrix), and each element gives the
     * size of the array along the respective dimension.
     */
    const IntVector *dimensions() const;

    /** @brief Names of vector elements.
     *
     * @return either a null pointer (if the elements do not have
     * names), or a pointer to a StringVector with the same number
     * of elements as \a *this .
     */
    const StringVector *names() const;

    /** @brief Associate names with the rows, columns or other
     *  dimensions of an R matrix or array.
     *
     * @param names If this is a null pointer, any names currently
     *          associated with the dimensions of \a *this will be
     *          removed.  Otherwise \a names must be a pointer to
     *          ListVector with as many elements as \a *this has
     *          dimensions.  Each element of the ListVector must
     *          be either a null pointer, signifying that no names
     *          are to be associated with the corresponding
     *          dimension, or a pointer to a StringVector with as
     *          many elements as the size of the array along the
     *          corresponding dimension, giving the names to be
     *          given to the 'slices' along that dimension.  For
     *          example the zeroth element of the ListVector, if
     *          non-null, will give the row names, and the
     *          following element will give the column names.  \a
     *          *this will assume ownership of this ListVector
     *          (rather than duplicating it), so the calling code
     *          must not subsequently modify it.
     */
    void setDimensionNames(ListVector *names);

    /** @brief Define the dimensions of R matrix or array.
     *
     * As a side-effect, this function will remove any dimension names.
     *
     * @param dims If this is a null pointer, any existing dimensions
     *          associated will be removed, and \a *this will
     *          cease to be a R matrix/array.  Otherwise \a dims
     *          must be a pointer to an IntVector with one or more
     *          elements, the product of the elements being equal
     *          to the number of elements in \a *this . The number
     *          of elements in \a dims is the required
     *          dimensionality of the array (e.g. 2 for a matrix),
     *          and each element gives the required size of the
     *          array along the respective dimension.  \a *this
     *          will assume ownership of this IntVector (rather
     *          than duplicating it), so the calling code must not
     *          subsequently modify it.
     */
    void setDimensions(IntVector *dims);

    /** @brief Associate names with the elements of a VectorBase.
     *
     * @param names Either a null pointer, in which case any
     * existing names will be removed, or a pointer to a
     * StringVector with the same number of elements as \a *this .
     * \a *this will assume ownership of this StringVector (rather
     * than duplicating it), so the calling code must not
     * subsequently modify it.
     */
    void setNames(StringVector *names);

    /** @brief Alter the size (number of elements) in the vector.
     *
     * @param new_size New size required.  Zero is permissible,
     *          but (as presently implemented) the new size must
     *          not be greater than the current size. 
     */
    void resize(R_xlen_t new_size);

    /** @brief Number of elements in the vector.
     *
     * @return The number of elements in the vector.
     */
    R_xlen_t size() const
    {
      return m_size;
    }

    R_xlen_t truelength() const
    {
      return m_truelength;
    }

    bool growable() const
    {
      return m_growable;
    }

    void setGrowable(bool on)
    {
      m_growable = on;
    }

    /** @brief The name by which this type is known in R.
     *
     * @return the name by which this type is known in R.
     */
    static const char *staticTypeName()
    {
      return "(vector type)";
    }

    virtual void *data() = 0;
    virtual const void *data() const = 0;

    // Virtual functions of RObject:
    unsigned int packGPBits() const override;
    void unpackGPBits(unsigned int gpbits) override;

    static int *chkzln(RObject *x);

    static inline R_xlen_t stdvec_length(RObject *x)
    {
      VectorBase *vb = dynamic_cast<VectorBase *>(x);
      return vb ? vb->size() : 0;
    }

    static inline R_xlen_t stdvec_truelength(RObject *x)
    {
      VectorBase *vb = dynamic_cast<VectorBase *>(x);
      return vb ? vb->truelength() : 0;
    }

    static inline void set_stdvec_truelength(RObject *x, R_xlen_t v)
    {
      VectorBase *vb = dynamic_cast<VectorBase *>(x);
      if (!vb)
        Rf_error(_("SET_TRUELENGTH invoked for a non-vector."));
      vb->m_truelength = v;
    }

    static inline void set_stdvec_length(RObject *x, R_xlen_t v)
    {
      VectorBase *vb = dynamic_cast<VectorBase *>(x);
      if (!vb)
        Rf_error(_("SETLENGTH invoked for a non-vector."));
      vb->resize(v);
      x->setScalar(v == 1);
    }

    static inline void set_truelength(RObject *x, R_xlen_t v)
    {
      if (x && x->altrep())
        Rf_error(_("can't set ALTREP truelength"));
      CXXR::VectorBase::set_stdvec_truelength(x, v);
    }

  protected:
    ~VectorBase() {}

    /** @brief Raise error on attempt to allocate overlarge vector.
     *
     * @param bytes Size of data block for which allocation failed.
     */
    static void tooBig(std::size_t bytes);

  private:
    R_xlen_t m_size;
    R_xlen_t m_truelength;
    bool m_growable;
  };

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

  template <typename T = void>
  inline T *stdvec_dataptr(RObject *x)
  {
    return reinterpret_cast<T *>(SEXP_downcast<VectorBase *>(x, false)->data());
  }
} // namespace CXXR

namespace R
{
  /**
   * @param x Pointer to an CXXR::VectorBase.
   *
   * @return The length of \a x, or 0 if \a x is a null pointer.  (In
   *         the case of certain hash tables, this means the 'capacity'
   *         of \a x , not all of which may be used.)
   */
  R_xlen_t STDVEC_LENGTH(SEXP x);

  /**
   * @param x Pointer to a CXXR::VectorBase.
   *
   * @return The 'true length' of \a x.  According to the R Internals
   *         document for R 2.4.1, this is only used for certain hash
   *         tables, and signifies the number of used slots in the
   *         table.
   *
   * @deprecated May be withdrawn in the future.
   */
  R_xlen_t STDVEC_TRUELENGTH(SEXP x);

#ifdef LONG_VECTOR_SUPPORT
  NORET R_len_t R_BadLongVector(SEXP, const char *, int);
#endif
} // namespace R

extern "C"
{
  /* Accessor functions */

  /* Vector Access Functions */

  /**
   * @param x Pointer to a CXXR::RObject.
   *
   * @return The length of \a x, or 0 if \a x is a null pointer, or is
   *         not a pointer to a vector object (VectorBase).  (In 
   *         the case of certain hash tables, this means the 'capacity'
   *         of \a x , not all of which may be used.)
   */
  int(LENGTH)(SEXP x);

  /**
   * @param x Pointer to a CXXR::VectorBase.
   *
   * @return The 'true length' of \a x.  According to the R Internals
   *         document for R 2.4.1, this is only used for certain hash
   *         tables, and signifies the number of used slots in the
   *         table.
   */
  R_xlen_t(TRUELENGTH)(SEXP x);

  /** @brief Set length of vector.
   *
   * @param x Pointer to a CXXR::VectorBase.
   *
   * @param v The required new length, which must not be greater than
   *          the current length.
   *
   * @deprecated May be withdrawn in future.  Currently used in
   * library/stats/src/isoreg.cpp , and possibly in packages.
   */
  void SETLENGTH(SEXP x, R_xlen_t v);

  /** @brief Set 'true length' of vector.
   *
   * @param x Pointer to a CXXR::VectorBase.
   *
   * @param v The required new 'true length'.
   *
   * @deprecated May be withdrawn in the future.
   */
  void SET_TRUELENGTH(SEXP x, R_xlen_t v);

  /** @brief Create a vector object.
   *
   *  Allocate a vector object.  This ensures only validity of
   *  ::SEXPTYPE values representing lists (as the elements must be
   *  initialized).  Initializing of other vector types is done in
   *  do_makevector().
   *  Regular Rf_allocVector() as a special case of allocVector3()
   *  with no custom allocator.
   *
   * @param stype The type of vector required.
   *
   * @param length The length of the vector to be created.
   *
   * @return Pointer to the created vector.
   */
  SEXP Rf_allocVector(SEXPTYPE stype, R_xlen_t length);

  /** @brief Create a vector object.
   *
   *  Allocate a vector object.  This ensures only validity of
   *  ::SEXPTYPE values representing lists (as the elements must be
   *  initialized).  Initializing of other vector types is done in
   *  do_makevector().
   *
   * @param stype The type of vector required.
   *
   * @param length The length of the vector to be created.
   *
   * @param length Custom allocator to be used.
   *
   * @return Pointer to the created vector.
   */
  SEXP Rf_allocVector3(SEXPTYPE type, R_xlen_t length, R_allocator_t *allocator);

  /** @brief Is an RObject a vector?
   *
   * Vector in this context embraces R matrices and arrays.
   *
   * @param s Pointer to the RObject to be tested.  The pointer may be
   *          null, in which case the function returns FALSE.
   *
   * @return TRUE iff \a s points to a vector object.
   */
  Rboolean Rf_isVector(SEXP s);

  /** @fn SEXP Rf_mkNamed(SEXPTYPE TYP, const char **names)
   *
   * @brief Create a named vector of type TYP
   *
   * @example const char *nms[] = {"xi", "yi", "zi", ""};
   *          mkNamed(VECSXP, nms);  =~= R  list(xi=, yi=, zi=)
   *
   * @param TYP a vector SEXP type (e.g. REALSXP)
   *
   * @param names names of list elements with null string appended
   *
   * @return (pointer to a) named vector of type TYP
   */
  SEXP Rf_mkNamed(SEXPTYPE TYP, const char **names);

  /** @brief shortcut for ScalarString(Rf_mkChar(s))
   *
   * @return string scalar
   *
   * @note from gram.y
   */
  SEXP Rf_mkString(const char *s);

  Rboolean Rf_isVectorList(SEXP s);
  Rboolean Rf_isVectorAtomic(SEXP s);
  Rboolean Rf_isMatrix(SEXP s);
  Rboolean Rf_isArray(SEXP s);
  Rboolean Rf_isTs(SEXP s);
  R_xlen_t XLENGTH_EX(SEXP x);
  R_xlen_t XTRUELENGTH(SEXP x);
  int LENGTH_EX(SEXP x, const char *file, int line);
  void *DATAPTR(SEXP x);
  void *STDVEC_DATAPTR(SEXP x);

  /** @brief The general (read only) data pointer function
   *
   * Function works as a dispatcher between ALTREP
   * or STDVEC representation of data.
   *
   * @return pointer to the (read only) data block
   */
  const void *DATAPTR_RO(SEXP x);

  const void *DATAPTR_OR_NULL(SEXP x);
  NORET SEXP *VECTOR_PTR(SEXP x);

  /* Growable vector support */
  int IS_GROWABLE(SEXP x);
  void SET_GROWABLE_BIT(SEXP x);
  R_xlen_t(XLENGTH)(SEXP x);
  int(IS_LONG_VEC)(SEXP x);

  /* temporary, to ease transition away from remapping */
  R_xlen_t Rf_XLENGTH(SEXP x);
} // extern "C"

#if (defined(R_NO_REMAP) && defined(COMPILING_IVORY)) && defined(__cplusplus)
const auto allocVector = Rf_allocVector;
const auto allocVector3 = Rf_allocVector3;
const auto mkNamed = Rf_mkNamed;
const auto mkString = Rf_mkString;
const auto isVector = Rf_isVector;
const auto isVectorAtomic = Rf_isVectorAtomic;
const auto isVectorList = Rf_isVectorList;
const auto isTs = Rf_isTs;
const auto isArray = Rf_isArray;
const auto isMatrix = Rf_isMatrix;
#endif

#endif /* VECTORBASE_HPP */
