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

#include <CXXR/GCRoot.hpp>
#include <CXXR/RObject.hpp>
#include <CXXR/SEXP_downcast.hpp>
#include <Localization.h>
#include <cstddef>

namespace CXXR
{
  /** @brief Untemplated base class for R vectors.
   */
  class VectorBase : public RObject
  {
  public:
    typedef std::size_t size_type;
    /**
     * @param stype The required ::SEXPTYPE.
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
    VectorBase(const VectorBase &pattern, bool deep)
        : RObject(pattern, deep), m_size(pattern.m_size), m_truelength(0), m_growable(pattern.m_growable)
    {
      if (!m_growable)
        m_truelength = pattern.m_truelength;
    }

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
      // m_gpbits |= GROWABLE_MASK;
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

    /** @brief Raise error on attempt to allocate overlarge vector.
     *
     * @param bytes Size of data block for which allocation failed.
     */
    static void tooBig(std::size_t bytes);
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
   * @return The 'true length' of \a x.  According to the R Internals
   *         document for R 2.4.1, this is only used for certain hash
   *         tables, and signifies the number of used slots in the
   *         table.
   */
  R_xlen_t(TRUELENGTH)(SEXP x);

  /**
   * Set length of vector.
   * @param x Pointer to a CXXR::VectorBase.
   * @param v The required new length.
   */
  void SETLENGTH(SEXP x, R_xlen_t v);

  /**
   * Set 'true length' of vector.
   * @param x Pointer to a CXXR::VectorBase.
   * @param v The required new 'true length'.
   */
  void SET_TRUELENGTH(SEXP x, R_xlen_t v);

  /**
   * @brief Create a vector object.
   *
   *  Allocate a vector object.  This ensures only validity of
   *  ::SEXPTYPE values representing lists (as the elements must be
   *  initialized).  Initializing of other vector types is done in
   *  do_makevector().
   *  Regular Rf_allocVector() as a special case of allocVector3()
   *  with no custom allocator.
   * 
   * @param stype The type of vector required.
   * @param length The length of the vector to be created.
   * @return Pointer to the created vector.
   */
  SEXP Rf_allocVector(SEXPTYPE type, R_xlen_t length);

  /**
   * @brief Create a vector object.
   *
   *  Allocate a vector object.  This ensures only validity of
   *  ::SEXPTYPE values representing lists (as the elements must be
   *  initialized).  Initializing of other vector types is done in
   *  do_makevector().
   * 
   * @param stype The type of vector required.
   * @param length The length of the vector to be created.
   * @param length Custom allocator to be used.
   * @return Pointer to the created vector.
   */
  SEXP Rf_allocVector3(SEXPTYPE type, R_xlen_t length, R_allocator_t *allocator);

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
  SEXP Rf_mkNamed(SEXPTYPE TYP, const char **names);

  /**
   * @brief shortcut for ScalarString(Rf_mkChar(s))
   *
   * @return string scalar
   *
   * @note from gram.y
 */
  SEXP Rf_mkString(const char *s);

  Rboolean Rf_isVectorList(SEXP s);
  Rboolean Rf_isVectorAtomic(SEXP s);
  Rboolean Rf_isVector(SEXP s);
  Rboolean Rf_isMatrix(SEXP s);
  Rboolean Rf_isArray(SEXP s);
  Rboolean Rf_isTs(SEXP s);
  R_xlen_t XLENGTH_EX(SEXP x);
  R_xlen_t XTRUELENGTH(SEXP x);
  int LENGTH_EX(SEXP x, const char *file, int line);
  void *DATAPTR(SEXP x);
  void *STDVEC_DATAPTR(SEXP x);

  /**
   * @brief The general (read only) data pointer function
   *
   * Function works as a dispatcher between ALTREP
   * or STDVEC representation of data.
   *
   * @return pointer to the (read only) data block
   */
  const void *DATAPTR_RO(SEXP x);

  const void *DATAPTR_OR_NULL(SEXP x);
  NORET SEXP *VECTOR_PTR(SEXP x);

#ifdef LONG_VECTOR_SUPPORT
  NORET R_len_t R_BadLongVector(SEXP, const char *, int);
#endif

  /* Growable vector support */
  int IS_GROWABLE(SEXP x);
  void SET_GROWABLE_BIT(SEXP x);

  R_xlen_t STDVEC_LENGTH(SEXP x);
  R_xlen_t STDVEC_TRUELENGTH(SEXP x);
  R_xlen_t(XLENGTH)(SEXP x);
  int(IS_LONG_VEC)(SEXP x);

  /* temporary, to ease transition away from remapping */
  R_xlen_t Rf_XLENGTH(SEXP x);
} // extern "C"

#if defined(R_NO_REMAP) && defined(COMPILING_IVORY) && defined(__cplusplus)
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
