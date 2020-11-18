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
     * @param stype The required <tt>SEXPTYPE</tt>.
     * @param sz The required number of elements in the vector.
     */
    VectorBase(SEXPTYPE stype, size_t sz)
        : RObject(stype)
    {
      u.vecsxp.m_length = sz;
      u.vecsxp.m_truelength = sz;
    }

    /** @brief Number of elements in the vector.
     *
     * @return The number of elements in the vector.
     */
    auto size() const
    {
      return length();
    }

    /** @brief The name by which this type is known in R.
     *
     * @return the name by which this type is known in R.
     */
    static const char *staticTypeName()
    {
      return "(vector type)";
    }

  public:
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
      if (CXXR::RObject::altrep(x))
        Rf_error("can't set ALTREP truelength");
      CXXR::VectorBase::set_stdvec_truelength(x, v);
    }
  };

  using VECSEXP = class CXXR::RObject *;

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

  template <typename T = void *>
  inline T stdvec_dataptr(RObject *x)
  {
    return static_cast<T>(x->m_data);
  }

  inline const char *r_char(RObject *x)
  {
    return stdvec_dataptr<const char *>(x);
  }

  /* writable char access for R internal use only */
  inline char *CHAR_RW(RObject *x)
  {
    return stdvec_dataptr<char *>(x);
  }

#define VECTOR_ELT(x, i) ((SEXP *)DATAPTR(x))[i]
#define VECTOR_PTR(x) ((SEXP *)DATAPTR(x))

} // namespace CXXR

#endif /* VECTORBASE_HPP */
