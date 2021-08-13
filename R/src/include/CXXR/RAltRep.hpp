/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2006   The R Development Core Team.
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
 *
 *  Rho is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the Rho website.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

/** @file RAltRep.hpp
 * @brief Class CXXR::AltRep.
 */

#ifndef RALTREP_HPP
#define RALTREP_HPP

#define CXXR_USE_OLD_ALTREP_IMPL

#include <CXXR/PairList.hpp>

namespace CXXR
{
    /** @brief Singly linked list of pairs for Alternative Representation (ALTREP)
     */
    class AltRep : public PairList
    {
    public:
        /**
         * @param cr Pointer to the 'car' of the element to be
         *           constructed.
         * @param tl Pointer to the 'tail' (LISP cdr) of the element
         *           to be constructed.
         * @param tg Pointer to the 'tag' of the element to be constructed.
         */
        explicit AltRep(RObject *cr = nullptr, PairList *tl = nullptr, const RObject *tg = nullptr)
            : PairList(cr, tl, tg)
        {
            setAltrep(true);
        }

        /** @brief The name by which this type is known in R.
         *
         * @return the name by which this type is known in R.
         */
        static const char *staticTypeName()
        {
            return "altrep";
        }

        /** @brief Object type.
         *
         * @param x Pointer to CXXR::AltRep.
         *
         * @return Wrapper \c SEXPTYPE of \a x, or NILSXP if x is a null pointer.
         */
        SEXPTYPE altsexptype() const
        {
            return m_wrapper_type;
        }

        /**
         * @deprecated Ought to be private.
         */
        void setAltsexptype(SEXPTYPE type)
        {
            m_wrapper_type = type;
        }

        // Virtual functions of RObject:
        AltRep *clone(Duplicate deep) const override;
        const char *typeName() const override;

    private:
        SEXPTYPE m_wrapper_type;
        // Declared private to ensure that AltRep objects are
        // allocated only using 'new':
        ~AltRep() {}

        // Not implemented yet.  Declared to prevent
        // compiler-generated versions:
        AltRep &operator=(const AltRep &);
    };
} // namespace CXXR

namespace R
{
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
    // SEXP R_virtrep_vec(SEXP, SEXP);
    SEXP R_tryUnwrap(SEXP);
    void R_init_altrep();
    void R_reinit_altrep_classes(DllInfo *);
} // namespace R

extern "C"
{
    SEXP R_tryWrap(SEXP x);
    SEXP ALTREP_CLASS(SEXP x);
    SEXP R_altrep_data1(SEXP x);
    SEXP R_altrep_data2(SEXP x);
    void R_set_altrep_data1(SEXP x, SEXP v);
    void R_set_altrep_data2(SEXP x, SEXP v);

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
} // extern "C"

#endif /* RALTREP_HPP */
