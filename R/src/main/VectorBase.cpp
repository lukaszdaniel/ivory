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
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
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

/** @file VectorBase.cpp
 *
 * @brief Implementation of class VectorBase and related functions.
 */

#include <CXXR/VectorBase.hpp>
#include <R_ext/Error.h>
#include <Rinternals.h>
#include <Localization.h>

namespace CXXR
{
    // Force the creation of non-inline embodiments of functions callable
    // from C:
    namespace ForceNonInline
    {
        const auto &STDVEC_LENGTHptr = STDVEC_LENGTH;
        const auto &SET_TRUELENGTHptr = SET_TRUELENGTH;
        const auto &STDVEC_TRUELENGTHptr = STDVEC_TRUELENGTH;
    } // namespace ForceNonInline

    namespace
    {
        // Used in {,un}packGPBits():
        constexpr unsigned int GROWABLE_MASK = 1 << 5;
    } // namespace

    unsigned int VectorBase::packGPBits() const
    {
        unsigned int ans = RObject::packGPBits();
        if (m_growable)
            ans |= GROWABLE_MASK;
        return ans;
    }

    void VectorBase::unpackGPBits(unsigned int gpbits)
    {
        RObject::unpackGPBits(gpbits);
        m_growable = ((gpbits & GROWABLE_MASK) != 0);
    }

    void VectorBase::resize(R_xlen_t new_size)
    {
        // if (new_size > m_size)
        // {
        //     Rf_error("VectorBase::resize() : requested size exceeds current size.");
        //     return;
        // }
        m_size = new_size;
    }

    // The error messages here match those used by CR (as of 3.0.2),
    // not including the malformed unit abbreviations.
    void VectorBase::tooBig(std::size_t bytes)
    {
        double dsize = double(bytes) / 1024.0;
        if (dsize > 1024.0 * 1024.0)
        {
            Rf_errorcall(nullptr, _("cannot allocate vector of size %0.1f GB"), dsize / 1024.0 / 1024.0);
        }
        else if (dsize > 1024.0)
        {
            Rf_errorcall(nullptr, _("cannot allocate vector of size %0.1f MB"), dsize / 1024.0);
        }
        else
        {
            Rf_errorcall(nullptr, _("cannot allocate vector of size %0.1f KB"), dsize);
        }
    }

    /* Growable vector support */
    unsigned int VectorBase::growable_bit_set(RObject *x)
    {
        if (!x)
            return 0;
        return SEXP_downcast<VectorBase *>(x)->growable();
    }

    void VectorBase::set_growable_bit(RObject *x)
    {
        if (!x)
            return;
        // x->m_gpbits |= GROWABLE_MASK;
        SEXP_downcast<VectorBase *>(x)->setGrowable(true);
    }
    int *VectorBase::chkzln(SEXP x)
    {
#ifdef CATCH_ZERO_LENGTH_ACCESS
        /* Attempts to read or write elements of a zero length vector will
         result in a segfault, rather than read and write random memory.
         Returning NULL would be more natural, but Matrix seems to assume
         that even zero-length vectors have non-NULL data pointers, so
         return (void *) 1 instead. Zero-length CHARSXP objects still have a
         trailing zero byte so they are not handled. */
        if (STDVEC_LENGTH(x) == 0 && TYPEOF(x) != CHARSXP)
            return (int *)1;
#endif
        return (int *)nullptr;
    }
} // namespace CXXR

// ***** C interface *****

#ifdef LONG_VECTOR_SUPPORT
NORET R_len_t R_BadLongVector(SEXP x, const char *file, int line)
{
    Rf_error(_("long vectors not supported yet: %s:%d"), file, line);
}
#endif

#ifdef STRICT_TYPECHECK
static void CHKVEC(SEXP x)
{
    switch (TYPEOF(x))
    {
    case CHARSXP:
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
    case VECSXP:
    case EXPRSXP:
    case RAWSXP:
    case WEAKREFSXP:
        break;
    default:
        Rf_error(_("cannot get data pointer of '%s' objects"), Rf_type2char(TYPEOF(x)));
    }
}
#else
#define CHKVEC(x) \
    do            \
    {             \
    } while (0)
#endif

void SETLENGTH(SEXP x, R_xlen_t v)
{
    if (ALTREP(x))
        Rf_error(_("SETLENGTH() cannot be applied to an ALTVEC object."));
    if (!Rf_isVector(x))
        Rf_error(_("SETLENGTH() can only be applied to a standard vector, not a '%s'"), Rf_type2char(TYPEOF(x)));
    CXXR::VectorBase::set_stdvec_length(x, v);
}

R_xlen_t XLENGTH_EX(SEXP x)
{
    return ALTREP(x) ? ALTREP_LENGTH(x) : STDVEC_LENGTH(x);
}

R_xlen_t XTRUELENGTH(SEXP x)
{
    return ALTREP(x) ? ALTREP_TRUELENGTH(x) : STDVEC_TRUELENGTH(x);
}

int LENGTH_EX(SEXP x, const char *file, int line)
{
    if (!x || x == R_NilValue)
        return 0;
    R_xlen_t len = XLENGTH(x);
#ifdef LONG_VECTOR_SUPPORT
    if (len > R_SHORT_LEN_MAX)
        R_BadLongVector(x, file, line);
#endif
    return (int)len;
}

void *STDVEC_DATAPTR(SEXP x)
{
    if (ALTREP(x))
        Rf_error(_("cannot get STDVEC_DATAPTR from ALTREP object"));
    if (!Rf_isVector(x) && TYPEOF(x) != WEAKREFSXP)
        Rf_error(_("STDVEC_DATAPTR can only be applied to a vector, not a '%s'"),
                 Rf_type2char(TYPEOF(x)));
    CXXR::VectorBase::chkzln(x);
    return CXXR::stdvec_dataptr<>(x);
}

void *DATAPTR(SEXP x)
{
    CHKVEC(x);
    if (ALTREP(x))
        return ALTVEC_DATAPTR(x);
#ifdef CATCH_ZERO_LENGTH_ACCESS
    /* Attempts to read or write elements of a zero length vector will
       result in a segfault, rather than read and write random memory.
       Returning NULL would be more natural, but Matrix seems to assume
       that even zero-length vectors have non-NULL data pointers, so
       return (void *) 1 instead. Zero-length CHARSXP objects still
       have a trailing zero byte so they are not handled. */
    else if (STDVEC_LENGTH(x) == 0 && TYPEOF(x) != CHARSXP)
        return (void *)1;
#endif
    else
        return STDVEC_DATAPTR(x);
}

const void *DATAPTR_RO(SEXP x)
{
    CHKVEC(x);
    if (ALTREP(x))
        return ALTVEC_DATAPTR_RO(x);
    else
        return STDVEC_DATAPTR(x);
}

const void *DATAPTR_OR_NULL(SEXP x)
{
    CHKVEC(x);
    if (ALTREP(x))
        return ALTVEC_DATAPTR_OR_NULL(x);
    else
        return STDVEC_DATAPTR(x);
}

NORET SEXP *VECTOR_PTR(SEXP x)
{
    Rf_error(_("not safe to return vector pointer"));
}

int(LENGTH)(SEXP x)
{
    return x == R_NilValue ? 0 : LENGTH(x);
}

R_xlen_t(XLENGTH)(SEXP x)
{
    return XLENGTH(x);
}

R_xlen_t(TRUELENGTH)(SEXP x)
{
    return TRUELENGTH(x);
}

int(IS_LONG_VEC)(SEXP x)
{
    return IS_LONG_VEC(x);
}

R_xlen_t Rf_XLENGTH(SEXP x)
{
    return XLENGTH(x);
}

int IS_GROWABLE(SEXP x)
{
    return CXXR::VectorBase::growable_bit_set(x) && XLENGTH(x) < XTRUELENGTH(x);
}

void SET_GROWABLE_BIT(SEXP x)
{
    CXXR::VectorBase::set_growable_bit(x);
}

void SET_TRUELENGTH(SEXP x, R_xlen_t v)
{
    CXXR::VectorBase::set_truelength(x, v);
}

R_xlen_t STDVEC_LENGTH(SEXP x)
{
    return CXXR::VectorBase::stdvec_length(x);
}

R_xlen_t STDVEC_TRUELENGTH(SEXP x)
{
    return CXXR::VectorBase::stdvec_truelength(x);
}

SEXP Rf_allocVector(SEXPTYPE type, R_xlen_t length = 1)
{
    return Rf_allocVector3(type, length, nullptr);
}

SEXP Rf_mkNamed(SEXPTYPE TYP, const char **names)
{
    R_xlen_t n;

    for (n = 0; strlen(names[n]) > 0; ++n)
    {
    }
    CXXR::GCRoot<> ans(Rf_allocVector(TYP, n));
    CXXR::GCRoot<> nms(Rf_allocVector(STRSXP, n));
    for (R_xlen_t i = 0; i < n; ++i)
        SET_STRING_ELT(nms, i, Rf_mkChar(names[i]));
    Rf_setAttrib(ans, R_NamesSymbol, nms);
    return ans;
}

SEXP Rf_mkString(const char *s)
{
    CXXR::GCRoot<> t(Rf_allocVector(STRSXP, (R_xlen_t)1));
    SET_STRING_ELT(t, (R_xlen_t)0, Rf_mkChar(s));
    return t;
}

Rboolean Rf_isVectorList(SEXP s)
{
    switch (TYPEOF(s))
    {
    case VECSXP:
    case EXPRSXP:
        return TRUE;
    default:
        return FALSE;
    }
}

Rboolean Rf_isVectorAtomic(SEXP s)
{
    switch (TYPEOF(s))
    {
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
    case RAWSXP:
        return TRUE;
    default: /* including NULL */
        return FALSE;
    }
}

Rboolean Rf_isVector(SEXP s) /* === isVectorList() or isVectorAtomic() */
{
    switch (TYPEOF(s))
    {
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
    case RAWSXP:

    case VECSXP:
    case EXPRSXP:
        return TRUE;
    default:
        return FALSE;
    }
}

Rboolean Rf_isMatrix(SEXP s)
{
    if (Rf_isVector(s))
    {
        SEXP t = Rf_getAttrib(s, R_DimSymbol);
        /* You are not supposed to be able to assign a non-integer dim,
	   although this might be possible by misuse of ATTRIB. */
        if (TYPEOF(t) == INTSXP && LENGTH(t) == 2)
            return TRUE;
    }
    return FALSE;
}

Rboolean Rf_isArray(SEXP s)
{
    if (Rf_isVector(s))
    {
        SEXP t = Rf_getAttrib(s, R_DimSymbol);
        /* You are not supposed to be able to assign a 0-length dim,
	 nor a non-integer dim */
        if ((TYPEOF(t) == INTSXP) && LENGTH(t) > 0)
            return TRUE;
    }
    return FALSE;
}

Rboolean Rf_isTs(SEXP s)
{
    return Rboolean(Rf_isVector(s) && Rf_getAttrib(s, R_TspSymbol) != R_NilValue);
}