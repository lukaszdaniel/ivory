/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *            (C) 2004  The R Foundation
 *  Copyright (C) 1998-2015 The R Core Team.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) anylater version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#define R_NO_REMAP

#include <CXXR/PairList.hpp>
#include <CXXR/Expression.hpp>
#include <CXXR/String.hpp>
#include <CXXR/DottedArgs.hpp>
#include <CXXR/IntVector.hpp>
#include <CXXR/LogicalVector.hpp>
#include <CXXR/RealVector.hpp>
#include <CXXR/ComplexVector.hpp>
#include <CXXR/RawVector.hpp>
#include <CXXR/StringVector.hpp>
#include <CXXR/Closure.hpp>
#include <CXXR/ExpressionVector.hpp>
#include <CXXR/RAltRep.hpp>
#include <Localization.h>
#include <Defn.h>

#include "duplicate.h"

using namespace R;
using namespace CXXR;

/*  duplicate  -  object duplication  */

#ifdef R_PROFILING
static unsigned long duplicate_counter = static_cast<unsigned long>(-1);

HIDDEN unsigned long R::get_duplicate_counter(void)
{
	return duplicate_counter;
}

HIDDEN void R::reset_duplicate_counter(void)
{
	duplicate_counter = 0;
	return;
}
#endif

template <bool DEEP = true>
SEXP CXXR_deep_duplicate(SEXP s)
{
	if (!s)
		return nullptr;
	GCStackRoot<> srt(s);

#ifdef R_PROFILING
	++duplicate_counter;
#endif
	SEXP t = RObject::clone(s, DEEP);
	if (!t)
		return s;
#ifdef R_MEMORY_PROFILING
	if (RTRACE(s) && !(TYPEOF(s) == CLOSXP || TYPEOF(s) == BUILTINSXP ||
					   TYPEOF(s) == SPECIALSXP || TYPEOF(s) == PROMSXP ||
					   TYPEOF(s) == ENVSXP))
	{
		memtrace_report(s, t);
		SET_RTRACE(t, 1);
	}
#endif
	t->expose();
	return t;
}

SEXP Rf_duplicate(SEXP s)
{
	return CXXR_deep_duplicate<true>(s);
}

SEXP Rf_shallow_duplicate(SEXP s)
{
	return CXXR_deep_duplicate<false>(s);
}

SEXP Rf_lazy_duplicate(SEXP s)
{
	switch (TYPEOF(s))
	{
	case NILSXP:
	case SYMSXP:
	case ENVSXP:
	case SPECIALSXP:
	case BUILTINSXP:
	case EXTPTRSXP:
	case BCODESXP:
	case WEAKREFSXP:
	case CHARSXP:
	case PROMSXP:
		break;
	case CLOSXP:
	case LISTSXP:
	case LANGSXP:
	case DOTSXP:
	case EXPRSXP:
	case VECSXP:
	case LGLSXP:
	case INTSXP:
	case REALSXP:
	case CPLXSXP:
	case RAWSXP:
	case STRSXP:
	case S4SXP:
		ENSURE_NAMEDMAX(s);
		break;
	default:
		UNIMPLEMENTED_TYPE("lazy_duplicate()", s);
	}
	return s;
}

/*****************/

/* Detect cycles that would be created by assigning 'child' as a
   component of 's' in a complex assignment without duplicating
   'child'.  This is called quite often but almost always returns
   FALSE. Could be made more efficient, at least with partial
   inlining, but probably not worth while until it starts showing up
   significantly in profiling. Based on code from Michael Lawrence. */
Rboolean R_cycle_detected(SEXP s, SEXP child)
{
	if (s == child)
	{
		switch (TYPEOF(child))
		{
		case NILSXP:
		case SYMSXP:
		case ENVSXP:
		case SPECIALSXP:
		case BUILTINSXP:
		case EXTPTRSXP:
		case BCODESXP:
		case WEAKREFSXP:
			/* it's a cycle but one that is OK */
			return FALSE;
		default:
			return TRUE;
		}
	}
	if (ATTRIB(child))
	{
		if (R_cycle_detected(s, ATTRIB(child)))
			return TRUE;
	}
	if (Rf_isPairList(child))
	{
		SEXP el = child;
		while (el)
		{
			if (s == el || R_cycle_detected(s, CAR(el)))
				return TRUE;
			if (ATTRIB(el) && R_cycle_detected(s, ATTRIB(el)))
				return TRUE;
			el = CDR(el);
		}
	}
	else if (Rf_isVectorList(child))
	{
		for (int i = 0; i < Rf_length(child); i++)
			if (R_cycle_detected(s, VECTOR_ELT(child, i)))
				return TRUE;
	}
	return FALSE;
}

void Rf_copyVector(SEXP s, SEXP t)
{
	SEXPTYPE sT = TYPEOF(s), tT = TYPEOF(t);
	if (sT != tT)
		Rf_error(_("vector types do not match in 'copyVector()'"));
	R_xlen_t ns = XLENGTH(s), nt = XLENGTH(t);
	switch (sT)
	{
	case STRSXP:
		xcopyStringWithRecycle(s, t, 0, ns, nt);
		break;
	case LGLSXP:
		xcopyWithRecycle(LOGICAL(s), LOGICAL(t), 0, ns, nt);
		break;
	case INTSXP:
		xcopyWithRecycle(INTEGER(s), INTEGER(t), 0, ns, nt);
		break;
	case REALSXP:
		xcopyWithRecycle(REAL(s), REAL(t), 0, ns, nt);
		break;
	case CPLXSXP:
		xcopyWithRecycle(COMPLEX(s), COMPLEX(t), 0, ns, nt);
		break;
	case EXPRSXP:
		xcopyVectorWithRecycle(s, t, 0, ns, nt);
		break;
	case VECSXP:
		xcopyVectorWithRecycle(s, t, 0, ns, nt);
		break;
	case RAWSXP:
		xcopyWithRecycle(RAW(s), RAW(t), 0, ns, nt);
		break;
	default:
		UNIMPLEMENTED_TYPE("copyVector()", s);
	}
}

void Rf_copyListMatrix(SEXP s, SEXP t, Rboolean byrow)
{
	int nr = Rf_nrows(s), nc = Rf_ncols(s);
	R_xlen_t ns = R_xlen_t(nr) * nc;
	SEXP pt = t;
	if (byrow)
	{
		R_xlen_t NR = nr;
		GCStackRoot<> tmp(Rf_allocVector(STRSXP, ns));
		for (int i = 0; i < nr; i++)
			for (int j = 0; j < nc; j++)
			{
				SET_STRING_ELT(tmp, i + j * NR, Rf_duplicate(CAR(pt)));
				pt = CDR(pt);
				if (pt == R_NilValue)
					pt = t;
			}
		for (int i = 0; i < ns; i++)
		{
			SETCAR(s, STRING_ELT(tmp, i++));
			s = CDR(s);
		}
	}
	else
	{
		for (int i = 0; i < ns; i++)
		{
			SETCAR(s, Rf_duplicate(CAR(pt)));
			s = CDR(s);
			pt = CDR(pt);
			if (pt == R_NilValue)
				pt = t;
		}
	}
}

inline static SEXP VECTOR_ELT_LD(SEXP x, R_xlen_t i)
{
	return Rf_lazy_duplicate(VECTOR_ELT(x, i));
}

void Rf_copyMatrix(SEXP s, SEXP t, Rboolean byrow)
{
	int nr = Rf_nrows(s), nc = Rf_ncols(s);
	R_xlen_t nt = XLENGTH(t);

	if (byrow)
	{
		switch (TYPEOF(s))
		{
		case STRSXP:
			FILL_MATRIX_BYROW_ITERATE(0, nr, nc, nt)
			SET_STRING_ELT(s, didx, STRING_ELT(t, sidx));
			break;
		case LGLSXP:
			FILL_MATRIX_BYROW_ITERATE(0, nr, nc, nt)
			LOGICAL(s)[didx] = LOGICAL(t)[sidx];
			break;
		case INTSXP:
			FILL_MATRIX_BYROW_ITERATE(0, nr, nc, nt)
			INTEGER(s)[didx] = INTEGER(t)[sidx];
			break;
		case REALSXP:
			FILL_MATRIX_BYROW_ITERATE(0, nr, nc, nt)
			REAL(s)[didx] = REAL(t)[sidx];
			break;
		case CPLXSXP:
			FILL_MATRIX_BYROW_ITERATE(0, nr, nc, nt)
			COMPLEX(s)[didx] = COMPLEX(t)[sidx];
			break;
		case EXPRSXP:
			FILL_MATRIX_BYROW_ITERATE(0, nr, nc, nt)
			SET_XVECTOR_ELT(s, didx, VECTOR_ELT_LD(t, sidx));
			break;
		case VECSXP:
			FILL_MATRIX_BYROW_ITERATE(0, nr, nc, nt)
			SET_VECTOR_ELT(s, didx, VECTOR_ELT_LD(t, sidx));
			break;
		case RAWSXP:
			FILL_MATRIX_BYROW_ITERATE(0, nr, nc, nt)
			RAW(s)[didx] = RAW(t)[sidx];
			break;
		default:
			UNIMPLEMENTED_TYPE("copyMatrix()", s);
		}
	}
	else
		Rf_copyVector(s, t);
}

#define COPY_ELT_WITH_RECYCLE(TNAME, GETELT, SETELT)                                              \
	HIDDEN void                                                                                   \
		xcopy##TNAME##WithRecycle(SEXP dst, SEXP src, R_xlen_t dstart, R_xlen_t n, R_xlen_t nsrc) \
	{                                                                                             \
                                                                                                  \
		if (nsrc >= n)                                                                            \
		{ /* no recycle needed */                                                                 \
			for (R_xlen_t i = 0; i < n; i++)                                                      \
				SETELT(dst, dstart + i, GETELT(src, i));                                          \
			return;                                                                               \
		}                                                                                         \
		if (nsrc == 1)                                                                            \
		{                                                                                         \
			SEXP val = GETELT(src, 0);                                                            \
			for (R_xlen_t i = 0; i < n; i++)                                                      \
				SETELT(dst, dstart + i, val);                                                     \
			return;                                                                               \
		}                                                                                         \
                                                                                                  \
		/* recycle needed */                                                                      \
		R_xlen_t sidx = 0;                                                                        \
		for (R_xlen_t i = 0; i < n; i++, sidx++)                                                  \
		{                                                                                         \
			if (sidx == nsrc)                                                                     \
				sidx = 0;                                                                         \
			SETELT(dst, dstart + i, GETELT(src, sidx));                                           \
		}                                                                                         \
	}

COPY_ELT_WITH_RECYCLE(String, STRING_ELT, SET_STRING_ELT)	 /* xcopyStringWithRecycle */
COPY_ELT_WITH_RECYCLE(Vector, VECTOR_ELT_LD, SET_VECTOR_ELT) /* xcopyVectorWithRecycle */

#define FILL_ELT_WITH_RECYCLE(TNAME, GETELT, SETELT)                                             \
	HIDDEN void xfill##TNAME##MatrixWithRecycle(SEXP dst, SEXP src,                              \
												R_xlen_t dstart, R_xlen_t drows, R_xlen_t srows, \
												R_xlen_t cols, R_xlen_t nsrc)                    \
	{                                                                                            \
                                                                                                 \
		FILL_MATRIX_ITERATE(dstart, drows, srows, cols, nsrc)                                    \
		SETELT(dst, didx, GETELT(src, sidx));                                                    \
	}

FILL_ELT_WITH_RECYCLE(String, STRING_ELT, SET_STRING_ELT) /* xfillStringMatrixWithRecycle */
FILL_ELT_WITH_RECYCLE(Vector, VECTOR_ELT, SET_VECTOR_ELT) /* xfillVectorMatrixWithRecycle */

/* For duplicating before modifying attributes duplicate_attr tries to
   wrap a larger vector object with an ALTREP wrapper, and falls back
   to duplicate or shallow_duplicate if the object can't be
   wrapped. The size threshold used seems to be reaonable but could be
   tested more extensively. */
constexpr R_xlen_t WRAP_THRESHOLD = 64;
static SEXP duplicate_attr(SEXP x, bool deep)
{
	if (Rf_isVector(x) && XLENGTH(x) >= WRAP_THRESHOLD)
	{
		GCStackRoot<> val(R_tryWrap(x));
		if (val != x)
		{
			if (deep)
			{
				/* the spine has been duplicated; we could just do the values */
				SET_ATTRIB(val, Rf_duplicate(ATTRIB(val)));
			}
			return val;
		}
	}
	return deep ? Rf_duplicate(x) : Rf_shallow_duplicate(x);
}

SEXP R_shallow_duplicate_attr(SEXP x) { return duplicate_attr(x, false); }
SEXP R_duplicate_attr(SEXP x) { return duplicate_attr(x, true); }
