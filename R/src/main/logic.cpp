/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999--2018  The R Core Team.
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
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

#include <CXXR/BuiltInFunction.hpp>
#include <CXXR/BinaryFunction.hpp>
#include <CXXR/UnaryFunction.hpp>
#include <CXXR/FixedVector.hpp>
#include <CXXR/LogicalVector.hpp>
#include <CXXR/RawVector.hpp>
#include <CXXR/PairList.hpp>
#include <Localization.h>
#include <Defn.h>
#include <Internal.h>
#include <R_ext/Itermacros.h>

using namespace R;
using namespace CXXR;
using namespace VectorOps;

/* interval at which to check interrupts, a guess */
// constexpr R_xlen_t NINTERRUPT = 10000000;


static SEXP lunary(SEXP, SEXP, SEXP);
static SEXP lbinary(SEXP, SEXP, SEXP);
static SEXP binaryLogic(int code, SEXP s1, SEXP s2);
static SEXP binaryLogic2(int code, SEXP s1, SEXP s2);

// Functionality to support do_logic() :
namespace
{
#if CXXR_FALSE
	LogicalVector *binaryLogic(int opcode, const LogicalVector *l,
							   const LogicalVector *r)
	{
		switch (opcode)
		{
		case 1:
		{
			return applyBinaryOperator(
				[](Logical l, Logical r)
				{ return l && r; },
				GeneralBinaryAttributeCopier(),
				l, r);
		}
		case 2:
		{
			return applyBinaryOperator(
				[](Logical l, Logical r)
				{ return l || r; },
				GeneralBinaryAttributeCopier(),
				l, r);
		}
		}
		return nullptr; // -Wall
	}

	RawVector *bitwiseBinary(int opcode, const RawVector *l, const RawVector *r)
	{
		switch (opcode)
		{
		case 1:
		{
			return applyBinaryOperator(
				[](Rbyte l, Rbyte r) -> Rbyte
				{ return l & r; },
				GeneralBinaryAttributeCopier(),
				l, r);
		}
		case 2:
		{
			return applyBinaryOperator(
				[](Rbyte l, Rbyte r) -> Rbyte
				{ return l | r; },
				GeneralBinaryAttributeCopier(),
				l, r);
		}
		}
		return nullptr; // -Wall
	}

	RObject *lbinary(const BuiltInFunction *op, RObject *x, RObject *y)
	{

		if (x && x->sexptype() == RAWSXP && y && y->sexptype() == RAWSXP)
		{
		}
		else if (!(Rf_isNull(x) || Rf_isNumber(x)) || !(Rf_isNull(y) || Rf_isNumber(y)))
			Rf_error(_("operations are possible only for numeric, logical or complex types"));

		R_xlen_t nx = Rf_xlength(x), ny = Rf_xlength(y);

		checkOperandsConformable(
			SEXP_downcast<VectorBase *>(x), SEXP_downcast<VectorBase *>(y));

		if (nx > 0 && ny > 0)
		{
			/* logical binary : "&" or "|" */
			if (x && x->sexptype() == RAWSXP && y && y->sexptype() == RAWSXP)
			{
				// Bitwise operations:
				RawVector *vl = SEXP_downcast<RawVector *>(x);
				RawVector *vr = SEXP_downcast<RawVector *>(y);
				return bitwiseBinary(op->variant(), vl, vr);
			}
			GCStackRoot<LogicalVector> vl;
			GCStackRoot<LogicalVector> vr;
			if (Rf_isNull(x))
				vl = SEXP_downcast<LogicalVector *>(
					Rf_allocVector(SEXPTYPE::LGLSXP, 0));
			else // Rf_isNumeric(x)
				vl = SEXP_downcast<LogicalVector *>(Rf_coerceVector(x, LGLSXP));
			if (Rf_isNull(y))
				vr = SEXP_downcast<LogicalVector *>(
					Rf_allocVector(SEXPTYPE::LGLSXP, 0));
			else // Rf_isNumeric(x)
				vr = SEXP_downcast<LogicalVector *>(Rf_coerceVector(y, LGLSXP));
			return binaryLogic(op->variant(), vl, vr);
		}
		else
		{ // nx == 0 || ny == 0
			GCStackRoot<> val(Rf_allocVector((isRaw(x) && isRaw(y)) ? RAWSXP : LGLSXP, 0));
			GeneralBinaryAttributeCopier::copyAttributes(
				SEXP_downcast<VectorBase *>(val.get()),
				SEXP_downcast<VectorBase *>(x),
				SEXP_downcast<VectorBase *>(y));
			return val;
		}
	}

	//lunary
	RObject *lnot(RObject *arg)
	{
		if (arg && arg->sexptype() == RAWSXP)
		{
			// Bit inversion:
			return applyUnaryOperator([](Rbyte x)
									  { return Rbyte(~x); },
									  CopyAllAttributes(),
									  SEXP_downcast<RawVector *>(arg));
		}
		else if (arg && arg->sexptype() == LGLSXP)
		{
			// Logical negation:
			return applyUnaryOperator(
				[](Logical x)
				{ return !x; },
				CopyAllAttributes(),
				SEXP_downcast<LogicalVector *>(arg));
		}
		else if (!Rf_isNumber(arg))
		{
			if (Rf_length(arg) == 0U) // For back-compatibility
				return LogicalVector::create(0);
			Rf_error(_("invalid argument type"));
		}
		else
		{
			// Logical negation:
			GCStackRoot<LogicalVector>
				lv(SEXP_downcast<LogicalVector *>(Rf_coerceVector(arg, LGLSXP)));
			return applyUnaryOperator(
				[](Logical x)
				{ return !x; },
				// Note: in other cases all attributes are copied.
				CopyLayoutAttributes(),
				lv.get());
		}
	}
#endif
} // anonymous namespace

/* & | ! */
HIDDEN SEXP do_logic(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP arg1 = CAR(args); //, arg2 = CADR(args)
    Rboolean attr1 = (Rboolean) (ATTRIB(arg1) != R_NilValue);
    if (attr1 || ATTRIB(CADR(args)) != R_NilValue) {
	SEXP ans;
	if (DispatchGroup("Ops", call, op, args, env, &ans))
	    return ans;
    }
    /* The above did dispatch to valid S3/S4 methods, including those with
     * "wrong" number of arguments.
     * Now require binary calls to `&` and `|`  or unary calls to `!` : */
    checkArity(op, args);

    if (CDR(args) == R_NilValue) { // one argument  <==>  !(arg1)
	if (!attr1 && IS_SCALAR(arg1, LGLSXP)) {
	    /* directly handle '!' operator for simple logical scalars. */
	    int v = SCALAR_LVAL(arg1);
	    return ScalarLogical(v == NA_LOGICAL ? v : ! v);
	}
	return lunary(call, op, arg1);
    }
    // else : two arguments
    return lbinary(call, op, args);
}

static SEXP lbinary(SEXP call, SEXP op, SEXP args)
{
/* logical binary : "&" or "|" */
    SEXP
	x = CAR(args),
	y = CADR(args);

    if (Rf_isRaw(x) && Rf_isRaw(y)) {
    }
    else if ( !(Rf_isNull(x) || isNumber(x)) ||
	      !(Rf_isNull(y) || isNumber(y)) )
	errorcall(call,
		  _("operations are possible only for numeric, logical or complex types"));

    R_xlen_t
	nx = xlength(x),
	ny = xlength(y);
    Rboolean
	xarray = isArray(x),
	yarray = isArray(y),
	xts = isTs(x),
	yts = isTs(y);
    SEXP dims, xnames, ynames;

	checkOperandsConformable(SEXP_downcast<VectorBase *>(x), SEXP_downcast<VectorBase *>(y));

    if (xarray || yarray) {
	if (xarray && yarray) {
	    PROTECT(dims = getAttrib(x, R_DimSymbol));
	}
	else if (xarray && (ny != 0 || nx == 0)) {
	    PROTECT(dims = getAttrib(x, R_DimSymbol));
	}
	else if (yarray && (nx != 0 || ny == 0)) {
	    PROTECT(dims = getAttrib(y, R_DimSymbol));
	} else
	    PROTECT(dims = R_NilValue);

	PROTECT(xnames = getAttrib(x, R_DimNamesSymbol));
	PROTECT(ynames = getAttrib(y, R_DimNamesSymbol));
    }
    else {
	PROTECT(dims = R_NilValue);
	PROTECT(xnames = getAttrib(x, R_NamesSymbol));
	PROTECT(ynames = getAttrib(y, R_NamesSymbol));
    }

    SEXP klass = nullptr, tsp = nullptr; // -Wall
    if (xts || yts) {
	if (xts && yts) {
	    /* could check ts conformance here */
	    PROTECT(tsp = getAttrib(x, R_TspSymbol));
	    PROTECT(klass = getAttrib(x, R_ClassSymbol));
	}
	else if (xts) {
	    PROTECT(tsp = getAttrib(x, R_TspSymbol));
	    PROTECT(klass = getAttrib(x, R_ClassSymbol));
	}
	else /*(yts)*/ {
	    PROTECT(tsp = getAttrib(y, R_TspSymbol));
	    PROTECT(klass = getAttrib(y, R_ClassSymbol));
	}
    }

  if (nx > 0 && ny > 0) {
	if(((nx > ny) ? nx % ny : ny % nx) != 0) // mismatch
            warningcall(call, _("longer object length is not a multiple of shorter object length"));

	if (Rf_isRaw(x) && Rf_isRaw(y)) {
	    x = binaryLogic2(PRIMVAL(op), x, y);
	}
	else {
	    if(Rf_isNull(x))
		x = SETCAR(args, allocVector(LGLSXP, 0));
	    else // isNumeric(x)
		x = SETCAR(args, coerceVector(x, LGLSXP));
	    if(Rf_isNull(y))
		y = SETCAR(args, allocVector(LGLSXP, 0));
	    else // isNumeric(y)
		y = SETCADR(args, coerceVector(y, LGLSXP));
	    x = binaryLogic(PRIMVAL(op), x, y);
	}
    } else { // nx == 0 || ny == 0
	x = allocVector((Rf_isRaw(x) && Rf_isRaw(y)) ? RAWSXP : LGLSXP, 0);
    }

    PROTECT(x);
    if (dims != R_NilValue) {
	setAttrib(x, R_DimSymbol, dims);
	if(xnames != R_NilValue)
	    setAttrib(x, R_DimNamesSymbol, xnames);
	else if(ynames != R_NilValue)
	    setAttrib(x, R_DimNamesSymbol, ynames);
    }
    else {
	if(xnames != R_NilValue && XLENGTH(x) == XLENGTH(xnames))
	    setAttrib(x, R_NamesSymbol, xnames);
	else if(ynames != R_NilValue && XLENGTH(x) == XLENGTH(ynames))
	    setAttrib(x, R_NamesSymbol, ynames);
    }

    if (xts || yts) {
	setAttrib(x, R_TspSymbol, tsp);
	setAttrib(x, R_ClassSymbol, klass);
	UNPROTECT(2);
    }
    UNPROTECT(4);
    return x;
}

static SEXP lunary(SEXP call, SEXP op, SEXP arg)
{
    SEXP x, dim, dimnames, names;
    R_xlen_t i, len;

    len = XLENGTH(arg);
    if (!isLogical(arg) && !isNumber(arg) && !isRaw(arg)) {
	/* For back-compatibility */
	if (!len) return allocVector(LGLSXP, 0);
	errorcall(call, _("invalid argument type"));
    }
    if (isLogical(arg) || isRaw(arg))
	// copy all attributes in this case
	x = PROTECT(shallow_duplicate(arg));
    else {
	x = PROTECT(allocVector(isRaw(arg) ? RAWSXP : LGLSXP, len));
	PROTECT(names = getAttrib(arg, R_NamesSymbol));
	PROTECT(dim = getAttrib(arg, R_DimSymbol));
	PROTECT(dimnames = getAttrib(arg, R_DimNamesSymbol));
	if(names != R_NilValue) setAttrib(x, R_NamesSymbol, names);
	if(dim != R_NilValue) setAttrib(x, R_DimSymbol, dim);
	if(dimnames != R_NilValue) setAttrib(x, R_DimNamesSymbol, dimnames);
	UNPROTECT(3);
    }
    switch(TYPEOF(arg)) {
    case LGLSXP:
	{
	    int *px = LOGICAL(x);
	    const int *parg = LOGICAL_RO(arg);
	    for (i = 0; i < len; i++) {
//	        if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
		int v = parg[i];
		px[i] = (v == NA_LOGICAL) ? NA_LOGICAL : v == 0;
	    }
	}
	break;
    case INTSXP:
	{
	    int *px = LOGICAL(x);
	    const int *parg = INTEGER_RO(arg);
	    for (i = 0; i < len; i++) {
//	        if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
		int v = parg[i];
		px[i] = (v == NA_INTEGER) ? NA_LOGICAL : v == 0;
	    }
	}
	break;
    case REALSXP:
	{
	    int *px = LOGICAL(x);
	    const double *parg = REAL_RO(arg);
	    for (i = 0; i < len; i++){
//	        if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
		double v = parg[i];
		px[i] = ISNAN(v) ? NA_LOGICAL : v == 0;
	    }
	}
	break;
    case CPLXSXP:
	{
	    int *px = LOGICAL(x);
	    const Rcomplex *parg = COMPLEX_RO(arg);
	    for (i = 0; i < len; i++) {
//	        if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
		Rcomplex v = parg[i];
		px[i] = (ISNAN(v.r) || ISNAN(v.i))
		    ? NA_LOGICAL : (v.r == 0. && v.i == 0.);
	    }
	}
	break;
    case RAWSXP:
	{
	    Rbyte *px = RAW(x);
	    const Rbyte *parg = RAW_RO(arg);
	    for (i = 0; i < len; i++) {
//	        if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
		px[i] = 0xFF ^ parg[i];
	    }
	}
	break;
    default:
	UNIMPLEMENTED_TYPE("lunary()", arg);
    }
    UNPROTECT(1);
    return x;
}

/* && || */
HIDDEN SEXP do_logic2(SEXP call, SEXP op, SEXP args, SEXP env)
{
/*  &&	and  ||	 */
    SEXP s1, s2;
    int x1, x2;
    int ans = FALSE;

    if (length(args) != 2)
	error(_("'%s' operator requires 2 arguments"), PRIMVAL(op) == 1 ? "&&" : "||");

    s1 = CAR(args);
    s2 = CADR(args);
    PROTECT(s1 = eval(s1, env));
    if (!isNumber(s1))
	errorcall(call, _("invalid '%s' type in 'x %s y'"), "x", PRIMVAL(op) == 1 ? "&&" : "||");

    x1 = asLogical2(s1, /*checking*/ 1, call, env);
    UNPROTECT(1); /* s1 */

#define get_2nd                                                  \
	PROTECT(s2 = eval(s2, env));                                 \
	if (!isNumber(s2))                                           \
		errorcall(call, _("invalid '%s' type in 'x %s y'"), "y", \
				  PRIMVAL(op) == 1 ? "&&" : "||");               \
	x2 = asLogical2(s2, 1, call, env);                           \
	UNPROTECT(1); /* s2 */

	switch (PRIMVAL(op)) {
    case 1: /* && */
	if (x1 == FALSE)
	    ans = FALSE;
	else {
	    get_2nd;
	    if (x1 == NA_LOGICAL)
		ans = (x2 == NA_LOGICAL || x2) ? NA_LOGICAL : x2;
	    else /* x1 == TRUE */
		ans = x2;
	}
	break;
    case 2: /* || */
	if (x1 == TRUE)
	    ans = TRUE;
	else {
	    get_2nd;
	    if (x1 == NA_LOGICAL)
		ans = (x2 == NA_LOGICAL || !x2) ? NA_LOGICAL : x2;
	    else /* x1 == FALSE */
		ans = x2;
	}
    }
    return ScalarLogical(ans);
}

static SEXP binaryLogic(int code, SEXP s1, SEXP s2)
{
    R_xlen_t i, n, n1, n2, i1, i2;
    int x1, x2;
    SEXP ans;

    n1 = XLENGTH(s1);
    n2 = XLENGTH(s2);
    n = (n1 > n2) ? n1 : n2;
    if (n1 == 0 || n2 == 0) {
	ans = allocVector(LGLSXP, 0);
	return ans;
    }
    ans = allocVector(LGLSXP, n);

    int *px1 = LOGICAL(s1);
    int *px2 = LOGICAL(s2);
    int *pa = LOGICAL(ans);

    switch (code) {
    case 1:		/* & : AND */
	MOD_ITERATE2(n, n1, n2, i, i1, i2, {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    x1 = px1[i1];
	    x2 = px2[i2];
	    if (x1 == 0 || x2 == 0)
		pa[i] = 0;
	    else if (x1 == NA_LOGICAL || x2 == NA_LOGICAL)
		pa[i] = NA_LOGICAL;
	    else
		pa[i] = 1;
	});
	break;
    case 2:		/* | : OR */
	MOD_ITERATE2(n, n1, n2, i, i1, i2, {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    x1 = px1[i1];
	    x2 = px2[i2];
	    if ((x1 != NA_LOGICAL && x1) || (x2 != NA_LOGICAL && x2))
		pa[i] = 1;
	    else if (x1 == 0 && x2 == 0)
		pa[i] = 0;
	    else
		pa[i] = NA_LOGICAL;
	});
	break;
    case 3:
	error(_("Unary operator '!' called with two arguments"));
	break;
    }
    return ans;
}

// called only when both  s1 and s2 are  RAWSXP
static SEXP binaryLogic2(int code, SEXP s1, SEXP s2)
{
    R_xlen_t i, n, n1, n2, i1, i2;
    Rbyte x1, x2;
    SEXP ans;

    n1 = XLENGTH(s1);
    n2 = XLENGTH(s2);
    n = (n1 > n2) ? n1 : n2;
    if (n1 == 0 || n2 == 0) {
	ans = allocVector(RAWSXP, 0);
	return ans;
    }
    ans = allocVector(RAWSXP, n);

    switch (code) {
    case 1:		/* & : AND */
	MOD_ITERATE2(n, n1, n2, i, i1, i2, {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    x1 = RAW(s1)[i1];
	    x2 = RAW(s2)[i2];
	    RAW(ans)[i] = x1 & x2;
	});
	break;
    case 2:		/* | : OR */
	MOD_ITERATE2(n, n1, n2, i, i1, i2, {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    x1 = RAW(s1)[i1];
	    x2 = RAW(s2)[i2];
	    RAW(ans)[i] = x1 | x2;
	});
	break;
    }
    return ans;
}

constexpr int _OP_ALL = 1;
constexpr int _OP_ANY = 2;

static Logical checkValues(int op, int na_rm, SEXP x, R_xlen_t n)
{
    R_xlen_t i;
    int has_na = 0;
    int *px = LOGICAL(x);
    for (i = 0; i < n; i++) {
//	if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	int xi = px[i];
	if (!na_rm && xi == NA_LOGICAL) has_na = 1;
	else {
	    if (xi == TRUE && op == _OP_ANY) return true;
	    if (xi == FALSE && op == _OP_ALL) return false;
	}
    }
    switch (op) {
    case _OP_ANY:
        return has_na ? Logical::NA() : false;
    case _OP_ALL:
        return has_na ? Logical::NA() : true;
    default:
	error(_("bad operator value for 'do_logic3()' function"));
    }
    return Logical::NA(); /* -Wall */
}

/* all, any */
HIDDEN SEXP do_logic3(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, s, t, call2;
    int narm, has_na = 0;
    /* initialize for behavior on empty vector
       all(logical(0)) -> TRUE
       any(logical(0)) -> FALSE
     */
    Logical val = PRIMVAL(op) == _OP_ALL ? true : false;

    PROTECT(args = fixup_NaRm(args));
    PROTECT(call2 = shallow_duplicate(call));
    R_args_enable_refcnt(args);
    SETCDR(call2, args);

    if (DispatchGroup("Summary", call2, op, args, env, &ans)) {
	UNPROTECT(2);
	SETCDR(call2, R_NilValue); /* clear refcnt on args */
	R_try_clear_args_refcnt(args);
	return ans;
    }
    SETCDR(call2, R_NilValue); /* clear refcnt on args */
    R_try_clear_args_refcnt(args);

    ans = matchArgExact(R_NaRmSymbol, &args);
    narm = asLogical2(ans, /*warn_level*/ 1, call, env);

    for (s = args; s != R_NilValue; s = CDR(s)) {
	t = CAR(s);
	/* Avoid memory waste from coercing empty inputs, and also
	   avoid warnings with empty lists coming from sapply */
	if(xlength(t) == 0) continue;
	/* coerceVector protects its argument so this actually works
	   just fine */
	if (TYPEOF(t) != LGLSXP) {
	    /* Coercion of integers seems reasonably safe, but for
	       other types it is more often than not an error.
	       One exception is perhaps the result of lapply, but
	       then sapply was often what was intended. */
	    if(TYPEOF(t) != INTSXP)
		warningcall(call,
			    _("coercing argument of type '%s' to logical"),
			    type2char(TYPEOF(t)));
	    t = coerceVector(t, LGLSXP);
	}
	val = checkValues(PRIMVAL(op), narm, t, XLENGTH(t));
	if (!val.isNA()) {
	    if ((PRIMVAL(op) == _OP_ANY && val.isTrue())
		|| (PRIMVAL(op) == _OP_ALL && val.isFalse())) {
		has_na = 0;
		break;
	    }
	} else has_na = 1;
    }
    UNPROTECT(2);
    return LogicalVector::createScalar(has_na ? Logical::NA() : val);
}
