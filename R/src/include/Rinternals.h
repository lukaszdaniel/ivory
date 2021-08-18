/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999--2021  The R Core Team.
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
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

/* This file is installed and available to packages, but only a small
   part of the contents is within the API.  See chapter 6 of 'Writing
   R Extensions'.
*/

/** @file Rinternals.h
 * @brief (As described in 'Writing R Extensions'.)
 */

#ifndef R_INTERNALS_H_
#define R_INTERNALS_H_

#ifdef __cplusplus
#include <cstdio>
#include <climits>
#include <limits>
#include <cstddef>
#include <string>
#include <iostream>
#else
#include <stdio.h>
#include <limits.h> /* for INT_MAX */
#include <stddef.h> /* for ptrdiff_t, which is required by C99 */
#endif

#ifdef __cplusplus
extern "C" {
#endif

#include <R_ext/Arith.h>
#include <R_ext/Boolean.h>
#include <R_ext/Complex.h>
#include <R_ext/Error.h>
#include <R_ext/Visibility.h>  // includes NORET macro
#include <R_ext/Memory.h>
#include <R_ext/Utils.h>
#include <R_ext/Print.h>
#include <R_ext/Rdynload.h> // for DL_FUNC

#include <R_ext/libextern.h>
#include <CXXR/RTypes.hpp> // for RObject, SEXPREC, Rbyte, R_*_t, 

/* both config.h and Rconfig.h set SIZEOF_SIZE_T, but Rconfig.h is
   skipped if config.h has already been included. */
#ifndef R_CONFIG_H
#include <Rconfig.h>
#endif

#if (SIZEOF_SIZE_T > 4)
#define LONG_VECTOR_SUPPORT
#endif

#ifndef TESTING_WRITE_BARRIER
#define TESTING_WRITE_BARRIER
#endif

#ifdef TESTING_WRITE_BARRIER
/* define inline-able functions */
#define STRICT_TYPECHECK
#define CATCH_ZERO_LENGTH_ACCESS
#endif

/* Fundamental Data Types:  These are largely Lisp
 * influenced structures, with the exception of LGLSXP,
 * INTSXP, REALSXP, CPLXSXP and STRSXP which are the
 * element types for S-like data objects.
 *
 *			--> TypeTable[] in ../main/util.cpp for  typeof()
 */

/*  These exact numeric values are seldom used, but they are, e.g., in
 *  ../main/subassign.cpp, and they are serialized.
*/

#include <CXXR/SEXPTYPE.hpp>


/* CXXR uses reference counting mechanism only */
#ifndef SWITCH_TO_REFCNT
#define SWITCH_TO_REFCNT
#endif

#ifndef COMPUTE_REFCNT_VALUES
#define COMPUTE_REFCNT_VALUES
#endif
#ifndef ADJUST_ENVIR_REFCNTS
#define ADJUST_ENVIR_REFCNTS
#endif

const char *R_CHAR(SEXP x);
#define CHAR(x)		R_CHAR(x)

/* Various tests with macro versions in the internal headers */
Rboolean Rf_isNull(SEXP s);
Rboolean Rf_isSymbol(SEXP s);
Rboolean Rf_isLogical(SEXP s);
Rboolean Rf_isReal(SEXP s);
Rboolean Rf_isComplex(SEXP s);
Rboolean Rf_isExpression(SEXP s);
Rboolean Rf_isEnvironment(SEXP s);
Rboolean Rf_isString(SEXP s);
Rboolean Rf_isObject(SEXP s);
Rboolean Rf_isRaw(SEXP s);


// =====
// These macros are required by stringi package ( version < 1.5.4)
#ifndef COMPILING_IVORY
#ifndef USING_R
#ifndef IS_BYTES
#define IS_BYTES(x) (LEVELS(x) & 2)
#endif
#ifndef IS_LATIN1
#define IS_LATIN1(x) (LEVELS(x) & 4)
#endif
#ifndef IS_ASCII
#define IS_ASCII(x) (LEVELS(x) & 64)
#endif
#ifndef IS_UTF8
#define IS_UTF8(x) (LEVELS(x) & 8)
#endif
#ifndef IS_CACHED
#define IS_CACHED(x) (LEVELS(x) & 32)
#endif
#ifndef ENC_KNOWN
#define ENC_KNOWN(x) (LEVELS(x) & 12)
#endif
#endif
#endif
// =====

#define IS_SIMPLE_SCALAR(x, type) \
    (IS_SCALAR(x, type) && ATTRIB(x) == R_NilValue)

#define INCREMENT_NAMED(x) do { } while (0)
#define DECREMENT_NAMED(x) do { } while (0)

/* Macros for some common idioms. */
#define MAYBE_SHARED(x) (REFCNT(x) > 1)
#define NO_REFERENCES(x) (REFCNT(x) == 0)
#define MAYBE_REFERENCED(x) (! NO_REFERENCES(x))
#define NOT_SHARED(x) (! MAYBE_SHARED(x))

/* Accessor functions.  Many are declared using () to avoid the macro
   definitions in the internal headers.
   The function STRING_ELT is used as an argument to arrayAssign even
   if the macro version is in use.
*/

/* General Cons Cell Attributes */
SEXP ATTRIB(SEXP x);
int  OBJECT(SEXP x);
int  MARK(SEXP x);
SEXPTYPE TYPEOF(SEXP x);
int  NAMED(SEXP x);
int  REFCNT(SEXP x);
void SET_ATTRIB(SEXP x, SEXP v);
void DUPLICATE_ATTRIB(SEXP to, SEXP from);
void SHALLOW_DUPLICATE_ATTRIB(SEXP to, SEXP from);
void MARK_NOT_MUTABLE(SEXP x);

/* S4 object testing */
int IS_S4_OBJECT(SEXP x);

/* Vector Access Functions */
int  (LENGTH)(SEXP x);
R_xlen_t (XLENGTH)(SEXP x);
R_xlen_t  (TRUELENGTH)(SEXP x);
int  (IS_LONG_VEC)(SEXP x);
int  LEVELS(SEXP x);

int  *LOGICAL(SEXP x);
int  *INTEGER(SEXP x);
Rbyte *RAW(SEXP x);
double *REAL(SEXP x);
Rcomplex *COMPLEX(SEXP x);
const int  *LOGICAL_RO(SEXP x);
const int  *INTEGER_RO(SEXP x);
const Rbyte *RAW_RO(SEXP x);
const double *REAL_RO(SEXP x);
const Rcomplex *COMPLEX_RO(SEXP x);
SEXP VECTOR_ELT(SEXP x, R_xlen_t i);
SEXP XVECTOR_ELT(SEXP x, R_xlen_t i);
void SET_STRING_ELT(SEXP x, R_xlen_t i, SEXP v);
SEXP SET_VECTOR_ELT(SEXP x, R_xlen_t i, SEXP v);
SEXP SET_XVECTOR_ELT(SEXP x, R_xlen_t i, SEXP v);
SEXP *STRING_PTR(SEXP x);
const SEXP *STRING_PTR_RO(SEXP x);
NORET SEXP *VECTOR_PTR(SEXP x);

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

/* List Access Functions */
/* These also work for ... objects */
#define CONS(a, b)	Rf_cons((a), (b))		/* data lists */
#define LCONS(a, b)	Rf_lcons((a), (b))		/* language lists */

SEXP TAG(SEXP e);
SEXP CDR(SEXP e);
SEXP CAAR(SEXP e);
SEXP CDAR(SEXP e);
SEXP CADR(SEXP e);
SEXP CDDR(SEXP e);
SEXP CDDDR(SEXP e);
SEXP CADDR(SEXP e);
SEXP CD4R(SEXP e);
SEXP CADDDR(SEXP e);
SEXP CAD3R(SEXP e);
SEXP CAD4R(SEXP e);
SEXP CAD5R(SEXP e);
int  MISSING(SEXP x);
void SET_TAG(SEXP x, SEXP y);
SEXP SETCAR(SEXP x, SEXP y);
SEXP SETCDR(SEXP x, SEXP y);
SEXP SETCADR(SEXP x, SEXP y);
SEXP SETCADDR(SEXP x, SEXP y);
SEXP SETCADDDR(SEXP x, SEXP y);
SEXP SETCAD4R(SEXP e, SEXP y);

/* S4Object Access Functions */
SEXP S4TAG(SEXP e);
void SET_S4TAG(SEXP x, SEXP y);

/* Closure Access Functions */
SEXP FORMALS(SEXP x);
SEXP BODY(SEXP x);
SEXP CLOENV(SEXP x);
int  RDEBUG(SEXP x);
int  RSTEP(SEXP x);
int  RTRACE(SEXP x);
void SET_RDEBUG(SEXP x, int v);
void SET_RSTEP(SEXP x, int v);
void SET_RTRACE(SEXP x, int v);
void SET_FORMALS(SEXP x, SEXP v);
void SET_BODY(SEXP x, SEXP v);
void SET_CLOENV(SEXP x, SEXP v);

/* Symbol Access Functions */
SEXP PRINTNAME(SEXP x);
SEXP SYMVALUE(SEXP x);
SEXP INTERNAL(SEXP x);
int  DDVAL(SEXP x);

/* Environment Access Functions */
SEXP FRAME(SEXP x);
SEXP ENCLOS(SEXP x);
SEXP HASHTAB(SEXP x);
int  ENVFLAGS(SEXP x);
int  ENV_RDEBUG(SEXP x);

/* Promise Access Functions */
SEXP PRCODE(SEXP x);
SEXP PRENV(SEXP x);
SEXP PRVALUE(SEXP x);
int  PRSEEN(SEXP x);

/* External pointer access macros */
SEXP EXTPTR_PROT(SEXP);
SEXP EXTPTR_TAG(SEXP);
void *EXTPTR_PTR(SEXP);

/* Pointer Protection and Unprotection */
/* We sometimes need to coerce a protected value and place the new
   coerced value under protection.  For these cases PROTECT_WITH_INDEX
   saves an index of the protection location that can be used to
   replace the protected value using REPROTECT. */
typedef int PROTECT_INDEX;
#ifdef COMPILING_IVORY
#define PROTECT(s)	CXXR_protect(s, __func__)
#define UNPROTECT(n)	CXXR_unprotect(n, __func__)
#define UNPROTECT_PTR(s)	CXXR_unprotect_ptr(s, __func__)
#define PROTECT_WITH_INDEX(x, iptr) CXXR_ProtectWithIndex(x, iptr, __func__)
#define REPROTECT(x, iptr) CXXR_Reprotect(x, iptr, __func__)
#else
#define PROTECT(s)	Rf_protect(s)
#define UNPROTECT(n)	Rf_unprotect(n)
#define UNPROTECT_PTR(s)	Rf_unprotect_ptr(s)
#define PROTECT_WITH_INDEX(x, i) R_ProtectWithIndex(x, i)
#define REPROTECT(x, i) R_Reprotect(x, i)
#endif

/* Evaluation Environment */
extern SEXP R_GlobalEnv;	    /* The "global" environment */

extern SEXP R_EmptyEnv;	    /* An empty environment at the root of the
				    	environment tree */
extern SEXP R_BaseEnv;	    /* The base environment; formerly R_NilValue */
LibExtern SEXP R_BaseNamespace;    /* The (fake) namespace for base */
LibExtern SEXP	R_NamespaceRegistry;/* Registry for registered namespaces */

LibExtern SEXP	R_Srcref;           /* Current srcref, for debuggers */

/* Special Values */
extern SEXP R_NilValue;	    /* The nil object */
extern SEXP R_UnboundValue;	    /* Unbound marker */
extern SEXP R_MissingArg;	    /* Missing argument marker */
extern SEXP R_InBCInterpreter;  /* To be found in BC interp. state (marker) */
extern SEXP R_CurrentExpression; /* Use current expression (marker) */
extern SEXP R_RestartToken;     /* Marker for restarted function calls */

/* Symbol Table Shortcuts */
extern SEXP R_AsCharacterSymbol;/* "as.character" */
extern SEXP R_baseSymbol; // <-- backcompatible version of:
extern SEXP R_BaseSymbol;	// "base"
extern SEXP R_BraceSymbol;	    /* "{" */
extern SEXP R_Bracket2Symbol;   /* "[[" */
extern SEXP R_BracketSymbol;    /* "[" */
extern SEXP R_ClassSymbol;	    /* "class" */
extern SEXP R_DeviceSymbol;	    /* ".Device" */
extern SEXP R_DimNamesSymbol;   /* "dimnames" */
extern SEXP R_DimSymbol;	    /* "dim" */
extern SEXP R_DollarSymbol;	    /* "$" */
extern SEXP R_DotsSymbol;	    /* "..." */
extern SEXP R_DoubleColonSymbol;// "::"
extern SEXP R_DropSymbol;	    /* "drop" */
extern SEXP R_EvalSymbol;	    /* "eval" */
extern SEXP R_FunctionSymbol;   /* "function" */
extern SEXP R_LastvalueSymbol;  /* ".Last.value" */
extern SEXP R_LevelsSymbol;	    /* "levels" */
extern SEXP R_ModeSymbol;	    /* "mode" */
extern SEXP R_NaRmSymbol;	    /* "na.rm" */
extern SEXP R_NameSymbol;	    /* "name" */
extern SEXP R_NamesSymbol;	    /* "names" */
extern SEXP R_NamespaceEnvSymbol;// ".__NAMESPACE__."
extern SEXP R_PackageSymbol;    /* "package" */
extern SEXP R_PreviousSymbol;   /* "previous" */
extern SEXP R_QuoteSymbol;	    /* "quote" */
extern SEXP R_RowNamesSymbol;   /* "row.names" */
extern SEXP R_SeedsSymbol;	    /* ".Random.seed" */
extern SEXP R_SortListSymbol;   /* "sort.list" */
extern SEXP R_SourceSymbol;	    /* "source" */
extern SEXP R_SpecSymbol;	// "spec"
extern SEXP R_TripleColonSymbol;// ":::"
extern SEXP R_TspSymbol;	    /* "tsp" */

extern SEXP R_dot_defined;      /* ".defined" */
extern SEXP R_dot_Method;       /* ".Method" */
extern SEXP R_dot_packageName;// ".packageName"
extern SEXP R_dot_target;       /* ".target" */
extern SEXP R_dot_Generic;      /* ".Generic" */

/* Missing Values - others from Arith.h */
#define NA_STRING	R_NaString
extern SEXP R_NaString;	    /* NA_STRING as a CHARSXP */
extern SEXP R_BlankString;	    /* "" as a CHARSXP */
LibExtern SEXP R_BlankScalarString;/* "" as a STRSXP */

/* srcref related functions */
SEXP R_GetCurrentSrcref(int);
SEXP R_GetSrcFilename(SEXP);

/*--- FUNCTIONS ------------------------------------------------------ */

/* Type Coercions of all kinds */

SEXP Rf_asChar(SEXP);
SEXP Rf_coerceVector(SEXP, SEXPTYPE);
SEXP Rf_PairToVectorList(SEXP x);
SEXP Rf_VectorToPairList(SEXP x);
SEXP Rf_asCharacterFactor(SEXP x);
int Rf_asLogical(SEXP x);
int Rf_asInteger(SEXP x);
double Rf_asReal(SEXP x);
Rcomplex Rf_asComplex(SEXP x);


#ifndef R_ALLOCATOR_TYPE
#define R_ALLOCATOR_TYPE
typedef struct R_allocator R_allocator_t;
#endif


/* Other Internally Used Functions, excluding those which are inline-able*/

char *Rf_acopy_string(const char *);
SEXP Rf_alloc3DArray(SEXPTYPE, int, int, int);
SEXP Rf_allocArray(SEXPTYPE, SEXP);
SEXP Rf_allocMatrix(SEXPTYPE, int, int);
SEXP Rf_allocList(int);
SEXP Rf_allocS4Object(void);
SEXP Rf_allocSExp(SEXPTYPE);
SEXP Rf_allocVector3(SEXPTYPE, R_xlen_t, R_allocator_t *);
R_xlen_t Rf_any_duplicated(SEXP x, Rboolean from_last);
R_xlen_t Rf_any_duplicated3(SEXP x, SEXP incomp, Rboolean from_last);
SEXP Rf_applyClosure(SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP Rf_classgets(SEXP, SEXP);
SEXP Rf_cons(SEXP, SEXP);
void Rf_copyMatrix(SEXP, SEXP, Rboolean);
void Rf_copyListMatrix(SEXP, SEXP, Rboolean);
void Rf_copyMostAttrib(SEXP, SEXP);
void Rf_copyVector(SEXP, SEXP);
void Rf_defineVar(SEXP symbol, SEXP value, SEXP rho);
SEXP Rf_dimgets(SEXP, SEXP);
SEXP Rf_dimnamesgets(SEXP, SEXP);
SEXP Rf_duplicate(SEXP);
SEXP Rf_shallow_duplicate(SEXP);
SEXP R_duplicate_attr(SEXP);
SEXP R_shallow_duplicate_attr(SEXP);
SEXP Rf_lazy_duplicate(SEXP);
/* the next really should not be here and is also in Defn.h */
SEXP Rf_duplicated(SEXP x, Rboolean from_last);
SEXP Rf_eval(SEXP e, SEXP rho);
SEXP Rf_findFun(SEXP symbol, SEXP rho);
SEXP Rf_findVar(SEXP symbol, SEXP rho);
SEXP Rf_findVarInFrame(SEXP, SEXP);
SEXP Rf_findVarInFrame3(SEXP, SEXP, Rboolean);
void R_removeVarFromFrame(SEXP name, SEXP env);
SEXP Rf_getAttrib(SEXP vec, SEXP name);
SEXP Rf_GetArrayDimnames(SEXP);
SEXP Rf_GetColNames(SEXP);
void Rf_GetMatrixDimnames(SEXP x, SEXP *rl, SEXP *cl, const char **rn, const char **cn);
SEXP Rf_GetOption(SEXP tag, SEXP rho); /* pre-2.13.0 compatibility */
SEXP Rf_GetOption1(SEXP tag);
int Rf_GetOptionDigits(void);
size_t Rf_GetOptionWidth(void);
SEXP Rf_GetRowNames(SEXP);
void Rf_gsetVar(SEXP symbol, SEXP value, SEXP rho);
SEXP Rf_install(const char *name);
SEXP Rf_installChar(SEXP x);
SEXP Rf_installNoTrChar(SEXP charSXP);
SEXP Rf_installTrChar(SEXP x);
Rboolean Rf_isOrdered(SEXP s);
Rboolean Rf_isUnordered(SEXP s);
Rboolean Rf_isUnsorted(SEXP x, Rboolean strictly);
SEXP Rf_lengthgets(SEXP x, R_len_t len);
SEXP Rf_xlengthgets(SEXP x, R_xlen_t len);
SEXP R_lsInternal(SEXP, Rboolean);
SEXP R_lsInternal3(SEXP env, Rboolean all, Rboolean sorted);
SEXP Rf_match(SEXP, SEXP, int);
SEXP Rf_namesgets(SEXP vec, SEXP val);
SEXP Rf_mkChar(const char *const name);
SEXP Rf_mkCharLen(const char *name, int len);
Rboolean Rf_NonNullStringMatch(SEXP, SEXP);
int Rf_ncols(SEXP);
int Rf_nrows(SEXP);
SEXP Rf_nthcdr(SEXP, int);

// ../main/character.cpp :
typedef enum
{
    Bytes,
    Chars,
    Width
} nchar_type;
int R_nchar(SEXP string, nchar_type type_, Rboolean allowNA, Rboolean keepNA, const char* msg_name);

SEXP R_ParseEvalString(const char *, SEXP);
void Rf_PrintValue(SEXP);
SEXP Rf_setAttrib(SEXP vec, SEXP name, SEXP val);
void Rf_setVar(SEXP symbol, SEXP value, SEXP rho);
SEXPTYPE Rf_str2type(const char *const s);
Rboolean Rf_StringBlank(SEXP x);
SEXP Rf_substitute(SEXP lang, SEXP rho);
SEXP Rf_topenv(SEXP target, SEXP envir);
const char *Rf_translateChar(SEXP);
const char *Rf_translateCharUTF8(SEXP);
const char *Rf_type2char(SEXPTYPE);
SEXP Rf_type2rstr(SEXPTYPE);
SEXP Rf_type2str(SEXPTYPE);
SEXP Rf_type2str_nowarn(SEXPTYPE);

SEXP R_tryEval(SEXP e, SEXP env, int *ErrorOccurred);
SEXP R_tryEvalSilent(SEXP e, SEXP env, int *ErrorOccurred);
SEXP R_GetCurrentEnv();

Rboolean Rf_isS4(SEXP);
SEXP Rf_asS4(SEXP, Rboolean, int);
SEXP Rf_S3Class(SEXP);
int Rf_isBasicClass(const char *);

/* cetype_t is an identifier reseved by POSIX, but it is
   well established as public.  Could remap by a #define though */
typedef enum
{
    CE_NATIVE = 0,
    CE_UTF8 = 1,
    CE_LATIN1 = 2,
    CE_BYTES = 3,
    CE_SYMBOL = 5,
    CE_ANY = 99
} cetype_t;

cetype_t Rf_getCharCE(SEXP);
SEXP Rf_mkCharCE(const char *, cetype_t);
SEXP Rf_mkCharLenCE(const char *, int, cetype_t);
const char *Rf_reEnc(const char *x, cetype_t ce_in, cetype_t ce_out, int subst);

#ifdef __MAIN__
#undef extern1
#undef LibExtern
#endif

/* Calling a function with arguments evaluated */
SEXP R_forceAndCall(SEXP e, int n, SEXP rho);

/* External pointer interface */
SEXP R_MakeExternalPtr(void *p, SEXP tag, SEXP prot);
void *R_ExternalPtrAddr(SEXP s);
SEXP R_ExternalPtrTag(SEXP s);
SEXP R_ExternalPtrProtected(SEXP s);
void R_ClearExternalPtr(SEXP s);
void R_SetExternalPtrAddr(SEXP s, void *p);
void R_SetExternalPtrTag(SEXP s, SEXP tag);
void R_SetExternalPtrProtected(SEXP s, SEXP p);
// Added in R 3.4.0
SEXP R_MakeExternalPtrFn(DL_FUNC p, SEXP tag, SEXP prot);
DL_FUNC R_ExternalPtrAddrFn(SEXP s);

/* Finalization interface */
typedef void (*R_CFinalizer_t)(SEXP);
void R_RegisterFinalizer(SEXP s, SEXP fun);
void R_RegisterCFinalizer(SEXP s, R_CFinalizer_t fun);
void R_RegisterFinalizerEx(SEXP s, SEXP fun, Rboolean onexit);
void R_RegisterCFinalizerEx(SEXP s, R_CFinalizer_t fun, Rboolean onexit);
void R_RunPendingFinalizers(void);

/* Weak reference interface */
SEXP R_MakeWeakRef(SEXP key, SEXP val, SEXP fin, Rboolean onexit);
SEXP R_MakeWeakRefC(SEXP key, SEXP val, R_CFinalizer_t fin, Rboolean onexit);
SEXP R_WeakRefKey(SEXP w);
SEXP R_WeakRefValue(SEXP w);
void R_RunWeakRefFinalizer(SEXP w);

SEXP R_PromiseExpr(SEXP);
SEXP R_ClosureExpr(SEXP);
SEXP R_BytecodeExpr(SEXP e);

/* Protected evaluation */
Rboolean R_ToplevelExec(void (*fun)(void *), void *data);
SEXP R_ExecWithCleanup(SEXP (*fun)(void *), void *data, void (*cleanfun)(void *), void *cleandata);
SEXP R_tryCatch(SEXP (*)(void *), void *,       /* body closure*/
		SEXP,                           /* condition classes (STRSXP) */
		SEXP (*)(SEXP, void *), void *, /* handler closure */
		void (*)(void *), void *);      /* finally closure */
SEXP R_tryCatchError(SEXP (*)(void *), void *,        /* body closure*/
		     SEXP (*)(SEXP, void *), void *); /* handler closure */
SEXP R_withCallingErrorHandler(SEXP (*)(void *), void *, /* body closure*/
			       SEXP (*)(SEXP, void *), void *); /* handler closure */
SEXP R_MakeUnwindCont();
NORET void R_ContinueUnwind(SEXP cont);
SEXP R_UnwindProtect(SEXP (*fun)(void *data), void *data,
                     void (*cleanfun)(void *data, Rboolean jump),
                     void *cleandata, SEXP cont);

/* Environment and Binding Features */
SEXP R_NewEnv(SEXP enclos, int hash, int size);
Rboolean R_IsPackageEnv(SEXP rho);
SEXP R_PackageEnvName(SEXP rho);
SEXP R_FindPackageEnv(SEXP info);
Rboolean R_IsNamespaceEnv(SEXP rho);
SEXP R_NamespaceEnvSpec(SEXP rho);
SEXP R_FindNamespace(SEXP info);
void R_LockEnvironment(SEXP env, Rboolean bindings);
Rboolean R_EnvironmentIsLocked(SEXP env);
void R_LockBinding(SEXP sym, SEXP env);
void R_unLockBinding(SEXP sym, SEXP env);
void R_MakeActiveBinding(SEXP sym, SEXP fun, SEXP env);
Rboolean R_BindingIsLocked(SEXP sym, SEXP env);
Rboolean R_BindingIsActive(SEXP sym, SEXP env);
SEXP R_ActiveBindingFunction(SEXP sym, SEXP env);
Rboolean R_HasFancyBindings(SEXP rho);


/* ../main/errors.cpp : */
/* needed for R_load/savehistory handling in front ends */
#if defined(__GNUC__) && __GNUC__ >= 3
void Rf_errorcall(SEXP, const char *, ...) __attribute__((noreturn));
#else
void Rf_errorcall(SEXP, const char *, ...);
#endif
void warningcall(SEXP, const char *, ...);
void Rf_warningcall_immediate(SEXP, const char *, ...);

/* Save/Load Interface */
#define R_XDR_DOUBLE_SIZE 8
#define R_XDR_INTEGER_SIZE 4

void R_XDREncodeDouble(double d, void *buf);
double R_XDRDecodeDouble(void *buf);
void R_XDREncodeInteger(int i, void *buf);
int R_XDRDecodeInteger(void *buf);

typedef void *R_pstream_data_t;

typedef enum
{
    R_pstream_any_format,
    R_pstream_ascii_format,
    R_pstream_binary_format,
    R_pstream_xdr_format,
    R_pstream_asciihex_format
} R_pstream_format_t;

typedef struct R_outpstream_st *R_outpstream_t;
struct R_outpstream_st
{
    R_pstream_data_t data;
    R_pstream_format_t type;
    int version;
    void (*OutChar)(R_outpstream_t, int);
    void (*OutBytes)(R_outpstream_t, /*const*/ void *, int);
    SEXP(*OutPersistHookFunc)(SEXP, SEXP);
    SEXP OutPersistHookData;
};

typedef struct R_inpstream_st *R_inpstream_t;
#define R_CODESET_MAX 63
struct R_inpstream_st
{
    R_pstream_data_t data;
    R_pstream_format_t type;
    int (*InChar)(R_inpstream_t);
    void (*InBytes)(R_inpstream_t, void *, int);
    SEXP (*InPersistHookFunc)(SEXP, SEXP);
    SEXP InPersistHookData;
    char native_encoding[R_CODESET_MAX + 1];
    void *nat2nat_obj;
    void *nat2utf8_obj;
};

void R_InitInPStream(R_inpstream_t stream, R_pstream_data_t data,
		     R_pstream_format_t type,
		     int (*inchar)(R_inpstream_t),
		     void (*inbytes)(R_inpstream_t, void *, int),
		     SEXP (*phook)(SEXP, SEXP), SEXP pdata);
void R_InitOutPStream(R_outpstream_t stream, R_pstream_data_t data,
		      R_pstream_format_t type, int version,
		      void (*outchar)(R_outpstream_t, int),
		      void (*outbytes)(R_outpstream_t, /*const*/ void *, int),
		      SEXP (*phook)(SEXP, SEXP), SEXP pdata);

#ifdef __cplusplus
void R_InitFileInPStream(R_inpstream_t stream, std::FILE *fp,
			 R_pstream_format_t type,
			 SEXP (*phook)(SEXP, SEXP), SEXP pdata);
void R_InitFileOutPStream(R_outpstream_t stream, std::FILE *fp,
			  R_pstream_format_t type, int version,
			  SEXP (*phook)(SEXP, SEXP), SEXP pdata);
#else
void R_InitFileInPStream(R_inpstream_t stream, FILE *fp,
			 R_pstream_format_t type,
			 SEXP (*phook)(SEXP, SEXP), SEXP pdata);
void R_InitFileOutPStream(R_outpstream_t stream, FILE *fp,
			  R_pstream_format_t type, int version,
			  SEXP (*phook)(SEXP, SEXP), SEXP pdata);
#endif

#ifdef NEED_CONNECTION_PSTREAMS
/* The connection interface is not available to packages.  To
   allow limited use of connection pointers this defines the opaque
   pointer type. */
#ifndef HAVE_RCONNECTION_TYPEDEF
typedef struct Rconn  *Rconnection;
#define HAVE_RCONNECTION_TYPEDEF
#endif
void R_InitConnOutPStream(R_outpstream_t stream, Rconnection con,
			  R_pstream_format_t type, int version,
			  SEXP (*phook)(SEXP, SEXP), SEXP pdata);
void R_InitConnInPStream(R_inpstream_t stream,  Rconnection con,
			 R_pstream_format_t type,
			 SEXP (*phook)(SEXP, SEXP), SEXP pdata);
#endif

void R_Serialize(SEXP s, R_outpstream_t ops);
SEXP R_Unserialize(R_inpstream_t ips);
SEXP R_SerializeInfo(R_inpstream_t ips);

/* slot management (in attrib.cpp) */
SEXP R_do_slot(SEXP obj, SEXP name);
SEXP R_do_slot_assign(SEXP obj, SEXP name, SEXP value);
int R_has_slot(SEXP obj, SEXP name);
/* S3-S4 class (inheritance), attrib.cpp */
SEXP R_S4_extends(SEXP klass, SEXP useTable);

/* class definition, new objects (objects.cpp) */
SEXP R_do_MAKE_CLASS(const char *what);
SEXP R_getClassDef(const char *what);
SEXP R_getClassDef_R(SEXP what);
Rboolean R_has_methods_attached(void);
Rboolean R_isVirtualClass(SEXP class_def, SEXP env);
Rboolean R_extends(SEXP class1, SEXP class2, SEXP env);
SEXP R_do_new_object(SEXP class_def);
/* supporting  a C-level version of  is(., .) : */
int R_check_class_and_super(SEXP x, const char **valid, SEXP rho);
int R_check_class_etc(SEXP x, const char **valid);

/* preserve objects across GCs */
void R_PreserveObject(SEXP);
void R_ReleaseObject(SEXP);

SEXP R_NewPreciousMSet(int);
void R_PreserveInMSet(SEXP x, SEXP mset);
void R_ReleaseFromMSet(SEXP x, SEXP mset);
void R_ReleaseMSet(SEXP mset, int keepSize);

/* Shutdown actions */
void R_dot_Last(void);		/* in main.cpp */
void R_RunExitFinalizers(void);	/* in memory.cpp */

/* Replacements for popen and system */
#ifdef HAVE_POPEN
# ifdef __cplusplus
std::FILE *R_popen(const char *, const char *);
# else
FILE *R_popen(const char *, const char *);
# endif
#endif
int R_system(const char *);

/* R_compute_identical:  C version of identical() function
   The third arg to R_compute_identical() consists of bitmapped flags for non-default options:
   currently the first 4 default to TRUE, so the flag is set for FALSE values:
   1 = !NUM_EQ
   2 = !SINGLE_NA
   4 = !ATTR_AS_SET
   8 = !IGNORE_BYTECODE
  16 = !IGNORE_ENV
  Default from R's default: 16
*/
Rboolean R_compute_identical(SEXP, SEXP, int);

SEXP R_body_no_src(SEXP x); // body(x) without "srcref" etc, ../main/utils.cpp

/* C version of R's  indx <- order(..., na.last, decreasing) :
   e.g.  arglist = Rf_lang2(x,y)  or  Rf_lang3(x,y,z) */
void R_orderVector(int *indx, int n, SEXP arglist, Rboolean nalast, Rboolean decreasing);
// C version of R's  indx <- order(x, na.last, decreasing) :
void R_orderVector1(int *indx, int n, SEXP x, Rboolean nalast, Rboolean decreasing);

#ifdef R_NO_REMAP
#else
#define acopy_string		Rf_acopy_string
#define addMissingVarsToNewEnv	Rf_addMissingVarsToNewEnv
#define alloc3DArray            Rf_alloc3DArray
#define allocArray		Rf_allocArray
#define allocFormalsList2	Rf_allocFormalsList2
#define allocFormalsList3	Rf_allocFormalsList3
#define allocFormalsList4	Rf_allocFormalsList4
#define allocFormalsList5	Rf_allocFormalsList5
#define allocFormalsList6	Rf_allocFormalsList6
#define allocList		Rf_allocList
#define allocMatrix		Rf_allocMatrix
#define allocS4Object		Rf_allocS4Object
#define allocSExp		Rf_allocSExp
#define allocVector		Rf_allocVector
#define allocVector3		Rf_allocVector3
#define any_duplicated		Rf_any_duplicated
#define any_duplicated3		Rf_any_duplicated3
#define applyClosure		Rf_applyClosure
#define arraySubscript		Rf_arraySubscript
#define asChar			Rf_asChar
#define asCharacterFactor	Rf_asCharacterFactor
#define asComplex		Rf_asComplex
#define asInteger		Rf_asInteger
#define asLogical		Rf_asLogical
#define asLogical2		Rf_asLogical2
#define asReal			Rf_asReal
#define asS4			Rf_asS4
#define classgets		Rf_classgets
#define coerceVector		Rf_coerceVector
#define conformable		Rf_conformable
#define cons			Rf_cons
#define fixSubset3Args		Rf_fixSubset3Args
#define copyListMatrix		Rf_copyListMatrix
#define copyMatrix		Rf_copyMatrix
#define copyMostAttrib		Rf_copyMostAttrib
#define copyVector		Rf_copyVector
#define countContexts		Rf_countContexts
#define CreateTag		Rf_CreateTag
#define defineVar		Rf_defineVar
#define dimgets			Rf_dimgets
#define dimnamesgets		Rf_dimnamesgets
#define DropDims                Rf_DropDims
#define duplicate		Rf_duplicate
#define duplicated		Rf_duplicated
#define elt			Rf_elt
#define errorcall		Rf_errorcall
#define eval			Rf_eval
#define ExtractSubset		Rf_ExtractSubset
#define findFun			Rf_findFun
#define findFun3		Rf_findFun3
#define findFunctionForBody	Rf_findFunctionForBody
#define findVar			Rf_findVar
#define findVarInFrame		Rf_findVarInFrame
#define findVarInFrame3		Rf_findVarInFrame3
#define FixupDigits		Rf_FixupDigits
#define FixupWidth		Rf_FixupWidth
#define GetArrayDimnames	Rf_GetArrayDimnames
#define getAttrib		Rf_getAttrib
#define getCharCE		Rf_getCharCE
#define GetColNames		Rf_GetColNames
#define GetMatrixDimnames	Rf_GetMatrixDimnames
#define GetOption1		Rf_GetOption1
#define GetOptionDigits		Rf_GetOptionDigits
#define GetOptionWidth		Rf_GetOptionWidth
#define GetOption		Rf_GetOption
#define GetRowNames		Rf_GetRowNames
#define gsetVar			Rf_gsetVar
#define inherits		Rf_inherits
#define install			Rf_install
#define installChar		Rf_installTrChar
#define installNoTrChar		Rf_installNoTrChar
#define installTrChar		Rf_installTrChar
#define installDDVAL		Rf_installDDVAL
#define installS3Signature	Rf_installS3Signature
#define isArray			Rf_isArray
#define isBasicClass            Rf_isBasicClass
#define isComplex		Rf_isComplex
#define isEnvironment		Rf_isEnvironment
#define isExpression		Rf_isExpression
#define isFactor		Rf_isFactor
#define isFrame			Rf_isFrame
#define isFree			Rf_isFree
#define isFunction		Rf_isFunction
#define isInteger		Rf_isInteger
#define isLanguage		Rf_isLanguage
#define isList			Rf_isList
#define isLogical		Rf_isLogical
#define isSymbol		Rf_isSymbol
#define isMatrix		Rf_isMatrix
#define isNewList		Rf_isNewList
#define isNull			Rf_isNull
#define isNumeric		Rf_isNumeric
#define isNumber		Rf_isNumber
#define isObject		Rf_isObject
#define isRaw   		Rf_isRaw
#define isOrdered		Rf_isOrdered
#define isPairList		Rf_isPairList
#define isPrimitive		Rf_isPrimitive
#define isReal			Rf_isReal
#define isS4			Rf_isS4
#define isString		Rf_isString
#define isTs			Rf_isTs
#define isUnmodifiedSpecSym	Rf_isUnmodifiedSpecSym
#define isUnordered		Rf_isUnordered
#define isUnsorted		Rf_isUnsorted
#define isUserBinop		Rf_isUserBinop
#define isValidString		Rf_isValidString
#define isValidStringF		Rf_isValidStringF
#define isVector		Rf_isVector
#define isVectorAtomic		Rf_isVectorAtomic
#define isVectorizable		Rf_isVectorizable
#define isVectorList		Rf_isVectorList
#define lang1			Rf_lang1
#define lang2			Rf_lang2
#define lang3			Rf_lang3
#define lang4			Rf_lang4
#define lang5			Rf_lang5
#define lang6			Rf_lang6
#define lastElt			Rf_lastElt
#define lazy_duplicate		Rf_lazy_duplicate
#define lcons			Rf_lcons
#ifndef __cplusplus
#define length(x)		Rf_length(x)
#endif
#define lengthgets		Rf_lengthgets
#define list1			Rf_list1
#define list2			Rf_list2
#define list3			Rf_list3
#define list4			Rf_list4
#define list5			Rf_list5
#define list6			Rf_list6
#define listAppend		Rf_listAppend
#define match			Rf_match
#define matchE			Rf_matchE
#define mkChar			Rf_mkChar
#define mkCharCE		Rf_mkCharCE
#define mkCharLen		Rf_mkCharLen
#define mkCharLenCE		Rf_mkCharLenCE
#define mkNamed			Rf_mkNamed
#define mkString		Rf_mkString
#define namesgets		Rf_namesgets
#define ncols			Rf_ncols
#define nlevels			Rf_nlevels
#define NonNullStringMatch	Rf_NonNullStringMatch
#define nrows			Rf_nrows
#define nthcdr			Rf_nthcdr
#define PairToVectorList	Rf_PairToVectorList
#define pmatch			Rf_pmatch
#define psmatch			Rf_psmatch
#define PrintValue		Rf_PrintValue
#define printwhere		Rf_printwhere
#define protect(x)			Rf_protect(x)
#define readS3VarsFromFrame	Rf_readS3VarsFromFrame
#define reEnc			Rf_reEnc
#define S3Class                 Rf_S3Class
#define ScalarComplex		Rf_ScalarComplex
#define ScalarInteger		Rf_ScalarInteger
#define ScalarLogical		Rf_ScalarLogical
#define ScalarReal		Rf_ScalarReal
#define ScalarString		Rf_ScalarString
#define ScalarRaw		Rf_ScalarRaw
#define setAttrib		Rf_setAttrib
#define setSVector		Rf_setSVector
#define setVar			Rf_setVar
#define shallow_duplicate	Rf_shallow_duplicate
#define str2type		Rf_str2type
#define stringSuffix		Rf_stringSuffix
#define stringPositionTr	Rf_stringPositionTr
#define StringBlank		Rf_StringBlank
#define substitute		Rf_substitute
#define topenv		        Rf_topenv
#define translateChar		Rf_translateChar
#define translateChar0		Rf_translateChar0
#define translateCharUTF8      	Rf_translateCharUTF8
#define type2char		Rf_type2char
#define type2rstr		Rf_type2rstr
#define type2str		Rf_type2str
#define type2str_nowarn		Rf_type2str_nowarn
#define unprotect(x)		Rf_unprotect(x)
#define unprotect_ptr		Rf_unprotect_ptr
#define VectorToPairList	Rf_VectorToPairList
#define warningcall_immediate	Rf_warningcall_immediate
#define xlength(x)		Rf_xlength(x)
#define xlengthgets		Rf_xlengthgets
#endif
#define Rf_warningcall		warningcall

/* need remapped names here for use with R_NO_REMAP */

/*
   These are the public inlinable functions that are provided in
   Rinlinedfuns.h It is *essential* that these do not appear in any
   other header file, with or without the Rf_ prefix.
*/
SEXP     Rf_allocVector(SEXPTYPE, R_xlen_t);
Rboolean Rf_conformable(SEXP, SEXP);
SEXP	 Rf_elt(SEXP, int);
Rboolean Rf_inherits(SEXP, const char *);
Rboolean Rf_isArray(SEXP);
Rboolean Rf_isFactor(SEXP);
Rboolean Rf_isFrame(SEXP);
Rboolean Rf_isFunction(SEXP);
Rboolean Rf_isInteger(SEXP);
Rboolean Rf_isLanguage(SEXP);
Rboolean Rf_isList(SEXP);
Rboolean Rf_isMatrix(SEXP);
Rboolean Rf_isNewList(SEXP);
Rboolean Rf_isNumber(SEXP);
Rboolean Rf_isNumeric(SEXP);
Rboolean Rf_isPairList(SEXP);
Rboolean Rf_isPrimitive(SEXP);
Rboolean Rf_isTs(SEXP);
Rboolean Rf_isUserBinop(SEXP);
Rboolean Rf_isValidString(SEXP);
Rboolean Rf_isValidStringF(SEXP);
Rboolean Rf_isVector(SEXP);
Rboolean Rf_isVectorAtomic(SEXP);
Rboolean Rf_isVectorList(SEXP);
Rboolean Rf_isVectorizable(SEXP);
SEXP	 Rf_lang1(SEXP);
SEXP	 Rf_lang2(SEXP, SEXP);
SEXP	 Rf_lang3(SEXP, SEXP, SEXP);
SEXP	 Rf_lang4(SEXP, SEXP, SEXP, SEXP);
SEXP	 Rf_lang5(SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP	 Rf_lang6(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP	 Rf_lastElt(SEXP);
SEXP	 Rf_lcons(SEXP, SEXP);
R_len_t  Rf_length(SEXP);
SEXP	 Rf_list1(SEXP);
SEXP	 Rf_list2(SEXP, SEXP);
SEXP	 Rf_list3(SEXP, SEXP, SEXP);
SEXP	 Rf_list4(SEXP, SEXP, SEXP, SEXP);
SEXP	 Rf_list5(SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP	 Rf_list6(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP	 Rf_listAppend(SEXP, SEXP);
SEXP	 Rf_mkNamed(SEXPTYPE, const char **);
SEXP	 Rf_mkString(const char *);
int	 Rf_nlevels(SEXP);
int	 Rf_stringPositionTr(SEXP, const char *);
SEXP	 Rf_ScalarComplex(Rcomplex);
SEXP	 Rf_ScalarInteger(int);
SEXP	 Rf_ScalarLogical(int);
SEXP	 Rf_ScalarRaw(Rbyte);
SEXP	 Rf_ScalarReal(double);
SEXP	 Rf_ScalarString(SEXP);
R_xlen_t  Rf_xlength(SEXP);
R_xlen_t  (XLENGTH)(SEXP x);
R_xlen_t  XTRUELENGTH(SEXP x);
int LENGTH_EX(SEXP x, const char *file, int line);
R_xlen_t XLENGTH_EX(SEXP x);
#ifdef COMPILING_IVORY
SEXP CXXR_protect(SEXP node, const char *function_name);
void CXXR_unprotect(unsigned int count, const char *function_name);
void CXXR_ProtectWithIndex(SEXP node, PROTECT_INDEX *iptr, const char *function_name);
void CXXR_Reprotect(SEXP node, PROTECT_INDEX iptr, const char *function_name);
void CXXR_unprotect_ptr(SEXP node, const char *function_name);
#endif
SEXP Rf_protect(SEXP);
void Rf_unprotect(int);
void R_ProtectWithIndex(SEXP, PROTECT_INDEX *);
void R_Reprotect(SEXP, PROTECT_INDEX);
void Rf_unprotect_ptr(SEXP);
SEXP CAR(SEXP e);
void *DATAPTR(SEXP x);
const void *DATAPTR_RO(SEXP x);
const void *DATAPTR_OR_NULL(SEXP x);
const int *LOGICAL_OR_NULL(SEXP x);
const int *INTEGER_OR_NULL(SEXP x);
const double *REAL_OR_NULL(SEXP x);
const Rcomplex *COMPLEX_OR_NULL(SEXP x);
const Rbyte *RAW_OR_NULL(SEXP x);
int INTEGER_ELT(SEXP x, R_xlen_t i);
double REAL_ELT(SEXP x, R_xlen_t i);
int LOGICAL_ELT(SEXP x, R_xlen_t i);
Rcomplex COMPLEX_ELT(SEXP x, R_xlen_t i);
Rbyte RAW_ELT(SEXP x, R_xlen_t i);
SEXP STRING_ELT(SEXP x, R_xlen_t i);
void SET_LOGICAL_ELT(SEXP x, R_xlen_t i, int v);
void SET_INTEGER_ELT(SEXP x, R_xlen_t i, int v);
void SET_REAL_ELT(SEXP x, R_xlen_t i, double v);
void SET_COMPLEX_ELT(SEXP x, R_xlen_t i, Rcomplex v);
void SET_RAW_ELT(SEXP x, R_xlen_t i, Rbyte v);

/* ALTREP support */
SEXP ALTREP_CLASS(SEXP x);
SEXP R_altrep_data1(SEXP x);
SEXP R_altrep_data2(SEXP x);
void R_set_altrep_data1(SEXP x, SEXP v);
void R_set_altrep_data2(SEXP x, SEXP v);

int *LOGICAL0(SEXP x);
int *INTEGER0(SEXP x);
double *REAL0(SEXP x);
Rcomplex *COMPLEX0(SEXP x);
Rbyte *RAW0(SEXP x);

int ALTREP(SEXP x);

/* ALTREP sorting support */
typedef enum Sortness
{
    SORTED_DECR_NA_1ST = -2,
    SORTED_DECR = -1,
    UNKNOWN_SORTEDNESS = INT_MIN, /*INT_MIN is NA_INTEGER! */
    SORTED_INCR = 1,
    SORTED_INCR_NA_1ST = 2,
    KNOWN_UNSORTED = 0
} Sortness;
#define KNOWN_SORTED(sorted) (sorted == SORTED_DECR ||        \
                              sorted == SORTED_INCR ||        \
                              sorted == SORTED_DECR_NA_1ST || \
                              sorted == SORTED_INCR_NA_1ST)

#define KNOWN_NA_1ST(sorted) (sorted == SORTED_INCR_NA_1ST || \
                              sorted == SORTED_DECR_NA_1ST)

#define KNOWN_INCR(sorted) (sorted == SORTED_INCR || \
                            sorted == SORTED_INCR_NA_1ST)

#define KNOWN_DECR(sorted) (sorted == SORTED_DECR || \
                            sorted == SORTED_DECR_NA_1ST)

/* stuff that probably shouldn't be in the API but is getting used */

void SET_TYPEOF(SEXP x, SEXPTYPE v); // used by Rcpp
void SET_OBJECT(SEXP x, int v); // used by Rcpp
void SET_S4_OBJECT(SEXP x); // used by Rcpp (maybe?)
void UNSET_S4_OBJECT(SEXP x); // used by Rcpp (maybe?)
const char *R_curErrorBuf(); // used by unix */
int IS_SCALAR(SEXP x, SEXPTYPE type); // used by symengine */
Rboolean Rf_psmatch(const char *, const char *, Rboolean); // used by rgl

/* stringi defines USE_RINTERNALS and NO_REMAP so needs these for now */
#if defined(USE_RINTERNALS) || defined(COMPILING_IVORY)
#undef isNull
#define isNull(s) (TYPEOF(s) == NILSXP)
#undef isSymbol
#define isSymbol(s) (TYPEOF(s) == SYMSXP)
#undef isLogical
#define isLogical(s) (TYPEOF(s) == LGLSXP)
#undef isReal
#define isReal(s) (TYPEOF(s) == REALSXP)
#undef isString
#define isString(s) (TYPEOF(s) == STRSXP)
#undef isObject
#define isObject(s) (OBJECT(s) != 0)
#undef isRaw
#define isRaw(s) (TYPEOF(s) == RAWSXP)
#endif

/* used in a couple of packages but should probably be dropped */
				/* match(.) NOT reached : for -Wall */
#define error_return(msg)	{ Rf_error(msg);	   return R_NilValue; }
#define errorcall_return(cl,msg){ Rf_errorcall(cl, msg);   return R_NilValue; }

void SETLENGTH(SEXP x, R_xlen_t v); // used by data.table and others
void SET_TRUELENGTH(SEXP x, R_xlen_t v); // used by data.table and others
void  SETLEVELS(SEXP x, int v); // used by quotedargs

void SET_ENVFLAGS(SEXP x, int v); // used by rlang and others
void SET_FRAME(SEXP x, SEXP v); // used by rlang and others
void SET_ENCLOS(SEXP x, SEXP v); // used by rlang and others
void SET_HASHTAB(SEXP x, SEXP v); // used by rlang and others

void SET_PRENV(SEXP x, SEXP v); // used by dplyr, others
void SET_PRVALUE(SEXP x, SEXP v); // used by dplyr, others
void SET_PRCODE(SEXP x, SEXP v); // used by magrittr, others

void *STDVEC_DATAPTR(SEXP x); // used by vroom

/* Growable vector support */ // used by multbxxc
int IS_GROWABLE(SEXP x);
void SET_GROWABLE_BIT(SEXP x);

// used by quotedargs
#define BCODE_CONSTS(x) CDR(x) // re-enable in Defn.h after removing here
void SET_NAMED(SEXP x, int v);

// used by igraph, lazyeval, nseval, rlang
#define PREXPR(e) R_PromiseExpr(e)

// used by rlang
#define BODY_EXPR(e) R_ClosureExpr(e)

// used by BIOC::matter; mightbe reasonable to include in API
SEXP R_tryWrap(SEXP x);

#ifdef __cplusplus
/** @brief Shorthand for Rf_length().
 */
inline R_len_t length(SEXP s)
{
    return Rf_length(s);
}
#endif

#ifdef __cplusplus
}
#endif

#if (defined(R_NO_REMAP) && defined(COMPILING_IVORY)) && defined(__cplusplus)
const auto acopy_string = Rf_acopy_string;
const auto alloc3DArray = Rf_alloc3DArray;
const auto allocArray = Rf_allocArray;
const auto allocList = Rf_allocList;
const auto allocMatrix = Rf_allocMatrix;
const auto allocS4Object = Rf_allocS4Object;
const auto allocSExp = Rf_allocSExp;
const auto any_duplicated = Rf_any_duplicated;
const auto any_duplicated3 = Rf_any_duplicated3;
const auto applyClosure = Rf_applyClosure;
const auto asChar = Rf_asChar;
const auto asCharacterFactor = Rf_asCharacterFactor;
const auto asComplex = Rf_asComplex;
const auto asInteger = Rf_asInteger;
const auto asLogical = Rf_asLogical;
const auto asReal = Rf_asReal;
const auto asS4	= Rf_asS4;
const auto classgets = Rf_classgets;
const auto coerceVector = Rf_coerceVector;
const auto cons = Rf_cons;
const auto copyListMatrix = Rf_copyListMatrix;
const auto copyMatrix = Rf_copyMatrix;
const auto copyMostAttrib = Rf_copyMostAttrib;
const auto copyVector = Rf_copyVector;
const auto defineVar = Rf_defineVar;
const auto dimgets = Rf_dimgets;
const auto dimnamesgets = Rf_dimnamesgets;
const auto duplicate = Rf_duplicate;
const auto duplicated = Rf_duplicated;
const auto errorcall = Rf_errorcall;
const auto eval = Rf_eval;
const auto findFun = Rf_findFun;
const auto findVar = Rf_findVar;
const auto findVarInFrame = Rf_findVarInFrame;
const auto findVarInFrame3 = Rf_findVarInFrame3;
const auto GetArrayDimnames = Rf_GetArrayDimnames;
const auto getAttrib = Rf_getAttrib;
const auto getCharCE = Rf_getCharCE;
const auto GetColNames = Rf_GetColNames;
const auto GetMatrixDimnames = Rf_GetMatrixDimnames;
const auto GetOption1 = Rf_GetOption1;
const auto GetOptionDigits = Rf_GetOptionDigits;
const auto GetOptionWidth = Rf_GetOptionWidth;
const auto GetOption = Rf_GetOption;
const auto GetRowNames = Rf_GetRowNames;
const auto gsetVar = Rf_gsetVar;
const auto install = Rf_install;
const auto installChar = Rf_installTrChar;
const auto installNoTrChar = Rf_installNoTrChar;
const auto installTrChar = Rf_installTrChar;
const auto isBasicClass = Rf_isBasicClass;
const auto isOrdered = Rf_isOrdered;
const auto isS4 = Rf_isS4;
const auto isUnordered = Rf_isUnordered;
const auto isUnsorted = Rf_isUnsorted;
const auto lazy_duplicate = Rf_lazy_duplicate;
const auto lengthgets = Rf_lengthgets;
const auto match = Rf_match;
const auto mkChar = Rf_mkChar;
const auto mkCharCE = Rf_mkCharCE;
const auto mkCharLen = Rf_mkCharLen;
const auto mkCharLenCE = Rf_mkCharLenCE;
const auto namesgets = Rf_namesgets;
const auto ncols = Rf_ncols;
const auto NonNullStringMatch = Rf_NonNullStringMatch;
const auto nrows = Rf_nrows;
const auto nthcdr = Rf_nthcdr;
const auto PairToVectorList = Rf_PairToVectorList;
const auto psmatch = Rf_psmatch;
const auto PrintValue = Rf_PrintValue;
const auto reEnc = Rf_reEnc;
const auto S3Class = Rf_S3Class;
const auto setAttrib = Rf_setAttrib;
const auto setVar = Rf_setVar;
const auto shallow_duplicate = Rf_shallow_duplicate;
const auto str2type = Rf_str2type;
const auto StringBlank = Rf_StringBlank;
const auto substitute = Rf_substitute;
const auto topenv = Rf_topenv;
const auto translateChar = Rf_translateChar;
const auto translateCharUTF8 = Rf_translateCharUTF8;
const auto type2char = Rf_type2char;
const auto type2rstr = Rf_type2rstr;
const auto type2str = Rf_type2str;
const auto type2str_nowarn = Rf_type2str_nowarn;
const auto unprotect_ptr = Rf_unprotect_ptr;
const auto VectorToPairList = Rf_VectorToPairList;
const auto warningcall_immediate = Rf_warningcall_immediate;
const auto xlengthgets = Rf_xlengthgets;
#endif

#endif /* R_INTERNALS_H_ */
