/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998--2020  The R Core Team.
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
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

/* Internal header, not installed */

/** @file Defn.h
 *
 * @brief A ragbag.
 */

#ifndef DEFN_H_
#define DEFN_H_

#ifndef __cplusplus
#error Defn.h can only be included in C++ files
#endif

// #ifdef USE_RINTERNALS
// #undef USE_RINTERNALS
// #endif

#include <utility>
#include <iostream>
#include <string>
#include <CXXR/RObject.hpp>
#include <CXXR/GCRoot.hpp>
#include <CXXR/ConsCell.hpp>
#include <CXXR/BuiltInFunction.hpp>
#include <CXXR/Symbol.hpp>
#include <CXXR/VectorBase.hpp>
#include <CXXR/FixedVector.hpp>
#include <CXXR/ComplexVector.hpp>
#include <CXXR/RawVector.hpp>
#include <CXXR/RealVector.hpp>
#include <CXXR/IntVector.hpp>
#include <CXXR/StringVector.hpp>
#include <CXXR/LogicalVector.hpp>
#include <CXXR/Environment.hpp>
#include <CXXR/Promise.hpp>
#include <CXXR/Closure.hpp>
#include <CXXR/RAllocStack.hpp>

/* To test the write barrier used by the generational collector,
   define TESTING_WRITE_BARRIER.  This makes the internal structure of
   SEXPRECs visible only inside of files that explicitly define
   USE_RINTERNALS, and all uses of RObject fields that do not go
   through the appropriate functions or macros will become compilation
   errors.  Since this does impose a small but noticable performance
   penalty, code that includes Defn.h (or code that explicitly defines
   USE_RINTERNALS) can access a RObject's fields directly. */

// #define TESTING_WRITE_BARRIER

// #ifndef TESTING_WRITE_BARRIER
// # define USE_RINTERNALS
// #endif

#include <R_ext/Visibility.h>

#ifdef __MAIN__
#define extern0 HIDDEN
#else
#define extern0 extern
#endif

constexpr int MAXELTSIZE = 8192; /* Used as a default for string buffer sizes, \
               and occasionally as a limit. */

#include <R_ext/Complex.h>

namespace R
{
    void CoercionWarning(int); /* warning code */
    int LogicalFromInteger(int, int &);
    int LogicalFromReal(double, int &);
    int LogicalFromComplex(Rcomplex, int &);
    int IntegerFromLogical(int, int &);
    int IntegerFromReal(double, int &);
    int IntegerFromComplex(Rcomplex, int &);
    double RealFromLogical(int, int &);
    double RealFromInteger(int, int &);
    double RealFromComplex(Rcomplex, int &);
    Rcomplex ComplexFromLogical(int, int &);
    Rcomplex ComplexFromInteger(int, int &);
    Rcomplex ComplexFromReal(double, int &);
} // namespace R

#define CALLED_FROM_DEFN_H 1
#include <Rinternals.h>		/*-> Arith.h, Boolean.h, Complex.h, Error.h,
				  Memory.h, PrtUtil.h, Utils.h */
#undef CALLED_FROM_DEFN_H

namespace R
{
    const char *translateCharFP(SEXP);
    const char *translateCharFP2(SEXP);
    const char *trCharUTF8(SEXP);
} // namespace R

extern0 SEXP	R_CommentSymbol;    /* "comment" */
extern0 SEXP	R_DotEnvSymbol;     /* ".Environment" */
extern0 SEXP	R_ExactSymbol;	    /* "exact" */
extern0 SEXP	R_RecursiveSymbol;  /* "recursive" */
extern0 SEXP	R_WholeSrcrefSymbol;   /* "wholeSrcref" */
extern0 SEXP	R_TmpvalSymbol;     /* "*tmp*" */
extern0 SEXP	R_UseNamesSymbol;   /* "use.names" */
extern0 SEXP	R_ColonSymbol;         /* ":" */
//extern0 SEXP	R_DoubleColonSymbol;   /* "::" */
//extern0 SEXP	R_TripleColonSymbol;   /* ":::" */
extern0 SEXP    R_ConnIdSymbol;  /* "conn_id" */
extern0 SEXP    R_DevicesSymbol;  /* ".Devices" */

extern0 SEXP    R_dot_Methods;  /* ".Methods" */
extern0 SEXP    R_dot_Group;  /* ".Group" */
extern0 SEXP    R_dot_Class;  /* ".Class" */
extern0 SEXP    R_dot_GenericCallEnv;  /* ".GenericCallEnv" */
extern0 SEXP    R_dot_GenericDefEnv;  /* ".GenericDefEnv" */

extern0 SEXP	R_StringHash;       /* Global hash of CHARSXPs */

/**** HASHASH uses the first bit -- see HASHASH_MASK defined below */

/* macros and declarations for managing CHARSXP cache */
# define CXHEAD(x) (x)
# define CXTAIL(x) ATTRIB(x)
SEXP (SET_CXTAIL)(SEXP x, SEXP y);

#include <Errormsg.h>

extern "C" void R_ProcessEvents(void);
#ifdef _WIN32
extern void R_WaitEvent(void);
#endif

#ifdef Unix
# define OSTYPE      "unix"
# define FILESEP     "/"
#endif /* Unix */

#ifdef _WIN32
# define OSTYPE      "windows"
# define FILESEP     "/"
#endif /* Win32 */

#ifdef HAVE_F77_UNDERSCORE
# define F77_SYMBOL(x)	x ## _
# define F77_QSYMBOL(x)	#x "_"
#else
# define F77_SYMBOL(x)	x
# define F77_QSYMBOL(x) #x
#endif

/*  Heap and Pointer Protection Stack Sizes.  */

/* These headers are all required by C99.
   However, we use types below such as uintptr_t which are optional in C11.
   And on some older systems they were in inttypes.h but not stdint.h.

   Up to 2.11.1 (r52035, May 2010) we had

#if !defined(HAVE_INTPTR_T) && !defined(intptr_t)
 typedef long intptr_t;
#endif
#if !defined(HAVE_UINTPTR_T) && !defined(uintptr_t)
 typedef unsigned long uintptr_t;
#endif
    but size_t might be better.

 */
#ifdef HAVE_INTTYPES_H
#include <inttypes.h>
#endif
/* According to POSIX inttypes.h should include stdint.h,
   but let's be sure. */
#ifdef HAVE_STDINT_H
#include <stdint.h>
#endif
#ifdef HAVE_LIMITS_H
#include <limits.h>
#endif

#include <CXXR/RTypes.hpp>

constexpr double Mega = 1048576.; /* 1 Mega Byte := 2^20 (= 1048576) Bytes */
constexpr double Giga = 1073741824.; /* 1 Giga Byte := 2^30 Bytes */

/*	R_PPSSIZE  The pointer protection stack size  */
/*	R_NSIZE	   The number of cons cells	 */
/*	R_VSIZE	   The vector heap size in bytes */
/*  These values are defaults and can be overridden in config.h
    The maxima and minima are in ../main/startup.cpp */

#ifndef R_PPSSIZE
constexpr long R_PPSSIZE = 50000L;
#endif
#ifndef R_NSIZE
constexpr long R_NSIZE = 350000L;
#endif
#ifndef R_VSIZE
constexpr long R_VSIZE = 67108864L;
#endif

/* some commonly needed headers */
#include <cmath>
#include <cstdlib>
#include <cstring>
#include <vector>

/* declare substitutions */
#if !defined(strdup) && defined(HAVE_DECL_STRDUP) && !HAVE_DECL_STRDUP
extern char *strdup(const char *s1);
#endif
#if !defined(strncascmp) && defined(HAVE_DECL_STRNCASECMP) && !HAVE_DECL_STRNCASECMP
extern int strncasecmp(const char *s1, const char *s2, size_t n);
#endif

/* safer alternative */
extern char *Rstrdup(const char *s);


/* Glibc manages to not define this in -pedantic -ansi */
#if defined(HAVE_PUTENV) && !defined(putenv) && defined(HAVE_DECL_PUTENV) && !HAVE_DECL_PUTENV
extern int putenv(char *string);
#endif


/* Maximal length in bytes of an entire path name.
   POSIX has required this to be at least 255/256, and X/Open at least 1024.
   Solaris has 1024, Linux glibc has 4192.
   File names are limited to FILENAME_MAX bytes (usually the same as PATH_MAX)
   or NAME_MAX (often 255/256).
 */
#if !defined(PATH_MAX)
#if defined(HAVE_SYS_PARAM_H)
#include <sys/param.h>
#endif
#if !defined(PATH_MAX)
#if defined(MAXPATHLEN)
/* Try BSD name */
#define PATH_MAX MAXPATHLEN
#elif defined(_WIN32)
/* seems this is now defined by MinGW to be 259, whereas FILENAME_MAX
   and MAX_PATH are 260.  It is not clear that this really is in bytes,
   but might be chars for the Unicode interfaces.

   260 is d:\ plus 256 chars plus nul.  Some but not all API calls
   allow filepaths of the form \\?\D:\very_long_path .
*/
constexpr size_t PATH_MAX = 260;
#else
/* quite possibly unlimited, so we make this large, and test when used */
constexpr size_t PATH_MAX = 5000;
#endif
#endif
#endif

constexpr int HSIZE = 49157;	/* The size of the hash table for symbols */
constexpr int MAXIDSIZE = 10000; /* Largest symbol size,                  \
               in bytes excluding terminator.                    \
               Was 256 prior to 2.13.0, now just a sanity check. \
            */

#ifdef USE_RINTERNALS

// Content moved to RObject.hpp

#else /* of USE_RINTERNALS */

Rboolean (IS_ACTIVE_BINDING)(SEXP b);
Rboolean (BINDING_IS_LOCKED)(SEXP b);
void (SET_ACTIVE_BINDING_BIT)(SEXP b);
void (LOCK_BINDING)(SEXP b);
void (UNLOCK_BINDING)(SEXP b);

void (SET_BASE_SYM_CACHED)(SEXP b);
void (UNSET_BASE_SYM_CACHED)(SEXP b);
Rboolean (BASE_SYM_CACHED)(SEXP b);

void (SET_SPECIAL_SYMBOL)(SEXP b);
void (UNSET_SPECIAL_SYMBOL)(SEXP b);
Rboolean (IS_SPECIAL_SYMBOL)(SEXP b);
void (SET_NO_SPECIAL_SYMBOLS)(SEXP b);
void (UNSET_NO_SPECIAL_SYMBOLS)(SEXP b);
Rboolean (NO_SPECIAL_SYMBOLS)(SEXP b);

#endif /* USE_RINTERNALS */

#include <RCNTXT.h>

/* Miscellaneous Definitions */
inline bool streql(const char *s, const char *t)
{
    return (strcmp(s, t) == 0);
}
inline bool streqln(const char *s, const char *t, size_t n)
{
    return (strncmp(s, t, n) == 0);
}

/* Arithmetic and Relation Operators */
enum ARITHOP_TYPE
{
    PLUSOP = 1,
    MINUSOP,
    TIMESOP,
    DIVOP,
    POWOP,
    MODOP,
    IDIVOP
};

enum RELOP_TYPE
{
    EQOP = 1,
    NEOP,
    LTOP,
    LEOP,
    GEOP,
    GTOP
};

enum MATPROD_TYPE
{
    MATPROD_DEFAULT = 1,
    MATPROD_INTERNAL,
    MATPROD_BLAS,
    MATPROD_DEFAULT_SIMD /* experimental */
};

/* File Handling */
constexpr int R_EOF = -1;


/*--- Global Variables ---------------------------------------------------- */

extern "C" {
#include <R_ext/libextern.h>

#ifdef __MAIN__
#define INI_as(v) = v
#define extern0 HIDDEN
#else
#define INI_as(v)
#define extern0 extern
#endif

LibExtern SEXP  R_SrcfileSymbol;    /* "srcfile" */
LibExtern SEXP  R_SrcrefSymbol;     /* "srcref" */


LibExtern Rboolean R_interrupts_suspended INI_as(FALSE);
LibExtern int R_interrupts_pending INI_as(0);

/* R Home Directory */
LibExtern char *R_Home;		    /* Root of the R tree */

/* Memory Management */
extern0 R_size_t R_NSize  INI_as(R_NSIZE);/* Size of cons cell heap */
extern0 R_size_t R_VSize  INI_as(R_VSIZE);/* Size of the vector heap */
extern0 bool	R_GCEnabled INI_as(true);
extern0 bool	R_in_gc INI_as(false);
extern0 bool	R_BCIntActive INI_as(false); /* bcEval called more recently than
                                            eval */
extern0 void*	R_BCpc INI_as(nullptr);/* current byte code instruction */
extern0 SEXP	R_BCbody INI_as(nullptr); /* current byte code object */
extern0 SEXP	R_NHeap;	    /* Start of the cons cell heap */
extern0 SEXP	R_FreeSEXP;	    /* Cons cell free list */
extern0 int	R_Is_Running;	    /* for Windows memory manager */

/* Evaluation Environment */
extern0 SEXP	R_CurrentExpr;	    /* Currently evaluating expression */
extern0 SEXP	R_ReturnedValue;    /* Slot for return-ing values */
extern0 SEXP*	R_SymbolTable;	    /* The symbol table */
#ifdef R_USE_SIGNALS
extern0 RCNTXT R_Toplevel;	      /* Storage for the toplevel context */
extern0 RCNTXT* R_ToplevelContext;  /* The toplevel context */
LibExtern RCNTXT* R_GlobalContext;    /* The global context */
extern0 RCNTXT* R_SessionContext;   /* The session toplevel context */
extern0 RCNTXT* R_ExitContext;      /* The active context for on.exit processing */
#endif
extern bool R_Visible;	    /* Value visibility flag */
extern0 int	R_EvalDepth	INI_as(0);	/* Evaluation recursion depth */
extern0 int	R_BrowseLines	INI_as(0);	/* lines/per call in browser :
						 * options(deparse.max.lines) */
extern0 int	R_Expressions	INI_as(5000);	/* options(expressions) */
extern0 int	R_Expressions_keep INI_as(5000);/* options(expressions) */
extern0 Rboolean R_KeepSource	INI_as(FALSE);	/* options(keep.source) */
extern0 Rboolean R_CBoundsCheck	INI_as(FALSE);	/* options(CBoundsCheck) */
extern0 MATPROD_TYPE R_Matprod	INI_as(MATPROD_DEFAULT);  /* options(matprod) */
extern0 size_t	R_WarnLength	INI_as(1000);	/* Error/warning max length */
extern0 int	R_nwarnings	INI_as(50);

/* C stack checking */
extern uintptr_t R_CStackLimit	INI_as((uintptr_t)-1);	/* C stack limit */
extern uintptr_t R_OldCStackLimit INI_as((uintptr_t)0); /* Old value while
							   handling overflow */
extern uintptr_t R_CStackStart	INI_as((uintptr_t)-1);	/* Initial stack address */
/* Default here is for Windows: set from configure in src/unix/system.cpp */
extern int	R_CStackDir	INI_as(1);	/* C stack direction */

#ifdef R_USE_SIGNALS
extern0 struct RPRSTACK *R_PendingPromises INI_as(nullptr); /* Pending promise stack */
#endif

/* File Input/Output */
LibExtern Rboolean R_Interactive INI_as(TRUE);	/* TRUE during interactive use*/
extern0 Rboolean R_Quiet	INI_as(FALSE);	/* Be as quiet as possible */
extern Rboolean  R_NoEcho	INI_as(FALSE);	/* do not echo R code */
extern0 Rboolean R_Verbose	INI_as(FALSE);	/* Be verbose */
/* extern int	R_Console; */	    /* Console active flag */
/* IoBuffer R_ConsoleIob; : --> ./IOStuff.h */
/* R_Consolefile is used in the internet module */
extern FILE*	R_Consolefile	INI_as(nullptr);	/* Console output file */
extern FILE*	R_Outputfile	INI_as(nullptr);	/* Output file */
extern0 int	R_ErrorCon	INI_as(2);	/* Error connection */
LibExtern char *R_TempDir	INI_as(nullptr);	/* Name of per-session dir */
extern0 char   *Sys_TempDir	INI_as(nullptr);	/* Name of per-session dir
						   if set by R itself */
extern0 char	R_StdinEnc[31]  INI_as("");	/* Encoding assumed for stdin */

/* Objects Used In Parsing  */
LibExtern int	R_ParseError	INI_as(0); /* Line where parse error occurred */
extern0 int	R_ParseErrorCol;    /* Column of start of token where parse error occurred */
extern0 SEXP	R_ParseErrorFile;   /* Source file where parse error was seen.  Either a
				       STRSXP or (when keeping srcrefs) a SrcFile ENVSXP */
constexpr size_t PARSE_ERROR_SIZE = 256;	    /* Parse error messages saved here */
LibExtern char	R_ParseErrorMsg[PARSE_ERROR_SIZE] INI_as("");
constexpr size_t PARSE_CONTEXT_SIZE = 256;	    /* Recent parse context kept in a circular buffer */
LibExtern char	R_ParseContext[PARSE_CONTEXT_SIZE] INI_as("");
LibExtern int	R_ParseContextLast INI_as(0); /* last character in context buffer */
LibExtern int	R_ParseContextLine; /* Line in file of the above */

/* Image Dump/Restore */
extern int	R_DirtyImage	INI_as(0);	/* Current image dirty */

/* History */
LibExtern char *R_HistoryFile;	/* Name of the history file */
LibExtern int	R_HistorySize;	/* Size of the history file */
LibExtern int	R_RestoreHistory;	/* restore the history file? */
extern void 	R_setupHistory(void);

/* Warnings/Errors */
extern0 int	R_CollectWarnings INI_as(0);	/* the number of warnings */
extern0 SEXP	R_Warnings;	    /* the warnings and their calls */
extern0 bool	R_ShowErrorMessages INI_as(true);	/* show error messages? */
extern0 SEXP	R_HandlerStack;	/* Condition handler stack */
extern0 SEXP	R_RestartStack;	/* Stack of available restarts */
extern0 Rboolean R_warn_partial_match_args   INI_as(FALSE);
extern0 Rboolean R_warn_partial_match_dollar INI_as(FALSE);
extern0 Rboolean R_warn_partial_match_attr INI_as(FALSE);
extern0 Rboolean R_ShowWarnCalls INI_as(FALSE);
extern0 Rboolean R_ShowErrorCalls INI_as(FALSE);
extern0 int	R_NShowCalls INI_as(50);

LibExtern bool utf8locale  INI_as(false);  /* is this a UTF-8 locale? */
LibExtern Rboolean mbcslocale  INI_as(FALSE);  /* is this a MBCS locale? */
extern0   bool latin1locale INI_as(false); /* is this a Latin-1 locale? */
#ifdef _WIN32
LibExtern unsigned int localeCP  INI_as(1252); /* the locale's codepage */
LibExtern unsigned int systemCP  INI_as(437);  /* the ANSI codepage, GetACP */
extern0   bool WinUTF8out  INI_as(false);  /* Use UTF-8 for output */
extern0   void WinCheckUTF8(void);
#endif

extern const char* OutDec	INI_as(".");  /* decimal point used for output */
extern0 Rboolean R_DisableNLinBrowser	INI_as(FALSE);
extern0 char R_BrowserLastCommand	INI_as('n');

/* Initialization of the R environment when it is embedded */
extern int Rf_initEmbeddedR(int argc, char *argv[]);

/* GUI type */

extern const char	*R_GUIType	INI_as("unknown");
extern bool R_isForkedChild		INI_as(false); /* was this forked? */

extern0 double cpuLimit			INI_as(-1.0);
extern0 double cpuLimit2	       	INI_as(-1.0);
extern0 double cpuLimitValue		INI_as(-1.0);
extern0 double elapsedLimit		INI_as(-1.0);
extern0 double elapsedLimit2		INI_as(-1.0);
extern0 double elapsedLimitValue       	INI_as(-1.0);

void resetTimeLimits(void);

constexpr size_t R_BCNODESTACKSIZE = 200000;

LibExtern R_bcstack_t *R_BCNodeStackTop, *R_BCNodeStackEnd;
extern0 R_bcstack_t *R_BCNodeStackBase;
extern0 R_bcstack_t *R_BCProtTop;
extern0 int R_jit_enabled INI_as(0); /* has to be 0 during R startup */
extern0 int R_compile_pkgs INI_as(0);
extern0 int R_check_constants INI_as(0);
extern0 int R_disable_bytecode INI_as(1); // TODO IVORY: should be set to 0 once we're done with SETJMP/LONGJMP
extern SEXP R_cmpfun1(SEXP); /* unconditional fresh compilation */
extern void R_init_jit_enabled(void);
extern void R_initAssignSymbols(void);
#ifdef R_USE_SIGNALS
extern SEXP R_findBCInterpreterSrcref(RCNTXT *);
#endif
extern SEXP R_getCurrentSrcref();
extern SEXP R_getBCInterpreterExpression();

void R_BCProtReset(R_bcstack_t *);

LibExtern int R_num_math_threads INI_as(1);
LibExtern int R_max_num_math_threads INI_as(1);

/* Pointer type and utilities for dispatch in the methods package */
using R_stdGen_ptr_t = SEXP(*)(SEXP, SEXP, SEXP); /* typedef */
//R_stdGen_ptr_t R_get_standardGeneric_ptr(void); /* get method */
R_stdGen_ptr_t R_set_standardGeneric_ptr(R_stdGen_ptr_t, SEXP); /* set method */
LibExtern SEXP R_MethodsNamespace;
SEXP R_deferred_default_method(void);
SEXP R_set_prim_method(SEXP fname, SEXP op, SEXP code_vec, SEXP fundef, SEXP mlist);
SEXP do_set_prim_method(SEXP op, const char *code_string, SEXP fundef, SEXP mlist);
void R_set_quick_method_check(R_stdGen_ptr_t);
SEXP R_primitive_methods(SEXP op);
SEXP R_primitive_generic(SEXP op);

/* smallest decimal exponent, needed in format.cpp, set in Init_R_Machine */
extern0 int R_dec_min_exponent		INI_as(-308);

/* structure for caching machine accuracy values */
struct AccuracyInfo
{
    int ibeta, it, irnd, ngrd, machep, negep, iexp, minexp, maxexp;
    double eps, epsneg, xmin, xmax;
};

LibExtern AccuracyInfo R_AccuracyInfo;

extern unsigned int max_contour_segments INI_as(25000);

/* used in package utils */
extern bool known_to_be_latin1 INI_as(false);
extern0 bool known_to_be_utf8 INI_as(false);

/* pre-allocated boolean values */
LibExtern SEXP R_TrueValue INI_as(nullptr);
LibExtern SEXP R_FalseValue INI_as(nullptr);
LibExtern SEXP R_LogicalNAValue INI_as(nullptr);

/* for PCRE as from R 3.4.0 */
extern0 bool R_PCRE_use_JIT INI_as(true);
#ifdef HAVE_PCRE2
extern0 int R_PCRE_study INI_as(-2);
#else
extern0 int R_PCRE_study INI_as(10);
#endif
extern0 int R_PCRE_limit_recursion;
} // extern "C"

#ifdef __MAIN__
#undef extern
#undef extern0
#undef LibExtern
#endif
#undef INI_as

#define checkArity(a, b) Rf_checkArityCall(a, b, call)

/*--- FUNCTIONS ------------------------------------------------------ */
#if 0 // #ifndef R_NO_REMAP
# define allocCharsxp		Rf_allocCharsxp
# define asVecSize		Rf_asVecSize
# define asXLength		Rf_asXLength
# define BindDomain		Rf_BindDomain
# define check1arg		Rf_check1arg
# define CheckFormals		Rf_CheckFormals
# define CleanEd		Rf_CleanEd
# define CoercionWarning       	Rf_CoercionWarning
# define ComplexFromInteger	Rf_ComplexFromInteger
# define ComplexFromLogical	Rf_ComplexFromLogical
# define ComplexFromReal	Rf_ComplexFromReal
# define ComplexFromString	Rf_ComplexFromString
# define copyMostAttribNoTs	Rf_copyMostAttribNoTs
# define createS3Vars		Rf_createS3Vars
# define currentTime		Rf_currentTime
# define CustomPrintValue	Rf_CustomPrintValue
# define ddfindVar		Rf_ddfindVar
# define deparse1		Rf_deparse1
# define deparse1m		Rf_deparse1m
# define deparse1w		Rf_deparse1w
# define deparse1line		Rf_deparse1line
# define deparse1s		Rf_deparse1s
# define DispatchGroup		Rf_DispatchGroup
# define DispatchOrEval		Rf_DispatchOrEval
# define DispatchAnyOrEval      Rf_DispatchAnyOrEval
# define EncodeChar             Rf_EncodeChar
# define EncodeRaw              Rf_EncodeRaw
# define EncodeReal2            Rf_EncodeReal2
# define EncodeString           Rf_EncodeString
# define EnsureString 		Rf_EnsureString
# define errorcall_cpy		Rf_errorcall_cpy
# define ErrorMessage		Rf_ErrorMessage
# define evalList		Rf_evalList
# define evalListKeepMissing	Rf_evalListKeepMissing
# define findcontext		Rf_findcontext
# define findVar1		Rf_findVar1
# define get1index		Rf_get1index
# define GetOptionCutoff       	Rf_GetOptionCutoff
# define InitArithmetic		Rf_InitArithmetic
# define InitConnections	Rf_InitConnections
# define InitEd			Rf_InitEd
# define InitFunctionHashing	Rf_InitFunctionHashing
# define InitBaseEnv		Rf_InitBaseEnv
# define InitGlobalEnv		Rf_InitGlobalEnv
# define InitGraphics		Rf_InitGraphics
# define InitMemory		Rf_InitMemory
# define InitNames		Rf_InitNames
# define InitOptions		Rf_InitOptions
# define InitStringHash		Rf_InitStringHash
# define InitS3DefaultTypes	Rf_InitS3DefaultTypes
# define InitTempDir		Rf_InitTempDir
# define InitTypeTables		Rf_InitTypeTables
# define initStack		Rf_initStack
# define IntegerFromComplex	Rf_IntegerFromComplex
# define IntegerFromLogical	Rf_IntegerFromLogical
# define IntegerFromReal	Rf_IntegerFromReal
# define IntegerFromString	Rf_IntegerFromString
# define internalTypeCheck	Rf_internalTypeCheck
# define isValidName		Rf_isValidName
# define ItemName		Rf_ItemName
# define jump_to_toplevel	Rf_jump_to_toplevel
# define KillAllDevices		Rf_KillAllDevices
# define LogicalFromComplex	Rf_LogicalFromComplex
# define LogicalFromInteger	Rf_LogicalFromInteger
# define LogicalFromReal	Rf_LogicalFromReal
# define LogicalFromString	Rf_LogicalFromString
# define mainloop		Rf_mainloop
# define makeSubscript		Rf_makeSubscript
# define markKnown		Rf_markKnown
# define mat2indsub		Rf_mat2indsub
# define matchArg		Rf_matchArg
# define matchArgExact		Rf_matchArgExact
# define matchArgs_NR		Rf_matchArgs_NR
# define matchArgs_RC		Rf_matchArgs_RC
# define matchPar		Rf_matchPar
# define Mbrtowc		Rf_mbrtowc
# define mbtoucs		Rf_mbtoucs
# define mbcsToUcs2		Rf_mbcsToUcs2
# define memtrace_report	Rf_memtrace_report
# define mkCLOSXP		Rf_mkCLOSXP
# define mkFalse		Rf_mkFalse
# define mkPROMISE		Rf_mkPROMISE
# define mkSYMSXP		Rf_mkSYMSXP
# define mkTrue			Rf_mkTrue
# define NewEnvironment		Rf_NewEnvironment
# define OneIndex		Rf_OneIndex
# define onintr			Rf_onintr
# define onintrNoResume		Rf_onintrNoResume
# define onsigusr1              Rf_onsigusr1
# define onsigusr2              Rf_onsigusr2
# define parse			Rf_parse
# define patchArgsByActuals	Rf_patchArgsByActuals
# define PrintInit              Rf_PrintInit
# define PrintDefaults		Rf_PrintDefaults
# define PrintGreeting		Rf_PrintGreeting
# define PrintValueEnv		Rf_PrintValueEnv
# define PrintValueRec		Rf_PrintValueRec
# define PrintVersion		Rf_PrintVersion
# define PrintVersion_part_1	Rf_PrintVersion_part_1
# define PrintVersionString    	Rf_PrintVersionString
# define PrintWarnings		Rf_PrintWarnings
# define promiseArgs		Rf_promiseArgs
# define RealFromComplex	Rf_RealFromComplex
# define RealFromInteger	Rf_RealFromInteger
# define RealFromLogical	Rf_RealFromLogical
# define RealFromString		Rf_RealFromString
# define Seql			Rf_Seql
# define sexptype2char		Rf_sexptype2char
# define Scollate		Rf_Scollate
# define sortVector		Rf_sortVector
# define SrcrefPrompt		Rf_SrcrefPrompt
# define ssort			Rf_ssort
# define StringFromComplex	Rf_StringFromComplex
# define StringFromInteger	Rf_StringFromInteger
# define StringFromLogical	Rf_StringFromLogical
# define StringFromReal		Rf_StringFromReal
# define strIsASCII		Rf_strIsASCII
# define StrToInternal		Rf_StrToInternal
# define strmat2intmat		Rf_strmat2intmat
# define TimeToSeed		Rf_TimeToSeed
# define translateCharFP	Rf_translateCharFP
# define translateCharFP2	Rf_translateCharFP2
# define trCharUTF8      	Rf_trCharUTF8
# define tspgets		Rf_tspgets
# define type2symbol		Rf_type2symbol
# define unbindVar		Rf_unbindVar
# define usemethod		Rf_usemethod
# define ucstomb		Rf_ucstomb
# define ucstoutf8		Rf_ucstoutf8
#ifdef ADJUST_ENVIR_REFCNTS
# define unpromiseArgs		Rf_unpromiseArgs
#endif
# define utf8toucs		Rf_utf8toucs
# define utf8towcs		Rf_utf8towcs
# define vectorIndex		Rf_vectorIndex
# define warningcall		Rf_warningcall
# define WarningMessage		Rf_WarningMessage
# define wcstoutf8		Rf_wcstoutf8
# define wtransChar		Rf_wtransChar
# define yychar			Rf_yychar
# define yylval			Rf_yylval
# define yynerrs		Rf_yynerrs
# define yyparse		Rf_yyparse
#endif //R_NO_REMAP

namespace R
{
    /* The maximum length of input line which will be asked for,
   in bytes, including the terminator */
    constexpr int CONSOLE_BUFFER_SIZE = 4096;
    int R_ReadConsole(const char *, unsigned char *, int, int);
    void R_WriteConsole(const char *, int); /* equivalent to R_WriteConsoleEx(a, b, 0) */
    void R_WriteConsoleEx(const char *, int, int);
    void R_ResetConsole(void);
} // namespace R
extern "C" void R_FlushConsole(void);
extern "C" void R_ClearerrConsole(void);
namespace R
{
    void R_Busy(int);
    int R_ShowFiles(int, const char **, const char **, const char *, bool, const char *);
    int R_EditFiles(int, const char **, const char **, const char *);
    size_t R_ChooseFile(int, char *, size_t);
} // namespace R
extern "C" char *R_HomeDir(void);
namespace R
{
    bool R_FileExists(const char *);
    bool R_HiddenFile(const char *);
    double R_FileMtime(const char *);
    int R_GetFDLimit();
    int R_EnsureFDLimit(int);

    /* environment cell access */
    struct R_varloc_t
    {
        SEXP cell;
    }; /* use struct to prevent casting */

    inline bool R_VARLOC_IS_NULL(const R_varloc_t &loc) { return ((loc).cell == nullptr); }
    R_varloc_t R_findVarLocInFrame(SEXP, SEXP);
    R_varloc_t R_findVarLoc(SEXP rho, SEXP symbol);
    SEXP R_GetVarLocValue(R_varloc_t vl);
    SEXP R_GetVarLocSymbol(R_varloc_t vl);
    Rboolean R_GetVarLocMISSING(R_varloc_t vl);
    void R_SetVarLocValue(R_varloc_t vl, SEXP value);

    /* deparse option bits: change do_dump if more are added */
    enum DeparseOptionBits
    {
        KEEPINTEGER = 1,
        QUOTEEXPRESSIONS = 2,
        SHOWATTRIBUTES = 4,
        USESOURCE = 8,
        WARNINCOMPLETE = 16,
        DELAYPROMISES = 32,
        KEEPNA = 64,
        S_COMPAT = 128,
        HEXNUMERIC = 256,
        DIGITS17 = 512,
        NICE_NAMES = 1024,
        /* common combinations of the above */
        SIMPLEDEPARSE = 0,
        DEFAULTDEPARSE = 1089, /* KEEPINTEGER | KEEPNA | NICE_NAMES, used for calls */
        FORSOURCING = 95       /* not DELAYPROMISES, used in edit.cpp */
    };

    /* Coercion functions */
    int LogicalFromString(SEXP, int &);
    int IntegerFromString(SEXP, int &);
    double RealFromString(SEXP, int &);
    Rcomplex ComplexFromString(SEXP, int &);
    SEXP StringFromLogical(int, int &);
    SEXP StringFromInteger(int, int &);
    SEXP StringFromReal(double, int &);
    SEXP StringFromComplex(Rcomplex, int &);
    SEXP EnsureString(SEXP);

    /* ../../main/print.cpp : */
    struct R_PrintData
    {
        int width;
        int na_width;
        int na_width_noquote;
        int digits;
        int scipen;
        int gap;
        int quote;
        int right;
        int max;
        SEXP na_string;
        SEXP na_string_noquote;
        int useSource;
        int cutoff; // for deparsed language objects
        SEXP env;
        SEXP callArgs;
    };

    /* Other Internally Used Functions */

    SEXP allocCharsxp(R_len_t);
    R_xlen_t asVecSize(SEXP x);
    R_xlen_t asXLength(SEXP x);
    void check1arg(SEXP, SEXP, const char *);
    void Rf_checkArityCall(SEXP op_, SEXP args, SEXP call);
    void CheckFormals(SEXP);
    void R_check_locale(void);
    void CleanEd(void);
    void copyMostAttribNoTs(SEXP inp, SEXP ans);
    SEXP createS3Vars(SEXP dotGeneric, SEXP dotGroup, SEXP dotClass, SEXP dotMethod, SEXP dotGenericCallEnv, SEXP dotGenericDefEnv);
    void CustomPrintValue(SEXP s, SEXP env);
    double currentTime(void);
    SEXP ddfindVar(SEXP symbol, SEXP rho);
    SEXP deparse1(SEXP call, bool abbrev, int opts);
    SEXP deparse1m(SEXP call, bool abbrev, int opts);
    SEXP deparse1w(SEXP call, bool abbrev, int opts);
    SEXP deparse1line(SEXP call, bool abbrev);
    SEXP deparse1line_(SEXP call, bool abbrev, int opts);
    SEXP deparse1s(SEXP call);
    bool DispatchAnyOrEval(SEXP call, SEXP op, const char *generic, SEXP args, SEXP rho, SEXP *ans, int dropmissing, int argsevald);
    bool DispatchOrEval(SEXP call, SEXP op, const char *generic, SEXP args, SEXP rho, SEXP *ans, int dropmissing, int argsevald);
    bool DispatchGroup(const char *group, SEXP call, SEXP op, SEXP args, SEXP rho, SEXP *ans);
    R_xlen_t dispatch_xlength(SEXP x, SEXP call, SEXP rho);
    R_len_t dispatch_length(SEXP x, SEXP call, SEXP rho);
    SEXP dispatch_subset2(SEXP, R_xlen_t, SEXP, SEXP);
    SEXP evalList(SEXP el, SEXP rho, SEXP call, int n);
    SEXP evalListKeepMissing(SEXP, SEXP);
    NORET void findcontext(int mask, SEXP env, SEXP val);
    SEXP findVar1(SEXP symbol, SEXP rho, SEXPTYPE mode, int inherits_);
    R_xlen_t get1index(SEXP, SEXP, R_xlen_t, int, int, SEXP);
    int GetOptionCutoff(void);
    void InitArithmetic(void);
    void InitConnections(void);
    void InitEd(void);
    void InitFunctionHashing(void);
    void InitBaseEnv(void);
    void InitGlobalEnv(void);
    bool R_current_trace_state(void);
    bool R_current_debug_state(void);
    bool R_has_methods(SEXP);
    void R_InitialData(void);
    std::pair<bool, SEXP> R_possible_dispatch(SEXP, SEXP, SEXP, SEXP, bool);
    bool inherits2(SEXP, const char *);
    void InitGraphics(void);
    void InitMemory(void);
    void InitNames(void);
    void InitOptions(void);
    void InitStringHash(void);
    void Init_R_Variables(SEXP);
    void InitTempDir(void);
    void R_reInitTempDir(int);
    void InitTypeTables(void);
    void initStack(void);
    void InitS3DefaultTypes(void);
    void internalTypeCheck(SEXP, SEXP, SEXPTYPE);
    bool isMethodsDispatchOn(void);
    bool isValidName(const char *);
} // namespace R
void KillAllDevices(void);
namespace R
{
    SEXP makeSubscript(SEXP x, SEXP s, R_xlen_t &stretch, SEXP call);
    SEXP markKnown(const char *const s, SEXP ref);
    SEXP mat2indsub(SEXP dims, SEXP s, SEXP call);
    SEXP matchArg(SEXP tag, SEXP *list);
    SEXP matchArgExact(SEXP tag, SEXP *list);
    SEXP matchArgs_NR(SEXP, SEXP, SEXP);
    SEXP matchArgs_RC(SEXP formals, SEXP supplied, SEXP call);
    SEXP matchPar(const char *tag, SEXP *list);
    void memtrace_report(void *old, void *_new);
    SEXP mkCLOSXP(SEXP formals, SEXP body, SEXP rho);
    SEXP mkFalse(void);
    SEXP mkPRIMSXP(int offset, bool eval);
    SEXP mkPROMISE(SEXP expr, SEXP rho);
    SEXP R_mkEVPROMISE(SEXP expr, SEXP val);
    SEXP R_mkEVPROMISE_NR(SEXP expr, SEXP val);
    SEXP mkSYMSXP(SEXP name, SEXP value);
    SEXP mkTrue(void);
    const char *R_nativeEncoding(void);
    SEXP NewEnvironment(SEXP namelist, SEXP valuelist, SEXP rho);
    RETSIGTYPE onsigusr1(int);
    RETSIGTYPE onsigusr2(int);
    R_xlen_t OneIndex(SEXP x, SEXP s, R_xlen_t nx, int partial, SEXP *newname, int pos, SEXP call);
    SEXP patchArgsByActuals(SEXP formals, SEXP supplied, SEXP cloenv);
    void PrintInit(R_PrintData &data, SEXP env);
    void PrintDefaults(void);
    void PrintGreeting(void);
    void PrintValueEnv(SEXP s, SEXP env);
    void PrintValueRec(SEXP s, R_PrintData &data);
    void PrintVersion(char *, size_t len);
    void PrintVersion_part_1(char *, size_t len);
    void PrintVersionString(char *, size_t len);
    void PrintWarnings(const char *hdr = nullptr);
    SEXP promiseArgs(SEXP el, SEXP rho);
    void Rcons_vprintf(const char *format, va_list arg);
    SEXP R_data_class(SEXP obj, bool singleString);
    SEXP R_data_class2(SEXP obj);
    char *R_LibraryFileName(const char *file, char *buf, size_t bsize);
    SEXP R_LoadFromFile(FILE *fp, int startup);
    SEXP R_NewHashedEnv(SEXP enclos, SEXP size);
    int R_Newhashpjw(const char *s);
    FILE *R_OpenLibraryFile(const char *file);
    SEXP R_Primitive(const char *primname);
    void R_SaveToFile(SEXP obj, FILE *fp, int ascii);
    void R_SaveToFileV(SEXP obj, FILE *fp, int ascii, int version);
    bool R_seemsOldStyleS4Object(SEXP object);
    int R_SetOptionWarn(int w);
    int R_SetOptionWidth(int w);
    void R_getProcTime(double *data);
    bool R_isMissing(SEXP symbol, SEXP rho);
    const char *sexptype2char(SEXPTYPE type);
    void sortVector(SEXP s, bool decreasing);
    void SrcrefPrompt(const char *prefix, SEXP srcref);
    void ssort(SEXP *x, int n);
    int StrToInternal(const char *s);
    SEXP strmat2intmat(SEXP s, SEXP dnamelist, SEXP call);
    SEXP substituteList(SEXP el, SEXP rho);
    unsigned int TimeToSeed(void);
    SEXP tspgets(SEXP vec, SEXP val);
    SEXP type2symbol(SEXPTYPE t);
    void unbindVar(SEXP symbol, SEXP rho);

#ifdef ADJUST_ENVIR_REFCNTS
    void unpromiseArgs(SEXP pargs);
#endif
    SEXP R_LookupMethod(SEXP method, SEXP rho, SEXP callrho, SEXP defrho);
    bool usemethod(const char *generic, SEXP obj, SEXP call, SEXP args, SEXP rho, SEXP callrho, SEXP defrho, SEXP *ans);
    SEXP vectorIndex(SEXP x, SEXP thesub, int start, int stop, int pok, SEXP call, bool dup);

    /* ../main/bind.cpp */
    SEXP ItemName(SEXP names, R_xlen_t i);

    /* ../main/errors.cpp : */
    NORET void errorcall_cpy(SEXP, const char *, ...);
    NORET void ErrorMessage(SEXP, int, ...);
    void WarningMessage(SEXP, int, ...);
    SEXP R_GetTraceback(int);     // including deparse()ing
    SEXP R_GetTracebackOnly(int); // no        deparse()ing

    R_size_t R_GetMaxVSize(void);
    void R_SetMaxVSize(R_size_t);
    R_size_t R_GetMaxNSize(void);
    void R_SetMaxNSize(R_size_t);
    R_size_t R_Decode2Long(char *p, int &ierr);

    void R_expand_binding_value(SEXP b);

    void R_args_enable_refcnt(SEXP args);

    /* ../main/devices.cpp, used in memory.cpp, gnuwin32/extra.cpp */
    constexpr int R_MaxDevices = 64;

    /* ../../main/printutils.cpp : */
    enum Rprt_adj
    {
        Rprt_adj_left = 0,
        Rprt_adj_right = 1,
        Rprt_adj_centre = 2,
        Rprt_adj_none = 3
    };

    int Rstrlen(SEXP, int);
    const char *EncodeRaw(Rbyte, const char *);
    const char *EncodeString(SEXP, int, int, Rprt_adj);
    const char *EncodeReal2(double, int, int, int);
    const char *EncodeChar(SEXP);

    /* main/raw.cpp */
    int mbrtoint(int &w, const char *s);

    /* main/sort.cpp */
    void orderVector1(int *indx, int n, SEXP key, Rboolean nalast,
                      Rboolean decreasing, SEXP rho);

    /* main/subset.cpp */
    SEXP R_subset3_dflt(SEXP x, SEXP input, SEXP call);

    /* main/subassign.cpp */
    SEXP R_subassign3_dflt(SEXP call, SEXP x, SEXP nlist, SEXP val);
} // namespace R

#include <wchar.h>

namespace R
{
    /* main/util.cpp */
    NORET void UNIMPLEMENTED_TYPE(const char *s, SEXP x);
    NORET void UNIMPLEMENTED_TYPE(const char *s, const SEXPTYPE t);
    bool strIsASCII(const char *str);
    int utf8clen(char c);
    int Rf_AdobeSymbol2ucs2(int n);
    double R_strtod5(const char *str, char **endptr, char dec, Rboolean NA, int exact);
    SEXP R_listCompact(SEXP s, bool keep_initial);

    using R_ucs2_t = unsigned short;
    size_t mbcsToUcs2(const char *in, R_ucs2_t *out, int nout, int enc);
    size_t utf8toucs(wchar_t *wc, const char *s);
    size_t utf8towcs(wchar_t *wc, const char *s, size_t n);
    size_t ucstomb(char *s, const unsigned int wc);
    //size_t ucstoutf8(char *s, const unsigned int wc);
    size_t mbtoucs(unsigned int *wc, const char *s, size_t n);
    size_t wcstoutf8(char *s, const wchar_t *wc, size_t n);

    const wchar_t *wtransChar(SEXP x); /* from sysutils.cpp */

    inline void mbs_init(mbstate_t *x) { memset(x, 0, sizeof(mbstate_t)); }
    size_t Mbrtowc(wchar_t *wc, const char *s, size_t n, mbstate_t *ps);
    bool mbcsValid(const char *str);
    char *mbcsTruncateToValid(char *s);
    bool utf8Valid(const char *str);
    char *Rf_strchr(const char *s, int c);
    char *Rf_strrchr(const char *s, int c);
    int Rvsnprintf_mbcs(char *buf, size_t size, const char *format, va_list ap);
    int Rsnprintf_mbcs(char *str, size_t size, const char *format, ...);

    SEXP fixup_NaRm(SEXP args);             /* summary.cpp */
    void invalidate_cached_recodings(void); /* from sysutils.cpp */
    void resetICUcollator(bool disable);    /* from util.cpp */
    void dt_invalidate_locale();            /* from Rstrptime.h */

    extern int R_OutputCon;                          /* from connections.cpp */
    extern int R_InitReadItemDepth, R_ReadItemDepth; /* from serialize.cpp */

    void get_current_mem(size_t &, size_t &, size_t &); /* from memory.cpp */
    unsigned long get_duplicate_counter(void);          /* from duplicate.cpp */
    void reset_duplicate_counter(void);                 /* from duplicate.cpp */
    void BindDomain(char *);                         /* from main.cpp */
    extern bool LoadInitFile;                           /* from startup.cpp */

    // Unix and Windows versions
    double R_getClockIncrement(void);
    void InitDynload(void);
} // namespace R

void R_CleanTempDir(void);

namespace R
{
#ifdef _WIN32
    void R_fixslash(char *s);
    void R_fixbackslash(char *s);
    wchar_t *filenameToWchar(const SEXP fn, const Rboolean expand);

#if defined(SUPPORT_UTF8_WIN32)
#define Mbrtowc(a, b, c, d) Rmbrtowc(a, b)
#define Rf_wcrtomb(a, b, c) Rwcrtomb(a, b)
#define Rf_mbstowcs(a, b, c) Rmbstowcs(a, b, c)
#define Rf_wcstombs(a, b, c) Rwcstombs(a, b, c)
    size_t Rmbrtowc(wchar_t *wc, const char *s);
    size_t Rwcrtomb(char *s, const wchar_t wc);
    size_t Rmbstowcs(wchar_t *wc, const char *s, size_t n);
    size_t Rwcstombs(char *s, const wchar_t *wc, size_t n);
#endif
#endif

    FILE *RC_fopen(const SEXP fn, const char *mode, const Rboolean expand);
    bool Seql(SEXP a, SEXP b);
    int Scollate(SEXP a, SEXP b);

    double R_strtod4(const char *str, char **endptr, char dec, Rboolean NA);
} // namespace R

double R_strtod(const char *str, char **endptr);
double R_atof(const char *str);

namespace R
{
    /* unix/sys-std.cpp, main/options.cpp */
    void set_rl_word_breaks(const char *str);

    /* From localecharset.cpp */
    extern const char *locale2charset(const char *);
} // namespace R

/*
   alloca is neither C99 nor POSIX.

   It might be better to try alloca.h first, see
   https://www.gnu.org/software/autoconf/manual/autoconf-2.60/html_node/Particular-Functions.html
*/
#ifdef __GNUC__
// This covers GNU, Clang and Intel compilers
// The undef is needed in case some other header, e.g. malloc.h, already did this
# undef alloca
# define alloca(x) __builtin_alloca((x))
#else
# ifdef HAVE_ALLOCA_H
// Needed for native compilers on Solaris and AIX
#  include <alloca.h>
# endif
// it might have been defined via some other standard header, e.g. stdlib.h
# if !HAVE_DECL_ALLOCA
#  include <stddef.h> // for size_t
extern void *alloca(size_t);
# endif
#endif

/* Required by C99, but might be slow */
#include <R_ext/Ldouble.h>

/* int_fast64_t is required by C99/C11
   Alternative would be to use intmax_t.
 */
#ifdef HAVE_INT64_T
# define LONG_INT int64_t
# define LONG_INT_MAX INT64_MAX
#elif defined(HAVE_INT_FAST64_T)
# define LONG_INT int_fast64_t
# define LONG_INT_MAX INT_FAST64_MAX
#endif

namespace R
{
    Rboolean R_access_X11(void); /* from src/unix/X11.cpp */
    SEXP R_execMethod(SEXP op, SEXP rho);
    SEXP Rf_csduplicated(SEXP x); /* from unique.cpp */

    // for reproducibility for now: use exp10 or pown later if accurate enough.
    template <typename T>
    inline auto Rexp10(const T &x) { return std::pow(10.0, x); }
} // namespace R

#endif /* DEFN_H_ */
/*
 *- Local Variables:
 *- page-delimiter: "^/\\*---"
 *- End:
 */
