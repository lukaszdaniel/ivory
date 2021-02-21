/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998--2020  The R Core Team.
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
 *
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

/*
 *	This code implements a non-moving generational collector
 *      with two or three generations.
 *
 *	Memory allocated by R_alloc is maintained in a stack.  Code
 *	that R_allocs memory must use vmaxget and vmaxset to obtain
 *	and reset the stack pointer.
 */

/** @file memory.cpp
 *
 * Memory management, garbage collection, and memory profiling.
 */

#define USE_RINTERNALS
#define COMPILING_MEMORY_C
#define R_USE_SIGNALS

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#define R_NO_REMAP

#include <cstdarg>
#include <iostream>
#include <R_ext/RS.h> /* for S4 allocation */
#include <CXXR/GCEdge.hpp>
#include <CXXR/GCManager.hpp>
#include <CXXR/GCRoot.hpp>
#include <CXXR/ProtectStack.hpp>
#include <CXXR/GCNode.hpp>
#include <CXXR/MemoryBank.hpp>
#include <CXXR/String.hpp>
#include <CXXR/Environment.hpp>
#include <CXXR/Expression.hpp>
#include <CXXR/DottedArgs.hpp>
#include <CXXR/ByteCode.hpp>
#include <CXXR/Promise.hpp>
#include <CXXR/Closure.hpp>
#include <CXXR/JMPException.hpp>
#include <CXXR/IntVector.hpp>
#include <CXXR/LogicalVector.hpp>
#include <CXXR/RealVector.hpp>
#include <CXXR/ComplexVector.hpp>
#include <CXXR/RawVector.hpp>
#include <CXXR/StringVector.hpp>
#include <CXXR/ExpressionVector.hpp>
#include <CXXR/ListVector.hpp>
#include <CXXR/UncachedString.hpp>
#include <CXXR/WeakRef.hpp>
#include <CXXR/ExternalPointer.hpp>
#include <CXXR/PairList.hpp>
#include <CXXR/RAltRep.hpp>
#include <CXXR/S4Object.hpp>
#include <CXXR/SEXP_downcast.hpp>
#include <R_ext/Print.h>

/* Declarations for Valgrind.

   These are controlled by the
     --with-valgrind-instrumentation=
   option to configure, which sets VALGRIND_LEVEL to the
   supplied value (default 0) and defines NVALGRIND if
   the value is 0.

   level 0 is no additional instrumentation
   level 1 marks uninitialized numeric, logical, integer, raw,
	   complex vectors and R_alloc memory
   level 2 marks the data section of vector nodes as inaccessible
	   when they are freed.

   level 3 was withdrawn in R 3.2.0.

   It may be necessary to define NVALGRIND for a non-gcc
   compiler on a supported architecture if it has different
   syntax for inline assembly language from gcc.

   For Win32, Valgrind is useful only if running under Wine.
*/
#ifdef _WIN32
# ifndef USE_VALGRIND_FOR_WINE
#define NVALGRIND 1
#endif
#endif


#ifndef VALGRIND_LEVEL
#define VALGRIND_LEVEL 0
#endif

#ifndef NVALGRIND
# ifdef HAVE_VALGRIND_MEMCHECK_H
#  include "valgrind/memcheck.h"
# else
// internal version of headers
#  include "vg/memcheck.h"
# endif
#endif


#include <Defn.h>
#include <Localization.h>
#include <Rinterface.h>
#include <Internal.h>
#include <R_ext/GraphicsEngine.h> /* GEDevDesc, GEgetDevice */
#include <R_ext/Rdynload.h>
#include <R_ext/Rallocators.h> /* for R_allocator_t structure */
#include <R_ext/Minmax.h>
#include <Rmath.h> // R_pow_di
#include <Print.h> // R_print

using namespace std;
using namespace R;
using namespace CXXR;

#if defined(_WIN32)
extern void *Rm_malloc(size_t n);
extern void *Rm_calloc(size_t n_elements, size_t element_size);
extern void Rm_free(void * p);
extern void *Rm_realloc(void * p, size_t n);
#define calloc Rm_calloc
#define malloc Rm_malloc
#define realloc Rm_realloc
#define free Rm_free
#endif

/* malloc uses size_t.  We are assuming here that size_t is at least
   as large as unsigned long.  Changed from int at 1.6.0 to (i) allow
   2-4Gb objects on 32-bit system and (ii) objects limited only by
   length on a 64-bit system.
*/

/* Report error encountered during garbage collection where for detecting
   problems it is better to abort, but for debugging (or some production runs,
   where external validation of results is possible) it may be preferred to
   continue. Configurable via _R_GC_FAIL_ON_ERROR_. Typically these problems
   are due to memory corruption.
*/
static void gc_error(const char *msg)
{
    if (GCManager::gc_fail_on_error())
        R_Suicide(msg);
    else if (R_in_gc)
        REprintf(msg);
    else
        error(msg);
}

/* These are used in profiling to separate out time in GC */
int R_gc_running() { return R_in_gc; }

#ifdef TESTING_WRITE_BARRIER
#define PROTECTCHECK
#endif

#ifdef PROTECTCHECK
/* This is used to help detect unprotected SEXP values.  It is most
   useful if the strict barrier is enabled as well. The strategy is:

       All GCs are full GCs

       New nodes are marked as NEWSXP

       After a GC all free nodes that are not of type NEWSXP are
       marked as type FREESXP

       Most calls to accessor functions check their SEXP inputs and
       SEXP outputs with CHK() to see if a reachable node is a
       FREESXP and signal an error if a FREESXP is found.

   Combined with GC torture this can help locate where an unprotected
   SEXP is being used.

   This approach will miss cases where an unprotected node has been
   re-allocated.  For these cases it is possible to set
   s_gc_inhibit_release to TRUE.  FREESXP nodes will not be reallocated,
   or large ones released, until s_gc_inhibit_release is set to FALSE
   again.  This will of course result in memory growth and should be
   used with care and typically in combination with OS mechanisms to
   limit process memory usage.  LT */

/* Before a node is marked as a FREESXP by the collector the previous
   type is recorded.  For now using the LEVELS field seems
   reasonable.  */
inline SEXPTYPE OLDTYPE(SEXP x) { return SEXPTYPE(LEVELS(x)); }
inline void SETOLDTYPE(SEXP x, int v) { SETLEVELS(x, v); }

inline static SEXP CHK(SEXP x)
{
    /* **** NULL check because of R_CurrentExpr */
    if (x && TYPEOF(x) == FREESXP)
        error(_("unprotected object (%p) encountered (was %s)"), x, sexptype2char(OLDTYPE(x)));
    return x;
}
#else
#define CHK(x) x
#endif

/**
 * @brief Translate SEXPTYPE enum to a character string
 * 
 * @param type SEXP object's type
 * 
 * @return name of the type
 * 
 * @note also called from typeName() in inspect.cpp
 */

HIDDEN
const char *R::sexptype2char(const SEXPTYPE type) {
    switch (type) {
    case NILSXP:	return "NILSXP";
    case SYMSXP:	return "SYMSXP";
    case LISTSXP:	return "LISTSXP";
    case CLOSXP:	return "CLOSXP";
    case ENVSXP:	return "ENVSXP";
    case PROMSXP:	return "PROMSXP";
    case LANGSXP:	return "LANGSXP";
    case SPECIALSXP:	return "SPECIALSXP";
    case BUILTINSXP:	return "BUILTINSXP";
    case CHARSXP:	return "CHARSXP";
    case LGLSXP:	return "LGLSXP";
    case INTSXP:	return "INTSXP";
    case REALSXP:	return "REALSXP";
    case CPLXSXP:	return "CPLXSXP";
    case STRSXP:	return "STRSXP";
    case DOTSXP:	return "DOTSXP";
    case ANYSXP:	return "ANYSXP";
    case VECSXP:	return "VECSXP";
    case EXPRSXP:	return "EXPRSXP";
    case BCODESXP:	return "BCODESXP";
    case EXTPTRSXP:	return "EXTPTRSXP";
    case WEAKREFSXP:	return "WEAKREFSXP";
    case S4SXP:		return "S4SXP";
    case RAWSXP:	return "RAWSXP";
    case NEWSXP:	return "NEWSXP"; /* should never happen */
    case FREESXP:	return "FREESXP";
    case SINGLESXP: return "SINGLEEXP";
    case FUNSXP:     return "FUNSXP";
    case ALTREP_SXP: return "ALTREP_SXP";
    case ATTRLISTSXP: return "ATTRLISTSXP";
    case ATTRLANGSXP: return "ATTRLANGSXP";
    case BASEENV_SXP: return "BASEENV_SXP";
    case EMPTYENV_SXP: return "EMPTYENV_SXP";
    case BCREPREF:    return "BCREPREF";
    case BCREPDEF:    return "BCREPDEF";
    case GENERICREFSXP: return "GENERICREFSXP";
    case CLASSREFSXP: return "CLASSREFSXP";
    case PERSISTSXP:  return "PERSISTSXP";
    case PACKAGESXP:  return "PACKAGESXP";
    case NAMESPACESXP: return "NAMESPACESXP";
    case BASENAMESPACE_SXP: return "BASENAMESPACE_SXP";
    case MISSINGARG_SXP: return "MISSINGARG_SXP";
    case UNBOUNDVALUE_SXP: return "UNBOUNDVALUE_SXP";
    case GLOBALENV_SXP:  return "GLOBALENV_SXP";
    case NILVALUE_SXP:  return "NILVALUE_SXP";
    case REFSXP:     return "REFSXP";
    default:		return "<unknown>";
    }
}

#ifdef R_MEMORY_PROFILING
static void R_ReportAllocation(R_size_t);
#endif

static void init_gc_grow_settings()
{
    char *arg;

    arg = getenv("R_GC_MEM_GROW");
    if (arg) {
	int which = (int) atof(arg);
	switch (which) {
	case 0: /* very conservative -- the SMALL_MEMORY settings */
        GCManager::setGCGrowIncrParameters(0.0, 0.0);
	    break;
	case 1: /* default */
	    break;
	case 2: /* somewhat aggressive */
        GCManager::setGCGrowIncrParameters(0.3, 0.3);
	    break;
	case 3: /* more aggressive */
        GCManager::setGCGrowIncrParameters(0.4, 0.4);
        GCManager::setGCGrowParameters(0.5, 0.5);
	    break;
	}
    }
    arg = getenv("R_GC_GROWFRAC");
    if (arg) {
	double frac = atof(arg);
	if (0.35 <= frac && frac <= 0.75) {
        GCManager::setGCGrowParameters(frac, frac);
	}
    }
    arg = getenv("R_GC_GROWINCRFRAC");
    if (arg) {
	double frac = atof(arg);
	if (0.05 <= frac && frac <= 0.80) {
        GCManager::setGCGrowIncrParameters(frac, frac);
	}
    }
    arg = getenv("R_GC_NGROWINCRFRAC");
    if (arg) {
	double frac = atof(arg);
	if (0.05 <= frac && frac <= 0.80)
        GCManager::setGCGrowIncrParameters(frac, 0.2);
    }
    arg = getenv("R_GC_VGROWINCRFRAC");
    if (arg) {
	double frac = atof(arg);
	if (0.05 <= frac && frac <= 0.80)
        GCManager::setGCGrowIncrParameters(0.2, frac);
    }
}

/* Maximal Heap Limits.  These variables contain upper limits on the
   heap sizes.  They could be made adjustable from the R level,
   perhaps by a handler for a recoverable error.

   Access to these values is provided with reader and writer
   functions; the writer function insures that the maximal values are
   never set below the current ones. */

HIDDEN R_size_t R::R_GetMaxVSize(void)
{
    if (GCManager::maxTriggerLevel() == R_SIZE_T_MAX) return R_SIZE_T_MAX;
    return GCManager::maxTriggerLevel() * sizeof(VECREC);
}

HIDDEN void R::R_SetMaxVSize(R_size_t size)
{
    if (size == R_SIZE_T_MAX) return;
    if (size/sizeof(VECREC) >= GCManager::triggerLevel()) GCManager::setMaxTriggerLevel((size + 1)/sizeof(VECREC));
}

HIDDEN R_size_t R::R_GetMaxNSize(void)
{
    return GCManager::maxNodeTriggerLevel();
}

HIDDEN void R::R_SetMaxNSize(R_size_t size)
{
    if (size >= GCManager::nodeTriggerLevel()) GCManager::setMaxNodeTriggerLevel(size);
}

HIDDEN SEXP do_maxVSize(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    constexpr double MB = 1048576.0;
    double newval = asReal(CAR(args));

    if (newval > 0)
    {
        if (newval == R_PosInf)
            GCManager::setMaxTriggerLevel(R_SIZE_T_MAX);
        else
            R_SetMaxVSize((R_size_t)(newval * MB));
    }

    if (GCManager::maxTriggerLevel() == R_SIZE_T_MAX)
        return ScalarReal(R_PosInf);
    else
        return ScalarReal(R_GetMaxVSize() / MB);
}

HIDDEN SEXP do_maxNSize(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    double newval = asReal(CAR(args));

    if (newval > 0)
    {
        if (newval == R_PosInf)
            GCManager::setMaxNodeTriggerLevel(R_SIZE_T_MAX);
        else
            R_SetMaxNSize((R_size_t)newval);
    }

    if (GCManager::maxNodeTriggerLevel() == R_SIZE_T_MAX)
        return ScalarReal(R_PosInf);
    else
        return ScalarReal(R_GetMaxNSize());
}

namespace
{
    /* Miscellaneous Globals. */
    SEXP R_PreciousList = nullptr; /* List of Persistent Objects */
} // namespace

/* R interface function */

HIDDEN SEXP do_regFinaliz(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int onexit;

    checkArity(op, args);

    if (TYPEOF(CAR(args)) != ENVSXP && TYPEOF(CAR(args)) != EXTPTRSXP)
        error(_("first argument must be environment or external pointer"));
    if (TYPEOF(CADR(args)) != CLOSXP)
        error(_("second argument must be a function"));

    onexit = asLogical(CADDR(args));
    if (onexit == NA_LOGICAL)
        error(_("third argument must be 'TRUE' or 'FALSE'"));

    R_RegisterFinalizerEx(CAR(args), CADR(args), (Rboolean)onexit);
    return R_NilValue;
}

/* The Generational Collector. */

namespace
{
    inline void MARK_THRU(GCNode::const_visitor *marker, GCNode *node)
    {
        if (node)
        {
            node->conductVisitor(marker);
        }
    }
} // namespace

// The MARK_THRU invocations below could be eliminated by
// encapsulating the pointers concerned in GCRoot<> objects declared
// at file/global/static scope.

void GCNode::gc(unsigned int num_old_gens_to_collect)
{
    // std::cerr << "GCNode::gc(" << num_old_gens_to_collect << ")\n";
    GCNode::check();
    // std::cerr << "Precheck completed OK\n";

    propagateAges();

    GCNode::Marker marker(num_old_gens_to_collect + 1);
    GCRootBase::visitRoots(&marker);
    ProtectStack::visitRoots(&marker);
    MARK_THRU(&marker, R_BlankScalarString);	        /* Builtin constants */

    MARK_THRU(&marker, R_Warnings);	           /* Warnings, if any */
    MARK_THRU(&marker, R_ReturnedValue);

    MARK_THRU(&marker, R_HandlerStack);          /* Condition handler stack */
    MARK_THRU(&marker, R_RestartStack);          /* Available restarts stack */

    MARK_THRU(&marker, R_BCbody);                /* Current byte code object */
    MARK_THRU(&marker, R_Srcref);                /* Current source reference */

    MARK_THRU(&marker, R_TrueValue);
    MARK_THRU(&marker, R_FalseValue);
    MARK_THRU(&marker, R_LogicalNAValue);

    MARK_THRU(&marker, R_print.na_string);
    MARK_THRU(&marker, R_print.na_string_noquote);

    if (R_SymbolTable)             /* in case of GC during startup */
        for (int i = 0; i < HSIZE; i++)
        { /* Symbol table */
            MARK_THRU(&marker, R_SymbolTable[i]);
            for (RObject *s = R_SymbolTable[i]; s != R_NilValue; s = CDR(s))
                if (ATTRIB(CAR(s)) != R_NilValue)
                    gc_error("****found a symbol with attributes\n");
        }

    if (R_CurrentExpr) /* Current expression */
        MARK_THRU(&marker, R_CurrentExpr);

    for (int i = 0; i < R_MaxDevices; i++)
    { /* Device display lists */
        GEDevDesc *gdd = GEgetDevice(i);
        if (gdd)
        {
            MARK_THRU(&marker, gdd->displayList);
            MARK_THRU(&marker, gdd->savedSnapshot);
            if (gdd->dev)
                MARK_THRU(&marker, gdd->dev->eventEnv);
        }
    }

    for (RCNTXT *ctxt = R_GlobalContext ; ctxt; ctxt = ctxt->nextContext()) {
	MARK_THRU(&marker, ctxt->onExit());       /* on.exit expressions */
	MARK_THRU(&marker, ctxt->getPromiseArgs());	   /* promises supplied to closure */
	MARK_THRU(&marker, ctxt->getCallFun());       /* the closure called */
	MARK_THRU(&marker, ctxt->getSysParent());     /* calling environment */
	MARK_THRU(&marker, ctxt->getCall());          /* the call */
	MARK_THRU(&marker, ctxt->workingEnvironment());        /* the closure environment */
	MARK_THRU(&marker, ctxt->getBCBody());        /* the current byte code object */
	MARK_THRU(&marker, ctxt->getHandlerStack());  /* the condition handler stack */
	MARK_THRU(&marker, ctxt->getRestartStack());  /* the available restarts stack */
	MARK_THRU(&marker, ctxt->getSrcRef());	   /* the current source reference */
	MARK_THRU(&marker, ctxt->getReturnValue());   /* For on.exit calls */
    }

    MARK_THRU(&marker, R_PreciousList);

    for (R_bcstack_t *sp = R_BCNodeStackBase; sp < R_BCNodeStackTop; sp++)
    {
        if (sp->tag == RAWMEM_TAG)
            sp += sp->u.ival;
        else if (sp->tag == 0 || R_bcstack_t::IS_PARTIAL_SXP_TAG(sp->tag))
            MARK_THRU(&marker, sp->u.sxpval);
    }

    /* identify weakly reachable nodes */
    WeakRef::markThru(num_old_gens_to_collect + 1);

    sweep(num_old_gens_to_collect + 1);

    // std::cerr << "Finishing garbage collection\n";
    GCNode::check();
    // std::cerr << "Postcheck completed OK\n";
}

/* public interface for controlling GC torture settings */
/* maybe, but in no header */
void R_gc_torture(int gap, int wait, Rboolean inhibit)
{
    if (gap != NA_INTEGER && gap >= 0)
        GCManager::setTortureParameters(gap, gap, false);
    if (gap > 0)
    {
        if (wait != NA_INTEGER && wait > 0)
            GCManager::setTortureParameters(gap, wait, false);
    }
#ifdef PROTECTCHECK
    if (gap > 0)
    {
        if (inhibit != NA_LOGICAL)
            GCManager::setInhibitor(inhibit);
    }
    else
        GCManager::setInhibitor(false);
#endif
}

HIDDEN SEXP do_gctorture(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int gap;
    SEXP old = ScalarLogical(GCManager::gc_force_wait() > 0);

    checkArity(op, args);

    if (isLogical(CAR(args)))
    {
        Rboolean on = (Rboolean)asLogical(CAR(args));
        if (on == NA_LOGICAL)
            gap = NA_INTEGER;
        else if (on)
            gap = 1;
        else
            gap = 0;
    }
    else
        gap = asInteger(CAR(args));

    R_gc_torture(gap, 0, FALSE);

    return old;
}

HIDDEN SEXP do_gctorture2(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int gap, wait;
    Rboolean inhibit;
    int old = GCManager::gc_force_gap();

    checkArity(op, args);
    gap = asInteger(CAR(args));
    wait = asInteger(CADR(args));
    inhibit = (Rboolean)asLogical(CADDR(args));
    R_gc_torture(gap, wait, inhibit);

    return ScalarInteger(old);
}

/* initialize gctorture settings from environment variables */
static void init_gctorture(void)
{
    char *arg = getenv("R_GCTORTURE");
    if (arg) {
	int gap = atoi(arg);
	if (gap > 0) {
        GCManager::setTortureParameters(gap, gap, false);
	    arg = getenv("R_GCTORTURE_WAIT");
	    if (arg) {
		int wait = atoi(arg);
		if (wait > 0)
            GCManager::setTortureParameters(gap, wait, false);
	    }
#ifdef PROTECTCHECK
	    arg = getenv("R_GCTORTURE_INHIBIT_RELEASE");
	    if (arg) {
		int inhibit = atoi(arg);
		if (inhibit > 0) GCManager::setInhibitor(true);
		else GCManager::setInhibitor(false);
	    }
#endif
	}
    }
}

HIDDEN SEXP do_gcinfo(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    std::ostream *report_os = GCManager::setReporting(nullptr);
    int want_reporting = Rf_asLogical(CAR(args));
    checkArity(op, args);
    if (want_reporting != NA_LOGICAL)
        GCManager::setReporting(want_reporting ? &std::cerr : nullptr);
    else
        GCManager::setReporting(report_os);
    return Rf_ScalarLogical(report_os != nullptr);
}

/* reports memory use to profiler in eval.cpp */

HIDDEN void R::get_current_mem(size_t &smallvsize,
                               size_t &largevsize,
                               size_t &nodes)
{
    smallvsize = 0;
    largevsize = MemoryBank::bytesAllocated();
    nodes = GCNode::numNodes();
}

namespace
{
    void post_gc_checks()
    {
        /* sanity check on logical scalar values */
        if (R_TrueValue && LOGICAL(R_TrueValue)[0] != TRUE)
        {
            LOGICAL(R_TrueValue)[0] = TRUE;
            gc_error(_("internal TRUE value has been modified"));
        }
        if (R_FalseValue && LOGICAL(R_FalseValue)[0] != FALSE)
        {
            LOGICAL(R_FalseValue)[0] = FALSE;
            gc_error(_("internal FALSE value has been modified"));
        }
        if (R_LogicalNAValue &&
            LOGICAL(R_LogicalNAValue)[0] != NA_LOGICAL)
        {
            LOGICAL(R_LogicalNAValue)[0] = NA_LOGICAL;
            gc_error(_("internal logical NA value has been modified"));
        }
    }
} // namespace

HIDDEN SEXP do_gc(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    std::ostream* report_os = GCManager::setReporting(Rf_asLogical(CAR(args)) ? &std::cerr : nullptr);
    bool reset_max = asLogical(CADR(args));
    bool full = asLogical(CADDR(args));
    GCManager::gc(0, full);
    post_gc_checks();
#ifndef IMMEDIATE_FINALIZERS
    R_RunPendingFinalizers();
#endif

    GCManager::setReporting(report_os);
    /*- now return the [used , gc trigger size] for cells and heap */
    GCRoot<> value(allocVector(REALSXP, 14));
    REAL(value)[0] = GCNode::numNodes();
    REAL(value)[1] = MemoryBank::bytesAllocated();
    REAL(value)[4] = GCManager::nodeTriggerLevel();
    REAL(value)[5] = GCManager::triggerLevel();
    /* next four are in 0.1MB, rounded up */
    REAL(value)[2] = NA_REAL;  // in CXXR, cells don't have a fixed size
    REAL(value)[3] = 0.1*ceil(10. * (MemoryBank::bytesAllocated())/Mega);
    REAL(value)[6] = NA_REAL; // in CXXR, cells don't have a fixed size
    REAL(value)[7] = 0.1*ceil(10. * GCManager::triggerLevel()/Mega);
    REAL(value)[8] = NA_REAL; // in CXXR, cells don't have a fixed size
    REAL(value)[9] = (GCManager::maxTriggerLevel() < R_SIZE_T_MAX) ? 0.1*ceil(10. * GCManager::maxTriggerLevel()/Mega) : NA_REAL;
    if (reset_max){
        GCManager::resetMaxTallies();
    }
    REAL(value)[10] = GCManager::maxNodes();
    REAL(value)[11] = GCManager::maxBytes();
    REAL(value)[12] = NA_REAL;  // in CXXR, cells don't have a fixed size
    REAL(value)[13] = 0.1*ceil(10. * GCManager::maxBytes()/Mega);

    return value;
}

static double gctimes[5], gcstarttimes[5];
static bool gctime_enabled = false;

/* this is primitive */
HIDDEN SEXP do_gctime(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans;

    if (args == R_NilValue)
	gctime_enabled = true;
    else {
	check1arg(args, call, "on");
	gctime_enabled = asLogical(CAR(args));
    }
    ans = allocVector(REALSXP, 5);
    REAL(ans)[0] = gctimes[0];
    REAL(ans)[1] = gctimes[1];
    REAL(ans)[2] = gctimes[2];
    REAL(ans)[3] = gctimes[3];
    REAL(ans)[4] = gctimes[4];
    return ans;
}

static void gc_start_timing(void)
{
    if (gctime_enabled)
        R_getProcTime(gcstarttimes);
}

static void gc_end_timing(void)
{
    if (gctime_enabled) {
	double times[5], delta;
	R_getProcTime(times);

	/* add delta to compensate for timer resolution */
#if 0
	/* this seems to over-compensate too */
	delta = R_getClockIncrement();
#else
	delta = 0;
#endif

	gctimes[0] += times[0] - gcstarttimes[0] + delta;
	gctimes[1] += times[1] - gcstarttimes[1] + delta;
	gctimes[2] += times[2] - gcstarttimes[2];
	gctimes[3] += times[3] - gcstarttimes[3];
	gctimes[4] += times[4] - gcstarttimes[4];
    }
}

/* InitMemory : Initialise the memory to be used in R. */
/* This includes: stack space, node space and vector space */

HIDDEN void R::InitMemory()
{
    char *arg;

    GCManager::setMonitors(gc_start_timing, gc_end_timing);
    GCManager::setReporting(R_Verbose ? &std::cerr : nullptr);
    GCManager::enableGC(R_VSize, R_NSize);
    CXXR::initializeMemorySubsystem();

    init_gctorture();
    init_gc_grow_settings();

    arg = getenv("_R_GC_FAIL_ON_ERROR_");
    if (arg && StringTrue(arg))
        GCManager::set_gc_fail_on_error(true);
    else if (arg && StringFalse(arg))
        GCManager::set_gc_fail_on_error(false);

    R_BCNodeStackBase = (R_bcstack_t *) malloc(R_BCNODESTACKSIZE * sizeof(R_bcstack_t));
    if (R_BCNodeStackBase == nullptr)
        R_Suicide(_("couldn't allocate node stack"));
    R_BCNodeStackTop = R_BCNodeStackBase;
    R_BCNodeStackEnd = R_BCNodeStackBase + R_BCNODESTACKSIZE;
    R_BCProtTop = R_BCNodeStackTop;

    R_HandlerStack = R_RestartStack = R_NilValue;

    /*  Unbound values which are to be preserved through GCs */
    R_PreciousList = R_NilValue;

    /*  The current source line */
    R_Srcref = R_NilValue;

    /* R_TrueValue and R_FalseValue */
    R_TrueValue = mkTrue();
    MARK_NOT_MUTABLE(R_TrueValue);
    R_FalseValue = mkFalse();
    MARK_NOT_MUTABLE(R_FalseValue);
    R_LogicalNAValue = allocVector(LGLSXP, 1);
    LOGICAL(R_LogicalNAValue)[0] = NA_LOGICAL;
    MARK_NOT_MUTABLE(R_LogicalNAValue);
}



/* Allocation functions that GC on initial failure */

void *R_malloc_gc(size_t n)
{
    void *np = malloc(n);
    if (np == nullptr)
    {
        R_gc();
        np = malloc(n);
    }
    return np;
}

void *R_calloc_gc(size_t n, size_t s)
{
    void *np = calloc(n, s);
    if (np == nullptr)
    {
        R_gc();
        np = calloc(n, s);
    }
    return np;
}

void *R_realloc_gc(void *p, size_t n)
{
    void *np = realloc(p, n);
    if (np == nullptr)
    {
        R_gc();
        np = realloc(p, n);
    }
    return np;
}


/* "allocSExp" allocate a RObject */

SEXP Rf_allocSExp(SEXPTYPE t)
{
    SEXP ans;
    switch (t)
    {
    case LISTSXP:
        ans = new PairList();
        break;
    case LANGSXP:
        ans = new Expression();
        break;
    case DOTSXP:
        ans = new DottedArgs();
        break;
    case BCODESXP:
        ans = new ByteCode();
        break;
    case CLOSXP:
        ans = new Closure();
        break;
    case ENVSXP:
        ans = new Environment();
        break;
    // case PROMSXP:
    //     ans = new Promise();
    //     break;
    default:
        std::cerr << "Inappropriate SEXPTYPE (" << sexptype2char(t) << ") for ConsCell." << std::endl;
        abort();
    }
    ans->expose();
    return ans;
}

/* cons is defined directly to avoid the need to protect its arguments
   unless a GC will actually occur. */
SEXP Rf_cons(SEXP car, SEXP cdr)
{
    return PairList::construct<PairList>(CHK(car), SEXP_downcast<PairList *>(CHK(cdr)));
}

HIDDEN SEXP CONS_NR(SEXP car, SEXP cdr)
{
    GCRoot<> crr(CHK(car));
    GCRoot<PairList> tlr(SEXP_downcast<CXXR::PairList *>(CHK(cdr)));
    SEXP s = new PairList(crr, tlr);
    s->expose();

    DISABLE_REFCNT(s);
    return s;
}

SEXP Rf_lcons(SEXP cr, SEXP tl)
{
    GCRoot<> crr(cr);
    GCRoot<PairList> tlr(SEXP_downcast<PairList *>(tl));
    Expression *ans = new Expression(crr, tlr);
    ans->expose();
    return ans;
}

/*----------------------------------------------------------------------

  NewEnvironment

  Create an environment by extending "rho" with a frame obtained by
  pairing the variable names given by the tags on "namelist" with
  the values given by the elements of "valuelist".

  NewEnvironment is defined directly to avoid the need to protect its
  arguments unless a GC will actually occur.  This definition allows
  the namelist argument to be shorter than the valuelist; in this
  case the remaining values must be named already.  (This is useful
  in cases where the entire valuelist is already named--namelist can
  then be R_NilValue.)

  The valuelist is destructively modified and used as the
  environment's frame.
*/
SEXP R::NewEnvironment(SEXP namelist, SEXP valuelist, SEXP rho)
{
    SEXP v = CHK(valuelist);
    SEXP n = CHK(namelist);

    while (v != R_NilValue && n != R_NilValue)
    {
        SET_TAG(v, TAG(n));
        v = CDR(v);
        n = CDR(n);
    }

    GCRoot<> namelistr(namelist);
    GCRoot<PairList> namevalr(SEXP_downcast<PairList *>(valuelist));
    GCRoot<Environment> rhor(SEXP_downcast<Environment *>(rho));
    Environment *ans = new Environment(rhor, namevalr);
    ans->expose();
    return ans;
}

/* mkPROMISE is defined directly do avoid the need to protect its arguments
   unless a GC will actually occur. */
HIDDEN SEXP R::mkPROMISE(SEXP expr, SEXP rho)
{
    GCRoot<> exprt(expr);
    GCRoot<Environment> rhort(SEXP_downcast<Environment *>(rho));

    /* precaution to ensure code does not get modified via
       substitute() and the like */
    ENSURE_NAMEDMAX(expr);

    Promise *s = new Promise(exprt, rhort);
    s->expose();

    INCREMENT_REFCNT(expr);
    INCREMENT_REFCNT(rho);
    SET_PRSEEN(s, 0);
    return s;
}

SEXP R::R_mkEVPROMISE(SEXP expr, SEXP val)
{
    SEXP prom = mkPROMISE(expr, R_NilValue);
    SET_PRVALUE(prom, val);
    return prom;
}

HIDDEN SEXP R::R_mkEVPROMISE_NR(SEXP expr, SEXP val)
{
    SEXP prom = mkPROMISE(expr, R_NilValue);
    DISABLE_REFCNT(prom);
    SET_PRVALUE(prom, val);
    return prom;
}

/* All vector objects must be a multiple of sizeof(RObject)
   bytes so that alignment is preserved for all objects */

/* Allocate a vector object (and also list-like objects).
   This ensures only validity of list-like (LISTSXP, VECSXP, EXPRSXP),
   STRSXP and CHARSXP types;  e.g., atomic types remain un-initialized
   and must be initialized upstream, e.g., in do_makevector().
*/

SEXP Rf_allocVector3(SEXPTYPE type, R_xlen_t length = 1, R_allocator_t *allocator = nullptr)
{
    RObject *s = nullptr;

    if (length > R_XLEN_T_MAX)
        error(_("vector is too large")); /**** put length into message */
    else if (length < 0)
        error(_("negative length vectors are not allowed"));
    /* number of vector cells to allocate */
    switch (type)
    {
    case NILSXP:
        return nullptr;
    case RAWSXP:
    {
#ifdef R_MEMORY_PROFILING
        R_ReportAllocation(convert2VEC<bool>(length) * sizeof(VECREC));
#endif
        s = new RawVector(length, allocator);
        break;
    }
    case CHARSXP:
        Rf_error(_("use of allocVector(CHARSXP ...) is defunct\n"));
    case LGLSXP:
    {
#ifdef R_MEMORY_PROFILING
        R_ReportAllocation(convert2VEC<int>(length) * sizeof(VECREC));
#endif
        s = new LogicalVector(length, allocator);
        break;
    }
    case INTSXP:
    {
#ifdef R_MEMORY_PROFILING
        R_ReportAllocation(convert2VEC<int>(length) * sizeof(VECREC));
#endif
        s = new IntVector(length, allocator);
        break;
    }
    case REALSXP:
    {
#ifdef R_MEMORY_PROFILING
        R_ReportAllocation(convert2VEC<double>(length) * sizeof(VECREC));
#endif
        s = new RealVector(length, allocator);
        break;
    }
    case CPLXSXP:
    {
#ifdef R_MEMORY_PROFILING
        R_ReportAllocation(convert2VEC<Rcomplex>(length) * sizeof(VECREC));
#endif
        s = new ComplexVector(length, allocator);
        break;
    }
    case STRSXP:
    {
#ifdef R_MEMORY_PROFILING
        R_ReportAllocation(convert2VEC<RObject>(length) * sizeof(VECREC));
#endif
        s = new StringVector(length);
        break;
    }
    case EXPRSXP:
    {
#ifdef R_MEMORY_PROFILING
        R_ReportAllocation(convert2VEC<RObject>(length) * sizeof(VECREC));
#endif
        s = new ExpressionVector(length);
        break;
    }
    case VECSXP:
    {
#ifdef R_MEMORY_PROFILING
        R_ReportAllocation(convert2VEC<RObject>(length) * sizeof(VECREC));
#endif
        s = new ListVector(length);
        break;
    }
    case LANGSXP:
    {
#ifdef LONG_VECTOR_SUPPORT
        if (length > R_SHORT_LEN_MAX)
            error(_("invalid length for pairlist"));
#endif
        if (length == 0)
            return nullptr;
        GCRoot<PairList> tl(PairList::makeList(length - 1));
        s = new Expression(nullptr, tl);
        break;
    }
    case LISTSXP:
    {
#ifdef LONG_VECTOR_SUPPORT
        if (length > R_SHORT_LEN_MAX)
            error(_("invalid length for pairlist"));
#endif
        return Rf_allocList((int)length);
    }
    default:
        Rf_error(_("invalid type/length (%s/%d) in vector allocation"), type2char(type), length);
    }
    s->expose();
    return s;
}

/* For future hiding of allocVector(CHARSXP) */
HIDDEN SEXP R::allocCharsxp(R_len_t length)
{
#ifdef R_MEMORY_PROFILING
        R_ReportAllocation(convert2VEC<char>(length + 1) * sizeof(VECREC));
#endif
        UncachedString *ans = new UncachedString(length);
        ans->expose();
        return ans;
}

SEXP Rf_allocList(const int n)
{
    if (n > 0)
        return PairList::makeList(n);
    return nullptr;
}

static SEXP allocFormalsList(const int nargs, ...)
{
    PairList *res = nullptr;
    PairList *n;
    va_list syms;
    va_start(syms, nargs);

    for (int i = 0; i < nargs; i++)
    {
        res = CXXR_cons(nullptr, res);
    }
    R_PreserveObject(res);

    n = res;
    for (int i = 0; i < nargs; i++)
    {
        SET_TAG(n, (SEXP)va_arg(syms, SEXP));
        MARK_NOT_MUTABLE(n);
        n = n->tail();
    }
    va_end(syms);

    return res;
}

SEXP Rf_allocFormalsList2(SEXP sym1, SEXP sym2)
{
    return allocFormalsList(2, sym1, sym2);
}

SEXP Rf_allocFormalsList3(SEXP sym1, SEXP sym2, SEXP sym3)
{
    return allocFormalsList(3, sym1, sym2, sym3);
}

SEXP Rf_allocFormalsList4(SEXP sym1, SEXP sym2, SEXP sym3, SEXP sym4)
{
    return allocFormalsList(4, sym1, sym2, sym3, sym4);
}

SEXP Rf_allocFormalsList5(SEXP sym1, SEXP sym2, SEXP sym3, SEXP sym4, SEXP sym5)
{
    return allocFormalsList(5, sym1, sym2, sym3, sym4, sym5);
}

SEXP Rf_allocFormalsList6(SEXP sym1, SEXP sym2, SEXP sym3, SEXP sym4, SEXP sym5, SEXP sym6)
{
    return allocFormalsList(6, sym1, sym2, sym3, sym4, sym5, sym6);
}

/* "gc" a mark-sweep or in-place generational garbage collector */

void R_gc(void)
{
    GCManager::gc(0, true);
    post_gc_checks();
#ifndef IMMEDIATE_FINALIZERS
    R_RunPendingFinalizers();
#endif
}

#ifdef THREADCHECK
# if !defined(Win32) && defined(HAVE_PTHREAD)
#   include <pthread.h>
HIDDEN void R_check_thread(const char *s)
{
    static Rboolean main_thread_inited = FALSE;
    static pthread_t main_thread;
    if (! main_thread_inited) {
        main_thread = pthread_self();
        main_thread_inited = TRUE;
    }
    if (! pthread_equal(main_thread, pthread_self())) {
        char buf[1024];
	size_t bsize = sizeof buf;
	memset(buf, 0, bsize);
        snprintf(buf, bsize - 1, "Wrong thread calling '%s'", s);
        R_Suicide(buf);
    }
}
# else
/* This could be implemented for Windows using their threading API */ 
HIDDEN void R_check_thread(const char *s) {}
# endif
#endif

HIDDEN SEXP do_memoryprofile(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int tmp;
    constexpr int n = 24;
    checkArity(op, args);
    GCRoot<> ans(allocVector(INTSXP, n));
    GCRoot<> nms(allocVector(STRSXP, n));
    for (int i = 0; i < n; i++) {
	INTEGER(ans)[i] = 0;
    SET_STRING_ELT(nms, i, Rf_type2str(SEXPTYPE(i > LGLSXP ? i + 2 : i)));
    }
    setAttrib(ans, R_NamesSymbol, nms);

    BEGIN_SUSPEND_INTERRUPTS {

      for (unsigned int gen = 0; gen < GCNode::numGenerations(); ++gen) {
	  for (const GCNode *s = GCNode::s_generation[gen]; s; s = s->next()) {
               if (const RObject* ob = SEXP_downcast<const RObject*>(s, false)) {
	      tmp = ob->sexptype();
	      if(tmp > LGLSXP) tmp -= 2;
	      INTEGER(ans)[tmp]++;
               }
	  }
      }
    } END_SUSPEND_INTERRUPTS;
    return ans;
}

/* S-like wrappers for calloc, realloc and free that check for error
   conditions */

void *R_chk_calloc(size_t nelem, size_t elsize)
{
    void *p;
#ifndef HAVE_WORKING_CALLOC
    if(nelem == 0)
	return(nullptr);
#endif
    p = calloc(nelem, elsize);
    if(!p) /* problem here is that we don't have a format for size_t. */
	error(_("'Calloc()' function could not allocate memory (%.0f of %u bytes)"), (double) nelem, elsize);
    return(p);
}

void *R_chk_realloc(void *ptr, size_t size)
{
    void *p;
    /* Protect against broken realloc */
    if (ptr)
        p = realloc(ptr, size);
    else
        p = malloc(size);
    if (!p)
        error(_("'Realloc()' function could not re-allocate memory (%.0f bytes)"),
              (double)size);
    return (p);
}

void R_chk_free(void *ptr)
{
    /* S-PLUS warns here, but there seems no reason to do so */
    /* if(!ptr) warning(_("attempt to free NULL pointer by Free")); */
    if(ptr) free(ptr); /* ANSI C says free has no effect on nullptr, but
			  better to be safe here */
}

/* This code keeps a list of objects which are not assigned to variables
   but which are required to persist across garbage collections.  The
   objects are registered with R_PreserveObject and deregistered with
   R_ReleaseObject. */

static SEXP DeleteFromList(SEXP object, SEXP list)
{
    if (CAR(list) == object)
        return CDR(list);
    else
    {
        SEXP last = list;
        for (SEXP head = CDR(list); head != R_NilValue; head = CDR(head))
        {
            if (CAR(head) == object)
            {
                SETCDR(last, CDR(head));
                return list;
            }
            else
                last = head;
        }
        return list;
    }
}

#define ALLOW_PRECIOUS_HASH
#ifdef ALLOW_PRECIOUS_HASH
/* This allows using a fixed size hash table. This makes deleting mush
   more efficient for applications that don't follow the "sparing use"
   advice in R-exts.texi. Using the hash table is enabled by starting
   R with the environment variable R_HASH_PRECIOUS set.

   Pointer hashing as used here isn't entirely portable (we do it in
   at least one othe rplace, in serialize.cpp) but it could be made so
   by computing a unique value based on the allocation page and
   position in the page. */

#define PHASH_SIZE 1069
#define PTRHASH(obj) (((R_size_t) (obj)) >> 3)

static bool use_precious_hash = false;
static bool precious_inited = false;

void R_PreserveObject(SEXP object)
{
    R_CHECK_THREAD;
    if (!precious_inited)
    {
        precious_inited = true;
        if (getenv("R_HASH_PRECIOUS"))
            use_precious_hash = true;
    }
    if (use_precious_hash)
    {
        if (!R_PreciousList)
            R_PreciousList = allocVector(VECSXP, PHASH_SIZE);
        int bin = PTRHASH(object) % PHASH_SIZE;
        SET_VECTOR_ELT(R_PreciousList, bin, CONS(object, VECTOR_ELT(R_PreciousList, bin)));
    }
    else
        R_PreciousList = CONS(object, R_PreciousList);
}

void R_ReleaseObject(SEXP object)
{
    R_CHECK_THREAD;
    if (!precious_inited)
        return; /* can't be anything to delete yet */
    if (use_precious_hash)
    {
        int bin = PTRHASH(object) % PHASH_SIZE;
        SET_VECTOR_ELT(R_PreciousList, bin,
                       DeleteFromList(object,
                                      VECTOR_ELT(R_PreciousList, bin)));
    }
    else
        R_PreciousList = DeleteFromList(object, R_PreciousList);
}
#else
void R_PreserveObject(SEXP object)
{
    R_CHECK_THREAD;
    R_PreciousList = CONS(object, R_PreciousList);
}

void R_ReleaseObject(SEXP object)
{
    R_CHECK_THREAD;
    R_PreciousList = DeleteFromList(object, R_PreciousList);
}
#endif


/* This code is similar to R_PreserveObject/R_ReleasObject, but objects are
   kept in a provided multi-set (which needs to be itself protected).
   When protected via PROTECT, the multi-set is automatically unprotected
   during long jump, and thus all its members are eventually reclaimed.
   These functions were introduced for parsers generated by bison, because
   one cannot instruct bison to use PROTECT/UNPROTECT when working with
   the stack of semantic values. */

/* Multi-set is defined by a triple (store, npreserved, initialSize)
     npreserved is the number of elements in the store (counting each instance
       of the same value)
     store is a VECSXP or R_NilValue
       when VECSXP, preserved values are stored at the beginning, filled up by
       R_NilValue
     initialSize is the size for the VECSXP to be allocated if preserving values
       while store is R_NilValue

    The representation is CONS(store, npreserved) with TAG()==initialSize
*/

/* Create new multi-set for protecting objects. initialSize may be zero
   (a hardcoded default is then used). */
SEXP R_NewPreciousMSet(int initialSize)
{
    SEXP npreserved, isize;

    /* npreserved is modified in place */
    npreserved = allocVector(INTSXP, 1);
    SET_INTEGER_ELT(npreserved, 0, 0);
    GCRoot<PairList> tail(CXXR_cons(npreserved, nullptr));
    GCRoot<PairList> mset(CXXR_cons(nullptr, tail));
    /* isize is not modified in place */
    if (initialSize < 0)
        error("'initialSize' must be non-negative");
    isize = ScalarInteger(initialSize);
    SET_TAG(mset, isize);
    return mset;
}

static void checkMSet(SEXP mset)
{
    SEXP store = CAR(mset);
    SEXP npreserved = CADR(mset);
    SEXP isize = TAG(mset);
    if (/*MAYBE_REFERENCED(mset) ||*/
        ((store != R_NilValue) &&
         (TYPEOF(store) != VECSXP /*|| MAYBE_REFERENCED(store)*/)) ||
        (TYPEOF(npreserved) != INTSXP || XLENGTH(npreserved) != 1 /*||
	 MAYBE_REFERENCED(npreserved)*/
         ) ||
        (TYPEOF(isize) != INTSXP || XLENGTH(isize) != 1))

        error("Invalid mset");
}

/* Add object to multi-set. The object will be protected as long as the
   multi-set is protected. */
void R_PreserveInMSet(SEXP x, SEXP mset)
{
    if (x == R_NilValue || isSymbol(x))
        return; /* no need to preserve */
    GCRoot<> xx(x);
    checkMSet(mset);
    SEXP store = CAR(mset);
    int *n = INTEGER(CADR(mset));
    if (store == R_NilValue)
    {
        R_xlen_t newsize = INTEGER_ELT(TAG(mset), 0);
        if (newsize == 0)
            newsize = 4; /* default minimum size */
        store = allocVector(VECSXP, newsize);
        SETCAR(mset, store);
    }
    R_xlen_t size = XLENGTH(store);
    if (*n == size)
    {
        R_xlen_t newsize = 2 * size;
        if (newsize >= R_INT_MAX || newsize < size)
            error("Multi-set overflow");
        GCRoot<> newstore(allocVector(VECSXP, newsize));
        for (R_xlen_t i = 0; i < size; i++)
            SET_VECTOR_ELT(newstore, i, VECTOR_ELT(store, i));
        SETCAR(mset, newstore);
        store = newstore;
    }
    SET_VECTOR_ELT(store, (*n)++, x);
}

/* Remove (one instance of) the object from the multi-set. If there is another
   instance of the object in the multi-set, it will still be protected. If there
   is no instance of the object, the function does nothing. */
void R_ReleaseFromMSet(SEXP x, SEXP mset)
{
    if (x == R_NilValue || isSymbol(x))
        return; /* not preserved */
    checkMSet(mset);
    SEXP store = CAR(mset);
    if (store == R_NilValue)
        return; /* not preserved */
    int *n = INTEGER(CADR(mset));
    for (R_xlen_t i = (*n) - 1; i >= 0; i--)
    {
        if (VECTOR_ELT(store, i) == x)
        {
            for (; i < (*n) - 1; i++)
                SET_VECTOR_ELT(store, i, VECTOR_ELT(store, i + 1));
            SET_VECTOR_ELT(store, i, R_NilValue);
            (*n)--;
            return;
        }
    }
    /* not preserved */
}

/* Release all objects from the multi-set, but the multi-set can be used for
   preserving more objects. */
void R_ReleaseMSet(SEXP mset, int keepSize)
{
    checkMSet(mset);
    SEXP store = CAR(mset);
    if (store == R_NilValue)
        return; /* already empty */
    int *n = INTEGER(CADR(mset));
    if (XLENGTH(store) <= keepSize)
    {
        /* just free the entries */
        for (R_xlen_t i = 0; i < *n; i++)
            SET_VECTOR_ELT(store, i, R_NilValue);
    }
    else
        SETCAR(mset, R_NilValue);
    *n = 0;
}

/*
   Added to API in R 3.4.0.
   Work around casting issues: works where it is needed.
 */
union fn_ptr
{
    void *p;
    DL_FUNC fn;
};

SEXP R_MakeExternalPtrFn(DL_FUNC p, SEXP tag, SEXP prot)
{
    fn_ptr tmp;
    tmp.fn = p;
    return R_MakeExternalPtr(tmp.p, tag, prot);
}

DL_FUNC R_ExternalPtrAddrFn(SEXP s)
{
    fn_ptr tmp;
    tmp.p =  EXTPTR_PTR(CHK(s));
    return tmp.fn;
}

/* The following functions are replacements for the accessor macros.
   They are used by code that does not have direct access to the
   internal representation of objects.  The replacement functions
   implement the write barrier. */

/* General Cons Cell Attributes */
SEXP ATTRIB(SEXP x) { return CHK(CXXR::RObject::attrib(CHK(x))); }
int OBJECT(SEXP x) { return CXXR::RObject::object(CHK(x)); }
int MARK(RObject *x) { return CXXR::GCNode::is_marked(static_cast<GCNode*>(CHK(x))); }
SEXPTYPE TYPEOF(SEXP x)
{
    if (!RObject::altrep(x))
        return RObject::typeof_(CHK(x));

    return AltRep::wrapper_type(SEXP_downcast<AltRep *>(CHK(x)));
}
int (NAMED)(SEXP x) { return CXXR::RObject::named(CHK(x)); }
int RTRACE(SEXP x) { return CXXR::FunctionBase::rtrace(CHK(x)); }
int LEVELS(SEXP x) { return CXXR::RObject::levels(CHK(x)); }
int (REFCNT)(SEXP x) { return CXXR::RObject::refcnt(CHK(x)); }
int (TRACKREFS)(SEXP x) { return CXXR::RObject::trackrefs(CHK(x)); }
int ALTREP(SEXP x) { return CXXR::RObject::altrep(CHK(x)); }
int IS_SCALAR(SEXP x, SEXPTYPE type) { return CXXR::RObject::is_scalar(CHK(x), type); }
int SIMPLE_SCALAR_TYPE(SEXP x) { return (RObject::scalar(CHK(x)) && CXXR::RObject::attrib(CHK(x)) == R_NilValue) ? CXXR::RObject::typeof_(CHK(x)) : 0; }
void (DECREMENT_REFCNT)(SEXP x) { DECREMENT_REFCNT(CHK(x)); }
void (INCREMENT_REFCNT)(SEXP x) { INCREMENT_REFCNT(CHK(x)); }
void (DISABLE_REFCNT)(SEXP x)  { DISABLE_REFCNT(CHK(x)); }
void (ENABLE_REFCNT)(SEXP x) { ENABLE_REFCNT(CHK(x)); }
void (MARK_NOT_MUTABLE)(SEXP x) { MARK_NOT_MUTABLE(CHK(x)); }
int ASSIGNMENT_PENDING(SEXP x) { return CXXR::RObject::assignment_pending(CHK(x)); }
void SET_ASSIGNMENT_PENDING(SEXP x, int v)
{
    CXXR::RObject::set_assignment_pending(CHK(x), v);
}
int (IS_ASSIGNMENT_CALL)(SEXP x) { return IS_ASSIGNMENT_CALL(CHK(x)); }
void (MARK_ASSIGNMENT_CALL)(SEXP x) { MARK_ASSIGNMENT_CALL(CHK(x)); }

void SET_ATTRIB(SEXP x, SEXP v) {
    if(CXXR::RObject::typeof_(v) != LISTSXP && CXXR::RObject::typeof_(v) != NILSXP)
	error(_("value of 'SET_ATTRIB' must be a pairlist or nullptr, not a '%s'"),
	      type2char(CXXR::RObject::typeof_(v)));
    RObject::fix_refcnt(x, CXXR::RObject::attrib(x), v);
    RObject::set_attrib(x, v);
}
void SET_OBJECT(SEXP x, int v) { CXXR::RObject::set_object(CHK(x), v); }
void SET_TYPEOF(SEXP x, SEXPTYPE v) { CXXR::RObject::set_typeof(CHK(x), v); }
void SET_NAMED(SEXP x, int v)
{
#ifndef SWITCH_TO_REFCNT
    CXXR::RObject::set_named(CHK(x), v);
#endif
}
void SET_RTRACE(SEXP x, int v) { CXXR::FunctionBase::set_rtrace(CHK(x), v); }
void SETLEVELS(SEXP x, int v) { CXXR::RObject::setlevels(CHK(x), v); }
void DUPLICATE_ATTRIB(SEXP to, SEXP from) {
    SET_ATTRIB(CHK(to), Rf_duplicate(CHK(CXXR::RObject::attrib(CHK(from)))));
    CXXR::RObject::set_object(CHK(to), CXXR::RObject::object(from));
    if(CXXR::RObject::is_s4_object(from)) { CXXR::RObject::set_s4_object(to);} else { CXXR::RObject::unset_s4_object(to);};
}
void SHALLOW_DUPLICATE_ATTRIB(SEXP to, SEXP from) {
    SET_ATTRIB(CHK(to), Rf_shallow_duplicate(CHK(CXXR::RObject::attrib(CHK(from)))));
    CXXR::RObject::set_object(CHK(to), CXXR::RObject::object(from));
    if(CXXR::RObject::is_s4_object(from)) { CXXR::RObject::set_s4_object(to);} else { CXXR::RObject::unset_s4_object(to);};
}

void (ENSURE_NAMEDMAX)(SEXP x) { ENSURE_NAMEDMAX(CHK(x)); }
void (ENSURE_NAMED)(SEXP x) { ENSURE_NAMED(CHK(x)); }
void (SETTER_CLEAR_NAMED)(SEXP x) { SETTER_CLEAR_NAMED(CHK(x)); }
void (RAISE_NAMED)(SEXP x, int n) { RAISE_NAMED(CHK(x), n); }

/* S4 object testing */
int IS_S4_OBJECT(SEXP x){ return CXXR::RObject::is_s4_object(CHK(x)); }
void SET_S4_OBJECT(SEXP x){ CXXR::RObject::set_s4_object(CHK(x)); }
void UNSET_S4_OBJECT(SEXP x){ CXXR::RObject::unset_s4_object(CHK(x)); }

/* JIT optimization support */
int NOJIT(SEXP x) { return CXXR::Closure::nojit(CHK(x)); }
int MAYBEJIT(SEXP x) { return CXXR::Closure::maybejit(CHK(x)); }
void SET_NOJIT(SEXP x) { CXXR::Closure::set_nojit(CHK(x)); }
void SET_MAYBEJIT(SEXP x) { CXXR::Closure::set_maybejit(CHK(x)); }
void UNSET_MAYBEJIT(SEXP x) { CXXR::Closure::unset_maybejit(CHK(x)); }

/* Growable vector support */
int IS_GROWABLE(SEXP x) { return CXXR::VectorBase::growable_bit_set(CHK(x)) && XLENGTH(CHK(x)) < XTRUELENGTH(CHK(x)); }
void SET_GROWABLE_BIT(SEXP x) { CXXR::VectorBase::set_growable_bit(CHK(x)); }

static constexpr int nvec[32] = {
    1, 1, 1, 1, 1, 1, 1, 1,
    1, 0, 0, 1, 1, 0, 0, 0,
    0, 1, 1, 0, 0, 1, 1, 0,
    0, 1, 1, 1, 1, 1, 1, 1};

inline static SEXP CHK2(SEXP x)
{
    x = CHK(x);
    if (nvec[TYPEOF(x)])
        error(_("LENGTH or similar applied to %s object"), type2char(TYPEOF(x)));
    return x;
}

/* Vector Accessors */
int (LENGTH)(SEXP x) { return x == R_NilValue ? 0 : LENGTH(CHK2(x)); }
R_xlen_t (XLENGTH)(SEXP x) { return XLENGTH(CHK2(x)); }
R_xlen_t (TRUELENGTH)(SEXP x) { return TRUELENGTH(CHK2(x)); }

void SETLENGTH(SEXP x, R_xlen_t v)
{
    if (ALTREP(x))
        error(_("SETLENGTH() cannot be applied to an ALTVEC object."));
    if (!isVector(x))
        error(_("SETLENGTH() can only be applied to a standard vector, not a '%s'"), type2char(TYPEOF(x)));
    CXXR::VectorBase::set_stdvec_length(CHK2(x), v);
}

void SET_TRUELENGTH(SEXP x, R_xlen_t v) { CXXR::VectorBase::set_truelength(CHK2(x), v); }
int  (IS_LONG_VEC)(SEXP x) { return IS_LONG_VEC(CHK2(x)); }
#if defined(TESTING_WRITE_BARRIER) || defined(COMPILING_IVORY)
R_xlen_t STDVEC_LENGTH(SEXP x) { return CXXR::VectorBase::stdvec_length(CHK2(x)); }
R_xlen_t STDVEC_TRUELENGTH(SEXP x) { return CXXR::VectorBase::stdvec_truelength(CHK2(x)); }
void SETALTREP(SEXP x, int v) { CXXR::RObject::set_altrep(x, v); }
#endif

/* temporary, to ease transition away from remapping */
R_xlen_t Rf_XLENGTH(SEXP x) { return XLENGTH(x); }

extern "C"
const char *R_CHAR(SEXP x) {
    if(TYPEOF(x) != CHARSXP) // Han-Tak proposes to prepend  'x && '
	error(_("'%s' function can only be applied to a charecter, not a '%s'"), "CHAR()",
	      type2char(TYPEOF(x)));
    return CXXR::r_char(CHK(x));
}

SEXP STRING_ELT(SEXP x, R_xlen_t i) {
    if(TYPEOF(x) != STRSXP)
	error(_("'%s' function can only be applied to a character vector, not a '%s'"), "STRING_ELT()",
	      type2char(TYPEOF(x)));
    if (ALTREP(x))
	return CHK(ALTSTRING_ELT(CHK(x), i));
    else {
	SEXP *ps = CXXR::stdvec_dataptr<SEXP>(CHK(x));
	return CHK(ps[i]);
    }
}

SEXP VECTOR_ELT(SEXP x, R_xlen_t i) {
    /* We need to allow vector-like types here */
    if(TYPEOF(x) != VECSXP &&
       TYPEOF(x) != EXPRSXP &&
       TYPEOF(x) != WEAKREFSXP)
	error(_("'%s' function can only be applied to a list, not a '%s'"), "VECTOR_ELT()",
	      type2char(TYPEOF(x)));
    if (TYPEOF(x) == EXPRSXP)
    {
        return XVECTOR_ELT(x, i);
    }
    // return CHK(LISTVECTOR_ELT(CHK(x), i));
    ListVector *lv = CXXR::SEXP_downcast<CXXR::ListVector *>(CHK(x), false);
    return CHK((*lv)[i]);
}

SEXP XVECTOR_ELT(SEXP x, R_xlen_t i) {
    /* We need to allow vector-like types here */
    if(TYPEOF(x) != EXPRSXP)
	error(_("'%s' function can only be applied to an expression, not a '%s'"), "XVECTOR_ELT()",
	      type2char(TYPEOF(x)));
    // return CHK(EXPRVECTOR_ELT(CHK(x), i));
    ExpressionVector *ev = CXXR::SEXP_downcast<CXXR::ExpressionVector *>(CHK(x), false);
    return CHK((*ev)[i]);
}

namespace
{
#ifdef CATCH_ZERO_LENGTH_ACCESS
/* Attempts to read or write elements of a zero length vector will
   result in a segfault, rather than read and write random memory.
   Returning NULL would be more natural, but Matrix seems to assume
   that even zero-length vectors have non-NULL data pointers, so
   return (void *) 1 instead. Zero-length CHARSXP objects still have a
   trailing zero byte so they are not handled. */
    int *CHKZLN(SEXP x)
    {
        CHK(x);
        if (STDVEC_LENGTH(x) == 0 && TYPEOF(x) != CHARSXP)
            return (int *)1;
        return nullptr;
    }
#else
    int *CHKZLN(SEXP x)
    {
        return (int *)nullptr;
    }
#endif
} // namespace

void *STDVEC_DATAPTR(SEXP x)
{
    if (ALTREP(x))
        Rf_error(_("cannot get STDVEC_DATAPTR from ALTREP object"));
    if (!Rf_isVector(x) && TYPEOF(x) != WEAKREFSXP)
        Rf_error(_("STDVEC_DATAPTR can only be applied to a vector, not a '%s'"),
                 Rf_type2char(TYPEOF(x)));
    CHKZLN(x);
    return CXXR::stdvec_dataptr<>(x);
}

int *LOGICAL(SEXP x)
{
    if (TYPEOF(x) != LGLSXP)
        Rf_error(_("'%s' function can only be applied to a logical, not a '%s'"), "LOGICAL()",
                 Rf_type2char(TYPEOF(x)));
    CHKZLN(x);
    return LOGICALVECTOR_LOGICAL(x);
}

const int *LOGICAL_RO(SEXP x)
{
    if (TYPEOF(x) != LGLSXP)
        Rf_error(_("'%s' function can only be applied to a logical, not a '%s'"),
                 "LOGICAL()", Rf_type2char(TYPEOF(x)));
    CHKZLN(x);
    return LOGICALVECTOR_LOGICAL_RO(x);
}

/* Maybe this should exclude logicals, but it is widely used */
int *INTEGER(SEXP x)
{
    if (TYPEOF(x) != INTSXP && TYPEOF(x) != LGLSXP)
        Rf_error(_("'%s' function can only be applied to an integer, not a '%s'"), "INTEGER()",
                 Rf_type2char(TYPEOF(x)));
    CHKZLN(x);
    return INTVECTOR_INTEGER(x);
}

const int *INTEGER_RO(SEXP x)
{
    if (TYPEOF(x) != INTSXP && TYPEOF(x) != LGLSXP)
        Rf_error(_("'%s' function can only be applied to an integer, not a '%s'"),
                 "INTEGER()", Rf_type2char(TYPEOF(x)));
    CHKZLN(x);
    return INTVECTOR_INTEGER_RO(x);
}

Rbyte *RAW(SEXP x)
{
    if (TYPEOF(x) != RAWSXP)
        Rf_error(_("'%s' function can only be applied to a raw, not a '%s'"), "RAW()",
                 Rf_type2char(TYPEOF(x)));
    CHKZLN(x);
    return RAWVECTOR_RAW(x);
}

const Rbyte *RAW_RO(SEXP x)
{
    if (TYPEOF(x) != RAWSXP)
        Rf_error(_("'%s' function can only be applied to a raw, not a '%s'"),
                 "RAW()", Rf_type2char(TYPEOF(x)));
    CHKZLN(x);
    return RAWVECTOR_RAW_RO(x);
}

double *REAL(SEXP x)
{
    if (TYPEOF(x) != REALSXP)
        Rf_error(_("'%s' function can only be applied to a numeric, not a '%s'"), "REAL()",
                 Rf_type2char(TYPEOF(x)));
    CHKZLN(x);
    return REALVECTOR_REAL(x);
}

const double *REAL_RO(SEXP x)
{
    if (TYPEOF(x) != REALSXP)
        Rf_error(_("'%s' function can only be applied to a numeric, not a '%s'"),
                 "REAL()", Rf_type2char(TYPEOF(x)));
    CHKZLN(x);
    return REALVECTOR_REAL_RO(x);
}

Rcomplex *COMPLEX(SEXP x)
{
    if (TYPEOF(x) != CPLXSXP)
        Rf_error(_("'%s' function can only be applied to a complex, not a '%s'"), "COMPLEX()",
                 Rf_type2char(TYPEOF(x)));
    CHKZLN(x);
    return COMPLEXVECTOR_COMPLEX(x);
}

const Rcomplex *COMPLEX_RO(SEXP x)
{
    if (TYPEOF(x) != CPLXSXP)
        Rf_error(_("'%s' function can only be applied to a complex, not a '%s'"),
                 "COMPLEX()", "complex", Rf_type2char(TYPEOF(x)));
    CHKZLN(x);
    return COMPLEXVECTOR_COMPLEX_RO(x);
}

SEXP *STRING_PTR(SEXP x)
{
    if (TYPEOF(x) != STRSXP)
        Rf_error(_("'%s' function can only be applied to a character, not a '%s'"),
                 "STRING_PTR()", Rf_type2char(TYPEOF(x)));
    CHKZLN(x);
    return STRINGVECTOR_STRING_PTR(x);
}

const SEXP *STRING_PTR_RO(SEXP x)
{
    if (TYPEOF(x) != STRSXP)
        Rf_error(_("'%s' function can only be applied to a character, not a '%s'"),
                 "STRING_PTR_RO()", Rf_type2char(TYPEOF(x)));
    CHKZLN(x);
    return STRINGVECTOR_STRING_PTR_RO(x);
}

NORET SEXP *VECTOR_PTR(SEXP x)
{
    Rf_error(_("not safe to return vector pointer"));
}

void SET_STRING_ELT(SEXP x, R_xlen_t i, SEXP v)
{
    if (TYPEOF(CHK(x)) != STRSXP)
        Rf_error(_("'%s' function can only be applied to a character vector, not a '%s'"), "SET_STRING_ELT()",
                 Rf_type2char(TYPEOF(x)));
    if (TYPEOF(CHK(v)) != CHARSXP)
        Rf_error(_("value of 'SET_STRING_ELT()' function must be a 'CHARSXP' not a '%s'"),
                 Rf_type2char(TYPEOF(v)));
    if (i < 0 || i >= XLENGTH(x))
        Rf_error(_("attempt to set index %ld/%ld in 'SET_STRING_ELT()' function"), (long long)i, (long long)XLENGTH(x));

    x->propagateAge(v);
    if (ALTREP(x))
        ALTSTRING_SET_ELT(x, i, v);
    else
    {
        SEXP *ps = CXXR::stdvec_dataptr<SEXP>(x);
        RObject::fix_refcnt(x, ps[i], v);
        ps[i] = v;
    }
}

/* check for a CONS-like object */
#ifdef TESTING_WRITE_BARRIER
R_INLINE static SEXP CHKCONS(SEXP e)
{
    if (ALTREP(e))
        return CHK(e);
    switch (TYPEOF(e))
    {
    case LISTSXP:
    case LANGSXP:
    case NILSXP:
    case DOTSXP:
    case CLOSXP:    /**** use separate accessors? */
    case BCODESXP:  /**** use separate accessors? */
    case ENVSXP:    /**** use separate accessors? */
    case PROMSXP:   /**** use separate accessors? */
    case EXTPTRSXP: /**** use separate accessors? */
        return CHK(e);
    default:
        error(_("CAR/CDR/TAG or similar applied to %s object"), type2char(TYPEOF(e)));
    }
}
#else
#define CHKCONS(e) CHK(e)
#endif

HIDDEN
int BNDCELL_TAG(SEXP cell) { return CXXR::RObject::bndcell_tag(cell); }
HIDDEN
void SET_BNDCELL_TAG(SEXP cell, int val) { CXXR::RObject::set_bndcell_tag(cell, val); }
HIDDEN
double (BNDCELL_DVAL)(SEXP cell) { return BNDCELL_DVAL(cell); }
HIDDEN
int (BNDCELL_IVAL)(SEXP cell) { return BNDCELL_IVAL(cell); }
HIDDEN
int (BNDCELL_LVAL)(SEXP cell) { return BNDCELL_LVAL(cell); }
HIDDEN
void (SET_BNDCELL_DVAL)(SEXP cell, double v) { SET_BNDCELL_DVAL(cell, v); }
HIDDEN
void (SET_BNDCELL_IVAL)(SEXP cell, int v) { SET_BNDCELL_IVAL(cell, v); }
HIDDEN
void (SET_BNDCELL_LVAL)(SEXP cell, int v) { SET_BNDCELL_LVAL(cell, v); }
HIDDEN
void (INIT_BNDCELL)(SEXP cell, int type) { INIT_BNDCELL(cell, type); }

static void CLEAR_BNDCELL_TAG(SEXP cell)
{
    if (BNDCELL_TAG(cell))
    {
        ConsCell::set_car0(cell, nullptr);
        SET_BNDCELL_TAG(cell, 0);
    }
}

HIDDEN
void SET_BNDCELL(SEXP cell, SEXP val)
{
    CLEAR_BNDCELL_TAG(cell);
    SETCAR(cell, val);
}

HIDDEN void R::R_expand_binding_value(SEXP b)
{
#if BOXED_BINDING_CELLS
    SET_BNDCELL_TAG(b, 0);
#else
    int typetag = BNDCELL_TAG(b);
    if (typetag) {
	union {
	    SEXP sxpval;
	    double dval;
	    int ival;
	} vv;
	SEXP val;
	vv.sxpval = CAR0(b);
	switch (typetag) {
	case REALSXP:
	    val = ScalarReal(vv.dval);
	    SET_BNDCELL(b, val);
	    INCREMENT_NAMED(val);
	    break;
	case INTSXP:
	    val = ScalarInteger(vv.ival);
	    SET_BNDCELL(b, val);
	    INCREMENT_NAMED(val);
	    break;
	case LGLSXP:
	    val = ScalarLogical(vv.ival);
	    SET_BNDCELL(b, val);
	    INCREMENT_NAMED(val);
	    break;
	}
    }
#endif
}

HIDDEN void R::R_args_enable_refcnt(SEXP args)
{
#ifdef SWITCH_TO_REFCNT
    /* args is escaping into user C code and might get captured, so
       make sure it is reference counting. Should be able to get rid
       of this function if we reduce use of CONS_NR. */
    for (SEXP a = args; a != R_NilValue; a = CDR(a))
	if (a && !TRACKREFS(a)) {
	    ENABLE_REFCNT(a);
	    INCREMENT_REFCNT(CAR(a));
	    INCREMENT_REFCNT(CDR(a));
#ifdef TESTING_WRITE_BARRIER
	    /* this should not see non-tracking arguments */
	    if (CAR(a) && !TRACKREFS(CAR(a)))
		error(_("argument not tracking references"));
#endif
	}
#endif
}

/* S4Object Accessors */
SEXP S4TAG(SEXP e)
{
    return CHK(CXXR::S4Object::tag(CHKCONS(e)));
}

void SET_S4TAG(SEXP x, SEXP v)
{
    if (CHKCONS(x) == nullptr || x == R_NilValue)
        Rf_error(_("incorrect value"));
    RObject::fix_refcnt(x, S4TAG(x), v);
    S4Object::set_tag(x, v);
}

/* List Accessors */
SEXP TAG(SEXP e)
{
    if (TYPEOF(e) == S4SXP)
    {
        return S4TAG(e);
    }
    else
    {
        return CHK(CXXR::ConsCell::tag(CHKCONS(e)));
    }
}
SEXP CAR0(SEXP e) { return CHK(CXXR::ConsCell::car0(CHKCONS(e))); }
SEXP CDR(SEXP e) { return CHK(CXXR::ConsCell::cdr(CHKCONS(e))); }
SEXP CAAR(SEXP e) { return CHK(CAR(CAR(CHKCONS(e)))); }
SEXP CDAR(SEXP e) { return CHK(CDR(CAR(CHKCONS(e)))); }
SEXP CADR(SEXP e) { return CHK(CAR(CDR(CHKCONS(e)))); }
SEXP CDDR(SEXP e) { return CHK(CDR(CDR(CHKCONS(e)))); }
SEXP CDDDR(SEXP e) { return CHK(CDR(CDR(CDR(CHKCONS(e))))); }
SEXP CD4R(SEXP e) { return CHK(CDR(CDR(CDR(CDR(CHKCONS(e)))))); }
SEXP CADDR(SEXP e) { return CHK(CAR(CDR(CDR(CHKCONS(e))))); }
SEXP CADDDR(SEXP e) { return CHK(CAR(CDR(CDR(CDR(CHKCONS(e)))))); }
SEXP CAD3R(SEXP e) { return CHK(CAR(CDR(CDR(CDR(CHKCONS(e)))))); }
SEXP CAD4R(SEXP e) { return CHK(CAR(CDR(CDR(CDR(CDR(CHKCONS(e))))))); }
SEXP CAD5R(SEXP e) { return CHK(CAR(CDR(CDR(CDR(CDR(CDR(CHKCONS(e)))))))); }
int MISSING(SEXP x) { return CXXR::ConsCell::missing(CHKCONS(x)); }

void SET_TAG(SEXP x, SEXP v)
{
    if (TYPEOF(x) == S4SXP)
    {
        SET_S4TAG(x, v);
    }
    else
    {
        if (CHKCONS(x) == nullptr || x == R_NilValue)
            Rf_error(_("incorrect value"));
        RObject::fix_refcnt(x, TAG(x), v);
        ConsCell::set_tag(x, v);
    }
}

SEXP SETCAR(SEXP x, SEXP y)
{
    if (CHKCONS(x) == nullptr || x == R_NilValue)
        Rf_error(_("incorrect value"));
    CLEAR_BNDCELL_TAG(x);
    if (y == CAR(x))
        return y;
    RObject::fix_binding_refcnt(x, CAR(x), y);
    ConsCell::set_car0(x, y);
    return y;
}

SEXP SETCDR(SEXP x, SEXP y)
{
    if (CHKCONS(x) == nullptr || x == R_NilValue)
        Rf_error(_("incorrect value"));
    RObject::fix_refcnt(x, CDR(x), y);
#ifdef TESTING_WRITE_BARRIER
    /* this should not add a non-tracking CDR to a tracking cell */
    if (TRACKREFS(x) && y && ! TRACKREFS(y))
	error(_("inserting non-tracking CDR in tracking cell"));
#endif
    ConsCell::set_cdr(x, y);
    return y;
}

SEXP SETCADR(SEXP x, SEXP y)
{
    SEXP cell;
    if (CHKCONS(x) == nullptr || x == R_NilValue ||
        CHKCONS(CDR(x)) == nullptr || CDR(x) == R_NilValue)
        error(_("incorrect value"));
    cell = CDR(x);
    CLEAR_BNDCELL_TAG(cell);
    RObject::fix_refcnt(cell, CAR(cell), y);
    ConsCell::set_car0(cell, y);
    return y;
}

SEXP SETCADDR(SEXP x, SEXP y)
{
    SEXP cell;
    if (CHKCONS(x) == nullptr || x == R_NilValue ||
        CHKCONS(CDR(x)) == nullptr || CDR(x) == R_NilValue ||
        CHKCONS(CDDR(x)) == nullptr || CDDR(x) == R_NilValue)
        error(_("incorrect value"));
    cell = CDDR(x);
    CLEAR_BNDCELL_TAG(cell);
    RObject::fix_refcnt(cell, CAR(cell), y);
    ConsCell::set_car0(cell, y);
    return y;
}

SEXP SETCADDDR(SEXP x, SEXP y)
{
    SEXP cell;
    if (CHKCONS(x) == nullptr || x == R_NilValue ||
        CHKCONS(CDR(x)) == nullptr || CDR(x) == R_NilValue ||
        CHKCONS(CDDR(x)) == nullptr || CDDR(x) == R_NilValue ||
        CHKCONS(CDDDR(x)) == nullptr || CDDDR(x) == R_NilValue)
        error(_("incorrect value"));
    cell = CDDDR(x);
    CLEAR_BNDCELL_TAG(cell);
    RObject::fix_refcnt(cell, CAR(cell), y);
    ConsCell::set_car0(cell, y);
    return y;
}


SEXP SETCAD4R(SEXP x, SEXP y)
{
    SEXP cell;
    if (CHKCONS(x) == nullptr || x == R_NilValue ||
        CHKCONS(CDR(x)) == nullptr || CDR(x) == R_NilValue ||
        CHKCONS(CDDR(x)) == nullptr || CDDR(x) == R_NilValue ||
        CHKCONS(CDDDR(x)) == nullptr || CDDDR(x) == R_NilValue ||
        CHKCONS(CD4R(x)) == nullptr || CD4R(x) == R_NilValue)
        error(_("incorrect value"));
    cell = CD4R(x);
    CLEAR_BNDCELL_TAG(cell);
    RObject::fix_refcnt(cell, CAR(cell), y);
    ConsCell::set_car0(cell, y);
    return y;
}

void SET_MISSING(SEXP x, int v) { CXXR::ConsCell::set_missing(CHKCONS(x), v); }

/* Closure Accessors */
SEXP FORMALS(SEXP x) { return CHK(CXXR::Closure::formals(CHK(x))); }
SEXP BODY(SEXP x) { return CHK(CXXR::Closure::body(CHK(x))); }
SEXP CLOENV(SEXP x) { return CHK(CXXR::Closure::cloenv(CHK(x))); }
int RDEBUG(SEXP x) { return CXXR::FunctionBase::rdebug(CHK(x)); }
int RSTEP(SEXP x) { return CXXR::Closure::rstep(CHK(x)); }

void SET_FORMALS(SEXP x, SEXP v) { RObject::fix_refcnt(x, CXXR::Closure::formals(x), v); CXXR::Closure::set_formals(x, v); }
void SET_BODY(SEXP x, SEXP v) { RObject::fix_refcnt(x, CXXR::Closure::body(x), v); CXXR::Closure::set_body(x, v); }
void SET_CLOENV(SEXP x, SEXP v) { RObject::fix_refcnt(x, CXXR::Closure::cloenv(x), v); CXXR::Closure::set_cloenv(x, v); }
void SET_RDEBUG(SEXP x, int v) { CXXR::FunctionBase::set_rdebug(CHK(x), v); }
void SET_RSTEP(SEXP x, int v) { CXXR::Closure::set_rstep(CHK(x), v); }

/* These are only needed with the write barrier on */
#if defined(TESTING_WRITE_BARRIER) || defined(COMPILING_IVORY)
/* Primitive Accessors */
/* not hidden since needed in some base packages */
int CXXR::PRIMOFFSET(SEXP x) { return CXXR::BuiltInFunction::primoffset(CHK(x)); }
#endif

/* Symbol Accessors */
SEXP PRINTNAME(SEXP x) { return CHK(CXXR::Symbol::printname(CHK(x))); }
SEXP SYMVALUE(SEXP x) { return CHK(CXXR::Symbol::symvalue(CHK(x))); }
SEXP INTERNAL(SEXP x) { return CHK(CXXR::Symbol::internal(CHK(x))); }
int DDVAL(SEXP x) { return CXXR::Symbol::ddval(CHK(x)); }

void SET_PRINTNAME(SEXP x, SEXP v) { RObject::fix_refcnt(x, CXXR::Symbol::printname(x), v); CXXR::Symbol::set_printname(x, v); }

void SET_SYMVALUE(SEXP x, SEXP v)
{
    if (CXXR::Symbol::symvalue(x) == v)
        return;
    RObject::fix_binding_refcnt(x, CXXR::Symbol::symvalue(x), v);
    CXXR::Symbol::set_symvalue(x, v);
}

void SET_INTERNAL(SEXP x, SEXP v) { RObject::fix_refcnt(x, CXXR::Symbol::internal(x), v); CXXR::Symbol::set_internal(x, v); }
void SET_DDVAL(SEXP x, int v) { CXXR::Symbol::set_ddval(CHK(x), v); }

/* Environment Accessors */
SEXP FRAME(SEXP x) { return CHK(CXXR::Environment::frame(CHK(x))); }
SEXP ENCLOS(SEXP x) { return CHK(CXXR::Environment::enclos(CHK(x))); }
SEXP HASHTAB(SEXP x) { return CHK(CXXR::Environment::hashtab(CHK(x))); }
int ENVFLAGS(SEXP x) { return CXXR::Environment::envflags(CHK(x)); }
int ENV_RDEBUG(SEXP x) { return CXXR::Environment::env_rdebug(CHK(x)); }

void SET_FRAME(SEXP x, SEXP v) { RObject::fix_refcnt(x, CXXR::Environment::frame(x), v); CXXR::Environment::set_frame(x, v); }
void SET_ENCLOS(SEXP x, SEXP v) { RObject::fix_refcnt(x, CXXR::Environment::enclos(x), v); CXXR::Environment::set_enclos(x, v); }
void SET_HASHTAB(SEXP x, SEXP v) { RObject::fix_refcnt(x, CXXR::Environment::hashtab(x), v); CXXR::Environment::set_hashtab(x, v); }
void SET_ENVFLAGS(SEXP x, int v) { CXXR::Environment::set_envflags(x, v); }
void SET_ENV_RDEBUG(SEXP x, int v) { CXXR::Environment::set_env_rdebug(CHK(x), v); }


/* Promise Accessors */
SEXP PRCODE(SEXP x) { return CHK(CXXR::Promise::prcode(CHK(x))); }
SEXP PRENV(SEXP x) { return CHK(CXXR::Promise::prenv(CHK(x))); }
SEXP PRVALUE(SEXP x) { return CHK(CXXR::Promise::prvalue(CHK(x))); }
int PRSEEN(SEXP x) { return CXXR::Promise::prseen(CHK(x)); }

void SET_PRENV(SEXP x, SEXP v) { RObject::fix_refcnt(x, CXXR::Promise::prenv(x), v); CXXR::Promise::set_prenv(x, v); }
void SET_PRVALUE(SEXP x, SEXP v) { RObject::fix_refcnt(x, CXXR::Promise::prvalue(x), v); CXXR::Promise::set_prvalue(x, v); }
void SET_PRCODE(SEXP x, SEXP v) { RObject::fix_refcnt(x, CXXR::Promise::prcode(x), v); CXXR::Promise::set_prcode(x, v); }
void SET_PRSEEN(SEXP x, int v) { CXXR::Promise::set_prseen(CHK(x), v); }

/* Hashing Accessors */
#if defined(TESTING_WRITE_BARRIER) || defined(COMPILING_IVORY)
HIDDEN
int HASHASH(SEXP x) { return CXXR::String::hashash(CHK(x)); }
HIDDEN
int HASHVALUE(SEXP x)
{
    const CXXR::String *str = CXXR::SEXP_downcast<CXXR::String *>(x, false);
    return str->hash();
}

HIDDEN
void SET_HASHASH(SEXP x, int v) { /* does nothing in CXXR */ }
HIDDEN
void SET_HASHVALUE(SEXP x, int v)
{
    const CXXR::String *str = CXXR::SEXP_downcast<CXXR::String *>(x, false);
    str->hash();
}
#endif

/* Test functions */
extern "C"
{
    Rboolean Rf_isNull(SEXP s) { return (Rboolean) (TYPEOF(CHK(s)) == NILSXP); }
    Rboolean Rf_isSymbol(SEXP s) { return (Rboolean) (TYPEOF(CHK(s)) == SYMSXP); }
    Rboolean Rf_isLogical(SEXP s) { return (Rboolean) (TYPEOF(CHK(s)) == LGLSXP); }
    Rboolean Rf_isReal(SEXP s) { return (Rboolean) (TYPEOF(CHK(s)) == REALSXP); }
    Rboolean Rf_isComplex(SEXP s) { return (Rboolean) (TYPEOF(CHK(s)) == CPLXSXP); }
    Rboolean Rf_isExpression(SEXP s) { return (Rboolean) (TYPEOF(CHK(s)) == EXPRSXP); }
    Rboolean Rf_isEnvironment(SEXP s) { return (Rboolean) (TYPEOF(CHK(s)) == ENVSXP); }
    Rboolean Rf_isString(SEXP s) { return (Rboolean) (TYPEOF(CHK(s)) == STRSXP); }
    Rboolean Rf_isObject(SEXP s) { return (Rboolean) (OBJECT(CHK(s)) != 0); }
    Rboolean Rf_isRaw(SEXP s) { return (Rboolean) (TYPEOF(CHK(s)) == RAWSXP); }
}

/* Bindings accessors */
HIDDEN Rboolean IS_ACTIVE_BINDING(SEXP b) {return (Rboolean) CXXR::RObject::is_active_binding(CHK(b));}
HIDDEN Rboolean BINDING_IS_LOCKED(SEXP b) {return (Rboolean) CXXR::RObject::binding_is_locked(CHK(b));}
HIDDEN void SET_ACTIVE_BINDING_BIT(SEXP b) { CXXR::RObject::set_active_binding_bit(CHK(b)); }
HIDDEN void LOCK_BINDING(SEXP b) { CXXR::RObject::lock_binding(CHK(b)); }
HIDDEN void UNLOCK_BINDING(SEXP b) { CXXR::RObject::unlock_binding(CHK(b)); }

HIDDEN void SET_BASE_SYM_CACHED(SEXP b) { CXXR::Symbol::set_base_sym_cached(CHK(b)); }
HIDDEN void UNSET_BASE_SYM_CACHED(SEXP b) { CXXR::Symbol::unset_base_sym_cached(CHK(b)); }
HIDDEN Rboolean BASE_SYM_CACHED(SEXP b) { return (Rboolean) CXXR::Symbol::base_sym_cached(CHK(b)); }

HIDDEN void SET_SPECIAL_SYMBOL(SEXP b) { CXXR::Symbol::set_special_symbol(CHK(b)); }
HIDDEN void UNSET_SPECIAL_SYMBOL(SEXP b) { CXXR::Symbol::unset_special_symbol(CHK(b)); }
HIDDEN Rboolean IS_SPECIAL_SYMBOL(SEXP b) { return (Rboolean) CXXR::Symbol::is_special_symbol(CHK(b)); }
HIDDEN void SET_NO_SPECIAL_SYMBOLS(SEXP b) { CXXR::Symbol::set_no_special_symbols(CHK(b)); }
HIDDEN void UNSET_NO_SPECIAL_SYMBOLS(SEXP b) { CXXR::Symbol::unset_no_special_symbols(CHK(b)); }
HIDDEN Rboolean NO_SPECIAL_SYMBOLS(SEXP b) { return (Rboolean) CXXR::Symbol::no_special_symbols(CHK(b)); }

/* R_FunTab accessors, only needed when write barrier is on */
/* Not hidden to allow experimentaiton without rebuilding R - LT */
/* HIDDEN */
int PRIMVAL(SEXP x) { return  R_FunTab[PRIMOFFSET(CHK(x))].code(); }
/* HIDDEN */
CXXR::CCODE PRIMFUN(SEXP x) { return R_FunTab[PRIMOFFSET(CHK(x))].cfun(); }
/* HIDDEN */
// void (SET_PRIMFUN)(SEXP x, CXXR::CCODE f) { R_FunTab[PRIMOFFSET(CHK(x))].m_cfun = f; }

/* for use when testing the write barrier */
HIDDEN int IS_BYTES(SEXP x) { return CXXR::String::is_bytes(CHK(x)); }
HIDDEN int IS_LATIN1(SEXP x) { return CXXR::String::is_latin1(CHK(x)); }
HIDDEN int IS_ASCII(SEXP x) { return CXXR::String::is_ascii(CHK(x)); }
HIDDEN int IS_UTF8(SEXP x) { return CXXR::String::is_utf8(CHK(x)); }
HIDDEN void SET_BYTES(SEXP x) { CXXR::String::set_bytes(CHK(x)); }
HIDDEN void SET_LATIN1(SEXP x) { CXXR::String::set_latin1(CHK(x)); }
HIDDEN void SET_UTF8(SEXP x) { CXXR::String::set_utf8(CHK(x)); }
HIDDEN void SET_ASCII(SEXP x) { CXXR::String::set_ascii(CHK(x)); }
int  ENC_KNOWN(SEXP x) { return CXXR::String::enc_known(CHK(x)); }
HIDDEN void SET_CACHED(SEXP x) { CXXR::String::set_cached(CHK(x)); }
int  IS_CACHED(SEXP x) { return CXXR::String::is_cached(CHK(x)); }

/*******************************************/
/* Non-sampling memory use profiler
   reports all large vector heap
   allocations */
/*******************************************/

#ifndef R_MEMORY_PROFILING
NORET SEXP do_Rprofmem(SEXP args)
{
    error(_("memory profiling is not available on this system"));
}

#else
static int R_IsMemReporting;  /* Rboolean more appropriate? */
static FILE *R_MemReportingOutfile;
static R_size_t R_MemReportingThreshold;

static void R_OutputStackTrace(FILE *file)
{
    for (RCNTXT *cptr = R_GlobalContext; cptr; cptr = cptr->nextContext())
    {
        if ((cptr->getCallFlag() & (CTXT_FUNCTION | CTXT_BUILTIN)) && TYPEOF(cptr->getCall()) == LANGSXP)
        {
            SEXP fun = CAR(cptr->getCall());
            fprintf(file, "\"%s\" ",
                    TYPEOF(fun) == SYMSXP ? CXXR::r_char(PRINTNAME(fun)) : "<Anonymous>");
        }
    }
}

static void R_ReportAllocation(R_size_t size)
{
    if (R_IsMemReporting)
    {
        if (size > R_MemReportingThreshold)
        {
            fprintf(R_MemReportingOutfile, "%lu :", (unsigned long)size);
            R_OutputStackTrace(R_MemReportingOutfile);
            fprintf(R_MemReportingOutfile, "\n");
        }
    }
    return;
}

static void R_EndMemReporting()
{
    if (R_MemReportingOutfile)
    {
        /* does not fclose always flush? */
        fflush(R_MemReportingOutfile);
        fclose(R_MemReportingOutfile);
        R_MemReportingOutfile = nullptr;
    }
    R_IsMemReporting = 0;
    MemoryBank::setMonitor(0);
    return;
}

static void R_InitMemReporting(SEXP filename, int append, R_size_t threshold)
{
    if (R_MemReportingOutfile)
        R_EndMemReporting();
    R_MemReportingOutfile = RC_fopen(filename, append ? "a" : "w", TRUE);
    if (R_MemReportingOutfile == nullptr)
        error(_("'Rprofmem()': cannot open output file '%s'"), filename);
    R_MemReportingThreshold = threshold;
    R_IsMemReporting = 1;
    MemoryBank::setMonitor(R_ReportAllocation, threshold);
    return;
}

SEXP do_Rprofmem(SEXP args)
{
    SEXP filename;
    R_size_t threshold;
    int append_mode;

    if (!isString(CAR(args)) || (LENGTH(CAR(args))) != 1)
        error(_("invalid '%s' argument"), "filename");
    append_mode = asLogical(CADR(args));
    filename = STRING_ELT(CAR(args), 0);
    threshold = (R_size_t)REAL(CADDR(args))[0];
    if (strlen(CXXR::r_char(filename)))
        R_InitMemReporting(filename, append_mode, threshold);
    else
        R_EndMemReporting();
    return R_NilValue;
}

#endif /* R_MEMORY_PROFILING */

/* RBufferUtils, moved from deparse.cpp */

#include "RBufferUtils.h"

void *R_AllocStringBuffer(size_t blen, R_StringBuffer *buf)
{
    return R_AllocStringBuffer(blen, *buf);
}

void *R_AllocStringBuffer(size_t blen, R_StringBuffer &buf)
{
    size_t blen1, bsize = buf.defaultSize;

    /* for backwards compatibility, this used to free the buffer */
    if(blen == (size_t)-1)
	error(_("'R_AllocStringBuffer( (size_t)-1 )' function is no longer allowed"));

    if(blen * sizeof(char) < buf.bufsize) return buf.data;
    blen1 = blen = (blen + 1) * sizeof(char);
    blen = (blen / bsize) * bsize;
    if(blen < blen1) blen += bsize;

    if(buf.data == nullptr) {
	buf.data = (char *) malloc(blen);
	if(buf.data)
	    buf.data[0] = '\0';
    } else
	buf.data = (char *) realloc(buf.data, blen);
    buf.bufsize = blen;
    if(!buf.data) {
	buf.bufsize = 0;
	/* don't translate internal error message */
	error(_("could not allocate memory (%u Mb) in 'R_AllocStringBuffer()' function"),
	      (unsigned int) blen/1024/1024);
    }
    return buf.data;
}
void R_FreeStringBuffer(R_StringBuffer &buf) { buf.R_FreeStringBuffer(); }

void R_StringBuffer::R_FreeStringBuffer()
{
    if (this->data)
    {
        free(this->data);
        this->bufsize = 0;
        this->data = nullptr;
    }
}
HIDDEN void R_FreeStringBufferL(R_StringBuffer &buf) { buf.R_FreeStringBufferL(); }

void R_StringBuffer::R_FreeStringBufferL()
{
    if (this->bufsize > this->defaultSize)
    {
        free(this->data);
        this->bufsize = 0;
        this->data = nullptr;
    }
}
