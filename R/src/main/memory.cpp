/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998--2021  The R Core Team.
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
#include <CXXR/GCStackRoot.hpp>
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
#ifndef USE_VALGRIND_FOR_WINE
#define NVALGRIND 1
#endif
#endif

#ifndef VALGRIND_LEVEL
#define VALGRIND_LEVEL 0
#endif

#ifndef NVALGRIND
#ifdef HAVE_VALGRIND_MEMCHECK_H
#include "valgrind/memcheck.h"
#else
// internal version of headers
#include "vg/memcheck.h"
#endif
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
extern void Rm_free(void *p);
extern void *Rm_realloc(void *p, size_t n);
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

/* These are used in profiling to separate out time in GC */
int R_gc_running() { return GCManager::R_in_gc(); }

#ifdef TESTING_WRITE_BARRIER
#define PROTECTCHECK
#endif

/** @brief Translate SEXPTYPE enum to a character string
 * 
 * @param type SEXP object's type
 * 
 * @return name of the type
 * 
 * @note also called from typeName() in inspect.cpp
 */
HIDDEN
const char *R::sexptype2char(const SEXPTYPE type)
{
    switch (type)
    {
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

static void init_gc_grow_settings()
{
    char *arg;

    arg = getenv("R_GC_MEM_GROW");
    if (arg)
    {
        int which = (int)atof(arg);
        switch (which)
        {
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
    if (arg)
    {
        double frac = atof(arg);
        if (0.35 <= frac && frac <= 0.75)
        {
            GCManager::setGCGrowParameters(frac, frac);
        }
    }
    arg = getenv("R_GC_GROWINCRFRAC");
    if (arg)
    {
        double frac = atof(arg);
        if (0.05 <= frac && frac <= 0.80)
        {
            GCManager::setGCGrowIncrParameters(frac, frac);
        }
    }
    arg = getenv("R_GC_NGROWINCRFRAC");
    if (arg)
    {
        double frac = atof(arg);
        if (0.05 <= frac && frac <= 0.80)
            GCManager::setGCGrowIncrParameters(frac, 0.2);
    }
    arg = getenv("R_GC_VGROWINCRFRAC");
    if (arg)
    {
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
    if (GCManager::maxTriggerLevel() == R_SIZE_T_MAX)
        return R_SIZE_T_MAX;
    return GCManager::maxTriggerLevel() * sizeof(VECREC);
}

HIDDEN void R::R_SetMaxVSize(R_size_t size)
{
    if (size == R_SIZE_T_MAX)
        return;
    if (size / sizeof(VECREC) >= GCManager::triggerLevel())
        GCManager::setMaxTriggerLevel((size + 1) / sizeof(VECREC));
}

HIDDEN R_size_t R::R_GetMaxNSize(void)
{
    return GCManager::maxNodeTriggerLevel();
}

HIDDEN void R::R_SetMaxNSize(R_size_t size)
{
    if (size >= GCManager::nodeTriggerLevel())
        GCManager::setMaxNodeTriggerLevel(size);
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
    /** @brief List of Persistent Objects
     */
    GCRoot<> R_PreciousList(nullptr);
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
// encapsulating the pointers concerned in GCStackRoot<> objects declared
// at file/global/static scope.

void GCNode::gc(unsigned int num_old_gens_to_collect)
{
    // std::cerr << "GCNode::gc(" << num_old_gens_to_collect << ")\n";
    GCNode::check();
    // std::cerr << "Precheck completed OK\n";

    propagateAges();

    GCNode::Marker marker(num_old_gens_to_collect + 1);
    GCRootBase::visitRoots(&marker);
    GCStackRootBase::visitRoots(&marker);
    ProtectStack::visitRoots(&marker);
    MARK_THRU(&marker, R_BlankScalarString); /* Builtin constants */

    MARK_THRU(&marker, R_Warnings); /* Warnings, if any */

    MARK_THRU(&marker, R_HandlerStack); /* Condition handler stack */
    MARK_THRU(&marker, R_RestartStack); /* Available restarts stack */

    MARK_THRU(&marker, R_BCbody); /* Current byte code object */
    MARK_THRU(&marker, R_Srcref); /* Current source reference */

    MARK_THRU(&marker, R_print.na_string);
    MARK_THRU(&marker, R_print.na_string_noquote);

    Symbol::visitTable(&marker); /* Symbol table */

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

    for (RCNTXT *ctxt = R_GlobalContext; ctxt; ctxt = ctxt->nextContext())
    {
        MARK_THRU(&marker, ctxt->onExit());             /* on.exit expressions */
        MARK_THRU(&marker, ctxt->getPromiseArgs());     /* promises supplied to closure */
        MARK_THRU(&marker, ctxt->getCallFun());         /* the closure called */
        MARK_THRU(&marker, ctxt->getSysParent());       /* calling environment */
        MARK_THRU(&marker, ctxt->getCall());            /* the call */
        MARK_THRU(&marker, ctxt->workingEnvironment()); /* the closure environment */
        MARK_THRU(&marker, ctxt->getBCBody());          /* the current byte code object */
        MARK_THRU(&marker, ctxt->getHandlerStack());    /* the condition handler stack */
        MARK_THRU(&marker, ctxt->getRestartStack());    /* the available restarts stack */
        MARK_THRU(&marker, ctxt->getSrcRef());          /* the current source reference */
        MARK_THRU(&marker, ctxt->getReturnValue());     /* For on.exit calls */
    }

    for (R_bcstack_t *sp = R_BCNodeStackBase; sp < R_BCNodeStackTop; sp++)
    {
        if (sp->tag == RAWMEM_TAG)
            sp += sp->u.ival;
        else if (sp->tag == NILSXP || R_bcstack_t::IS_PARTIAL_SXP_TAG(sp->tag))
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
    if (arg)
    {
        int gap = atoi(arg);
        if (gap > 0)
        {
            GCManager::setTortureParameters(gap, gap, false);
            arg = getenv("R_GCTORTURE_WAIT");
            if (arg)
            {
                int wait = atoi(arg);
                if (wait > 0)
                    GCManager::setTortureParameters(gap, wait, false);
            }
#ifdef PROTECTCHECK
            arg = getenv("R_GCTORTURE_INHIBIT_RELEASE");
            if (arg)
            {
                int inhibit = atoi(arg);
                if (inhibit > 0)
                    GCManager::setInhibitor(true);
                else
                    GCManager::setInhibitor(false);
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
            GCManager::gc_error(_("internal TRUE value has been modified"));
        }
        if (R_FalseValue && LOGICAL(R_FalseValue)[0] != FALSE)
        {
            LOGICAL(R_FalseValue)[0] = FALSE;
            GCManager::gc_error(_("internal FALSE value has been modified"));
        }
        if (R_LogicalNAValue &&
            LOGICAL(R_LogicalNAValue)[0] != NA_LOGICAL)
        {
            LOGICAL(R_LogicalNAValue)[0] = NA_LOGICAL;
            GCManager::gc_error(_("internal logical NA value has been modified"));
        }
    }
} // namespace

HIDDEN SEXP do_gc(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    std::ostream *report_os = GCManager::setReporting(Rf_asLogical(CAR(args)) ? &std::cerr : nullptr);
    bool reset_max = asLogical(CADR(args));
    bool full = asLogical(CADDR(args));
    GCManager::gc(0, full);
    post_gc_checks();
#ifndef IMMEDIATE_FINALIZERS
    R_RunPendingFinalizers();
#endif

    GCManager::setReporting(report_os);
    /*- now return the [used , gc trigger size] for cells and heap */
    GCStackRoot<> value(allocVector(REALSXP, 14));
    REAL(value)[0] = GCNode::numNodes();
    REAL(value)[1] = MemoryBank::bytesAllocated();
    REAL(value)[4] = GCManager::nodeTriggerLevel();
    REAL(value)[5] = GCManager::triggerLevel();
    /* next four are in 0.1MB, rounded up */
    REAL(value)[2] = NA_REAL; // in CXXR, cells don't have a fixed size
    REAL(value)[3] = 0.1 * ceil(10. * (MemoryBank::bytesAllocated()) / Mega);
    REAL(value)[6] = NA_REAL; // in CXXR, cells don't have a fixed size
    REAL(value)[7] = 0.1 * ceil(10. * GCManager::triggerLevel() / Mega);
    REAL(value)[8] = NA_REAL; // in CXXR, cells don't have a fixed size
    REAL(value)[9] = (GCManager::maxTriggerLevel() < R_SIZE_T_MAX) ? 0.1 * ceil(10. * GCManager::maxTriggerLevel() / Mega) : NA_REAL;
    if (reset_max)
    {
        GCManager::resetMaxTallies();
    }
    REAL(value)[10] = GCManager::maxNodes();
    REAL(value)[11] = GCManager::maxBytes();
    REAL(value)[12] = NA_REAL; // in CXXR, cells don't have a fixed size
    REAL(value)[13] = 0.1 * ceil(10. * GCManager::maxBytes() / Mega);

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
    else
    {
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
    if (gctime_enabled)
    {
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

    R_BCNodeStackBase = (R_bcstack_t *)malloc(R_BCNODESTACKSIZE * sizeof(R_bcstack_t));
    if (!R_BCNodeStackBase)
        R_Suicide(_("couldn't allocate node stack"));
    R_BCNodeStackTop = R_BCNodeStackBase;
    R_BCNodeStackEnd = R_BCNodeStackBase + R_BCNODESTACKSIZE;
    R_BCProtTop = R_BCNodeStackTop;

    R_HandlerStack = R_RestartStack = nullptr;

    /*  Unbound values which are to be preserved through GCs */
    R_PreciousList = nullptr;

    /*  The current source line */
    R_Srcref = nullptr;
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
        ans = GCNode::expose(new PairList());
        break;
    case LANGSXP:
        ans = GCNode::expose(new Expression());
        break;
    case DOTSXP:
        ans = GCNode::expose(new DottedArgs());
        break;
    case BCODESXP:
        ans = GCNode::expose(new ByteCode());
        break;
    case CLOSXP:
        ans = GCNode::expose(new Closure());
        break;
    case ENVSXP:
        ans = GCNode::expose(new Environment());
        break;
    // case PROMSXP:
    //     ans = GCNode::expose(new Promise());
    //     break;
    default:
        std::cerr << "Inappropriate SEXPTYPE (" << sexptype2char(t) << ") for ConsCell." << std::endl;
        abort();
    }

    return ans;
}

/* cons is defined directly to avoid the need to protect its arguments
   unless a GC will actually occur. */
SEXP Rf_cons(SEXP car, SEXP cdr)
{
    return PairList::construct<PairList>(car, SEXP_downcast<PairList *>(cdr));
}

HIDDEN SEXP CONS_NR(SEXP car, SEXP cdr)
{
    GCStackRoot<> crr(car);
    GCStackRoot<PairList> tlr(SEXP_downcast<CXXR::PairList *>(cdr));
    SEXP s = GCNode::expose(new PairList());

    DISABLE_REFCNT(s);
    SETCAR(s, car);
    SETCDR(s, cdr);
    return s;
}

SEXP Rf_lcons(SEXP cr, SEXP tl)
{
    GCStackRoot<> crr(cr);
    GCStackRoot<PairList> tlr(SEXP_downcast<PairList *>(tl));
    return GCNode::expose(new Expression(crr, tlr));
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
    SEXP v = valuelist;
    SEXP n = namelist;

    while (v && n)
    {
        SET_TAG(v, TAG(n));
        v = CDR(v);
        n = CDR(n);
    }

    GCStackRoot<> namelistr(namelist);
    GCStackRoot<PairList> namevalr(SEXP_downcast<PairList *>(valuelist));
    GCStackRoot<Environment> rhor(SEXP_downcast<Environment *>(rho));
    return GCNode::expose(new Environment(rhor, namevalr));
}

/* mkPROMISE is defined directly do avoid the need to protect its arguments
   unless a GC will actually occur. */
HIDDEN SEXP R::mkPROMISE(SEXP expr, SEXP rho)
{
    GCStackRoot<> exprt(expr);
    GCStackRoot<Environment> rhort(SEXP_downcast<Environment *>(rho));

    /* precaution to ensure code does not get modified via
       substitute() and the like */
    ENSURE_NAMEDMAX(expr);

    return GCNode::expose(new Promise(exprt, rhort));
}

SEXP R::R_mkEVPROMISE(SEXP expr, SEXP val)
{
    SEXP prom = mkPROMISE(expr, nullptr);
    SET_PRVALUE(prom, val);
    return prom;
}

HIDDEN SEXP R::R_mkEVPROMISE_NR(SEXP expr, SEXP val)
{
    SEXP prom = mkPROMISE(expr, nullptr);
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
    if (length > R_XLEN_T_MAX)
        Rf_error(_("vector is too large")); /**** put length into message */
    else if (length < 0)
        Rf_error(_("negative length vectors are not allowed"));

    switch (type)
    {
    case NILSXP:
        return nullptr;
    case RAWSXP:
        return RawVector::create(length, allocator);
    case CHARSXP:
        Rf_error(_("use of allocVector(CHARSXP ...) is defunct\n"));
    case LGLSXP:
        return LogicalVector::create(length, allocator);
    case INTSXP:
        return IntVector::create(length, allocator);
    case REALSXP:
        return RealVector::create(length, allocator);
    case CPLXSXP:
        return ComplexVector::create(length, allocator);
    case STRSXP:
        return GCNode::expose(new StringVector(length));
    case EXPRSXP:
        return GCNode::expose(new ExpressionVector(length));
    case VECSXP:
        return GCNode::expose(new ListVector(length));
    case LANGSXP:
    {
#ifdef LONG_VECTOR_SUPPORT
        if (length > R_SHORT_LEN_MAX)
            Rf_error(_("invalid length for pairlist"));
#endif
        if (length == 0)
            return nullptr;
        GCStackRoot<PairList> tl(PairList::makeList(length - 1));
        return GCNode::expose(new Expression(nullptr, tl));
    }
    case LISTSXP:
    {
#ifdef LONG_VECTOR_SUPPORT
        if (length > R_SHORT_LEN_MAX)
            Rf_error(_("invalid length for pairlist"));
#endif
        return Rf_allocList(int(length));
    }
    default:
        Rf_error(_("invalid type/length (%s/%d) in vector allocation"), type2char(type), length);
    }

    return nullptr;
}

/* For future hiding of allocVector(CHARSXP) */
HIDDEN SEXP R::allocCharsxp(R_len_t length)
{
    return GCNode::expose(new UncachedString(length));
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
#if !defined(_WIN32) && defined(HAVE_PTHREAD)
#include <pthread.h>
HIDDEN void R_check_thread(const char *s)
{
    static Rboolean main_thread_inited = FALSE;
    static pthread_t main_thread;
    if (!main_thread_inited)
    {
        main_thread = pthread_self();
        main_thread_inited = TRUE;
    }
    if (!pthread_equal(main_thread, pthread_self()))
    {
        char buf[1024];
        size_t bsize = sizeof buf;
        memset(buf, 0, bsize);
        snprintf(buf, bsize - 1, "Wrong thread calling '%s'", s);
        R_Suicide(buf);
    }
}
#else
/* This could be implemented for Windows using their threading API */
HIDDEN void R_check_thread(const char *s) {}
#endif
#endif

namespace CXXR
{
    GCNode *GCNode::countObjects()
    {
        constexpr int n = 24;
        int tmp;
        GCStackRoot<> ans(IntVector::create(n));

        for (int i = 0; i < n; i++)
        {
            INTEGER(ans)[i] = 0;
        }

        BEGIN_SUSPEND_INTERRUPTS
        {
            for (unsigned int gen = 0; gen < GCNode::numGenerations(); ++gen)
            {
                for (const GCNode *s = GCNode::s_generation[gen]; s; s = s->next())
                {
                    if (const RObject *ob = SEXP_downcast<const RObject *>(s, false))
                    {
                        tmp = ob->sexptype();
                        if (tmp > LGLSXP)
                            tmp -= 2;
                        INTEGER(ans)[tmp]++;
                    }
                }
            }
        }
        END_SUSPEND_INTERRUPTS;
        return ans;
    }
} // namespace CXXR

HIDDEN SEXP do_memoryprofile(SEXP call, SEXP op, SEXP args, SEXP env)
{
    constexpr int n = 24;
    checkArity(op, args);

    GCStackRoot<> ans(SEXP_downcast<IntVector *>(GCNode::countObjects()));
    GCStackRoot<> nms(Rf_allocVector(STRSXP, n));

    for (int i = 0; i < n; i++)
    {
        SET_STRING_ELT(nms, i, Rf_type2str(SEXPTYPE(i > LGLSXP ? i + 2 : i)));
    }
    Rf_setAttrib(ans, R_NamesSymbol, nms);

    return ans;
}

/* S-like wrappers for calloc, realloc and free that check for error
   conditions */

void *R_chk_calloc(size_t nelem, size_t elsize)
{
    void *p;
#ifndef HAVE_WORKING_CALLOC
    if (nelem == 0)
        return (nullptr);
#endif
    p = calloc(nelem, elsize);
    if (!p) /* problem here is that we don't have a format for size_t. */
        error(_("'Calloc()' function could not allocate memory (%.0f of %u bytes)"), (double)nelem, elsize);
    return (p);
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
    if (ptr)
        free(ptr); /* ANSI C says free has no effect on nullptr, but
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
#define PTRHASH(obj) (((R_size_t)(obj)) >> 3)

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
    GCStackRoot<PairList> tail(CXXR_cons(npreserved, nullptr));
    GCStackRoot<PairList> mset(CXXR_cons(nullptr, tail));
    /* isize is not modified in place */
    if (initialSize < 0)
        error(_("'initialSize' must be non-negative"));
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

        error(_("Invalid mset"));
}

/* Add object to multi-set. The object will be protected as long as the
   multi-set is protected. */
void R_PreserveInMSet(SEXP x, SEXP mset)
{
    if (x == R_NilValue || isSymbol(x))
        return; /* no need to preserve */
    GCStackRoot<> xx(x);
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
            error(_("Multi-set overflow"));
        GCStackRoot<> newstore(allocVector(VECSXP, newsize));
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
    tmp.p = EXTPTR_PTR(s);
    return tmp.fn;
}

HIDDEN void R::R_expand_binding_value(SEXP b)
{
#if BOXED_BINDING_CELLS
    SET_BNDCELL_TAG(b, NILSXP);
#else
    int typetag = BNDCELL_TAG(b);
    if (typetag)
    {
        union
        {
            SEXP sxpval;
            double dval;
            int ival;
        } vv;
        SEXP val;
        vv.sxpval = CAR0(b);
        switch (typetag)
        {
        case REALSXP:
	    PROTECT(b);
            val = ScalarReal(vv.dval);
            SET_BNDCELL(b, val);
	    UNPROTECT(1);
            break;
        case INTSXP:
	    PROTECT(b);
            val = ScalarInteger(vv.ival);
            SET_BNDCELL(b, val);
	    UNPROTECT(1);
            break;
        case LGLSXP:
	    PROTECT(b);
            val = ScalarLogical(vv.ival);
            SET_BNDCELL(b, val);
	    UNPROTECT(1);
            break;
        }
    }
#endif
}

HIDDEN void CXXR::R_expand_binding_value(Frame::Binding *b)
{
#if BOXED_BINDING_CELLS
    if (b)
        b->setBndCellTag(NILSXP);
#else
    int typetag = b ? b->bndcellTag() : NILSXP;
    if (typetag)
    {
        union
        {
            RObject *sxpval;
            double dval;
            int ival;
        } vv;
        RObject *val;
        vv.sxpval = b ? b->value() : nullptr;
        switch (typetag)
        {
        case REALSXP:
            val = Rf_ScalarReal(vv.dval);
            if (b)
                b->setValue(val);
            break;
        case INTSXP:
            val = Rf_ScalarInteger(vv.ival);
            if (b)
                b->setValue(val);
            break;
        case LGLSXP:
            val = Rf_ScalarLogical(vv.ival);
            if (b)
                b->setValue(val);
            break;
        }
    }
#endif
}

HIDDEN void R::R_args_enable_refcnt(SEXP args)
{
    /* args is escaping into user C code and might get captured, so
       make sure it is reference counting. Should be able to get rid
       of this function if we reduce use of CONS_NR. */
    for (SEXP a = args; a != R_NilValue; a = CDR(a))
        if (a && !TRACKREFS(a))
        {
            ENABLE_REFCNT(a);
            INCREMENT_REFCNT(CAR(a));
            INCREMENT_REFCNT(CDR(a));
#ifdef TESTING_WRITE_BARRIER
            /* this should not see non-tracking arguments */
            if (CAR(a) && !TRACKREFS(CAR(a)))
                error(_("argument not tracking references"));
#endif
        }
}

HIDDEN void R::R_try_clear_args_refcnt(SEXP args)
{
    /* If args excapes properly its reference count will have been
       incremented. If it has no references, then it can be reverted
       to NR and the reference counts on its CAR and CDR can be
       decremented. */
    while (args && NO_REFERENCES(args))
    {
        SEXP next = CDR(args);
        DISABLE_REFCNT(args);
        DECREMENT_REFCNT(CAR(args));
        DECREMENT_REFCNT(CDR(args));
        args = next;
    }
}

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
static bool R_IsMemReporting;
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

void MemoryBank::R_ReportAllocation(R_size_t size)
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
    R_IsMemReporting = false;
    MemoryBank::setMonitor(nullptr);
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
    R_IsMemReporting = true;
    MemoryBank::setMonitor(MemoryBank::R_ReportAllocation, threshold);
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
    if (blen == (size_t)-1)
        error(_("'R_AllocStringBuffer( (size_t)-1 )' function is no longer allowed"));

    if (blen * sizeof(char) < buf.bufsize)
        return buf.data;
    blen1 = blen = (blen + 1) * sizeof(char);
    blen = (blen / bsize) * bsize;
    if (blen < blen1)
        blen += bsize;

    if (buf.data == nullptr)
    {
        buf.data = (char *)malloc(blen);
        if (buf.data)
            buf.data[0] = '\0';
    }
    else
        buf.data = (char *)realloc(buf.data, blen);
    buf.bufsize = blen;
    if (!buf.data)
    {
        buf.bufsize = 0;
        error(_("could not allocate memory (%u MB) in 'R_AllocStringBuffer()' function"),
              (unsigned int)blen / 1024 / 1024);
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
