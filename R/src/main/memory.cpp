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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#define R_NO_REMAP

#include <cstdarg>
#include <iostream>
#include <R_ext/RS.h> /* for S4 allocation */
#include <CXXR/GCEdge.hpp>
#include <CXXR/GCManager.hpp>
#include <CXXR/MemoryBank.hpp>
#include <CXXR/WeakRef.hpp>
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


#define R_USE_SIGNALS 1
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
 * @note also called from typename() in inspect.cpp
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
    case intCHARSXP: return "intCHARSXP";
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

#define GC_PROT(X)                                                        \
    do                                                                    \
    {                                                                     \
        int __wait__ = GCManager::gc_force_wait();                        \
        int __gap__ = GCManager::gc_force_gap();                          \
        Rboolean __release__ = (Rboolean)GCManager::gc_inhibit_release(); \
        X;                                                                \
        GCManager::setTortureParameters(__gap__, __wait__, __release__);  \
    } while (false)

/* There are three levels of collections.  Level 0 collects only the
   youngest generation, level 1 collects the two youngest generations,
   and level 2 collects all generations.  Higher level collections
   occur at least after specified numbers of lower level ones.  After
   LEVEL_0_FREQ level zero collections a level 1 collection is done;
   after every LEVEL_1_FREQ level 1 collections a level 2 collection
   occurs.  Thus, roughly, every LEVEL_0_FREQ-th collection is a level
   1 collection and every (LEVEL_0_FREQ * LEVEL_1_FREQ)-th collection
   is a level 2 collection.  */

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
// static R_size_t R_MaxVSize = R_SIZE_T_MAX;
// static R_size_t R_MaxNSize = R_SIZE_T_MAX;

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

HIDDEN void R::R_SetPPSize(R_size_t size)
{
    R_PPStackSize = (int) size;
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

    SEXP R_VStack = nullptr;       /* R_alloc stack pointer */
    SEXP R_PreciousList = nullptr; /* List of Persistent Objects */

    /* Debugging Routines. */

#ifdef DEBUG_ADJUST_HEAP
    static void DEBUG_ADJUST_HEAP_PRINT(double node_occup, double vect_occup)
    {
        REprintf(_("Node occupancy: %.0f%%\nVector occupancy: %.0f%%\n"), 100.0 * node_occup, 100.0 * vect_occup);
        REprintf(_("Total allocation: %lu\n"), MemoryBank::bytesAllocated());
        REprintf(_("Ncells %lu\nVcells %lu\n"), GCManager::nodeTriggerLevel(), GCManager::triggerLevel());
    }
#else
#define DEBUG_ADJUST_HEAP_PRINT(node_occup, vect_occup)
#endif /* DEBUG_ADJUST_HEAP */

    inline void INIT_REFCNT(SEXP x)
    {
#ifdef COMPUTE_REFCNT_VALUES
        SET_REFCNT(x, 0);
        SET_TRACKREFS(x, TRUE);
#endif
    }

#ifdef COMPUTE_REFCNT_VALUES
    void FIX_REFCNT_EX(SEXP x, SEXP old, SEXP new_, Rboolean chkpnd)
    {
        if (TRACKREFS(x))
        {
            if (old != new_)
            {
                if (old)
                {
                    if ((chkpnd) && ASSIGNMENT_PENDING(x))
                        SET_ASSIGNMENT_PENDING(x, FALSE);
                    else
                        DECREMENT_REFCNT(old);
                }
                if (new_)
                    INCREMENT_REFCNT(new_);
            }
        }
    }
#define FIX_REFCNT(x, old, new_) FIX_REFCNT_EX(x, old, new_, FALSE)
#define FIX_BINDING_REFCNT(x, old, new_) \
    FIX_REFCNT_EX(x, old, new_, TRUE)
#else
#define FIX_REFCNT(x, old, new_) \
    do                           \
    {                            \
    } while (false)
    void FIX_BINDING_REFCNT(SEXP x, SEXP old, SEXP new_)
    {
        if (ASSIGNMENT_PENDING(x) && old &&
            old != new_)
            SET_ASSIGNMENT_PENDING(x, FALSE);
    }
#endif

    inline void CHECK_OLD_TO_NEW(RObject *from_old, RObject *to_new)
    {
        GCEdge<> e(from_old, nullptr);
        e.redirect(from_old, to_new);
    }

} // namespace

static void checkKey(SEXP key)
{
    switch (TYPEOF(key)) {
    case NILSXP:
    case ENVSXP:
    case EXTPTRSXP:
    case BCODESXP:
	break;
    default: error(_("can only weakly reference/finalize reference objects"));
    }
}

SEXP R_MakeWeakRef(SEXP key, SEXP val, SEXP fin, Rboolean onexit)
{
    checkKey(key);
    switch (TYPEOF(fin))
    {
    case NILSXP:
    case CLOSXP:
    case BUILTINSXP:
    case SPECIALSXP:
        break;
    default:
        error(_("finalizer must be a function or NULL"));
    }
    return new WeakRef(key, val, fin, onexit);
}

SEXP R_MakeWeakRefC(SEXP key, SEXP val, R_CFinalizer_t fin, Rboolean onexit)
{
    checkKey(key);
    return new WeakRef(key, val, fin, onexit);
}

SEXP R_WeakRefKey(SEXP w)
{
    if (TYPEOF(w) != WEAKREFSXP)
        error(_("not a weak reference"));

    WeakRef* wr = SEXP_downcast<WeakRef*>(w);

    return wr->key();
}

SEXP R_WeakRefValue(SEXP w)
{
    if (TYPEOF(w) != WEAKREFSXP)
        error(_("not a weak reference"));

    WeakRef* wr = SEXP_downcast<WeakRef*>(w);

    SEXP v = wr->value();
    if (v)
        ENSURE_NAMEDMAX(v);
    return v;
}

void R_RunWeakRefFinalizer(SEXP x)
{
    WeakRef::runWeakRefFinalizer(x);
}

void WeakRef::runWeakRefFinalizer(RObject *x)
{

    WeakRef *w = SEXP_downcast<WeakRef*>(x);
    if (TYPEOF(w) != WEAKREFSXP)
        error(_("not a weak reference"));

    Rboolean oldintrsusp = R_interrupts_suspended;
    R_interrupts_suspended = TRUE;
    w->finalize();

    R_interrupts_suspended = oldintrsusp;
}

void WeakRef::finalize()
{
    R_CFinalizer_t Cfin = m_Cfinalizer;
    SEXP key, Rfin;
    PROTECT(key = m_key);
    PROTECT(Rfin = m_Rfinalizer);
    // Do this now to ensure that finalizer is run only once, even if
    // an error occurs:
    tombstone();
    if (Cfin)
        Cfin(key);
    else if (Rfin)
    {
        SEXP e;
        PROTECT(e = LCONS(Rfin, LCONS(key, nullptr)));
        eval(e, R_GlobalEnv);
        UNPROTECT(1);
    }
    UNPROTECT(2);
}

bool RunFinalizers(void)
{
	return WeakRef::runFinalizers();
}

bool WeakRef::runFinalizers()
{
    R_CHECK_THREAD;
    /* Prevent this function from running again when already in
       progress. Jumps can only occur inside the top level context
       where they will be caught, so the flag is guaranteed to be
       reset at the end. */
    static bool running = false;
    if (running)
        return false;
    running = true;


    WRList* finalization_pending = getFinalizationPending();
    bool finalizer_run = !finalization_pending->empty();

    WeakRef::check();

    WRList::reverse_iterator lit = finalization_pending->rbegin();
    WRList::reverse_iterator litend = finalization_pending->rend();
    while (lit != litend)
    {
	        WeakRef* wr = *lit--;
            /**** use R_ToplevelExec here? */
            RCNTXT thiscontext;
            RCNTXT *volatile saveToplevelContext;
            volatile SEXP topExp, oldHStack, oldRStack, oldRVal;
            volatile bool oldvis;
            PROTECT(oldHStack = R_HandlerStack);
            PROTECT(oldRStack = R_RestartStack);
            PROTECT(oldRVal = R_ReturnedValue);
            oldvis = R_Visible;
            R_HandlerStack = R_NilValue;
            R_RestartStack = R_NilValue;

            /* A top level context is established for the finalizer to
	       insure that any errors that might occur do not spill
	       into the call that triggered the collection. */
            thiscontext.start(CTXT_TOPLEVEL, R_NilValue, R_GlobalEnv, R_BaseEnv, R_NilValue, R_NilValue);
            saveToplevelContext = R_ToplevelContext;
            PROTECT(topExp = R_CurrentExpr);
            volatile int savestack = R_PPStackTop;

            if (!SETJMP(thiscontext.getCJmpBuf()))
            {
                R_GlobalContext = R_ToplevelContext = &thiscontext;

                runWeakRefFinalizer(wr);
            }
            thiscontext.end();
            UNPROTECT(1); /* next */
            R_ToplevelContext = saveToplevelContext;
            R_PPStackTop = savestack;
            R_CurrentExpr = topExp;
            R_HandlerStack = oldHStack;
            R_RestartStack = oldRStack;
            R_ReturnedValue = oldRVal;
            R_Visible = oldvis;
            UNPROTECT(4); /* topExp, oldRVal, oldRStack, oldHStack */
    }
    running = false;
    return finalizer_run;
}

void R_RunExitFinalizers(void)
{
    R_checkConstants(TRUE);
    WeakRef::runExitFinalizers();
}

void WeakRef::runExitFinalizers()
{
    WeakRef::check();
    WRList* live = getLive();
    WRList* finalization_pending = getFinalizationPending();
    WRList::iterator lit = live->begin();
    while (lit != live->end()) {
	WeakRef* wr = *lit++;
	if (wr->m_finalize_on_exit) {
	    wr->m_ready_to_finalize = true;
	    wr->transfer(live, finalization_pending);
	}
    }
    runFinalizers();
}

void R_RunPendingFinalizers(void)
{
    WeakRef::runPendingFinalizers();
}

void WeakRef::runPendingFinalizers()
{
    WRList *finalization_pending = getFinalizationPending();
    if (!finalization_pending->empty())
        runFinalizers();
}

void R_RegisterFinalizerEx(SEXP s, SEXP fun, Rboolean onexit)
{
    R_MakeWeakRef(s, R_NilValue, fun, onexit);
}

void R_RegisterFinalizer(SEXP s, SEXP fun)
{
    R_RegisterFinalizerEx(s, fun, FALSE);
}

void R_RegisterCFinalizerEx(SEXP s, R_CFinalizer_t fun, Rboolean onexit)
{
    R_MakeWeakRefC(s, R_NilValue, fun, onexit);
}

void R_RegisterCFinalizer(SEXP s, R_CFinalizer_t fun)
{
    R_RegisterCFinalizerEx(s, fun, FALSE);
}

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
    inline void MARK_THRU(GCNode::Marker *marker, const GCNode *node)
    {
        if (node)
            node->conductVisitor(marker);
    }
} // namespace

void GCNode::gc(unsigned int num_old_gens_to_collect)
{
    // std::cerr << "GCNode::gc(" << num_old_gens_to_collect << ")\n";
    GCNode::check();
    // std::cerr << "Precheck completed OK\n";

    GCNode::Marker marker(num_old_gens_to_collect);
    MARK_THRU(&marker, NA_STRING);	        /* Builtin constants */
    MARK_THRU(&marker, R_BlankString);
    MARK_THRU(&marker, R_BlankScalarString);
    MARK_THRU(&marker, R_CurrentExpression);
    MARK_THRU(&marker, R_UnboundValue);
    MARK_THRU(&marker, R_RestartToken);
    MARK_THRU(&marker, R_MissingArg);
    MARK_THRU(&marker, R_InBCInterpreter);

    MARK_THRU(&marker, R_GlobalEnv);	           /* Global environment */
    MARK_THRU(&marker, R_BaseEnv);
    MARK_THRU(&marker, R_EmptyEnv);
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

    for (RCNTXT *ctxt = R_GlobalContext ; ctxt != nullptr ; ctxt = ctxt->nextContext()) {
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

    for (int i = 0; i < R_PPStackTop; i++) /* Protected pointers */
        MARK_THRU(&marker, R_PPStack[i]);

    MARK_THRU(&marker, R_VStack); /* R_alloc stack */

    for (R_bcstack_t *sp = R_BCNodeStackBase; sp < R_BCNodeStackTop; sp++)
    {
        if (sp->tag == RAWMEM_TAG)
            sp += sp->u.ival;
        else if (sp->tag == 0 || IS_PARTIAL_SXP_TAG(sp->tag))
            MARK_THRU(&marker, sp->u.sxpval);
    }

    /* identify weakly reachable nodes */
    WeakRef::markThru(num_old_gens_to_collect);

    /* process CHARSXP cache */
    if (R_StringHash) /* in case of GC during initialization */
    {
        RObject *t;
        RObject *s;
        int nc = 0;

        for (int i = 0; i < LENGTH(R_StringHash); i++)
        {
            s = VECTOR_ELT(R_StringHash, i);
            t = R_NilValue;
            while (s != R_NilValue)
            {
                if (CXHEAD(s) && (CXHEAD(s)->m_gcgen <= num_old_gens_to_collect) && !CXHEAD(s)->isMarked())
                {                        /* remove unused CHARSXP and cons cell */
                    if (t == R_NilValue) /* head of list */
                    {
                        VECTOR_ELT(R_StringHash, i) = CXTAIL(s);
                    }
                    else
                    {
                        SET_CXTAIL(t, CXTAIL(s));
                    }
                }
                else
                {
                    MARK_THRU(&marker, s);
                    MARK_THRU(&marker, CXHEAD(s));
                    t = s;
                }
                s = CXTAIL(s);
            }
            if (VECTOR_ELT(R_StringHash, i) != R_NilValue)
                nc++;
        }
        SET_TRUELENGTH(R_StringHash, nc); /* SET_HASHPRI, really */
    }
    MARK_THRU(&marker, R_StringHash);

    // Sweep.  gen must be signed here or the loop won't terminate!
    for (int gen = num_old_gens_to_collect; gen >= 0; --gen)
    {
        if (gen == int(s_last_gen))
        {
            // Delete unmarked nodes and unmark the rest:
            const GCNode *node = s_genpeg[gen]->next();
            while (node != s_genpeg[gen])
            {
                const GCNode *next = node->next();
                if (!node->isMarked())
                {
                    delete node;
                }
                else
                {
                    node->m_marked = false;
                }
                node = next;
            }
        }
        else
        {
            // Delete unmarked nodes, unmark the rest and promote them
            // to the next generation:
            const GCNode *node = s_genpeg[gen]->next();
            while (node != s_genpeg[gen])
            {
                const GCNode *next = node->next();
                if (!node->isMarked())
                {
                    delete node;
                }
                else
                {
                    node->m_marked = false;
                    ++node->m_gcgen;
                }
                node = next;
            }
            s_genpeg[gen + 1]->splice(s_genpeg[gen]->next(), s_genpeg[gen]);
            s_gencount[gen + 1] += s_gencount[gen];
            s_gencount[gen] = 0;
        }
    }

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
        GCManager::setInhibitor(false;
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

HIDDEN SEXP do_gc(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP value;

    checkArity(op, args);
    std::ostream* report_os = GCManager::setReporting(Rf_asLogical(CAR(args)) ? &std::cerr : nullptr);
    bool reset_max = asLogical(CADR(args));
    bool full = asLogical(CADDR(args));
    GCManager::gc(0, full);
#ifndef IMMEDIATE_FINALIZERS
    R_RunPendingFinalizers();
#endif

    GCManager::setReporting(report_os);
    /*- now return the [used , gc trigger size] for cells and heap */
    PROTECT(value = allocVector(REALSXP, 14));
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
    UNPROTECT(1);
    return value;
}

namespace
{
    constexpr int PP_REDZONE_SIZE = 1000L;
    int R_StandardPPStackSize, R_RealPPStackSize;

} // namespace

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
    GCManager::initialize(R_VSize, R_NSize);
    GCManager::setReporting(R_Verbose ? &std::cerr : nullptr);

    init_gctorture();
    init_gc_grow_settings();

    arg = getenv("_R_GC_FAIL_ON_ERROR_");
    if (arg && StringTrue(arg))
        GCManager::set_gc_fail_on_error(true);
    else if (arg && StringFalse(arg))
        GCManager::set_gc_fail_on_error(false);

    R_StandardPPStackSize = R_PPStackSize;
    R_RealPPStackSize = R_PPStackSize + PP_REDZONE_SIZE;
    if (!(R_PPStack = (SEXP *) malloc(R_RealPPStackSize * sizeof(SEXP))))
	R_Suicide(_("couldn't allocate memory for pointer stack"));
    R_PPStackTop = 0;
#if VALGRIND_LEVEL > 1
    VALGRIND_MAKE_MEM_NOACCESS(R_PPStack+R_PPStackSize, PP_REDZONE_SIZE);
#endif

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

/* Since memory allocated from the heap is non-moving, R_alloc just
   allocates off the heap as RAWSXP/REALSXP and maintains the stack of
   allocations through the ATTRIB pointer.  The stack pointer R_VStack
   is traced by the collector. */
void *vmaxget(void)
{
    return (void *) R_VStack;
}

void vmaxset(const void *ovmax)
{
    R_VStack = (SEXP) ovmax;
}

char *R_alloc(size_t nelem, int eltsize)
{
    R_size_t size = nelem * eltsize;
    /* doubles are a precaution against integer overflow on 32-bit */
    double dsize = (double) nelem * eltsize;
    if (dsize > 0) {
	SEXP s;
#ifdef LONG_VECTOR_SUPPORT
	/* 64-bit platform: previous version used REALSXPs */
	if(dsize > (double) R_XLEN_T_MAX)  /* currently 4096 TB */
	    error(_("cannot allocate memory block of size %0.f TB"),
		  dsize/R_pow_di(1024.0, 4));
	s = allocVector(RAWSXP, size + 1);
#else
	if(dsize > R_LEN_T_MAX) /* must be in the Gb range */
	    error(_("cannot allocate memory block of size %0.1f GB"),
		  dsize/R_pow_di(1024.0, 3));
	s = allocVector(RAWSXP, size + 1);
#endif
	RObject::set_attrib(s, R_VStack);
	R_VStack = s;
	return (char *) DATAPTR(s);
    }
    /* One programmer has relied on this, but it is undocumented! */
    else return nullptr;
}

#ifdef HAVE_STDALIGN_H
#include <stdalign.h>
#endif

#include <cstdint>

long double *R_allocLD(size_t nelem)
{
#if __cplusplus || __alignof_is_defined
    // This is C11: picky compilers may warn.
    size_t ld_align = alignof(long double);
#elif __GNUC__
    // This is C99, but do not rely on it.
    size_t ld_align = offsetof(struct { char __a; long double __b; }, __b);
#else
    size_t ld_align = 0x0F; // value of x86_64, known others are 4 or 8
#endif
    if (ld_align > 8) {
	uintptr_t tmp = (uintptr_t) R_alloc(nelem + 1, sizeof(long double));
	tmp = (tmp + ld_align - 1) & ~((uintptr_t)ld_align - 1);
	return (long double *) tmp;
    } else {
	return (long double *) R_alloc(nelem, sizeof(long double));
    }
}


/* S COMPATIBILITY */

char *S_alloc(long nelem, int eltsize)
{
    R_size_t size = nelem * eltsize;
    char *p = R_alloc(nelem, eltsize);

    if (p)
        memset(p, 0, size);
    return p;
}

char *S_realloc(char *p, long new_, long old, int size)
{
    size_t nold;
    char *q;
    /* shrinking is a no-op */
    if (new_ <= old)
        return p; // so nnew > 0 below
    q = R_alloc((size_t)new_, size);
    nold = (size_t)old * size;
    memcpy(q, p, nold);
    memset(q + nold, 0, (size_t)new_ * size - nold);
    return q;
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
/* call gc if necessary */

SEXP Rf_allocSExp(SEXPTYPE t)
{
    SEXP s = nullptr;
    if (GCManager::FORCE_GC() || GCManager::nodeTriggerLevel() <= GCNode::numNodes())
    {
        GCManager::gc(0);
    }

    s = new RObject(t);
    INIT_REFCNT(s);
    RObject::set_car0(s, R_NilValue);
    RObject::set_cdr(s, R_NilValue);
    return s;
}

/* cons is defined directly to avoid the need to protect its arguments
   unless a GC will actually occur. */
SEXP Rf_cons(SEXP car, SEXP cdr)
{
    if (GCManager::FORCE_GC() || GCManager::nodeTriggerLevel() <= GCNode::numNodes())
    {
        PROTECT(car);
        PROTECT(cdr);
        GCManager::gc(0);
        UNPROTECT(2);
    }

        PROTECT(car);
        PROTECT(cdr);
        SEXP s = new RObject(LISTSXP);
        UNPROTECT(2);


    INIT_REFCNT(s);
    RObject::set_car0(s, CHK(car));
    if (car)
        INCREMENT_REFCNT(car);
    RObject::set_cdr(s, CHK(cdr));
    if (cdr)
        INCREMENT_REFCNT(cdr);
    return s;
}

HIDDEN SEXP CONS_NR(SEXP car, SEXP cdr)
{
    SEXP s = nullptr;
    if (GCManager::FORCE_GC() || GCManager::nodeTriggerLevel() <= GCNode::numNodes())
    {
        PROTECT(car);
        PROTECT(cdr);
        GCManager::gc(0);
        UNPROTECT(2);
    }

        PROTECT(car);
        PROTECT(cdr);
        s = new RObject(LISTSXP);
        UNPROTECT(2);


    INIT_REFCNT(s);
    DISABLE_REFCNT(s);
    RObject::set_car0(s, CHK(car));
    RObject::set_cdr(s, CHK(cdr));
    return s;
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
    SEXP v = nullptr, n = nullptr, newrho = nullptr;
    if (GCManager::FORCE_GC() || GCManager::nodeTriggerLevel() <= GCNode::numNodes())
    {
        PROTECT(namelist);
        PROTECT(valuelist);
        PROTECT(rho);
        GCManager::gc(0);
        UNPROTECT(3);
    }

        PROTECT(namelist);
        PROTECT(valuelist);
        PROTECT(rho);
        newrho = new RObject(ENVSXP);
        UNPROTECT(3);

    INIT_REFCNT(newrho);
    Environment::set_frame(newrho, valuelist);
    INCREMENT_REFCNT(valuelist);
    Environment::set_enclos(newrho, CHK(rho));
    if (rho)
        INCREMENT_REFCNT(rho);

    v = CHK(valuelist);
    n = CHK(namelist);
    while (v != R_NilValue && n != R_NilValue)
    {
        SET_TAG(v, TAG(n));
        v = CDR(v);
        n = CDR(n);
    }
    return (newrho);
}

/* mkPROMISE is defined directly do avoid the need to protect its arguments
   unless a GC will actually occur. */
HIDDEN SEXP R::mkPROMISE(SEXP expr, SEXP rho)
{
    SEXP s = nullptr;
    if (GCManager::FORCE_GC() || GCManager::nodeTriggerLevel() <= GCNode::numNodes())
    {
        PROTECT(expr);
        PROTECT(rho);
        GCManager::gc(0);
        UNPROTECT(2);
    }

        PROTECT(expr);
        PROTECT(rho);
        s = new RObject(PROMSXP);
        UNPROTECT(2);

    /* precaution to ensure code does not get modified via
       substitute() and the like */
    ENSURE_NAMEDMAX(expr);

    INIT_REFCNT(s);
    Promise::set_prcode(s, CHK(expr));
    INCREMENT_REFCNT(expr);
    Promise::set_prenv(s, CHK(rho));
    INCREMENT_REFCNT(rho);
    Promise::set_prvalue(s, R_UnboundValue);
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
    SEXP s = nullptr;     /* For the generational collector it would be safer to
		   work in terms of a VECSEXP here, but that would
		   require several casts below... */
    R_size_t size = 0;

#if VALGRIND_LEVEL > 0
    R_size_t actual_size = 0;
#endif

    if (length > R_XLEN_T_MAX)
	error(_("vector is too large")); /**** put length into message */
    else if (length < 0 )
	error(_("negative length vectors are not allowed"));
    /* number of vector cells to allocate */
    switch (type) {
    case NILSXP:
	return R_NilValue;
    case RAWSXP:
	size = BYTE2VEC(length);
#if VALGRIND_LEVEL > 0
	actual_size = length;
#endif
	break;
    case CHARSXP:
	error(_("use of allocVector(CHARSXP ...) is defunct\n"));
    case intCHARSXP:
	type = CHARSXP;
	size = BYTE2VEC(length + 1);
#if VALGRIND_LEVEL > 0
	actual_size = length + 1;
#endif
	break;
    case LGLSXP:
    case INTSXP:
	if (length <= 0)
	    size = 0;
	else {
	    if (length > (R_xlen_t) (R_SIZE_T_MAX / sizeof(int)))
		error(_("cannot allocate vector of length %d"), length);
	    size = INT2VEC(length);
#if VALGRIND_LEVEL > 0
	    actual_size = length*sizeof(int);
#endif
	}
	break;
    case REALSXP:
	if (length <= 0)
	    size = 0;
	else {
	    if (length > (R_xlen_t) (R_SIZE_T_MAX / sizeof(double)))
		error(_("cannot allocate vector of length %d"), length);
	    size = FLOAT2VEC(length);
#if VALGRIND_LEVEL > 0
	    actual_size = length * sizeof(double);
#endif
	}
	break;
    case CPLXSXP:
	if (length <= 0)
	    size = 0;
	else {
	    if (length > (R_xlen_t) (R_SIZE_T_MAX / sizeof(Rcomplex)))
		error(_("cannot allocate vector of length %d"), length);
	    size = COMPLEX2VEC(length);
#if VALGRIND_LEVEL > 0
	    actual_size = length * sizeof(Rcomplex);
#endif
	}
	break;
    case STRSXP:
    case EXPRSXP:
    case VECSXP:
	if (length <= 0)
	    size = 0;
	else {
	    if (length > (R_xlen_t) (R_SIZE_T_MAX / sizeof(SEXP)))
		error(_("cannot allocate vector of length %d"), length);
	    size = PTR2VEC(length);
#if VALGRIND_LEVEL > 0
	    actual_size = length * sizeof(SEXP);
#endif
	}
	break;
    case LANGSXP:
	if(length == 0) return R_NilValue;
#ifdef LONG_VECTOR_SUPPORT
	if (length > R_SHORT_LEN_MAX) error(_("invalid length for pairlist"));
#endif
	s = allocList((int) length);
	SET_TYPEOF(s, LANGSXP);
	return s;
    case LISTSXP:
#ifdef LONG_VECTOR_SUPPORT
	if (length > R_SHORT_LEN_MAX) error(_("invalid length for pairlist"));
#endif
	return allocList((int) length);
    default:
	error(_("invalid type/length (%s/%d) in vector allocation"),
	      type2char(type), length);
    }

    size_t bytes = size * sizeof(VECREC);

    /* we need to do the gc here so allocSExp doesn't! */
    if (GCManager::FORCE_GC() ||
        (GCManager::nodeTriggerLevel() <= GCNode::numNodes()) ||
        (GCManager::triggerLevel() < bytes + MemoryBank::bytesAllocated()))
    {
        GCManager::gc(bytes);
    }

    s = new RObject(type);
    s->m_databytes = bytes;
	// We don't want the garbage collector trying to mark this
	// node's children yet:
    R::VectorBase::set_stdvec_length(s, 0);

    if (size >= 0) {
	    bool success = false;
	    if (size < (R_SIZE_T_MAX / sizeof(VECREC))) {
            try
            {
                PROTECT(s);
                s->m_data = MemoryBank::allocate(s->m_databytes);
                UNPROTECT(1);
                success = true;
            }
            catch (bad_alloc)
            {
                // Leave s itself to the garbage collector.
                success = false;
            }
#ifdef R_MEMORY_PROFILING
		R_ReportAllocation(bytes);
#endif
	    } else s = nullptr; /* suppress warning */
	    if (!success) {
		double dsize = (double)bytes/1024.0;
		if(dsize > 1024.0*1024.0)
		    errorcall(R_NilValue,
			      _("cannot allocate vector of size %0.1f GB"),
			      dsize/1024.0/1024.0);
		if(dsize > 1024.0)
		    errorcall(R_NilValue,
			      _("cannot allocate vector of size %0.1f MB"),
			      dsize/1024.0);
		else
		    errorcall(R_NilValue,
			      _("cannot allocate vector of size %0.f KB"),
			      dsize);
	    }
    }
    R::VectorBase::set_stdvec_length(s, length);
    R::RObject::set_altrep(s, 0);
    R::VectorBase::set_stdvec_truelength(s, 0);
    INIT_REFCNT(s);

    /* The following prevents disaster in the case */
    /* that an uninitialised string vector is marked */
    /* Direct assignment is OK since the node was just allocated and */
    /* so is at least as new as R_NilValue and R_BlankString */
    if (type == EXPRSXP || type == VECSXP) {
	SEXP *data = STRING_PTR(s);
#if VALGRIND_LEVEL > 1
	VALGRIND_MAKE_MEM_DEFINED(STRING_PTR(s), actual_size);
#endif
	for (R_xlen_t i = 0; i < length; i++)
	    data[i] = R_NilValue;
    }
    else if(type == STRSXP) {
	SEXP *data = STRING_PTR(s);
#if VALGRIND_LEVEL > 1
	VALGRIND_MAKE_MEM_DEFINED(STRING_PTR(s), actual_size);
#endif
	for (R_xlen_t i = 0; i < length; i++)
	    data[i] = R_BlankString;
    }
    else if (type == CHARSXP || type == intCHARSXP) {
#if VALGRIND_LEVEL > 0
	VALGRIND_MAKE_MEM_UNDEFINED(CHAR(s), actual_size);
#endif
	CHAR_RW(s)[length] = 0;
    }
#if VALGRIND_LEVEL > 0
    else if (type == REALSXP)
	VALGRIND_MAKE_MEM_UNDEFINED(REAL(s), actual_size);
    else if (type == INTSXP)
	VALGRIND_MAKE_MEM_UNDEFINED(INTEGER(s), actual_size);
    else if (type == LGLSXP)
	VALGRIND_MAKE_MEM_UNDEFINED(LOGICAL(s), actual_size);
    else if (type == CPLXSXP)
	VALGRIND_MAKE_MEM_UNDEFINED(COMPLEX(s), actual_size);
    else if (type == RAWSXP)
	VALGRIND_MAKE_MEM_UNDEFINED(RAW(s), actual_size);
#endif
    return s;
}

/* For future hiding of allocVector(CHARSXP) */
HIDDEN SEXP R::allocCharsxp(R_len_t len)
{
    return Rf_allocVector(intCHARSXP, len);
}

SEXP Rf_allocList(const int n)
{
    SEXP result;
    result = R_NilValue;
    for (int i = 0; i < n; i++)
        result = CONS(R_NilValue, result);
    return result;
}

SEXP Rf_allocS4Object(void)
{
    SEXP s;
    GC_PROT(s = new RObject(S4SXP));
    SET_S4_OBJECT(s);
    return s;
}

static SEXP allocFormalsList(const int nargs, ...)
{
    SEXP res = R_NilValue;
    SEXP n;
    va_list syms;
    va_start(syms, nargs);

    for (int i = 0; i < nargs; i++)
    {
        res = CONS(R_NilValue, res);
    }
    R_PreserveObject(res);

    n = res;
    for (int i = 0; i < nargs; i++)
    {
        SET_TAG(n, (SEXP)va_arg(syms, SEXP));
        MARK_NOT_MUTABLE(n);
        n = CDR(n);
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
    SEXP ans, nms;
    int tmp;
    constexpr int n = 24;
    checkArity(op, args);
    PROTECT(ans = allocVector(INTSXP, n));
    PROTECT(nms = allocVector(STRSXP, n));
    for (int i = 0; i < n; i++) {
	INTEGER(ans)[i] = 0;
    SET_STRING_ELT(nms, i, Rf_type2str(SEXPTYPE(i > LGLSXP ? i + 2 : i)));
    }
    setAttrib(ans, R_NamesSymbol, nms);

    BEGIN_SUSPEND_INTERRUPTS {

      for (unsigned int gen = 0; gen < GCNode::numGenerations(); ++gen) {
	  for (const GCNode *s = GCNode::s_genpeg[gen]->next();
	       s != GCNode::s_genpeg[gen];
	       s = s->next()) {
               if (const RObject* ob = SEXP_downcast<const RObject*>(s, false)) {
	      tmp = ob->sexptype();
	      if(tmp > LGLSXP) tmp -= 2;
	      INTEGER(ans)[tmp]++;
               }
	  }
      }
    } END_SUSPEND_INTERRUPTS;
    UNPROTECT(2);
    return ans;
}

/* "protect" push a single argument onto R_PPStack */

/* In handling a stack overflow we have to be careful not to use
   PROTECT. error(_("Rf_protect(): stack overflow")) would call deparse1,
   which uses PROTECT and segfaults.*/

/* However, the traceback creation in the normal error handler also
   does a PROTECT, as does the jumping code, at least if there are
   cleanup expressions to handle on the way out.  So for the moment
   we'll allocate a slightly larger PP stack and only enable the added
   red zone during handling of a stack overflow error.  LT */

static void reset_pp_stack(void *data)
{
    int *poldpps = (int *)data;
    R_PPStackSize = *poldpps;
}

NORET void R_signal_protect_error(void)
{
    RCNTXT cntxt;
    int oldpps = R_PPStackSize;

    cntxt.start(CTXT_CCODE, R_NilValue, R_BaseEnv, R_BaseEnv, R_NilValue, R_NilValue);
    cntxt.setContextEnd(&reset_pp_stack, &oldpps);

    if (R_PPStackSize < R_RealPPStackSize)
	R_PPStackSize = R_RealPPStackSize;
    errorcall(R_NilValue, _("Rf_protect(): protection stack overflow"));

    cntxt.end(); /* not reached */
}

NORET void R_signal_unprotect_error(void)
{
    error(n_("unprotect(): only %d protected item",
		   "unprotect(): only %d protected items", R_PPStackTop),
	  R_PPStackTop);
}

#ifndef INLINE_PROTECT
SEXP Rf_protect(SEXP s)
{
    R_CHECK_THREAD;
    if (R_PPStackTop >= R_PPStackSize)
	R_signal_protect_error();
    R_PPStack[R_PPStackTop++] = CHK(s);
    return s;
}


/* "unprotect" pop argument list from top of R_PPStack */

void Rf_unprotect(int l)
{
    R_CHECK_THREAD;
    if (R_PPStackTop >=  l)
	R_PPStackTop -= l;
    else R_signal_unprotect_error();
}
#endif

/* "unprotect_ptr" remove pointer from somewhere in R_PPStack */

void Rf_unprotect_ptr(SEXP s)
{
    R_CHECK_THREAD;
    int i = R_PPStackTop;

    /* go look for  s  in  R_PPStack */
    /* (should be among the top few items) */
    do {
	if (i == 0)
	    error(_("unprotect_ptr: pointer not found"));
    } while ( R_PPStack[--i] != s );

    /* OK, got it, and  i  is indexing its location */
    /* Now drop stack above it, if any */

    while (++i < R_PPStackTop) R_PPStack[i - 1] = R_PPStack[i];

    R_PPStackTop--;
}

/* Debugging function:  is s protected? */

int Rf_isProtected(SEXP s)
{
    R_CHECK_THREAD;
    int i = R_PPStackTop;

    /* go look for  s  in  R_PPStack */
    do
    {
        if (i == 0)
            return (i);
    } while (R_PPStack[--i] != s);

    /* OK, got it, and  i  is indexing its location */
    return (i);
}

#ifndef INLINE_PROTECT
void R_ProtectWithIndex(SEXP s, PROTECT_INDEX *pi)
{
    Rf_protect(s);
    *pi = R_PPStackTop - 1;
}
#endif

NORET void R_signal_reprotect_error(PROTECT_INDEX i)
{
    error(n_("R_Reprotect: only %d protected item, can't reprotect index %d",
		   "R_Reprotect: only %d protected items, can't reprotect index %d",
		   R_PPStackTop),
	  R_PPStackTop, i);
}

#ifndef INLINE_PROTECT
void R_Reprotect(SEXP s, PROTECT_INDEX i)
{
    R_CHECK_THREAD;
    if (i >= R_PPStackTop || i < 0)
	R_signal_reprotect_error(i);
    R_PPStack[i] = s;
}
#endif

#ifdef UNUSED
/* remove all objects from the protection stack from index i upwards
   and return them in a vector. The order in the vector is from new
   to old. */
SEXP R_CollectFromIndex(PROTECT_INDEX i)
{
    R_CHECK_THREAD;
    SEXP res;
    int top = R_PPStackTop, j = 0;
    if (i > top) i = top;
    res = Rf_protect(allocVector(VECSXP, top - i));
    while (i < top)
	SET_VECTOR_ELT(res, j++, R_PPStack[--top]);
    R_PPStackTop = top; /* this includes the protect we used above */
    return res;
}
#endif

/* "initStack" initialize environment stack */
HIDDEN void R::initStack(void)
{
    R_PPStackTop = 0;
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
    if (! precious_inited) {
	precious_inited = true;
	if (getenv("R_HASH_PRECIOUS"))
	    use_precious_hash = true;
    }
    if (use_precious_hash) {
	if (R_PreciousList == R_NilValue)
	    R_PreciousList = allocVector(VECSXP, PHASH_SIZE);
	int bin = PTRHASH(object) % PHASH_SIZE;
	SET_VECTOR_ELT(R_PreciousList, bin,
		       CONS(object, VECTOR_ELT(R_PreciousList, bin)));
    }
    else
	R_PreciousList = CONS(object, R_PreciousList);
}

void R_ReleaseObject(SEXP object)
{
    R_CHECK_THREAD;
    if (! precious_inited)
	return; /* can't be anything to delete yet */
    if (use_precious_hash) {
	int bin = PTRHASH(object) % PHASH_SIZE;
	SET_VECTOR_ELT(R_PreciousList, bin,
		       DeleteFromList(object,
				      VECTOR_ELT(R_PreciousList, bin)));    
    }
    else
	R_PreciousList =  DeleteFromList(object, R_PreciousList);
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
    R_PreciousList =  DeleteFromList(object, R_PreciousList);
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
    SEXP npreserved, mset, isize;

    /* npreserved is modified in place */
    npreserved = allocVector(INTSXP, 1);
    SET_INTEGER_ELT(npreserved, 0, 0);
    PROTECT(mset = CONS(R_NilValue, npreserved));
    /* isize is not modified in place */
    if (initialSize < 0)
	error("'initialSize' must be non-negative");
    isize = ScalarInteger(initialSize);
    SET_TAG(mset, isize);
    UNPROTECT(1); /* mset */
    return mset;
}

static void checkMSet(SEXP mset)
{
    SEXP store = CAR(mset);
    SEXP npreserved = CDR(mset);
    SEXP isize = TAG(mset);
    if (/*MAYBE_REFERENCED(mset) ||*/
	((store != R_NilValue) &&
	 (TYPEOF(store) != VECSXP /*|| MAYBE_REFERENCED(store)*/)) ||
	(TYPEOF(npreserved) != INTSXP || XLENGTH(npreserved) != 1 /*||
	 MAYBE_REFERENCED(npreserved)*/) ||
	(TYPEOF(isize) != INTSXP || XLENGTH(isize) != 1))

	error("Invalid mset");
}

/* Add object to multi-set. The object will be protected as long as the
   multi-set is protected. */
void R_PreserveInMSet(SEXP x, SEXP mset)
{
    if (x == R_NilValue || isSymbol(x))
	return; /* no need to preserve */
    PROTECT(x);
    checkMSet(mset);
    SEXP store = CAR(mset);
    int *n = INTEGER(CDR(mset));
    if (store == R_NilValue) {
	R_xlen_t newsize = INTEGER_ELT(TAG(mset), 0);
	if (newsize == 0)
	    newsize = 4; /* default minimum size */
	store = allocVector(VECSXP, newsize);
	SETCAR(mset, store);
    }
    R_xlen_t size = XLENGTH(store);
    if (*n == size) {
	R_xlen_t newsize = 2 * size;
	if (newsize >= R_INT_MAX || newsize < size)
	    error("Multi-set overflow");
	SEXP newstore = PROTECT(allocVector(VECSXP, newsize));
	for(R_xlen_t i = 0; i < size; i++)
	    SET_VECTOR_ELT(newstore, i, VECTOR_ELT(store, i));
	SETCAR(mset, newstore);
	UNPROTECT(1); /* newstore */
	store = newstore;
    }
    UNPROTECT(1); /* x */
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
    int *n = INTEGER(CDR(mset));
    for(R_xlen_t i = (*n) - 1; i >= 0; i--) {
	if (VECTOR_ELT(store, i) == x) {
	    for(;i < (*n) - 1; i++)
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
    int *n = INTEGER(CDR(mset));
    if (XLENGTH(store) <= keepSize) {
	/* just free the entries */
	for(R_xlen_t i = 0; i < *n; i++)
	    SET_VECTOR_ELT(store, i, R_NilValue);
    } else
	SETCAR(mset, R_NilValue);
    *n = 0;
}

/* External Pointer Objects */
SEXP R_MakeExternalPtr(void *p, SEXP tag, SEXP prot)
{
    SEXP s = Rf_allocSExp(EXTPTRSXP);
    RObject::set_extptr_ptr(s, reinterpret_cast<RObject*>(p));
    RObject::set_extptr_prot(s, CHK(prot));
    RObject::set_extptr_tag(s, CHK(tag));
    return s;
}

void *R_ExternalPtrAddr(SEXP s)
{
    return EXTPTR_PTR(CHK(s));
}

SEXP R_ExternalPtrTag(SEXP s)
{
    return CHK(EXTPTR_TAG(CHK(s)));
}

SEXP R_ExternalPtrProtected(SEXP s)
{
    return CHK(EXTPTR_PROT(CHK(s)));
}

void R_ClearExternalPtr(SEXP s)
{
    RObject::set_extptr_ptr(s, nullptr);
}

void R_SetExternalPtrAddr(SEXP s, void *p)
{
    RObject::set_extptr_ptr(s, reinterpret_cast<RObject*>(p));
}

void R_SetExternalPtrTag(SEXP s, SEXP tag)
{
    FIX_REFCNT(s, EXTPTR_TAG(s), tag);
    CHECK_OLD_TO_NEW(s, tag);
    RObject::set_extptr_tag(s, tag);
}

void R_SetExternalPtrProtected(SEXP s, SEXP p)
{
    FIX_REFCNT(s, EXTPTR_PROT(s), p);
    CHECK_OLD_TO_NEW(s, p);
    RObject::set_extptr_prot(s, p);
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
    SEXP s = Rf_allocSExp(EXTPTRSXP);
    tmp.fn = p;
    RObject::set_extptr_ptr(s, reinterpret_cast<RObject*>(tmp.p));
    RObject::set_extptr_prot(s, CHK(prot));
    RObject::set_extptr_tag(s, CHK(tag));
    return s;
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
SEXP (ATTRIB)(SEXP x) { return CHK(R::RObject::attrib(CHK(x))); }
int (OBJECT)(SEXP x) { return R::RObject::object(CHK(x)); }
int (MARK)(RObject *x) { return R::GCNode::is_marked(static_cast<GCNode*>(CHK(x))); }
SEXPTYPE (TYPEOF)(SEXP x) { return R::RObject::typeof_(CHK(x)); }
int (NAMED)(SEXP x) { return R::RObject::named(CHK(x)); }
int (RTRACE)(SEXP x) { return R::RObject::rtrace(CHK(x)); }
int (LEVELS)(SEXP x) { return R::RObject::levels(CHK(x)); }
int (REFCNT)(SEXP x) { return R::RObject::refcnt(CHK(x)); }
int (TRACKREFS)(SEXP x) { return R::RObject::trackrefs(CHK(x)); }
int (ALTREP)(SEXP x) { return R::RObject::altrep(CHK(x)); }
int (IS_SCALAR)(SEXP x, SEXPTYPE type) { return R::RObject::is_scalar(CHK(x), type); }
int (SIMPLE_SCALAR_TYPE)(SEXP x) { return (RObject::scalar(CHK(x)) && R::RObject::attrib(CHK(x)) == R_NilValue) ? R::RObject::typeof_(CHK(x)) : 0; }
void (DECREMENT_REFCNT)(SEXP x) { DECREMENT_REFCNT(CHK(x)); }
void (INCREMENT_REFCNT)(SEXP x) { INCREMENT_REFCNT(CHK(x)); }
void (DISABLE_REFCNT)(SEXP x)  { DISABLE_REFCNT(CHK(x)); }
void (ENABLE_REFCNT)(SEXP x) { ENABLE_REFCNT(CHK(x)); }
void (MARK_NOT_MUTABLE)(SEXP x) { MARK_NOT_MUTABLE(CHK(x)); }
int (ASSIGNMENT_PENDING)(SEXP x) { return R::RObject::assignment_pending(CHK(x)); }
void (SET_ASSIGNMENT_PENDING)(SEXP x, int v)
{
    R::RObject::set_assignment_pending(CHK(x), v);
}
int (IS_ASSIGNMENT_CALL)(SEXP x) { return IS_ASSIGNMENT_CALL(CHK(x)); }
void (MARK_ASSIGNMENT_CALL)(SEXP x) { MARK_ASSIGNMENT_CALL(CHK(x)); }

void (SET_ATTRIB)(SEXP x, SEXP v) {
    if(R::RObject::typeof_(v) != LISTSXP && R::RObject::typeof_(v) != NILSXP)
	error(_("value of 'SET_ATTRIB' must be a pairlist or nullptr, not a '%s'"),
	      type2char(R::RObject::typeof_(v)));
    FIX_REFCNT(x, R::RObject::attrib(x), v);
    CHECK_OLD_TO_NEW(x, v);
    RObject::set_attrib(x, v);
}
void (SET_OBJECT)(SEXP x, int v) { R::RObject::set_object(CHK(x), v); }
void (SET_TYPEOF)(SEXP x, SEXPTYPE v) { R::RObject::set_typeof(CHK(x), v); }
void (SET_NAMED)(SEXP x, int v)
{
#ifndef SWITCH_TO_REFCNT
    SET_NAMED(CHK(x), v);
#endif
}
void (SET_RTRACE)(SEXP x, int v) { R::RObject::set_rtrace(CHK(x), v); }
void (SETLEVELS)(SEXP x, int v) { R::RObject::setlevels(CHK(x), v); }
void DUPLICATE_ATTRIB(SEXP to, SEXP from) {
    SET_ATTRIB(CHK(to), duplicate(CHK(R::RObject::attrib(CHK(from)))));
    R::RObject::set_object(CHK(to), R::RObject::object(from));
    if(R::RObject::is_s4_object(from)) { R::RObject::set_s4_object(to);} else { R::RObject::unset_s4_object(to);};
}
void SHALLOW_DUPLICATE_ATTRIB(SEXP to, SEXP from) {
    SET_ATTRIB(CHK(to), shallow_duplicate(CHK(R::RObject::attrib(CHK(from)))));
    R::RObject::set_object(CHK(to), R::RObject::object(from));
    if(R::RObject::is_s4_object(from)) { R::RObject::set_s4_object(to);} else { R::RObject::unset_s4_object(to);};
}

void (ENSURE_NAMEDMAX)(SEXP x) { ENSURE_NAMEDMAX(CHK(x)); }
void (ENSURE_NAMED)(SEXP x) { ENSURE_NAMED(CHK(x)); }
void (SETTER_CLEAR_NAMED)(SEXP x) { SETTER_CLEAR_NAMED(CHK(x)); }
void (RAISE_NAMED)(SEXP x, int n) { RAISE_NAMED(CHK(x), n); }

/* S4 object testing */
int (IS_S4_OBJECT)(SEXP x){ return R::RObject::is_s4_object(CHK(x)); }
void (SET_S4_OBJECT)(SEXP x){ R::RObject::set_s4_object(CHK(x)); }
void (UNSET_S4_OBJECT)(SEXP x){ R::RObject::unset_s4_object(CHK(x)); }

/* JIT optimization support */
int (NOJIT)(SEXP x) { return R::RObject::nojit(CHK(x)); }
int (MAYBEJIT)(SEXP x) { return R::RObject::maybejit(CHK(x)); }
void (SET_NOJIT)(SEXP x) { R::RObject::set_nojit(CHK(x)); }
void (SET_MAYBEJIT)(SEXP x) { R::RObject::set_maybejit(CHK(x)); }
void (UNSET_MAYBEJIT)(SEXP x) { R::RObject::unset_maybejit(CHK(x)); }

/* Growable vector support */
int (IS_GROWABLE)(SEXP x) { return R::RObject::growable_bit_set(CHK(x)) && XLENGTH(CHK(x)) < XTRUELENGTH(CHK(x)); }
void (SET_GROWABLE_BIT)(SEXP x) { R::RObject::set_growable_bit(CHK(x)); }

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

void (SETLENGTH)(SEXP x, R_xlen_t v)
{
    if (ALTREP(x))
        error(_("SETLENGTH() cannot be applied to an ALTVEC object."));
    if (!isVector(x))
        error(_("SETLENGTH() can only be applied to a standard vector, not a '%s'"), type2char(TYPEOF(x)));
    R::VectorBase::set_stdvec_length(CHK2(x), v);
}

void (SET_TRUELENGTH)(SEXP x, R_xlen_t v) { R::VectorBase::set_truelength(CHK2(x), v); }
int  (IS_LONG_VEC)(SEXP x) { return IS_LONG_VEC(CHK2(x)); }
#if defined(TESTING_WRITE_BARRIER) || defined(COMPILING_IVORY)
R_xlen_t (STDVEC_LENGTH)(SEXP x) { return R::VectorBase::stdvec_length(CHK2(x)); }
R_xlen_t (STDVEC_TRUELENGTH)(SEXP x) { return R::VectorBase::stdvec_truelength(CHK2(x)); }
void (SETALTREP)(SEXP x, int v) { R::RObject::set_altrep(x, v); }
#endif

/* temporary, to ease transition away from remapping */
R_xlen_t Rf_XLENGTH(SEXP x) { return XLENGTH(x); }

extern "C"
const char *(R_CHAR)(SEXP x) {
    if(TYPEOF(x) != CHARSXP) // Han-Tak proposes to prepend  'x && '
	error(_("'%s' function can only be applied to a charecter, not a '%s'"), "CHAR()",
	      type2char(TYPEOF(x)));
    return (const char *) R::r_char(CHK(x));
}

SEXP (STRING_ELT)(SEXP x, R_xlen_t i) {
    if(TYPEOF(x) != STRSXP)
	error(_("'%s' function can only be applied to a character vector, not a '%s'"), "STRING_ELT()",
	      type2char(TYPEOF(x)));
    if (ALTREP(x))
	return CHK(ALTSTRING_ELT(CHK(x), i));
    else {
	SEXP *ps = (SEXP*) R::stdvec_dataptr(CHK(x));
	return CHK(ps[i]);
    }
}

SEXP (VECTOR_ELT)(SEXP x, R_xlen_t i) {
    /* We need to allow vector-like types here */
    if(TYPEOF(x) != VECSXP &&
       TYPEOF(x) != EXPRSXP &&
       TYPEOF(x) != WEAKREFSXP)
	error(_("'%s' function can only be applied to a list, not a '%s'"), "VECTOR_ELT()",
	      type2char(TYPEOF(x)));
    return CHK(VECTOR_ELT(CHK(x), i));
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

void *(STDVEC_DATAPTR)(SEXP x)
{
    if (ALTREP(x))
	error(_("cannot get STDVEC_DATAPTR from ALTREP object"));
    if (! isVector(x) && TYPEOF(x) != WEAKREFSXP)
	error(_("STDVEC_DATAPTR can only be applied to a vector, not a '%s'"),
	      type2char(TYPEOF(x)));
    CHKZLN(x);
    return R::stdvec_dataptr(x);
}

int *(LOGICAL)(SEXP x) {
    if(TYPEOF(x) != LGLSXP)
	error(_("'%s' function can only be applied to a logical, not a '%s'"), "LOGICAL()",
	      type2char(TYPEOF(x)));
    CHKZLN(x);
    return LOGICAL(x);
}

const int *(LOGICAL_RO)(SEXP x) {
    if(TYPEOF(x) != LGLSXP)
	error(_("'%s' function can only be applied to a logical, not a '%s'"),
	      "LOGICAL()", type2char(TYPEOF(x)));
    CHKZLN(x);
    return LOGICAL_RO(x);
}

/* Maybe this should exclude logicals, but it is widely used */
int *(INTEGER)(SEXP x) {
    if(TYPEOF(x) != INTSXP && TYPEOF(x) != LGLSXP)
	error(_("'%s' function can only be applied to an integer, not a '%s'"), "INTEGER()",
	      type2char(TYPEOF(x)));
    CHKZLN(x);
    return INTEGER(x);
}

const int *(INTEGER_RO)(SEXP x) {
    if(TYPEOF(x) != INTSXP && TYPEOF(x) != LGLSXP)
	error(_("'%s' function can only be applied to an integer, not a '%s'"),
	      "INTEGER()", type2char(TYPEOF(x)));
    CHKZLN(x);
    return INTEGER_RO(x);
}

Rbyte *(RAW)(SEXP x) {
    if(TYPEOF(x) != RAWSXP)
	error(_("'%s' function can only be applied to a raw, not a '%s'"), "RAW()",
	      type2char(TYPEOF(x)));
    CHKZLN(x);
    return RAW(x);
}

const Rbyte *(RAW_RO)(SEXP x) {
    if(TYPEOF(x) != RAWSXP)
	error(_("'%s' function can only be applied to a raw, not a '%s'"),
	      "RAW()", type2char(TYPEOF(x)));
    CHKZLN(x);
    return RAW(x);
}

double *(REAL)(SEXP x) {
    if(TYPEOF(x) != REALSXP)
	error(_("'%s' function can only be applied to a numeric, not a '%s'"), "REAL()",
	      type2char(TYPEOF(x)));
    CHKZLN(x);
    return REAL(x);
}

const double *(REAL_RO)(SEXP x) {
    if(TYPEOF(x) != REALSXP)
	error(_("'%s' function can only be applied to a numeric, not a '%s'"),
	      "REAL()", type2char(TYPEOF(x)));
    CHKZLN(x);
    return REAL_RO(x);
}

Rcomplex *(COMPLEX)(SEXP x) {
    if(TYPEOF(x) != CPLXSXP)
	error(_("'%s' function can only be applied to a complex, not a '%s'"), "COMPLEX()",
	      type2char(TYPEOF(x)));
    CHKZLN(x);
    return COMPLEX(x);
}

const Rcomplex *(COMPLEX_RO)(SEXP x) {
    if(TYPEOF(x) != CPLXSXP)
	error(_("'%s' function can only be applied to a complex, not a '%s'"),
	      "COMPLEX()", "complex", type2char(TYPEOF(x)));
    CHKZLN(x);
    return COMPLEX_RO(x);
}

SEXP *(STRING_PTR)(SEXP x) {
    if(TYPEOF(x) != STRSXP)
	error(_("'%s' function can only be applied to a character, not a '%s'"),
	      "STRING_PTR()", type2char(TYPEOF(x)));
    CHKZLN(x);
    return STRING_PTR(x);
}

const SEXP *(STRING_PTR_RO)(SEXP x) {
    if(TYPEOF(x) != STRSXP)
	error(_("'%s' function can only be applied to a character, not a '%s'"),
	      "STRING_PTR_RO()", type2char(TYPEOF(x)));
    CHKZLN(x);
    return STRING_PTR_RO(x);
}

NORET SEXP *(VECTOR_PTR)(SEXP x)
{
  error(_("not safe to return vector pointer"));
}

void (SET_STRING_ELT)(SEXP x, R_xlen_t i, SEXP v) {
    if(TYPEOF(CHK(x)) != STRSXP)
	error(_("'%s' function can only be applied to a character vector, not a '%s'"), "SET_STRING_ELT()",
	      type2char(TYPEOF(x)));
    if(TYPEOF(CHK(v)) != CHARSXP)
       error(_("value of 'SET_STRING_ELT()' function must be a 'CHARSXP' not a '%s'"),
	     type2char(TYPEOF(v)));
    if (i < 0 || i >= XLENGTH(x))
	error(_("attempt to set index %ld/%ld in 'SET_STRING_ELT()' function"), (long long)i, (long long)XLENGTH(x));
    CHECK_OLD_TO_NEW(x, v);
    if (ALTREP(x))
	ALTSTRING_SET_ELT(x, i, v);
    else {
	SEXP *ps = (SEXP*) R::stdvec_dataptr(x);
	FIX_REFCNT(x, ps[i], v);
	ps[i] = v;
    }
}

SEXP (SET_VECTOR_ELT)(SEXP x, R_xlen_t i, SEXP v) {
    /*  we need to allow vector-like types here */
    if(TYPEOF(x) != VECSXP &&
       TYPEOF(x) != EXPRSXP &&
       TYPEOF(x) != WEAKREFSXP) {
	error(_("'%s' function can only be applied to a list, not a '%s'"), "SET_VECTOR_ELT()",
	      type2char(TYPEOF(x)));
    }
    if (i < 0 || i >= XLENGTH(x))
	error(_("attempt to set index %ld/%ld in 'SET_VECTOR_ELT()' function"), (long long)i, (long long)XLENGTH(x));
    FIX_REFCNT(x, VECTOR_ELT(x, i), v);
    CHECK_OLD_TO_NEW(x, v);
    return VECTOR_ELT(x, i) = v;
}

/* check for a CONS-like object */
#ifdef TESTING_WRITE_BARRIER
R_INLINE static SEXP CHKCONS(SEXP e)
{
    if (ALTREP(e))
	return CHK(e);
    switch (TYPEOF(e)) {
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
	error(_("CAR/CDR/TAG or similar applied to %s object"),
	      type2char(TYPEOF(e)));
    }
}
#else
#define CHKCONS(e) CHK(e)
#endif

HIDDEN
int (BNDCELL_TAG)(SEXP cell) { return R::RObject::bndcell_tag(cell); }
HIDDEN
void (SET_BNDCELL_TAG)(SEXP cell, int val) { R::RObject::set_bndcell_tag(cell, val); }
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
        RObject::set_car0(cell, R_NilValue);
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

/* List Accessors */
SEXP (TAG)(SEXP e) { return CHK(R::RObject::tag(CHKCONS(e))); }
SEXP (CAR0)(SEXP e) { return CHK(R::RObject::car0(CHKCONS(e))); }
SEXP (CDR)(SEXP e) { return CHK(R::RObject::cdr(CHKCONS(e))); }
SEXP (CAAR)(SEXP e) { return CHK(CAR(CAR(CHKCONS(e)))); }
SEXP (CDAR)(SEXP e) { return CHK(CDR(CAR(CHKCONS(e)))); }
SEXP (CADR)(SEXP e) { return CHK(CAR(CDR(CHKCONS(e)))); }
SEXP (CDDR)(SEXP e) { return CHK(CDR(CDR(CHKCONS(e)))); }
SEXP (CDDDR)(SEXP e) { return CHK(CDR(CDR(CDR(CHKCONS(e))))); }
SEXP (CD4R)(SEXP e) { return CHK(CDR(CDR(CDR(CDR(CHKCONS(e)))))); }
SEXP (CADDR)(SEXP e) { return CHK(CAR(CDR(CDR(CHKCONS(e))))); }
SEXP (CADDDR)(SEXP e) { return CHK(CAR(CDR(CDR(CDR(CHKCONS(e)))))); }
SEXP (CAD3R)(SEXP e) { return CHK(CAR(CDR(CDR(CDR(CHKCONS(e)))))); }
SEXP (CAD4R)(SEXP e) { return CHK(CAR(CDR(CDR(CDR(CDR(CHKCONS(e))))))); }
SEXP (CAD5R)(SEXP e) { return CHK(CAR(CDR(CDR(CDR(CDR(CDR(CHKCONS(e)))))))); }
int (MISSING)(SEXP x) { return R::RObject::missing(CHKCONS(x)); }

void (SET_TAG)(SEXP x, SEXP v)
{
    if (CHKCONS(x) == nullptr || x == R_NilValue)
        error(_("incorrect value"));
    FIX_REFCNT(x, TAG(x), v);
    CHECK_OLD_TO_NEW(x, v);
    RObject::set_tag(x, v);
}

SEXP(SETCAR)(SEXP x, SEXP y)
{
    if (CHKCONS(x) == nullptr || x == R_NilValue)
        error(_("incorrect value"));
    CLEAR_BNDCELL_TAG(x);
    if (y == CAR(x))
        return y;
    FIX_BINDING_REFCNT(x, CAR(x), y);
    CHECK_OLD_TO_NEW(x, y);
    RObject::set_car0(x, y);
    return y;
}

SEXP (SETCDR)(SEXP x, SEXP y)
{
    if (CHKCONS(x) == nullptr || x == R_NilValue)
        error(_("incorrect value"));
    FIX_REFCNT(x, CDR(x), y);
#ifdef TESTING_WRITE_BARRIER
    /* this should not add a non-tracking CDR to a tracking cell */
    if (TRACKREFS(x) && y && ! TRACKREFS(y))
	error(_("inserting non-tracking CDR in tracking cell"));
#endif
    CHECK_OLD_TO_NEW(x, y);
    RObject::set_cdr(x, y);
    return y;
}

SEXP (SETCADR)(SEXP x, SEXP y)
{
    SEXP cell;
    if (CHKCONS(x) == nullptr || x == R_NilValue ||
        CHKCONS(CDR(x)) == nullptr || CDR(x) == R_NilValue)
        error(_("incorrect value"));
    cell = CDR(x);
    CLEAR_BNDCELL_TAG(cell);
    FIX_REFCNT(cell, CAR(cell), y);
    CHECK_OLD_TO_NEW(cell, y);
    RObject::set_car0(cell, y);
    return y;
}

SEXP (SETCADDR)(SEXP x, SEXP y)
{
    SEXP cell;
    if (CHKCONS(x) == nullptr || x == R_NilValue ||
        CHKCONS(CDR(x)) == nullptr || CDR(x) == R_NilValue ||
        CHKCONS(CDDR(x)) == nullptr || CDDR(x) == R_NilValue)
        error(_("incorrect value"));
    cell = CDDR(x);
    CLEAR_BNDCELL_TAG(cell);
    FIX_REFCNT(cell, CAR(cell), y);
    CHECK_OLD_TO_NEW(cell, y);
    RObject::set_car0(cell, y);
    return y;
}

SEXP (SETCADDDR)(SEXP x, SEXP y)
{
    SEXP cell;
    if (CHKCONS(x) == nullptr || x == R_NilValue ||
        CHKCONS(CDR(x)) == nullptr || CDR(x) == R_NilValue ||
        CHKCONS(CDDR(x)) == nullptr || CDDR(x) == R_NilValue ||
        CHKCONS(CDDDR(x)) == nullptr || CDDDR(x) == R_NilValue)
        error(_("incorrect value"));
    cell = CDDDR(x);
    CLEAR_BNDCELL_TAG(cell);
    FIX_REFCNT(cell, CAR(cell), y);
    CHECK_OLD_TO_NEW(cell, y);
    RObject::set_car0(cell, y);
    return y;
}


SEXP (SETCAD4R)(SEXP x, SEXP y)
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
    FIX_REFCNT(cell, CAR(cell), y);
    CHECK_OLD_TO_NEW(cell, y);
    RObject::set_car0(cell, y);
    return y;
}

void *(EXTPTR_PTR)(SEXP x) { return R::RObject::extptr_ptr(CHK(x)); }

void (SET_MISSING)(SEXP x, int v) { R::RObject::set_missing(CHKCONS(x), v); }

/* Closure Accessors */
SEXP (FORMALS)(SEXP x) { return CHK(R::Closure::formals(CHK(x))); }
SEXP (BODY)(SEXP x) { return CHK(R::Closure::body(CHK(x))); }
SEXP (CLOENV)(SEXP x) { return CHK(R::Closure::cloenv(CHK(x))); }
int (RDEBUG)(SEXP x) { return R::Closure::rdebug(CHK(x)); }
int (RSTEP)(SEXP x) { return R::Closure::rstep(CHK(x)); }

void (SET_FORMALS)(SEXP x, SEXP v) { FIX_REFCNT(x, R::Closure::formals(x), v); CHECK_OLD_TO_NEW(x, v); R::Closure::set_formals(x, v); }
void (SET_BODY)(SEXP x, SEXP v) { FIX_REFCNT(x, R::Closure::body(x), v); CHECK_OLD_TO_NEW(x, v); R::Closure::set_body(x, v); }
void (SET_CLOENV)(SEXP x, SEXP v) { FIX_REFCNT(x, R::Closure::cloenv(x), v); CHECK_OLD_TO_NEW(x, v); R::Closure::set_cloenv(x, v); }
void (SET_RDEBUG)(SEXP x, int v) { R::Closure::set_rdebug(CHK(x), v); }
void (SET_RSTEP)(SEXP x, int v) { R::Closure::set_rstep(CHK(x), v); }

/* These are only needed with the write barrier on */
#if defined(TESTING_WRITE_BARRIER) || defined(COMPILING_IVORY)
/* Primitive Accessors */
/* not hidden since needed in some base packages */
int (PRIMOFFSET)(SEXP x) { return R::RObject::primoffset(CHK(x)); }
HIDDEN
void (SET_PRIMOFFSET)(SEXP x, int v) { R::RObject::set_primoffset(CHK(x), v); }
#endif

/* Symbol Accessors */
SEXP (PRINTNAME)(SEXP x) { return CHK(R::Symbol::printname(CHK(x))); }
SEXP (SYMVALUE)(SEXP x) { return CHK(R::Symbol::symvalue(CHK(x))); }
SEXP (INTERNAL)(SEXP x) { return CHK(R::Symbol::internal(CHK(x))); }
int (DDVAL)(SEXP x) { return R::Symbol::ddval(CHK(x)); }

void (SET_PRINTNAME)(SEXP x, SEXP v) { FIX_REFCNT(x, R::Symbol::printname(x), v); CHECK_OLD_TO_NEW(x, v); R::Symbol::set_printname(x, v); }

void (SET_SYMVALUE)(SEXP x, SEXP v)
{
    if (R::Symbol::symvalue(x) == v)
        return;
    FIX_BINDING_REFCNT(x, R::Symbol::symvalue(x), v);
    CHECK_OLD_TO_NEW(x, v);
    R::Symbol::set_symvalue(x, v);
}

void (SET_INTERNAL)(SEXP x, SEXP v) { FIX_REFCNT(x, R::Symbol::internal(x), v); CHECK_OLD_TO_NEW(x, v); R::Symbol::set_internal(x, v); }
void (SET_DDVAL)(SEXP x, int v) { R::Symbol::set_ddval(CHK(x), v); }

/* Environment Accessors */
SEXP (FRAME)(SEXP x) { return CHK(R::Environment::frame(CHK(x))); }
SEXP (ENCLOS)(SEXP x) { return CHK(R::Environment::enclos(CHK(x))); }
SEXP (HASHTAB)(SEXP x) { return CHK(R::Environment::hashtab(CHK(x))); }
int (ENVFLAGS)(SEXP x) { return R::Environment::envflags(CHK(x)); }

void (SET_FRAME)(SEXP x, SEXP v) { FIX_REFCNT(x, R::Environment::frame(x), v); CHECK_OLD_TO_NEW(x, v); R::Environment::set_frame(x, v); }
void (SET_ENCLOS)(SEXP x, SEXP v) { FIX_REFCNT(x, R::Environment::enclos(x), v); CHECK_OLD_TO_NEW(x, v); R::Environment::set_enclos(x, v); }
void (SET_HASHTAB)(SEXP x, SEXP v) { FIX_REFCNT(x, R::Environment::hashtab(x), v); CHECK_OLD_TO_NEW(x, v); R::Environment::set_hashtab(x, v); }
void (SET_ENVFLAGS)(SEXP x, int v) { R::Environment::set_envflags(x, v); }

/* Promise Accessors */
SEXP (PRCODE)(SEXP x) { return CHK(R::Promise::prcode(CHK(x))); }
SEXP (PRENV)(SEXP x) { return CHK(R::Promise::prenv(CHK(x))); }
SEXP (PRVALUE)(SEXP x) { return CHK(R::Promise::prvalue(CHK(x))); }
int (PRSEEN)(SEXP x) { return R::Promise::prseen(CHK(x)); }

void (SET_PRENV)(SEXP x, SEXP v) { FIX_REFCNT(x, R::Promise::prenv(x), v); CHECK_OLD_TO_NEW(x, v); R::Promise::set_prenv(x, v); }
void (SET_PRVALUE)(SEXP x, SEXP v) { FIX_REFCNT(x, R::Promise::prvalue(x), v); CHECK_OLD_TO_NEW(x, v); R::Promise::set_prvalue(x, v); }
void (SET_PRCODE)(SEXP x, SEXP v) { FIX_REFCNT(x, R::Promise::prcode(x), v); CHECK_OLD_TO_NEW(x, v); R::Promise::set_prcode(x, v); }
void (SET_PRSEEN)(SEXP x, int v) { R::Promise::set_prseen(CHK(x), v); }

/* Hashing Accessors */
#if defined(TESTING_WRITE_BARRIER) || defined(COMPILING_IVORY)
HIDDEN
int (HASHASH)(SEXP x) { return R::RObject::hashash(CHK(x)); }
HIDDEN
int (HASHVALUE)(SEXP x) { return (int)TRUELENGTH(CHK(x)); }

HIDDEN
void (SET_HASHASH)(SEXP x, int v) { R::RObject::set_hashash(CHK(x), v); }
HIDDEN
void (SET_HASHVALUE)(SEXP x, int v) { SET_TRUELENGTH(x, (int)v); }
#endif

HIDDEN
SEXP (SET_CXTAIL)(SEXP x, SEXP v) {
#ifdef USE_TYPE_CHECKING
    if(TYPEOF(v) != CHARSXP && TYPEOF(v) != NILSXP)
	error(_("value of 'SET_CXTAIL()' function must be a 'char' or nullptr, not a '%s'"),
	      type2char(TYPEOF(v)));
#endif
    /*CHECK_OLD_TO_NEW(x, v); *//* not needed since not properly traced */
    RObject::set_attrib(x, v);
    return x;
}

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
HIDDEN Rboolean
(IS_ACTIVE_BINDING)(SEXP b) {return (Rboolean) R::RObject::is_active_binding(CHK(b));}
HIDDEN Rboolean
(BINDING_IS_LOCKED)(SEXP b) {return (Rboolean) R::RObject::binding_is_locked(CHK(b));}
HIDDEN void
(SET_ACTIVE_BINDING_BIT)(SEXP b) { R::RObject::set_active_binding_bit(CHK(b)); }
HIDDEN void (LOCK_BINDING)(SEXP b) { R::RObject::lock_binding(CHK(b)); }
HIDDEN void (UNLOCK_BINDING)(SEXP b) { R::RObject::unlock_binding(CHK(b)); }

HIDDEN
void (SET_BASE_SYM_CACHED)(SEXP b) { R::RObject::set_base_sym_cached(CHK(b)); }
HIDDEN
void (UNSET_BASE_SYM_CACHED)(SEXP b) { R::RObject::unset_base_sym_cached(CHK(b)); }
HIDDEN
Rboolean (BASE_SYM_CACHED)(SEXP b) { return (Rboolean) R::RObject::base_sym_cached(CHK(b)); }

HIDDEN
void (SET_SPECIAL_SYMBOL)(SEXP b) { R::RObject::set_special_symbol(CHK(b)); }
HIDDEN
void (UNSET_SPECIAL_SYMBOL)(SEXP b) { R::RObject::unset_special_symbol(CHK(b)); }
HIDDEN
Rboolean (IS_SPECIAL_SYMBOL)(SEXP b) { return (Rboolean) R::RObject::is_special_symbol(CHK(b)); }
HIDDEN
void (SET_NO_SPECIAL_SYMBOLS)(SEXP b) { R::RObject::set_no_special_symbols(CHK(b)); }
HIDDEN
void (UNSET_NO_SPECIAL_SYMBOLS)(SEXP b) { R::RObject::unset_no_special_symbols(CHK(b)); }
HIDDEN
Rboolean (NO_SPECIAL_SYMBOLS)(SEXP b) { return (Rboolean) R::RObject::no_special_symbols(CHK(b)); }

/* R_FunTab accessors, only needed when write barrier is on */
/* Not hidden to allow experimentaiton without rebuilding R - LT */
/* HIDDEN */
int (PRIMVAL)(SEXP x) { return  R_FunTab[PRIMOFFSET(CHK(x))].code; }
/* HIDDEN */
CCODE (PRIMFUN)(SEXP x) { return R_FunTab[PRIMOFFSET(CHK(x))].cfun; }
/* HIDDEN */
void (SET_PRIMFUN)(SEXP x, CCODE f) { R_FunTab[PRIMOFFSET(CHK(x))].cfun = f; }

/* for use when testing the write barrier */
HIDDEN int (IS_BYTES)(SEXP x) { return R::RObject::is_bytes(CHK(x)); }
HIDDEN int (IS_LATIN1)(SEXP x) { return R::RObject::is_latin1(CHK(x)); }
HIDDEN int (IS_ASCII)(SEXP x) { return R::RObject::is_ascii(CHK(x)); }
HIDDEN int (IS_UTF8)(SEXP x) { return R::RObject::is_utf8(CHK(x)); }
HIDDEN void (SET_BYTES)(SEXP x) { R::RObject::set_bytes(CHK(x)); }
HIDDEN void (SET_LATIN1)(SEXP x) { R::RObject::set_latin1(CHK(x)); }
HIDDEN void (SET_UTF8)(SEXP x) { R::RObject::set_utf8(CHK(x)); }
HIDDEN void (SET_ASCII)(SEXP x) { R::RObject::set_ascii(CHK(x)); }
int  (ENC_KNOWN)(SEXP x) { return R::RObject::enc_known(CHK(x)); }
HIDDEN void (SET_CACHED)(SEXP x) { R::RObject::set_cached(CHK(x)); }
int  (IS_CACHED)(SEXP x) { return R::RObject::is_cached(CHK(x)); }

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
    RCNTXT *cptr;

    for (cptr = R_GlobalContext; cptr; cptr = cptr->nextContext())
    {
        if ((cptr->getCallFlag() & (CTXT_FUNCTION | CTXT_BUILTIN)) && TYPEOF(cptr->getCall()) == LANGSXP)
        {
            SEXP fun = CAR(cptr->getCall());
            fprintf(file, "\"%s\" ",
                    TYPEOF(fun) == SYMSXP ? R::r_char(PRINTNAME(fun)) : "<Anonymous>");
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
    if (strlen(R::r_char(filename)))
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

/* ======== This needs direct access to gp field for efficiency ======== */

/* this has NA_STRING = NA_STRING */
HIDDEN bool R::Seql(SEXP a, SEXP b)
{
    /* The only case where pointer comparisons do not suffice is where
      we have two strings in different encodings (which must be
      non-ASCII strings). Note that one of the strings could be marked
      as unknown. */
    if (a == b)
        return true;
    /* Leave this to compiler to optimize */
    if (IS_CACHED(a) && IS_CACHED(b) && ENC_KNOWN(a) == ENC_KNOWN(b))
        return false;
    else
    {
        SEXP vmax = R_VStack;
        bool result = streql(translateCharUTF8(a), translateCharUTF8(b));
        R_VStack = vmax; /* discard any memory used by translateCharUTF8 */
        return result;
    }
}

#ifdef LONG_VECTOR_SUPPORT
NORET R_len_t R_BadLongVector(SEXP x, const char *file, int line)
{
    error(_("long vectors not supported yet: %s:%d"), file, line);
}
#endif
