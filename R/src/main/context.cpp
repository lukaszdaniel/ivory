/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998-2020   The R Core Team.
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
 *
 *
 *  Contexts:
 *
 *  A linked-list of execution contexts is kept so that control-flow
 *  constructs like "next", "break" and "return" will work.  It is also
 *  used for error returns to top-level.
 *
 *	context[k] -> context[k-1] -> ... -> context[0]
 *	^				     ^
 *	R_GlobalContext			     R_ToplevelContext
 *
 *  Contexts are allocated on the stack as the evaluator invokes itself
 *  recursively.  The memory is reclaimed naturally on return through
 *  the recursions (the R_GlobalContext pointer needs adjustment).
 *
 *  A context contains the following information (and more):
 *
 *	nextcontext	the next level context
 *	cjmpbuf		longjump information for non-local return
 *	cstacktop	the current level of the pointer protection stack
 *	callflag	the context "type"
 *	call		the call (name of function, or expression to
 *			get the function) that effected this
 *			context if a closure, otherwise often nullptr.
 *	callfun		the function, if this was a closure.
 *	cloenv		for closures, the environment of the closure.
 *	sysparent	the environment the closure was called from
 *	conexit		code for on.exit calls, to be executed in cloenv
 *			at exit from the closure (normal or abnormal).
 *	cend		a pointer to function which executes if there is
 *			non-local return (i.e. an error)
 *	cenddata	a void pointer to data for cend to use
 *	vmax		the current setting of the R_alloc stack
 *	srcref		the srcref at the time of the call
 *
 *  Context types can be one of:
 *
 *	CTXT_TOPLEVEL	The toplevel context
 *	CTXT_BREAK	target for "break"
 *	CTXT_NEXT	target for "next"
 *	CTXT_LOOP	target for either "break" or "next"
 *	CTXT_RETURN	target for "return" (i.e. a closure)
 *	CTXT_BROWSER	target for "return" to exit from browser
 *	CTXT_CCODE	other functions that need clean up if an error occurs
 *	CTXT_RESTART	a function call to restart was made inside the
 *			closure.
 *
 *	Code (such as the sys.xxx) that looks for CTXT_RETURN must also
 *	look for a CTXT_RESTART and CTXT_GENERIC.
 *	The mechanism used by restart is to change
 *	the context type; error/errorcall then looks for a RESTART and does
 *	a long jump there if it finds one.
 *
 *  A context is created with a call to
 *
 *	void RCNTXT::begincontext(RCNTXT &cptr, int flags,
 *			  SEXP syscall, SEXP env, SEXP
 *			  sysp, SEXP promargs, SEXP callfun)
 *
 *  which sets up the context pointed to by cptr in the appropriate way.
 *  When the context goes "out-of-scope" a call to
 *
 *	void RCNTXT::endcontext(RCNTXT *cptr)
 *
 *  restores the previous context (i.e. it adjusts the R_GlobalContext
 *  pointer).
 *
 *  The non-local jump to a given context takes place in a call to
 *
 *	void findcontext(int mask, SEXP env, SEXP val)
 *
 *  This causes "val" to be stuffed into a globally accessable place and
 *  then a search to take place back through the context list for an
 *  appropriate context.  The kind of context sort is determined by the
 *  value of "mask".  The value of mask should be the logical OR of all
 *  the context types desired.
 *
 *  The value of "mask" is returned as the value of the setjump call at
 *  the level longjumped to.  This is used to distinguish between break
 *  and next actions.
 *
 *  Contexts can be used as a wrapper around functions that create windows
 *  or open files. These can then be shut/closed gracefully if an error
 *  occurs.
 */

/** @file context.cpp
 *
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#define R_NO_REMAP
#define R_USE_SIGNALS 1

#include <CXXR/GCStackRoot.hpp>
#include <CXXR/ProtectStack.hpp>
#include <CXXR/BuiltInFunction.hpp>
#include <CXXR/JMPException.hpp>
#include <CXXR/Expression.hpp>
#include <CXXR/IntVector.hpp>
#include <CXXR/RawVector.hpp>
#include <CXXR/Symbol.hpp>
#include <CXXR/Evaluator.hpp>
#include <CXXR/Promise.hpp>
#include <Localization.h>
#include <RContext.h>
#include <Defn.h>
#include <Internal.h>

using namespace R;
using namespace CXXR;

RContext CXXR::R_Toplevel;         /* Storage for the toplevel context */
RContext *CXXR::R_ToplevelContext; /* The toplevel context */
RContext *CXXR::R_GlobalContext;   /* The global context */
RContext *CXXR::R_SessionContext;  /* The session toplevel context */
RContext *CXXR::R_ExitContext;     /* The active context for on.exit processing */

/* R_run_onexits - runs the conexit/cend code for all contexts from
   R_GlobalContext down to but not including the argument context.
   This routine does not stop at a CTXT_TOPLEVEL--the code that
   determines the argument is responsible for making sure
   CTXT_TOPLEVEL's are not crossed unless appropriate. */

RHIDDEN void RCNTXT::R_run_onexits(RCNTXT *cptr)
{
    for (RCNTXT *c = R_GlobalContext; c != cptr; c = c->nextContext()) {
	// a user embedding R incorrectly triggered this (PR#15420)
	if (!c)
	    error(_("bad target context--should NEVER happen if R was called correctly"));
	if (c->getContextEnd()) {
	    void (*cend)(void *) = c->getContextEnd();
	    c->setContextEnd(nullptr); /* prevent recursion */
	    R_HandlerStack = c->getHandlerStack();
	    R_RestartStack = c->getRestartStack();
	    cend(c->getContextEndData());
	}
	if (c->workingEnvironment() != R_NilValue && c->onExit() != R_NilValue) {
	    GCStackRoot<> s(c->onExit());
	    RCNTXT* savecontext = R_ExitContext;
	    R_ExitContext = c;
	    c->setOnExit(R_NilValue); /* prevent recursion */
	    /* we are in intermediate jump, so returnValue is undefined */
	    c->setReturnValue(nullptr);
	    R_HandlerStack = c->getHandlerStack();
	    R_RestartStack = c->getRestartStack();
	    /* Since these are run before any jumps rather than after
	       jumping to the context where the exit handler was set
	       we need to make sure there is enough room on the
	       evaluation stack in case the jump is from handling a
	       stack overflow. To be safe it is good to also call
	       R_CheckStack. LT */
	    Evaluator::extraDepth(true);
	    R_CheckStack();
	    for (; s != R_NilValue; s = CDR(s)) {
		c->setOnExit(CDR(s));
		eval(CAR(s), c->workingEnvironment());
	    }
	    R_ExitContext = savecontext;
	}
	if (R_ExitContext == c)
	    R_ExitContext = nullptr; /* Not necessary?  Better safe than sorry. */
    }
}


/* R_restore_globals - restore global variables from a target context
   before a JMPException's throw.  The target context itself is not restored here
   since this is done in R_jumpctxt below. */

void RCNTXT::R_restore_globals()
{
    ProtectStack::restoreSize(getCStackTop());
    R_BCIntActive = getBCIntactive();
    R_BCpc = getBCPC();
    R_BCbody = getBCBody();
    Evaluator::setDepth(getEvalDepth());
    vmaxset(getVMax());
    R_interrupts_suspended = (Rboolean) getIntSusp();
    R_HandlerStack = getHandlerStack();
    R_RestartStack = getRestartStack();
    while (R_PendingPromises != getPrStack()) {
	/* The value INTERRUPTED installed in PRSEEN allows forcePromise in
	   eval.cpp to signal a warning when asked to evaluate a promise
	   whose evaluation has been interrupted by a jump. */
	SET_PRSEEN(R_PendingPromises->promise, Promise::EvaluationStatus::INTERRUPTED);
	R_PendingPromises = R_PendingPromises->next;
    }
    /* Need to reset R_Expressions in case we are jumping after
       handling a stack overflow. */
    Evaluator::extraDepth(false);
    R_BCNodeStackTop = getNodeStack();
    R_Srcref = getSrcRef();
    R_BCProtReset(getBCProtTop());
}

RCNTXT *RCNTXT::first_jump_target(int mask)
{
    for (RCNTXT *c = R_GlobalContext; c && c != this; c = c->nextContext())
    {
        if ((c->workingEnvironment() != R_NilValue && c->onExit() != R_NilValue) ||
            c->getCallFlag() == CTXT_UNWIND)
        {
            c->setJumpTarget(this);
            c->setJumpMask(mask);
            return c;
        }
    }
    return this;
}

/* R_jumpctxt - jump to the named context */

RHIDDEN NORET void RCNTXT::R_jumpctxt(int mask, SEXP val)
{
    bool savevis = Evaluator::resultPrinted();
    RCNTXT *cptr;

    /* find the target for the first jump -- either an intermediate
       context with an on.exit action to run or the final target if
       there are no intermediate on.exit actions */
    cptr = first_jump_target(mask);

    /* run cend code for all contexts down to but not including
       the first jump target */
    RCNTXT::R_run_onexits(cptr);
    Evaluator::enableResultPrinting(savevis);

    R_ReturnedValue = val;
    R_GlobalContext = cptr;
    R_GlobalContext->R_restore_globals();

    /* if we are in the process of handling a C stack overflow we need
       to restore the C stack limit before the jump */
    if (R_OldCStackLimit != 0) {
	R_CStackLimit = R_OldCStackLimit;
	R_OldCStackLimit = 0;
    }

    throw JMPException(cptr, mask);
}


/* begincontext - begin an execution context */

/* begincontext and endcontext are used in dataentry.cpp and modules */
void RCNTXT::begincontext(RCNTXT *cptr, int flags,
                          SEXP syscall, SEXP env, SEXP sysp,
                          SEXP promargs, SEXP callfun)
{
    cptr->start(flags, syscall, env, sysp, promargs, callfun);
}

void RCNTXT::begincontext(RCNTXT &cptr, int flags,
                          SEXP syscall, SEXP env, SEXP sysp,
                          SEXP promargs, SEXP callfun)
{
    cptr.start(flags, syscall, env, sysp, promargs, callfun);
}

void RCNTXT::start(int flags,
                   SEXP syscall, SEXP env, SEXP sysp,
                   SEXP promargs, SEXP callfun)
{
    setCStackTop(ProtectStack::size());
    setBCPC(R_BCpc);
    setBCBody(R_BCbody);
    setBCIntactive(R_BCIntActive);
    setEvalDepth(Evaluator::depth());
    setCallFlag(flags);
    setCall(syscall);
    setWorkingEnvironment(env);
    setSysParent(sysp);
    setOnExit(R_NilValue);
    setContextEnd(nullptr);
    setPromiseArgs(promargs);
    setCallFun(callfun);
    setVMax(vmaxget());
    setIntSusp(R_interrupts_suspended);
    setHandlerStack(R_HandlerStack);
    setRestartStack(R_RestartStack);
    setPrStack(R_PendingPromises);
    setNodeStack(R_BCNodeStackTop);
    setBCProtTop(R_BCProtTop);
    setSrcRef(R_Srcref);
    setBrowserFinish(R_GlobalContext->getBrowserFinish());
    setNextContext(R_GlobalContext);
    setReturnValue(nullptr);
    setJumpTarget(nullptr);
    setJumpMask(0);

    R_GlobalContext = this;
}

/* endcontext - end an execution context */
void RCNTXT::endcontext(RCNTXT *cptr) { cptr->end(); }
void RCNTXT::endcontext(RCNTXT &cptr) { cptr.end(); }

void RCNTXT::end()
{
    R_HandlerStack = R_UnwindHandlerStack(getHandlerStack());
    R_RestartStack = getRestartStack();
    RCNTXT *jumptarget = getJumpTarget();
    if (workingEnvironment() != R_NilValue && onExit() != R_NilValue)
    {
        GCStackRoot<> s(onExit());
        bool savevis = Evaluator::resultPrinted();
        RCNTXT *savecontext = R_ExitContext;
        GCStackRoot<> saveretval(R_ReturnedValue);
        R_ExitContext = this;
        setOnExit(R_NilValue);  /* prevent recursion */
        setJumpTarget(nullptr); /* in case on.exit expr calls return() */
        R_FixupExitingHandlerResult(saveretval);
        if (getReturnValue()) // why is this needed???
            INCREMENT_LINKS(getReturnValue());
        for (; s != R_NilValue; s = CDR(s))
        {
            setOnExit(CDR(s));
            eval(CAR(s), workingEnvironment());
        }
        if (getReturnValue()) // why is this needed???
            DECREMENT_LINKS(getReturnValue());
        R_ReturnedValue = saveretval;
        R_ExitContext = savecontext;
        Evaluator::enableResultPrinting(savevis);
    }
    if (R_ExitContext == this)
        R_ExitContext = nullptr;
    /* continue jumping if this was reached as an intermetiate jump */
    if (jumptarget)
        /* returnValue is undefined */
        jumptarget->R_jumpctxt(getJumpMask(), R_ReturnedValue);

    R_GlobalContext = nextContext();
}

/* findcontext - find the correct context */

RHIDDEN NORET void R::findcontext(int mask, SEXP env, SEXP val)
{
    if (mask & CTXT_LOOP)
    { /* break/next */
        for (RCNTXT *cptr = R_GlobalContext;
             cptr && cptr->getCallFlag() != CTXT_TOPLEVEL;
             cptr = cptr->nextContext())
            if (cptr->getCallFlag() & CTXT_LOOP && cptr->workingEnvironment() == env)
                cptr->R_jumpctxt(mask, val);
        error(_("no loop for break/next, jumping to top level"));
    }
    else
    { /* return; or browser */
        for (RCNTXT *cptr = R_GlobalContext;
             cptr && cptr->getCallFlag() != CTXT_TOPLEVEL;
             cptr = cptr->nextContext())
            if ((cptr->getCallFlag() & mask) && cptr->workingEnvironment() == env)
                cptr->R_jumpctxt(mask, val);
        error(_("no function to return from, jumping to top level"));
    }
}

RHIDDEN NORET void RCNTXT::R_JumpToContext(RCNTXT *target, int mask, SEXP val)
{
    for (RCNTXT *cptr = R_GlobalContext;
         cptr && cptr->getCallFlag() != CTXT_TOPLEVEL;
         cptr = cptr->nextContext())
    {
        if (cptr == target)
            cptr->R_jumpctxt(mask, val);
        if (cptr == R_ExitContext)
            R_ExitContext = nullptr;
    }
    error(_("target context is not on the stack"));
}

/* R_sysframe - look back up the context stack until the */
/* nth closure context and return that workingEnvironment(). */
/* R_sysframe(0) means the R_GlobalEnv environment */
/* negative n counts back from the current frame */
/* positive n counts up from the globalEnv */

RHIDDEN SEXP RCNTXT::R_sysframe(int n)
{
    RCNTXT *cptr = this;
    if (n == 0)
        return R_GlobalEnv;

    if (n == NA_INTEGER)
        error(_("NA argument is invalid"));

    if (n > 0)
        n = cptr->Rf_framedepth() - n;
    else
        n = -n;

    if (n < 0)
        error(_("not that many frames on the stack"));

    while (cptr->nextContext())
    {
        if (cptr->getCallFlag() & CTXT_FUNCTION)
        {
            if (n == 0)
            { /* we need to detach the enclosing env */
                return cptr->workingEnvironment();
            }
            else
                n--;
        }
        cptr = cptr->nextContext();
    }
    if (n == 0 && cptr->nextContext() == nullptr)
        return R_GlobalEnv;
    else
        error(_("not that many frames on the stack"));
    return R_NilValue; /* just for -Wall */
}

/* We need to find the environment that can be returned by sys.frame */
/* (so it needs to be on the workingEnvironment() pointer of a context) that matches */
/* the environment where the closure arguments are to be evaluated. */
/* It would be much simpler if sysparent just returned cptr->getSysParent() */
/* but then we wouldn't be compatible with S. */

RHIDDEN int RCNTXT::R_sysparent(int n)
{
    RCNTXT *cptr = this;
    int j;
    SEXP s;
    if (n <= 0)
        errorcall(R_ToplevelContext->getCall(), _("only positive values of 'n' are allowed"));
    while (cptr->nextContext() && n > 1)
    {
        if (cptr->getCallFlag() & CTXT_FUNCTION)
            n--;
        cptr = cptr->nextContext();
    }
    /* make sure we're looking at a return context */
    while (cptr->nextContext() && !(cptr->getCallFlag() & CTXT_FUNCTION))
        cptr = cptr->nextContext();
    s = cptr->getSysParent();
    if (s == R_GlobalEnv)
        return 0;
    j = 0;
    while (cptr)
    {
        if (cptr->getCallFlag() & CTXT_FUNCTION)
        {
            j++;
            if (cptr->workingEnvironment() == s)
                n = j;
        }
        cptr = cptr->nextContext();
    }
    n = j - n + 1;
    if (n < 0)
        n = 0;
    return n;
}

RHIDDEN int RCNTXT::Rf_framedepth()
{
    RCNTXT *cptr = this;
    int nframe = 0;
    while (cptr->nextContext())
    {
        if (cptr->getCallFlag() & CTXT_FUNCTION)
            nframe++;
        cptr = cptr->nextContext();
    }
    return nframe;
}

SEXP RCNTXT::getCallWithSrcref()
{
    GCStackRoot<> result(shallow_duplicate(getCall()));
    if (getSrcRef() && !isNull(getSrcRef()))
    {
        SEXP sref;
        if (getSrcRef() == R_InBCInterpreter)
            /* FIXME: this is expensive, it might be worth changing sys.call */
            /* to return srcrefs only on request (add `with.source` option) */
            sref = R_findBCInterpreterSrcref(this);
        else
            sref = getSrcRef();
        setAttrib(result, R_SrcrefSymbol, duplicate(sref));
    }

    return result;
}

RHIDDEN SEXP RCNTXT::R_syscall(int n)
{
    RCNTXT *cptr = this;
    /* negative n counts back from the current frame */
    /* positive n counts up from the globalEnv */
    if (n > 0)
	n = cptr->Rf_framedepth() - n;
    else
	n = - n;
    if(n < 0)
	error(_("not that many frames on the stack"));
    while (cptr->nextContext()) {
	if (cptr->getCallFlag() & CTXT_FUNCTION ) {
	    if (n == 0)
		return cptr->getCallWithSrcref();
	    else
		n--;
	}
	cptr = cptr->nextContext();
    }
    if (n == 0 && cptr->nextContext() == nullptr)
	return cptr->getCallWithSrcref();
    error(_("not that many frames on the stack"));
    return R_NilValue;	/* just for -Wall */
}

RHIDDEN SEXP RCNTXT::R_sysfunction(int n)
{
    RCNTXT *cptr = this;
    if (n > 0)
        n = cptr->Rf_framedepth() - n;
    else
        n = -n;
    if (n < 0)
        error(_("not that many frames on the stack"));
    while (cptr->nextContext())
    {
        if (cptr->getCallFlag() & CTXT_FUNCTION)
        {
            if (n == 0)
                return duplicate(cptr->getCallFun()); /***** do we need to DUP? */
            else
                n--;
        }
        cptr = cptr->nextContext();
    }
    if (n == 0 && cptr->nextContext() == nullptr)
        return duplicate(cptr->getCallFun()); /***** do we need to DUP? */
    error(_("not that many frames on the stack"));
    return R_NilValue; /* just for -Wall */
}

/* count how many contexts of the specified type are present on the stack */
/* browser contexts are a bit special because they are transient and for  */
/* any closure context with the debug bit set one will be created; so we  */
/* need to count those as well                                            */
int R::Rf_countContexts(int ctxttype, bool browser)
{
    int n = 0;
    RCNTXT *cptr = R_GlobalContext;

    while (cptr != R_ToplevelContext)
    {
        if (cptr->getCallFlag() == ctxttype)
            n++;
        else if (browser)
        {
            if (cptr->getCallFlag() & CTXT_FUNCTION && ENV_RDEBUG(cptr->workingEnvironment()))
                n++;
        }
        cptr = cptr->nextContext();
    }
    return n;
}

/* functions to support looking up information about the browser */
/* contexts that are in the evaluation stack */

RHIDDEN SEXP do_sysbrowser(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP rval = R_NilValue;
    RCNTXT *cptr;
    RCNTXT *prevcptr = nullptr;
    int n;

    checkArity(op, args);
    n = asInteger(CAR(args));
    if(n < 1 ) error(_("number of contexts must be positive"));

    /* first find the closest  browser context */
    cptr = R_GlobalContext;
    while (cptr != R_ToplevelContext) {
	if (cptr->getCallFlag() == CTXT_BROWSER) {
		break;
	}
	cptr = cptr->nextContext();
    }
    /* error if not a browser context */

    if( !(cptr->getCallFlag() == CTXT_BROWSER) )
	error(_("no browser context to query"));

    switch (PRIMVAL(op)) {
    case 1: /* text */
    case 2: /* condition */
	/* first rewind to the right place if needed */
	/* note we want n>1, as we have already      */
	/* rewound to the first context              */
	if( n > 1 ) {
	   while (cptr != R_ToplevelContext && n > 0 ) {
	       if (cptr->getCallFlag() == CTXT_BROWSER) {
		   n--;
		   break;
	       }
	       cptr = cptr->nextContext();
	   }
	}
	if( !(cptr->getCallFlag() == CTXT_BROWSER) )
	   error(_("not that many calls to browser are active"));

	if( PRIMVAL(op) == 1 )
	    rval = CAR(cptr->getPromiseArgs());
	else
	    rval = CADR(cptr->getPromiseArgs());
	break;
    case 3: /* turn on debugging n levels up */
	while ( (cptr != R_ToplevelContext) && n > 0 ) {
	    if (cptr->getCallFlag() & CTXT_FUNCTION)
		  n--;
	    prevcptr = cptr;
	    cptr = cptr->nextContext();
	}
	if( !(cptr->getCallFlag() & CTXT_FUNCTION) )
	    error(_("not that many functions on the call stack"));
	if( prevcptr && prevcptr->getSrcRef() == R_InBCInterpreter ) {
	    if ( TYPEOF(cptr->getCallFun()) == CLOSXP &&
		    TYPEOF(BODY(cptr->getCallFun())) == BCODESXP )
		warning(_("debug flag in compiled function has no effect"));
	    else
		warning(_("debug will apply when function leaves compiled code"));
	}
	SET_ENV_RDEBUG(cptr->workingEnvironment(), 1);
	break;
    }
    return rval;
}

/* An implementation of S's frame access functions. They usually count */
/* up from the globalEnv while we like to count down from the currentEnv. */
/* So if the argument is negative count down if positive count up. */
/* We don't want to count the closure that do_sys is contained in, so the */
/* indexing is adjusted to handle this. */

RHIDDEN SEXP do_sys(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int i, n  = -1, nframe;
    SEXP rval, t;
    RCNTXT *cptr;

    checkArity(op, args);
    /* first find the context that sys.xxx needs to be evaluated in */
    cptr = R_GlobalContext;
    t = cptr->getSysParent();
    while (cptr != R_ToplevelContext) {
	if (cptr->getCallFlag() & CTXT_FUNCTION )
	    if (cptr->workingEnvironment() == t)
		break;
	cptr = cptr->nextContext();
    }

    if (length(args) == 1) n = asInteger(CAR(args));

    switch (PRIMVAL(op)) {
    case 1: /* parent */
	if(n == NA_INTEGER)
	    error(_("invalid '%s' argument"), "n");
	i = nframe = cptr->Rf_framedepth();
	/* This is a pretty awful kludge, but the alternative would be
	   a major redesign of everything... -pd */
	while (n-- > 0)
	    i = cptr->R_sysparent(nframe - i + 1);
	return ScalarInteger(i);
    case 2: /* call */
	if(n == NA_INTEGER)
	    error(_("invalid '%s' argument"), "which");
	return cptr->R_syscall(n);
    case 3: /* frame */
	if(n == NA_INTEGER)
	    error(_("invalid '%s' argument"), "which");
	return cptr->R_sysframe(n);
    case 4: /* sys.nframe */
	return ScalarInteger(cptr->Rf_framedepth());
    case 5: /* sys.calls */
	nframe = cptr->Rf_framedepth();
	PROTECT(rval = allocList(nframe));
	t=rval;
	for(i = 1; i <= nframe; i++, t = CDR(t))
	    SETCAR(t, cptr->R_syscall(i));
	UNPROTECT(1);
	return rval;
    case 6: /* sys.frames */
	nframe = cptr->Rf_framedepth();
	PROTECT(rval = allocList(nframe));
	t = rval;
	for(i = 1; i <= nframe; i++, t = CDR(t))
	    SETCAR(t, cptr->R_sysframe(i));
	UNPROTECT(1);
	return rval;
    case 7: /* sys.on.exit */
	{
        SEXP conexit = cptr->onExit();
        if (conexit == R_NilValue)
            return R_NilValue;
        else if (CDR(conexit) == R_NilValue)
            return CAR(conexit);
        else
            return LCONS(R_BraceSymbol, conexit);
    }
    case 8: /* sys.parents */
	nframe = cptr->Rf_framedepth();
	rval = allocVector(INTSXP, nframe);
	for(i = 0; i < nframe; i++)
	    INTEGER(rval)[i] = cptr->R_sysparent(nframe - i);
	return rval;
    case 9: /* sys.function */
	if(n == NA_INTEGER)
	    error(_("invalid '%s' value"), "which");
	return cptr->R_sysfunction(n);
    default:
	error(_("internal error in '%s' function"), "do_sys()");
	return R_NilValue;/* just for -Wall */
    }
}

RHIDDEN SEXP do_parentframe(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);

    int n = asInteger(CAR(args));
    if (n == NA_INTEGER || n < 1)
        error(_("invalid '%s' value"), "n");

    RCNTXT *cptr = RCNTXT::R_findParentContext(R_GlobalContext, n);

    return cptr ? cptr->getSysParent() : R_GlobalEnv;
}

/* R_findExecContext - Find a context frame older than `cptr` that has
   `envir` as execution environment (the `cloenv` field). */
RHIDDEN RCNTXT *RCNTXT::R_findExecContext(RCNTXT *cptr, SEXP envir)
{
    while (cptr->nextContext())
    {
        if ((cptr->getCallFlag() & CTXT_FUNCTION) != 0 && cptr->workingEnvironment() == envir)
            return cptr;
        cptr = cptr->nextContext();
    }
    return nullptr;
}

/* R_findParentContext - Find a context frame older than `cptr` whose
   execution environment (`cloenv` field) is the same as cptr's
   calling environment (`sysparent` field). In other words, find the
   frame where `cptr->syscall` was (seemingly) called. This algorithm
   powers `parent.frame()`. */
RHIDDEN RCNTXT *RCNTXT::R_findParentContext(RCNTXT *cptr, int n)
{
    while ((cptr = R_findExecContext(cptr, cptr->getSysParent())) != nullptr)
    {
        if (n == 1)
            return cptr;
        n--;
    }
    return nullptr;
}

/* R_ToplevelExec - call fun(data) within a top level context to
   insure that this functin cannot be left by a JMPException's throw.  R errors in
   the call to fun will result in a jump to top level. The return
   value is TRUE if fun returns normally, FALSE if it results in a
   jump to top level. */

Rboolean R_ToplevelExec(void (*fun)(void *), void *data)
{
    RCNTXT thiscontext;
    RCNTXT * volatile saveToplevelContext;
    volatile SEXP topExp, oldHStack, oldRStack, oldRVal;
    volatile bool oldvis;
    Rboolean result;

    PROTECT(topExp = R_CurrentExpr);
    PROTECT(oldHStack = R_HandlerStack);
    PROTECT(oldRStack = R_RestartStack);
    PROTECT(oldRVal = R_ReturnedValue);
    oldvis = Evaluator::resultPrinted();
    R_HandlerStack = R_NilValue;
    R_RestartStack = R_NilValue;
    saveToplevelContext = R_ToplevelContext;

    thiscontext.start(CTXT_TOPLEVEL, R_NilValue, R_GlobalEnv, R_BaseEnv, R_NilValue, R_NilValue);
    bool redo = false;
    bool jumped = false;
    do
    {
        redo = false;

        try
        {
            if (!jumped)
            {
                R_GlobalContext = R_ToplevelContext = &thiscontext;
                fun(data);
                result = TRUE;
            }
            else
            {
                result = FALSE;
            }
            thiscontext.end();
        }
        catch (CXXR::JMPException &e)
        {
            if (e.context() != &thiscontext)
                throw;
            redo = true;
            jumped = true;
        }
    } while (redo);

    R_ToplevelContext = saveToplevelContext;
    R_CurrentExpr = topExp;
    R_HandlerStack = oldHStack;
    R_RestartStack = oldRStack;
    R_ReturnedValue = oldRVal;
    Evaluator::enableResultPrinting(oldvis);
    UNPROTECT(4);

    return result;
}

/* Return the current environment. */
SEXP R_GetCurrentEnv()
{
    return R_GlobalContext->getSysParent();
}

/*
  This is a simple interface for evaluating R expressions
  from C with a guarantee that one will return to the
  point in the code from which the call was made (if it does
  return at all).
  This uses R_TopleveExec to do this.  It is important
  in applications that embed R or wish to make general
  callbacks to R with error handling.

  It is currently hidden with a data structure definition
  and C routine visible only here. The R_tryEval() is the
  only visible aspect. This can be lifted into the header
  files if necessary. (DTL)

  R_tryEval is in Rinternals.h (so public), but not in the API.
 */
struct ProtectedEvalData
{
    SEXP expression;
    SEXP val;
    SEXP env;
};

static void protectedEval(void *d)
{
    ProtectedEvalData *data = static_cast<ProtectedEvalData *>(d);
    SEXP env = R_GlobalEnv;
    if (data->env)
    {
        env = data->env;
    }
    data->val = eval(data->expression, env);
    R_PreserveObject(data->val);
}

SEXP R_tryEval(SEXP e, SEXP env, int *ErrorOccurred)
{
    Rboolean ok;
    ProtectedEvalData data;

    data.expression = e;
    data.val = nullptr;
    data.env = env;

    ok = R_ToplevelExec(protectedEval, &data);
    if (ErrorOccurred)
    {
        *ErrorOccurred = (ok == FALSE);
    }
    if (ok == FALSE)
        data.val = nullptr;
    else
        R_ReleaseObject(data.val);

    return (data.val);
}

/* Temporary hack to suppress error message printing around a
   R_tryEval call for use in methods_list_dispatch.cpp; should be
   replaced once we have a way of establishing error handlers from C
   code (probably would want a calling handler if we want to allow
   user-defined calling handlers to enter a debugger, for
   example). LT */
SEXP R_tryEvalSilent(SEXP e, SEXP env, int *ErrorOccurred)
{
    SEXP val;
    bool oldshow = R_ShowErrorMessages;
    R_ShowErrorMessages = FALSE;
    val = R_tryEval(e, env, ErrorOccurred);
    R_ShowErrorMessages = oldshow;
    return val;
}

SEXP R_ExecWithCleanup(SEXP (*fun)(void *), void *data,
                       void (*cleanfun)(void *), void *cleandata)
{
    RCNTXT cntxt;

    cntxt.start(CTXT_CCODE, R_NilValue, R_BaseEnv, R_BaseEnv, R_NilValue, R_NilValue);
    cntxt.setContextEnd(cleanfun, cleandata);

    GCStackRoot<> result(fun(data));
    cleanfun(cleandata);

    cntxt.end();

    return result;
}

/* Unwind-protect mechanism to support C++ stack unwinding. */

struct unwind_cont_t
{
    int jumpmask;
    RCNTXT *jumptarget;
};

SEXP R_MakeUnwindCont()
{
    return GCNode::expose(new Expression(nullptr, {Rf_allocVector(RAWSXP, sizeof(unwind_cont_t))}));
}

#define RAWDATA(x) ((void *) RAW0(x))

NORET void R_ContinueUnwind(SEXP cont)
{
    SEXP retval = CAR(cont);
    unwind_cont_t *u = (unwind_cont_t *)RAWDATA(CADR(cont));
    u->jumptarget->R_jumpctxt(u->jumpmask, retval);
}

SEXP R_UnwindProtect(SEXP (*fun)(void *data), void *data,
		     void (*cleanfun)(void *data, Rboolean jump),
		     void *cleandata, SEXP cont)
{
    RCNTXT thiscontext;
    SEXP result = nullptr;
    Rboolean jump;

    /* Allow simple usage with a nullptr continuation token. This _could_
       result in a failure in allocation or exceeding the PROTECT
       stack limit before calling fun(), so fun() and cleanfun should
       be written accordingly. */
    if (cont == nullptr) {
	PROTECT(cont = R_MakeUnwindCont());
	result = R_UnwindProtect(fun, data, cleanfun, cleandata, cont);
	UNPROTECT(1);
	return result;
    }

    thiscontext.start(CTXT_UNWIND, R_NilValue, R_GlobalEnv, R_BaseEnv, R_NilValue, R_NilValue);
    bool redo = false;
    bool jumped = false;
    do
    {
        redo = false;
        try
        {
            if (!jumped)
            {
                result = fun(data);
                SETCAR(cont, result);
                jump = FALSE;
            }
            else
            {
                jump = TRUE;
                SETCAR(cont, R_ReturnedValue);
                unwind_cont_t *u = (unwind_cont_t *)RAWDATA(CADR(cont));
                u->jumpmask = thiscontext.getJumpMask();
                u->jumptarget = thiscontext.getJumpTarget();
                thiscontext.setJumpTarget(nullptr);
            }
            thiscontext.end();
        }
        catch (CXXR::JMPException &e)
        {
            if (e.context() != &thiscontext)
                throw;
            redo = true;
            jumped = true;
        }
    } while (redo);

    cleanfun(cleandata, jump);

    if (jump)
        R_ContinueUnwind(cont);

    return result;
}
