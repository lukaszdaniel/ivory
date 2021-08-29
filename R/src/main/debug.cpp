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
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#define R_NO_REMAP
#define R_USE_SIGNALS 1

#include <CXXR/BuiltInFunction.hpp>
#include <CXXR/VectorBase.hpp>
#include <CXXR/StringVector.hpp>
#include <CXXR/PairList.hpp>
#include <CXXR/LogicalVector.hpp>
#include <CXXR/Evaluator.hpp>
#include <CXXR/Symbol.hpp>
#include <Localization.h>
#include <RContext.h>
#include <Defn.h>
#include <Internal.h>

using namespace R;
using namespace CXXR;

HIDDEN SEXP do_debug(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans = R_NilValue;

    checkArity(op,args);
#define find_char_fun                                         \
    if (isValidString(CAR(args)))                             \
    {                                                         \
        SEXP s;                                               \
        PROTECT(s = installTrChar(STRING_ELT(CAR(args), 0))); \
        SETCAR(args, findFun(s, rho));                        \
        UNPROTECT(1);                                         \
    }
    find_char_fun

    if (TYPEOF(CAR(args)) != CLOSXP &&
	TYPEOF(CAR(args)) != SPECIALSXP &&
	TYPEOF(CAR(args)) != BUILTINSXP)
	error(_("argument must be a function"));
    switch(PRIMVAL(op)) {
    case 0: // debug()
	SET_RDEBUG(CAR(args), 1);
	break;
    case 1: // undebug()
	if( RDEBUG(CAR(args)) != 1 )
	    warning(_("argument is not being debugged"));
	SET_RDEBUG(CAR(args), 0);
	break;
    case 2: // isdebugged()
	ans = ScalarLogical(RDEBUG(CAR(args)));
	break;
    case 3: // debugonce()
	SET_RSTEP(CAR(args), 1);
	break;
    }
    return ans;
}

/* primitives .primTrace() and .primUntrace() */
HIDDEN SEXP do_trace(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);

    find_char_fun

    if (TYPEOF(CAR(args)) != CLOSXP &&
	TYPEOF(CAR(args)) != SPECIALSXP &&
	TYPEOF(CAR(args)) != BUILTINSXP)
	    errorcall(call, _("'%s' argument must be a function"), "what");

    switch(PRIMVAL(op)) {
    case 0:
	SET_RTRACE(CAR(args), 1);
	break;
    case 1:
	SET_RTRACE(CAR(args), 0);
	break;
    }
    return R_NilValue;
}


/* maintain global trace & debug state */

static bool tracing_state = true, debugging_state = true;
const bool &GET_TRACE_STATE = tracing_state;
const bool &GET_DEBUG_STATE = debugging_state;
inline void SET_TRACE_STATE(bool value) { tracing_state = value; }
inline void SET_DEBUG_STATE(bool value) { debugging_state = value; }

HIDDEN SEXP do_traceOnOff(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    SEXP onOff = CAR(args);
    bool trace = (PRIMVAL(op) == 0),
	prev = trace ? GET_TRACE_STATE : GET_DEBUG_STATE;

    if(length(onOff) > 0) {
	int _new = asLogical(onOff);
	if(_new == true || _new == false)
	    if(trace) SET_TRACE_STATE(_new);
	    else      SET_DEBUG_STATE(_new);
	else
	    error(_("'%s' argument must be TRUE or FALSE"), trace ? "tracingState" : "debuggingState");
    }
    return ScalarLogical(prev);
}

// GUIs, packages, etc can query:
bool R::R_current_trace_state() { return GET_TRACE_STATE; }
bool R::R_current_debug_state() { return GET_DEBUG_STATE; }


/* memory tracing */
/* report when a traced object is duplicated */

#ifdef R_MEMORY_PROFILING

HIDDEN SEXP do_tracemem(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP object;
    char buffer[21];

    checkArity(op, args);
    check1arg(args, call, "x");

    object = CAR(args);
    if (FunctionBase::isA(object))
	errorcall(call, _("argument must not be a function"));

    if(object == R_NilValue)
	errorcall(call, _("cannot trace NULL"));

    if(TYPEOF(object) == ENVSXP || TYPEOF(object) == PROMSXP)
	errorcall(call,
		  _("'tracemem()' function is not useful for promise and environment objects"));
    if(TYPEOF(object) == EXTPTRSXP || TYPEOF(object) == WEAKREFSXP)
	errorcall(call,
		  _("'tracemem()' function is not useful for weak reference or external pointer objects"));

    SET_RTRACE(object, 1);
    snprintf(buffer, 21, "<%p>", (void *) object);
    return mkString(buffer);
}

HIDDEN SEXP do_untracemem(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP object;

    checkArity(op, args);
    check1arg(args, call, "x");

    object = CAR(args);
    if (FunctionBase::isA(object))
        errorcall(call, _("argument must not be a function"));

    if (RTRACE(object))
        SET_RTRACE(object, 0);
    return R_NilValue;
}

#else

HIDDEN NORET SEXP do_tracemem(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    check1arg(args, call, "x");
    errorcall(call, _("R was not compiled with support for memory profiling"));
}

HIDDEN NORET SEXP do_untracemem(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    check1arg(args, call, "x");
    errorcall(call, _("R was not compiled with support for memory profiling"));
}

#endif /* R_MEMORY_PROFILING */

#ifndef R_MEMORY_PROFILING
void R::memtrace_report(void *old, void *_new)
{
    return;
}
#else
static void memtrace_stack_dump(void)
{
    for (RContext *cptr = R_GlobalContext; cptr; cptr = cptr->nextContext())
    {
        if ((cptr->getCallFlag() & (CTXT_FUNCTION | CTXT_BUILTIN)) && TYPEOF(cptr->getCall()) == LANGSXP)
        {
            SEXP fun = CAR(cptr->getCall());
            Rprintf("%s ",
                    TYPEOF(fun) == SYMSXP ? translateChar(PRINTNAME(fun)) : "<Anonymous>");
        }
    }
    Rprintf("\n");
}

void R::memtrace_report(void *old, void *_new)
{
    if (!R_current_trace_state())
        return;
    Rprintf("tracemem[%p -> %p]: ", (void *)old, _new);
    memtrace_stack_dump();
}

void RObject::traceMemory(const RObject *src1, const RObject *src2,
                          const RObject *src3)
{
    setMemoryTracing(true);
    Rprintf("tracemem[");
    bool needs_comma = false;
    if (src1->memoryTraced())
    {
        Rprintf("%p", src1);
        needs_comma = true;
    }
    if (src2 && src2->memoryTraced())
    {
        if (needs_comma)
            Rprintf(", ");
        Rprintf("%p", src2);
        needs_comma = true;
    }
    if (src3 && src3->memoryTraced())
    {
        if (needs_comma)
            Rprintf(", ");
        Rprintf("%p", src3);
    }
    Rprintf(" -> %p]: ", this);
    memtrace_stack_dump();
}
#endif /* R_MEMORY_PROFILING */

HIDDEN SEXP do_retracemem(SEXP call, SEXP op, SEXP args, SEXP rho)
{
#ifdef R_MEMORY_PROFILING
    SEXP object, previous, ans, argList;
    char buffer[21];
    static GCRoot<> do_retracemem_formals(nullptr);
    Rboolean visible; 

    if (do_retracemem_formals == nullptr)
	do_retracemem_formals = allocFormalsList2(Symbol::obtain("x"),
						  R_PreviousSymbol);

    PROTECT(argList =  matchArgs_NR(do_retracemem_formals, args, call));
    if(CAR(argList) == R_MissingArg) SETCAR(argList, R_NilValue);
    if(CADR(argList) == R_MissingArg) SETCAR(CDR(argList), R_NilValue);

    object = CAR(argList);
    if (FunctionBase::isA(object))
	errorcall(call, _("argument must not be a function"));

    previous = CADR(argList);
    if(!isNull(previous) && (!isString(previous) || LENGTH(previous) != 1))
	    errorcall(call, _("invalid '%s' argument"), "previous");

    if (RTRACE(object)) {
	snprintf(buffer, 21, "<%p>", (void *) object);
	visible = TRUE;
	ans = mkString(buffer);
    } else {
	visible = FALSE;
	ans = R_NilValue;
    }

    if (previous != R_NilValue){
	SET_RTRACE(object, 1);
	if (R_current_trace_state()) {
	    /* FIXME: previous will have <0x....> whereas other values are
	       without the < > */
	    Rprintf("tracemem[%s -> %p]: ",
		    translateChar(STRING_ELT(previous, 0)), (void *) object);
	    memtrace_stack_dump();
	}
    }
    UNPROTECT(1);
    Evaluator::enableResultPrinting(visible);
    return ans;
#else
    Evaluator::enableResultPrinting(false); /* for consistency with other case */
    return R_NilValue;
#endif
}
