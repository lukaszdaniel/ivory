/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995--2021  The R Core Team.
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

/** @file errors.cpp
 *
 * Error and warning handling.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#define R_NO_REMAP
#define R_USE_SIGNALS 1

#include <CXXR/GCStackRoot.hpp>
#include <CXXR/JMPException.hpp>
#include <CXXR/RObject.hpp>
#include <CXXR/String.hpp>
#include <CXXR/Environment.hpp>
#include <CXXR/ListVector.hpp>
#include <CXXR/Expression.hpp>
#include <CXXR/StringVector.hpp>
#include <CXXR/IntVector.hpp>
#include <CXXR/LogicalVector.hpp>
#include <CXXR/Symbol.hpp>
#include <CXXR/Evaluator.hpp>
#include <Localization.h>
#include <RContext.h>
#include <R.h>
#include <Defn.h>
#include <Internal.h>
/* -> Errormsg.h */
#include <Startup.h> /* rather cleanup ..*/
#include <Rconnections.h>
#include <Rinterface.h>
#include <R_ext/GraphicsEngine.h> /* for GEonExit */
#include <R_ext/Minmax.h>
#include <R_ext/Print.h>
#include <cstdarg>

using namespace std;
using namespace R;
using namespace CXXR;

/* eval() sets Evaluator::enableResultPrinting(true). Thas may not be wanted when eval() is
   used in C code. This is a version that saves/restores R_Visible.
   This should probably be moved to eval.cpp, be make public, and used
   in  more places. LT */
static SEXP evalKeepVis(SEXP e, SEXP rho)
{
	if (!e)
		return nullptr; // CXXR change
	bool oldvis = Evaluator::resultPrinted();
	SEXP val = Rf_eval(e, rho);
	Evaluator::enableResultPrinting(oldvis);
	return val;
}

/* Total line length, in chars, before splitting in warnings/errors */
#define LONGWARN max(75, int(GetOptionWidth() - 5))

/*
Different values of inError are used to indicate different places
in the error handling:
inError = 1: In internal error handling, e.g. `verrorcall_dflt`, others.
inError = 2: Writing traceback
inError = 3: In user error handler (i.e. options(error=handler))
*/
static int inError = 0;
static int inWarning = 0;
static int inPrintWarnings = 0;
static int immediateWarning = 0;
static int noBreakWarning = 0;

static void try_jump_to_restart(void);
// The next is crucial to the use of NORET attributes.
NORET static void jump_to_top_ex(Rboolean, Rboolean, Rboolean, Rboolean, Rboolean);
static void signalInterrupt(void);
static const char * R_ConciseTraceback(SEXP call, int skip);

/* Interface / Calling Hierarchy :

  R__stop()   -> do_error ->   errorcall --> (eventually) jump_to_top_ex
			 /
		error

  R__warning()-> do_warning   -> warningcall -> if(warn >= 2) errorcall
			     /
		warning /

  ErrorMessage()-> errorcall   (but with message from ErrorDB[])

  WarningMessage()-> warningcall (but with message from WarningDB[]).
*/

extern "C"
NORET void R_SignalCStackOverflow(intptr_t usage)
{
    /* We do need some stack space to process error recovery, so
       temporarily raise the limit.  We have 5% head room because we
       reduced R_CStackLimit to 95% of the initial value in
       setup_Rmainloop.
    */
    if (R_OldCStackLimit == 0) {
	R_OldCStackLimit = R_CStackLimit;
	R_CStackLimit = (uintptr_t) (R_CStackLimit / 0.95);
    }

    errorcall(R_NilValue, _("C stack usage %ld is too close to the limit"), usage);
    /* Do not translate this, to save stack space */
}

void R_CheckStack(void)
{
    int dummy;
    intptr_t usage = R_CStackDir * (R_CStackStart - (uintptr_t)&dummy);

    /* printf("usage %ld\n", usage); */
    if((int) R_CStackLimit != -1 && usage > ((intptr_t) R_CStackLimit))
	R_SignalCStackOverflow(usage);
}

void R_CheckStack2(size_t extra)
{
    int dummy;
    intptr_t usage = R_CStackDir * (R_CStackStart - (uintptr_t)&dummy);

    /* do it this way, as some compilers do usage + extra
       in unsigned arithmetic */
    usage += extra;
    if((int) R_CStackLimit != -1 && usage > ((intptr_t) R_CStackLimit))
	R_SignalCStackOverflow(usage);

}

void R_CheckUserInterrupt(void)
{
    R_CheckStack();

    /* Don't do any processing of interrupts, timing limits, or other
       asynchronous events if interrupts are suspended. */
    if (R_interrupts_suspended) return;

    /* This is the point where GUI systems need to do enough event
       processing to determine whether there is a user interrupt event
       pending.  Need to be careful not to do too much event
       processing though: if event handlers written in R are allowed
       to run at this point then we end up with concurrent R
       evaluations and that can cause problems until we have proper
       concurrency support. LT */

    R_ProcessEvents(); /* Also processes timing limits */
    if (R_interrupts_pending) Rf_onintr();
}

static SEXP getInterruptCondition();

static void onintrEx(Rboolean resumeOK)
{
    if (R_interrupts_suspended) {
	R_interrupts_pending = TRUE;
	return;
    }
    else R_interrupts_pending = FALSE;

    if (resumeOK) {
	SEXP rho = R_GlobalContext->workingEnvironment();
	int dbflag = ENV_RDEBUG(rho);
	RCNTXT restartcontext;
	restartcontext.start(CTXT_RESTART, R_NilValue, R_GlobalEnv, R_BaseEnv, R_NilValue, R_NilValue);
	bool redo = false;
	bool jumped = false;
	do
	{
		redo = false;
		try
		{
			if (!jumped)
			{
				RCNTXT::R_InsertRestartHandlers(&restartcontext, "resume");
				signalInterrupt();
			}
			else
			{
				SET_ENV_RDEBUG(rho, dbflag); /* in case browser() has messed with it */
				R_ReturnedValue = nullptr;
				Evaluator::enableResultPrinting(false);
				restartcontext.end();
				return;
			}
			restartcontext.end();
		}
		catch (CXXR::JMPException &e)
		{
			if (e.context() != &restartcontext)
				throw;
			redo = true;
			jumped = true;
		}
	} while (redo);
    }
    else signalInterrupt();

    /* Interrupts do not inherit from error, so we should not run the
       user error handler. But we have been, so as a transition,
       continue to use options('error') if options('interrupt') is not
       set */
    Rboolean tryUserError = (Rboolean) (GetOption1(Symbol::obtain("interrupt")) == R_NilValue);

    REprintf("\n");
    /* Attempt to save a traceback, show warnings, and reset console;
       also stop at restart (try/browser) frames.  Not clear this is
       what we really want, but this preserves current behavior */
    jump_to_top_ex(TRUE, tryUserError, TRUE, TRUE, FALSE);
}

void Rf_onintr()  { onintrEx(TRUE); }
void onintrNoResume() { onintrEx(FALSE); }

/* SIGUSR1: save and quit
   SIGUSR2: save and quit, don't run .Last or on.exit().

   These do far more processing than is allowed in a signal handler ....
*/

HIDDEN RETSIGTYPE R::onsigusr1(int dummy)
{
    if (R_interrupts_suspended) {
	/**** ought to save signal and handle after suspend */
	REprintf(_("interrupts suspended; signal ignored"));
	signal(SIGUSR1, onsigusr1);
	return;
    }

    inError = 1;

    if(R_CollectWarnings) PrintWarnings();

    R_ResetConsole();
    R_FlushConsole();
    R_ClearerrConsole();
    R_ParseError = 0;
    R_ParseErrorFile = nullptr;
    R_ParseErrorMsg[0] = '\0';

    /* Bail out if there is a browser/try on the stack--do we really
       want this?  No, as from R 2.4.0
    try_jump_to_restart(); */

    /* Run all onexit/cend code on the stack (without stopping at
       intervening CTXT_TOPLEVEL's.  Since intervening CTXT_TOPLEVEL's
       get used by what are conceptually concurrent computations, this
       is a bit like telling all active threads to terminate and clean
       up on the way out. */
    RCNTXT::R_run_onexits();

    R_CleanUp(SA_SAVE, 2, 1); /* quit, save,  .Last, status=2 */
}


HIDDEN RETSIGTYPE R::onsigusr2(int dummy)
{
    inError = 1;

    if (R_interrupts_suspended) {
	/**** ought to save signal and handle after suspend */
	REprintf(_("interrupts suspended; signal ignored"));
	signal(SIGUSR2, onsigusr2);
	return;
    }

    if(R_CollectWarnings) PrintWarnings();

    R_ResetConsole();
    R_FlushConsole();
    R_ClearerrConsole();
    R_ParseError = 0;
    R_ParseErrorFile = nullptr;
    R_ParseErrorMsg[0] = '\0';
    R_CleanUp(SA_SAVE, 0, 0);
}


static void setupwarnings(void)
{
    R_Warnings = Rf_allocVector(VECSXP, R_nwarnings);
    Rf_setAttrib(R_Warnings, R_NamesSymbol, Rf_allocVector(STRSXP, R_nwarnings));
}

/* Rvsnprintf_mbcs: like vsnprintf, but guaranteed to null-terminate and not to
   split multi-byte characters, except if size is zero in which case the buffer
   is untouched and thus may not be null-terminated.

   This function may be invoked by the error handler via REvprintf.  Do not
   change it unless you are SURE that your changes are compatible with the
   error handling mechanism.

   REvprintf is also used in R_Suicide on Unix.

   Dangerous pattern: `Rvsnprintf_mbcs(buf, size - n, )` with n >= size */
#ifdef _WIN32
int trio_vsnprintf(char *buffer, size_t bufferSize, const char *format,
		   va_list args);

HIDDEN
int R::Rvsnprintf_mbcs(char *buf, size_t size, const char *format, va_list ap)
{
    int val;
    val = trio_vsnprintf(buf, size, format, ap);
    if (size) {
	if (val < 0) buf[0] = '\0'; /* not all uses check val < 0 */
	else buf[size-1] = '\0';
	if (val >= size)
	    mbcsTruncateToValid(buf);
    }
    return val;
}
#else
HIDDEN
int R::Rvsnprintf_mbcs(char *buf, size_t size, const char *format, va_list ap)
{
    int val;
    val = vsnprintf(buf, size, format, ap);
    if (size) {
	if (val < 0) buf[0] = '\0'; /* not all uses check val < 0 */
	else buf[size-1] = '\0';
	if (val >= (int) size)
	    mbcsTruncateToValid(buf);
    }
    return val;
}
#endif

/* Rsnprintf_mbcs: like snprintf, but guaranteed to null-terminate and
   not to split multi-byte characters, except if size is zero in which
   case the buffer is untouched and thus may not be null-terminated.

   Dangerous pattern: `Rsnprintf_mbcs(buf, size - n, )` with maybe n >= size*/
HIDDEN
int R::Rsnprintf_mbcs(char *str, size_t size, const char *format, ...)
{
    int val;
    va_list ap;

    va_start(ap, format);
    val = Rvsnprintf_mbcs(str, size, format, ap);
    va_end(ap);

    return val;
}

/* Rstrncat: like strncat, but guaranteed not to split multi-byte characters */
static char *Rstrncat(char *dest, const char *src, size_t n)
{
    size_t after;
    size_t before = strlen(dest);

    strncat(dest, src, n);

    after = strlen(dest);
    if (after - before == n)
	/* the string may have been truncated, but we cannot know for sure
	   because str may not be null terminated */
	mbcsTruncateToValid(dest + before);

    return dest;
}

/* Rstrncpy: like strncpy, but guaranteed to null-terminate and not to
   split multi-byte characters */
static char *Rstrncpy(char *dest, const char *src, size_t n)
{
	strncpy(dest, src, n);
	if (n)
	{
		dest[n - 1] = '\0';
		mbcsTruncateToValid(dest);
	}
	return dest;
}

constexpr size_t BUFSIZE = 8192;
R_INLINE static void RprintTrunc(char *buf, int truncated)
{
    if (truncated) {
	char *msg = _("[... truncated]");
	if (strlen(buf) + 1 + strlen(msg) < BUFSIZE) {
	    strcat(buf, " ");
	    strcat(buf, msg);
	}
    }
}

static SEXP getCurrentCall()
{
	RCNTXT *c = R_GlobalContext;

	/* This can be called before R_GlobalContext is defined, so... */
	/* If profiling is on, this can be a CTXT_BUILTIN */

	if (c && (c->getCallFlag() & CTXT_BUILTIN))
		c = c->nextContext();
	if (c == R_GlobalContext && R_BCIntActive)
	{
		return R_getBCInterpreterExpression();
	}

	return c ? c->getCall() : R_NilValue;
}

void Rf_warning(const char *format, ...)
{
    char buf[BUFSIZE], *p;

    va_list ap;
    va_start(ap, format);
    size_t psize;
    int pval;

    psize = min(BUFSIZE, R_WarnLength+1);
    pval = Rvsnprintf_mbcs(buf, psize, format, ap);
    va_end(ap);
    p = buf + strlen(buf) - 1;
    if(strlen(buf) > 0 && *p == '\n') *p = '\0';
    RprintTrunc(buf, pval >= (int) psize);
    warningcall(getCurrentCall(), "%s", buf);
}

/* declarations for internal condition handling */

static void vsignalError(SEXP call, const char *format, va_list ap);
static void vsignalWarning(SEXP call, const char *format, va_list ap);
NORET static void invokeRestart(SEXP, SEXP);

static void reset_inWarning(void *data)
{
    inWarning = 0;
}

#include <rlocale.h>

static int wd(const char *buf)
{
    int nc = (int) mbstowcs(nullptr, buf, 0), nw;
    if(nc > 0 && nc < 2000) {
	wchar_t wc[2000];
	mbstowcs(wc, buf, nc + 1);
	// FIXME: width could conceivably exceed MAX_INT.
#ifdef USE_RI18N_WIDTH
	nw = Ri18n_wcswidth(wc, 2147483647);
#else
	nw = wcswidth(wc, 2147483647);
#endif
	return (nw < 1) ? nc : nw;
    }
    return nc;
}

static void vwarningcall_dflt(SEXP call, const char *format, va_list ap)
{
    int w;
    SEXP names, s;
    const char *dcall;
    char buf[BUFSIZE];
    RCNTXT *cptr;
    RCNTXT cntxt;
    size_t psize;
    int pval;

    if (inWarning)
	return;

    s = GetOption1(Symbol::obtain("warning.expression"));
    if( s != R_NilValue ) {
	if( !isLanguage(s) &&  ! isExpression(s) ) {
	    error(_("invalid \"%s\" option"), "warning.expression");
	}
	cptr = R_GlobalContext;
	while ( !(cptr->getCallFlag() & CTXT_FUNCTION) && cptr->getCallFlag() )
	    cptr = cptr->nextContext();
	evalKeepVis(s, cptr->workingEnvironment());
	return;
    }

    w = asInteger(GetOption1(Symbol::obtain("warn")));

    if( w == NA_INTEGER ) /* set to a sensible value */
	w = 0;

    if( w <= 0 && immediateWarning ) w = 1;

    if( w < 0 || inWarning || inError) /* ignore if w<0 or already in here*/
	return;

    /* set up a context which will restore inWarning if there is an exit */
    cntxt.start(CTXT_CCODE, R_NilValue, R_BaseEnv, R_BaseEnv, R_NilValue, R_NilValue);
    cntxt.setContextEnd(&reset_inWarning);

    inWarning = 1;

    if(w >= 2) { /* make it an error */
	psize = min(BUFSIZE, R_WarnLength+1);
	pval = Rvsnprintf_mbcs(buf, psize, format, ap);
	RprintTrunc(buf, pval >= (int) psize);
	inWarning = 0; /* PR#1570 */
	errorcall(call, _("(converted from warning) %s"), buf);
    }
    else if(w == 1) {	/* print as they happen */
	const char *tr;
	if( call != R_NilValue ) {
	    dcall = CHAR(STRING_ELT(deparse1s(call), 0));
	} else dcall = "";
	psize = min(BUFSIZE, R_WarnLength+1);
	pval = Rvsnprintf_mbcs(buf, psize, format, ap);
	RprintTrunc(buf, pval >= (int) psize);

	if(dcall[0] == '\0') REprintf(_("Warning:"));
	else {
	    REprintf(_("Warning in command '%s':"), dcall);
	    if(!(noBreakWarning ||
		 ( mbcslocale && 18 + wd(dcall) + wd(buf) <= (int) LONGWARN) ||
		 (!mbcslocale && 18 + strlen(dcall) + strlen(buf) <= (size_t) LONGWARN)))
		REprintf("\n ");
	}
	REprintf(" %s\n", buf);
	if(R_ShowWarnCalls && call != R_NilValue) {
	    tr = R_ConciseTraceback(call, 0);
	    if (strlen(tr)) {REprintf(_("Calls:")); REprintf(" %s\n", tr);}
	}
    }
    else if(w == 0) {	/* collect them */
	if(!R_CollectWarnings) setupwarnings();
	if(R_CollectWarnings < R_nwarnings) {
	    SET_VECTOR_ELT(R_Warnings, R_CollectWarnings, call);
	    psize = min(BUFSIZE, R_WarnLength+1);
	    pval = Rvsnprintf_mbcs(buf, psize, format, ap);
	    RprintTrunc(buf, pval >= (int) psize);
	    if(R_ShowWarnCalls && call != R_NilValue) {
		const char *tr =  R_ConciseTraceback(call, 0);
		size_t nc = strlen(tr);
		if (nc && nc + (int)strlen(buf) + 8 < BUFSIZE) {
		    strcat(buf, "\n");
		    strcat(buf, _("Calls:"));
		    strcat(buf, " ");
		    strcat(buf, tr);
		}
	    }
	    names = CAR(ATTRIB(R_Warnings));
	    SET_STRING_ELT(names, R_CollectWarnings++, mkChar(buf));
	}
    }
    /* else:  w <= -1 */
    cntxt.end();
    inWarning = 0;
}

static void warningcall_dflt(SEXP call, const char *format,...)
{
    va_list ap;

    va_start(ap, format);
    vwarningcall_dflt(call, format, ap);
    va_end(ap);
}

void warningcall(SEXP call, const char *format, ...)
{
    va_list ap;
    va_start(ap, format);
    vsignalWarning(call, format, ap);
    va_end(ap);
}

void Rf_warningcall_immediate(SEXP call, const char *format, ...)
{
    va_list ap;

    immediateWarning = 1;
    va_start(ap, format);
    vsignalWarning(call, format, ap);
    va_end(ap);
    immediateWarning = 0;
}

static void cleanup_PrintWarnings(void *data)
{
	if (R_CollectWarnings)
	{
		R_CollectWarnings = 0;
		R_Warnings = R_NilValue;
		REprintf(_("Lost warning messages"));
		REprintf("\n");
	}
	inPrintWarnings = 0;
}

HIDDEN
void R::PrintWarnings(const char *hdr)
{
    int i;
	const char *header = hdr ? hdr : n_("Warning message:", "Warning messages:", R_CollectWarnings);
	SEXP names, s, t;
    RCNTXT cntxt;

    if (R_CollectWarnings == 0)
	return;
    else if (inPrintWarnings) {
	if (R_CollectWarnings) {
	    R_CollectWarnings = 0;
	    R_Warnings = R_NilValue;
	    REprintf(_("Lost warning messages"));
	    REprintf("\n");
	}
	return;
    }

    /* set up a context which will restore inPrintWarnings if there is an exit */
    cntxt.start( CTXT_CCODE, R_NilValue, R_BaseEnv, R_BaseEnv, R_NilValue, R_NilValue);
    cntxt.setContextEnd(&cleanup_PrintWarnings);

    inPrintWarnings = 1;
    if( R_CollectWarnings == 1 ) {
	REprintf("%s\n", header);
	names = CAR(ATTRIB(R_Warnings));
	if( VECTOR_ELT(R_Warnings, 0) == R_NilValue )
	    REprintf("%s \n", CHAR(STRING_ELT(names, 0)));
	else {
	    const char *dcall, *msg = CHAR(STRING_ELT(names, 0));
	    dcall = CHAR(STRING_ELT(deparse1s(VECTOR_ELT(R_Warnings, 0)), 0));
	    REprintf(_("In command '%s':"), dcall);
	    if (mbcslocale) {
		int msgline1;
		const char *p = strchr(msg, '\n');
		if (p) {
		    char *q = (char*) (p);
		    *q = '\0';
		    msgline1 = wd(msg);
		    *q = '\n';
		} else msgline1 = wd(msg);
		if (6 + wd(dcall) + msgline1 > (int) LONGWARN) REprintf("\n ");
	    } else {
		size_t msgline1 = strlen(msg);
		const char *p = strchr(msg, '\n');
		if (p) msgline1 = (int)(p - msg);
		if (6 + strlen(dcall) + msgline1 > (size_t) LONGWARN) REprintf("\n ");
	    }
	    REprintf(" %s\n", msg);
	}
    } else if( R_CollectWarnings <= 10 ) {
	REprintf("%s\n", header);
	names = CAR(ATTRIB(R_Warnings));
	for(i = 0; i < R_CollectWarnings; i++) {
	    if( VECTOR_ELT(R_Warnings, i) == R_NilValue ) {
		REprintf("%d: %s \n", i+1, CHAR(STRING_ELT(names, i)));
	    } else {
		const char *dcall, *msg = CHAR(STRING_ELT(names, i));
		dcall = CHAR(STRING_ELT(deparse1s(VECTOR_ELT(R_Warnings, i)), 0));
		REprintf("%d: ", i + 1);
		REprintf(_("In command '%s':"), dcall); 
		if (mbcslocale) {
		    int msgline1;
		    char *p = (char*) strchr(msg, '\n');
		    if (p) {
			*p = '\0';
			msgline1 = wd(msg);
			*p = '\n';
		    } else msgline1 = wd(msg);
		    if (10 + wd(dcall) + msgline1 > (int) LONGWARN) {
			REprintf("\n ");
		    }
		} else {
		    size_t msgline1 = strlen(msg);
		    const char *p = strchr(msg, '\n');
		    if (p) msgline1 = (int)(p - msg);
		    if (10 + strlen(dcall) + msgline1 > (size_t) LONGWARN) {
			REprintf("\n ");
		    }
		}
		REprintf(" %s\n", msg);
	    }
	}
    } else {
		if (R_CollectWarnings < R_nwarnings)
		{
			REprintf(n_("There was %d warning (use 'warnings()' to see it)",
						"There were %d warnings (use 'warnings()' to see them)",
						R_CollectWarnings),
					 R_CollectWarnings);
		}
		else
		{
			REprintf(_("There were %d or more warnings (use 'warnings()' to see the first %d)"),
					 R_nwarnings, R_nwarnings);
		}
		REprintf("\n");
	}
	/* now truncate and install last.warning */
    PROTECT(s = allocVector(VECSXP, R_CollectWarnings));
    PROTECT(t = allocVector(STRSXP, R_CollectWarnings));
    names = CAR(ATTRIB(R_Warnings));
    for(i = 0; i < R_CollectWarnings; i++) {
	SET_VECTOR_ELT(s, i, VECTOR_ELT(R_Warnings, i));
	SET_STRING_ELT(t, i, STRING_ELT(names, i));
    }
    setAttrib(s, R_NamesSymbol, t);
    SET_SYMVALUE(Symbol::obtain("last.warning"), s);
    UNPROTECT(2);

    cntxt.end();

    inPrintWarnings = 0;
    R_CollectWarnings = 0;
    R_Warnings = R_NilValue;
    return;
}

/* Return a constructed source location (e.g. filename#123) from a srcref.  If the srcref
   is not valid "" will be returned.
*/

static SEXP GetSrcLoc(SEXP srcref)
{
	SEXP sep, line, result, srcfile;
	if (TYPEOF(srcref) != INTSXP || length(srcref) < 4)
		return ScalarString(mkChar(""));

	PROTECT(srcref);
	PROTECT(srcfile = R_GetSrcFilename(srcref));
	SEXP e2 = PROTECT(lang2(Symbol::obtain("basename"), srcfile));
	PROTECT(srcfile = eval(e2, R_BaseEnv));
	PROTECT(sep = ScalarString(mkChar("#")));
	PROTECT(line = ScalarInteger(INTEGER(srcref)[0]));
	SEXP e = PROTECT(lang4(Symbol::obtain("paste0"), srcfile, sep, line));
	result = eval(e, R_BaseEnv);
	UNPROTECT(7);
	return result;
}

static char errbuf[BUFSIZE + 1]; /* add 1 to leave room for a null byte */

#define ERRBUFCAT(txt) Rstrncat(errbuf, txt, BUFSIZE - strlen(errbuf))

const char *R_curErrorBuf()
{
	return (const char *)errbuf;
}

/* temporary hook to allow experimenting with alternate error mechanisms */
static void (*R_ErrorHook)(SEXP, char *) = nullptr;

static void restore_inError(void *data)
{
	int *poldval = (int *)data;
	inError = *poldval;
	Evaluator::extraDepth(false);
}

/* Do not check constants on error more than this number of times per one
   R process lifetime; if so many errors are generated, the performance
   overhead due to the checks would be too high, and the program is doing
   something strange anyway (i.e. running no-segfault tests). The constant
   checks in GC and session exit (or .Call) do not have such limit. */
static int allowedConstsChecks = 1000;

/* Construct newline terminated error message, write it to global errbuf, and
   possibly display with REprintf. */
NORET static void verrorcall_dflt(SEXP call, const char *format, va_list ap)
{
    if (allowedConstsChecks > 0) {
	allowedConstsChecks--;
	R_checkConstants(TRUE);
    }
    RCNTXT cntxt;
    char *p;
    const char *tr;
    int oldInError;

    if (inError) {
	/* fail-safe handler for recursive errors */
	if(inError == 3) {
	     /* Can REprintf generate an error? If so we should guard for it */
	    REprintf(_("Error during wrapup: "));
	    /* this does NOT try to print the call since that could
	       cause a cascade of error calls */
	    Rvsnprintf_mbcs(errbuf, sizeof(errbuf), format, ap);
	    REprintf("%s\n", errbuf);
	}
	if (R_Warnings != R_NilValue) {
	    R_CollectWarnings = 0;
	    R_Warnings = R_NilValue;
	    REprintf(_("Lost warning messages"));
	    REprintf("\n");
	}
	REprintf(_("Error: no more error handlers available (recursive errors?); invoking 'abort' restart\n"));
	Evaluator::extraDepth(false);
	jump_to_top_ex(FALSE, FALSE, FALSE, FALSE, FALSE);
    }

    /* set up a context to restore inError value on exit */
    cntxt.start(CTXT_CCODE, R_NilValue, R_BaseEnv, R_BaseEnv, R_NilValue, R_NilValue);
    cntxt.setContextEnd(&restore_inError, &oldInError);
    oldInError = inError;
    inError = 1;

    // For use with Rv?snprintf, which truncates at size - 1, hence the + 1
    size_t msg_len = min(BUFSIZE, R_WarnLength) + 1;

    if(call != R_NilValue) {
	char tmp[BUFSIZE], tmp2[BUFSIZE];
	const char *head = _("Error in command:"), *tail = "\n  ";
	SEXP srcloc = R_NilValue; // -Wall
	size_t len = 0;	// indicates if srcloc has been set
	int nprotect = 0, skip = NA_INTEGER;
	SEXP opt = GetOption1(Symbol::obtain("show.error.locations"));
	if (!isNull(opt)) {
	    if (TYPEOF(opt) == STRSXP && length(opt) == 1) {
	    	if (pmatch(ScalarString(mkChar("top")), opt, FALSE)) skip = 0;
	    	else if (pmatch(ScalarString(mkChar("bottom")), opt, FALSE)) skip = -1;
	    } else if (TYPEOF(opt) == LGLSXP)
	    	skip = asLogical(opt) == 1 ? 0 : NA_INTEGER;
	    else
	    	skip = asInteger(opt);
	}

	const char *dcall = CHAR(STRING_ELT(deparse1s(call), 0));
	Rsnprintf_mbcs(tmp2, BUFSIZE,  _("Error in command '%s': "), dcall); 
	if (skip != NA_INTEGER) {
	    PROTECT(srcloc = GetSrcLoc(R_GetCurrentSrcref(skip)));
	    nprotect++;
	    len = strlen(CHAR(STRING_ELT(srcloc, 0)));
	    if (len)
		Rsnprintf_mbcs(tmp2, BUFSIZE,  _("Error in command '%s' (from %s): "), 
			       dcall, CHAR(STRING_ELT(srcloc, 0)));
	}

	Rvsnprintf_mbcs(tmp, max(msg_len - strlen(head), size_t(0)), format, ap);
	if (strlen(tmp2) + strlen(tail) + strlen(tmp) < BUFSIZE) {
	    if(len) Rsnprintf_mbcs(errbuf, BUFSIZE,  
				   _("Error in command '%s' (from %s): "),
				   dcall, CHAR(STRING_ELT(srcloc, 0)));
	    else Rsnprintf_mbcs(errbuf, BUFSIZE,  _("Error in command '%s': "), dcall);
	    if (mbcslocale) {
		int msgline1;
		char *p = strchr(tmp, '\n');
		if (p) {
		    *p = '\0';
		    msgline1 = wd(tmp);
		    *p = '\n';
		} else msgline1 = wd(tmp);
		// gcc 8 warns here
		// 'output may be truncated copying between 0 and 8191 bytes from a string of length 8191'
		// but truncation is intentional.
		if (14 + wd(dcall) + msgline1 > (int) LONGWARN)
		    ERRBUFCAT(tail);
	    } else {
		size_t msgline1 = strlen(tmp);
		char *p = strchr(tmp, '\n');
		if (p) msgline1 = (int)(p - tmp);
		if (14 + strlen(dcall) + msgline1 > (size_t) LONGWARN)
		    ERRBUFCAT(tail);
	    }
	    ERRBUFCAT(tmp);
	} else {
	    Rsnprintf_mbcs(errbuf, BUFSIZE, _("Error: "));
	    ERRBUFCAT(tmp);
	}
	UNPROTECT(nprotect);
    }
    else {
	Rsnprintf_mbcs(errbuf, BUFSIZE, _("Error: "));
	p = errbuf + strlen(errbuf);
	Rvsnprintf_mbcs(p, max(msg_len - strlen(errbuf), size_t(0)), format, ap);
    }
    /* Approximate truncation detection, may produce false positives.  Assumes
       MB_CUR_MAX > 0. Note: approximation is fine, as the string may include
       dots, anyway */
    size_t nc = strlen(errbuf); // > 0, ignoring possibility of failure
    if (nc > BUFSIZE - 1 - (MB_CUR_MAX - 1)) {
	size_t end = min(nc + 1, (BUFSIZE + 1) - 4); // room for "...\n\0"
	for(size_t i = end; i <= BUFSIZE + 1; ++i) errbuf[i - 1] = '\0';
	mbcsTruncateToValid(errbuf);
	ERRBUFCAT("...\n");
    } else {
	p = errbuf + nc - 1;
	if(*p != '\n') {
	    ERRBUFCAT("\n");  // guaranteed to have room for this
	    ++nc;
	}
	if(R_ShowErrorCalls && call != R_NilValue) {  /* assume we want to avoid deparse */
	    tr = R_ConciseTraceback(call, 0);
	    size_t nc_tr = strlen(tr);
	    if (nc_tr) {
		char * call_trans = _("Calls:");
		if (nc_tr + nc + strlen(call_trans) + 2 < BUFSIZE + 1) {
		    ERRBUFCAT(call_trans);
		    ERRBUFCAT(" ");
		    ERRBUFCAT(tr);
		    ERRBUFCAT("\n");
		}
	    }
	}
    }
    if (R_ShowErrorMessages) REprintf("%s", errbuf);

    if( R_ShowErrorMessages && R_CollectWarnings ) {
	PrintWarnings(n_("Additional warning message:", "Additional warning messages:", R_CollectWarnings));
    }

    jump_to_top_ex(TRUE, TRUE, TRUE, TRUE, FALSE);

    /* not reached */
    cntxt.end();
    inError = oldInError;
}

NORET static void errorcall_dflt(SEXP call, const char *format, ...)
{
	va_list ap;

	va_start(ap, format);
	verrorcall_dflt(call, format, ap);
	va_end(ap);
}

NORET void Rf_errorcall(SEXP call, const char *format, ...)
{
	va_list ap;

	if (call == R_CurrentExpression)
		/* behave like error( */
		call = getCurrentCall();

	va_start(ap, format);
	vsignalError(call, format, ap);
	va_end(ap);

	if (R_ErrorHook)
	{
		char buf[BUFSIZE];
		void (*hook)(SEXP, char *) = R_ErrorHook;
		R_ErrorHook = nullptr; /* to avoid recursion */
		va_start(ap, format);
		Rvsnprintf_mbcs(buf, min(BUFSIZE, R_WarnLength), format, ap);
		va_end(ap);
		hook(call, buf);
	}

	va_start(ap, format);
	verrorcall_dflt(call, format, ap);
	va_end(ap);
}

/* Like errorcall, but copies all data for the error message into a buffer
   before doing anything else. */
HIDDEN NORET void R::errorcall_cpy(SEXP call, const char *format, ...)
{
	char buf[BUFSIZE];

	va_list ap;
	va_start(ap, format);
	Rvsnprintf_mbcs(buf, BUFSIZE, format, ap);
	va_end(ap);

	errorcall(call, "%s", buf);
}

// geterrmessage(): Return (the global) 'errbuf' as R string
HIDDEN SEXP do_geterrmessage(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    SEXP res = PROTECT(allocVector(STRSXP, 1));
    SET_STRING_ELT(res, 0, mkChar(errbuf));
    UNPROTECT(1);
    return res;
}

void Rf_error(const char *format, ...)
{
    char buf[BUFSIZE];

    va_list ap;
    va_start(ap, format);
    Rvsnprintf_mbcs(buf, min(BUFSIZE, R_WarnLength), format, ap);
    va_end(ap);
    errorcall(getCurrentCall(), "%s", buf);
}

static void try_jump_to_restart(void)
{
    for (SEXP list = R_RestartStack; list != R_NilValue; list = CDR(list)) {
	SEXP restart = CAR(list);
	if (TYPEOF(restart) == VECSXP && LENGTH(restart) > 1) {
	    SEXP name = VECTOR_ELT(restart, 0);
	    if (TYPEOF(name) == STRSXP && LENGTH(name) == 1) {
		const char *cname = CHAR(STRING_ELT(name, 0));
		if (streql(cname, "browser") ||
		    streql(cname, "tryRestart") ||
		    streql(cname, "abort")) /**** move abort eventually? */
		    invokeRestart(restart, R_NilValue);
	    }
	}
    }
}

/* Unwind the call stack in an orderly fashion */
/* calling the code installed by on.exit along the way */
/* and finally longjmping to the innermost TOPLEVEL context */

static void jump_to_top_ex(Rboolean traceback,
			   Rboolean tryUserHandler,
			   Rboolean processWarnings,
			   Rboolean resetConsole,
			   Rboolean ignoreRestartContexts)
{
    RCNTXT cntxt;
    SEXP s;
    int haveHandler, oldInError;

    /* set up a context to restore inError value on exit */
    cntxt.start(CTXT_CCODE, R_NilValue, R_BaseEnv, R_BaseEnv, R_NilValue, R_NilValue);
    cntxt.setContextEnd(&restore_inError, &oldInError);

    oldInError = inError;

    haveHandler = FALSE;

    /* don't use options("error") when handling a C stack overflow */
    if (R_OldCStackLimit == 0 && tryUserHandler && inError < 3) {
	if (! inError)
	    inError = 1;

	/* now see if options("error") is set */
	s = GetOption1(Symbol::obtain("error"));
	haveHandler = ( s != R_NilValue );
	if (haveHandler) {
	    if( !isLanguage(s) &&  ! isExpression(s) ) { /* shouldn't happen */
		REprintf(_("invalid \"%s\" option"), "error");
		REprintf("\n");
	    } else {
		inError = 3;
		if (isLanguage(s))
		    eval(s, R_GlobalEnv);
		else /* expression */
		    {
			int i, n = LENGTH(s);
			for (i = 0 ; i < n ; i++)
			    eval(XVECTOR_ELT(s, i), R_GlobalEnv);
		    }
		inError = oldInError;
	    }
	}
	inError = oldInError;
    }

    /* print warnings if there are any left to be printed */
    if( processWarnings && R_CollectWarnings )
	PrintWarnings();

    /* reset some stuff--not sure (all) this belongs here */
    if (resetConsole) {
	R_ResetConsole();
	R_FlushConsole();
	R_ClearerrConsole();
	R_ParseError = 0;
	R_ParseErrorFile = nullptr;
	R_ParseErrorMsg[0] = '\0';
    }

    /*
     * Reset graphics state
     */
    GEonExit();

    /* WARNING: If oldInError > 0 ABSOLUTELY NO ALLOCATION can be
       triggered after this point except whatever happens in writing
       the traceback.  The error could be an out of memory error and
       any allocation could result in an infinite-loop condition. All
       you can do is reset things and exit.  */

    /* jump to a browser/try if one is on the stack */
    if (! ignoreRestartContexts)
	try_jump_to_restart();
    /* at this point, i.e. if we have not exited in
       try_jump_to_restart, we are heading for R_ToplevelContext */

    /* only run traceback if we are not going to bail out of a
       non-interactive session */
    if (R_Interactive || haveHandler) {
	/* write traceback if requested, unless we're already doing it
	   or there is an inconsistency between inError and oldInError
	   (which should not happen) */
	if (traceback && inError < 2 && inError == oldInError) {
	    inError = 2;
	    PROTECT(s = R_GetTracebackOnly(0));
	    SET_SYMVALUE(Symbol::obtain(".Traceback"), s);
	    /* should have been defineVar
	       setVar(Symbol::obtain(".Traceback"), s, R_GlobalEnv); */
	    UNPROTECT(1);
	    inError = oldInError;
	}
    }

    R_ToplevelContext->R_jumpctxt(0, nullptr);
}

NORET void jump_to_toplevel()
{
    /* no traceback, no user error option; for now, warnings are
       printed here and console is reset -- eventually these should be
       done after arriving at the jump target.  Now ignores
       try/browser frames--it really is a jump to toplevel */
    jump_to_top_ex(FALSE, FALSE, TRUE, TRUE, TRUE);
}

/* #define DEBUG_GETTEXT 1 */


/* Called from do_gettext() and do_ngettext() */
static char * determine_domain_gettext(SEXP domain_, SEXP rho)
{
    const char *domain = "";
    char *buf; // will be returned

    /* The passed rho is ignored because (n)gettext is called through a
     * helper function in stop.R. And the context is more useful anyhow */
    if(isNull(domain_)) {
	RCNTXT *cptr = nullptr;
	for (cptr = R_GlobalContext; cptr != NULL && cptr->getCallFlag() != CTXT_TOPLEVEL;
	     cptr = cptr->nextContext())
	    if (cptr->getCallFlag() & CTXT_FUNCTION) {
		/* stop() etc have internal call to .makeMessage */
		const char *cfn = CHAR(STRING_ELT(deparse1s(CAR(cptr->getCall())), 0));

		if(streql(cfn, "stop") || streql(cfn, "warning") || streql(cfn, "message"))
		    continue;
		else
		    break;
	    }

	/* First we try to see if sysparent leads us to a namespace, because gettext
	   might have a different environment due to being called from (in?) a closure.
	   If that fails we try cloenv, as the original code did. */
	/* FIXME: should we only do this search when cptr->callflag & CTXT_FUNCTION? */
	SEXP ns = R_NilValue;
	for(size_t attempt = 0; attempt < 2 && isNull(ns); attempt++) {
	    rho = (cptr == nullptr) ?
		R_EmptyEnv :
		attempt == 0 ? cptr->getSysParent() : cptr->workingEnvironment();
	    while(rho != R_EmptyEnv) {
		if (rho == R_GlobalEnv) break;
		else if (R_IsNamespaceEnv(rho)) {
		    ns = R_NamespaceEnvSpec(rho);
		    break;
		}
		rho = ENCLOS(rho);
	    }
	}
	if (!isNull(ns)) {
	    PROTECT(ns);
	    domain = translateChar(STRING_ELT(ns, 0));
	    if (strlen(domain)) {
		size_t len = strlen(domain)+3;
		buf = R_alloc(len, sizeof(char));
		Rsnprintf_mbcs(buf, len, "R-%s", domain);
		UNPROTECT(1); /* ns */
#ifdef DEBUG_GETTEXT
		REprintf("Managed to determine 'domain' from environment as: '%s'\n", buf);
#endif
		return buf;
	    }
	    UNPROTECT(1); /* ns */ 
	}
	return NULL;

    } else if(isString(domain_)) {
	domain = translateChar(STRING_ELT(domain_, 0));
	if (!strlen(domain))
	    return NULL;
	buf = R_alloc(strlen(domain) + 1, sizeof(char));
	strcpy(buf, domain);
	return buf;

    } else if(isLogical(domain_) && LENGTH(domain_) == 1 && LOGICAL(domain_)[0] == NA_LOGICAL)
	return NULL;
    else error(_("invalid '%s' value"), "domain");
}


/* gettext(domain, string) */
HIDDEN SEXP do_gettext(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
#ifdef ENABLE_NLS
    SEXP string = CADR(args);
    int n = LENGTH(string);

    if(isNull(string) || !n) return string;

    if(!isString(string)) error(_("invalid '%s' value"), "string");

    char * domain = determine_domain_gettext(CAR(args), rho);

    if(domain && strlen(domain)) {
	SEXP ans = PROTECT(allocVector(STRSXP, n));
	for(int i = 0; i < n; i++) {
	    int ihead = 0, itail = 0;
	    const char * This = translateChar(STRING_ELT(string, i));
	    char *tmp, *head = nullptr, *tail = nullptr, *p, *tr;

	    R_CheckStack2(strlen(This) + 1);
	    tmp = (char *) alloca(strlen(This) + 1);
	    strcpy(tmp, This);
	    /* strip leading and trailing white spaces and
	       add back after translation */
	    for(p = tmp;
		*p && (*p == ' ' || *p == '\t' || *p == '\n');
		p++, ihead++) ;

	    if(ihead > 0) {
		R_CheckStack2(ihead + 1);
		head = (char *) alloca(ihead + 1);
		Rstrncpy(head, tmp, ihead + 1);
		tmp += ihead;
	    }

	    if(strlen(tmp))
		for(p = tmp+strlen(tmp)-1;
		    p >= tmp && (*p == ' ' || *p == '\t' || *p == '\n');
		    p--, itail++) ;

	    if(itail > 0) {
		R_CheckStack2(itail + 1);
		tail = (char *) alloca(itail + 1);
		strcpy(tail, tmp+strlen(tmp)-itail);
		tmp[strlen(tmp)-itail] = '\0';
	    }

	    if(strlen(tmp)) {
#ifdef DEBUG_GETTEXT
		REprintf(_("translating '%s' in domain '%s'\n"), tmp, domain);
#endif
		tr = dgettext(domain, tmp);
 		R_CheckStack2(        strlen(tr) + ihead + itail + 1);
		tmp = (char *) alloca(strlen(tr) + ihead + itail + 1);
		tmp[0] ='\0';
		if(ihead > 0) strcat(tmp, head);
		strcat(tmp, tr);
		if(itail > 0) strcat(tmp, tail);
		SET_STRING_ELT(ans, i, mkChar(tmp));
	    } else
		SET_STRING_ELT(ans, i, mkChar(This));
	}
	UNPROTECT(1);
	return ans;
    } else
#endif
	// no NLS or no domain :
	return CADR(args);
}

/* ngettext(n, msg1, msg2, domain) */
HIDDEN SEXP do_ngettext(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP msg1 = CADR(args), msg2 = CADDR(args);
    int n = asInteger(CAR(args));

    checkArity(op, args);
    if(n == NA_INTEGER || n < 0) error(_("invalid '%s' argument"), "n");
    if(!isString(msg1) || LENGTH(msg1) != 1)
	error(_("'%s' argument must be a character string"), "msg1");
    if(!isString(msg2) || LENGTH(msg2) != 1)
	error(_("'%s' argument must be a character string"), "msg2");

#ifdef ENABLE_NLS
    SEXP sdom = CADDDR(args);
    char * domain = determine_domain_gettext(sdom, rho);

    if(domain && strlen(domain)) {
	/* libintl seems to malfunction if given a message of "" */
	if(length(STRING_ELT(msg1, 0))) {
	    char *fmt = dngettext(domain,
		      translateChar(STRING_ELT(msg1, 0)),
		      translateChar(STRING_ELT(msg2, 0)),
		      n);
	    return mkString(fmt);
	}
    }
#endif
    return n == 1 ? msg1 : msg2;
}


/* bindtextdomain(domain, dirname) */
HIDDEN SEXP do_bindtextdomain(SEXP call, SEXP op, SEXP args, SEXP rho)
{
#ifdef ENABLE_NLS
    char *res;

    checkArity(op, args);
    if(!isString(CAR(args)) || LENGTH(CAR(args)) != 1)
	error(_("invalid '%s' value"), "domain");
    if(isNull(CADR(args))) {
	res = bindtextdomain(translateChar(STRING_ELT(CAR(args),0)), nullptr);
    } else {
	if(!isString(CADR(args)) || LENGTH(CADR(args)) != 1)
	    error(_("invalid '%s' value"), "dirname");
	res = bindtextdomain(translateChar(STRING_ELT(CAR(args),0)), translateChar(STRING_ELT(CADR(args),0)));
    }
    if(res) return mkString(res);
    /* else this failed */
#endif
    return R_NilValue;
}

static SEXP findCall(void)
{
	for (RCNTXT *cptr = R_GlobalContext->nextContext();
		 cptr && cptr->getCallFlag() != CTXT_TOPLEVEL;
		 cptr = cptr->nextContext())
		if (cptr->getCallFlag() & CTXT_FUNCTION)
			return cptr->getCall();
	return R_NilValue;
}

HIDDEN NORET SEXP do_stop(SEXP call, SEXP op, SEXP args, SEXP rho)
{
/* error(.) : really doesn't return anything; but all do_foo() must be SEXP */
    SEXP c_call;
    checkArity(op, args);

    if(asLogical(CAR(args))) /* find context -> "Error in ..:" */
	c_call = findCall();
    else
	c_call = R_NilValue;

    args = CDR(args);

    if (CAR(args) != R_NilValue) { /* message */
      SETCAR(args, coerceVector(CAR(args), STRSXP));
      if(!isValidString(CAR(args)))
	  errorcall(c_call, _(" [invalid string in '%s']"), "stop(.)");
      errorcall(c_call, "%s", translateChar(STRING_ELT(CAR(args), 0)));
    }
    else
      errorcall(c_call, "");
    /* never called: */
}

HIDDEN SEXP do_warning(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP c_call;
    checkArity(op, args);

    if(asLogical(CAR(args))) /* find context -> "... in: ..:" */
	c_call = findCall();
    else
	c_call = R_NilValue;

    args = CDR(args);
    if(asLogical(CAR(args))) { /* immediate = TRUE */
	immediateWarning = 1;
    } else
	immediateWarning = 0;
    args = CDR(args);
    if(asLogical(CAR(args))) { /* noBreak = TRUE */
	noBreakWarning = 1;
    } else
	noBreakWarning = 0;
    args = CDR(args);
    if (CAR(args) != R_NilValue) {
	SETCAR(args, coerceVector(CAR(args), STRSXP));
	if(!isValidString(CAR(args)))
	    warningcall(c_call, _(" [invalid string in '%s']"), "warning(.)");
	else
	    warningcall(c_call, "%s", translateChar(STRING_ELT(CAR(args), 0)));
    }
    else
	warningcall(c_call, "");
    immediateWarning = 0; /* reset to internal calls */
    noBreakWarning = 0;

    return CAR(args);
}

/* Error recovery for incorrect argument count error. */
HIDDEN NORET void WrongArgCount(const char *s)
{
    error(_("incorrect number of arguments passed to '%s' function"), s);
}


NORET void UNIMPLEMENTED(const char *s)
{
    error(_("unimplemented feature in '%s' function"), s);
}

/* ERROR_.. codes in Errormsg.h */
static struct {
    const R_ERROR code;
    const char* const format;
}
const ErrorDB[] = {
    { ERROR_NUMARGS,		N_("invalid number of arguments")	},
    { ERROR_ARGTYPE,		N_("invalid argument type")		},

    { ERROR_TSVEC_MISMATCH,	N_("time-series/vector length mismatch")},
    { ERROR_INCOMPAT_ARGS,	N_("incompatible arguments")		},

    { ERROR_UNIMPLEMENTED,	N_("unimplemented feature in '%s' function")	},
    { ERROR_UNKNOWN,		N_("unknown error (report this!)")	}
};

static struct {
    const R_WARNING code;
    const char* const format;
}
WarningDB[] = {
    { WARNING_coerce_NA,	N_("NA values introduced by coercion")	},
    { WARNING_coerce_INACC,	N_("inaccurate integer conversion in coercion")},
    { WARNING_coerce_IMAG,	N_("imaginary parts discarded in coercion") },

    { WARNING_UNKNOWN,		N_("unknown warning (report this!)")	},
};


HIDDEN NORET void R::ErrorMessage(SEXP call, int which_error, ...)
{
    int i;
    char buf[BUFSIZE];
    va_list ap;

    i = 0;
    while(ErrorDB[i].code != ERROR_UNKNOWN) {
	if (ErrorDB[i].code == which_error)
	    break;
	i++;
    }

    va_start(ap, which_error);
    Rvsnprintf_mbcs(buf, BUFSIZE, _(ErrorDB[i].format), ap);
    va_end(ap);
    errorcall(call, "%s", buf);
}

HIDDEN void R::WarningMessage(SEXP call, int which_warn, ...)
{
    int i;
    char buf[BUFSIZE];
    va_list ap;

    i = 0;
    while(WarningDB[i].code != WARNING_UNKNOWN) {
	if (WarningDB[i].code == which_warn)
	    break;
	i++;
    }

    va_start(ap, which_warn);
    Rvsnprintf_mbcs(buf, BUFSIZE, _(WarningDB[i].format), ap);
    va_end(ap);
    warningcall(call, "%s", buf);
}

static void R_SetErrmessage(const char *s)
{
    Rstrncpy(errbuf, s, sizeof(errbuf) - 1);
}

static void R_PrintDeferredWarnings(void)
{
    if( R_ShowErrorMessages && R_CollectWarnings ) {
	PrintWarnings(n_("Additional warning message:", "Additional warning messages:", R_CollectWarnings));
    }
}
/*
 * Return the traceback without deparsing the calls
 */
HIDDEN
SEXP R::R_GetTracebackOnly(int skip)
{
    int nback = 0, ns = skip;
    SEXP s, t;

    for (RCNTXT *c = R_GlobalContext;
	 c != nullptr && c->getCallFlag() != CTXT_TOPLEVEL;
	 c = c->nextContext())
	if (c->getCallFlag() & (CTXT_FUNCTION | CTXT_BUILTIN) ) {
	    if (ns > 0)
		ns--;
	    else
		nback++;
	}

    PROTECT(s = allocList(nback));
    t = s;
    for (RCNTXT *c = R_GlobalContext ;
	 c != nullptr && c->getCallFlag() != CTXT_TOPLEVEL;
	 c = c->nextContext())
	if (c->getCallFlag() & (CTXT_FUNCTION | CTXT_BUILTIN) ) {
	    if (skip > 0)
		skip--;
	    else {
		SETCAR(t, duplicate(c->getCall()));
		if (c->getSrcRef() && !isNull(c->getSrcRef())) {
		    SEXP sref;
		    if (c->getSrcRef() == R_InBCInterpreter)
			sref = R_findBCInterpreterSrcref(c);
		    else
			sref = c->getSrcRef();
		    setAttrib(CAR(t), R_SrcrefSymbol, duplicate(sref));
		}
		t = CDR(t);
	    }
	}
    UNPROTECT(1);
    return s;
}
/*
 * Return the traceback with calls deparsed
 */
HIDDEN
SEXP R::R_GetTraceback(int skip)
{
    int nback = 0;
    SEXP s, t, u, v;
    s = PROTECT(R_GetTracebackOnly(skip));
    for(t = s; t != R_NilValue; t = CDR(t)) nback++;
    u = v = PROTECT(allocList(nback));

    for(t = s; t != R_NilValue; t = CDR(t), v=CDR(v)) {
	SEXP sref = getAttrib(CAR(t), R_SrcrefSymbol);
	SEXP dep = PROTECT(deparse1m(CAR(t), FALSE, DEFAULTDEPARSE));
	if (!isNull(sref))
	    setAttrib(dep, R_SrcrefSymbol, duplicate(sref));
	SETCAR(v, dep);
	UNPROTECT(1);
    }
    UNPROTECT(2);
    return u;
}

HIDDEN SEXP do_traceback(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int skip;

    checkArity(op, args);
    skip = asInteger(CAR(args));

    if (skip == NA_INTEGER || skip < 0 )
    	error(_("invalid '%s' value"), "skip");

    return R_GetTraceback(skip);
}

static const char *R_ConciseTraceback(SEXP call, int skip)
{
    static char buf[560];

    size_t nl;
    int ncalls = 0;
    bool too_many = false;
    const char *top = "" /* -Wall */;

    buf[0] = '\0';
    for (RCNTXT *c = R_GlobalContext;
	 c != nullptr && c->getCallFlag() != CTXT_TOPLEVEL;
	 c = c->nextContext())
	if (c->getCallFlag() & (CTXT_FUNCTION | CTXT_BUILTIN) ) {
	    if (skip > 0)
		skip--;
	    else {
		SEXP fun = CAR(c->getCall());
		const char *this_ = (TYPEOF(fun) == SYMSXP) ?
		    CHAR(PRINTNAME(fun)) : "<Anonymous>";
		if(streql(this_, "stop") ||
		   streql(this_, "warning") ||
		   streql(this_, "suppressWarnings") ||
		   streql(this_, ".signalSimpleWarning")) {
		    buf[0] =  '\0'; ncalls = 0; too_many = false;
		} else {
		    ncalls++;
		    if(too_many) {
			top = this_;
		    } else if(strlen(buf) > (size_t) R_NShowCalls) {
			memmove(buf+4, buf, strlen(buf)+1);
			memcpy(buf, "... ", 4);
			too_many = true;
			top = this_;
		    } else if(strlen(buf)) {
			nl = strlen(this_);
			memmove(buf+nl+4, buf, strlen(buf)+1);
			memcpy(buf, this_, strlen(this_));
			memcpy(buf+nl, " -> ", 4);
		    } else
			memcpy(buf, this_, strlen(this_)+1);
		}
	    }
	}
    if(too_many && (nl = strlen(top)) < 50) {
	memmove(buf+nl+1, buf, strlen(buf)+1);
	memcpy(buf, top, strlen(top));
	memcpy(buf+nl, " ", 1);
    }
    /* don't add Calls if it adds no extra information */
    /* However: do we want to include the call in the list if it is a
       primitive? */
    if (ncalls == 1 && TYPEOF(call) == LANGSXP) {
	SEXP fun = CAR(call);
	const char *this_ = (TYPEOF(fun) == SYMSXP) ?
	    CHAR(PRINTNAME(fun)) : "<Anonymous>";
	if(streql(buf, this_)) return "";
    }
    return buf;
}

namespace
{
	struct HandlerEntry : RObject
	{
		GCEdge<String> m_class;
		GCEdge<Environment> m_parent_environment;
		GCEdge<> m_handler;
		GCEdge<Environment> m_environment;
		GCEdge<ListVector> m_result;
		bool m_calling;

		HandlerEntry(String *the_class, Environment *parent_env,
					 RObject *handler, Environment *environment,
					 ListVector *result, bool calling)
			: m_calling(calling)
		{
			m_class = the_class;
			m_parent_environment = parent_env;
			m_handler = handler;
			m_environment = environment;
			m_result = result;
		}

		static const char *staticTypeName()
		{
			return "(error handler entry)";
		}

		// Virtual function of GCNode:
		void visitReferents(const_visitor *v) const override;
	};

	void HandlerEntry::visitReferents(const_visitor *v) const
	{
		RObject::visitReferents(v);
		if (m_class)
			m_class->conductVisitor(v);
		if (m_parent_environment)
			m_parent_environment->conductVisitor(v);
		if (m_handler)
			m_handler->conductVisitor(v);
		if (m_environment)
			m_environment->conductVisitor(v);
		if (m_result)
			m_result->conductVisitor(v);
	}
} // namespace

static SEXP mkHandlerEntry(SEXP klass, SEXP parentenv, SEXP handler, SEXP rho,
						   SEXP result, int calling)
{
	HandlerEntry *entry = GCNode::expose(new HandlerEntry(SEXP_downcast<String *>(klass),
														  SEXP_downcast<Environment *>(parentenv), handler,
														  SEXP_downcast<Environment *>(rho),
														  SEXP_downcast<ListVector *>(result),
														  (calling != 0)));
	return entry;
}

namespace
{
	/**** rename these??*/
	bool IS_CALLING_ENTRY(SEXP e)
	{
		return SEXP_downcast<HandlerEntry *>(e)->m_calling;
	}

	String *ENTRY_CLASS(SEXP e)
	{
		return SEXP_downcast<HandlerEntry *>(e)->m_class;
	}

	// Environment *ENTRY_CALLING_ENVIR(SEXP e)
	// {
	// 	return SEXP_downcast<HandlerEntry *>(e)->m_parent_environment;
	// }

	RObject *ENTRY_HANDLER(SEXP e)
	{
		return SEXP_downcast<HandlerEntry *>(e)->m_handler;
	}

	Environment *ENTRY_TARGET_ENVIR(SEXP e)
	{
		return SEXP_downcast<HandlerEntry *>(e)->m_environment;
	}

	ListVector *ENTRY_RETURN_RESULT(SEXP e)
	{
		return SEXP_downcast<HandlerEntry *>(e)->m_result;
	}

	void CLEAR_ENTRY_CALLING_ENVIR(SEXP e)
	{
		SEXP_downcast<HandlerEntry *>(e)->m_parent_environment = nullptr;
	}

	void CLEAR_ENTRY_TARGET_ENVIR(SEXP e)
	{
		SEXP_downcast<HandlerEntry *>(e)->m_environment = nullptr;
	}
} // namespace

HIDDEN SEXP R_UnwindHandlerStack(SEXP target)
{
    SEXP hs;

    /* check that the target is in the current stack */
    for (hs = R_HandlerStack; hs != target && hs != R_NilValue; hs = CDR(hs))
	if (hs == target)
	    break;
    if (hs != target)
	return target; /* restoring a saved stack */

    for (hs = R_HandlerStack; hs != target; hs = CDR(hs)) {
	/* pop top handler; may not be needed */
	R_HandlerStack = CDR(hs);

	/* clear the two environments to reduce reference counts */
	CLEAR_ENTRY_CALLING_ENVIR(CAR(hs));
	CLEAR_ENTRY_TARGET_ENVIR(CAR(hs));
    }
    return target;
}

constexpr R_xlen_t RESULT_SIZE = 4;

static GCRoot<> R_HandlerResultToken(nullptr);

HIDDEN void R_FixupExitingHandlerResult(SEXP result)
{
    /* The internal error handling mechanism stores the error message
       in 'errbuf'.  If an on.exit() action is processed while jumping
       to an exiting handler for such an error, then RCNTXT::endcontext()
       calls R_FixupExitingHandlerResult to save the error message
       currently in the buffer before processing the on.exit
       action. This is in case an error occurs in the on.exit action
       that over-writes the buffer. The allocation should occur in a
       more favorable stack context than before the jump. The
       R_HandlerResultToken is used to make sure the result being
       modified is associated with jumping to an exiting handler. */
    if (result &&
	TYPEOF(result) == VECSXP &&
	XLENGTH(result) == RESULT_SIZE &&
	VECTOR_ELT(result, 0) == R_NilValue &&
	VECTOR_ELT(result, RESULT_SIZE - 1) == R_HandlerResultToken) {
	SET_VECTOR_ELT(result, 0, mkString(errbuf));
    }
}

HIDDEN SEXP do_addCondHands(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP classes, handlers, parentenv, target, oldstack, newstack, result;
    int calling, i, n;
    PROTECT_INDEX osi;

    if (R_HandlerResultToken == nullptr) {
	R_HandlerResultToken = allocVector(VECSXP, 1);
	R_PreserveObject(R_HandlerResultToken);
    }

    checkArity(op, args);

    classes = CAR(args); args = CDR(args);
    handlers = CAR(args); args = CDR(args);
    parentenv = CAR(args); args = CDR(args);
    target = CAR(args); args = CDR(args);
    calling = asLogical(CAR(args));

    if (classes == R_NilValue || handlers == R_NilValue)
	return R_HandlerStack;

    if (TYPEOF(classes) != STRSXP || TYPEOF(handlers) != VECSXP ||
	LENGTH(classes) != LENGTH(handlers))
	error(_("bad handler data"));

    n = LENGTH(handlers);
    oldstack = R_HandlerStack;

    PROTECT(result = allocVector(VECSXP, RESULT_SIZE));
    SET_VECTOR_ELT(result, RESULT_SIZE - 1, R_HandlerResultToken);
    PROTECT_WITH_INDEX(newstack = oldstack, &osi);

    for (i = n - 1; i >= 0; i--) {
	SEXP klass = STRING_ELT(classes, i);
	SEXP handler = VECTOR_ELT(handlers, i);
	SEXP entry = mkHandlerEntry(klass, parentenv, handler, target, result,
				    calling);
	REPROTECT(newstack = CONS(entry, newstack), osi);
    }

    R_HandlerStack = newstack;
    UNPROTECT(2);

    return oldstack;
}

HIDDEN SEXP do_resetCondHands(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    R_HandlerStack = CAR(args);
    return R_NilValue;
}

static SEXP findSimpleErrorHandler(void)
{
    for (SEXP list = R_HandlerStack; list != R_NilValue; list = CDR(list)) {
	SEXP entry = CAR(list);
	if (streql(CHAR(ENTRY_CLASS(entry)), "simpleError") ||
	    streql(CHAR(ENTRY_CLASS(entry)), "error") ||
	    streql(CHAR(ENTRY_CLASS(entry)), "condition"))
	    return list;
    }
    return R_NilValue;
}

static void vsignalWarning(SEXP call, const char *format, va_list ap)
{
    char buf[BUFSIZE];
    SEXP hooksym, hcall, qcall, qfun;

    hooksym = Symbol::obtain(".signalSimpleWarning");
    if (SYMVALUE(hooksym) != R_UnboundValue &&
	SYMVALUE(R_QuoteSymbol) != R_UnboundValue) {
	qfun = lang3(R_DoubleColonSymbol, R_BaseSymbol, R_QuoteSymbol);
	PROTECT(qfun);
	PROTECT(qcall = GCNode::expose(new Expression(qfun, {call})));
	PROTECT(hcall = CONS(qcall, R_NilValue));
	Rvsnprintf_mbcs(buf, BUFSIZE - 1, format, ap);
	hcall = CONS(mkString(buf), hcall);
	PROTECT(hcall = LCONS(hooksym, hcall));
	evalKeepVis(hcall, R_GlobalEnv);
	UNPROTECT(4);
    }
    else vwarningcall_dflt(call, format, ap);
}

NORET static void gotoExitingHandler(SEXP cond, SEXP call, SEXP entry)
{
	SEXP rho = ENTRY_TARGET_ENVIR(entry);
	SEXP result = ENTRY_RETURN_RESULT(entry);
	SET_VECTOR_ELT(result, 0, cond);
	SET_VECTOR_ELT(result, 1, call);
	SET_VECTOR_ELT(result, 2, ENTRY_HANDLER(entry));
	findcontext(CTXT_FUNCTION, rho, result);
}

static void vsignalError(SEXP call, const char *format, va_list ap)
{
    char localbuf[BUFSIZE];
    SEXP list, oldstack;

    PROTECT(oldstack = R_HandlerStack);
    Rvsnprintf_mbcs(localbuf, BUFSIZE - 1, format, ap);
    while ((list = findSimpleErrorHandler()) != R_NilValue) {
	char *buf = errbuf;
	SEXP entry = CAR(list);
	R_HandlerStack = CDR(list);
	Rstrncpy(buf, localbuf, BUFSIZE);
	/*	Rvsnprintf(buf, BUFSIZE - 1, format, ap);*/
	if (IS_CALLING_ENTRY(entry)) {
	    if (ENTRY_HANDLER(entry) == R_RestartToken) {
		UNPROTECT(1); /* oldstack */
		return; /* go to default error handling; do not reset stack */
	    } else {
		/* if we are in the process of handling a C stack
		   overflow, treat all calling handlers as failed */
		if (R_OldCStackLimit)
		    break;
		Expression *hcall;
		Expression *qcall;
		/* protect oldstack here, not outside loop, so handler
		   stack gets unwound in case error is protect stack
		   overflow */
		PROTECT(oldstack);
		static Symbol *hooksym = Symbol::obtain(".handleSimpleError");
		Expression *qfun = GCNode::expose(new Expression(R_DoubleColonSymbol, {R_BaseSymbol, R_QuoteSymbol}));
		PROTECT(qfun);
		PROTECT(qcall = GCNode::expose(new Expression(qfun, {call})));
		PROTECT(hcall = GCNode::expose(new Expression(hooksym, {ENTRY_HANDLER(entry), Rf_mkString(buf), qcall})));
		hcall->evaluate(Environment::global());
		UNPROTECT(4);
	    }
	}
	else gotoExitingHandler(R_NilValue, call, entry);
    }
    R_HandlerStack = oldstack;
    UNPROTECT(1); /* oldstack */
}

static SEXP findConditionHandler(SEXP cond)
{
	SEXP classes = getAttrib(cond, R_ClassSymbol);

	if (TYPEOF(classes) != STRSXP)
		return R_NilValue;

	/**** need some changes here to allow conditions to be S4 classes */
	for (SEXP list = R_HandlerStack; list != R_NilValue; list = CDR(list))
	{
		SEXP entry = CAR(list);
		for (int i = 0; i < LENGTH(classes); i++)
			if (streql(CHAR(ENTRY_CLASS(entry)), CHAR(STRING_ELT(classes, i))))
				return list;
	}
	return R_NilValue;
}

HIDDEN SEXP do_signalCondition(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP list, cond, msg, ecall, oldstack;

    checkArity(op, args);

    cond = CAR(args);
    msg = CADR(args);
    ecall = CADDR(args);

    PROTECT(oldstack = R_HandlerStack);
    while ((list = findConditionHandler(cond)) != R_NilValue) {
	SEXP entry = CAR(list);
	R_HandlerStack = CDR(list);
	if (IS_CALLING_ENTRY(entry)) {
	    SEXP h = ENTRY_HANDLER(entry);
	    if (h == R_RestartToken) {
		const char *msgstr = nullptr;
		if (TYPEOF(msg) == STRSXP && LENGTH(msg) > 0)
		    msgstr = translateChar(STRING_ELT(msg, 0));
		else error(_("error message is not a character string"));
		errorcall_dflt(ecall, "%s", msgstr);
	    }
	    else {
		Expression *hcall = GCNode::expose(new Expression(h, {cond}));
		hcall->evaluate(Environment::global());
	    }
	}
	else gotoExitingHandler(cond, ecall, entry);
    }
    R_HandlerStack = oldstack;
    UNPROTECT(1);
    return R_NilValue;
}

static SEXP findInterruptHandler(void)
{
	for (SEXP list = R_HandlerStack; list != R_NilValue; list = CDR(list))
	{
		SEXP entry = CAR(list);
		if (streql(CHAR(ENTRY_CLASS(entry)), "interrupt") ||
			streql(CHAR(ENTRY_CLASS(entry)), "condition"))
			return list;
	}
	return R_NilValue;
}

static SEXP getInterruptCondition(void)
{
    /**** FIXME: should probably pre-allocate this */
    SEXP cond, klass;
    PROTECT(cond = allocVector(VECSXP, 0));
    PROTECT(klass = allocVector(STRSXP, 2));
    SET_STRING_ELT(klass, 0, mkChar("interrupt"));
    SET_STRING_ELT(klass, 1, mkChar("condition"));
    classgets(cond, klass);
    UNPROTECT(2);
    return cond;
}

static void signalInterrupt(void)
{
    SEXP list, cond, oldstack;

    PROTECT(oldstack = R_HandlerStack);
    while ((list = findInterruptHandler()) != R_NilValue) {
	SEXP entry = CAR(list);
	R_HandlerStack = CDR(list);
	PROTECT(cond = getInterruptCondition());
	if (IS_CALLING_ENTRY(entry)) {
	    SEXP h = ENTRY_HANDLER(entry);
	    SEXP hcall = GCNode::expose(new Expression(h, {cond}));
	    PROTECT(hcall);
	    evalKeepVis(hcall, R_GlobalEnv);
	    UNPROTECT(1);
	}
	else gotoExitingHandler(cond, R_NilValue, entry);
	UNPROTECT(1);
    }
    R_HandlerStack = oldstack;
    UNPROTECT(1);

    SEXP h = GetOption1(Symbol::obtain("interrupt"));
    if (h != R_NilValue) {
	SEXP call = PROTECT(LCONS(h, R_NilValue));
	evalKeepVis(call, R_GlobalEnv);
	UNPROTECT(1);
    }
}

HIDDEN void RCNTXT::R_InsertRestartHandlers(RCNTXT *cptr, const char *cname)
{
    SEXP klass, rho, entry, name;

    if ((cptr->getHandlerStack() != R_HandlerStack ||
	 cptr->getRestartStack() != R_RestartStack)) {
	if (cptr->isRestartBitSet())
	    return;
	else
	    error(_("handler or restart stack mismatch in old restart"));
    }

    /**** need more here to keep recursive errors in browser? */
    rho = cptr->workingEnvironment();
    PROTECT(klass = mkChar("error"));
    entry = mkHandlerEntry(klass, rho, R_RestartToken, rho, R_NilValue, TRUE);
    R_HandlerStack = CONS(entry, R_HandlerStack);
    UNPROTECT(1);
    PROTECT(name = mkString(cname));
    PROTECT(entry = allocVector(VECSXP, 2));
    SET_VECTOR_ELT(entry, 0, name);
    SET_VECTOR_ELT(entry, 1, R_MakeExternalPtr(cptr, R_NilValue, R_NilValue));
    setAttrib(entry, R_ClassSymbol, mkString("restart"));
    R_RestartStack = CONS(entry, R_RestartStack);
    UNPROTECT(2);
}

HIDDEN SEXP do_dfltWarn(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);

    if (TYPEOF(CAR(args)) != STRSXP || LENGTH(CAR(args)) != 1)
	error(_("bad error message"));
    const char *msg = translateChar(STRING_ELT(CAR(args), 0));
    SEXP ecall = CADR(args);

    warningcall_dflt(ecall, "%s", msg);
    return R_NilValue;
}

HIDDEN NORET SEXP do_dfltStop(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);

    if (TYPEOF(CAR(args)) != STRSXP || LENGTH(CAR(args)) != 1)
	error(_("bad error message"));
    const char *msg = translateChar(STRING_ELT(CAR(args), 0));
    SEXP ecall = CADR(args);

    errorcall_dflt(ecall, "%s", msg);
}


/*
 * Restart Handling
 */

HIDDEN SEXP do_getRestart(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int i;
    SEXP list;
    checkArity(op, args);
    i = asInteger(CAR(args));
    for (list = R_RestartStack;
	 list != R_NilValue && i > 1;
	 list = CDR(list), i--);
    if (list != R_NilValue)
	return CAR(list);
    else if (i == 1) {
	/**** need to pre-allocate */
	SEXP name, entry;
	PROTECT(name = mkString("abort"));
	PROTECT(entry = allocVector(VECSXP, 2));
	SET_VECTOR_ELT(entry, 0, name);
	SET_VECTOR_ELT(entry, 1, R_NilValue);
	setAttrib(entry, R_ClassSymbol, mkString("restart"));
	UNPROTECT(2);
	return entry;
    }
    else return R_NilValue;
}

/* very minimal error checking --just enough to avoid a segfault */
static inline void CHECK_RESTART(SEXP r)
{
	if (TYPEOF(r) != VECSXP || LENGTH(r) < 2)
		error(_("bad restart"));
}

HIDDEN SEXP do_addRestart(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	checkArity(op, args);
	CHECK_RESTART(CAR(args));
	R_RestartStack = CONS(CAR(args), R_RestartStack);
	return R_NilValue;
}

static inline SEXP RESTART_EXIT(SEXP r) { return VECTOR_ELT(r, 1); }

NORET static void invokeRestart(SEXP r, SEXP arglist)
{
    SEXP exit = RESTART_EXIT(r);

    if (exit == R_NilValue) {
	R_RestartStack = R_NilValue;
	jump_to_toplevel();
    }
    else {
	for (; R_RestartStack != R_NilValue;
	     R_RestartStack = CDR(R_RestartStack))
	    if (exit == RESTART_EXIT(CAR(R_RestartStack))) {
		R_RestartStack = CDR(R_RestartStack);
		if (TYPEOF(exit) == EXTPTRSXP) {
		    RCNTXT *c = static_cast<RCNTXT *>(R_ExternalPtrAddr(exit));
		    RCNTXT::R_JumpToContext(c, CTXT_RESTART, R_RestartToken);
		}
		else findcontext(CTXT_FUNCTION, exit, arglist);
	    }
	error(_("restart is not on stack"));
    }
}

HIDDEN NORET SEXP do_invokeRestart(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	checkArity(op, args);
	CHECK_RESTART(CAR(args));
	invokeRestart(CAR(args), CADR(args));
}

HIDDEN SEXP do_addTryHandlers(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	checkArity(op, args);
	if (R_GlobalContext == R_ToplevelContext ||
		!(R_GlobalContext->getCallFlag() & CTXT_FUNCTION))
		error(_("not in a try context"));
	R_GlobalContext->setRestartBitOn();
	RCNTXT::R_InsertRestartHandlers(R_GlobalContext, "tryRestart");
	return R_NilValue;
}

HIDDEN SEXP do_seterrmessage(SEXP call, SEXP op, SEXP args, SEXP env)
{
	SEXP msg;

	checkArity(op, args);
	msg = CAR(args);
	if (!isString(msg) || LENGTH(msg) != 1)
		error(_("error message is not a character string"));
	R_SetErrmessage(CHAR(STRING_ELT(msg, 0)));
	return R_NilValue;
}

HIDDEN SEXP do_printDeferredWarnings(SEXP call, SEXP op, SEXP args, SEXP env)
{
	checkArity(op, args);
	R_PrintDeferredWarnings();
	return R_NilValue;
}

HIDDEN SEXP do_interruptsSuspended(SEXP call, SEXP op, SEXP args, SEXP env)
{
	int orig_value = R_interrupts_suspended;
	if (args != R_NilValue)
		R_interrupts_suspended = (Rboolean)asLogical(CAR(args));
	return ScalarLogical(orig_value);
}

HIDDEN void R_BadValueInRCode(SEXP value, SEXP call, SEXP rho, const char *rawmsg,
                  const char *errmsg, const char *warnmsg,
                  const char *varname, Rboolean warnByDefault)
{
    /* disable GC so that use of this temporary checking code does not
       introduce new PROTECT errors e.g. in asLogical() use */
	GCManager::GCInhibitor no_gc;
    R_CHECK_THREAD;

    int nprotect = 0;
    char *check = getenv(varname);
    const void *vmax = vmaxget();
    Rboolean err = (Rboolean) (check && StringTrue(check));
    if (!err && check && StringFalse(check))
	check = nullptr; /* disabled */
    Rboolean abort = FALSE; /* R_Suicide/abort */
    Rboolean verbose = FALSE;
    Rboolean warn = FALSE;
    const char *pkgname = 0;
    if (!err && check) {
	const char *pprefix = "package:";
	const char *aprefix = "abort";
	const char *vprefix = "verbose";
	const char *wprefix = "warn";
	const char *cpname = "_R_CHECK_PACKAGE_NAME_";
	size_t lpprefix = strlen(pprefix);
	size_t laprefix = strlen(aprefix);
	size_t lvprefix = strlen(vprefix);
	size_t lwprefix = strlen(wprefix);
	size_t lcpname = strlen(cpname);
	Rboolean ignore = FALSE;

	SEXP spkg = R_NilValue;
	for(; rho != R_EmptyEnv; rho = ENCLOS(rho))
	    if (R_IsPackageEnv(rho)) {
		PROTECT(spkg = R_PackageEnvName(rho));
		nprotect++;
		break;
	    } else if (R_IsNamespaceEnv(rho)) {
		PROTECT(spkg = R_NamespaceEnvSpec(rho));
		nprotect++;
		break;
	    }
	if (spkg != R_NilValue)
	    pkgname = translateChar(STRING_ELT(spkg, 0));

	while (check[0] != '\0') {
	    if (streqln(pprefix, check, lpprefix)) {
		/* check starts with "package:" */
		check += lpprefix;
		size_t arglen = 0;
		const char *sep = strchr(check, ',');
		if (sep)
		    arglen = sep - check;
		else
		    arglen = strlen(check);
		ignore = TRUE;
		if (pkgname) {
		    if (streqln(check, pkgname, arglen) && strlen(pkgname) == arglen)
			ignore = FALSE;
		    if (streqln(check, cpname, arglen) && lcpname == arglen) {
			/* package name specified in _R_CHECK_PACKAGE_NAME */
			const char *envpname = getenv(cpname);
			if (envpname && streql(envpname, pkgname))
			    ignore = FALSE;
		    }
		}
		check += arglen;
	    } else if (streqln(aprefix, check, laprefix)) {
		/* check starts with "abort" */
		check += laprefix;
		abort = TRUE;
	    } else if (streqln(vprefix, check, lvprefix)) {
		/* check starts with "verbose" */
		check += lvprefix;
		verbose = TRUE;
	    } else if (streqln(wprefix, check, lwprefix)) {
		/* check starts with "warn" */
		check += lwprefix;
		warn = TRUE;
	    } else if (check[0] == ',') {
		check++;
	    } else
		error(_("invalid value of %s"), varname);
	}
	if (ignore) {
	    abort = FALSE; /* err is FALSE */
	    verbose = FALSE;
	    warn = FALSE;
	} else if (!abort && !warn)
	    err = TRUE;
    }
    if (verbose) {
	int oldout = R_OutputCon;
	R_OutputCon = 2;
	int olderr = R_ErrorCon;
	R_ErrorCon = 2;
	REprintf(" ----------- FAILURE REPORT -------------- \n");
	REprintf(" --- failure: %s ---\n", rawmsg);
	REprintf(" --- srcref --- \n");
	SrcrefPrompt("", R_getCurrentSrcref());
	REprintf("\n");
	if (pkgname) {
	    REprintf(" --- package (from environment) --- \n");
	    REprintf("%s\n", pkgname);
	}
	REprintf(" --- call from context --- \n");
	PrintValue(R_GlobalContext->getCall());
	REprintf(" --- call from argument --- \n");
	PrintValue(call);
	REprintf(" --- R stacktrace ---\n");
	printwhere();
	REprintf(" --- value of length: %d type: %s ---\n",
		 length(value), type2char(TYPEOF(value)));
	PrintValue(value);
	REprintf(" --- function from context --- \n");
	if (R_GlobalContext->getCallFun() != nullptr &&
	    TYPEOF(R_GlobalContext->getCallFun()) == CLOSXP)
	    PrintValue(R_GlobalContext->getCallFun());
	REprintf(" --- function search by body ---\n");
	if (R_GlobalContext->getCallFun() != nullptr &&
	    TYPEOF(R_GlobalContext->getCallFun()) == CLOSXP)
	    findFunctionForBody(R_ClosureExpr(R_GlobalContext->getCallFun()));
	REprintf(" ----------- END OF FAILURE REPORT -------------- \n");
	R_OutputCon = oldout;
	R_ErrorCon = olderr;
    }
    if (abort)
	R_Suicide(rawmsg);
    else if (err)
	errorcall(call, errmsg);
    else if (warn || warnByDefault)
	warningcall(call, warnmsg);
    vmaxset(vmax);
    UNPROTECT(nprotect);
}


/* These functions are to be used in error messages, and available for others to use in the API
   GetCurrentSrcref returns the first non-NULL srcref after skipping skip of them.  If it
   doesn't find one it returns NULL. */

SEXP R_GetCurrentSrcref(int skip)
{
    RCNTXT *c = R_GlobalContext;
    SEXP srcref = R_Srcref;
    if (skip < 0) { /* to count up from the bottom, we need to count them all first */
	while (c) {
	    if (srcref && srcref != R_NilValue)
		skip++;
	    srcref = c->getSrcRef();
	    c = c->nextContext();
	};
	if (skip < 0) return R_NilValue; /* not enough there */
	c = R_GlobalContext;
	srcref = R_Srcref;
    }
    while (c && (skip || !srcref || srcref == R_NilValue)) {
	if (srcref && srcref != R_NilValue)
	    skip--;
	srcref = c->getSrcRef();
	c = c->nextContext();
    }
    if (skip || !srcref)
	srcref = R_NilValue;
    return srcref;
}

/* Return the filename corresponding to a srcref, or "" if none is found */

SEXP R_GetSrcFilename(SEXP srcref)
{
	SEXP srcfile = getAttrib(srcref, R_SrcfileSymbol);
	if (TYPEOF(srcfile) != ENVSXP)
		return ScalarString(mkChar(""));
	srcfile = findVar(Symbol::obtain("filename"), srcfile);
	if (TYPEOF(srcfile) != STRSXP)
		return ScalarString(mkChar(""));
	return srcfile;
}

/*
 * C level tryCatch support
 */

/* There are two functions:

       R_TryCatchError    handles error conditions;

       R_TryCatch         can handle any condition type and allows a
                          finalize action.
*/

SEXP R_tryCatchError(SEXP (*body)(void *), void *bdata,
		     SEXP (*handler)(SEXP, void *), void *hdata)
{
    SEXP val;
    SEXP cond = Rf_mkString("error");

    PROTECT(cond);
    val = R_tryCatch(body, bdata, cond, handler, hdata, nullptr, nullptr);
    UNPROTECT(1);
    return val;
}

/* This implementation uses R's tryCatch via calls from C to R to
   invoke R's tryCatch, and then back to C to invoke the C
   body/handler functions via a .Internal helper. This makes the
   implementation fairly simple but not fast. If performance becomes
   an issue we can look into a pure C implementation. LT */

struct tryCatchData_t {
    SEXP (*body)(void *);
    void *bdata;
    SEXP (*handler)(SEXP, void *);
    void *hdata;
    void (*finally)(void *);
    void *fdata;
    Rboolean suspended;
};

static SEXP default_tryCatch_handler(SEXP cond, void *data)
{
    return R_NilValue;
}

static void default_tryCatch_finally(void *data) { }

static GCRoot<> trycatch_callback(nullptr);
static const char* trycatch_callback_source =
    "function(addr, classes, fin) {\n"
    "    handler <- function(cond)\n"
    "        .Internal(C_tryCatchHelper(addr, 1L, cond))\n"
    "    handlers <- rep_len(alist(handler), length(classes))\n"
    "    names(handlers) <- classes\n"
    "    if (fin)\n"
    "	     handlers <- c(handlers,\n"
    "            alist(finally = .Internal(C_tryCatchHelper(addr, 2L))))\n"
    "    args <- c(alist(.Internal(C_tryCatchHelper(addr, 0L))), handlers)\n"
    "    do.call('tryCatch', args)\n"
    "}";

SEXP R_tryCatch(SEXP (*body)(void *), void *bdata,
		SEXP conds,
		SEXP (*handler)(SEXP, void *), void *hdata,
		void (*finally)(void *), void *fdata)
{
    if (body == nullptr) error(_("body function is required"));

    if (trycatch_callback == nullptr) {
	trycatch_callback = R_ParseEvalString(trycatch_callback_source,
					      R_BaseNamespace);
	R_PreserveObject(trycatch_callback);
    }

    tryCatchData_t tcd = {
	.body = body,
	.bdata = bdata,
	.handler = handler ? handler : default_tryCatch_handler,
	.hdata = hdata,
	.finally = finally ? finally : default_tryCatch_finally,
	.fdata = fdata,
	.suspended = R_interrupts_suspended
    };

    /* Interrupts are suspended while in the infrastructure R code and
       enabled, if they were on entry to R_tryCatch, while calling the
       body function in do_tryCatchHelper */

    R_interrupts_suspended = TRUE;

    if (conds == nullptr) conds = allocVector(STRSXP, 0);
    PROTECT(conds);
    LogicalVector *fin = finally ? R_TrueValue : R_FalseValue;
    SEXP tcdptr = R_MakeExternalPtr(&tcd, R_NilValue, R_NilValue);
    SEXP expr = lang4(trycatch_callback, tcdptr, conds, fin);
    PROTECT(expr);
    SEXP val = evalKeepVis(expr, R_GlobalEnv);
    UNPROTECT(2); /* conds, expr */
    R_interrupts_suspended = tcd.suspended;
    return val;
}

SEXP do_tryCatchHelper(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP eptr = CAR(args);
    SEXP sw = CADR(args);
    SEXP cond = CADDR(args);

    if (TYPEOF(eptr) != EXTPTRSXP)
	error(_("this is not an external pointer"));

    tryCatchData_t *ptcd = (tryCatchData_t*) R_ExternalPtrAddr(CAR(args));

    switch (asInteger(sw)) {
    case 0:
	if (ptcd->suspended)
	    /* Interrupts were suspended for the call to R_TryCatch,
	       so leave them that way */
	    return ptcd->body(ptcd->bdata);
	else {
	    /* Interrupts were not suspended for the call to
	       R_TryCatch, but were suspended for the call through
	       R. So enable them for the body and suspend again on the
	       way out. */
	    R_interrupts_suspended = FALSE;
	    SEXP val = ptcd->body(ptcd->bdata);
	    R_interrupts_suspended = TRUE;
	    return val;
	}
    case 1:
	if (ptcd->handler)
	    return ptcd->handler(cond, ptcd->hdata);
	else return R_NilValue;
    case 2:
	if (ptcd->finally)
	    ptcd->finally(ptcd->fdata);
	return R_NilValue;
    default: return R_NilValue; /* should not happen */
    }
}


/* R_withCallingErrorHandler establishes a calling handler for
   conditions inheriting from class 'error'. The handler is
   established without calling back into the R implementation. This
   should therefore be much more efficient than the current R_tryCatch
   implementation. */

SEXP R_withCallingErrorHandler(SEXP (*body)(void *), void *bdata,
			       SEXP (*handler)(SEXP, void *), void *hdata)
{
    /* This defines the lambda expression for th handler. The `addr`
       variable will be defined in the closure environment and contain
       an external pointer to the callback data. */
    static const char* wceh_callback_source =
	"function(cond) .Internal(C_tryCatchHelper(addr, 1L, cond))";

    static GCRoot<> wceh_callback(nullptr);
    static GCRoot<CachedString> wceh_class(nullptr);
    static GCRoot<Symbol> addr_sym(nullptr);

    if (body == nullptr) error(_("must supply a body function"));

    if (wceh_callback == nullptr) {
	wceh_callback = R_ParseEvalString(wceh_callback_source,
					  R_BaseNamespace);
	R_PreserveObject(wceh_callback);
	wceh_class = CachedString::obtain("error");
	R_PreserveObject(wceh_class);
	addr_sym = Symbol::obtain("addr");
    }

    /* record the C-level handler information */
	tryCatchData_t tcd = {
		.body = nullptr,
		.bdata = nullptr,
		.handler = handler ? handler : default_tryCatch_handler,
		.hdata = hdata,
		.finally = nullptr,
		.fdata = nullptr,
		.suspended = FALSE};

	SEXP tcdptr = R_MakeExternalPtr(&tcd, R_NilValue, R_NilValue);

    /* create the R handler function closure */
    SEXP env = CONS(tcdptr, R_NilValue);
    SET_TAG(env, addr_sym);
    env = NewEnvironment(R_NilValue, env, R_BaseNamespace);
    PROTECT(env);
    SEXP h = duplicate(wceh_callback);
    SET_CLOENV(h, env);
    UNPROTECT(1); /* env */

    /* push the handler on the handler stack */
    SEXP oldstack = R_HandlerStack;
    PROTECT(oldstack);
    PROTECT(h);
    SEXP entry = mkHandlerEntry(wceh_class, R_GlobalEnv, h, R_NilValue,
				R_NilValue, /* OK for a calling handler */
				TRUE);
    R_HandlerStack = CONS(entry, R_HandlerStack);
    UNPROTECT(1); /* h */

    SEXP val = body(bdata);

    /* restore the handler stack */
    R_HandlerStack = oldstack;
    UNPROTECT(1); /* oldstack */

    return val;
}

HIDDEN SEXP do_addGlobHands(SEXP call, SEXP op,SEXP args, SEXP rho)
{
    SEXP oldstk = R_ToplevelContext->getHandlerStack();

    R_HandlerStack = R_NilValue;
    do_addCondHands(call, op, args, rho);

    /* This is needed to handle intermediate contexts that would
       restore the handler stack to the value when begincontext was
       called. This function should only be called in a context where
       there are no handlers on the stack. */
#ifdef DODO
    for (RCNTXT *cptr = R_GlobalContext;
	 cptr != R_ToplevelContext;
	 cptr = cptr->nextContext())
	if (cptr->getHandlerStack() == R_NilValue)
	    cptr->setHandlerStack(R_HandlerStack);
#endif
    for (RCNTXT *cptr = R_GlobalContext;
	 cptr != R_ToplevelContext;
	 cptr = cptr->nextContext())
	if (cptr->getHandlerStack() == oldstk)
	    cptr->setHandlerStack(R_HandlerStack);
	else error(_("should not be called with handlers on the stack"));

    R_ToplevelContext->setHandlerStack(R_HandlerStack);
    return R_NilValue;
}
