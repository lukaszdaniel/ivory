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

/** @file RContext.h
 *
 * Class RContext and related functions.
 */

#ifndef RCONTEXT_H
#define RCONTEXT_H

#include <R_ext/Visibility.h>
#include <CXXR/Promise.hpp>
#include <CXXR/RTypes.hpp>
#include <BCNodeStack.h>

#ifdef _WIN32
#include <psignal.h>
#else
#include <csignal>
#include <csetjmp>
#endif

#ifdef HAVE_POSIX_SETJMP
#define SIGJMP_BUF sigjmp_buf
#define SIGSETJMP(x, s) sigsetjmp(x, s)
#define SIGLONGJMP(x, i) siglongjmp(x, i)
#define JMP_BUF sigjmp_buf
#else
#define SIGJMP_BUF jmp_buf
#define SIGSETJMP(x, s) setjmp(x)
#define SIGLONGJMP(x, i) longjmp(x, i)
#define JMP_BUF jmp_buf
#endif

/* The Various Context Types.

 * In general the type is a bitwise OR of the values below.
 * Note that CTXT_LOOP is already the or of CTXT_NEXT and CTXT_BREAK.
 * Only functions should have the third bit turned on;
 * this allows us to move up the context stack easily
 * with either RETURN's or GENERIC's or RESTART's.
 * If you add a new context type for functions make sure
 *   CTXT_NEWTYPE & CTXT_FUNCTION > 0
 */
enum CTXT
{
    CTXT_TOPLEVEL = 0,
    CTXT_NEXT = 1,
    CTXT_BREAK = 2,
    CTXT_LOOP = 3, /* break OR next target */
    CTXT_FUNCTION = 4,
    CTXT_CCODE = 8,
    CTXT_RETURN = 12,
    CTXT_BROWSER = 16,
    CTXT_GENERIC = 20,
    CTXT_RESTART = 32,
    CTXT_BUILTIN = 64, /* used in profiling */
    CTXT_UNWIND = 128
};

/*    1 2 4 8 ...
TOP   0 0 0 0 0 0  = 0
NEX   1 0 0 0 0 0  = 1
BRE   0 1 0 0 0 0  = 2
LOO   1 1 0 0 0 0  = 3
FUN   0 0 1 0 0 0  = 4
CCO   0 0 0 1 0 0  = 8
BRO   0 0 0 0 1 0  = 16
RET   0 0 1 1 0 0  = 12
GEN   0 0 1 0 1 0  = 20
RES   0 0 0 0 0 0 1 = 32
BUI   0 0 0 0 0 0 0 1 = 64
*/

namespace CXXR
{
    /* Evaluation Context Structure */
    class RContext
    {
    private:
        RContext *m_nextcontext; /* The next context up the chain */
        int m_callflag;          /* The context "type" */
        JMP_BUF m_cjmpbuf;       /* C stack and register information */
        size_t m_cstacktop;      /* Top of the pointer protection stack */
        int m_evaldepth;         /* evaluation depth at inception */
        SEXP m_promargs;         /* Promises supplied to closure */
        SEXP m_callfun;          /* The closure called */
        SEXP m_sysparent;        /* environment the closure was called from */
        SEXP m_call;             /* The call that effected this context*/
        SEXP m_cloenv;           /* The environment */
        SEXP m_conexit;          /* Interpreted "on.exit" code */
        void (*m_cend)(void *);  /* C "on.exit" thunk */
        void *m_cenddata;        /* data for C "on.exit" thunk */
        void *m_vmax;            /* top of R_alloc stack */
        int m_intsusp;           /* interrupts are suspended */
        bool m_gcenabled;        /* R_GCEnabled value */
        bool m_bcintactive;      /* R_BCIntActive value */
        SEXP m_bcbody;           /* R_BCbody value */
        void *m_bcpc;            /* R_BCpc value */
        SEXP m_handlerstack;     /* condition handler stack */
        SEXP m_restartstack;     /* stack of available restarts */
        RPRSTACK *m_prstack;     /* stack of pending promises */
        R_bcstack_t *m_nodestack;
        R_bcstack_t *m_bcprottop;
        SEXP m_srcref;          /* The source line in effect */
        bool m_browserfinish;   /* should browser finish this context without
                                   stopping */
        SEXP m_returnValue;     /* only set during on.exit calls */
        RContext *m_jumptarget; /* target for a continuing jump */
        int m_jumpmask;         /* associated JMPException argument */
        SEXP getCallWithSrcref();
        RContext *first_jump_target(int mask);

    public:
        RContext() : m_nextcontext(nullptr), m_callflag(0), m_cjmpbuf(), m_cstacktop(0), m_evaldepth(0), m_promargs(nullptr),
                     m_callfun(nullptr), m_sysparent(nullptr), m_call(nullptr), m_cloenv(nullptr), m_conexit(nullptr), m_cend(nullptr), m_cenddata(nullptr),
                     m_vmax(nullptr), m_intsusp(0), m_gcenabled(false), m_bcintactive(false), m_bcbody(nullptr), m_bcpc(nullptr), m_handlerstack(nullptr),
                     m_restartstack(nullptr), m_prstack(nullptr), m_nodestack(nullptr), m_bcprottop(nullptr), m_srcref(nullptr), m_browserfinish(false),
                     m_returnValue(nullptr), m_jumptarget(nullptr), m_jumpmask(0){};
        ~RContext(){};
        SEXP getHandlerStack() const { return m_handlerstack; }
        void setHandlerStack(SEXP handler) { m_handlerstack = handler; }
        SEXP onExit() const { return m_conexit; }
        void setOnExit(SEXP x) { m_conexit = x; }
        SEXP workingEnvironment() const { return m_cloenv; }
        void setWorkingEnvironment(SEXP x) { m_cloenv = x; }
        RContext *nextContext() const { return m_nextcontext; }
        void setNextContext(RContext *ctxt) { m_nextcontext = ctxt; }
        SEXP getReturnValue() const { return m_returnValue; }
        void setReturnValue(SEXP rv) { m_returnValue = rv; }
        void *getContextEndData() const { return m_cenddata; };
        void setContextEndData(void *data = nullptr) { m_cenddata = data; }
        void setContextEnd(void (*cendf)(void *) = nullptr) { m_cend = cendf; }
        void setContextEnd(void (*cendf)(void *), void *data)
        {
            m_cend = cendf;
            m_cenddata = data;
        }
        auto getContextEnd() { return (m_cend); }
        int &getCallFlag() { return m_callflag; }
        int getCallFlag() const { return m_callflag; }
        void setCallFlag(int cflag) { m_callflag = cflag; }
        SEXP getSysParent() const { return m_sysparent; }
        void setSysParent(SEXP sp) { m_sysparent = sp; }
        SEXP getRestartStack() const { return m_restartstack; }
        void setRestartStack(SEXP rs) { m_restartstack = rs; }
        int getCStackTop() const { return m_cstacktop; }
        void setCStackTop(int stacktop) { m_cstacktop = stacktop; }
        bool getGCEnabled() const { return m_gcenabled; }
        void setGCEnabled(bool enabled) { m_gcenabled = enabled; }
        bool getBCIntactive() const { return m_bcintactive; }
        void setBCIntactive(bool active) { m_bcintactive = active; }
        void *getBCPC() const { return m_bcpc; }
        void setBCPC(void *bc) { m_bcpc = bc; }
        SEXP getBCBody() const { return m_bcbody; }
        void setBCBody(SEXP body) { m_bcbody = body; }
        int getEvalDepth() const { return m_evaldepth; }
        void setEvalDepth(int depth) { m_evaldepth = depth; }
        int getIntSusp() const { return m_intsusp; }
        void setIntSusp(int susp) { m_intsusp = susp; }
        void *getVMax() const { return m_vmax; }
        void setVMax(void *vm) { m_vmax = vm; }
        RPRSTACK *getPrStack() const { return m_prstack; }
        void setPrStack(RPRSTACK *rpr) { m_prstack = rpr; }
        RContext *getJumpTarget() const { return m_jumptarget; }
        void setJumpTarget(RContext *target) { m_jumptarget = target; }
        int getJumpMask() const { return m_jumpmask; }
        void setJumpMask(int mask) { m_jumpmask = mask; }
        R_bcstack_t *getNodeStack() const { return m_nodestack; }
        void setNodeStack(R_bcstack_t *stack) { m_nodestack = stack; }
        SEXP getSrcRef() const { return m_srcref; }
        void setSrcRef(SEXP src) { m_srcref = src; }
        R_bcstack_t *getBCProtTop() const { return m_bcprottop; }
        void setBCProtTop(R_bcstack_t *stack) { m_bcprottop = stack; }
        auto getCJmpBuf() { return m_cjmpbuf; }
        SEXP getCallFun() const { return m_callfun; }
        void setCallFun(SEXP cfun) { m_callfun = cfun; }
        SEXP getPromiseArgs() const { return m_promargs; }
        void setPromiseArgs(SEXP pargs) { m_promargs = pargs; }
        SEXP getCall() const { return m_call; }
        void setCall(SEXP call) { m_call = call; }
        bool getBrowserFinish() const { return m_browserfinish; }
        void setBrowserFinish(bool finish) { m_browserfinish = finish; }
        bool isRestartBitSet() const { return (m_callflag & CTXT_RESTART); }
        void setRestartBitOn() { m_callflag |= CTXT_RESTART; }
        void setRestartBitOff() { m_callflag &= ~CTXT_RESTART; }
        int R_sysparent(int n);
        SEXP R_sysfunction(int n);
        SEXP R_syscall(int n);
        void R_restore_globals();
        static void R_run_onexits(RContext *cptr = nullptr);
        NORET void R_jumpctxt(int mask, SEXP val);
        SEXP R_sysframe(int n);
        int Rf_framedepth();
        void start(int, SEXP, SEXP, SEXP, SEXP, SEXP);
        static void begincontext(RContext &, int, SEXP, SEXP, SEXP, SEXP, SEXP);
        static void begincontext(RContext *, int, SEXP, SEXP, SEXP, SEXP, SEXP);
        static SEXP dynamicfindVar(SEXP, RContext *);
        void end();
        static void endcontext(RContext &);
        static void endcontext(RContext *);
        static void R_InsertRestartHandlers(RContext *, const char *);
        NORET static void R_JumpToContext(RContext *, int, SEXP);
        static RContext *R_findExecContext(RContext *, SEXP);
        static RContext *R_findParentContext(RContext *, int);
        static RContext *findProfContext(RContext *cptr);
    };

    extern RContext R_Toplevel;         /* Storage for the toplevel context */
    extern RContext *R_ToplevelContext; /* The toplevel context */
    extern RContext *R_GlobalContext;   /* The global context */
    extern RContext *R_SessionContext;  /* The session toplevel context */
    extern RContext *R_ExitContext;     /* The active context for on.exit processing */
} // namespace CXXR

using RCNTXT = CXXR::RContext;

SEXP R_findBCInterpreterSrcref(RCNTXT *cptr);
void R_FixupExitingHandlerResult(SEXP result); /* defined in error.cpp */
SEXP R_UnwindHandlerStack(SEXP target);        /* defined in error.cpp */

#endif // RCONTEXT_H
