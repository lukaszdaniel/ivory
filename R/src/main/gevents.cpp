/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2004-2007  The R Foundation
 *  Copyright (C) 2013-2017  The R Core Team
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


 *  This is an implementation of modal event handling in R graphics
 *  by Duncan Murdoch
 */

/** @file gevents.cpp
 *
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#define R_NO_REMAP

#include <CXXR/Expression.hpp>
#include <CXXR/IntVector.hpp>
#include <CXXR/RealVector.hpp>
#include <CXXR/Symbol.hpp>
#include <Localization.h>
#include <R.h>
#include <Defn.h>
#include <Rmath.h>
#include <R_ext/GraphicsEngine.h>
#include <R_ext/Print.h>

using namespace R;
using namespace CXXR;

static const char *const mouseHandlers[] =
	{"onMouseDown", "onMouseUp", "onMouseMove"};

static const char *const keybdHandler = "onKeybd";

static const char *const idleHandler = "onIdle";

static void checkHandler(const char *const name, SEXP eventEnv)
{
	SEXP handler = findVar(install(name), eventEnv);
	if (TYPEOF(handler) == CLOSXP)
		warning(_("'%s' events not supported in this device"), name);
}

extern "C"
SEXP do_setGraphicsEventEnv(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP eventEnv;
    int devnum;
    pGEDevDesc gdd;
    pDevDesc dd;

    checkArity(op, args);

    devnum = INTEGER(CAR(args))[0] - 1;
    if(devnum < 1 || devnum >= R_MaxDevices)
	error(_("invalid graphical device number"));

    gdd = GEgetDevice(devnum);
    if(!gdd) errorcall(call, _("invalid device"));
    dd = gdd->dev;
    args=CDR(args);

    eventEnv = CAR(args);
    if (TYPEOF(eventEnv) != ENVSXP)
	error(_("internal error"));

    if (!dd->canGenMouseDown &&
	!dd->canGenMouseUp &&
	!dd->canGenMouseMove &&
	!dd->canGenKeybd &&
	!dd->canGenIdle)
	error(_("this graphics device does not support event handling"));

    if (!dd->canGenMouseDown) checkHandler(mouseHandlers[0], eventEnv);
    if (!dd->canGenMouseUp)   checkHandler(mouseHandlers[1], eventEnv);
    if (!dd->canGenMouseMove) checkHandler(mouseHandlers[2], eventEnv);
    if (!dd->canGenKeybd)     checkHandler(keybdHandler, eventEnv);
    if (!dd->canGenIdle)      checkHandler(idleHandler, eventEnv);

    dd->eventEnv = eventEnv;

    return nullptr;
}

extern "C"
SEXP do_getGraphicsEventEnv(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int devnum;
    pGEDevDesc gdd;

    checkArity(op, args);

    devnum = INTEGER(CAR(args))[0];
    if(devnum == NA_INTEGER)
	error(_("invalid graphical device number"));
    devnum--;
    if(devnum < 1 || devnum >= R_MaxDevices)
	error(_("invalid graphical device number"));

    gdd = GEgetDevice(devnum);
    if(!gdd) errorcall(call, _("invalid device"));
    return gdd->dev->eventEnv;
}

/* helper function to check if there is at least one open graphics device listening for events. Returns TRUE if so, FALSE if no listening devices are found */

bool haveListeningDev()
{
    bool ret = false;
    pDevDesc dd;
    pGEDevDesc gd;
    if(!NoDevices())
    {
	for(int i = 1; i < NumDevices(); i++)
	{
	    if ((gd = GEgetDevice(i)) && (dd = gd->dev)
		 && dd->gettingEvent){
		ret = true;
		break;
	    }
	}
    }
    return ret;
}

extern "C"
SEXP do_getGraphicsEvent(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP result = R_NilValue, prompt;
    pDevDesc dd;
    pGEDevDesc gd;
    int i, count=0, devNum;

    checkArity(op, args);

    prompt = CAR(args);
    if (!isString(prompt) || !length(prompt)) error(_("invalid prompt"));

    /* NB:  cleanup of event handlers must be done by driver in onExit handler */

    if (!NoDevices()) {
	/* Initialize all devices */
	i = 1;
	devNum = curDevice();
	while (i++ < NumDevices()) {
	    if ((gd = GEgetDevice(devNum)) && (dd = gd->dev)) {
		if (dd->gettingEvent)
		    error(_("recursive use of 'getGraphicsEvent()' function is not supported"));
		if (dd->eventEnv != R_NilValue) {
		    if (dd->eventHelper) dd->eventHelper(dd, 1);
		    dd->gettingEvent = TRUE;
		    defineVar(Symbol::obtain("result"), R_NilValue, dd->eventEnv);
		    count++;
		}
	    }
	    devNum = nextDevice(devNum);
	}
	if (!count)
	    error(_("no graphics event handlers set"));

	Rprintf("%s\n", CHAR(asChar(prompt)));
	R_FlushConsole();

	/* Poll them */
	while (result == R_NilValue) {
	    /* make sure we still have at least one device listening for events, and throw an error if not*/
	    if(!haveListeningDev())
		return R_NilValue;
#ifdef _WIN32
	    R_WaitEvent();
#endif
	    R_ProcessEvents();
	    R_CheckUserInterrupt();
	    i = 1;
	    devNum = curDevice();
	    while (i++ < NumDevices()) {
		if ((gd = GEgetDevice(devNum)) && (dd = gd->dev)) {
		    if (dd->eventEnv != R_NilValue) {
			if (dd->eventHelper) dd->eventHelper(dd, 2);
			result = findVar(Symbol::obtain("result"), dd->eventEnv);
			if (result != R_NilValue && result != R_UnboundValue) {
			    break;
			}
		    }
		}
		devNum = nextDevice(devNum);
	    }
	}
	/* clean up */
	i = 1;
	devNum = curDevice();
	while (i++ < NumDevices()) {
	    if ((gd = GEgetDevice(devNum)) && (dd = gd->dev)) {
		if (dd->eventEnv != R_NilValue) {
		    if (dd->eventHelper) dd->eventHelper(dd, 0);
		    dd->gettingEvent = FALSE;
		}
	    }
	    devNum = nextDevice(devNum);
	}

    }
    return result;
}

/* used in devWindows.cpp and cairoDevice */
void Rf_doMouseEvent(pDevDesc dd, R_MouseEvent event,
		  int buttons, double x, double y)
{
    int i;
    SEXP handler, bvec, sx, sy, temp, result;

    dd->gettingEvent = FALSE; /* avoid recursive calls */

    PROTECT(handler = findVar(install(mouseHandlers[event]), dd->eventEnv));
    if (TYPEOF(handler) == PROMSXP) {
	handler = eval(handler, dd->eventEnv);
	UNPROTECT(1); /* handler */
	PROTECT(handler);
    }
    if (TYPEOF(handler) == CLOSXP) {
	SEXP s_which = Symbol::obtain("which");
	defineVar(s_which, ScalarInteger(ndevNumber(dd)+1), dd->eventEnv);
	// Be portable: see PR#15793
	int len = ((buttons & leftButton) != 0)
	  + ((buttons & middleButton) != 0)
	  + ((buttons & rightButton) != 0);

	PROTECT(bvec = allocVector(INTSXP, len));
	i = 0;
	if (buttons & leftButton) INTEGER(bvec)[i++] = 0;
	if (buttons & middleButton) INTEGER(bvec)[i++] = 1;
	if (buttons & rightButton) INTEGER(bvec)[i++] = 2;

	PROTECT(sx = ScalarReal( (x - dd->left) / (dd->right - dd->left) ));
	PROTECT(sy = ScalarReal((y - dd->bottom) / (dd->top - dd->bottom) ));
	PROTECT(temp = lang4(handler, bvec, sx, sy));
	PROTECT(result = eval(temp, dd->eventEnv));
	defineVar(Symbol::obtain("result"), result, dd->eventEnv);
	UNPROTECT(5);
	R_FlushConsole();
    }
    UNPROTECT(1); /* handler */
    dd->gettingEvent = TRUE;
    return;
}

static const char *const keynames[] =
	{"Left", "Up", "Right", "Down",
	 "F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9", "F10", "F11", "F12",
	 "PgUp", "PgDn", "End", "Home", "Ins", "Del"};

/* used in devWindows.cpp and cairoDevice */
void Rf_doKeybd(pDevDesc dd, R_KeyName rkey,
	     const char *keyname)
{
    SEXP handler, skey, temp, result;

    dd->gettingEvent = FALSE; /* avoid recursive calls */

    PROTECT(handler = findVar(install(keybdHandler), dd->eventEnv));
    if (TYPEOF(handler) == PROMSXP) {
	handler = eval(handler, dd->eventEnv);
	UNPROTECT(1); /* handler */
	PROTECT(handler);
    }

    if (TYPEOF(handler) == CLOSXP) {
	SEXP s_which = Symbol::obtain("which");
	defineVar(s_which, ScalarInteger(ndevNumber(dd)+1), dd->eventEnv);
	PROTECT(skey = mkString(keyname ? keyname : keynames[rkey]));
	PROTECT(temp = lang2(handler, skey));
	PROTECT(result = eval(temp, dd->eventEnv));
	defineVar(Symbol::obtain("result"), result, dd->eventEnv);
	UNPROTECT(3);
	R_FlushConsole();
    }
    UNPROTECT(1); /* handler */
    dd->gettingEvent = TRUE;
    return;
}

/* Copy-modified from doKeybd -- Frederick Eaton 12 Jun 2016 */
/* This "doIdle" (executing new "onIdle" hook) should enable users of
   getGraphicsEvent to do background processing, e.g. reading from a
   stream and updating a plot, in-between handling of keyboard and
   mouse events.
 */
void Rf_doIdle(pDevDesc dd)
{
    SEXP handler, temp, result;

    dd->gettingEvent = FALSE; /* avoid recursive calls */

    PROTECT(handler = findVar(install(idleHandler), dd->eventEnv));
    if (TYPEOF(handler) == PROMSXP) {
	handler = eval(handler, dd->eventEnv);
	UNPROTECT(1); /* handler */
	PROTECT(handler);
    }

    if (TYPEOF(handler) == CLOSXP) {
	SEXP s_which = Symbol::obtain("which");
	defineVar(s_which, ScalarInteger(ndevNumber(dd)+1), dd->eventEnv);
	PROTECT(temp = lang1(handler));
	PROTECT(result = eval(temp, dd->eventEnv));
	defineVar(Symbol::obtain("result"), result, dd->eventEnv);
	UNPROTECT(2);
	R_FlushConsole();
    }
    UNPROTECT(1); /* handler */
    dd->gettingEvent = TRUE;
    return;
}

Rboolean Rf_doesIdle(pDevDesc dd)
{
	SEXP handler = findVar(install(idleHandler), dd->eventEnv);
	return (Rboolean)((handler != R_UnboundValue) &&
					  (handler != R_NilValue));
}
