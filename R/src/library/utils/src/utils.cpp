/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2012-2021   The R Core Team.
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

#include <cstring>
#include <CXXR/GCStackRoot.hpp>
#include <CXXR/RObject.hpp>
#include <CXXR/String.hpp>
#include <CXXR/StringVector.hpp>
#include <R.h>
#include <Rinternals.h>

#include "utils.h"
#include "localization.h"

using namespace R;
using namespace CXXR;

/* from src/main/eval.cpp */
SEXP do_Rprof(SEXP args);

SEXP Rprof(SEXP args)
{
    return do_Rprof(CDR(args));
}

/* from src/main/memory.cpp */
SEXP do_Rprofmem(SEXP args);

SEXP Rprofmem(SEXP args)
{
    return do_Rprofmem(CDR(args));
}

/* from src/main/dounzip.cpp */
SEXP Runzip(SEXP args);

SEXP unzip(SEXP args)
{
    return Runzip(CDR(args));
}

#include <wctype.h>
#include "rlocale.h" // may remap iswctype, wctype

/* Declarations from Defn.h */
int ENC_KNOWN(SEXP x);
extern Rboolean utf8locale;
namespace R
{
    const wchar_t *wtransChar(SEXP x);
}

#if defined(USE_RI18N_FNS) || (defined(HAVE_ISWCTYPE) && defined(HAVE_WCTYPE))
SEXP charClass(SEXP x, SEXP scl)
{
    if (!isString(scl) || length(scl) != 1)
	error(_("argument 'class' must be a character string"));
    const char *cl = CHAR(STRING_ELT(scl, 0));
    wctype_t wcl = wctype(cl);
    if(wcl == 0)
	error(_("character class \"%s\" is invalid"), cl);

    R_xlen_t n;
    GCStackRoot<> xx(x);
    GCStackRoot<> ans;
    if (isString(x)) {
	if (XLENGTH(x) != 1)
	    error(_("argument 'x' must be a length-1 character vector"));
	SEXP sx = STRING_ELT(x, 0);
	if (!(IS_ASCII(sx) || IS_UTF8(sx) || (utf8locale && !ENC_KNOWN(sx))))
	    error(_("argument 'x' must be UTF-8 encoded (including ASCII)"));
	const wchar_t *wx = R::wtransChar(sx);
	n = wcslen(wx);
	ans = allocVector(LGLSXP, n);
	int *pans = LOGICAL(ans);
	for (R_xlen_t i = 0; i < n; i++) {
	    // casting in case wchar_t is signed short: avoid sign extension
	    int this_ = (int)(unsigned int)wx[i];
	    pans[i] = iswctype(this_, wcl);
	}
    } else {
	x = coerceVector(x, INTSXP);
	n = XLENGTH(x);
	const int* px = INTEGER(x);
	ans = allocVector(LGLSXP, n);
	int *pans = LOGICAL(ans);
	for (R_xlen_t i = 0; i < n; i++) {
	    int this_ = px[i];
	    if (this_ < 0) pans[i] = NA_LOGICAL;
	    else pans[i] = iswctype(this_, wcl);
	}
    }
    return ans;
}
#else
SEXP charClass(SEXP x, SEXP scl)
{
    error(_("'charClass()' function is not available on this platform"));
    return R_NilValue;
}
#endif


#include <lzma.h>

SEXP crc64(SEXP in)
{
    uint64_t crc = 0;
    char ans[17];
    if (!isString(in)) error(_("'%s' argument must be a character string"), "input");
    const char *str = CHAR(STRING_ELT(in, 0));

    /* Seems this is really 64-bit only on 64-bit platforms */
    crc = lzma_crc64((uint8_t *)str, strlen(str), crc);
    snprintf(ans, 17, "%lx", (long unsigned int) crc);
    return mkString(ans);
}

// As from 3.3.0 this means on Unix.
#if defined(HAVE_ARPA_INET_H)
#include <netdb.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

SEXP nsl(SEXP hostname)
{
    SEXP ans = R_NilValue;
    const char *name; char ip[] = "xxx.xxx.xxx.xxx";
    struct hostent *hp;

    if (!isString(hostname) || length(hostname) != 1)
	error(_("'hostname' argument must be a character vector of length 1"));
    name = translateChar(STRING_ELT(hostname, 0));

    hp = gethostbyname(name);

    if (hp == nullptr) {		/* cannot resolve the address */
	warning(_("'nsl()' was unable to resolve host '%s'"), name);
    } else {
	if (hp->h_addrtype == AF_INET) {
	    struct in_addr in;
	    memcpy(&in.s_addr, *(hp->h_addr_list), sizeof (in.s_addr));
	    strcpy(ip, inet_ntoa(in));
	} else {
	    warning(_("unknown format returned by 'gethostbyname()'"));
	}
	ans = mkString(ip);
    }
    return ans;
}
#else
SEXP nsl(SEXP hostname)
{
    warning(_("'nsl()' is not supported on this platform"));
    return R_NilValue;
}
#endif
