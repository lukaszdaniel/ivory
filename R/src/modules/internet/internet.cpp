/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2000-2020   The R Core Team.
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

/* <UTF8> the only interpretation of char is ASCII */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

// for contexts
#define R_USE_SIGNALS 1
#include <Localization.h>

#include <Defn.h>
#include <Fileio.h>
#include <Rconnections.h>
#include <R_ext/R-ftp-http.h>
#include <cerrno>
#include <R_ext/Print.h>
#include <R_ext/Visibility.h>

static void *in_R_HTTPOpen(const char *url, const char *agent, const char *headers,
						   int cacheOK);
static int in_R_HTTPRead(void *ctx, char *dest, int len);
static void in_R_HTTPClose(void *ctx);

static void *in_R_FTPOpen(const char *url);
static int in_R_FTPRead(void *ctx, char *dest, int len);
static void in_R_FTPClose(void *ctx);

SEXP in_do_curlVersion(SEXP call, SEXP op, SEXP args, SEXP rho);
SEXP in_do_curlGetHeaders(SEXP call, SEXP op, SEXP args, SEXP rho);
SEXP in_do_curlDownload(SEXP call, SEXP op, SEXP args, SEXP rho);
Rconnection in_newCurlUrl(const char *description, const char * const mode, SEXP headers, int type);

#ifdef _WIN32
static void *in_R_HTTPOpen2(const char *url, const char *agent, const char *headers, int cacheOK);
static int in_R_HTTPRead2(void *ctx, char *dest, int len);
static void in_R_HTTPClose2(void *ctx);
static void *in_R_FTPOpen2(const char *url);

#define Ri_HTTPOpen(url, agent, headers, cacheOK) \
	(meth ? in_R_HTTPOpen2(url, agent, headers, cacheOK) : in_R_HTTPOpen(url, agent, headers, cacheOK));

#define Ri_HTTPRead(ctx, dest, len) \
	(meth ? in_R_HTTPRead2(ctx, dest, len) : in_R_HTTPRead(ctx, dest, len))

#define Ri_HTTPClose(ctx)     \
	if (meth)                 \
		in_R_HTTPClose2(ctx); \
	else                      \
		in_R_HTTPClose(ctx);

#define Ri_FTPOpen(url) \
	(meth ? in_R_FTPOpen2(url) : in_R_FTPOpen(url));

#define Ri_FTPRead(ctx, dest, len) \
	(meth ? in_R_HTTPRead2(ctx, dest, len) : in_R_FTPRead(ctx, dest, len))

#define Ri_FTPClose(ctx)      \
	if (meth)                 \
		in_R_HTTPClose2(ctx); \
	else                      \
		in_R_FTPClose(ctx);

#else
#define Ri_HTTPOpen in_R_HTTPOpen
#define Ri_HTTPRead in_R_HTTPRead
#define Ri_HTTPClose in_R_HTTPClose
#define Ri_FTPOpen in_R_FTPOpen
#define Ri_FTPRead in_R_FTPRead
#define Ri_FTPClose in_R_FTPClose
#endif

#include <Rmodules/Rinternet.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
/* Solaris and AIX define open as open64 under some circumstances */
# undef open
#endif

/* ------------------- internet access functions  --------------------- */

static Rboolean IDquiet = TRUE;

static Rboolean url_open(Rconnection con)
{
    void *ctxt;
    char *url = con->description;
    UrlScheme type = ((Rurlconn)(con->connprivate))->type;
    int mlen;

    if(con->mode[0] != 'r') {
	REprintf(_("can only open URLs for reading"));
	return FALSE;
    }

    switch(type) {
#ifdef _WIN32
    case HTTPSsh:
	    warning(_("for https:// URLs use method = \"wininet\""));
	    return FALSE;
#endif
    case HTTPsh:
    {
	SEXP sagent, agentFun;
	const char *agent;
	SEXP s_makeUserAgent = install("makeUserAgent");
	agentFun = PROTECT(lang1(s_makeUserAgent)); // defaults to ,TRUE
	SEXP utilsNS = PROTECT(R_FindNamespace(mkString("utils")));
	struct urlconn *uc = (urlconn*) con->connprivate;
	sagent = eval(agentFun, utilsNS);
	UNPROTECT(1); /* utilsNS */
	PROTECT(sagent);
	if(TYPEOF(sagent) == NILSXP)
	    agent = nullptr;
	else
	    agent = CHAR(STRING_ELT(sagent, 0));
	ctxt = in_R_HTTPOpen(url, agent, uc->headers, 0);
	UNPROTECT(2);
	if(ctxt == nullptr) {
	  /* if we call error() we get a connection leak*/
	  /* so do_url has to raise the error*/
	  /* error("cannot open URL '%s'", url); */
	    return FALSE;
	}
	((Rurlconn)(con->connprivate))->ctxt = ctxt;
    }
	break;
    case FTPsh:
	ctxt = in_R_FTPOpen(url);
	if(ctxt == nullptr) {
	  /* if we call error() we get a connection leak*/
	  /* so do_url has to raise the error*/
	  /* error("cannot open URL '%s'", url); */
	    return FALSE;
	}
	((Rurlconn)(con->connprivate))->ctxt = ctxt;
	break;

    default:
	warning(_("scheme not supported in URL '%s'"), url);
	return FALSE;
    }

    con->isopen = TRUE;
    con->canwrite = (Rboolean) (con->mode[0] == 'w' || con->mode[0] == 'a');
    con->canread = (Rboolean) !con->canwrite;
    mlen = (int) strlen(con->mode);
    if(mlen >= 2 && con->mode[mlen - 1] == 'b') con->text = FALSE;
    else con->text = TRUE;
    con->save = -1000;
    set_iconv(con);
    return TRUE;
}

static void url_close(Rconnection con)
{
    UrlScheme type = ((Rurlconn)(con->connprivate))->type;
    struct urlconn *uc = (urlconn*) con->connprivate;
    switch(type) {
    case HTTPsh:
    case HTTPSsh:
	if (uc && uc->headers) free(uc->headers);
	in_R_HTTPClose(uc->ctxt);
	break;
    case FTPsh:
	in_R_FTPClose(uc->ctxt);
	break;
    default:
	break;
    }
    con->isopen = FALSE;
}

static int url_fgetc_internal(Rconnection con)
{
    UrlScheme type = ((Rurlconn)(con->connprivate))->type;
    void * ctxt = ((Rurlconn)(con->connprivate))->ctxt;
    unsigned char c;
    size_t n = 0; /* -Wall */

    switch(type) {
    case HTTPsh:
    case HTTPSsh:
	n = in_R_HTTPRead(ctxt, (char *)&c, 1);
	break;
    case FTPsh:
	n = in_R_FTPRead(ctxt, (char *)&c, 1);
	break;
    default:
	break;
    }
    return (n == 1) ? c : R_EOF;
}

static size_t url_read(void *ptr, size_t size, size_t nitems,
		       Rconnection con)
{
    UrlScheme type = ((Rurlconn)(con->connprivate))->type;
    void * ctxt = ((Rurlconn)(con->connprivate))->ctxt;
    size_t n = 0; /* -Wall */

    switch(type) {
    case HTTPsh:
    case HTTPSsh:
	n = in_R_HTTPRead(ctxt, static_cast<char *>(ptr), (int)(size*nitems));
	break;
    case FTPsh:
	n = in_R_FTPRead(ctxt, static_cast<char *>(ptr), (int)(size*nitems));
	break;
    default:
	break;
    }
    return n/size;
}

#ifdef _WIN32
static Rboolean url_open2(Rconnection con)
{
    void *ctxt;
    char *url = con->description;
    UrlScheme type = ((Rurlconn)(con->connprivate))->type;
    int mlen;

    if(con->mode[0] != 'r') {
	REprintf(_("can only open URLs for reading"));
	return FALSE;
    }

    switch(type) {
    case HTTPSsh:
    case HTTPsh:
    {
	SEXP sagent, agentFun;
	const char *agent;
	SEXP s_makeUserAgent = install("makeUserAgent");
	struct urlconn * uc = (urlconn*) con->connprivate;
	agentFun = PROTECT(lang2(s_makeUserAgent, ScalarLogical(0)));
	sagent = PROTECT(eval(agentFun, R_FindNamespace(mkString("utils"))));
	if(TYPEOF(sagent) == NILSXP)
	    agent = nullptr;
	else
	    agent = CHAR(STRING_ELT(sagent, 0));
	ctxt = in_R_HTTPOpen2(url, agent, uc->headers, 0);
	UNPROTECT(2);
	if(ctxt == nullptr) {
	  /* if we call error() we get a connection leak*/
	  /* so do_url has to raise the error*/
	  /* error("cannot open URL '%s'", url); */
	    return FALSE;
	}
	((Rurlconn)(con->connprivate))->ctxt = ctxt;
    }
	break;
    case FTPsh:
	ctxt = in_R_FTPOpen2(url);
	if(ctxt == nullptr) {
	  /* if we call error() we get a connection leak*/
	  /* so do_url has to raise the error*/
	  /* error("cannot open URL '%s'", url); */
	    return FALSE;
	}
	((Rurlconn)(con->connprivate))->ctxt = ctxt;
	break;

    default:
	warning(_("scheme not supported in URL '%s'"), url);
	return FALSE;
    }

    con->isopen = TRUE;
    con->canwrite = (con->mode[0] == 'w' || con->mode[0] == 'a');
    con->canread = !con->canwrite;
    mlen = (int) strlen(con->mode);
    if(mlen >= 2 && con->mode[mlen - 1] == 'b') con->text = FALSE;
    else con->text = TRUE;
    con->save = -1000;
    set_iconv(con);
    return TRUE;
}

static void url_close2(Rconnection con)
{
    UrlScheme type = ((Rurlconn)(con->connprivate))->type;
    switch(type) {
    case HTTPsh:
    case HTTPSsh:
    case FTPsh:
	in_R_HTTPClose2(((Rurlconn)(con->connprivate))->ctxt);
	break;
    default:
	break;
    }
    con->isopen = FALSE;
}

static int url_fgetc_internal2(Rconnection con)
{
    UrlScheme type = ((Rurlconn)(con->connprivate))->type;
    void * ctxt = ((Rurlconn)(con->connprivate))->ctxt;
    unsigned char c;
    size_t n = 0; /* -Wall */

    switch(type) {
    case HTTPsh:
    case HTTPSsh:
    case FTPsh:
	n = in_R_HTTPRead2(ctxt, (char *)&c, 1);
	break;
    default:
	break;
    }
    return (n == 1) ? c : R_EOF;
}

static size_t url_read2(void *ptr, size_t size, size_t nitems,
			Rconnection con)
{
    UrlScheme type = ((Rurlconn)(con->connprivate))->type;
    void * ctxt = ((Rurlconn)(con->connprivate))->ctxt;
    size_t n = 0; /* -Wall */

    switch(type) {
    case HTTPsh:
    case HTTPSsh:
    case FTPsh:
	n = in_R_HTTPRead2(ctxt, ptr, (int)(size*nitems));
	break;
    default:
	break;
    }
    return n/size;
}
#endif

static Rconnection in_R_newurl(const char *description, const char * const mode, SEXP headers, int type)
{
    Rconnection newconn;
    newconn = (Rconnection) malloc(sizeof(struct Rconn));
    if(!newconn) error(_("allocation of url connection failed"));
    newconn->connclass = (char *) malloc(strlen("url") + 1);
    if(!newconn->connclass) {
	free(newconn);
	error(_("allocation of url connection failed"));
        /* for Solaris 12.5 */ newconn = nullptr;
    }
    strcpy(newconn->connclass, "url");
    newconn->description = (char *) malloc(strlen(description) + 1);
    if(!newconn->description) {
	free(newconn->connclass); free(newconn);
	error(_("allocation of url connection failed"));
        /* for Solaris 12.5 */ newconn = nullptr;
    }
    init_con(newconn, description, CE_NATIVE, mode);
    newconn->canwrite = FALSE;
#ifdef _WIN32
    if (type) {
	newconn->open = &url_open2;
	newconn->read = &url_read2;
	newconn->close = &url_close2;
	newconn->fgetc_internal = &url_fgetc_internal2;
    } else
#endif
    {
	newconn->open = &url_open;
	newconn->read = &url_read;
	newconn->close = &url_close;
	newconn->fgetc_internal = &url_fgetc_internal;
    }
    newconn->fgetc = &dummy_fgetc;
    newconn->connprivate = (void *) malloc(sizeof(struct urlconn));
	struct urlconn *uc = (urlconn*) newconn->connprivate;
    if(!newconn->connprivate) {
	free(newconn->description); free(newconn->connclass); free(newconn);
	error(_("allocation of url connection failed"));
	/* for Solaris 12.5 */ newconn = nullptr;
    }
    uc->headers = nullptr;
    if(!isNull(headers)) {
	uc->headers = strdup(CHAR(STRING_ELT(headers, 0)));
	if(!uc->headers) {
	    free(newconn->description); free(newconn->connclass); free(newconn->connprivate); free(newconn);
	    error(_("allocation of url connection failed"));
	    /* for Solaris 12.5 */ newconn = nullptr;
	}
    }

    IDquiet = TRUE;
    return newconn;
}



static void putdots(DLsize_t *pold, DLsize_t newt)
{
    DLsize_t i, old = *pold;
    *pold = newt;
    for(i = old; i < newt; i++) {
	REprintf(".");
	if ((i + 1) % 50 == 0)
	{
		REprintf("\n");
	}
	else if ((i + 1) % 10 == 0)
	{
		REprintf(" ");
	}
	}
    if(R_Consolefile) fflush(R_Consolefile);
}

static void putdashes(int *pold, int newi)
{
    int i, old = *pold;
    *pold = newi;
    for(i = old; i < newi; i++)  REprintf("=");
    if(R_Consolefile) fflush(R_Consolefile);
}

/* note, ALL the possible structures have the first two elements */
struct inetconn
{
	DLsize_t length;
	char *type;
	void *ctxt;
};

#ifdef _WIN32
#include <ga.h>

struct winprogressbar
{
	window wprog;
	progressbar pb;
	label l_url;
	RCNTXT cntxt;
	int pc;
};

static winprogressbar pbar = {NULL, NULL, NULL};

static void doneprogressbar(void *data)
{
    winprogressbar *pbar = data;
    hide(pbar->wprog);
}
#endif

/* download(url, destfile, quiet, mode, headers, cacheOK) */

constexpr size_t CPBUFSIZE = 65536;
constexpr size_t IBUFSIZE = 4096;
static SEXP in_do_download(SEXP args)
{
    SEXP scmd, sfile, smode, sheaders;
    const char *url, *file, *mode;
    int quiet, status = 0, cacheOK;
#ifdef _WIN32
    char pbuf[30];
    int pc;
#endif

    scmd = CAR(args); args = CDR(args);
    if(!isString(scmd) || length(scmd) < 1)
	error(_("invalid '%s' argument"), "url");
    if(length(scmd) > 1)
	warning(_("only first element of '%s' argument will be used"), "url");
    url = CHAR(STRING_ELT(scmd, 0));
    sfile = CAR(args); args = CDR(args);
    if(!isString(sfile) || length(sfile) < 1)
	error(_("invalid '%s' argument"), "destfile");
    if(length(sfile) > 1)
	warning(_("only first element of '%s' argument will be used"), "destfile");
    file = translateChar(STRING_ELT(sfile, 0));
    quiet = asLogical(CAR(args)); args = CDR(args);
	IDquiet = (Rboolean) quiet;
    if(quiet == NA_LOGICAL)
	error(_("invalid '%s' argument"), "quiet");
    smode =  CAR(args); args = CDR(args);
    if(!isString(smode) || length(smode) != 1)
	error(_("invalid '%s' argument"), "mode");
    mode = CHAR(STRING_ELT(smode, 0));
    cacheOK = asLogical(CAR(args)); args = CDR(args);
    if(cacheOK == NA_LOGICAL)
	error(_("invalid '%s' argument"), "cacheOK");
    bool file_URL = (streqln(url, "file://", 7));
    sheaders = CAR(args);
    if(TYPEOF(sheaders) != NILSXP && !isString(sheaders))
        error(_("invalid '%s' argument"), "headers");
#ifdef _WIN32
    int meth = asLogical(CADR(args));
    if(meth == NA_LOGICAL)
	error(_("invalid '%s' argument"), "method");
//    if(meth == 0) meth = UseInternet2;
    if (!file_URL && R_Interactive && !quiet && !pbar.wprog) {
	pbar.wprog = newwindow(_("Download progress"), rect(0, 0, 540, 100),
			       Titlebar | Centered);
	setbackground(pbar.wprog, dialog_bg());
	pbar.l_url = newlabel(" ", rect(10, 15, 520, 25), AlignCenter);
	pbar.pb = newprogressbar(rect(20, 50, 500, 20), 0, 1024, 1024, 1);
	pbar.pc = 0;
    }
#endif
    if(file_URL) {
	FILE *in, *out;
	static char buf[CPBUFSIZE];
	size_t n;
	int nh = 7, mlen;
#ifdef _WIN32
	/* on Windows we have file:///d:/path/to
	   whereas on Unix it is file:///path/to */
	if (strlen(url) > 9 && url[7] == '/' && url[9] == ':') nh = 8;
#endif

	/* Use binary transfers? */
	mlen = (int) strlen(mode);
	in = R_fopen(R_ExpandFileName(url+nh),
	             (mlen >= 2 && mode[mlen - 1] == 'b') ? "rb" : "r");
	if(!in) {
	    error(_("cannot open URL '%s', reason '%s'"), url, strerror(errno));
	}

	out = R_fopen(R_ExpandFileName(file), mode);
	if(!out) {
	    fclose(in);
	    error(_("cannot open destfile '%s', reason '%s'"), file, strerror(errno));
	}
	while((n = fread(buf, 1, CPBUFSIZE, in)) > 0) {
	    size_t res = fwrite(buf, 1, n, out);
	    if(res != n) error(_("write failed"));
	}
	fclose(out); fclose(in);

    } else if (strncmp(url, "http://", 7) == 0
#ifdef _WIN32
	       || ((strncmp(url, "https://", 8) == 0) && meth)
#endif
	) {

	FILE *out;
	void *ctxt;
	DLsize_t len, total, guess, nbytes = 0;
	char buf[IBUFSIZE];
	int ndashes = 0;
	DLsize_t ndots = 0;
#ifdef _WIN32
	int factor = 1;
#endif

	out = R_fopen(R_ExpandFileName(file), mode);
	if(!out) {
	    error(_("cannot open destfile '%s', reason '%s'"), file, strerror(errno));
	}

	R_Busy(1);
	if(!quiet) REprintf(_("Trying URL '%s'\n"), url);
	SEXP agentFun, sagent;
#ifdef _WIN32
	R_FlushConsole();
	if(meth)
	    agentFun = PROTECT(lang2(install("makeUserAgent"), ScalarLogical(0)));
	else
	    agentFun = PROTECT(lang1(install("makeUserAgent")));
#else
	agentFun = PROTECT(lang1(install("makeUserAgent")));
#endif
	SEXP utilsNS = PROTECT(R_FindNamespace(mkString("utils")));
	sagent = eval(agentFun, utilsNS);
	UNPROTECT(1); /* utilsNS */
	PROTECT(sagent);
	const char *cagent = (TYPEOF(sagent) == NILSXP) ?
	    NULL : CHAR(STRING_ELT(sagent, 0));
	/* TODO: flatten headers */
	const char *cheaders = (TYPEOF(sheaders) == NILSXP) ?
	    NULL : CHAR(STRING_ELT(sheaders, 0));
	ctxt = Ri_HTTPOpen(url, cagent, cheaders, cacheOK);
	UNPROTECT(2);
	if(ctxt == nullptr) status = 1;
	else {
//	    if(!quiet) REprintf(_("Opened URL\n"), url);
	    guess = total = ((inetconn *)ctxt)->length;
#ifdef _WIN32
	    if(R_Interactive) {
		if (guess <= 0) guess = 100 * 1024;
		if (guess > 1e9) factor = guess/1e6;
		R_FlushConsole();
		strcpy(buf, "URL: ");
		if(strlen(url) > 60) {
		    strcat(buf, "... ");
		    strcat(buf, url + (strlen(url) - 60));
		} else strcat(buf, url);
		if(!quiet) {
		    settext(pbar.l_url, buf);
		    setprogressbarrange(pbar.pb, 0, guess/factor);
		    setprogressbar(pbar.pb, 0);
		    settext(pbar.wprog, _("Download progress"));
		    show(pbar.wprog);
		    RCNTXT::begincontext((pbar.cntxt), CTXT_CCODE, R_NilValue, R_NilValue,
				 R_NilValue, R_NilValue, R_NilValue);
		    pbar.cntxt.setContextEnd(&doneprogressbar);
		    pbar.cntxt.setContextEndData(&pbar);
		    pbar.pc = 0;
		}
	    }
#endif
	    while ((len = Ri_HTTPRead(ctxt, buf, sizeof(buf))) > 0) {
		size_t res = fwrite(buf, 1, len, out);
		if(res != size_t(len)) error(_("write failed"));
		nbytes += len;
		if(!quiet) {
#ifdef _WIN32
		    if(R_Interactive) {
			if(nbytes > guess) {
			    guess *= 2;
			    if (guess > 1e9) factor = guess/1e6;
			    setprogressbarrange(pbar.pb, 0, guess/factor);
			}
			setprogressbar(pbar.pb, nbytes/factor);
			if (total > 0) {
			    pc = 0.499 + 100.0*nbytes/total;
			    if (pc > pbar.pc) {
				snprintf(pbuf, 30, _("%d%% downloaded"), pc);
				settext(pbar.wprog, pbuf);
				pbar.pc = pc;
			    }
			}
		    } else
#endif
		    {
			if(guess <= 0) putdots(&ndots, nbytes/1024);
			else putdashes(&ndashes, (int)(50*nbytes/guess));
		    }
		}
	    }
	    Ri_HTTPClose(ctxt);
	    if(!quiet) {
#ifdef _WIN32
		if(!R_Interactive) REprintf("\n");
#else
		REprintf("\n");
#endif
		if (nbytes > 1024 * 1024)
		{
			REprintf(_("Downloaded %0.1f MB"), (double)nbytes / 1024 / 1024);
		}
		else if (nbytes > 10240)
		{
			REprintf(_("Downloaded %d KB"), (int)nbytes / 1024);
		}
		else
		{
			REprintf(n_("Downloaded %d byte", "Downloaded %d bytes", (int)nbytes), (int)nbytes);
		}
		REprintf("\n\n");
		}
#ifdef _WIN32
	    R_FlushConsole();
	    if(R_Interactive && !quiet) {
		RCNTXT::endcontext((pbar.cntxt));
		doneprogressbar(&pbar);
	    }
#endif
	    if (total > 0 && total != nbytes)
		warning(_("downloaded length %0.f != reported length %0.f"), (double)nbytes, (double)total);
	}
	fclose(out);
	if (status == 1 && strchr(mode, 'w')) unlink(R_ExpandFileName(file));
	R_Busy(0);
	if (status == 1) error(_("cannot open URL '%s'"), url);

    } else if (strncmp(url, "ftp://", 6) == 0) {

	FILE *out;
	void *ctxt;
	DLsize_t len, total, guess, nbytes = 0;
	char buf[IBUFSIZE];
	int ndashes = 0;
	DLsize_t ndots = 0;
#ifdef _WIN32
	int factor = 1;
#endif

	out = R_fopen(R_ExpandFileName(file), mode);
	if(!out) {
	    error(_("cannot open destfile '%s', reason '%s'"),
		  file, strerror(errno));
	}

	R_Busy(1);
	if(!quiet) REprintf(_("Trying URL '%s'\n"), url);
#ifdef _WIN32
	R_FlushConsole();
#endif
	ctxt = Ri_FTPOpen(url);
	if(ctxt == nullptr) status = 1;
	else {
//	    if(!quiet) REprintf(_("Opened URL\n"), url);
	    guess = total = ((inetconn *)ctxt)->length;
#ifdef _WIN32
	    if(R_Interactive && !quiet) {
		if (guess <= 0) guess = 100 * 1024;
		if (guess > 1e9) factor = guess/1e6;
		R_FlushConsole();
		strcpy(buf, "URL: ");
		if(strlen(url) > 60) {
		    strcat(buf, "... ");
		    strcat(buf, url + (strlen(url) - 60));
		} else strcat(buf, url);
		settext(pbar.l_url, buf);
		setprogressbarrange(pbar.pb, 0, guess/factor);
		setprogressbar(pbar.pb, 0);
		settext(pbar.wprog, _("Download progress"));
		show(pbar.wprog);

		/* set up a context which will close progressbar on error. */
		RCNTXT::begincontext((pbar.cntxt), CTXT_CCODE, R_NilValue, R_NilValue,
			     R_NilValue, R_NilValue, R_NilValue);
		pbar.cntxt.setContextEnd(&doneprogressbar);
		pbar.cntxt.setContextEndData(&pbar);
		pbar.pc = 0;
	    }
#endif
	    while ((len = Ri_FTPRead(ctxt, buf, sizeof(buf))) > 0) {
		size_t res = fwrite(buf, 1, len, out);
		if(res != size_t(len)) error(_("write failed"));
		nbytes += len;
		if(!quiet) {
#ifdef _WIN32
		    if(R_Interactive) {
			if(nbytes > guess) {
			    guess *= 2;
			    if (guess > 1e9) factor = guess/1e6;
			    setprogressbarrange(pbar.pb, 0, guess/factor);
			}
			setprogressbar(pbar.pb, nbytes/factor);
			if (total > 0) {
			    pc = 0.499 + 100.0*nbytes/total;
			if (pc > pbar.pc) {
			    snprintf(pbuf, 30, _("%d%% downloaded"), pc);
			    settext(pbar.wprog, pbuf);
			    pbar.pc = pc;
			}
			}
		    } else
#endif
		    {
			if(guess <= 0) putdots(&ndots, nbytes/1024);
			else putdashes(&ndashes, (int)(50*nbytes/guess));
		    }
		}
	    }
	    Ri_FTPClose(ctxt);
	    if(!quiet) {
#ifdef _WIN32
		if(!R_Interactive) REprintf("\n");
#else
		REprintf("\n");
#endif
		if (nbytes > 1024 * 1024)
		{
			REprintf(_("Downloaded %0.1f MB"), (double)nbytes / 1024 / 1024);
		}
		else if (nbytes > 10240)
		{
			REprintf(_("Downloaded %d KB"), (int)nbytes / 1024);
		}
		else
		{
			REprintf(n_("Downloaded %d byte", "Downloaded %d bytes", (int)nbytes), (int)nbytes);
		}
		REprintf("\n\n");
		}
#ifdef _WIN32
	    R_FlushConsole();
	    if(R_Interactive && !quiet) {
		RCNTXT::endcontext((pbar.cntxt));
		doneprogressbar(&pbar);
	    }
#endif
	    if (total > 0 && total != nbytes)
		warning(_("downloaded length %0.f != reported length %0.f"),
			(double)nbytes, (double)total);
	}
	R_Busy(0);
	fclose(out);
	if (status == 1 && strchr(mode, 'w')) unlink(R_ExpandFileName(file));
	if (status == 1) error(_("cannot open URL '%s'"), url);
    } else
	error(_("scheme not supported in URL '%s'"), url);

    return ScalarInteger(status);
}


void *in_R_HTTPOpen(const char *url, const char *agent, const char *headers, int cacheOK)
{
    inetconn *con;
    void *ctxt;
    int timeout = asInteger(GetOption1(install("timeout")));
    DLsize_t len = -1;
    char *type = nullptr;
    char *fullheaders = nullptr;

    if(timeout == NA_INTEGER || timeout <= 0) timeout = 60;

    RxmlNanoHTTPTimeout(timeout);

    if (agent || headers) {
	fullheaders = (char*) malloc((agent ? strlen(agent) : 0) +
			     (headers ? strlen(headers) : 0) + 1);
	if(!fullheaders) error(_("could not allocate memory for http headers"));
	fullheaders[0] = '\0';
	if (agent) strcat(fullheaders, agent);
	if (headers) strcat(fullheaders, headers);
    }

    ctxt = RxmlNanoHTTPOpen(url, NULL, fullheaders, cacheOK);
    if (fullheaders) free(fullheaders);

    if(ctxt != nullptr) {
	int rc = RxmlNanoHTTPReturnCode(ctxt);
	if(rc != 200) {
	    // FIXME: should this be ctxt->location, after redirection?
	    warning(_("cannot open URL '%s': %s status was '%d %s'"),
		    url, "HTTP", rc, RxmlNanoHTTPStatusMsg(ctxt));
	    RxmlNanoHTTPClose(ctxt);
	    return nullptr;
	} else {
	    type = RxmlNanoHTTPContentType(ctxt);
	    len = RxmlNanoHTTPContentLength(ctxt);
	    if(!IDquiet){
			if (len > 1024 * 1024)
			{ // might be longer than long, and is on 64-bit windows
				REprintf(n_("Content type '%s' length %0.0f byte (%0.1f MB)",
							"Content type '%s' length %0.0f bytes (%0.1f MB)",
							len),
						 type ? type : "unknown", (double)len, len / 1024.0 / 1024.0);
			}
			else if (len > 10240)
			{
				REprintf(n_("Content type '%s' length %d byte (%d KB)",
							"Content type '%s' length %d bytes (%d KB)",
							(int)len),
						 type ? type : "unknown", (int)len, (int)(len / 1024));
			}
			else if (len >= 0)
			{
				REprintf(n_("Content type '%s' length %d byte",
							"Content type '%s' length %d bytes",
							(int)len),
						 type ? type : "unknown", (int)len);
			}
			else
			{
				REprintf(_("Content type '%s' length unknown"), type ? type : "unknown", len);
			}
			REprintf("\n");
#ifdef _WIN32
		R_FlushConsole();
#endif
	    }
	}
    } else return nullptr;
    con = (inetconn *) malloc(sizeof(inetconn));
    if(con) {
	con->length = len;
	con->type = type;
	con->ctxt = ctxt;
    }
    return con;
}

static int in_R_HTTPRead(void *ctx, char *dest, int len)
{
	return RxmlNanoHTTPRead(((inetconn *)ctx)->ctxt, dest, len);
}

static void in_R_HTTPClose(void *ctx)
{
	if (ctx)
	{
		RxmlNanoHTTPClose(((inetconn *)ctx)->ctxt);
		free(ctx);
	}
}

static void *in_R_FTPOpen(const char *url)
{
    inetconn *con;
    void *ctxt;
    int timeout = asInteger(GetOption1(install("timeout")));
    DLsize_t len = 0;

    if(timeout == NA_INTEGER || timeout <= 0) timeout = 60;
    RxmlNanoFTPTimeout(timeout);
    ctxt = RxmlNanoFTPOpen(url);
    if(!ctxt) return nullptr;
    if(!IDquiet) {
	len = RxmlNanoFTPContentLength(ctxt);
	if(len >= 0)
	    REprintf(_("ftp data connection made, file length %ld bytes\n"), len);
	else
	    REprintf(_("ftp data connection made, file length unknown\n"));
#ifdef _WIN32
	R_FlushConsole();
#endif
    }
    con = (inetconn *) malloc(sizeof(inetconn));
    if(con) {
	con->length = len;
	con->type = nullptr;
	con->ctxt = ctxt;
    }
    return con;
}

static int in_R_FTPRead(void *ctx, char *dest, int len)
{
	return RxmlNanoFTPRead(((inetconn *)ctx)->ctxt, dest, len);
}

static void in_R_FTPClose(void *ctx)
{
	if (ctx)
	{
		RxmlNanoFTPClose(((inetconn *)ctx)->ctxt);
		free(ctx);
	}
}

#ifdef _WIN32

#define WIN32_LEAN_AND_MEAN 1
#include <windows.h>
#include <wininet.h>
typedef struct wictxt
{
	DLsize_t length;
	char *type;
	HINTERNET hand;
	HINTERNET session;
} wIctxt, *WIctxt;

static void *in_R_HTTPOpen2(const char *url, const char *agent, const char *headers,
			    int cacheOK)
{
    WIctxt  wictxt;
    DWORD status = 0, len = 0, d1 = 4, d2 = 0, d3 = 100;
    char buf[101], *p;

    wictxt = (WIctxt) malloc(sizeof(wIctxt));
    wictxt->length = -1;
    wictxt->type = nullptr;
    wictxt->hand =
	InternetOpen(agent, INTERNET_OPEN_TYPE_PRECONFIG, NULL, NULL, 0);
    if(!wictxt->hand) {
	free(wictxt);
	/* error("cannot open Internet connection"); */
	return nullptr;
    }

    // use keep-alive semantics, do not use local WinINet cache.
    DWORD flags = INTERNET_FLAG_KEEP_CONNECTION | INTERNET_FLAG_NO_CACHE_WRITE |
                  INTERNET_FLAG_IGNORE_REDIRECT_TO_HTTPS | INTERNET_FLAG_IGNORE_REDIRECT_TO_HTTP;
    if(!cacheOK) flags |= INTERNET_FLAG_PRAGMA_NOCACHE;
    wictxt->session = InternetOpenUrl(wictxt->hand, url, headers, headers ? -1 : 0, flags, 0);
    if(!wictxt->session) {
	DWORD err1 = GetLastError(), err2, blen = 101;
	InternetCloseHandle(wictxt->hand);
	free(wictxt);
	if (err1 == ERROR_INTERNET_EXTENDED_ERROR) {
	    InternetGetLastResponseInfo(&err2, buf, &blen);
	    /* some of these messages end in \r\n */
	    while(true) {
		p = buf + strlen(buf) - 1;
		if(*p == '\n' || *p == '\r') *p = '\0'; else break;
	    }
	    warning(_("InternetOpenUrl failed: '%s'"), buf);
	    return nullptr;
	} else {
	    FormatMessage(
		FORMAT_MESSAGE_FROM_HMODULE,
		GetModuleHandle("wininet.dll"),
		err1,
		MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
		buf, 101, NULL);
	    /* some of these messages end in \r\n */
	    while(true) {
		p = buf + strlen(buf) - 1;
		if(*p == '\n' || *p == '\r') *p = '\0'; else break;
	    }
	    warning(_("InternetOpenUrl failed: '%s'"), buf);
	    return nullptr;
	}
    }

    HttpQueryInfo(wictxt->session,
		  HTTP_QUERY_STATUS_CODE | HTTP_QUERY_FLAG_NUMBER,
		  &status, &d1, &d2);
    if(status != 200) {
	d2 = 0;
	HttpQueryInfo(wictxt->session,
		      HTTP_QUERY_STATUS_TEXT, &buf, &d3, &d2);
	InternetCloseHandle(wictxt->session);
	InternetCloseHandle(wictxt->hand);
	free(wictxt);
	warning(_("cannot open URL '%s': %s status was '%d %s'"),
		url, "HTTP", status, buf);
	return nullptr;
    }

    HttpQueryInfo(wictxt->session,
		  HTTP_QUERY_CONTENT_TYPE, &buf, &d3, &d2);
    d2 = 0;
    // NB: this can only retrieve in a DWORD, so up to 2GB or 4GB?
    if (HttpQueryInfo(wictxt->session,
		      HTTP_QUERY_CONTENT_LENGTH | HTTP_QUERY_FLAG_NUMBER,
		      &len, &d1, &d2))
	wictxt->length = len;
    wictxt->type = Rstrdup(buf);
    if(!IDquiet) {
		if (len > 1024 * 1024)
		{
			REprintf(n_("Content type '%s' length %0.0f byte (%0.1f MB)", "Content type '%s' length %0.0f bytes (%0.1f MB)", len), buf, (double)len, len / 1024.0 / 1024.0);
		}
		else if (len > 10240)
		{
			REprintf(n_("Content type '%s' length %d byte (%d KB)", "Content type '%s' length %d bytes (%d KB)", (int)len), buf, (int)len, (int)(len / 1024));
		}
		else if (wictxt->length >= 0) /* signed; len is not */
		{
			REprintf(n_("Content type '%s' length %d byte", "Content type '%s' length %d bytes", (int)len), buf, (int)len);
		}
		else
		{
			REprintf(_("Content type '%s' length unknown"), buf);
		}
		REprintf("\n");
		R_FlushConsole();
    }

    R_ProcessEvents();
    return (void *)wictxt;
}

static int in_R_HTTPRead2(void *ctx, char *dest, int len)
{
    DWORD nread;

    InternetReadFile(((WIctxt)ctx)->session, dest, len, &nread);
    R_ProcessEvents();
    return (int) nread;
}


static void in_R_HTTPClose2(void *ctx)
{
    InternetCloseHandle(((WIctxt)ctx)->session);
    InternetCloseHandle(((WIctxt)ctx)->hand);
    if(((WIctxt)ctx)->type) free(((WIctxt)ctx)->type);
    free(ctx);
}

static void *in_R_FTPOpen2(const char *url)
{
    WIctxt  wictxt;

    wictxt = (WIctxt) malloc(sizeof(wIctxt));
    wictxt->length = -1;
    wictxt->type = nullptr;

    wictxt->hand =
	InternetOpen("R", INTERNET_OPEN_TYPE_PRECONFIG, NULL, NULL, 0);
    if(!wictxt->hand) {
	free(wictxt);
	return nullptr;
    }

    DWORD flag = INTERNET_FLAG_KEEP_CONNECTION | INTERNET_FLAG_NO_CACHE_WRITE;
    wictxt->session = InternetOpenUrl(wictxt->hand, url, NULL, 0,
    	flag | INTERNET_FLAG_PASSIVE, 0);
    if(!wictxt->session)
    	wictxt->session = InternetOpenUrl(wictxt->hand, url, NULL, 0, flag, 0);
    if(!wictxt->session) {
	char buf[256];
	DWORD err1 = GetLastError(), err2, blen = 256;
	InternetCloseHandle(wictxt->hand);
	free(wictxt);
	if (err1 == ERROR_INTERNET_EXTENDED_ERROR) {
	    InternetGetLastResponseInfo(&err2, buf, &blen);
	    warning(_("InternetOpenUrl failed: '%s'"), buf);
	    return nullptr;
	} else {
	    FormatMessage(
		FORMAT_MESSAGE_FROM_HMODULE,
		GetModuleHandle("wininet.dll"),
		err1,
		MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
		buf, 101, NULL);
	    warning(_("InternetOpenUrl failed: '%s'"), buf);
	    return nullptr;
	}
    }
    R_ProcessEvents();
    return (void *)wictxt;
}
#endif // Win32


constexpr size_t MBUFSIZE = 8192;
void RxmlMessage(int level, const char *format, ...)
{
    int clevel;
    char buf[MBUFSIZE], *p;
    va_list ap;

    clevel = asInteger(GetOption1(install("internet.info")));
    if(clevel == NA_INTEGER) clevel = 2;

    if(level < clevel) return;

    va_start(ap, format);
    vsnprintf(buf, MBUFSIZE, format, ap);
    buf[MBUFSIZE-1] = '\0';
    va_end(ap);
    p = buf + strlen(buf) - 1;
    if(strlen(buf) > 0 && *p == '\n') *p = '\0';
    warning(buf);
}

#include "sock.h"
#define STRICT_R_HEADERS
#include <R_ext/RS.h> /* for R_Calloc */
#include <R_ext/Rdynload.h>

extern "C"
VISIBLE void R_init_internet(DllInfo *info)
{
    R_InternetRoutines *tmp;
    tmp = R_Calloc(1, R_InternetRoutines);

    tmp->download = in_do_download;
    tmp->newurl =  in_R_newurl;
    tmp->newsock = in_R_newsock;
    tmp->newservsock = in_R_newservsock;

    tmp->HTTPOpen = in_R_HTTPOpen;
    tmp->HTTPRead = in_R_HTTPRead;
    tmp->HTTPClose = in_R_HTTPClose;

    tmp->FTPOpen = in_R_FTPOpen;
    tmp->FTPRead = in_R_FTPRead;
    tmp->FTPClose = in_R_FTPClose;

    tmp->sockopen = in_Rsockopen;
    tmp->socklisten = in_Rsocklisten;
    tmp->sockconnect = in_Rsockconnect;
    tmp->sockclose = in_Rsockclose;
    tmp->sockread = in_Rsockread;
    tmp->sockwrite = in_Rsockwrite;

    tmp->sockselect = in_Rsockselect;

    tmp->HTTPDCreate = in_R_HTTPDCreate;
    tmp->HTTPDStop = in_R_HTTPDStop;

    tmp->curlVersion = in_do_curlVersion;
    tmp->curlGetHeaders = in_do_curlGetHeaders;
    tmp->curlDownload = in_do_curlDownload;
    tmp->newcurlurl =  in_newCurlUrl;

    R_setInternetRoutines(tmp);
}
