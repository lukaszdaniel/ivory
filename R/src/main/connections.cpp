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

/* Notes on so-called 'Large File Support':

   The C stdio functions such as fseek and ftell are defined using
   'long' for file positioning: also fread/fwrite use size_t for size
   and number of items.  (The latter can cause problems with
   reading/writing large blocks, but not in R.)  POSIX introduced
   off_t and fseeko/ftello to allow larger file sizes, since 'long'
   may limit file positioning to 2GB.  (C99 introduced fpos_t and
   f[gs]etpos.)

   Note that the issue really only arises if 'long' is 32-bit, which
   is not the case on all known 64-bit platforms except Windows.
   However, off_t (defined in sys/types.h) is itself often 32-bit,
   which has led to workarounds.  On Linux systems, the macros

   __USE_FILE_OFFSET64
   __USE_LARGEFILE64

   select between __off_t and __off64_t.  Since these are different
   types, the functions using them have to be remapped, and the
   __off64_t versions call fopen64, fseeko64, ftello64 and so on.

   These macros are not intended to be used directly but via (features.h)

   _FILE_OFFSET_BITS=N  Select default filesystem interface.
   _LARGEFILE_SOURCE    Some more functions for correct standard I/O.
   _LARGEFILE64_SOURCE  Additional functionality from LFS for large files.

   The last makes system calls like open64 visible directly, and so
   should not be needed in R.

   This is commonly known as LFS; _but_ 'LFS Linux' is something else.
   See http://en.wikipedia.org/wiki/Large_file_support and
   http://www.suse.de/~aj/linux_lfs.html

   Solaris has a similar scheme: see 'man lf64', 'man lfcompile' and
   'man lfcompile64'.

   On macOS, off_t is typedef-ed to __darwin_off_t, which is
   __int64_t, so the issue never arises.  Similarly on FreeBSD.

   The situation with Windows is similar, but off64_t, fseeko64 etc
   need to be selected explicitly (even on Win64).

   There are also issues with the glob(), readdir(), stat() system
   calls: see platform.cpp and sysutils.cpp

   saveload.cpp uses f[gs]etpos: they have 64-bit versions on LFS Linux
   and Solaris.  But this only used for pre-1.4.0 formats, and fpos_t
   is 64-bit on Windows.
*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#define R_NO_REMAP
#define R_USE_SIGNALS 1

#include <CXXR/BuiltInFunction.hpp>
#include <Localization.h>
#include <RContext.h>
#include <Defn.h>
#include <Internal.h>
#include <Fileio.h>
#include <Rconnections.h>
#include <R_ext/Complex.h>
#include <R_ext/R-ftp-http.h>
#include <R_ext/RS.h>		/* R_chk_calloc and Free */
#include <R_ext/Riconv.h>
//#include <R_ext/Print.h> // REprintf, REvprintf
#include <R_ext/Minmax.h>

using namespace std;
using namespace R;

#undef ERROR			/* for compilation on Windows */

#ifdef _WIN32
#include <trioremap.h>
#endif

namespace R
{
    HIDDEN int R_OutputCon; /* used in printutils.cpp */
}

static void con_destroy(int i);

#include <cerrno>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
/* Solaris and AIX define open as open64 under some circumstances */
# undef open
/* AIX defines truncate as truncate64 under some circumstances */
# undef truncate
#endif

#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

/* This works on Win64 where long is 4 bytes but long long is 8 bytes. */
#if defined __GNUC__ && __GNUC__ >= 2
__extension__ typedef long long int _lli_t;
#else
typedef long long int _lli_t;
#endif

/* Win32 does have popen, but it does not work in GUI applications,
   so test that later */
#ifdef _WIN32
#include <Startup.h>
#endif

constexpr int NCONNECTIONS = 128; /* need one per cluster node */
constexpr int NSINKS = 21;

static Rconnection Connections[NCONNECTIONS];
static SEXP OutTextData;

static int R_SinkNumber;
static int SinkCons[NSINKS], SinkConsClose[NSINKS], R_SinkSplit[NSINKS];

/* We need a unique id for a connection to ensure that the finalizer
   does not try to close it after it is already closed.  And that id
   will be passed as a pointer, so it seemed easiest to use void *.
*/
static void *current_id = nullptr;

/* ------------- admin functions (see also at end) ----------------- */

int Rconn::NextConnection()
{
    int i;
    for (i = 3; i < NCONNECTIONS; i++)
        if (!Connections[i])
            break;
    if (i >= NCONNECTIONS)
    {
        R_gc(); /* Try to reclaim unused ones */
        for (i = 3; i < NCONNECTIONS; i++)
            if (!Connections[i])
                break;
        if (i >= NCONNECTIONS)
            error(_("all connections are in use"));
    }
    return i;
}

//static int ConnIndex(Rconnection con) { return con->ConnIndex(); }

int Rconn::ConnIndex() const
{
    int i;
    for (i = 0; i < NCONNECTIONS; i++)
        if (Connections[i] == this)
            break;
    if (i >= NCONNECTIONS)
        error(_("connection was not found"));
    return i;
}

/* internal, not the same as R function getConnection */
Rconnection getConnection(int n)
{
    Rconnection con = nullptr;

    if(n < 0 || n >= NCONNECTIONS || n == NA_INTEGER ||
       !(con = Connections[n]))
	error(_("invalid connection"));
    return con;

}

HIDDEN int getActiveSink(int n)
{
    if (n >= R_SinkNumber || n < 0)
        return 0;
    if (R_SinkSplit[R_SinkNumber - n])
        return SinkCons[R_SinkNumber - n - 1];
    else
        return 0;
}

static void conFinalizer(SEXP ptr)
{
    int i, ncon;
    void *cptr = R_ExternalPtrAddr(ptr);

    if(!cptr) return;

    for(i = 3; i < NCONNECTIONS; i++)
	if(Connections[i] && Connections[i]->id == cptr) {
	    ncon = i;
	    break;
	}
    if(i >= NCONNECTIONS) return;
    {
	Rconnection thisconn = getConnection(ncon);
	if(strcmp(thisconn->connclass, "textConnection"))
	    warning(_("closing unused connection %d (%s)"), ncon, thisconn->description);
    }

    con_destroy(ncon);
    R_ClearExternalPtr(ptr); /* not really needed */
}

/* for use in REvprintf */
HIDDEN Rconnection getConnection_no_err(int n)
{
    Rconnection con = nullptr;

    if (n < 0 || n >= NCONNECTIONS || n == NA_INTEGER ||
        !(con = Connections[n]))
        return nullptr;
    return con;
}
NORET static void set_iconv_error(Rconnection con, const char *from, const char *to) { con->set_iconv_error(from, to); }
NORET void Rconn::set_iconv_error(const char *from, const char *to) const
{
    char buf[100];
    snprintf(buf, 100, _("unsupported conversion from '%s' to '%s'"), from, to);
    con_destroy(this->ConnIndex());
    error(buf);
}

/* ------------------- buffering --------------------- */

constexpr size_t RBUFFCON_LEN_DEFAULT = 4096;

size_t Rconn::buff_set_len(size_t len)
{
    size_t unread_len = 0;
    unsigned char *buff;

    if (this->buff_len == len)
        return len;

    if (this->buff)
    {
        unread_len = this->buff_stored_len - this->buff_pos;
        len = max(len, unread_len);
    }

    buff = (unsigned char *)malloc(sizeof(unsigned char) * len);

    if (this->buff)
    {
        memcpy(buff, this->buff + this->buff_pos, unread_len);
        free(this->buff);
    }

    this->buff = buff;
    this->buff_len = len;
    this->buff_pos = 0;
    this->buff_stored_len = unread_len;

    return len;
}

void Rconn::buff_init()
{
    this->buff_pos = this->buff_stored_len = 0;
    this->buff_set_len(RBUFFCON_LEN_DEFAULT);
}

static void buff_reset(Rconnection con) {
    size_t unread_len = con->buff_stored_len - con->buff_pos;

    if (unread_len > 0)
	memmove(con->buff, con->buff + con->buff_pos, unread_len);

    con->buff_pos = 0;
    con->buff_stored_len = unread_len;
}

static size_t buff_fill(Rconnection con) {
    size_t free_len, read_len;

    buff_reset(con);

    free_len = con->buff_len - con->buff_stored_len;
    read_len = con->read(con->buff, sizeof(unsigned char), free_len, con);
    if ((int)read_len < 0)
	error(_("error reading from the connection"));
    con->buff_stored_len += read_len;

    return read_len;
}

static int buff_fgetc(Rconnection con)
{
    size_t unread_len;

    unread_len = con->buff_stored_len - con->buff_pos;
    if (unread_len == 0) {
	size_t filled_len = buff_fill(con);
	if (filled_len == 0)
	    return R_EOF;
    }

    return con->buff[con->buff_pos++];
}

static double buff_seek(Rconnection con, double where, int origin, int rw)
{
    size_t unread_len = con->buff_stored_len - con->buff_pos;

    if (rw == 2) /* write */
	return con->seek(con, where, origin, rw);

    if (ISNA(where)) /* tell */
	return con->seek(con, where, origin, rw) - unread_len;

    if (origin == 2) { /* current */
	if (where < unread_len) {
	    con->buff_pos += (size_t) where;
	    return con->seek(con, NA_REAL, origin, rw);
	} else {
	    where -= unread_len;
	}
    }
    con->buff_pos = con->buff_stored_len = 0;

    return con->seek(con, where, origin, rw);
}

void Rconn::set_buffer()
{
    if (this->canread && this->text)
    {
        this->buff_init();
    }
}

void Rf_set_iconv(Rconnection con)
{
    void *tmp;

    /* need to test if this is text, open for reading to writing or both,
       and set inconv and/or outconv */
    if(!con->text || !strlen(con->encname) ||
       streql(con->encname, "native.enc")) {
	con->UTF8out = FALSE;
	return;
    }
    if(con->canread) {
	size_t onb = 50;
	char *ob = con->oconvbuff;
	/* UTF8out is set in readLines() and scan()
	   Was Windows-only until 2.12.0, but we now require iconv.
	 */
	Rboolean useUTF8 = (Rboolean) (!utf8locale && con->UTF8out);
	const char *enc =
	    streql(con->encname, "UTF-8-BOM") ? "UTF-8" : con->encname;
	tmp = Riconv_open(useUTF8 ? "UTF-8" : "", enc);
	if(tmp != (void *)-1) con->inconv = tmp;
	else set_iconv_error(con, con->encname, useUTF8 ? "UTF-8" : "");
	con->EOF_signalled = FALSE;
	/* initialize state, and prepare any initial bytes */
	Riconv(tmp, nullptr, nullptr, &ob, &onb);
	con->navail = (short)(50-onb); con->inavail = 0;
	/* libiconv can handle BOM marks on Windows Unicode files, but
	   glibc's iconv cannot. Aargh ... */
	if(streql(con->encname, "UCS-2LE") ||
	   streql(con->encname, "UTF-16LE")) con->inavail = -2;
	/* Discaard BOM */
	if(streql(con->encname, "UTF-8-BOM")) con->inavail = -3;
    }
    if(con->canwrite) {
	size_t onb = 25;
	char *ob = con->init_out;
	tmp = Riconv_open(con->encname, "");
	if(tmp != (void *)-1) con->outconv = tmp;
	else set_iconv_error(con, con->encname, "");
	/* initialize state, and prepare any initial bytes */
	Riconv(tmp, nullptr, nullptr, &ob, &onb);
	ob[25-onb] = '\0';
    }
}


/* ------------------- null connection functions --------------------- */

NORET static Rboolean null_open(Rconnection con)
{
    error(_("'%s' is not enabled for this connection"), "open");
}

static void null_close(Rconnection con)
{
    con->isopen = FALSE;
}

static void null_destroy(Rconnection con)
{
    if(con->connprivate) free(con->connprivate);
}

NORET static int null_vfprintf(Rconnection con, const char *format, va_list ap)
{
    error(_("'%s' is not enabled for this connection"), "printing");
}

/* va_copy is C99, but a draft standard had __va_copy.  Glibc has
   __va_copy declared unconditionally */


#if defined(HAVE_VASPRINTF) && !HAVE_DECL_VASPRINTF
int vasprintf(char **strp, const char *fmt, va_list ap);
#endif

constexpr int BUFSIZE = 10000;
// similar to Rcons_vprintf in printutils.cpp
int dummy_vfprintf(Rconnection con, const char *format, va_list ap)
{
    R_CheckStack2(BUFSIZE); // prudence
    char buf[BUFSIZE], *b = buf;
    int res;
    const void *vmax = nullptr; /* -Wall*/
    int usedVasprintf = FALSE;
    va_list aq;

    va_copy(aq, ap);
    res = Rvsnprintf_mbcs(buf, BUFSIZE, format, aq);
    va_end(aq);
#ifdef HAVE_VASPRINTF
    if(res >= BUFSIZE || res < 0) {
	res = vasprintf(&b, format, ap);
	if (res < 0) {
	    b = buf;
	    warning(_("printing of extremely long output is truncated"));
	    res = (int)strlen(buf);
	} else usedVasprintf = TRUE;
    }
#else
    if(res >= BUFSIZE) { /* res is the desired output length */
	vmax = vmaxget();
	/* apparently some implementations count short,
	   <http://unixpapa.com/incnote/stdio.html>
	   so add some margin here */
	b = R_alloc(res + 101, sizeof(char));
	vsnprintf(b, res + 100, format, ap);
    } else if(res < 0) {
	/* Some non-C99 conforming vsnprintf implementations return -1 on
	   truncation instead of only on error. */
	vmax = vmaxget();
	b = R_alloc(10*BUFSIZE, sizeof(char));
	res = Rvsnprintf_mbcs(b, 10*BUFSIZE, format, ap);
	if (res < 0 || res >= 10*BUFSIZE) {
	    warning(_("printing of extremely long output is truncated"));
	    res = (int)strlen(b);
	}
    }
#endif /* HAVE_VASPRINTF */
    if(con->outconv) { /* translate the buffer */
	char outbuf[BUFSIZE+1], *ob;
	const char *ib = b;
	size_t inb = res, onb, ires;
	Rboolean again = FALSE;
	size_t ninit = strlen(con->init_out);
	do {
	    onb = BUFSIZE; /* leave space for nul */
	    ob = outbuf;
	    if(ninit) {
		strcpy(ob, con->init_out);
		ob += ninit; onb -= ninit; ninit = 0;
	    }
	    errno = 0;
	    ires = Riconv(con->outconv, &ib, &inb, &ob, &onb);
	    again = (Rboolean) (ires == (size_t)(-1) && errno == E2BIG);
	    if(ires == (size_t)(-1) && errno != E2BIG)
		/* is this safe? */
		warning(_("invalid char string in output conversion"));
	    *ob = '\0';
	    con->write(outbuf, 1, ob - outbuf, con);
	} while(again && inb > 0);  /* it seems some iconv signal -1 on
				       zero-length input */
    } else
	con->write(b, 1, res, con);
    if(vmax) vmaxset(vmax);
    if(usedVasprintf) free(b);
    return res;
}

int dummy_fgetc(Rconnection con)
{
    int c;
    Rboolean checkBOM = FALSE, checkBOM8 = FALSE;

    if(con->inconv) {
	while(con->navail <= 0) {
	    /* Probably in all cases there will be at most one iteration
	       of the loop. It could iterate multiple times only if the input
	       encoding could have \r or \n as a part of a multi-byte coded
	       character.
	    */
	    unsigned int i, inew = 0;
	    char *p, *ob;
	    const char *ib;
	    size_t inb, onb, res;

	    if(con->EOF_signalled) return R_EOF;
	    if(con->inavail == -2) {
		con->inavail = 0;
		checkBOM = TRUE;
	    }
	    if(con->inavail == -3) {
		con->inavail = 0;
		checkBOM8 = TRUE;
	    }
	    p = con->iconvbuff + con->inavail;
	    for(i = con->inavail; i < 25; i++) {
		if (con->buff)
		    c = buff_fgetc(con);
		else
		    c = con->fgetc_internal(con);
		if(c == R_EOF){ con->EOF_signalled = TRUE; break; }
		*p++ = (char) c;
		con->inavail++;
		inew++;
		if(!con->buff && (c == '\n' || c == '\r'))
		    /* Possibly a line separator: better stop filling in the
		       encoding conversion buffer if not buffering the input
		       anyway, as not to confuse interactive applications
		       (PR17634).
		    */
		    break;
	    }
	    if(inew == 0) return R_EOF;
	    if(checkBOM && con->inavail >= 2 &&
	       ((int)con->iconvbuff[0] & 0xff) == 255 &&
	       ((int)con->iconvbuff[1] & 0xff) == 254) {
		con->inavail -= (short) 2;
		memmove(con->iconvbuff, con->iconvbuff+2, con->inavail);
	    }
	    if(inew == 0) return R_EOF;
	    if(checkBOM8 && con->inavail >= 3 &&
	       !memcmp(con->iconvbuff, "\xef\xbb\xbf", 3)) {
		con->inavail -= (short) 3;
		memmove(con->iconvbuff, con->iconvbuff+3, con->inavail);
	    }
	    ib = con->iconvbuff; inb = con->inavail;
	    ob = con->oconvbuff; onb = 50;
	    errno = 0;
	    res = Riconv(con->inconv, &ib, &inb, &ob, &onb);
	    con->inavail = (short) inb;
	    con->next = con->oconvbuff;
	    con->navail = (short)(50 - onb);
	    if(res == (size_t)-1) { /* an error condition */
		if(errno == EINVAL || errno == E2BIG) {
		    /* incomplete input char or no space in output buffer */
		    memmove(con->iconvbuff, ib, inb);
		} else {/*  EILSEQ invalid input */
		    warning(_("invalid input found on input connection '%s'"), con->description);
		    con->inavail = 0;
		    if (con->navail == 0) return R_EOF;
		    con->EOF_signalled = TRUE;
		}
	    }
	}
	con->navail--;
	/* the cast prevents sign extension of 0xFF to -1 (R_EOF) */
	return (unsigned char)*con->next++;
    } else if (con->buff)
	return buff_fgetc(con);
    else
	return con->fgetc_internal(con);
}

NORET static int null_fgetc(Rconnection con)
{
    error(_("'%s' is not enabled for this connection"), "getc");
}

NORET static double null_seek(Rconnection con, double where, int origin, int rw)
{
    error(_("'%s' is not enabled for this connection"), "seek");
}

NORET static void null_truncate(Rconnection con)
{
    error(_("'%s' is not enabled for this connection"), "truncation");
}

static int null_fflush(Rconnection con)
{
    return 0;
}

NORET static size_t null_read(void *ptr, size_t size, size_t nitems,
			Rconnection con)
{
    error(_("'%s' is not enabled for this connection"), "read");
}

NORET static size_t null_write(const void *ptr, size_t size, size_t nitems,
			 Rconnection con)
{
    error(_("'%s' is not enabled for this connection"), "write");
}

void Rf_init_con(Rconnection newconn, const char *description, int enc,
	      const char * const mode)
{
    strcpy(newconn->description, description);
    newconn->enc = enc;
    strncpy(newconn->mode, mode, 4); newconn->mode[4] = '\0';
    newconn->isopen = newconn->incomplete = newconn->blocking = newconn->isGzcon = FALSE;
    newconn->canread = newconn->canwrite = TRUE; /* in principle */
    newconn->canseek = FALSE;
    newconn->text = TRUE;
    newconn->open = &null_open;
    newconn->close = &null_close;
    newconn->destroy = &null_destroy;
    newconn->vfprintf = &null_vfprintf;
    newconn->fgetc = newconn->fgetc_internal = &null_fgetc;
    newconn->seek = &null_seek;
    newconn->truncate = &null_truncate;
    newconn->fflush = &null_fflush;
    newconn->read = &null_read;
    newconn->write = &null_write;
    newconn->nPushBack = 0;
    newconn->save = newconn->save2 = -1000;
    newconn->connprivate = nullptr;
    newconn->inconv = newconn->outconv = nullptr;
    newconn->UTF8out = FALSE;
    newconn->buff = nullptr;
    newconn->buff_pos = newconn->buff_stored_len = newconn->buff_len = 0;
    /* increment id, avoid NULL */
    current_id = (void *)((size_t) current_id+1);
    if(!current_id) current_id = (void *) 1;
    newconn->id = current_id;
    newconn->ex_ptr = nullptr;
    newconn->status = NA_INTEGER;
}

/* ------------------- file connections --------------------- */

#ifdef _WIN32
#define f_seek fseeko64
#define f_tell ftello64
#define OFF_T off64_t
#elif defined(HAVE_OFF_T) && defined(HAVE_FSEEKO)
#define f_seek fseeko
#define f_tell ftello
#define OFF_T off_t
#else
#define f_seek fseek
#define f_tell ftell
#define OFF_T long
#endif

#ifdef _WIN32
size_t R::utf8towcs(wchar_t *wc, const char *s, size_t n);
#endif

typedef struct fileconn
{
    FILE *fp;
    OFF_T rpos, wpos;
    Rboolean last_was_write;
    Rboolean raw;
#ifdef _WIN32
    Rboolean anon_file;
    Rboolean use_fgetwc;
    Rboolean have_wcbuffered;
    char wcbuf;
    char name[PATH_MAX + 1];
#endif
} * Rfileconn;

static Rboolean shouldBuffer(int fd)
{
#ifdef HAVE_SYS_STAT_H
    struct stat sb;
    int err = fstat(fd, &sb);
    return err ? FALSE : (Rboolean)S_ISREG(sb.st_mode);
#else
    return FALSE;
#endif
}

/* returns FALSE on error */
static Rboolean isDir(FILE *fd)
{
#ifdef HAVE_SYS_STAT_H
    struct stat sb;
    int err = fstat(fileno(fd), &sb);
    return err ? FALSE : (Rboolean) S_ISDIR(sb.st_mode);
#else
    return FALSE;
#endif
}

/* returns FALSE on error */
static Rboolean isDirPath(const char *path)
{
#ifdef HAVE_SYS_STAT_H
#ifdef Win32
    struct _stati64 sb;
    if (!_stati64(path, &sb) && (sb.st_mode & S_IFDIR))
        return TRUE;
#else
    struct stat sb;
    if (!stat(path, &sb) && S_ISDIR(sb.st_mode))
        return TRUE;
#endif
#endif
    return FALSE;
}

static Rboolean file_open(Rconnection con)
{
    const char *name;
    FILE *fp = nullptr;
    Rfileconn thisconn = (Rfileconn) con->connprivate;
    Rboolean temp = FALSE;
#ifdef HAVE_FCNTL
    int fd, flags;
#endif
    int mlen = (int) strlen(con->mode); // short

    if(strlen(con->description) == 0) {
	temp = TRUE;
	name = R_tmpnam("Rf", R_TempDir);
    } else name = R_ExpandFileName(con->description);
    errno = 0; /* some systems require this */
    if(strcmp(name, "stdin")) {
#ifdef _WIN32
	char mode[20]; /* 4 byte mode plus "t,ccs=UTF-16LE" plus one for luck. */
	strncpy(mode, con->mode, 4);
	mode[4] = '\0';
	if (!strpbrk(mode, "bt"))
	    strcat(mode, "t");
	// See PR#16737, https://docs.microsoft.com/en-us/cpp/c-runtime-library/reference/fopen-wfopen?view=vs-2019
	// ccs= is also supported by glibc but not macOS
	if (strchr(mode, 't')
	    && (streql(con->encname, "UTF-16LE") || streql(con->encname, "UCS-2LE"))) {
	    strcat(mode, ",ccs=UTF-16LE");
	    if (con->canread) {
	    	thisconn->use_fgetwc = TRUE;
	    	thisconn->have_wcbuffered = FALSE;
	    }
	}
	if(con->enc == CE_UTF8) {
	    int n = strlen(name);
	    wchar_t wname[2 * (n+1)], wmode[20];
	    mbstowcs(wmode, mode, 19);
	    R_CheckStack();
	    utf8towcs(wname, name, n+1);
	    fp = _wfopen(wname, wmode);
	    if(!fp) {
		warning(_("cannot open file '%ls': %s"), wname, strerror(errno));
		return FALSE;
	    }
	    if (isDir(fp)) {
		warning(_("cannot open file '%ls': it is a directory"), wname);
		fclose(fp);
		return FALSE;
	    }
	} else {
	    fp = R_fopen(name, mode);
	}
#else
	fp = R_fopen(name, con->mode);
#endif
    } else {  /* use file("stdin") to refer to the file and not the console */
#ifdef HAVE_FDOPEN
	int dstdin = dup(0);
# ifdef Win32
	if (strchr(con->mode, 'b'))
	    /* fdopen won't set dstdin to binary mode */
	    setmode(dstdin, _O_BINARY);
# endif
        fp = fdopen(dstdin, con->mode);
	con->canseek = FALSE;
#else
	warning(_("cannot open file '%s': %s"), name,
		_("'fdopen' is not supported on this platform"));
#endif
    }
    if(!fp) {
	warning(_("cannot open file '%s': %s"), name, strerror(errno));
	return FALSE;
    }
    if (isDir(fp)) {
	warning(_("cannot open file '%s': it is a directory"), name);
	fclose(fp);
	return FALSE;
    }
    if(temp) {
	/* This will fail on Windows, so arrange to remove in
	 * file_close.  An alternative strategy would be to manipulate
	 * the underlying file handle to add FILE_SHARE_DELETE (so the
	 * unlink is valid) or FILE_FLAG_DELETE_ON_CLOSE.  E.g. create
	 * via CreateFile, get an fd by _open_osfhandle and a file
	 * stream by fdopen.  See
	 * e.g. http://www.codeproject.com/KB/files/handles.aspx
	 */
	unlink(name);
#ifdef _WIN32
	strncpy(thisconn->name, name, PATH_MAX);
	thisconn->name[PATH_MAX - 1] = '\0';
#endif
	free((char *) name); /* only free if allocated by R_tmpnam */
    }
#ifdef _WIN32
    thisconn->anon_file = temp;
#endif
    thisconn->fp = fp;
    con->isopen = TRUE;
    con->canwrite = (Rboolean) (con->mode[0] == 'w' || con->mode[0] == 'a');
    con->canread = (Rboolean) !con->canwrite;
    if(mlen >= 2 && con->mode[1] == '+')
	con->canread = con->canwrite = TRUE;
    thisconn->last_was_write = (Rboolean) !con->canread;
    thisconn->rpos = 0;
    if(con->canwrite) thisconn->wpos = f_tell(fp);
    if(mlen >= 2 && con->mode[mlen-1] == 'b') con->text = FALSE;
    else con->text = TRUE;
    con->save = -1000;
    if (shouldBuffer(fileno(fp)))
	con->set_buffer();
    set_iconv(con);

#ifdef HAVE_FCNTL
    if(!con->blocking) {
	fd = fileno(fp);
	flags = fcntl(fd, F_GETFL);
	flags |= O_NONBLOCK;
	fcntl(fd, F_SETFL, flags);
    }
#endif
    return TRUE;
}

static void file_close(Rconnection con)
{
    Rfileconn thisconn = (Rfileconn)con->connprivate;
    if (con->isopen) // && strcmp(con->description, "stdin"))
        con->status = fclose(thisconn->fp);
    con->isopen = FALSE;
#ifdef _WIN32
    if (thisconn->anon_file)
        unlink(thisconn->name);
#endif
}

static int file_vfprintf(Rconnection con, const char *format, va_list ap)
{
    Rfileconn thisconn = (Rfileconn) con->connprivate;

    if(!thisconn->last_was_write) {
	thisconn->rpos = f_tell(thisconn->fp);
	thisconn->last_was_write = TRUE;
	f_seek(thisconn->fp, thisconn->wpos, SEEK_SET);
    }
    if(con->outconv) return dummy_vfprintf(con, format, ap);
    else return vfprintf(thisconn->fp, format, ap);
}

static int file_fgetc_internal(Rconnection con)
{
    Rfileconn thisconn = (Rfileconn) con->connprivate;
    FILE *fp = thisconn->fp;
    int c;

    if(thisconn->last_was_write) {
	thisconn->wpos = f_tell(thisconn->fp);
	thisconn->last_was_write = FALSE;
	f_seek(thisconn->fp, thisconn->rpos, SEEK_SET);
    }
#ifdef _WIN32
    if (thisconn->use_fgetwc) {
    	if (thisconn->have_wcbuffered) {
    	    c = thisconn->wcbuf;
    	    thisconn->have_wcbuffered = FALSE;
    	} else {
    	    wint_t wc = fgetwc(fp);
    	    c = (char) wc & 0xFF;
    	    thisconn->wcbuf = (char) wc >> 8;
    	    thisconn->have_wcbuffered = TRUE;
    	}
    } else
#endif
    c =fgetc(fp);
    return feof(fp) ? R_EOF : c;
}

static double file_seek(Rconnection con, double where, int origin, int rw)
{
    Rfileconn thisconn = (Rfileconn) con->connprivate;
    FILE *fp = thisconn->fp;
    OFF_T pos;
    int whence = SEEK_SET;

    /* make sure both positions are set */
    pos = f_tell(fp);
    if(thisconn->last_was_write) thisconn->wpos = pos; else thisconn->rpos = pos;
    if(rw == 1) {
	if(!con->canread) error(_("connection is not open for reading"));
	pos = thisconn->rpos;
	thisconn->last_was_write = FALSE;
    }
    if(rw == 2) {
	if(!con->canwrite) error(_("connection is not open for writing"));
	pos = thisconn->wpos;
	thisconn->last_was_write = TRUE;
    }
    if(ISNA(where)) return (double) pos;

    switch(origin) {
    case 2: whence = SEEK_CUR; break;
    case 3: whence = SEEK_END;
//#ifdef _WIN32
	    /* work around a bug in MinGW runtime 3.8 fseeko64, PR#7896
	       seems no longer to be needed */
//	    if(con->canwrite) fflush(fp);
//#endif
	    break;
    default: whence = SEEK_SET;
    }
    f_seek(fp, (OFF_T) where, whence);
    if(thisconn->last_was_write) thisconn->wpos = f_tell(thisconn->fp);
    else thisconn->rpos = f_tell(thisconn->fp);
    return (double) pos;
}

static void file_truncate(Rconnection con)
{
    Rfileconn thisconn = (Rfileconn) con->connprivate;
#ifdef HAVE_FTRUNCATE
    FILE *fp = thisconn->fp;
    int fd = fileno(fp);
/* ftruncate64 is in Mingw-64 trunk, but not in current toolkit */
# ifdef W64_to_come
    off64_t size = lseek64(fd, 0, SEEK_CUR);
# else
    OFF_T size = lseek(fd, 0, SEEK_CUR);
# endif
#endif

    if(!con->isopen || !con->canwrite)
	error(_("can only truncate connections open for writing"));

    if(!thisconn->last_was_write) thisconn->rpos = f_tell(thisconn->fp);
#ifdef W64_to_come
    if(ftruncate64(fd, size)) error(_("file truncation failed"));
#elif defined(HAVE_FTRUNCATE)
    if(ftruncate(fd, size)) error(_("file truncation failed"));
#else
    error(_("file truncation is unavailable on this platform"));
#endif
    thisconn->last_was_write = TRUE;
    thisconn->wpos = f_tell(thisconn->fp);
}

static int file_fflush(Rconnection con)
{
    FILE *fp = ((Rfileconn) (con->connprivate))->fp;

    return fflush(fp);
}

static size_t file_read(void *ptr, size_t size, size_t nitems,
			Rconnection con)
{
    Rfileconn thisconn = (Rfileconn) con->connprivate;
    FILE *fp = thisconn->fp;

    if(thisconn->last_was_write) {
	thisconn->wpos = f_tell(thisconn->fp);
	thisconn->last_was_write = FALSE;
	f_seek(thisconn->fp, thisconn->rpos, SEEK_SET);
    }
    return fread(ptr, size, nitems, fp);
}

static size_t file_write(const void *ptr, size_t size, size_t nitems,
			 Rconnection con)
{
    Rfileconn thisconn = (Rfileconn) con->connprivate;
    FILE *fp = thisconn->fp;

    if(!thisconn->last_was_write) {
	thisconn->rpos = f_tell(thisconn->fp);
	thisconn->last_was_write = TRUE;
	f_seek(thisconn->fp, thisconn->wpos, SEEK_SET);
    }
    return fwrite(ptr, size, nitems, fp);
}

static Rconnection newfile(const char *description, int enc, const char *mode,
			   int raw)
{
    Rconnection newconn;
    newconn = (Rconnection) malloc(sizeof(struct Rconn));
    if(!newconn) error(_("allocation of file connection failed"));
    newconn->connclass = (char *) malloc(strlen("file") + 1);
    if(!newconn->connclass) {
	free(newconn);
	error(_("allocation of file connection failed"));
	/* for Solaris 12.5 */ newconn = nullptr;
    }
    strcpy(newconn->connclass, "file");
    newconn->description = (char *) malloc(strlen(description) + 1);
    if(!newconn->description) {
	free(newconn->connclass); free(newconn);
	error(_("allocation of file connection failed"));
	/* for Solaris 12.5 */ newconn = nullptr;
    }
    init_con(newconn, description, enc, mode);
    newconn->open = &file_open;
    newconn->close = &file_close;
    newconn->vfprintf = &file_vfprintf;
    newconn->fgetc_internal = &file_fgetc_internal;
    newconn->fgetc = &dummy_fgetc;
    newconn->seek = &file_seek;
    newconn->truncate = &file_truncate;
    newconn->fflush = &file_fflush;
    newconn->read = &file_read;
    newconn->write = &file_write;
    newconn->canseek = (Rboolean) (raw == 0);
    newconn->connprivate = (void *) malloc(sizeof(struct fileconn));
    if(!newconn->connprivate) {
	free(newconn->description); free(newconn->connclass); free(newconn);
	error(_("allocation of file connection failed"));
	/* for Solaris 12.5 */ newconn = nullptr;
    }
    ((Rfileconn) (newconn->connprivate))->raw = (Rboolean) raw;
#ifdef Win32
    ((Rfileconn) (newconn->connprivate))->use_fgetwc = FALSE;
#endif
    return newconn;
}

/* file() is now implemented as an op of do_url */


/* ------------------- fifo connections --------------------- */

#if defined(HAVE_MKFIFO) && defined(HAVE_FCNTL_H)

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#include <cerrno>

typedef struct fifoconn {
    int fd;
} *Rfifoconn;


static Rboolean fifo_open(Rconnection con)
{
    const char *name;
    Rfifoconn thisconn = (Rfifoconn) con->connprivate;
    int fd, flags, res;
    int mlen = (int) strlen(con->mode); // short
    struct stat sb;
    Rboolean temp = FALSE;

    if(strlen(con->description) == 0) {
	temp = TRUE;
	name = R_tmpnam("Rf", R_TempDir);
    } else name = R_ExpandFileName(con->description);
    con->canwrite = (Rboolean) (con->mode[0] == 'w' || con->mode[0] == 'a');
    con->canread = (Rboolean) !con->canwrite;
    if(mlen >= 2 && con->mode[1] == '+') con->canread = TRUE;

    /* if we are to write, create the fifo if needed */
    if(con->canwrite) {
	res = stat(name, &sb);
	if(res) { /* error, does not exist? */
	    errno = 0;
	    res = mkfifo(name, 00644);
	    if(res) {
		warning(_("cannot create fifo connection '%s', reason '%s'"), name, strerror(errno));
	    }
	    if(res) return FALSE;
	} else {
	    if(!(sb.st_mode & S_IFIFO)) {
		warning(_("'%s' exists but is not a fifo connection"), name);
		return FALSE;
	    }
	}
    }

    if(con->canread && con->canwrite) flags = O_RDWR;
    else if(con->canread) flags = O_RDONLY;
    else flags = O_WRONLY;
    if(!con->blocking) flags |= O_NONBLOCK;
    if(con->mode[0] == 'a') flags |= O_APPEND;
    errno = 0; /* precaution */
    fd = open(name, flags);
    if(fd < 0) {
	if(errno == ENXIO) warning(_("fifo connection '%s' is not ready"), name);
	else warning(_("cannot open fifo connection '%s'"), name);
	return FALSE;
    }
    if(temp) {
	unlink(name);
	free((char *) name); /* allocated by R_tmpnam */
    }

    thisconn->fd = fd;
    con->isopen = TRUE;

    if(mlen >= 2 && con->mode[mlen-1] == 'b') con->text = FALSE;
    else con->text = TRUE;
    set_iconv(con);
    con->save = -1000;
    return TRUE;
}

static void fifo_close(Rconnection con)
{
    con->status = close(((Rfifoconn) (con->connprivate))->fd);
    con->isopen = FALSE;
}

static int fifo_fgetc_internal(Rconnection con)
{
    Rfifoconn thisconn = (Rfifoconn) con->connprivate;
    unsigned char c;
    ssize_t n;

    n = read(thisconn->fd, (char *) &c, 1);
    return (n == 1) ? c : R_EOF;
}

static size_t fifo_read(void *ptr, size_t size, size_t nitems,
			Rconnection con)
{
    Rfifoconn thisconn = (Rfifoconn) con->connprivate;

    /* uses 'size_t' for len */
    if ((double) size * (double) nitems > (double) SSIZE_MAX)
	error(_("specified block is too large"));
    return read(thisconn->fd, ptr, size * nitems)/size;
}

static size_t fifo_write(const void *ptr, size_t size, size_t nitems,
			 Rconnection con)
{
    Rfifoconn thisconn = (Rfifoconn) con->connprivate;

    /* uses 'size_t' for len */
    if ((double) size * (double) nitems > (double) SSIZE_MAX)
      error(_("specified block is too large"));
    return write(thisconn->fd, ptr, size * nitems)/size;
}

#elif defined(_WIN32)  // ----- Windows part ------

// PR#15600, based on https://github.com/0xbaadf00d/r-project_win_fifo
#define WIN32_LEAN_AND_MEAN 1
#include <windows.h>
#include <cwchar>

/* Microsoft addition, not supported in Win XP
errno_t strcat_s(char *strDestination, size_t numberOfElements,
		 const char *strSource);
*/

typedef struct fifoconn
{
    HANDLE hdl_namedpipe;
    LPOVERLAPPED overlapped_write;
} *Rfifoconn;

static char* win_getlasterror_str(void)
{
    LPVOID lpv_tempmsg = nullptr;
    unsigned int err_msg_len;
    char *err_msg = nullptr;

    err_msg_len =
	FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER |
		      FORMAT_MESSAGE_FROM_SYSTEM |
		      FORMAT_MESSAGE_IGNORE_INSERTS, nullptr, GetLastError(),
		      MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
		      (LPTSTR)&lpv_tempmsg, 0, nullptr);
    err_msg = (char*) malloc(err_msg_len);
    if (!err_msg) return nullptr;
    ZeroMemory(err_msg, err_msg_len);
    strncpy(err_msg, (LPTSTR)lpv_tempmsg, err_msg_len - sizeof(wchar_t));
    LocalFree(lpv_tempmsg);
    return err_msg;
}

static Rboolean	fifo_open(Rconnection con)
{
    Rfifoconn thisconn = con->connprivate;
    unsigned int uin_pipname_len = strlen(con->description);
    unsigned int uin_mode_len = strlen(con->mode);
    char *hch_pipename = nullptr;
    const char *hch_tempname = nullptr;
    Rboolean boo_retvalue = TRUE;

    /* Prepare FIFO filename */
    if (!uin_pipname_len) {
	hch_pipename = R_tmpnam("fifo", "\\\\.\\pipe\\");
    } else {
	if (strncmp("\\\\.\\pipe\\", con->description, 9) != 0) {
	    uin_pipname_len += strlen("\\\\.\\pipe\\") + 1;
	    hch_pipename = (char*) malloc(uin_pipname_len);
	    if (!hch_pipename) error(_("allocation of fifo name failed"));
	    ZeroMemory(hch_pipename, uin_pipname_len);
/*	    strcpy_s(hch_pipename, uin_pipname_len, "\\\\.\\pipe\\");  Win XP doesn't support this */
	    strcpy(hch_pipename, "\\\\.\\pipe\\");
	} else {
	    hch_pipename = (char*)malloc(uin_pipname_len);
	    if (!hch_pipename) error(_("allocation of fifo name failed"));
	    ZeroMemory(hch_pipename, uin_pipname_len);
	}
	hch_tempname = R_ExpandFileName(con->description);
/*	strcat_s(hch_pipename, uin_pipname_len, hch_tempname);  Win XP doesn't support this */
	strcat(hch_pipename, hch_tempname);
    }

    /* Prepare FIFO open mode */
    con->canwrite = (Rboolean) (con->mode[0] == 'w' || con->mode[0] == 'a');
    con->canread = (Rboolean) !con->canwrite;
    if (uin_mode_len >= 2 && con->mode[1] == '+') con->canread = TRUE;

    /*
    ** FIFO using Windows API -> CreateNamedPipe() OR CreateFile()
    ** http://msdn.microsoft.com/en-us/library/windows/desktop/aa363858(v=vs.85).aspx
    ** http://msdn.microsoft.com/en-us/library/windows/desktop/aa365150(v=vs.85).aspx
    */
    thisconn->hdl_namedpipe = nullptr;
    thisconn->overlapped_write = CreateEventA(nullptr, TRUE, TRUE, nullptr);
    if (con->canwrite) {
	SECURITY_ATTRIBUTES win_namedpipe_secattr = {0};
	win_namedpipe_secattr.nLength = sizeof(SECURITY_ATTRIBUTES);
	win_namedpipe_secattr.lpSecurityDescriptor = nullptr;
	win_namedpipe_secattr.bInheritHandle = FALSE;

	thisconn->hdl_namedpipe =
	    CreateNamedPipeA(hch_pipename,
			     (con->canread ? PIPE_ACCESS_DUPLEX :
			      PIPE_ACCESS_OUTBOUND) | FILE_FLAG_OVERLAPPED,
			     PIPE_TYPE_BYTE, PIPE_UNLIMITED_INSTANCES , 0, 0,
			     FILE_FLAG_NO_BUFFERING, &win_namedpipe_secattr);
	if (thisconn->hdl_namedpipe == INVALID_HANDLE_VALUE) {
	    /*
	    ** If GetLastError() return 231 (All pipe instances are busy) == File
	    ** already exist on Unix/Linux...
	    */
	    if (GetLastError() != 231) {
		char *hch_err_msg = win_getlasterror_str();
		warning(_("cannot create fifo '%s', reason '%s'"), hch_pipename, hch_err_msg);
		free(hch_err_msg);
		boo_retvalue = FALSE;
	    }
	}
    }

    /* Open existing named pipe */
    if ((boo_retvalue || GetLastError() == 231) &&
	thisconn->hdl_namedpipe <= (HANDLE)(LONG_PTR) 0) {
	DWORD dwo_openmode = 0;
	if (con->canread) dwo_openmode |= GENERIC_READ;
	if (con->canwrite) dwo_openmode |= GENERIC_WRITE;
	thisconn->hdl_namedpipe =
	    CreateFileA(hch_pipename, dwo_openmode,
			FILE_SHARE_READ | FILE_SHARE_WRITE,
			nullptr, OPEN_EXISTING,
			FILE_ATTRIBUTE_NORMAL | FILE_FLAG_OVERLAPPED,
			nullptr);
	if (thisconn->hdl_namedpipe == INVALID_HANDLE_VALUE) {
	    char *hch_err_msg = win_getlasterror_str();
	    warning(_("cannot open fifo '%s', reason '%s'"), hch_pipename, hch_err_msg);
	    free(hch_err_msg);
	    boo_retvalue = FALSE;
	}
    }

    /* Free malloc-ed variables */
    free(hch_pipename);
    if (hch_tempname) free((void*) hch_tempname);

    /* Finalize FIFO configuration (only if FIFO is opened/created) */
    if (boo_retvalue && thisconn->hdl_namedpipe) {
	con->isopen = TRUE;
	con->text = uin_mode_len >= 2 && con->mode[uin_mode_len - 1] == 'b';
	set_iconv(con);
	con->save = -1000;
    }

    /* Done */
    return boo_retvalue;
}

static void fifo_close(Rconnection con)
{
    Rfifoconn thisconn = con->connprivate;
    con->isopen = FALSE;
    con->status = CloseHandle(thisconn->hdl_namedpipe) ? 0 : -1;
    if (thisconn->overlapped_write) CloseHandle(thisconn->overlapped_write);
}

static size_t fifo_read(void* ptr, size_t size, size_t nitems, Rconnection con)
{
    Rfifoconn thisconn = con->connprivate;
    size_t read_byte = 0;

    // avoid integer overflow
    if ((double)size * sizeof(wchar_t) * nitems > UINT_MAX)
	error(_("specified block is too large"));

    wchar_t *buffer = (wchar_t*)malloc((size * sizeof(wchar_t)) * nitems);
    if (!buffer) error(_("allocation of fifo buffer failed"));
    ReadFile(thisconn->hdl_namedpipe, buffer,
	     (size * sizeof(wchar_t)) * nitems, (LPDWORD)&read_byte,
	     thisconn->overlapped_write);
    wcstombs(ptr, buffer, read_byte / sizeof(wchar_t));
    free(buffer);
    return (read_byte / sizeof(wchar_t)) / size;
}

static size_t fifo_write(const void *ptr, size_t size, size_t nitems, Rconnection con)
{
    Rfifoconn thisconn = con->connprivate;
    size_t written_bytes = 0;

    if (size * sizeof(wchar_t) * nitems > UINT_MAX)
	error(_("specified block is too large"));

    /* Wait for a client process to connect */
    ConnectNamedPipe(thisconn->hdl_namedpipe, nullptr);

    /* Convert char* to wchar_t* */
    int str_len = size * nitems;
    wchar_t *buffer = malloc((str_len + 1) * sizeof(wchar_t));
    if (!buffer) error(_("allocation of fifo buffer failed"));
    mbstowcs(buffer, (const char*) ptr, str_len);

    /* Write data */
    if (WriteFile(thisconn->hdl_namedpipe, buffer,
		  size * sizeof(wchar_t) * nitems, (LPDWORD) &written_bytes,
		  nullptr) == FALSE && GetLastError() != ERROR_IO_PENDING) {
	char *hch_err_msg = win_getlasterror_str();
	warning(_("cannot write FIFO '%s'"), hch_err_msg);
	free(hch_err_msg);
    }

    /* Free data malloc-ed by windows_towchar */
    free(buffer);

    /* Done */
    return written_bytes / nitems;
}

static int fifo_fgetc_internal(Rconnection con)
{
    Rfifoconn  thisconn = con->connprivate;
    DWORD available_bytes = 0;
    DWORD read_byte = 0;
    DWORD len = 1 * sizeof(wchar_t);
    wchar_t c;

    /* Check available bytes on named pipe */
    PeekNamedPipe(thisconn->hdl_namedpipe, nullptr, 0, nullptr, &available_bytes, nullptr);

    /* Read char if available bytes > 0, otherwize, return R_EOF */
    if (available_bytes > 0) {
	ReadFile(thisconn->hdl_namedpipe, &c, len, &read_byte, nullptr);
	return (read_byte == len) ? (char) c : R_EOF;
    }
    return R_EOF;
}

#endif // WIN32

static Rconnection newfifo(const char *description, const char *mode)
{
    Rconnection newconn;
    newconn = (Rconnection) malloc(sizeof(struct Rconn));
    if(!newconn) error(_("allocation of fifo connection failed"));
    newconn->connclass = (char *) malloc(strlen("fifo") + 1);
    if(!newconn->connclass) {
	free(newconn);
	error(_("allocation of fifo connection failed"));
	/* for Solaris 12.5 */ newconn = nullptr;
    }
    strcpy(newconn->connclass, "fifo");
    newconn->description = (char *) malloc(strlen(description) + 1);
    if(!newconn->description) {
	free(newconn->connclass); free(newconn);
	error(_("allocation of fifo connection failed"));
	/* for Solaris 12.5 */ newconn = nullptr;
    }
    init_con(newconn, description, CE_NATIVE, mode);
    newconn->open = &fifo_open;
    newconn->close = &fifo_close;
    newconn->vfprintf = &dummy_vfprintf;
    newconn->fgetc_internal = &fifo_fgetc_internal;
    newconn->fgetc = &dummy_fgetc;
    newconn->seek = &null_seek;
    newconn->truncate = &null_truncate;
    newconn->fflush = &null_fflush;
    newconn->read = &fifo_read;
    newconn->write = &fifo_write;
    newconn->connprivate = (void *) malloc(sizeof(struct fifoconn));
    if(!newconn->connprivate) {
	free(newconn->description); free(newconn->connclass); free(newconn);
	error(_("allocation of fifo connection failed"));
	/* for Solaris 12.5 */ newconn = nullptr;
    }
    return newconn;
}

HIDDEN SEXP do_fifo(SEXP call, SEXP op, SEXP args, SEXP env)
{
#if (defined(HAVE_MKFIFO) && defined(HAVE_FCNTL_H)) || defined(_WIN32)
    SEXP sfile, sopen, ans, classs, enc;
    const char *file, *open;
    int ncon, block;
    Rconnection con = nullptr;

    checkArity(op, args);
    sfile = CAR(args);
    if(!isString(sfile) || LENGTH(sfile) != 1 ||
       STRING_ELT(sfile, 0) == NA_STRING)
	error(_("invalid '%s' argument"), "description");
    if(length(sfile) > 1)
	warning(_("only first element of '%s' argument will be used"), "description");
    file = translateCharFP(STRING_ELT(sfile, 0)); /* for now, like fopen */
    sopen = CADR(args);
    if(!isString(sopen) || LENGTH(sopen) != 1)
	error(_("invalid '%s' argument"), "open");
    block = asLogical(CADDR(args));
    if(block == NA_LOGICAL)
	error(_("invalid '%s' argument"), "block");
    enc = CADDDR(args);
    if(!isString(enc) || LENGTH(enc) != 1 ||
       strlen(CHAR(STRING_ELT(enc, 0))) > 100) /* ASCII */
	error(_("invalid '%s' argument"), "encoding");
    open = CHAR(STRING_ELT(sopen, 0)); /* ASCII */
    if(strlen(file) == 0) {
	if(!strlen(open)) open ="w+";
	if(strcmp(open, "w+") != 0 && strcmp(open, "w+b") != 0) {
	    open ="w+";
	    warning(_("'%s' only supports 'open = \"w+\"' and 'open = \"w+b\"': using the former"), "fifo(\"\")");
	}
    }
    ncon = Rconn::NextConnection();
    con = Connections[ncon] = newfifo(file, strlen(open) ? open : (char *) "r");
    con->blocking = (Rboolean) block;
    strncpy(con->encname, CHAR(STRING_ELT(enc, 0)), 100); /* ASCII */
    con->encname[100 - 1] = '\0';
    con->ex_ptr = PROTECT(R_MakeExternalPtr(con->id, install("connection"), R_NilValue));

    /* open it if desired */
    if(strlen(open)) {
	Rboolean success = con->open(con);
	if(!success) {
	    con_destroy(ncon);
	    error(_("cannot open the connection"));
	}
    }

    PROTECT(ans = ScalarInteger(ncon));
    PROTECT(classs = allocVector(STRSXP, 2));
    SET_STRING_ELT(classs, 0, mkChar("fifo"));
    SET_STRING_ELT(classs, 1, mkChar("connection"));
    classgets(ans, classs);
    setAttrib(ans, R_ConnIdSymbol, (SEXP) con->ex_ptr);
    R_RegisterCFinalizerEx((SEXP) con->ex_ptr, conFinalizer, FALSE);
    UNPROTECT(3);

    return ans;
#else
    error(_("fifo connections are not available on this system"));
    return R_NilValue;		/* -Wall */
#endif
}

/* ------------------- pipe connections --------------------- */

static Rboolean pipe_open(Rconnection con)
{
    FILE *fp;
    char mode[3];
    Rfileconn thisconn = (Rfileconn) con->connprivate;
    int mlen;

#ifdef _WIN32
    strncpy(mode, con->mode, 2);
    mode[2] = '\0';
#else
    mode[0] = con->mode[0];
    mode[1] = '\0';
#endif
    errno = 0;
#ifdef _WIN32
    if(con->enc == CE_UTF8) {
	int n = strlen(con->description);
	wchar_t wname[2 * (n+1)], wmode[10];
	R_CheckStack();
	utf8towcs(wname, con->description, n+1);
	mbstowcs(wmode, con->mode, 10);
	fp = _wpopen(wname, wmode);
	if(!fp) {
	    warning(_("cannot open 'pipe()' command '%ls': %s"), wname, strerror(errno));
	    return FALSE;
	}
    } else
#endif
	fp = R_popen(con->description, mode);
    if(!fp) {
	warning(_("cannot open 'pipe()' command '%s': %s"), con->description, strerror(errno));
	return FALSE;
    }
    thisconn->fp = fp;
    con->isopen = TRUE;
    con->canwrite = (Rboolean) (con->mode[0] == 'w');
    con->canread = (Rboolean) !con->canwrite;
    mlen = (int) strlen(con->mode);
    if(mlen >= 2 && con->mode[mlen - 1] == 'b') con->text = FALSE;
    else con->text = TRUE;
    thisconn->last_was_write = (Rboolean) !con->canread;
    thisconn->rpos = thisconn->wpos = 0;
    set_iconv(con);
    con->save = -1000;
    return TRUE;
}

static void pipe_close(Rconnection con)
{
    con->status = pclose(((Rfileconn) (con->connprivate))->fp);
    con->isopen = FALSE;
}

static Rconnection newpipe(const char *description, int ienc, const char *mode)
{
    Rconnection newconn;
    newconn = (Rconnection) malloc(sizeof(struct Rconn));
    if(!newconn) error(_("allocation of pipe connection failed"));
    newconn->connclass = (char *) malloc(strlen("pipe") + 1);
    if(!newconn->connclass) {
	free(newconn);
	error(_("allocation of pipe connection failed"));
	/* for Solaris 12.5 */ newconn = nullptr;
    }
    strcpy(newconn->connclass, "pipe");
    newconn->description = (char *) malloc(strlen(description) + 1);
    if(!newconn->description) {
	free(newconn->connclass); free(newconn);
	error(_("allocation of pipe connection failed"));
	/* for Solaris 12.5 */ newconn = nullptr;
    }
    init_con(newconn, description, ienc, mode);
    newconn->open = &pipe_open;
    newconn->close = &pipe_close;
    newconn->vfprintf = &file_vfprintf;
    newconn->fgetc_internal = &file_fgetc_internal;
    newconn->fgetc = &dummy_fgetc;
    newconn->fflush = &file_fflush;
    newconn->read = &file_read;
    newconn->write = &file_write;
    newconn->connprivate = (void *) malloc(sizeof(struct fileconn));
    if(!newconn->connprivate) {
	free(newconn->description); free(newconn->connclass); free(newconn);
	error(_("allocation of pipe connection failed"));
	/* for Solaris 12.5 */ newconn = nullptr;
    }
    return newconn;
}

#ifdef _WIN32
extern Rconnection
newWpipe(const char *description, int enc, const char *mode);
#endif

HIDDEN SEXP do_pipe(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP scmd, sopen, ans, classs, enc;
    const char *file, *open;
    int ncon;
    cetype_t ienc = CE_NATIVE;
    Rconnection con = nullptr;

    checkArity(op, args);
    scmd = CAR(args);
    if(!isString(scmd) || LENGTH(scmd) != 1 ||
       STRING_ELT(scmd, 0) == NA_STRING)
	error(_("invalid '%s' argument"), "description");
    if(LENGTH(scmd) > 1)
	warning(_("only first element of '%s' argument will be used"), "description");
#ifdef _WIN32
    if( !IS_ASCII(STRING_ELT(scmd, 0)) ) {
	ienc = CE_UTF8;
	file = trCharUTF8(STRING_ELT(scmd, 0));
    } else {
	ienc = CE_NATIVE;
	file = translateCharFP(STRING_ELT(scmd, 0));
    }
#else
    file = translateCharFP(STRING_ELT(scmd, 0));
#endif
    sopen = CADR(args);
    if(!isString(sopen) || LENGTH(sopen) != 1)
	error(_("invalid '%s' argument"), "open");
    open = CHAR(STRING_ELT(sopen, 0)); /* ASCII */
    enc = CADDR(args);
    if(!isString(enc) || LENGTH(enc) != 1 ||
       strlen(CHAR(STRING_ELT(enc, 0))) > 100) /* ASCII */
	error(_("invalid '%s' argument"), "encoding");

    ncon = Rconn::NextConnection();
#ifdef _WIN32
    if(CharacterMode != RTerm)
	con = newWpipe(file, ienc, strlen(open) ? open : "r");
    else
#endif
	con = newpipe(file, ienc, strlen(open) ? open : "r");
    Connections[ncon] = con;
    strncpy(con->encname, CHAR(STRING_ELT(enc, 0)), 100); /* ASCII */
    con->encname[100 - 1] = '\0';
    con->ex_ptr = PROTECT(R_MakeExternalPtr(con->id, install("connection"), R_NilValue));

    /* open it if desired */
    if(strlen(open)) {
	Rboolean success = con->open(con);
	if(!success) {
	    con_destroy(ncon);
	    error(_("cannot open the connection"));
	}
    }

    PROTECT(ans = ScalarInteger(ncon));
    PROTECT(classs = allocVector(STRSXP, 2));
    SET_STRING_ELT(classs, 0, mkChar("pipe"));
#ifdef _WIN32
    if(CharacterMode != RTerm)
	SET_STRING_ELT(classs, 0, mkChar("pipeWin32"));
#endif
    SET_STRING_ELT(classs, 1, mkChar("connection"));
    classgets(ans, classs);
    setAttrib(ans, R_ConnIdSymbol, (SEXP) con->ex_ptr);
    R_RegisterCFinalizerEx((SEXP) con->ex_ptr, conFinalizer, FALSE);
    UNPROTECT(3);

    return ans;
}

/* ------------------- [bgx]zipped file connections --------------------- */

#include "gzio.h"

/* needs to be declared before con_close1 */
typedef struct gzconn {
    Rconnection con;
    int cp; /* compression level */
    z_stream s;
    int z_err, z_eof;
    uLong crc;
    Byte buffer[Z_BUFSIZE];
    int nsaved;
    char saved[2];
    Rboolean allow;
} *Rgzconn;


typedef struct gzfileconn {
    void *fp;
    int compress;
} *Rgzfileconn;

static Rboolean gzfile_open(Rconnection con)
{
    gzFile fp;
    char mode[6];
    Rgzfileconn gzcon = (Rgzfileconn) con->connprivate;
    const char *name;

    strcpy(mode, con->mode);
    /* Must open as binary */
    if(strchr(con->mode, 'w')) snprintf(mode, 6, "wb%1d", gzcon->compress);
    else if (con->mode[0] == 'a') snprintf(mode, 6, "ab%1d", gzcon->compress);
    else strcpy(mode, "rb");
    errno = 0; /* precaution */
    name = R_ExpandFileName(con->description);
    /* We cannot use isDir, because we cannot get the fd from gzFile
       (it would be possible with gzdopen, if supported) */
    if (isDirPath(name)) {
	warning(_("cannot open file '%s': it is a directory"), name);
	return FALSE;
    }
    fp = R_gzopen(name, mode);
    if(!fp) {
	warning(_("cannot open compressed file '%s', probable reason '%s'"),
	        name, strerror(errno));
	return FALSE;
    }
    ((Rgzfileconn) (con->connprivate))->fp = fp;
    con->isopen = TRUE;
    con->canwrite = (Rboolean) (con->mode[0] == 'w' || con->mode[0] == 'a');
    con->canread = (Rboolean) !con->canwrite;
    con->text = strchr(con->mode, 'b') ? FALSE : TRUE;
    con->set_buffer();
    set_iconv(con);
    con->save = -1000;
    return TRUE;
}

static void gzfile_close(Rconnection con)
{
    R_gzclose((gzFile) ((Rgzfileconn) (con->connprivate))->fp);
    con->isopen = FALSE;
}

static int gzfile_fgetc_internal(Rconnection con)
{
    gzFile fp = (gzFile) ((Rgzfileconn) (con->connprivate))->fp;
    unsigned char c;

    return R_gzread(fp, &c, 1) == 1 ? c : R_EOF;
}

/* This can only seek forwards when writing (when it writes nul bytes).
   When reading, it either seeks forwards or rewinds and reads again */
static double gzfile_seek(Rconnection con, double where, int origin, int rw)
{
    gzFile  fp = (gzFile) ((Rgzfileconn) (con->connprivate))->fp;
    Rz_off_t pos = R_gztell(fp);
    int res, whence = SEEK_SET;

    if (ISNA(where)) return (double) pos;

    switch(origin) {
    case 2: whence = SEEK_CUR; break;
    case 3: error(_("'whence = \"end\"' option is not implemented for gzfile connections"));
    default: whence = SEEK_SET;
    }
    res = R_gzseek(fp, (z_off_t) where, whence);
    if(res == -1)
	warning(_("seek on a gzfile connection returned an internal error"));
    return (double) pos;
}

static int gzfile_fflush(Rconnection con)
{
    return 0;
}

static size_t gzfile_read(void *ptr, size_t size, size_t nitems,
			Rconnection con)
{
    gzFile fp = (gzFile) ((Rgzfileconn) (con->connprivate))->fp;
    /* uses 'unsigned' for len */
    if ((double) size * (double) nitems > UINT_MAX)
	error(_("specified block is too large"));
    return R_gzread(fp, ptr, (unsigned int) (size*nitems))/size;
}

static size_t gzfile_write(const void *ptr, size_t size, size_t nitems,
			   Rconnection con)
{
    gzFile fp = (gzFile) ((Rgzfileconn) (con->connprivate))->fp;
    /* uses 'unsigned' for len */
    if ((double) size * (double) nitems > UINT_MAX)
	error(_("specified block is too large"));
    return R_gzwrite(fp, (voidp) ptr, (unsigned int) (size*nitems))/size;
}

static Rconnection newgzfile(const char *description, const char *mode,
			     int compress)
{
    Rconnection newconn;
    newconn = (Rconnection) malloc(sizeof(struct Rconn));
    if(!newconn) error(_("allocation of gzfile connection failed"));
    newconn->connclass = (char *) malloc(strlen("gzfile") + 1);
    if(!newconn->connclass) {
	free(newconn);
	error(_("allocation of gzfile connection failed"));
	/* for Solaris 12.5 */ newconn = nullptr;
    }
    strcpy(newconn->connclass, "gzfile");
    newconn->description = (char *) malloc(strlen(description) + 1);
    if(!newconn->description) {
	free(newconn->connclass); free(newconn);
	error(_("allocation of gzfile connection failed"));
	/* for Solaris 12.5 */ newconn = nullptr;
    }
    init_con(newconn, description, CE_NATIVE, mode);

    newconn->canseek = TRUE;
    newconn->open = &gzfile_open;
    newconn->close = &gzfile_close;
    newconn->vfprintf = &dummy_vfprintf;
    newconn->fgetc_internal = &gzfile_fgetc_internal;
    newconn->fgetc = &dummy_fgetc;
    newconn->seek = &gzfile_seek;
    newconn->fflush = &gzfile_fflush;
    newconn->read = &gzfile_read;
    newconn->write = &gzfile_write;
    newconn->connprivate = (void *) malloc(sizeof(struct gzfileconn));
    if(!newconn->connprivate) {
	free(newconn->description); free(newconn->connclass); free(newconn);
	error(_("allocation of gzfile connection failed"));
	/* for Solaris 12.5 */ newconn = nullptr;
    }
    ((Rgzfileconn) newconn->connprivate)->compress = compress;
    return newconn;
}

#include <bzlib.h>
typedef struct bzfileconn {
    FILE *fp;
    BZFILE *bfp;
    int compress;
} *Rbzfileconn;

static Rboolean bzfile_open(Rconnection con)
{
    Rbzfileconn bz = (Rbzfileconn) con->connprivate;
    FILE* fp;
    BZFILE* bfp;
    int bzerror;
    char mode[] = "rb";
    const char *name;

    con->canwrite = (Rboolean) (con->mode[0] == 'w' || con->mode[0] == 'a');
    con->canread = (Rboolean) !con->canwrite;
    /* regardless of the R view of the file, the file must be opened in
       binary mode where it matters */
    mode[0] = con->mode[0];
    errno = 0; /* precaution */
    name = R_ExpandFileName(con->description);
    fp = R_fopen(name, mode);
    if(!fp) {
	warning(_("cannot open bzip2-ed file '%s', probable reason '%s'"),
		name, strerror(errno));
	return FALSE;
    }
    if (isDir(fp)) {
	warning(_("cannot open file '%s': it is a directory"), name);
	fclose(fp);
	return FALSE;
    }
    if(con->canread) {
	bfp = BZ2_bzReadOpen(&bzerror, fp, 0, 0, nullptr, 0);
	if(bzerror != BZ_OK) {
	    BZ2_bzReadClose(&bzerror, bfp);
	    fclose(fp);
	    warning(_("file '%s' appears not to be compressed by bzip2"),
		    R_ExpandFileName(con->description));
	    return FALSE;
	}
    } else {
	bfp = BZ2_bzWriteOpen(&bzerror, fp, bz->compress, 0, 0);
	if(bzerror != BZ_OK) {
	    BZ2_bzWriteClose(&bzerror, bfp, 0, nullptr, nullptr);
	    fclose(fp);
	    warning(_("initializing bzip2 compression for file '%s' failed"),
		    R_ExpandFileName(con->description));
	    return FALSE;
	}
    }
    bz->fp = fp;
    bz->bfp = bfp;
    con->isopen = TRUE;
    con->text = strchr(con->mode, 'b') ? FALSE : TRUE;
    con->set_buffer();
    set_iconv(con);
    con->save = -1000;
    return TRUE;
}

static void bzfile_close(Rconnection con)
{
    int bzerror;
    Rbzfileconn bz = (Rbzfileconn) con->connprivate;

    if(con->canread)
	BZ2_bzReadClose(&bzerror, bz->bfp);
    else
	BZ2_bzWriteClose(&bzerror, bz->bfp, 0, nullptr, nullptr);
    fclose(bz->fp);
    con->isopen = FALSE;
}

static size_t bzfile_read(void *ptr, size_t size, size_t nitems,
			  Rconnection con)
{
    Rbzfileconn bz = (Rbzfileconn) con->connprivate;
    int nread = 0,  nleft;
    int bzerror;

    /* BZ2 uses 'int' for len */
    if ((double) size * (double) nitems > R_INT_MAX)
	error(_("specified block is too large"));

    nleft = (int)(size * nitems);
    /* we try to fill the buffer, because fgetc can interact with the stream boundaries
       resulting in truncated text streams while binary streams work fine */
    while (nleft > 0) {
	/* Need a cast as 'nread' needs to be interpreted in bytes */
	int n = BZ2_bzRead(&bzerror, bz->bfp, (char *) ptr + nread, nleft);
	if (bzerror == BZ_STREAM_END) { /* this could mean multiple streams so we need to check */
	    char *unused, *next_unused = nullptr;
	    int nUnused;
	    BZ2_bzReadGetUnused(&bzerror, bz->bfp, (void**) &unused, &nUnused);
	    if (bzerror == BZ_OK) {
		if (nUnused > 0) { /* unused bytes present - need to retain them */
		    /* given that this should be rare I don't want to add that overhead
		       to the entire bz structure so we allocate memory temporarily */
		    next_unused = (char*) malloc(nUnused);
		    if (!next_unused)
			error(_("allocation of overflow buffer for bzfile failed"));
		    memcpy(next_unused, unused, nUnused);
		}
		if (nUnused > 0 || !feof(bz->fp)) {
		    BZ2_bzReadClose(&bzerror, bz->bfp);
		    bz->bfp = BZ2_bzReadOpen(&bzerror, bz->fp, 0, 0, next_unused, nUnused);
		    if(bzerror != BZ_OK)
			warning(_("file '%s' has trailing content that appears not to be compressed by bzip2"),
				R_ExpandFileName(con->description));
		}
		if (next_unused) free(next_unused);
	    }
	} else if (bzerror != BZ_OK) {
	    /* bzlib docs say in this case n is invalid - but historically
	       we still used n in that case, so I keep it for now */
	    nread += n;
	    break;
	}
	nread += n;
	nleft -= n;
    }

    return nread / size;
}

static int bzfile_fgetc_internal(Rconnection con)
{
    char buf[1];
    size_t size;

    size = bzfile_read(buf, 1, 1, con);
    return (size < 1) ? R_EOF : (buf[0] % 256);
}

static size_t bzfile_write(const void *ptr, size_t size, size_t nitems,
			   Rconnection con)
{
    Rbzfileconn bz = (Rbzfileconn) con->connprivate;
    int bzerror;

    /* uses 'int' for len */
    if ((double) size * (double) nitems > R_INT_MAX)
	error(_("specified block is too large"));
    BZ2_bzWrite(&bzerror, bz->bfp, (voidp) ptr, (int) (size*nitems));
    if(bzerror != BZ_OK) return 0;
    else return nitems;
}

static Rconnection newbzfile(const char *description, const char *mode,
			     int compress)
{
    Rconnection newconn;
    newconn = (Rconnection) malloc(sizeof(struct Rconn));
    if(!newconn) error(_("allocation of bzfile connection failed"));
    newconn->connclass = (char *) malloc(strlen("bzfile") + 1);
    if(!newconn->connclass) {
	free(newconn);
	error(_("allocation of bzfile connection failed"));
	/* for Solaris 12.5 */ newconn = nullptr;
    }
    strcpy(newconn->connclass, "bzfile");
    newconn->description = (char *) malloc(strlen(description) + 1);
    if(!newconn->description) {
	free(newconn->connclass); free(newconn);
	error(_("allocation of bzfile connection failed"));
	/* for Solaris 12.5 */ newconn = nullptr;
    }
    init_con(newconn, description, CE_NATIVE, mode);

    newconn->canseek = FALSE;
    newconn->open = &bzfile_open;
    newconn->close = &bzfile_close;
    newconn->vfprintf = &dummy_vfprintf;
    newconn->fgetc_internal = &bzfile_fgetc_internal;
    newconn->fgetc = &dummy_fgetc;
    newconn->seek = &null_seek;
    newconn->fflush = &null_fflush;
    newconn->read = &bzfile_read;
    newconn->write = &bzfile_write;
    newconn->connprivate = (void *) malloc(sizeof(struct bzfileconn));
    if(!newconn->connprivate) {
	free(newconn->description); free(newconn->connclass); free(newconn);
	error(_("allocation of bzfile connection failed"));
	/* for Solaris 12.5 */ newconn = nullptr;
    }
    ((Rbzfileconn) newconn->connprivate)->compress = compress;
    return newconn;
}

#include <lzma.h>

typedef struct xzfileconn {
    FILE *fp;
    lzma_stream stream;
    lzma_action action;
    int compress;
    int type;
    lzma_filter filters[2];
    lzma_options_lzma opt_lzma;
    unsigned char buf[BUFSIZE];
} *Rxzfileconn;

static Rboolean xzfile_open(Rconnection con)
{
    Rxzfileconn xz = (Rxzfileconn) con->connprivate;
    lzma_ret ret;
    char mode[] = "rb";
    const char *name;

    con->canwrite = (Rboolean) (con->mode[0] == 'w' || con->mode[0] == 'a');
    con->canread = (Rboolean) !con->canwrite;
    /* regardless of the R view of the file, the file must be opened in
       binary mode where it matters */
    mode[0] = con->mode[0];
    errno = 0; /* precaution */
    name = R_ExpandFileName(con->description);
    xz->fp = R_fopen(name, mode);
    if(!xz->fp) {
	warning(_("cannot open compressed file '%s', probable reason '%s'"),
		name, strerror(errno));
	return FALSE;
    }
    if (isDir(xz->fp)) {
	warning(_("cannot open file '%s': it is a directory"), name);
	fclose(xz->fp);
	return FALSE;
    }
    if(con->canread) {
	xz->action = LZMA_RUN;
	/* probably about 80Mb is required, but 512Mb seems OK as a limit */
	if (xz->type == 1)
	    ret = lzma_alone_decoder(&xz->stream, 536870912);
	else
	    ret = lzma_stream_decoder(&xz->stream, 536870912,
				      LZMA_CONCATENATED);
	if (ret != LZMA_OK) {
	    warning(_("cannot initialize lzma decoder, error %d"), ret);
	    return FALSE;
	}
	xz->stream.avail_in = 0;
    } else {
	lzma_stream *strm = &xz->stream;
	uint32_t preset_number = abs(xz->compress);
	if(xz->compress < 0) preset_number |= LZMA_PRESET_EXTREME;
	if(lzma_lzma_preset(&xz->opt_lzma, preset_number))
	    error(_("problem with setting presets"));
	xz->filters[0].id = LZMA_FILTER_LZMA2;
	xz->filters[0].options = &(xz->opt_lzma);
	xz->filters[1].id = LZMA_VLI_UNKNOWN;

	ret = lzma_stream_encoder(strm, xz->filters, LZMA_CHECK_CRC32);
	if (ret != LZMA_OK) {
	    warning(_("cannot initialize lzma encoder, error %d"), ret);
	    return FALSE;
	}
    }
    con->isopen = TRUE;
    con->text = strchr(con->mode, 'b') ? FALSE : TRUE;
    con->set_buffer();
    set_iconv(con);
    con->save = -1000;
    return TRUE;
}

static void xzfile_close(Rconnection con)
{
    Rxzfileconn xz = (Rxzfileconn) con->connprivate;

    if(con->canwrite) {
	lzma_ret ret;
	lzma_stream *strm = &(xz->stream);
	size_t nout, res;
	unsigned char buf[BUFSIZE];
	while(true) {
	    strm->avail_out = BUFSIZE; strm->next_out = buf;
	    ret = lzma_code(strm, LZMA_FINISH);
	    nout = BUFSIZE - strm->avail_out;
	    res = fwrite(buf, 1, nout, xz->fp);
	    if (res != nout) error(_("'fwrite' error"));
	    if (ret != LZMA_OK) break;
	}
    }
    lzma_end(&(xz->stream));
    fclose(xz->fp);
    con->isopen = FALSE;
}

static size_t xzfile_read(void *ptr, size_t size, size_t nitems,
			  Rconnection con)
{
    Rxzfileconn xz = (Rxzfileconn) con->connprivate;
    lzma_stream *strm = &(xz->stream);
    lzma_ret ret;
    size_t s = size*nitems, have, given = 0;
    unsigned char *p = (unsigned char *) ptr;

    if (!s) return 0;

    while(true) {
	if (strm->avail_in == 0 && xz->action != LZMA_FINISH) {
	    strm->next_in = xz->buf;
	    strm->avail_in = fread(xz->buf, 1, BUFSIZ, xz->fp);
	    if (feof(xz->fp)) xz->action = LZMA_FINISH;
	}
	strm->avail_out = s; strm->next_out = p;
	ret = lzma_code(strm, xz->action);
	have = s - strm->avail_out;  given += have;
	//printf("available: %d, ready: %d/%d\n", strm->avail_in, given, s);
	if (ret != LZMA_OK) {
	    if (ret != LZMA_STREAM_END) {
		switch(ret) {
		case LZMA_MEM_ERROR:
		case LZMA_MEMLIMIT_ERROR:
		    warning(_("lzma decoder needed more memory"));
		    break;
		case LZMA_FORMAT_ERROR:
		    warning(_("lzma decoder format error"));
		    break;
		case LZMA_DATA_ERROR:
		    warning(_("lzma decoder corrupt data"));
		    break;
		default:
		    warning(_("lzma decoding result %d"), ret);
		}
	    }
	    return given/size;
	}
	s -= have;
	if (!s) return nitems;
	p += have;
    }
}

static int xzfile_fgetc_internal(Rconnection con)
{
    char buf[1];
    size_t size = xzfile_read(buf, 1, 1, con);

    return (size < 1) ? R_EOF : (buf[0] % 256);
}


static size_t xzfile_write(const void *ptr, size_t size, size_t nitems,
			   Rconnection con)
{
    Rxzfileconn xz = (Rxzfileconn) con->connprivate;
    lzma_stream *strm = &(xz->stream);
    lzma_ret ret;
    size_t s = size*nitems, nout, res;
    const unsigned char *p = (const unsigned char *) ptr;
    unsigned char buf[BUFSIZE];

    if (!s) return 0;

    strm->avail_in = s;
    strm->next_in = p;
    while(true) {
	strm->avail_out = BUFSIZE; strm->next_out = buf;
	ret = lzma_code(strm, LZMA_RUN);
	if (ret > 1) {
	    switch(ret) {
	    case LZMA_MEM_ERROR:
		warning(_("lzma encoder needed more memory"));
		break;
	    default:
		warning(_("lzma encoding result %d"), ret);
	    }
	    return 0;
	}
	nout = BUFSIZE - strm->avail_out;
	res = fwrite(buf, 1, nout, xz->fp);
	if (res != nout) error(_("'fwrite' error"));
	if (strm->avail_in == 0) return nitems;
    }
}

static Rconnection newxzfile(const char *description, const char *mode, int type, int compress)
{
    Rconnection newconn;
    newconn = (Rconnection) malloc(sizeof(struct Rconn));
    if(!newconn) error(_("allocation of xzfile connection failed"));
    newconn->connclass = (char *) malloc(strlen("xzfile") + 1);
    if(!newconn->connclass) {
	free(newconn);
	error(_("allocation of xzfile connection failed"));
	/* for Solaris 12.5 */ newconn = nullptr;
    }
    strcpy(newconn->connclass, "xzfile");
    newconn->description = (char *) malloc(strlen(description) + 1);
    if(!newconn->description) {
	free(newconn->connclass); free(newconn);
	error(_("allocation of xzfile connection failed"));
	/* for Solaris 12.5 */ newconn = nullptr;
    }
    init_con(newconn, description, CE_NATIVE, mode);

    newconn->canseek = FALSE;
    newconn->open = &xzfile_open;
    newconn->close = &xzfile_close;
    newconn->vfprintf = &dummy_vfprintf;
    newconn->fgetc_internal = &xzfile_fgetc_internal;
    newconn->fgetc = &dummy_fgetc;
    newconn->seek = &null_seek;
    newconn->fflush = &null_fflush;
    newconn->read = &xzfile_read;
    newconn->write = &xzfile_write;
    newconn->connprivate = (void *) malloc(sizeof(struct xzfileconn));
    memset(newconn->connprivate, 0, sizeof(struct xzfileconn));
    if(!newconn->connprivate) {
	free(newconn->description); free(newconn->connclass); free(newconn);
	error(_("allocation of xzfile connection failed"));
	/* for Solaris 12.5 */ newconn = nullptr;
    }
    ((Rxzfileconn) newconn->connprivate)->type = type;
    ((Rxzfileconn) newconn->connprivate)->compress = compress;
    return newconn;
}

/* op 0 is gzfile, 1 is bzfile, 2 is xv/lzma */
HIDDEN SEXP do_gzfile(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP sfile, sopen, ans, classs, enc;
    const char *file, *open;
    int ncon, compress = 9;
    Rconnection con = nullptr;
    int type = PRIMVAL(op);
    int subtype = 0;

    checkArity(op, args);
    sfile = CAR(args);
    if(!isString(sfile) || LENGTH(sfile) != 1 ||
       STRING_ELT(sfile, 0) == NA_STRING)
	error(_("invalid '%s' argument"), "description");
    if(LENGTH(sfile) > 1)
	warning(_("only first element of '%s' argument will be used"), "description");
    file = translateCharFP(STRING_ELT(sfile, 0));
    sopen = CADR(args);
    if(!isString(sopen) || LENGTH(sopen) != 1)
	error(_("invalid '%s' argument"), "open");
    enc = CADDR(args);
    if(!isString(enc) || LENGTH(enc) != 1 ||
       strlen(CHAR(STRING_ELT(enc, 0))) > 100) /* ASCII */
	error(_("invalid '%s' argument"), "encoding");
    if(type < 2) {
	compress = asInteger(CADDDR(args));
	if(compress == NA_LOGICAL || compress < 0 || compress > 9)
	    error(_("invalid '%s' argument"), "compress");
    }
    if(type == 2) {
	compress = asInteger(CADDDR(args));
	if(compress == NA_LOGICAL || abs(compress) > 9)
	    error(_("invalid '%s' argument"), "compress");
    }
    open = CHAR(STRING_ELT(sopen, 0)); /* ASCII */
    if (type == 0 && (!open[0] || open[0] == 'r')) {
	/* check magic no */
	FILE *fp = fopen(R_ExpandFileName(file), "rb");
	char buf[7];
	if (fp) {
	    size_t res;
	    memset(buf, 0, 7); res = fread(buf, 5, 1, fp); fclose(fp);
	    if(res == 1) {
		if(streqln(buf, "BZh", 3)) type = 1;
		if((buf[0] == '\xFD') && streqln(buf+1, "7zXZ", 4)) type = 2;
		if((buf[0] == '\xFF') && streqln(buf+1, "LZMA", 4)) {
		    type = 2; subtype = 1;
		}
		if(!memcmp(buf, "]\0\0\200\0", 5)) {
		    type = 2; subtype = 1;
		}
		if((buf[0] == '\x89') && streqln(buf+1, "LZO", 3))
		    error(_("this is a file compressed using '%s' which this build of R does not support"), "lzop");
	    }
	}
    }
    switch(type) {
    case 0:
	con = newgzfile(file, strlen(open) ? open : "rb", compress);
	break;
    case 1:
	con = newbzfile(file, strlen(open) ? open : "rb", compress);
	break;
    case 2:
	con = newxzfile(file, strlen(open) ? open : "rb", subtype, compress);
	break;
    }
    ncon = Rconn::NextConnection();
    Connections[ncon] = con;
    con->blocking = TRUE;
    strncpy(con->encname, CHAR(STRING_ELT(enc, 0)), 100); /* ASCII */
    con->encname[100 - 1] = '\0';

    /* see the comment in do_url */
    if (con->encname[0] && !streql(con->encname, "native.enc"))
	con->canseek = FALSE;
    con->ex_ptr = PROTECT(R_MakeExternalPtr(con->id, install("connection"), R_NilValue));

    /* open it if desired */
    if(strlen(open)) {
	Rboolean success = con->open(con);
	if(!success) {
	    con_destroy(ncon);
	    error(_("cannot open the connection"));
	}
    }

    PROTECT(ans = ScalarInteger(ncon));
    PROTECT(classs = allocVector(STRSXP, 2));
    switch(type) {
    case 0:
	SET_STRING_ELT(classs, 0, mkChar("gzfile"));
	break;
    case 1:
	SET_STRING_ELT(classs, 0, mkChar("bzfile"));
	break;
    case 2:
	SET_STRING_ELT(classs, 0, mkChar("xzfile"));
	break;
    }
    SET_STRING_ELT(classs, 1, mkChar("connection"));
    classgets(ans, classs);
    setAttrib(ans, R_ConnIdSymbol, (SEXP) con->ex_ptr);
    R_RegisterCFinalizerEx((SEXP) con->ex_ptr, conFinalizer, FALSE);
    UNPROTECT(3);

    return ans;
}

/* ------------------- clipboard connections --------------------- */

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN 1
#include <windows.h>
extern int GA_clipboardhastext(void); /* from ga.h */
#endif

#ifdef Unix
Rboolean R_ReadClipboard(Rclpconn clpcon, char *type);
#endif

static Rboolean clp_open(Rconnection con)
{
    Rclpconn thisconn = (Rclpconn) con->connprivate;

    con->isopen = TRUE;
    con->canwrite = (Rboolean) (con->mode[0] == 'w' || con->mode[0] == 'a');
    con->canread = (Rboolean) !con->canwrite;
    thisconn->pos = 0;
    if(con->canread) {
	/* copy the clipboard contents now */
#ifdef _WIN32
	HGLOBAL hglb;
	char *pc;
	if(GA_clipboardhastext() &&
	   OpenClipboard(nullptr) &&
	   (hglb = GetClipboardData(CF_TEXT)) &&
	   (pc = (char *)GlobalLock(hglb))) {
	    int len = (int) strlen(pc);  // will be fairly small
	    thisconn->buff = (char *)malloc(len + 1);
	    thisconn->last = thisconn->len = len;
	    if(thisconn->buff) {
		strcpy(thisconn->buff, pc);
		GlobalUnlock(hglb);
		CloseClipboard();
	    } else {
		GlobalUnlock(hglb);
		CloseClipboard();
		thisconn->buff = nullptr; thisconn->last = thisconn->len = 0;
		warning(_("memory allocation to copy clipboard failed"));
		return FALSE;
	    }
	} else {
	    thisconn->buff = nullptr; thisconn->last = thisconn->len = 0;
	    warning(_("clipboard cannot be opened or contains no text"));
	    return FALSE;
	}
#else
	Rboolean res = R_ReadClipboard(thisconn, con->description);
	if(!res) return FALSE;
#endif
    } else {
	int len = (thisconn->sizeKB)*1024;
	thisconn->buff = (char *) malloc(len + 1);
	if(!thisconn->buff) {
	    warning(_("memory allocation to open clipboard failed"));
	    return FALSE;
	}
	thisconn->len = len;
	thisconn->last = 0;
    }
    con->text = TRUE;
    /* Not calling con->set_buffer() as the data is already buffered */
    set_iconv(con);
    con->save = -1000;
    thisconn->warned = FALSE;

    return TRUE;
}

static void clp_writeout(Rconnection con)
{
#ifdef _WIN32
    Rclpconn thisconn = con->connprivate;

    HGLOBAL hglb;
    char *s, *p;
    if ( (hglb = GlobalAlloc(GHND, thisconn->len)) &&
	 (s = (char *)GlobalLock(hglb)) ) {
	p = thisconn->buff;
	while(p < thisconn->buff + thisconn->pos) *s++ = *p++;
	*s = '\0';
	GlobalUnlock(hglb);
	if (!OpenClipboard(nullptr) || !EmptyClipboard()) {
	    warning(_("unable to open the clipboard"));
	    GlobalFree(hglb);
	} else {
	    if(!SetClipboardData(CF_TEXT, hglb)) {
		warning(_("unable to write to the clipboard"));
		GlobalFree(hglb);
	    }
	    CloseClipboard();
	}
    }
#endif
}

static void clp_close(Rconnection con)
{
    Rclpconn thisconn = (Rclpconn) con->connprivate;

    con->isopen = FALSE;
    if(con->canwrite)
	clp_writeout(con);
    if(thisconn-> buff) free(thisconn->buff);
}

static int clp_fgetc_internal(Rconnection con)
{
    Rclpconn thisconn = (Rclpconn) con->connprivate;

    if (thisconn->pos >= thisconn->len) return R_EOF;
    return thisconn->buff[thisconn->pos++];
}

static double clp_seek(Rconnection con, double where, int origin, int rw)
{
    Rclpconn thisconn = (Rclpconn) con->connprivate;
    int newpos, oldpos = thisconn->pos;

    if(ISNA(where)) return oldpos;

    switch(origin) {
    case 2: newpos = thisconn->pos + (int) where; break;
    case 3: newpos = thisconn->last + (int) where; break;
    default: newpos = (int) where;
    }
    if(newpos < 0 || newpos >= thisconn->last)
	error(_("attempt to seek outside the range of the clipboard"));
    else thisconn->pos = newpos;

    return (double) oldpos;
}

static void clp_truncate(Rconnection con)
{
    Rclpconn thisconn = (Rclpconn) con->connprivate;

    if(!con->isopen || !con->canwrite)
	error(_("can only truncate connections open for writing"));
    thisconn->last = thisconn->pos;
}

static int clp_fflush(Rconnection con)
{
    if(!con->isopen || !con->canwrite) return 1;
    clp_writeout(con);
    return 0;
}

static size_t clp_read(void *ptr, size_t size, size_t nitems,
			Rconnection con)
{
    Rclpconn thisconn = (Rclpconn) con->connprivate;
    int available = thisconn->len - thisconn->pos, request = (int)(size*nitems), used;
    if ((double) size * (double) nitems > R_INT_MAX)
	error(_("specified block is too large"));
    used = (request < available) ? request : available;
    strncpy((char*) ptr, thisconn->buff + thisconn->pos, used);
    thisconn->pos += used;
    return (size_t) used/size;
}

static size_t clp_write(const void *ptr, size_t size, size_t nitems,
			 Rconnection con)
{
    Rclpconn thisconn = (Rclpconn) con->connprivate;
    int i, len = (int)(size * nitems), used = 0;
    char c, *p = (char *) ptr, *q = thisconn->buff + thisconn->pos;

    if(!con->canwrite)
	error(_("clipboard connection is open for reading only"));
    if ((double) size * (double) nitems > R_INT_MAX)
	error(_("specified block is too large"));

    for(i = 0; i < len; i++) {
	if(thisconn->pos >= thisconn->len) break;
	c = *p++;
#ifdef _WIN32
    /* clipboard requires CRLF termination */
	if(c == '\n') {
	    *q++ = '\r';
	    thisconn->pos++;
	    if(thisconn->pos >= thisconn->len) break;
	}
#endif
	*q++ = c;
	thisconn->pos++;
	used++;
    }
    if (used < len && !thisconn->warned) {
	warning(_("clipboard buffer is full and output lost"));
	thisconn->warned = TRUE;
    }
    if(thisconn->last < thisconn->pos) thisconn->last = thisconn->pos;
    return (size_t) used/size;
}

static Rconnection newclp(const char *url, const char *inmode)
{
    Rconnection newconn;
    const char *description;
    int sizeKB = 32;
    char mode[4];

    mode[3] = '\0';
    strncpy(mode, inmode, 3);

    if(strlen(mode) == 2 && mode[1] == 't') mode[1] = '\0';

    if(strlen(mode) != 1 ||
       (mode[0] != 'r' && mode[0] != 'w'))
	error(_("'mode' for the clipboard must be 'r' or 'w'"));
#ifdef Unix
    if(mode[0] != 'r')
	error(_("'mode' for the clipboard must be 'r' on Unix"));
#endif
    newconn = (Rconnection) malloc(sizeof(struct Rconn));
    if(!newconn) error(_("allocation of clipboard connection failed"));
    if(streqln(url, "clipboard", 9)) description = "clipboard";
    else description = url;
    newconn->connclass = (char *) malloc(strlen(description) + 1);
    if(!newconn->connclass) {
	free(newconn);
	error(_("allocation of clipboard connection failed"));
	/* for Solaris 12.5 */ newconn = nullptr;
    }
    strcpy(newconn->connclass, description);
    newconn->description = (char *) malloc(strlen(description) + 1);
    if(!newconn->description) {
	free(newconn->connclass); free(newconn);
	error(_("allocation of clipboard connection failed"));
	/* for Solaris 12.5 */ newconn = nullptr;
    }
    init_con(newconn, description, CE_NATIVE, mode);
    newconn->open = &clp_open;
    newconn->close = &clp_close;
    newconn->vfprintf = &dummy_vfprintf;
    newconn->fgetc_internal = &clp_fgetc_internal;
    newconn->fgetc = &dummy_fgetc;
    newconn->seek = &clp_seek;
    newconn->truncate = &clp_truncate;
    newconn->fflush = &clp_fflush;
    newconn->read = &clp_read;
    newconn->write = &clp_write;
    newconn->canseek = TRUE;
    newconn->connprivate = (void *) malloc(sizeof(struct clpconn));
    if(!newconn->connprivate) {
	free(newconn->description); free(newconn->connclass); free(newconn);
	error(_("allocation of clipboard connection failed"));
	/* for Solaris 12.5 */ newconn = nullptr;
    }
    ((Rclpconn) newconn->connprivate)->buff = nullptr;
    if (streqln(url, "clipboard-", 10)) {
	sizeKB = atoi(url+10);
	if(sizeKB < 32) sizeKB = 32;
	/* Rprintf("setting clipboard size to %dKB\n", sizeKB); */
    }
    ((Rclpconn) newconn->connprivate)->sizeKB = sizeKB;
    return newconn;
}

/* ------------------- terminal connections --------------------- */

static unsigned char ConsoleBuf[CONSOLE_BUFFER_SIZE + 1];
static unsigned char *ConsoleBufp;
static int ConsoleBufCnt;

static int ConsoleGetchar(void)
{
    if (--ConsoleBufCnt < 0)
    {
        ConsoleBuf[CONSOLE_BUFFER_SIZE] = '\0';
        if (R_ReadConsole("", ConsoleBuf, CONSOLE_BUFFER_SIZE, 0) == 0)
        {
            R_ClearerrConsole();
            return R_EOF;
        }
        ConsoleBufp = ConsoleBuf;
        ConsoleBufCnt = (int)strlen((char *)ConsoleBuf); // must be short
        ConsoleBufCnt--;
    }
    return *ConsoleBufp++;
}

static int stdin_fgetc(Rconnection con)
{
    return ConsoleGetchar();
}

static int stdout_vfprintf(Rconnection con, const char *format, va_list ap)
{
    if (R_Outputfile)
        vfprintf(R_Outputfile, format, ap);
    else
        Rcons_vprintf(format, ap);
    return 0;
}

static int stdout_fflush(Rconnection con)
{
    if (R_Outputfile)
        return fflush(R_Outputfile);
    return 0;
}

static int stderr_vfprintf(Rconnection con, const char *format, va_list ap)
{
    REvprintf(format, ap);
    return 0;
}

static int stderr_fflush(Rconnection con)
{
    /* normally stderr and hence unbuffered, but it needs not be,
       e.g. it is stdout on Win9x */
    if (R_Consolefile)
        return fflush(R_Consolefile);
    return 0;
}

static Rconnection newterminal(const char *description, const char *mode)
{
    Rconnection newconn;
    newconn = (Rconnection) malloc(sizeof(struct Rconn));
    if(!newconn) error(_("allocation of terminal connection failed"));
    newconn->connclass = (char *) malloc(strlen("terminal") + 1);
    if(!newconn->connclass) {
	free(newconn);
	error(_("allocation of terminal connection failed"));
	/* for Solaris 12.5 */ newconn = nullptr;
    }
    strcpy(newconn->connclass, "terminal");
    newconn->description = (char *) malloc(strlen(description) + 1);
    if(!newconn->description) {
	free(newconn->connclass); free(newconn);
	error(_("allocation of terminal connection failed"));
	/* for Solaris 12.5 */ newconn = nullptr;
    }
    init_con(newconn, description, CE_NATIVE, mode);
    newconn->isopen = TRUE;
    newconn->canread = Rboolean(streql(mode, "r"));
    newconn->canwrite = Rboolean(streql(mode, "w"));
    newconn->destroy = &null_close;
    newconn->connprivate = nullptr;
    return newconn;
}


HIDDEN SEXP do_stdin(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, classs;
    Rconnection con = getConnection(0);

    checkArity(op, args);
    PROTECT(ans = ScalarInteger(0));
    PROTECT(classs = allocVector(STRSXP, 2));
    SET_STRING_ELT(classs, 0, mkChar(con->connclass));
    SET_STRING_ELT(classs, 1, mkChar("connection"));
    classgets(ans, classs);
    UNPROTECT(2);
    return ans;
}

HIDDEN SEXP do_stdout(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, classs;
    Rconnection con = getConnection(R_OutputCon);

    checkArity(op, args);
    PROTECT(ans = ScalarInteger(R_OutputCon));
    PROTECT(classs = allocVector(STRSXP, 2));
    SET_STRING_ELT(classs, 0, mkChar(con->connclass));
    SET_STRING_ELT(classs, 1, mkChar("connection"));
    classgets(ans, classs);
    UNPROTECT(2);
    return ans;
}


HIDDEN SEXP do_stderr(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, classs;
    Rconnection con = getConnection(2);

    checkArity(op, args);
    PROTECT(ans = ScalarInteger(2));
    PROTECT(classs = allocVector(STRSXP, 2));
    SET_STRING_ELT(classs, 0, mkChar(con->connclass));
    SET_STRING_ELT(classs, 1, mkChar("connection"));
    classgets(ans, classs);
    UNPROTECT(2);
    return ans;
}

HIDDEN SEXP do_isatty(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int con;
    /* FIXME: is this correct for consoles? */
    checkArity(op, args);
    con = asInteger(CAR(args));
    return ScalarLogical(con == NA_LOGICAL ? FALSE : R_isatty(con) );
}

/* ------------------- raw connections --------------------- */

/* Possible future redesign: store nbytes as TRUELENGTH */

typedef struct rawconn {
    SEXP data; /* all the data, stored as a raw vector */
    /* replace nbytes by TRUELENGTH in due course? */
    size_t pos; /* current pos */
    size_t nbytes; /* number of bytes (same pos for read and write) */
} *Rrawconn;

/* copy a raw vector into a buffer */
static void raw_init(Rconnection con, SEXP raw)
{
    Rrawconn thisconn = (Rrawconn)con->connprivate;

    thisconn->data = MAYBE_REFERENCED(raw) ? duplicate(raw) : raw;
    R_PreserveObject(thisconn->data);
    thisconn->nbytes = XLENGTH(thisconn->data);
    thisconn->pos = 0;
}

static Rboolean raw_open(Rconnection con)
{
    return TRUE;
}

static void raw_close(Rconnection con)
{
}

static void raw_destroy(Rconnection con)
{
    Rrawconn thisconn = (Rrawconn) con->connprivate;

    R_ReleaseObject(thisconn->data);
    free(thisconn);
}

static void raw_resize(Rrawconn thisconn, size_t needed)
{
    size_t nalloc = 64;
    SEXP tmp;

    if (needed > 8192)
        nalloc = (size_t)(1.2 * (double)needed); /* 20% over-allocation */
    else
        while (nalloc < needed)
            nalloc *= 2; /* use powers of 2 if small */
    PROTECT(tmp = allocVector(RAWSXP, nalloc));
    memcpy(RAW(tmp), RAW(thisconn->data), thisconn->nbytes);
    R_ReleaseObject(thisconn->data);
    thisconn->data = tmp;
    R_PreserveObject(thisconn->data);
    UNPROTECT(1);
}

static size_t raw_write(const void *ptr, size_t size, size_t nitems,
			Rconnection con)
{
    Rrawconn thisconn = (Rrawconn) con->connprivate;
    size_t freespace = XLENGTH(thisconn->data) - thisconn->pos, bytes = size*nitems;

    if ((double) size * (double) nitems + (double) thisconn->pos > (double) R_XLEN_T_MAX)
	error(_("attempting to add too many elements to raw vector"));
    /* resize may fail, when this will give an error */
    if(bytes >= freespace) raw_resize(thisconn, bytes + thisconn->pos);
    /* the source just might be this raw vector */
    memmove(RAW(thisconn->data) + thisconn->pos, ptr, bytes);
    thisconn->pos += bytes;
    if(thisconn->nbytes < thisconn->pos) thisconn->nbytes = thisconn->pos;
    return nitems;
}

static void raw_truncate(Rconnection con)
{
    Rrawconn thisconn = (Rrawconn)con->connprivate;
    thisconn->nbytes = thisconn->pos;
}

static size_t raw_read(void *ptr, size_t size, size_t nitems,
		       Rconnection con)
{
    Rrawconn thisconn = (Rrawconn) con->connprivate;
    size_t available = thisconn->nbytes - thisconn->pos, request = size*nitems, used;

    if ((double) size * (double) nitems + (double) thisconn->pos > (double) R_XLEN_T_MAX)
	error(_("specified block is too large"));
    used = (request < available) ? request : available;
    memmove(ptr, RAW(thisconn->data) + thisconn->pos, used);
    thisconn->pos += used;
    return used/size;
}

static int raw_fgetc(Rconnection con)
{
    Rrawconn thisconn = (Rrawconn) con->connprivate;
    if(thisconn->pos >= thisconn->nbytes) return R_EOF;
    else return (int) RAW(thisconn->data)[thisconn->pos++];
}

static double raw_seek(Rconnection con, double where, int origin, int rw)
{
    Rrawconn thisconn = (Rrawconn) con->connprivate;
    double newpos;
    size_t oldpos = thisconn->pos;

    if(ISNA(where)) return (double) oldpos;

    /* Do the calculations here as double to avoid integer overflow */
    switch(origin) {
    case 2: newpos = (double) thisconn->pos + where; break;
    case 3: newpos = (double) thisconn->nbytes + where; break;
    default: newpos = where;
    }
    if(newpos < 0 || newpos > thisconn->nbytes)
	error(_("attempt to seek outside the range of the raw connection"));
    else thisconn->pos = (size_t) newpos;

    return (double) oldpos;
}

static Rconnection newraw(const char *description, SEXP raw, const char *mode)
{
    Rconnection newconn;

    newconn = (Rconnection) malloc(sizeof(struct Rconn));
    if(!newconn) error(_("allocation of raw connection failed"));
    newconn->connclass = (char *) malloc(strlen("rawConnection") + 1);
    if(!newconn->connclass) {
	free(newconn);
	error(_("allocation of raw connection failed"));
	/* for Solaris 12.5 */ newconn = nullptr;
    }
    strcpy(newconn->connclass, "rawConnection");
    newconn->description = (char *) malloc(strlen(description) + 1);
    if(!newconn->description) {
	free(newconn->connclass); free(newconn);
	error(_("allocation of raw connection failed"));
	/* for Solaris 12.5 */ newconn = nullptr;
    }
    init_con(newconn, description, CE_NATIVE, mode);
    newconn->isopen = TRUE;
    newconn->text = FALSE;
    newconn->blocking = TRUE;
    newconn->canseek = TRUE;
    newconn->canwrite = (Rboolean) (mode[0] == 'w' || mode[0] == 'a');
    newconn->canread = (Rboolean) (mode[0] == 'r');
    if(strlen(mode) >= 2 && mode[1] == '+') newconn->canread = newconn->canwrite = TRUE;
    newconn->open = &raw_open;
    newconn->close = &raw_close;
    newconn->destroy = &raw_destroy;
    if(newconn->canwrite) {
	newconn->write = &raw_write;
	newconn->vfprintf = &dummy_vfprintf;
	newconn->truncate = &raw_truncate;
    }
    if(newconn->canread) {
	newconn->read = &raw_read;
	newconn->fgetc = &raw_fgetc;
    }
    newconn->seek = &raw_seek;
    newconn->connprivate = (void*) malloc(sizeof(struct rawconn));
    if(!newconn->connprivate) {
	free(newconn->description); free(newconn->connclass); free(newconn);
	error(_("allocation of raw connection failed"));
	/* for Solaris 12.5 */ newconn = nullptr;
    }
    raw_init(newconn, raw);
    if(mode[0] == 'a') raw_seek(newconn, 0, 3, 0);
    return newconn;
}

// .Internal(rawConnection(deparse(substitute(object)), object, open))
HIDDEN SEXP do_rawconnection(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP sfile, sraw, sopen, ans, classs;
    const char *desc, *open;
    int ncon;
    Rconnection con = nullptr;

    checkArity(op, args);
    sfile = CAR(args);
    if(!isString(sfile) || LENGTH(sfile) != 1 ||
       STRING_ELT(sfile, 0) == NA_STRING)
	error(_("invalid '%s' argument"), "description");
    desc = translateCharFP(STRING_ELT(sfile, 0));
    sraw = CADR(args);
    sopen = CADDR(args);
    if(!isString(sopen) || LENGTH(sopen) != 1)
	error(_("invalid '%s' argument"), "open");
    open = CHAR(STRING_ELT(sopen, 0)); /* ASCII */
    if(strchr(open, 't'))
	error(_("invalid '%s' argument"), "open");
    ncon = Rconn::NextConnection();
    if(TYPEOF(sraw) != RAWSXP)
	error(_("invalid '%s' argument"), "raw");
    con = Connections[ncon] = newraw(desc, sraw, open);

    /* already opened */

    PROTECT(ans = ScalarInteger(ncon));
    PROTECT(classs = allocVector(STRSXP, 2));
    SET_STRING_ELT(classs, 0, mkChar("rawConnection"));
    SET_STRING_ELT(classs, 1, mkChar("connection"));
    classgets(ans, classs);
    con->ex_ptr = R_MakeExternalPtr(con->id, install("connection"), R_NilValue);
    setAttrib(ans, R_ConnIdSymbol, (SEXP) con->ex_ptr);
    R_RegisterCFinalizerEx((SEXP) con->ex_ptr, conFinalizer, FALSE);
    UNPROTECT(2);
    return ans;
}

static Rconnection getConnectionCheck(SEXP rcon, const char *cls,
				      const char *var)
{
    if(!inherits(rcon, cls))
	error(_("'%s' is not a %s"), var, cls);
    Rconnection con = getConnection(asInteger(rcon));
    /* check that the R class and internal class match */
    if (strcmp(con->connclass, cls))
	error(_("internal connection is not a %s"), cls);
    return con;
}

HIDDEN SEXP do_rawconvalue(SEXP call, SEXP op, SEXP args, SEXP env)
{
    Rconnection con = nullptr;
    Rrawconn thisconn;
    SEXP ans;

    checkArity(op, args);
    con = getConnectionCheck(CAR(args), "rawConnection", "con");
    if (!con->canwrite)
        error(_("'%s' is not an output of class %s"), "con", "\"rawConnection\"");
    thisconn = (Rrawconn)con->connprivate;
    ans = allocVector(RAWSXP, thisconn->nbytes); /* later, use TRUELENGTH? */
    memcpy(RAW(ans), RAW(thisconn->data), thisconn->nbytes);
    return ans;
}

/* ------------------- text connections --------------------- */

typedef struct textconn {
    char *data;  /* all the data */
    size_t cur, nchars; /* current pos and number of chars */
    char save; /* pushback */
} *Rtextconn;

typedef struct outtextconn {
    size_t len;  /* number of lines */
    SEXP namesymbol;
    SEXP data;
    char *lastline;
    size_t lastlinelength; /* buffer size */
} *Routtextconn;

/* read a R character vector into a buffer  --- helper for newtext() */
static void text_init(Rconnection con, SEXP text, int type)
{
    R_xlen_t nlines = xlength(text);  // not very plausible that this is long
    size_t nchars = 0; /* -Wall */
    double dnc = 0.0;
    Rtextconn thisconn = (Rtextconn) con->connprivate;
    const void *vmax = vmaxget();

    for(R_xlen_t i = 0; i < nlines; i++)
	dnc +=
	    /*     type =  1 |    2    |    3    <==>
	     * encoding = "" | "bytes" | "UTF-8" */
	    (double) strlen(type == 1 ? translateChar(STRING_ELT(text, i))
			    : ((type == 3) ?translateCharUTF8(STRING_ELT(text, i))
			       : CHAR(STRING_ELT(text, i))) ) + 1;
    if (dnc >= (double) SIZE_MAX)
	error(_("too many characters for text connection"));
    else nchars = (size_t) dnc;
    thisconn->data = (char *) malloc(nchars+1);
    if(!thisconn->data) {
	free(thisconn); free(con->description); free(con->connclass); free(con);
	error(_("cannot allocate memory for text connection"));
    }
    char *t = thisconn->data;
    for(R_xlen_t i = 0; i < nlines; i++) {
	const char *s = (type == 1) ? translateChar(STRING_ELT(text, i))
	    : ((type == 3) ? translateCharUTF8(STRING_ELT(text, i))
	       : CHAR(STRING_ELT(text, i)));
	while(*s) *t++ = *s++;
	*t++ = '\n';
    }
    *t = '\0';
    thisconn->nchars = nchars;
    thisconn->cur = thisconn->save = 0;
    vmaxset(vmax);
}

static Rboolean text_open(Rconnection con)
{
    con->save = -1000;
    return TRUE;
}

static void text_close(Rconnection con)
{
}

static void text_destroy(Rconnection con)
{
    Rtextconn thisconn = (Rtextconn) con->connprivate;

    free(thisconn->data);
    /* thisconn->cur = thisconn->nchars = 0; */
    free(thisconn);
}

static int text_fgetc(Rconnection con)
{
    Rtextconn thisconn = (Rtextconn)con->connprivate;
    if (thisconn->save)
    {
        int c;
        c = thisconn->save;
        thisconn->save = 0;
        return c;
    }
    if (thisconn->cur >= thisconn->nchars)
        return R_EOF;
    else
        return (int)(thisconn->data[thisconn->cur++]);
}

static double text_seek(Rconnection con, double where, int origin, int rw)
{
    if (where >= 0)
        error(_("seek is not relevant for text connection"));
    return 0; /* if just asking, always at the beginning */
}

// helper for do_textconnection(.., open = "r") :
static Rconnection newtext(const char *description, SEXP text, int type)
{
    Rconnection newconn;
    newconn = (Rconnection) malloc(sizeof(struct Rconn));
    if(!newconn) error(_("allocation of text connection failed"));
    newconn->connclass = (char *) malloc(strlen("textConnection") + 1);
    if(!newconn->connclass) {
	free(newconn);
	error(_("allocation of text connection failed"));
	/* for Solaris 12.5 */ newconn = nullptr;
    }
    strcpy(newconn->connclass, "textConnection");
    newconn->description = (char *) malloc(strlen(description) + 1);
    if(!newconn->description) {
	free(newconn->connclass); free(newconn);
	error(_("allocation of text connection failed"));
	/* for Solaris 12.5 */ newconn = nullptr;
    }
    init_con(newconn, description, CE_NATIVE, "r");
    newconn->isopen = TRUE;
    newconn->canwrite = FALSE;
    newconn->open = &text_open;
    newconn->close = &text_close;
    newconn->destroy = &text_destroy;
    newconn->fgetc = &text_fgetc;
    newconn->seek = &text_seek;
    newconn->connprivate = (void*) malloc(sizeof(struct textconn));
    if(!newconn->connprivate) {
	free(newconn->description); free(newconn->connclass); free(newconn);
	error(_("allocation of text connection failed"));
	/* for Solaris 12.5 */ newconn = nullptr;
    }
    text_init(newconn, text, type);
    return newconn;
}


static SEXP mkCharLocal(const char *s)
{
    cetype_t ienc = CE_NATIVE;
    if(known_to_be_latin1) ienc = CE_LATIN1;
    if(known_to_be_utf8) ienc = CE_UTF8;
    return mkCharCE(s, ienc);
}

static void outtext_close(Rconnection con)
{
    Routtextconn thisconn = (Routtextconn) con->connprivate;
    int idx = con->ConnIndex();
    SEXP tmp, env = VECTOR_ELT(OutTextData, idx);

    if(thisconn->namesymbol &&
       findVarInFrame3(env, thisconn->namesymbol, FALSE) != R_UnboundValue)
	R_unLockBinding(thisconn->namesymbol, env);
    if(strlen(thisconn->lastline) > 0) {
	PROTECT(tmp = xlengthgets(thisconn->data, ++thisconn->len));
	SET_STRING_ELT(tmp, thisconn->len - 1, mkCharLocal(thisconn->lastline));
	if(thisconn->namesymbol) defineVar(thisconn->namesymbol, tmp, env);
	ENSURE_NAMEDMAX(tmp);
	thisconn->data = tmp;
	UNPROTECT(1);
    }
}

static void outtext_destroy(Rconnection con)
{
    Routtextconn thisconn = (Routtextconn) con->connprivate;
    int idx = con->ConnIndex();
    /* OutTextData is preserved, and that implies that the environment
       we are writing it and hence the character vector is protected.
       However, thisconn could be quite expensive.
    */
    SET_VECTOR_ELT(OutTextData, idx, R_NilValue);
    if(!thisconn->namesymbol) R_ReleaseObject(thisconn->data);
    free(thisconn->lastline); free(thisconn);
}

constexpr size_t LAST_LINE_LEN = 256;

static int text_vfprintf(Rconnection con, const char *format, va_list ap)
{
    Routtextconn thisconn = (Routtextconn) con->connprivate;
    char buf[BUFSIZE], *b = buf, *p, *q;
    const void *vmax = nullptr;
    int res = 0, buffree,
	already = (int) strlen(thisconn->lastline); // we do not allow longer lines
    SEXP tmp;

    va_list aq;
    va_copy(aq, ap);
    if(already >= BUFSIZE) {
	/* This will fail so just call vsnprintf to get the length of
	   the new piece */
	res = vsnprintf(buf, 0, format, aq);
	if(res > 0) res += already;
	buffree = 0;
    } else {
	strcpy(b, thisconn->lastline);
	p = b + already;
	buffree = BUFSIZE - already; // checked < BUFSIZE above
	res = vsnprintf(p, buffree, format, aq);
    }
    va_end(aq);
    if(res >= buffree) { /* res is the desired output length */
	vmax = vmaxget();
	b = R_alloc(res + already + 1, sizeof(char));
	strcpy(b, thisconn->lastline);
	p = b + already;
	vsprintf(p, format, ap);
    } else if(res < 0) { /* just a failure indication */
const int NBUFSIZE = (already + 100*BUFSIZE);
	vmax = vmaxget();
	b = R_alloc(NBUFSIZE, sizeof(char));
	strncpy(b, thisconn->lastline, NBUFSIZE); /* `already` < NBUFSIZE */
	*(b + NBUFSIZE - 1) = '\0';
	p = b + already;
	res = Rvsnprintf_mbcs(p, NBUFSIZE - already, format, ap);
	if (res < 0 || res >= NBUFSIZE - already) {
	    warning(_("printing of extremely long output is truncated"));
	}
    }

    /* copy buf line-by-line to object */
    for(p = b; ; p = q+1) {
	q = Rf_strchr(p, '\n');
	if(q) {
	    int idx = con->ConnIndex();
	    SEXP env = VECTOR_ELT(OutTextData, idx);
	    *q = '\0';
	    PROTECT(tmp = xlengthgets(thisconn->data, ++thisconn->len));
	    SET_STRING_ELT(tmp, thisconn->len - 1, mkCharLocal(p));
	    if(thisconn->namesymbol) {
		if(findVarInFrame3(env, thisconn->namesymbol, FALSE)
		   != R_UnboundValue) R_unLockBinding(thisconn->namesymbol, env);
		defineVar(thisconn->namesymbol, tmp, env);
		R_LockBinding(thisconn->namesymbol, env);
	    } else {
		R_ReleaseObject(thisconn->data);
		R_PreserveObject(tmp);
	    }
	    thisconn->data = tmp;
	    ENSURE_NAMEDMAX(tmp);
	    UNPROTECT(1);
	} else {
	    /* retain the last line */
	    if(strlen(p) >= thisconn->lastlinelength) {
		size_t newlen = strlen(p) + 1;
		if (newlen > R_INT_MAX) error(_("last line is too long"));
		void * tmp = realloc(thisconn->lastline, newlen);
		if (tmp) {
		    thisconn->lastline = (char*) tmp;
		    thisconn->lastlinelength = (int) newlen;
		} else {
		    warning(_("allocation problem for last line"));
		    thisconn->lastline = nullptr;
		    thisconn->lastlinelength = 0;
		}
	    }
	    strcpy(thisconn->lastline, p);
	    con->incomplete = (Rboolean) (strlen(thisconn->lastline) > 0);
	    break;
	}
    }
    if(vmax) vmaxset(vmax);
    return res;
}

// finalizing helper for  newouttext() :
static void outtext_init(Rconnection con, SEXP stext, const char *mode, int idx)
{
    Routtextconn thisconn = (Routtextconn) con->connprivate;
    SEXP val;

    if(stext == R_NilValue) {
	thisconn->namesymbol = nullptr;
	    /* create variable pointed to by con->description */
	val = allocVector(STRSXP, 0);
	R_PreserveObject(val);
    } else {
	thisconn->namesymbol = install(con->description);
	if(streql(mode, "w")) {
	    /* create variable pointed to by con->description */
	    PROTECT(val = allocVector(STRSXP, 0));
	    defineVar(thisconn->namesymbol, val, VECTOR_ELT(OutTextData, idx));
	    /* Not clear if this is needed, but be conservative */
	    ENSURE_NAMEDMAX(val);
	    UNPROTECT(1);
	} else {
	    /* take over existing variable */
	    val = findVar1(thisconn->namesymbol, VECTOR_ELT(OutTextData, idx),
			   STRSXP, FALSE);
	    if(val == R_UnboundValue) {
		warning(_("text connection: appending to a non-existent char vector"));
		PROTECT(val = allocVector(STRSXP, 0));
		defineVar(thisconn->namesymbol, val, VECTOR_ELT(OutTextData, idx));
		ENSURE_NAMEDMAX(val);
		UNPROTECT(1);
	    }
	    PROTECT(val);
	    R_LockBinding(thisconn->namesymbol, VECTOR_ELT(OutTextData, idx));
	    UNPROTECT(1);
	}
    }
    thisconn->len = LENGTH(val);
    thisconn->data = val;
    thisconn->lastline[0] = '\0';
    thisconn->lastlinelength = LAST_LINE_LEN;
}

// helper for do_textconnection(.., open = "w" or "a") :
static Rconnection newouttext(const char *description, SEXP stext,
			      const char *mode, int idx)
{
    Rconnection newconn;
    void *tmp;

    newconn = (Rconnection) malloc(sizeof(struct Rconn));
    if(!newconn) error(_("allocation of text connection failed"));
    newconn->connclass = (char *) malloc(strlen("textConnection") + 1);
    if(!newconn->connclass) {
	free(newconn);
	error(_("allocation of text connection failed"));
 	/* for Solaris 12.5 */ newconn = nullptr;
   }
    strcpy(newconn->connclass, "textConnection");
    newconn->description = (char *) malloc(strlen(description) + 1);
    if(!newconn->description) {
	free(newconn->connclass); free(newconn);
	error(_("allocation of text connection failed"));
	/* for Solaris 12.5 */ newconn = nullptr;
    }
    init_con(newconn, description, CE_NATIVE, mode);
    newconn->isopen = TRUE;
    newconn->canread = FALSE;
    newconn->open = &text_open;
    newconn->close = &outtext_close;
    newconn->destroy = &outtext_destroy;
    newconn->vfprintf = &text_vfprintf;
    newconn->seek = &text_seek;
    newconn->connprivate = (void*) malloc(sizeof(struct outtextconn));
    if(!newconn->connprivate) {
	free(newconn->description); free(newconn->connclass); free(newconn);
	error(_("allocation of text connection failed"));
	/* for Solaris 12.5 */ newconn = nullptr;
    }
    tmp = malloc(LAST_LINE_LEN);
    ((Routtextconn) newconn->connprivate)->lastline = (char*) tmp;
    if(!tmp) {
	free(newconn->connprivate);
	free(newconn->description); free(newconn->connclass); free(newconn);
	error(_("allocation of text connection failed"));
	/* for Solaris 12.5 */ newconn = nullptr;
    }
    outtext_init(newconn, stext, mode, idx);
    return newconn;
}

// .Internal(textConnection(name, object, open, env, type))
HIDDEN SEXP do_textconnection(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP sdesc, stext, sopen, ans, classs, venv;
    const char *desc, *open;
    int ncon, type;
    Rconnection con = nullptr;

    checkArity(op, args);
    sdesc = CAR(args);
    if(!isString(sdesc) || LENGTH(sdesc) != 1 ||
       STRING_ELT(sdesc, 0) == NA_STRING)
	error(_("invalid '%s' argument"), "description");
    desc = translateChar(STRING_ELT(sdesc, 0));
    stext = CADR(args); // object
    sopen = CADDR(args);
    if(!isString(sopen) || LENGTH(sopen) != 1)
	error(_("invalid '%s' argument"), "open");
    open = CHAR(STRING_ELT(sopen, 0)); /* ASCII */
    venv = CADDDR(args);
    if (isNull(venv))
	error(_("use of NULL environment is defunct"));
    if (!isEnvironment(venv))
	error(_("invalid '%s' argument"), "environment");
    type = asInteger(CAD4R(args));
    if (type == NA_INTEGER)
	error(_("invalid '%s' argument"), "encoding");
    ncon = Rconn::NextConnection();
    if(!strlen(open) || streqln(open, "r", 1)) {
	if(!isString(stext))
	    error(_("invalid '%s' argument"), "text");
	con = Connections[ncon] = newtext(desc, stext, type);
    } else if (streqln(open, "w", 1) || streqln(open, "a", 1)) {
	if (OutTextData == nullptr) {
	    OutTextData = allocVector(VECSXP, NCONNECTIONS);
	    R_PreserveObject(OutTextData);
	}
	SET_VECTOR_ELT(OutTextData, ncon, venv);
	if(stext == R_NilValue)
	    con = Connections[ncon] = newouttext("NULL", stext, open, ncon);
	else if(isString(stext) && LENGTH(stext) == 1)
	    con = Connections[ncon] =
		newouttext(translateChar(STRING_ELT(stext, 0)), stext,
			   open, ncon);
	else
	    error(_("invalid '%s' argument"), "text");
    }
    else
	error(_("unsupported mode"));
    /* already opened */

    PROTECT(ans = ScalarInteger(ncon));
    PROTECT(classs = allocVector(STRSXP, 2));
    SET_STRING_ELT(classs, 0, mkChar("textConnection"));
    SET_STRING_ELT(classs, 1, mkChar("connection"));
    classgets(ans, classs);
    con->ex_ptr = R_MakeExternalPtr(con->id, install("connection"), R_NilValue);
    setAttrib(ans, R_ConnIdSymbol, (SEXP) con->ex_ptr);
    R_RegisterCFinalizerEx((SEXP) con->ex_ptr, conFinalizer, FALSE);
    UNPROTECT(2);
    return ans;
}

HIDDEN SEXP do_textconvalue(SEXP call, SEXP op, SEXP args, SEXP env)
{
    Rconnection con = nullptr;
    Routtextconn thisconn;

    checkArity(op, args);
    con = getConnectionCheck(CAR(args), "textConnection", "con");
    if (!con->canwrite)
        error(_("'%s' is not an output of class %s"), "con", "\"rawConnection\"");
    thisconn = (Routtextconn)con->connprivate;
    return thisconn->data;
}

/* ------------------- socket connections  --------------------- */


/* socketConnection(host, port, server, blocking, open, encoding, timeout) */
/* socketAccept(socket, blocking, open, encoding, timeout) */
HIDDEN SEXP do_sockconn(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP scmd, sopen, ans, concl, enc;
    const char *host, *open;
    int ncon, port, server, blocking, timeout, serverfd;
    Rconnection con = nullptr;
    Rservsockconn scon = nullptr;

    checkArity(op, args);
    if (PRIMVAL(op) == 0) { /* socketConnection */
	scmd = CAR(args);
	if(!isString(scmd) || LENGTH(scmd) != 1)
	    error(_("invalid '%s' argument"), "host");
	host = translateCharFP(STRING_ELT(scmd, 0));
	args = CDR(args);
	port = asInteger(CAR(args));
	if(port == NA_INTEGER || port < 0)
	    error(_("invalid '%s' argument"), "port");
	args = CDR(args);
	server = asLogical(CAR(args));
	if(server == NA_LOGICAL)
	    error(_("invalid '%s' argument"), "server");
	serverfd = -1;
    } else { /* socketAccept */
	scon = (Rservsockconn) getConnectionCheck(CAR(args), "servsockconn", "socket")->connprivate;
	port = scon->port;
	server = 1;
	host = "localhost"; /* ignored */
	serverfd = scon->fd;
    }
    args = CDR(args);
    blocking = asLogical(CAR(args));
    if(blocking == NA_LOGICAL)
	error(_("invalid '%s' argument"), "blocking");
    args = CDR(args);
    sopen = CAR(args);
    if(!isString(sopen) || LENGTH(sopen) != 1)
	error(_("invalid '%s' argument"), "open");
    open = CHAR(STRING_ELT(sopen, 0)); /* ASCII */
    args = CDR(args);
    enc = CAR(args);
    if(!isString(enc) || LENGTH(enc) != 1 ||
       strlen(CHAR(STRING_ELT(enc, 0))) > 100) /* ASCII */
	error(_("invalid '%s' argument"), "encoding");
    args = CDR(args);
    timeout = asInteger(CAR(args));

    ncon = Rconn::NextConnection();
    con = R_newsock(host, port, server, serverfd, open, timeout);
    Connections[ncon] = con;
    con->blocking = (Rboolean) blocking;
    strncpy(con->encname, CHAR(STRING_ELT(enc, 0)), 100); /* ASCII */
    con->encname[100 - 1] = '\0';
    con->ex_ptr = PROTECT(R_MakeExternalPtr(con->id, install("connection"), R_NilValue));

    /* open it if desired */
    if(strlen(open)) {
	Rboolean success = con->open(con);
	if(!success) {
	    con_destroy(ncon);
	    error(_("cannot open the connection"));
	}
    }

    PROTECT(ans = ScalarInteger(ncon));
    PROTECT(concl = allocVector(STRSXP, 2));
    SET_STRING_ELT(concl, 0, mkChar("sockconn"));
    SET_STRING_ELT(concl, 1, mkChar("connection"));
    classgets(ans, concl);
    setAttrib(ans, R_ConnIdSymbol, (SEXP) con->ex_ptr);
    R_RegisterCFinalizerEx((SEXP) con->ex_ptr, conFinalizer, FALSE);
    UNPROTECT(3);
    return ans;
}

/* ------------------- unz connections  --------------------- */

/* .Internal(unz(paste(description, filename, sep = ":"),
 *               open, encoding)) */
HIDDEN SEXP do_unz(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP sfile, sopen, ans, classs, enc;
    const char *file, *open;
    int ncon;
    Rconnection con = nullptr;

    checkArity(op, args);
    sfile = CAR(args);
    if(!isString(sfile) || LENGTH(sfile) != 1 ||
       STRING_ELT(sfile, 0) == NA_STRING)
	error(_("invalid '%s' argument"), "description");
    if(length(sfile) > 1)
	warning(_("only first element of '%s' argument will be used"), "description");
    file = translateCharFP(STRING_ELT(sfile, 0));
    sopen = CADR(args);
    if(!isString(sopen) || LENGTH(sopen) != 1)
	error(_("invalid '%s' argument"), "open");
    enc = CADDR(args);
    if(!isString(enc) || LENGTH(enc) != 1 ||
       strlen(CHAR(STRING_ELT(enc, 0))) > 100) /* ASCII */
	error(_("invalid '%s' argument"), "encoding");
    open = CHAR(STRING_ELT(sopen, 0)); /* ASCII */
    ncon = Rconn::NextConnection();
    con = Connections[ncon] = R_newunz(file, strlen(open) ? open : "r"); // see dounzip.cpp for the details

    strncpy(con->encname, CHAR(STRING_ELT(enc, 0)), 100); /* ASCII */
    con->encname[100 - 1] = '\0';
    con->ex_ptr = PROTECT(R_MakeExternalPtr(con->id, install("connection"), R_NilValue));

    /* open it if desired */
    if(strlen(open)) {
	Rboolean success = con->open(con);
	if(!success) {
	    con_destroy(ncon);
	    error(_("cannot open the connection"));
	}
    }

    PROTECT(ans = ScalarInteger(ncon));
    PROTECT(classs = allocVector(STRSXP, 2));
    SET_STRING_ELT(classs, 0, mkChar("unz"));
    SET_STRING_ELT(classs, 1, mkChar("connection"));
    classgets(ans, classs);
    setAttrib(ans, R_ConnIdSymbol, (SEXP) con->ex_ptr);
    R_RegisterCFinalizerEx((SEXP) con->ex_ptr, conFinalizer, FALSE);
    UNPROTECT(3);

    return ans;
}

/* -------------- open, close, seek, truncate, flush ------------------ */

HIDDEN SEXP do_open(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int i, block;
    Rconnection con=nullptr;
    SEXP sopen;
    const char *open;
    Rboolean success;

    checkArity(op, args);
    if(!inherits(CAR(args), "connection"))
	error(_("'%s' argument is not an object of class %s"), "con", "\"connection\"");
    i = asInteger(CAR(args));
    con = getConnection(i);
    if(i < 3) error(_("cannot open standard connections"));
    if(con->isopen) {
	warning(_("connection is already open"));
	return R_NilValue;
    }
    sopen = CADR(args);
    if(!isString(sopen) || LENGTH(sopen) != 1)
	error(_("invalid '%s' argument"), "open");
    block = asLogical(CADDR(args));
    if(block == NA_LOGICAL)
	error(_("invalid '%s' argument"), "blocking");
    open = CHAR(STRING_ELT(sopen, 0)); /* ASCII */
    if(strlen(open) > 0) strcpy(con->mode, open);
    con->blocking = (Rboolean) block;
    success = con->open(con);
    if(!success) {
	/* con_destroy(i); user might have a reference */
	error(_("cannot open the connection"));
    }
    return R_NilValue;
}

HIDDEN SEXP do_isopen(SEXP call, SEXP op, SEXP args, SEXP env)
{
    Rconnection con;
    int rw, res;

    checkArity(op, args);
    con = getConnection(asInteger(CAR(args)));
    rw = asInteger(CADR(args));
    res = con->isopen != FALSE;
    switch(rw) {
    case 0: break;
    case 1: res = res & con->canread; break;
    case 2: res = res & con->canwrite; break;
    default: error(_("unknown 'rw' value"));
    }
    return ScalarLogical(res);
}

HIDDEN SEXP do_isincomplete(SEXP call, SEXP op, SEXP args, SEXP env)
{
    Rconnection con;

    checkArity(op, args);
    if(!inherits(CAR(args), "connection"))
	error(_("'%s' argument is not an object of class %s"), "con", "\"connection\"");
    con = getConnection(asInteger(CAR(args)));
    return ScalarLogical(con->incomplete != FALSE);
}

HIDDEN SEXP do_isseekable(SEXP call, SEXP op, SEXP args, SEXP env)
{
    Rconnection con;

    checkArity(op, args);
    if (!inherits(CAR(args), "connection"))
        error(_("'%s' argument is not an object of class %s"), "con", "\"connection\"");
    con = getConnection(asInteger(CAR(args)));
    return ScalarLogical(con->canseek != FALSE);
}

void Rconn::checkClose()
{
    if (this->isopen)
    {
        errno = 0;
        this->close(this);
        if (this->status != NA_INTEGER && this->status < 0)
        {
            int serrno = errno;
            if (serrno)
                warning(_("Problem closing connection:  %s"), strerror(serrno));
            else
                warning(_("Problem closing connection"));
        }
    }
}

static int con_close1(Rconnection con)
{
    int status;
    con->checkClose();
    status = con->status;
    if(con->isGzcon) {
	Rgzconn priv = (Rgzconn) con->connprivate;
	con_close1(priv->con);
	R_ReleaseObject((SEXP) priv->con->ex_ptr);
    }
    /* close inconv and outconv if open */
    if(con->inconv) Riconv_close(con->inconv);
    if(con->outconv) Riconv_close(con->outconv);
    con->destroy(con);
    free(con->connclass);
    con->connclass = nullptr;
    free(con->description);
    con->description = nullptr;
    /* clear the pushBack */
    if(con->nPushBack > 0) { // already cleared on closed connection,
	                     // so no double-free pace -fanalyzer

	for(size_t j = 0; j < con->nPushBack; j++)
	    free(con->PushBack[j]);
	free(con->PushBack);
    }
    con->nPushBack = 0;
    if (con->buff) {
	free(con->buff);
	con->buff = nullptr;
    }
    con->buff_len = con->buff_pos = con->buff_stored_len = 0;
    con->open = &null_open;
    con->close = &null_close;
    con->destroy = &null_destroy;
    con->vfprintf = &null_vfprintf;
    con->fgetc = con->fgetc_internal = &null_fgetc;
    con->seek = &null_seek;
    con->truncate = &null_truncate;
    con->fflush = &null_fflush;
    con->read = &null_read;
    con->write = &null_write;
    return status;
}


static void con_destroy(int i)
{
    Rconnection con = nullptr;

    con = getConnection(i);
    con_close1(con);
    free(Connections[i]);
    Connections[i] = nullptr;
}


HIDDEN SEXP do_close(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int i, j;

    checkArity(op, args);
    if(!inherits(CAR(args), "connection"))
	error(_("'%s' argument is not an object of class %s"), "con", "\"connection\"");
    i = asInteger(CAR(args));
    if(i < 3) error(_("cannot close standard connections"));
    for(j = 0; j < R_SinkNumber; j++)
	if(i == SinkCons[j])
	    error(_("cannot close 'output' sink connection"));
    if(i == R_ErrorCon)
	error(_("cannot close 'message' sink connection"));
    Rconnection con = getConnection(i);
    int status = con_close1(con);
    free(Connections[i]);
    Connections[i] = nullptr;
    return (status != NA_INTEGER) ? ScalarInteger(status) : R_NilValue;
}

static double Rconn_seek(Rconnection con, double where, int origin, int rw) {
    if (con->buff)
	return buff_seek(con, where, origin, rw);
    return con->seek(con, where, origin, rw);
}

/* seek(con, where = numeric(), origin = "start", rw = "") */
HIDDEN SEXP do_seek(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int origin, rw;
    Rconnection con = nullptr;
    double where;

    checkArity(op, args);
    if(!inherits(CAR(args), "connection"))
	error(_("'%s' argument is not an object of class %s"), "con", "\"connection\"");
    con = getConnection(asInteger(CAR(args)));
    if(!con->isopen) error(_("connection is not open"));
    where = asReal(CADR(args));
    origin = asInteger(CADDR(args));
    rw = asInteger(CADDDR(args));
    if(!ISNAN(where) && con->nPushBack > 0) {
	/* clear pushback */
	for(size_t j = 0; j < con->nPushBack; j++) free(con->PushBack[j]);
	free(con->PushBack);
	con->nPushBack = 0;
    }
    return ScalarReal(Rconn_seek(con, where, origin, rw));
}

/* truncate(con) */
HIDDEN SEXP do_truncate(SEXP call, SEXP op, SEXP args, SEXP env)
{
    Rconnection con = nullptr;

    checkArity(op, args);
    if(!inherits(CAR(args), "connection"))
	error(_("'%s' argument is not an object of class %s"), "con", "\"connection\"");
    con = getConnection(asInteger(CAR(args)));
    con->truncate(con);
    return R_NilValue;
}

HIDDEN SEXP do_flush(SEXP call, SEXP op, SEXP args, SEXP env)
{
    Rconnection con = nullptr;

    checkArity(op, args);
    if(!inherits(CAR(args), "connection"))
	error(_("'%s' argument is not an object of class %s"), "con", "\"connection\"");
    con = getConnection(asInteger(CAR(args)));
    if(con->canwrite) con->fflush(con);
    return R_NilValue;
}

/* ------------------- read, write  text --------------------- */

int Rconn_fgetc(Rconnection con)
{
    char *curLine;
    int c;

    if (con->save2 != -1000) {
	c = con->save2;
	con->save2 = -1000;
	return c;
    }
    if(con->nPushBack <= 0) {
	/* map CR or CRLF to LF */
	if (con->save != -1000) {
	    c = con->save;
	    con->save = -1000;
	    return c;
	}
	c = con->fgetc(con);
	if (c == '\r') {
	    c = con->fgetc(con);
	    if (c != '\n') {
		con->save = (c != '\r') ? c : '\n';
		return('\n');
	    }
	}
	return c;
    }
    curLine = con->PushBack[con->nPushBack-1];
    c = (unsigned char) curLine[con->posPushBack++];
    if(con->posPushBack >= strlen(curLine)) {
	/* last character on a line, so pop the line */
	free(curLine);
	con->nPushBack--;
	con->posPushBack = 0;
	if(con->nPushBack == 0) free(con->PushBack);
    }
    return c;
}

#ifdef UNUSED
int Rconn_ungetc(int c, Rconnection con)
{
    con->save2 = c;
    return c;
}
#endif

/* read one line (without trailing newline) from con and store it in buf */
/* return number of characters read, -1 on EOF */
HIDDEN
size_t Rconn_getline(Rconnection con, char *buf, size_t bufsize)
{
    int c;
    ssize_t nbuf = -1;

    while((c = Rconn_fgetc(con)) != R_EOF) {
	if((size_t) nbuf + 1 >= bufsize) error(_("line is longer than buffer size %lu"), (unsigned long) bufsize);
	if(c != '\n'){
	    buf[++nbuf] = (char) c; /* compiler-defined conversion behavior */
	} else {
	    buf[++nbuf] = '\0';
	    break;
	}
    }
    /* Make sure it is null-terminated and count is correct, even if
     *  file did not end with newline.
     */
    if(nbuf >= 0 && buf[nbuf]) {
	if((size_t) nbuf+1 >= bufsize) error(_("line is longer than buffer size %lu"), (unsigned long) bufsize);
	buf[++nbuf] = '\0';
    }
    return (size_t) nbuf;
}

int Rconn_printf(Rconnection con, const char *format, ...)
{
    int res;
    errno = 0;
    va_list ap;
    va_start(ap, format);
    /* Parentheses added for FC4 with gcc4 and -D_FORTIFY_SOURCE=2 */
    res = (con->vfprintf)(con, format, ap);
    va_end(ap);
    /* PR#17243:  write.table and friends silently failed if the disk was full (or there was another error) */
    if (res < 0) {
	if (errno)
	    error(_("Error writing to connection:  %s"), strerror(errno));
	else
	    error(_("Error writing to connection"));
    }
    return res;
}

static void con_cleanup(void *data)
{
    Rconnection con = (Rconnection) data;
    con->checkClose();
}

/* readLines(con = stdin(), n = 1, ok = TRUE, warn = TRUE) */
constexpr size_t BUF_SIZE = 1000;
HIDDEN SEXP do_readLines(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans = R_NilValue, ans2;
    int ok, warn, skipNul, c;
    size_t nbuf, buf_size = BUF_SIZE;
    cetype_t oenc = CE_NATIVE;
    Rconnection con = nullptr;
    bool wasopen;
    char *buf;
    const char *encoding;
    RCNTXT cntxt;
    R_xlen_t i, n, nn, nnn, nread;

    checkArity(op, args);
    if(!inherits(CAR(args), "connection"))
	error(_("'%s' argument is not an object of class %s"), "con", "\"connection\"");
    con = getConnection(asInteger(CAR(args))); args = CDR(args);
    n = asVecSize(CAR(args)); args = CDR(args);
    if(n == -999)
	error(_("invalid '%s' argument"), "n");
    ok = asLogical(CAR(args));  args = CDR(args);
    if(ok == NA_LOGICAL)
	error(_("invalid '%s' argument"), "ok");
    warn = asLogical(CAR(args));  args = CDR(args);
    if(warn == NA_LOGICAL)
	error(_("invalid '%s' argument"), "warn");
    if(!isString(CAR(args)) || LENGTH(CAR(args)) != 1)
	error(_("invalid '%s' value"), "encoding");
    encoding = CHAR(STRING_ELT(CAR(args), 0));  args = CDR(args); /* ASCII */
    skipNul = asLogical(CAR(args));
    if(skipNul == NA_LOGICAL)
	error(_("invalid '%s' argument"), "skipNul");

    wasopen = con->isopen;
    if(!wasopen) {
	char mode[5];
	con->UTF8out = TRUE;  /* a request */
	strcpy(mode, con->mode);
	strcpy(con->mode, "rt");
	if(!con->open(con)) error(_("cannot open the connection"));
	strcpy(con->mode, mode);
	/* Set up a context which will close the connection on error */
	cntxt.start(CTXT_CCODE, R_NilValue, R_BaseEnv, R_BaseEnv, R_NilValue, R_NilValue);
	cntxt.setContextEnd(&con_cleanup, con);
	if(!con->canread) error(_("cannot read from this connection"));
    } else {
	if(!con->canread) error(_("cannot read from this connection"));
	/* for a non-blocking connection, more input may
	   have become available, so re-position */
	if(con->canseek && !con->blocking)
	    Rconn_seek(con, Rconn_seek(con, -1, 1, 1), 1, 1);
    }
    con->incomplete = FALSE;
    if(con->UTF8out || streql(encoding, "UTF-8")) oenc = CE_UTF8;
    else if(streql(encoding, "latin1")) oenc = CE_LATIN1;

    buf = (char *) malloc(buf_size);
    if(!buf)
	error(_("cannot allocate buffer in 'readLines()'"));
    nn = (n < 0) ? 1000 : n; /* initially allocate space for 1000 lines */
    nnn = (n < 0) ? R_XLEN_T_MAX : n;
    PROTECT(ans = allocVector(STRSXP, nn));
    for(nread = 0; nread < nnn; nread++) {
	if(nread >= nn) {
	    double dnn = 2.* nn;
	    if (dnn > (double) R_XLEN_T_MAX) error(_("too many items"));
	    ans2 = allocVector(STRSXP, 2*nn);
	    for(i = 0; i < nn; i++)
		SET_STRING_ELT(ans2, i, STRING_ELT(ans, i));
	    nn *= 2;
	    UNPROTECT(1); /* old ans */
	    PROTECT(ans = ans2);
	}
	nbuf = 0;
	while((c = Rconn_fgetc(con)) != R_EOF) {
	    if(nbuf == buf_size-1) {  /* need space for the terminator */
		buf_size *= 2;
		char *tmp = (char *) realloc(buf, buf_size);
		if(!buf) {
		    free(buf);
		    error(_("cannot allocate buffer in 'readLines()'"));
		} else buf = tmp;
	    }
	    if(skipNul && c == '\0') continue;
	    if(c != '\n')
		/* compiler-defined conversion behavior */
		buf[nbuf++] = (char) c;
	    else
		break;
	}
	buf[nbuf] = '\0';
	/* Remove UTF-8 BOM */
	const char *qbuf = buf;
	// avoid valgrind warning if < 3 bytes
	if (nread == 0 && utf8locale && strlen(buf) >= 3 &&
	    !memcmp(buf, "\xef\xbb\xbf", 3)) qbuf = buf + 3;
	SET_STRING_ELT(ans, nread, mkCharCE(qbuf, oenc));
	if (warn && strlen(buf) < nbuf)
	    warning(_("line %d appears to contain an embedded nul"), nread + 1);
	if(c == R_EOF) goto no_more_lines;
    }

    UNPROTECT(1);
    if (!wasopen)
    {
        cntxt.end();
        con->close(con);
    }
    free(buf);
    return ans;
no_more_lines:
    if(nbuf > 0) { /* incomplete last line */
	if(con->text && !con->blocking &&
	   (strcmp(con->connclass, "gzfile") != 0)) {
	    /* push back the rest */
	    con_pushback(con, FALSE, buf);
	    con->incomplete = TRUE;
	} else {
	    nread++;
	    if(warn)
		warning(_("incomplete final line found on '%s'"), con->description);
	}
    }
    free(buf);
    if(nread < nnn && !ok)
	error(_("too few lines read in 'readLines()'"));
    PROTECT(ans2 = allocVector(STRSXP, nread));
    for(i = 0; i < nread; i++)
	SET_STRING_ELT(ans2, i, STRING_ELT(ans, i));
    UNPROTECT(2);
    if (!wasopen)
    {
        cntxt.end();
        con->close(con);
    }
    return ans2;
}

/* writeLines(text, con = stdout(), sep = "\n", useBytes) */
HIDDEN SEXP do_writelines(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int con_num, useBytes;
    bool wasopen;
    Rconnection con = nullptr;
    const char *ssep;
    SEXP text, sep;
    RCNTXT cntxt;

    checkArity(op, args);
    text = CAR(args);
    if(!isString(text)) error(_("invalid '%s' argument"), "text");
    if(!inherits(CADR(args), "connection"))
	error(_("'%s' argument is not an object of class %s"), "con", "\"connection\"");
    con_num = asInteger(CADR(args));
    con = getConnection(con_num);
    sep = CADDR(args);
    if(!isString(sep)) error(_("invalid '%s' argument"), "sep");
    useBytes = asLogical(CADDDR(args));
    if(useBytes == NA_LOGICAL)
	error(_("invalid '%s' argument"), "useBytes");

    wasopen = con->isopen;
    if(!wasopen) {
	char mode[5];
	/* Documented behaviour */
	strcpy(mode, con->mode);
	strcpy(con->mode, "wt");
	if(!con->open(con)) error(_("cannot open the connection"));
	strcpy(con->mode, mode);
	/* Set up a context which will close the connection on error */
	cntxt.start(CTXT_CCODE, R_NilValue, R_BaseEnv, R_BaseEnv, R_NilValue, R_NilValue);
	cntxt.setContextEnd(&con_cleanup, con);
    }
    if(!con->canwrite) error(_("cannot write to this connection"));
    /* NB: translateChar0() is the same as CHAR() for IS_BYTES strings */
    if(useBytes)
	ssep = CHAR(STRING_ELT(sep, 0));
    else
	ssep = translateChar0(STRING_ELT(sep, 0));

    /* New for 2.7.0: split the output if sink was split.
       It would be slightly simpler just to call Rvprintf if the
       connection was stdout(), but this way is more efficent */
    if(con_num == R_OutputCon) {
	int j = 0;
	Rconnection con0;
	do {
	    con0 = getConnection(con_num);
	    for(R_xlen_t i = 0; i < XLENGTH(text); i++)
		Rconn_printf(con0, "%s%s",
			     useBytes ? CHAR(STRING_ELT(text, i)) :
			     translateChar0(STRING_ELT(text, i)), ssep);
	    con0->fflush(con0);
	    con_num = getActiveSink(j++);
	} while (con_num > 0);
    } else {
	for(R_xlen_t i = 0; i < XLENGTH(text); i++)
	    Rconn_printf(con, "%s%s",
			 useBytes ? CHAR(STRING_ELT(text, i)) :
			 translateChar0(STRING_ELT(text, i)), ssep);
    }

    if(!wasopen) {
    	cntxt.end();
    	con->checkClose();
    }
    return R_NilValue;
}

/* ------------------- read, write  binary --------------------- */

static void swapb(void *result, int size)
{
    int i;
    char *p = (char *)result, tmp;

    if (size == 1)
        return;
    for (i = 0; i < size / 2; i++)
    {
        tmp = p[i];
        p[i] = p[size - i - 1];
        p[size - i - 1] = tmp;
    }
}

static SEXP readOneString(Rconnection con)
{
    char buf[10001], *p;
    int pos, m;

    for(pos = 0; pos < 10000; pos++) {
	p = buf + pos;
	m = (int) con->read(p, sizeof(char), 1, con);
	if (m < 0) error(_("error reading from the connection"));
	if(!m) {
	    if(pos > 0)
		warning(_("incomplete string at end of file has been discarded"));
	    return R_NilValue;
	}
	if(*p == '\0') break;
    }
    if(pos == 10000)
	warning(_("null terminator was not found: breaking string at 10000 bytes"));
    return mkChar(buf);
}

static R_xlen_t rawRead(char *p, int size, R_xlen_t n, Rbyte *bytes, R_xlen_t nbytes, R_xlen_t *np)
{
    R_xlen_t avail, m;

    avail = (nbytes - *np)/size;
    m = n;
    if (m > avail) m = avail;
    if (m > 0) {
	memcpy(p, bytes + *(np), m*size);
	*np += m*size;
    }
    return m;
}

static SEXP rawOneString(Rbyte *bytes, R_xlen_t nbytes, R_xlen_t *np)
{
    Rbyte *p;
    R_xlen_t i;
    char *buf;
    SEXP res;

    /* just look for null terminator */
    for(i = *np, p = bytes+(*np); i < nbytes; p++, i++)
	if(*p == '\0') break;
    if(i < nbytes) { /* has terminator */
	p = bytes+(*np);
	*np = i+1;
	return mkChar((char *) p);
    }
    /* so no terminator */
    buf = (char*) R_chk_calloc(nbytes - (*np) + 1, 1);
    memcpy(buf, bytes+(*np), nbytes-(*np));
    res = mkChar(buf);
    Free(buf);
    *np = nbytes;
    return res;
}

/* readBin(con, what, n, swap) */
constexpr int BLOCK = 8096;

HIDDEN SEXP do_readbin(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans = R_NilValue, swhat;
    int size, signd, swap, sizedef = 4, mode = 1;
    const char *what;
    void *p = nullptr;
    bool wasopen = TRUE, isRaw = FALSE;
    Rconnection con = nullptr;
    Rbyte *bytes = nullptr;
    RCNTXT cntxt;
    R_xlen_t i, n,  m = 0, nbytes = 0, np = 0;

    checkArity(op, args);

    if(TYPEOF(CAR(args)) == RAWSXP) {
	isRaw = TRUE;
	bytes = RAW(CAR(args));
	nbytes = XLENGTH(CAR(args));
    } else {
	con = getConnection(asInteger(CAR(args)));
	if(con->text) error(_("can only read from a binary connection"));
    }

    args = CDR(args);
    swhat = CAR(args); args = CDR(args);
    if(!isString(swhat) || LENGTH(swhat) != 1)
	error(_("invalid '%s' argument"), "what");
    what = CHAR(STRING_ELT(swhat, 0)); /* ASCII */
    n = asVecSize(CAR(args)); args = CDR(args);
    if(n < 0) error(_("invalid '%s' argument"), "n");
    size = asInteger(CAR(args)); args = CDR(args);
    signd = asLogical(CAR(args)); args = CDR(args);
    if(signd == NA_LOGICAL)
	error(_("invalid '%s' argument"), "signed");
    swap = asLogical(CAR(args));
    if(swap == NA_LOGICAL)
	error(_("invalid '%s' argument"), "swap");
    if(!isRaw) {
	wasopen = con->isopen;
	if(!wasopen) {
	    /* Documented behaviour */
	    char mode[5];
	    strcpy(mode, con->mode);
	    strcpy(con->mode, "rb");
	    if(!con->open(con)) error(_("cannot open the connection"));
	    strcpy(con->mode, mode);
	    /* Set up a context which will close the connection on error */
	    cntxt.start(CTXT_CCODE, R_NilValue, R_BaseEnv, R_BaseEnv, R_NilValue, R_NilValue);
	    cntxt.setContextEnd(&con_cleanup, con);
	}
	if(!con->canread) error(_("cannot read from this connection"));
    }
    if(streql(what, "character")) {
	SEXP onechar;
	PROTECT(ans = allocVector(STRSXP, n));
	for(i = 0, m = 0; i < n; i++) {
	    onechar = isRaw ? rawOneString(bytes, nbytes, &np)
		: readOneString(con);
	    if(onechar != R_NilValue) {
		SET_STRING_ELT(ans, i, onechar);
		m++;
	    } else break;
	}
    } else if(streql(what, "complex")) {
	if(size == NA_INTEGER) size = sizeof(Rcomplex);
	if(size != sizeof(Rcomplex))
	    error(_("size changing is not supported for complex vectors"));
	PROTECT(ans = allocVector(CPLXSXP, n));
	p = (void *) COMPLEX(ans);
	if(isRaw) m = rawRead((char*) p, size, n, bytes, nbytes, &np);
	else {
	    /* Do this in blocks to avoid large buffers in the connection */
	    char *pp = (char*) p;
	    R_xlen_t m0, n0 = n;
	    m = 0;
	    while(n0) {
		size_t n1 = (n0 < BLOCK) ? n0 : BLOCK;
		m0 = con->read(pp, size, n1, con);
		if (m0 < 0) error(_("error reading from the connection"));
		m += m0;
		if ((size_t) m0 < n1) break;
		n0 -= n1;
		pp += n1 * size;
	    }
	}
	if(swap)
	    for(i = 0; i < m; i++) {
		swapb(&(COMPLEX(ans)[i].r), sizeof(double));
		swapb(&(COMPLEX(ans)[i].i), sizeof(double));
	    }
    } else {
	if (streql(what, "integer") || streql(what, "int")) {
	    sizedef = sizeof(int); mode = 1;

#if (SIZEOF_LONG == 8) && (SIZEOF_LONG > SIZEOF_INT)
#  define CASE_LONG_ETC case sizeof(long):
#elif (SIZEOF_LONG_LONG == 8) && (SIZEOF_LONG_LONG > SIZEOF_INT)
#  define CASE_LONG_ETC case sizeof(_lli_t):
#else
#  define CASE_LONG_ETC
#endif

#define CHECK_INT_SIZES(SIZE, DEF)                                \
    do                                                            \
    {                                                             \
        if (SIZE == NA_INTEGER)                                   \
            SIZE = DEF;                                           \
        switch (SIZE)                                             \
        {                                                         \
        case sizeof(signed char):                                 \
        case sizeof(short):                                       \
        case sizeof(int):                                         \
            CASE_LONG_ETC                                         \
            break;                                                \
        default:                                                  \
            error(_("size %d is unknown on this machine"), SIZE); \
        }                                                         \
    } while (0)

        CHECK_INT_SIZES(size, sizedef);
	    PROTECT(ans = allocVector(INTSXP, n));
	    p = (void *) INTEGER(ans);
	} else if (streql(what, "logical")) {
	    sizedef = sizeof(int); mode = 1;
	    CHECK_INT_SIZES(size, sizedef);
	    PROTECT(ans = allocVector(LGLSXP, n));
	    p = (void *) LOGICAL(ans);
	} else if (streql(what, "raw")) {
	    sizedef = 1; mode = 1;
	    if(size == NA_INTEGER) size = sizedef;
	    switch (size) {
	    case 1:
		break;
	    default:
		error(_("raw is always of size 1"));
	    }
	    PROTECT(ans = allocVector(RAWSXP, n));
	    p = (void *) RAW(ans);
	} else if (streql(what, "numeric") || streql(what, "double")) {
	    sizedef = sizeof(double); mode = 2;
	    if(size == NA_INTEGER) size = sizedef;
	    switch (size) {
	    case sizeof(double):
	    case sizeof(float):
#if HAVE_LONG_DOUBLE && (SIZEOF_LONG_DOUBLE > SIZEOF_DOUBLE)
	    case sizeof(long double):
#endif
		break;
	    default:
		error(_("size %d is unknown on this machine"), size);
	    }
	    PROTECT(ans = allocVector(REALSXP, n));
	    p = (void *) REAL(ans);
	} else
	    error(_("invalid '%s' argument"), "what");

	if(!signd && (mode != 1 || size > 2))
	    warning(_("'signed = FALSE' is only valid for integers of sizes 1 and 2"));
	if(size == sizedef) {
	    if(isRaw) {
		m = rawRead((char*) p, size, n, bytes, nbytes, &np);
	    } else {
		/* Do this in blocks to avoid large buffers in the connection */
		char *pp = (char*) p;
		R_xlen_t m0, n0 = n;
		m = 0;
		while(n0) {
		    size_t n1 = (n0 < BLOCK) ? n0 : BLOCK;
		    m0 = con->read(pp, size, n1, con);
		    if (m0 < 0) error(_("error reading from the connection"));
		    m += m0;
		    if ((size_t) m0 < n1) break;
		    n0 -= n1;
		    pp += n1 * size;
		}
	    }
	    if(swap && size > 1)
		for(i = 0; i < m; i++) swapb((char *)p+i*size, size);
	} else {
        R_xlen_t s;
        union
        {
            signed char sch;
            unsigned char uch;
            signed short ssh;
            unsigned short ush;
            long l;
            long long ll;
            float f;
#if HAVE_LONG_DOUBLE
            long double ld;
#endif
        } u;
        if ((size_t) size > sizeof u)
		error(_("size %d is unknown on this machine"), size);
	    if(mode == 1) { /* integer result */
		for(i = 0, m = 0; i < n; i++) {
		    s = isRaw ? rawRead((char*) &u, size, 1, bytes, nbytes, &np)
			: (int) con->read((char*) &u, size, 1, con);
		    if (s < 0) error(_("error reading from the connection"));
		    if(s) m++; else break;
		    if(swap && size > 1) swapb((char *) &u, size);
		    switch(size) {
		    case sizeof(signed char):
			if(signd)
			    INTEGER(ans)[i] = u.sch;
			else
			    INTEGER(ans)[i] = u.uch;
			break;
		    case sizeof(short):
			if(signd)
			    INTEGER(ans)[i] = u.ssh;
			else
			    INTEGER(ans)[i] = u.ush;
			break;
#if SIZEOF_LONG == 8
		    case sizeof(long):
			INTEGER(ans)[i] = (int) u.l;
			break;
#elif SIZEOF_LONG_LONG == 8
		    case sizeof(_lli_t):
			INTEGER(ans)[i] = (int) u.ll;
			break;
#endif
		    default:
			error(_("size %d is unknown on this machine"), size);
		    }
		}
	    } else if (mode == 2) { /* double result */
		for(i = 0, m = 0; i < n; i++) {
		    s = isRaw ? rawRead((char*) &u, size, 1, bytes, nbytes, &np)
			: (int) con->read((char*) &u, size, 1, con);
		    if (s < 0) error(_("error reading from the connection"));
		    if(s) m++; else break;
		    if(swap && size > 1) swapb((char *) &u, size);
		    switch(size) {
		    case sizeof(float):
			REAL(ans)[i] = u.f;
			break;
#if HAVE_LONG_DOUBLE && (SIZEOF_LONG_DOUBLE > SIZEOF_DOUBLE)
		    case sizeof(long double):
			REAL(ans)[i] = (double) u.ld;
			break;
#endif
		    default:
			error(_("size %d is unknown on this machine"), size);
		    }
		}
	    }
	}
    }
    if(!wasopen) {cntxt.end(); con->close(con);}
    if(m < n)
	ans = xlengthgets(ans, m);
    UNPROTECT(1);
    return ans;
}

/* writeBin(object, con, size, swap, useBytes) */
HIDDEN SEXP do_writebin(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    SEXP object = CAR(args);
    if(!isVectorAtomic(object))
	error(_("'x' argument is not an atomic vector type"));
    Rboolean
	isRaw = (Rboolean) (TYPEOF(CADR(args)) == RAWSXP),
	wasopen = isRaw;
    Rconnection con = nullptr;
    if(!isRaw) {
	con = getConnection(asInteger(CADR(args)));
	if(con->text) error(_("can only write to a binary connection"));
	wasopen = con->isopen;
	if(!con->canwrite) error(_("cannot write to this connection"));
    }

    int size = asInteger(CADDR(args)),
	swap = asLogical(CADDDR(args));
    if(swap == NA_LOGICAL)
	error(_("invalid '%s' argument"), "swap");
    int useBytes = asLogical(CAD4R(args));
    if(useBytes == NA_LOGICAL)
	error(_("invalid '%s' argument"), "useBytes");
    R_xlen_t i, len = XLENGTH(object);
    if(len == 0)
	return (isRaw) ? allocVector(RAWSXP, 0) : R_NilValue;

#ifndef LONG_VECTOR_SUPPORT
    /* without long vectors RAW vectors are limited to 2^31 - 1 bytes */
    if(len * (double)size > R_INT_MAX) {
	if(isRaw)
	    error(_("only 2^31-1 bytes can be written to a raw vector"));
	else
	    error(_("only 2^31-1 bytes can be written in a single 'writeBin()' call"));
    }
#endif

    RCNTXT cntxt;
    if(!wasopen) {
	/* Documented behaviour */
	char mode[5];
	strcpy(mode, con->mode);
	strcpy(con->mode, "wb");
	if(!con->open(con)) error(_("cannot open the connection"));
	strcpy(con->mode, mode);
	/* Set up a context which will close the connection on error */
	cntxt.start(CTXT_CCODE, R_NilValue, R_BaseEnv, R_BaseEnv, R_NilValue, R_NilValue);
	cntxt.setContextEnd(&con_cleanup, con);
	if(!con->canwrite) error(_("cannot write to this connection"));
    }

    SEXP ans = R_NilValue;
    if(TYPEOF(object) == STRSXP) {
	const char *s;
	if(isRaw) {
	    Rbyte *bytes;
	    size_t np, outlen = 0;
	    if(useBytes)
		for(i = 0; i < len; i++)
		    outlen += strlen(CHAR(STRING_ELT(object, i))) + 1;
	    else
		for(i = 0; i < len; i++)
		    outlen += strlen(translateChar0(STRING_ELT(object, i))) + 1;
	    PROTECT(ans = allocVector(RAWSXP, outlen));
	    bytes = RAW(ans);
	    /* translateChar0() is the same as CHAR for IS_BYTES strings */
	    for(i = 0, np = 0; i < len; i++) {
		if(useBytes)
		    s = CHAR(STRING_ELT(object, i));
		else
		    s = translateChar0(STRING_ELT(object, i));
		memcpy(bytes+np, s, strlen(s) + 1);
		np +=  strlen(s) + 1;
	    }
	} else {
	    /* translateChar0() is the same as CHAR for IS_BYTES strings */
	    for(i = 0; i < len; i++) {
		if(useBytes)
		    s = CHAR(STRING_ELT(object, i));
		else
		    s = translateChar0(STRING_ELT(object, i));
		size_t nwrite = con->write(s, sizeof(char), strlen(s) + 1, con);
		if(!nwrite) {
		    warning(_("problem with writing to connection"));
		    break;
		}
	    }
	}
    } else {
	switch(TYPEOF(object)) {
	case LGLSXP:
	case INTSXP:
	    CHECK_INT_SIZES(size, sizeof(int));
	    break;
	case REALSXP:
	    if(size == NA_INTEGER) size = sizeof(double);
	    switch (size) {
	    case sizeof(double):
	    case sizeof(float):
#if HAVE_LONG_DOUBLE && (SIZEOF_LONG_DOUBLE > SIZEOF_DOUBLE)
	    case sizeof(long double):
#endif
		break;
	    default:
		error(_("size %d is unknown on this machine"), size);
	    }
	    break;
	case CPLXSXP:
	    if(size == NA_INTEGER) size = sizeof(Rcomplex);
	    if(size != sizeof(Rcomplex))
		error(_("size changing is not supported for complex vectors"));
	    break;
	case RAWSXP:
	    if(size == NA_INTEGER) size = 1;
	    if(size != 1)
		error(_("size changing is not supported for raw vectors"));
	    break;
	default:
	    UNIMPLEMENTED_TYPE("writeBin()", object);
	}
	char *buf = (char*) R_chk_calloc(len, size);
	R_xlen_t j;
	switch(TYPEOF(object)) {
	case LGLSXP:
	case INTSXP:
	    switch (size) {
	    case sizeof(int):
		memcpy(buf, INTEGER(object), size * len);
		break;
#if SIZEOF_LONG == 8
	    case sizeof(long):
	    {
		for (i = 0, j = 0; i < len; i++, j += size) {
		    long l1 = (long) INTEGER(object)[i];
		    memcpy(buf + j, &l1, size);
		}
		break;
	    }
#elif SIZEOF_LONG_LONG == 8
	    case sizeof(_lli_t):
	    {
		for (i = 0, j = 0; i < len; i++, j += size) {
		    _lli_t ll1 = (_lli_t) INTEGER(object)[i];
		    memcpy(buf + j, &ll1, size);
		}
		break;
	    }
#endif
	    case 2:
	    {
		for (i = 0, j = 0; i < len; i++, j += size) {
		    short s1 = (short) INTEGER(object)[i];
		    memcpy(buf + j, &s1, size);
		}
		break;
	    }
	    case 1:
		for (i = 0; i < len; i++)
		    /* compiler-defined conversion behavior */
		    buf[i] = (signed char) INTEGER(object)[i];
		break;
	    default:
		error(_("size %d is unknown on this machine"), size);
	    }
	    break;
	case REALSXP:
	    switch (size) {
	    case sizeof(double):
		memcpy(buf, REAL(object), size * len);
		break;
	    case sizeof(float):
	    {
		for (i = 0, j = 0; i < len; i++, j += size) {
		    float f1 = (float) REAL(object)[i];
		    memcpy(buf+j, &f1, size);
		}
		break;
	    }
#if HAVE_LONG_DOUBLE && (SIZEOF_LONG_DOUBLE > SIZEOF_DOUBLE)
	    case sizeof(long double):
	    {
		/* some systems have problems with memcpy from
		   the address of an automatic long double,
		   e.g. ix86/x86_64 Linux with gcc4 */
		static long double ld1;
		for (i = 0, j = 0; i < len; i++, j += size) {
		    ld1 = (long double) REAL(object)[i];
		    memcpy(buf+j, &ld1, size);
		}
		break;
	    }
#endif
	    default:
		error(_("size %d is unknown on this machine"), size);
	    }
	    break;
	case CPLXSXP:
	    memcpy(buf, COMPLEX(object), size * len);
	    break;
	case RAWSXP:
	    memcpy(buf, RAW(object), len); /* size = 1 */
	    break;
	default:  // -Wswitch
		break;
	}

	if(swap && size > 1) {
	    if (TYPEOF(object) == CPLXSXP)
		for(i = 0; i < len; i++) {
		    int sz = size/2;
		    swapb(buf+sz*2*i, sz);
		    swapb(buf+sz*(2*i+1), sz);
		}
	    else
		for(i = 0; i < len; i++) swapb(buf+size*i, size);
	}

	/* write it now */
	if(isRaw) { /* for non-long vectors, we checked size*len < 2^31-1 above */
	    PROTECT(ans = allocVector(RAWSXP, size*len));
	    memcpy(RAW(ans), buf, size*len);
	} else {
	    size_t nwrite = con->write(buf, size, len, con);
	    if((R_xlen_t) nwrite < len) warning(_("problem with writing to connection"));
	}
	Free(buf);
    }

    if(!wasopen) {
        cntxt.end();
        con->checkClose();
    }
    if(isRaw) {
	UNPROTECT(1);
	R_Visible = true;
    } else R_Visible = false;
    return ans;
}

/* FIXME: could do any MBCS locale, but would need pushback */
static SEXP readFixedString(Rconnection con, int len, int useBytes, bool *warnOnNul)
{
    SEXP ans;
    char *buf;
    int  m;
    const void *vmax = vmaxget();

    if(utf8locale && !useBytes) {
	int i, clen;
	char *p, *q;

	p = buf = (char *) R_alloc(MB_CUR_MAX*len+1, sizeof(char));
	memset(buf, 0, MB_CUR_MAX*len+1);
	for(i = 0; i < len; i++) {
	    q = p;
	    m = (int) con->read(p, sizeof(char), 1, con);
	    if(!m) { if(i == 0) return R_NilValue; else break;}
	    clen = utf8clen(*p++);
	    if(clen > 1) {
		m = (int) con->read(p, sizeof(char), clen - 1, con);
		if(m < clen - 1) error(_("invalid UTF-8 input in 'readChar()'"));
		p += clen - 1;
		/* NB: this only checks validity of multi-byte characters */
		if((int)mbrtowc(nullptr, q, clen, nullptr) < 0)
		    error(_("invalid UTF-8 input in 'readChar()'"));
	    } else if (*q == '\0' && *warnOnNul) {
		*warnOnNul = FALSE;
		warning(_("truncating string with embedded nuls"));
	    }
	}
    } else {
	buf = (char *) R_alloc(len+1, sizeof(char));
	memset(buf, 0, len+1);
	m = (int) con->read(buf, sizeof(char), len, con);
	if(len && !m) return R_NilValue;
	if (strlen(buf) < (size_t) m && *warnOnNul) {
	    *warnOnNul = FALSE;
	    warning(_("truncating string with embedded nuls"));
	}
    }
    /* String may contain nuls which we now (R >= 2.8.0) assume to be
       padding and ignore */
    ans = mkChar(buf);
    vmaxset(vmax);
    return ans;
}

static SEXP rawFixedString(Rbyte *bytes, int len, int nbytes, int *np, int useBytes)
{
    char *buf;
    SEXP res;
    const void *vmax = vmaxget();

    if(*np + len > nbytes) {
	len = nbytes - *np;
	if (!len) return(R_NilValue);
    }

    /* Note: mkCharLenCE signals an error on embedded nuls. */
    if(utf8locale && !useBytes) {
	int i, clen, iread = *np;
	char *p;
	Rbyte *q;

	p = buf = (char *) R_alloc(MB_CUR_MAX*len+1, sizeof(char));
	for(i = 0; i < len; i++, p += clen, iread += clen) {
	    if (iread >= nbytes) break;
	    q = bytes + iread;
	    clen = utf8clen(*q);
	    if (iread + clen > nbytes)
		error(_("invalid UTF-8 input in 'readChar()'"));
	    memcpy(p, q, clen);
	}
	clen = iread - (*np);
	*np = iread;
	*p = '\0';
	res = mkCharLenCE(buf, clen, CE_NATIVE);
    } else {
	/* no terminator */
	buf = (char*) R_chk_calloc(len + 1, 1);
	memcpy(buf, bytes + (*np), len);
	*np += len;
	res = mkCharLenCE(buf, len, CE_NATIVE);
	Free(buf);
    }
    vmaxset(vmax);
    return res;
}


/* readChar(con, nchars) */
HIDDEN SEXP do_readchar(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans = R_NilValue, onechar, nchars;
    R_xlen_t i, n, m = 0;
    int nbytes = 0, np = 0, useBytes;
    bool wasopen = TRUE, isRaw = FALSE, warnOnNul = TRUE;
    Rconnection con = nullptr;
    Rbyte *bytes = nullptr;
    RCNTXT cntxt;
    checkArity(op, args);

    if(TYPEOF(CAR(args)) == RAWSXP) {
	isRaw = TRUE;
	bytes = RAW(CAR(args));
	nbytes = LENGTH(CAR(args));
    } else {
	con = getConnection(asInteger(CAR(args)));
	if(!con->canread)
	    error(_("cannot read from this connection"));
    }
    /* We did as.integer in the wrapper */
    nchars = CADR(args);
    n = XLENGTH(nchars);
    if(n == 0) return allocVector(STRSXP, 0);
    useBytes = asLogical(CADDR(args));
    if(useBytes == NA_LOGICAL)
	error(_("invalid '%s' argument"), "useBytes");

    if (!isRaw) {
	wasopen = con->isopen;
	if(!wasopen) {
	    /* Documented behaviour */
	    char mode[5];
	    strcpy(mode, con->mode);
	    strcpy(con->mode, "rb");
	    if(!con->open(con)) error(_("cannot open the connection"));
	    strcpy(con->mode, mode);
	    /* Set up a context which will close the connection on error */
	    cntxt.start(CTXT_CCODE, R_NilValue, R_BaseEnv, R_BaseEnv, R_NilValue, R_NilValue);
	    cntxt.setContextEnd(&con_cleanup, con);
	}
	if(!con->canread) error(_("cannot read from this connection"));
    }
    if (mbcslocale && !utf8locale && !useBytes)
	warning(_("can only read in bytes in a non-UTF-8 MBCS locale" ));
    PROTECT(ans = allocVector(STRSXP, n));
    if(!isRaw && con->text &&
       (con->buff || con->nPushBack >= 0 || con->inconv))

	/* could be turned into runtime error */
	warning(_("text connection used with '%s' function, results may be incorrect"),
	          "readChar()");
    for(i = 0, m = 0; i < n; i++) {
	int len = INTEGER(nchars)[i];
	if(len == NA_INTEGER || len < 0)
	    error(_("invalid '%s' argument"), "nchars");
	onechar = isRaw ? rawFixedString(bytes, len, nbytes, &np, useBytes)
	    : readFixedString(con, len, useBytes, &warnOnNul);
	if(onechar != R_NilValue) {
	    SET_STRING_ELT(ans, i, onechar);
	    m++;
	} else break;
    }

    if(m < n) {
	PROTECT(ans = xlengthgets(ans, m));
	UNPROTECT(1);
    }
    UNPROTECT(1);
    if (!wasopen)
    {
        cntxt.end();
        con->close(con);
    }

    return ans;
}

/* writeChar(object, con, nchars, sep, useBytes) */
HIDDEN SEXP do_writechar(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP object, nchars, sep, ans = R_NilValue, si;
    R_xlen_t i, n, len;
    int useBytes;
    size_t slen, tlen, lenb, lenc;
    char *buf;
    const char *s, *ssep = "";
    bool wasopen = TRUE, usesep, isRaw = FALSE;
    Rconnection con = nullptr;
    mbstate_t mb_st;
    RCNTXT cntxt;

    checkArity(op, args);
    object = CAR(args);
    if(TYPEOF(object) != STRSXP)
	error(_("invalid '%s' argument"), "object");
    if(TYPEOF(CADR(args)) == RAWSXP) {
	isRaw = TRUE;
    } else {
	con = getConnection(asInteger(CADR(args)));
	if(!con->canwrite)
	    error(_("cannot write to this connection"));
	wasopen = con->isopen;
    }

    /* We did as.integer in the wrapper */
    nchars = CADDR(args);
    sep = CADDDR(args);
    useBytes = asLogical(CAD4R(args));
    if(useBytes == NA_LOGICAL)
	error(_("invalid '%s' argument"), "useBytes");

    if(isNull(sep)) {
	usesep = FALSE;
	slen = 0;
    } else {
	usesep = TRUE;
	if (!isString(sep) || LENGTH(sep) != 1)
	    error(_("invalid '%s' argument"), "sep");
	if(useBytes)
	    ssep = CHAR(STRING_ELT(sep, 0));
	else
	    ssep = translateChar(STRING_ELT(sep, 0));
	slen = strlen(ssep) + 1;
    }
    n = XLENGTH(nchars);
    if(XLENGTH(object) < n)
	error(_("'object' argument is too short"));
    if(n == 0) {
	if(isRaw) return allocVector(RAWSXP, 0); else return R_NilValue;
    }

    len = 0;
    if (!isRaw) {
	for(i = 0; i < n; i++) {
	    /* This is not currently needed, just future-proofing in case
	       the logic gets changed */
	    if(useBytes)
		tlen = strlen(CHAR(STRING_ELT(object, i)));
	    else
		tlen = strlen(translateChar(STRING_ELT(object, i)));
	    if ((R_xlen_t) tlen > len) len = tlen;
	    int tt = INTEGER(nchars)[i];
	    if(tt == NA_INTEGER || tt < 0)
		error(_("invalid '%s' argument"), "nchars");
	    if (tt > len) len = tt;
	}
	buf = (char *) R_alloc(len + slen, sizeof(char));
    } else {
	double dlen = 0;
	for (i = 0; i < n; i++)
	    dlen += (double)(INTEGER(nchars)[i] + slen);
	if (dlen > (double) R_XLEN_T_MAX)
	    error(_("too much data for a raw vector on this platform"));
	len = (R_xlen_t) dlen;
	PROTECT(ans = allocVector(RAWSXP, len));
	buf = (char*) RAW(ans);
    }

    if(!wasopen) {
	/* Documented behaviour */
	char mode[5];
	strcpy(mode, con->mode);
	strcpy(con->mode, "wb");
	if(!con->open(con)) error(_("cannot open the connection"));
	strcpy(con->mode, mode);
	/* Set up a context which will close the connection on error */
	cntxt.start(CTXT_CCODE, R_NilValue, R_BaseEnv, R_BaseEnv, R_NilValue, R_NilValue);
	cntxt.setContextEnd(&con_cleanup, con);
	if(!con->canwrite) error(_("cannot write to this connection"));
    }

    if(!isRaw && con->text && con->outconv)
	/* could be turned into runtime error */
	warning(_("text connection used with '%s' function, results may be incorrect"),
	          "writeChar()");

    for(i = 0; i < n; i++) {
	len = INTEGER(nchars)[i];
	si = STRING_ELT(object, i);
	if(strlen(CHAR(si)) < (size_t) LENGTH(si)) {
	    if(len > LENGTH(si)) {
		warning(_("writeChar: more bytes requested than are in the string - will zero-pad"));
	    }
	    memset(buf, '\0', len + slen);
	    memcpy(buf, CHAR(si), len);
	    if (usesep) {
		strcpy(buf + len, ssep);
		len += slen;
	    }
	    if (!isRaw) {
		size_t nwrite = con->write(buf, sizeof(char), len, con);
		if(!nwrite) {
		    warning(_("problem with writing to connection"));
		    break;
		}
	    } else
		buf += len;
	} else {
	    if(useBytes)
		s = CHAR(si);
	    else
		s = translateChar(si);
	    lenb = lenc = strlen(s);
	    if(mbcslocale) lenc = mbstowcs(nullptr, s, 0);
	    /* As from 1.8.1, zero-pad if too many chars are requested. */
	    if(len > (R_xlen_t) lenc) {
		warning(_("writeChar: more characters requested than are in the string - will zero-pad"));
		lenb += (len - lenc);
	    }
	    if(len < (R_xlen_t) lenc) {
		if(mbcslocale) {
		    /* find out how many bytes we need to write */
		    size_t used;
		    const char *p = s;
		    mbs_init(&mb_st);
            lenb = 0;
		    for(R_xlen_t i = 0; i < len; i++) {
			used = Mbrtowc(nullptr, p, MB_CUR_MAX, &mb_st);
			p += used;
			lenb += used;
		    }
		} else
		    lenb = len;
	    }
	    memset(buf, '\0', lenb + slen);
	    strncpy(buf, s, lenb);
	    if (usesep) {
		strcpy(buf + lenb, ssep);
		lenb += slen;
	    }
	    if (!isRaw) {
		size_t nwrite = con->write(buf, sizeof(char), lenb, con);
		if(!nwrite) {
		    warning(_("problem with writing to connection"));
		    break;
		}
	    } else
		buf += lenb;
	}
    }
    if(!wasopen) {
        cntxt.end();
        con->checkClose();
    }
    if(isRaw) {
	UNPROTECT(1);
	R_Visible = true;
    } else {
	ans = R_NilValue;
	R_Visible = false;
    }
    return ans;
}

/* ------------------- push back text  --------------------- */


/* used in readLines and scan */
void Rf_con_pushback(Rconnection con, Rboolean newLine, char *line)
{
    int nexists = con->nPushBack;
    char **q;

    if (nexists == R_INT_MAX)
	error(_("maximum number of pushback lines exceeded"));
    if(nexists > 0) {
	q = (char **) realloc(con->PushBack, (nexists+1)*sizeof(char *));
    } else {
	q = (char **) malloc(sizeof(char *));
    }
    if(!q) error(_("could not allocate space for '%s'"), "pushback");
    else con->PushBack = q;
    q += nexists;
    *q = (char *) malloc(strlen(line) + 1 + newLine);
    if(!(*q)) error(_("could not allocate space for '%s'"), "pushback");
    strcpy(*q, line);
    if(newLine) strcat(*q, "\n");
    q++;
    con->posPushBack = 0;
    con->nPushBack++;
}


HIDDEN SEXP do_pushback(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int i, n, nexists, newLine, type;
    Rconnection con = nullptr;
    SEXP stext;
    const char *p;
    char **q;

    checkArity(op, args);

    stext = CAR(args);
    if(!isString(stext))
	error(_("invalid '%s' argument"), "data");
    con = getConnection(asInteger(CADR(args)));
    newLine = asLogical(CADDR(args));
    if(newLine == NA_LOGICAL)
	error(_("invalid '%s' argument"), "newLine");
    type = asInteger(CADDDR(args));
    if(!con->canread && !con->isopen)
	error(_("can only push back on open readable connections"));
    if(!con->text)
	error(_("can only push back on text-mode connections"));
    nexists = con->nPushBack;
    if((n = LENGTH(stext)) > 0) {
	if(nexists > 0)
	    q = (char **) realloc(con->PushBack, (n+nexists)*sizeof(char *));
	else
	    q = (char **) malloc(n*sizeof(char *));
	if(!q) error(_("could not allocate space for '%s'"), "pushback");
	con->PushBack = q;
	q += nexists;
	for(i = 0; i < n; i++) {
	    p = type == 1 ? translateChar(STRING_ELT(stext, n - i - 1))
			  : ((type == 3) ? translateCharUTF8(STRING_ELT(stext, n - i - 1))
					 : CHAR(STRING_ELT(stext, n - i - 1)));
	    *q = (char *) malloc(strlen(p) + 1 + newLine);
	    if(!(*q)) error(_("could not allocate space for '%s'"), "pushback");
	    strcpy(*q, p);
	    if(newLine) strcat(*q, "\n");
	    q++;
	}
	con->posPushBack = 0;
	con->nPushBack += n;
    }
    return R_NilValue;
}

HIDDEN SEXP do_pushbacklength(SEXP call, SEXP op, SEXP args, SEXP env)
{
    Rconnection con = nullptr;

    checkArity(op, args);
    con = getConnection(asInteger(CAR(args)));
    return ScalarInteger(con->nPushBack);
}

HIDDEN SEXP do_clearpushback(SEXP call, SEXP op, SEXP args, SEXP env)
{
    Rconnection con = nullptr;

    checkArity(op, args);
    con = getConnection(asInteger(CAR(args)));

    if(con->nPushBack > 0) { // so con_close1 has not been called
	for(size_t j = 0; j < con->nPushBack; j++) free(con->PushBack[j]);
	free(con->PushBack);
	con->nPushBack = 0;
    }
    return R_NilValue;
}

/* ------------------- sink functions  --------------------- */

/* Switch output to connection number icon, or pop stack if icon < 0
 */

static Rboolean switch_or_tee_stdout(int icon, int closeOnExit, int tee)
{
    int toclose;

    if(icon == R_OutputCon) return FALSE;

    if(icon >= 0 && R_SinkNumber >= NSINKS - 1)
	error(_("sink stack is full"));

    if(icon == 0)
	error(_("cannot switch output to 'stdin'"));
    else if(icon == 1 || icon == 2) {
	R_OutputCon = SinkCons[++R_SinkNumber] = icon;
	R_SinkSplit[R_SinkNumber] = tee;
	SinkConsClose[R_SinkNumber] = 0;
    } else if(icon >= 3) {
	Rconnection con = getConnection(icon); /* checks validity */
	toclose = 2*closeOnExit;
	if(!con->isopen) {
	    char mode[5];
	    strcpy(mode, con->mode);
	    strcpy(con->mode, "wt");
	    if(!con->open(con)) error(_("cannot open the connection"));
	    strcpy(con->mode, mode);
	    if(!con->canwrite) {
		con->close(con);
		error(_("cannot write to this connection"));
	    }
	    toclose = 1;
	} else if(!con->canwrite)
	    error(_("cannot write to this connection"));
	R_OutputCon = SinkCons[++R_SinkNumber] = icon;
	SinkConsClose[R_SinkNumber] = toclose;
	R_SinkSplit[R_SinkNumber] = tee;
	R_PreserveObject((SEXP) con->ex_ptr);
   } else { /* removing a sink */
	if (R_SinkNumber <= 0) {
	    warning(_("no sink to remove"));
	    return FALSE;
	} else {
	    R_OutputCon = SinkCons[--R_SinkNumber];
	    if((icon = SinkCons[R_SinkNumber + 1]) >= 3) {
		Rconnection con = getConnection(icon);
		R_ReleaseObject((SEXP) con->ex_ptr);
		if(SinkConsClose[R_SinkNumber + 1] == 1) { /* close it */
		    con->checkClose();
		} else if (SinkConsClose[R_SinkNumber + 1] == 2) /* destroy it */
		    con_destroy(icon);
	    }
	}
    }
    return TRUE;
}

/* This is only used by cat() */
HIDDEN Rboolean switch_stdout(int icon, int closeOnExit)
{
  return switch_or_tee_stdout(icon, closeOnExit, 0);
}

HIDDEN SEXP do_sink(SEXP call, SEXP op, SEXP args, SEXP rho)
{
  int icon, closeOnExit, errcon, tee;

    checkArity(op, args);
    icon = asInteger(CAR(args));
    closeOnExit = asLogical(CADR(args));
    if(closeOnExit == NA_LOGICAL)
	error(_("invalid '%s' argument"), "closeOnExit");
    errcon = asLogical(CADDR(args));
    if(errcon == NA_LOGICAL) error(_("invalid '%s' argument"), "type");
    tee = asLogical(CADDDR(args));
    if(tee == NA_LOGICAL) error(_("invalid '%s' argument"), "split");

    if(!errcon) {
	/* allow space for cat() to use sink() */
	if(icon >= 0 && R_SinkNumber >= NSINKS - 2)
	    error(_("sink stack is full"));
	switch_or_tee_stdout(icon, closeOnExit, tee);
    } else {
	if(icon < 0) {
	    R_ReleaseObject((SEXP) getConnection(R_ErrorCon)->ex_ptr);
	    R_ErrorCon = 2;
	} else {
	    getConnection(icon); /* check validity */
	    R_ErrorCon = icon;
	    R_PreserveObject((SEXP) getConnection(icon)->ex_ptr);
	}
    }

    return R_NilValue;
}

HIDDEN SEXP do_sinknumber(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int errcon;
    checkArity(op, args);

    errcon = asLogical(CAR(args));
    if(errcon == NA_LOGICAL)
	error(_("invalid '%s' argument"), "type");
    return ScalarInteger(errcon ? R_SinkNumber : R_ErrorCon);
}

#ifdef _WIN32
void WinCheckUTF8(void)
{
    if(EmitEmbeddedUTF8) /* RGui */
	WinUTF8out = (SinkCons[R_SinkNumber] == 1 ||
	              SinkCons[R_SinkNumber] == 2) && localeCP != 65001;
    else
	WinUTF8out = false;
}
#endif

/* ------------------- admin functions  --------------------- */

HIDDEN void R::InitConnections()
{
    int i;
    Connections[0] = newterminal("stdin", "r");
    Connections[0]->fgetc = stdin_fgetc;
    Connections[1] = newterminal("stdout", "w");
    Connections[1]->vfprintf = stdout_vfprintf;
    Connections[1]->fflush = stdout_fflush;
    Connections[2] = newterminal("stderr", "w");
    Connections[2]->vfprintf = stderr_vfprintf;
    Connections[2]->fflush = stderr_fflush;
    for(i = 3; i < NCONNECTIONS; i++) Connections[i] = nullptr;
    R_OutputCon = 1;
    R_SinkNumber = 0;
    SinkCons[0] = 1; R_ErrorCon = 2;
}

HIDDEN SEXP do_getallconnections(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int i, j=0, n=0;
    SEXP ans;
    checkArity(op, args);
    for(i = 0; i < NCONNECTIONS; i++)
	if(Connections[i]) n++;
    PROTECT(ans = allocVector(INTSXP, n));
    for(i = 0; i < NCONNECTIONS; i++)
	if(Connections[i])
	    INTEGER(ans)[j++] = i;
    UNPROTECT(1);
    return ans;
}

HIDDEN SEXP do_getconnection(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, classs;
    int what;
    Rconnection con;

    checkArity(op, args);
    what = asInteger(CAR(args));
    if (what == NA_INTEGER)
	error(_("there is no connection NA"));
    if (what < 0 || what >= NCONNECTIONS || !Connections[what])
	error(_("there is no connection %d"), what);

    con = Connections[what];
    PROTECT(ans = ScalarInteger(what));
    PROTECT(classs = allocVector(STRSXP, 2));
    SET_STRING_ELT(classs, 0, mkChar(con->connclass));
    SET_STRING_ELT(classs, 1, mkChar("connection"));
    classgets(ans, classs);
    if (what > 2)
	setAttrib(ans, R_ConnIdSymbol, (SEXP) con->ex_ptr);
    UNPROTECT(2);
    return ans;
}

HIDDEN SEXP do_sumconnection(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, names, tmp;
    Rconnection Rcon;

    checkArity(op, args);
    Rcon = getConnection(asInteger(CAR(args)));
    PROTECT(ans = allocVector(VECSXP, 7));
    PROTECT(names = allocVector(STRSXP, 7));
    SET_STRING_ELT(names, 0, mkChar("description"));
    PROTECT(tmp = allocVector(STRSXP, 1));
    if(Rcon->enc == CE_UTF8)
	SET_STRING_ELT(tmp, 0, mkCharCE(Rcon->description, CE_UTF8));
    else
	SET_STRING_ELT(tmp, 0, mkChar(Rcon->description));
    SET_VECTOR_ELT(ans, 0, tmp);
    SET_STRING_ELT(names, 1, mkChar("class"));
    SET_VECTOR_ELT(ans, 1, mkString(Rcon->connclass));
    SET_STRING_ELT(names, 2, mkChar("mode"));
    SET_VECTOR_ELT(ans, 2, mkString(Rcon->mode));
    SET_STRING_ELT(names, 3, mkChar("text"));
    SET_VECTOR_ELT(ans, 3, mkString(Rcon->text? "text":"binary"));
    SET_STRING_ELT(names, 4, mkChar("opened"));
    SET_VECTOR_ELT(ans, 4, mkString(Rcon->isopen? "opened":"closed"));
    SET_STRING_ELT(names, 5, mkChar("can read"));
    SET_VECTOR_ELT(ans, 5, mkString(Rcon->canread? "yes":"no"));
    SET_STRING_ELT(names, 6, mkChar("can write"));
    SET_VECTOR_ELT(ans, 6, mkString(Rcon->canwrite? "yes":"no"));
    setAttrib(ans, R_NamesSymbol, names);
    UNPROTECT(3);
    return ans;
}


#if defined(USE_WININET_ASYNC) && !defined(USE_WININET)
#define USE_WININET 2
#endif

// in internet module: 'type' is unused
extern Rconnection R_newCurlUrl(const char *description, const char * const mode, SEXP headers, int type);


/* op = 0: .Internal( url(description, open, blocking, encoding, method, headers))
   op = 1: .Internal(file(description, open, blocking, encoding, method, raw))
*/
HIDDEN SEXP do_url(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP scmd, sopen, ans, classs, enc, headers = R_NilValue,
	headers_flat = R_NilValue;
    const char *class2 = "url";
    const char *url, *open;
    int ncon, block, raw = 0, defmeth,
	meth = 0, // 0: "default" | "internal" | "wininet", 1: "libcurl"
	winmeth;  // 0: "internal", 1: "wininet" (Windows only)
    cetype_t ienc = CE_NATIVE;
    Rconnection con = nullptr;

    checkArity(op, args);
    // --------- description
    scmd = CAR(args);
    if(!isString(scmd) || LENGTH(scmd) != 1 ||
       STRING_ELT(scmd, 0) == NA_STRING)
	error(_("invalid '%s' argument"), "description");
    if(LENGTH(scmd) > 1)
	warning(_("only first element of '%s' argument will be used"), "description");
#ifdef _WIN32
    winmeth = 1;
    if(PRIMVAL(op) == 1 && !IS_ASCII(STRING_ELT(scmd, 0)) ) { // file(<non-ASCII>, *)
	ienc = CE_UTF8;
	url = trCharUTF8(STRING_ELT(scmd, 0));
    } else {
	ienc = getCharCE(STRING_ELT(scmd, 0));
	if(ienc == CE_UTF8)
	    url = CHAR(STRING_ELT(scmd, 0));
	else
	    url = translateCharFP(STRING_ELT(scmd, 0));
    }
#else
    winmeth = 0;
    url = translateCharFP(STRING_ELT(scmd, 0));
#endif

    UrlScheme type = HTTPsh;	/* -Wall */
    Rboolean inet = TRUE;
    if (streqln(url, "http://", 7))
	type = HTTPsh;
    else if (streqln(url, "ftp://", 6))
	type = FTPsh;
    else if (streqln(url, "https://", 8))
	type = HTTPSsh;
    // ftps:// is available via most libcurl, only
    // The internal and wininet methods will create a connection
    // but refuse to open it so as from R 3.2.0 we switch to libcurl
    else if (streqln(url, "ftps://", 7))
	type = FTPSsh;
    else
	inet = FALSE; // file:// URL or a file path

    // --------- open
    sopen = CADR(args);
    if(!isString(sopen) || LENGTH(sopen) != 1)
	error(_("invalid '%s' argument"), "open");
    open = CHAR(STRING_ELT(sopen, 0)); /* ASCII */
    // --------- blocking
    block = asLogical(CADDR(args));
    if(block == NA_LOGICAL)
	error(_("invalid '%s' argument"), "blocking");
    // --------- encoding
    enc = CADDDR(args);
    if(!isString(enc) || LENGTH(enc) != 1 ||
       strlen(CHAR(STRING_ELT(enc, 0))) > 100) /* ASCII */
	error(_("invalid '%s' argument"), "encoding");

    // --------- method
    const char *cmeth = CHAR(asChar(CAD4R(args)));
    meth = streql(cmeth, "libcurl"); // 1 if "libcurl", else 0
    defmeth = streql(cmeth, "default");
#ifndef _WIN32
    if(defmeth) meth = 1;
#endif
    if (streql(cmeth, "wininet")) {
#ifdef _WIN32
	winmeth = 1;  // it already was as this is the default
#else
	error(_("'method = \"wininet\"' is only supported on Windows"));
#endif
    }
#ifdef _WIN32
    else if (streql(cmeth, "internal")) winmeth = 0;
#endif

    // --------- raw, for file() only
    if(PRIMVAL(op) == 1) {
	raw = asLogical(CAD4R(CDR(args)));
	if(raw == NA_LOGICAL)
	    error(_("invalid '%s' argument"), "raw");
    }

    // --------- headers, for url() only
    if(PRIMVAL(op) == 0) {
	SEXP lheaders = CAD4R(CDR(args));
	if (!isNull(lheaders)) {
	    headers = VECTOR_ELT(lheaders, 0);
	    headers_flat = VECTOR_ELT(lheaders, 1);
	}
    }

    if(!meth) {
	if (streqln(url, "ftps://", 7)) {
#ifdef HAVE_LIBCURL
	    if (defmeth) meth = 1; else
#endif
		error(_("ftps:// URLs are not supported by this method"));
	}
#ifdef _WIN32
	if (!winmeth && streqln(url, "https://", 8)) {
# ifdef HAVE_LIBCURL
	    if (defmeth) meth = 1; else
# endif
		error(_("https:// URLs are not supported by this method"));
	}
#else // Unix
	if (streqln(url, "https://", 8)) {
	    // We check the libcurl build does support https as from R 3.3.0
	    if (defmeth) meth = 1; else
		error(_("https:// URLs are not supported by the \"internal\" method"));
	}
#endif
    }

    ncon = Rconn::NextConnection();
    if(streqln(url, "file://", 7)) {
	int nh = 7;
#ifdef _WIN32
	/* on Windows we have file:///d:/path/to
	   whereas on Unix it is file:///path/to */
	if (strlen(url) > 9 && url[7] == '/' && url[9] == ':') nh = 8;
#endif
	con = newfile(url + nh, ienc, strlen(open) ? open : "r", raw);
	class2 = "file";
    } else if (inet) {
	if(meth) {
# ifdef HAVE_LIBCURL
	    con = R_newCurlUrl(url, strlen(open) ? open : "r", headers, 0);
# else
	    error(_("'url(method = \"libcurl\")' is not supported on this platform"));
# endif
	} else {
	    con = R_newurl(url, strlen(open) ? open : "r", headers_flat, winmeth);
	    ((Rurlconn)con->connprivate)->type = type;
	}
    } else {
	if(PRIMVAL(op) == 1) { /* call to file() */
	    if(strlen(url) == 0) {
		if(!strlen(open)) open ="w+";
		if(strcmp(open, "w+") != 0 && strcmp(open, "w+b") != 0) {
		    open ="w+";
	            warning(_("'%s' only supports 'open = \"w+\"' and 'open = \"w+b\"': using the former"), "file(\"\")");
		}
	    }
	    if(streql(url, "clipboard") ||
#ifdef _WIN32
	       strnstreqlncmp(url, "clipboard-", 10)
#else
	       streql(url, "X11_primary")
	       || streql(url, "X11_secondary")
	       || streql(url, "X11_clipboard")
#endif
		)
		con = newclp(url, strlen(open) ? open : "r");
	    else {
		const char *efn = R_ExpandFileName(url);
#ifndef _WIN32
		if (!raw) {
		    struct stat sb;
		    int res = stat(efn, &sb);
		    if (!res && (sb.st_mode & S_IFIFO)) {
			raw = TRUE;
			warning(_("using 'raw = TRUE' because '%s' is a fifo or pipe"),
				url);
		    } else if (!res && !(sb.st_mode & S_IFREG) &&
			       strcmp(efn, "/dev/null"))
			/* not setting 'raw' to FALSE because character devices may be
			   seekable; unfortunately there is no reliable way to detect
			   that without changing the device state */
			warning(_("'raw = FALSE' but '%s' is not a regular file"),
			        url);
		}
#endif
		if (!raw &&
		    (!strlen(open) || streql(open, "r") || streql(open, "rt"))) {
		    /* check if this is a compressed file */
		    FILE *fp = fopen(efn, "rb");
		    char buf[7];
		    int ztype = -1, subtype = 0, compress = 0;
		    if (fp) {
			memset(buf, 0, 7);
			size_t res = fread(buf, 5, 1, fp);
			fclose(fp);
			if(res == 1) {
			    if(buf[0] == '\x1f' && buf[1] == '\x8b') ztype = 0;
			    if(streqln(buf, "BZh", 3)) ztype = 1;
			    if((buf[0] == '\xFD') && streqln(buf+1, "7zXZ", 4))
				ztype = 2;
			    if((buf[0] == '\xFF') && streqln(buf+1, "LZMA", 4))
			    { ztype = 2; subtype = 1;}
			    if(!memcmp(buf, "]\0\0\200\0", 5))
			    { ztype = 2; subtype = 1;}
			}
		    }
		    switch(ztype) {
		    case -1:
			con = newfile(url, ienc, strlen(open) ? open : "r", raw);
			break;
		    case 0:
			con = newgzfile(url, strlen(open) ? open : "rt", compress);
			break;
		    case 1:
			con = newbzfile(url, strlen(open) ? open : "rt", compress);
			break;
		    case 2:
			con = newxzfile(url, strlen(open) ? open : "rt", subtype, compress);
			break;
		    }
		} else
		    con = newfile(url, ienc, strlen(open) ? open : "r", raw);
	    }
	    class2 = "file";
	} else { // url()
	    error(_("URL scheme unsupported by this method"));
	}
    }

    Connections[ncon] = con;
    con->blocking = (Rboolean) block;
    strncpy(con->encname, CHAR(STRING_ELT(enc, 0)), 100); /* ASCII */
    con->encname[100 - 1] = '\0';

    /* only text-mode connections are affected, but we can't tell that
       until the connection is opened, and why set an encoding on a
       connection intended to be used in binary mode? */
    if (con->encname[0] && !streql(con->encname, "native.enc"))
	con->canseek = FALSE;
    /* This is referenced in do_getconnection, so set up before
       any warning */
    con->ex_ptr = PROTECT(R_MakeExternalPtr(con->id, install("connection"),
					    R_NilValue));

    /* open it if desired */
    if(strlen(open)) {
	Rboolean success = con->open(con);
	if(!success) {
	    con_destroy(ncon);
	    error(_("cannot open the connection"));
	}
    }

    PROTECT(ans = ScalarInteger(ncon));
    PROTECT(classs = allocVector(STRSXP, 2));
    SET_STRING_ELT(classs, 0, mkChar(class2));
    SET_STRING_ELT(classs, 1, mkChar("connection"));
    classgets(ans, classs);
    setAttrib(ans, R_ConnIdSymbol, (SEXP) con->ex_ptr);
    R_RegisterCFinalizerEx((SEXP) con->ex_ptr, conFinalizer, FALSE);
    UNPROTECT(3);

    return ans;
}

size_t R_WriteConnection(Rconnection con, const void *buf, size_t n)
{
    if(!con->isopen) error(_("connection is not open"));
    if(!con->canwrite) error(_("cannot write to this connection"));

    return con->write(buf, 1, n, con);
}

size_t R_ReadConnection(Rconnection con, void *buf, size_t n)
{
    if(!con->isopen) error(_("connection is not open"));
    if(!con->canread) error(_("cannot read from this connection"));

    return con->read(buf, 1, n, con);
}

Rconnection R_GetConnection(SEXP sConn) {
    if (!inherits(sConn, "connection")) error(_("invalid connection"));
    return getConnection(asInteger(sConn));
}

/* ------------------- (de)compression functions  --------------------- */

/* Code for gzcon connections is modelled on gzio.cpp from zlib 1.2.3 */

#define get_byte() (icon->read(&ccc, 1, 1, icon), ccc)

static Rboolean gzcon_open(Rconnection con)
{
    Rgzconn priv = (Rgzconn) con->connprivate;
    Rconnection icon = priv->con;

    if(!icon->isopen && !icon->open(icon)) return FALSE;
    con->isopen = TRUE;
    con->canwrite = icon->canwrite;
    con->canread = (Rboolean) !con->canwrite;
    con->save = -1000;

    priv->s.zalloc = (alloc_func)0;
    priv->s.zfree = (free_func)0;
    priv->s.opaque = (voidpf)0;
    priv->s.next_in = Z_NULL;
    priv->s.next_out = Z_NULL;
    priv->s.avail_in = priv->s.avail_out = 0;
    priv->z_err = Z_OK;
    priv->z_eof = 0;
    priv->crc = crc32(0L, Z_NULL, 0);

    if(con->canread) {
	/* read header */
	char c, ccc, method, flags, dummy[6];
	unsigned char head[2];
	uInt len;

	icon->read(head, 1, 2, icon);
	if(head[0] != gz_magic[0] || head[1] != gz_magic[1]) {
	    if(!priv->allow) {
		warning(_("file stream does not have gzip magic number"));
		return FALSE;
	    }
	    priv->nsaved = 2;
	    priv->saved[0] = head[0];
	    priv->saved[1] = head[1];
	    return TRUE;
	}
	icon->read(&method, 1, 1, icon);
	icon->read(&flags, 1, 1, icon);
	if (method != Z_DEFLATED || (flags & RESERVED) != 0) {
	    warning(_("file stream does not have valid gzip header"));
	    return FALSE;
	}
	icon->read(dummy, 1, 6, icon);
	if ((flags & EXTRA_FIELD) != 0) { /* skip the extra field */
	    len  =  (uInt) get_byte();
	    len += ((uInt) get_byte()) << 8;
	    /* len is garbage if EOF but the loop below will quit anyway */
	    while (len-- != 0 && get_byte() != EOF) ;
	}
	if ((flags & ORIG_NAME) != 0) { /* skip the original file name */
	    while ((c = get_byte()) != 0 && c != EOF) ;
	}
	if ((flags & COMMENT) != 0) {   /* skip the .gz file comment */
	    while ((c = get_byte()) != 0 && c != EOF) ;
	}
	if ((flags & HEAD_CRC) != 0) {  /* skip the header crc */
	    for (len = 0; len < 2; len++) (void) get_byte();
	}
	priv->s.next_in  = priv->buffer;
	inflateInit2(&(priv->s), -MAX_WBITS);
    } else {
	/* write a header */
	char head[11];
	snprintf(head, 11, "%c%c%c%c%c%c%c%c%c%c", gz_magic[0], gz_magic[1],
		Z_DEFLATED, 0 /*flags*/, 0,0,0,0 /*time*/, 0 /*xflags*/,
		OS_CODE);
	icon->write(head, 1, 10, icon);
	deflateInit2(&(priv->s), priv->cp, Z_DEFLATED, -MAX_WBITS,
		     8, Z_DEFAULT_STRATEGY);
	priv->s.next_out = priv->buffer;
	priv->s.avail_out = Z_BUFSIZE;
    }

    return TRUE;
}

static void putLong(Rconnection con, uLong x)
{
    int n;
    unsigned char buf[4];

    for (n = 0; n < 4; n++)
    {
        buf[n] = (x & 0xff);
        x >>= 8;
    }
    con->write(&buf, 4, 1, con);
}

static void gzcon_close(Rconnection con)
{
    Rgzconn priv = (Rgzconn) con->connprivate;
    Rconnection icon = priv->con;

    if(icon->canwrite) {
	uInt len;
	int done = 0;
	priv->s.avail_in = 0; /* should be zero already anyway */
	while(true) {
	    len = Z_BUFSIZE - priv->s.avail_out;

	    if (len != 0) {
		if (icon->write(priv->buffer, 1, len, icon) != len) {
		    priv->z_err = Z_ERRNO;
		    error(_("writing error while flushing gzcon connection"));
		}
		priv->s.next_out = priv->buffer;
		priv->s.avail_out = Z_BUFSIZE;
	    }
	    if (done) break;
	    priv->z_err = deflate(&(priv->s), Z_FINISH);

	    /* deflate has finished flushing only when it hasn't used up
	     * all the available space in the output buffer:
	     */
	    done = (priv->s.avail_out != 0 || priv->z_err == Z_STREAM_END);

	    if (priv->z_err != Z_OK && priv->z_err != Z_STREAM_END) break;
	}
	deflateEnd(&(priv->s));
	/* NB: these must be little-endian */
	putLong(icon, priv->crc);
	putLong(icon, (uLong) (priv->s.total_in & 0xffffffff));
    } else inflateEnd(&(priv->s));

    if(icon->isopen) icon->close(icon);
    con->isopen = FALSE;
}

static int gzcon_byte(Rgzconn priv)
{
    Rconnection icon = priv->con;

    if (priv->z_eof)
        return EOF;
    if (priv->s.avail_in == 0)
    {
        priv->s.avail_in = (uInt)icon->read(priv->buffer, 1, Z_BUFSIZE, icon);
        if (priv->s.avail_in == 0)
        {
            priv->z_eof = 1;
            return EOF;
        }
        priv->s.next_in = priv->buffer;
    }
    priv->s.avail_in--;
    return *(priv->s.next_in)++;
}

static size_t gzcon_read(void *ptr, size_t size, size_t nitems,
			 Rconnection con)
{
    Rgzconn priv = (Rgzconn) con->connprivate;
    Rconnection icon = priv->con;
    Bytef *start = (Bytef*) ptr;
    uLong crc;
    int n;

    if (priv->z_err == Z_STREAM_END) return 0;  /* EOF */

    /* wrapped connection only needs to handle R_INT_MAX */
    if ((double) size * (double) nitems > R_INT_MAX)
	error(_("specified block is too large"));
    if (priv->nsaved >= 0) { /* non-compressed mode */
	size_t len = size*nitems;
	int i, nsaved = priv->nsaved;
	if (len == 0) return 0;
	if (len >= 2) {
	    for(i = 0; i < priv->nsaved; i++)
		((char *)ptr)[i] = priv->saved[i];
	    priv->nsaved = 0;
	    return (nsaved + icon->read((char *) ptr+nsaved, 1, len - nsaved,
					icon))/size;
	}
	if (len == 1) { /* size must be one */
	    if (nsaved > 0) {
		((char *) ptr)[0] = priv->saved[0];
		priv->saved[0] = priv->saved[1];
		priv->nsaved--;
		return 1;
	    } else
		return icon->read(ptr, 1, 1, icon);
	}
    }

    priv->s.next_out = (Bytef*) ptr;
    priv->s.avail_out = (uInt)(size*nitems);

    while (priv->s.avail_out != 0) {
	if (priv->s.avail_in == 0 && !priv->z_eof) {
	    priv->s.avail_in = (uInt)icon->read(priv->buffer, 1, Z_BUFSIZE, icon);
	    if (priv->s.avail_in == 0) priv->z_eof = 1;
	    priv->s.next_in = priv->buffer;
	}
	priv->z_err = inflate(&(priv->s), Z_NO_FLUSH);

	if (priv->z_err == Z_STREAM_END) {
	    /* Check CRC */
	    priv->crc = crc32(priv->crc, start,
			      (uInt)(priv->s.next_out - start));
	    start = priv->s.next_out;
	    crc = 0;
	    for (n = 0; n < 4; n++) {
		crc >>= 8;
		crc += ((uLong) gzcon_byte(priv) << 24);
	    }
	    if (crc != priv->crc) {
		priv->z_err = Z_DATA_ERROR;
		REprintf(_("crc error %x %x\n"), crc, priv->crc);
	    }
	    /* finally, get (and ignore) length */
	    for (n = 0; n < 4; n++) gzcon_byte(priv);
	}
	if (priv->z_err != Z_OK || priv->z_eof) break;
    }
    priv->crc = crc32(priv->crc, start, (uInt)(priv->s.next_out - start));
    return (size_t)(size*nitems - priv->s.avail_out)/size;
}

static size_t gzcon_write(const void *ptr, size_t size, size_t nitems,
			  Rconnection con)
{
    Rgzconn priv = (Rgzconn) con->connprivate;
    Rconnection icon = priv->con;

    if ((double) size * (double) nitems > R_INT_MAX)
	error(_("specified block is too large"));
    priv->s.next_in = (Bytef*) ptr;
    priv->s.avail_in = (uInt)(size*nitems);

    while (priv->s.avail_in != 0) {
	if (priv->s.avail_out == 0) {
	    priv->s.next_out = priv->buffer;
	    if (icon->write(priv->buffer, 1, Z_BUFSIZE, icon) != Z_BUFSIZE) {
		priv->z_err = Z_ERRNO;
		warning(_("write error on gzcon connection"));
		break;
	    }
	    priv->s.avail_out = Z_BUFSIZE;
	}
	priv->z_err = deflate(&(priv->s), Z_NO_FLUSH);
	if (priv->z_err != Z_OK) break;
    }
    priv->crc = crc32(priv->crc, (const Bytef *) ptr, (uInt) (size*nitems));
    return (size_t) (size*nitems - priv->s.avail_in)/size;
}

static int gzcon_fgetc(Rconnection con)
{
    unsigned char c;
    size_t n = gzcon_read(&c, 1, 1, con);
    return (n == 1) ? c : R_EOF;
}

/* gzcon(con, level, allowNonCompressed) */
HIDDEN SEXP do_gzcon(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, classs;
    int icon, level, allow;
    Rconnection incon = nullptr, newconn = nullptr;
    char *m, description[1000];
    const char* mode = nullptr; /* -Wall */
    Rboolean text;

    checkArity(op, args);
    if(!inherits(CAR(args), "connection"))
	error(_("'%s' argument is not an object of class %s"), "con", "\"connection\"");
    incon = getConnection(icon = asInteger(CAR(args)));
    level = asInteger(CADR(args));
    if(level == NA_INTEGER || level < 0 || level > 9)
	error(_("'level' argument must be one of 0 ... 9"));
    allow = asLogical(CADDR(args));
    if(allow == NA_INTEGER)
	error(_("'%s' argument must be TRUE or FALSE"), "allowNonCompression");
    text = (Rboolean) asLogical(CADDDR(args));
    if(text == NA_INTEGER)
        error(_("'%s' argument must be TRUE or FALSE"), "text");

    if(incon->isGzcon) {
	warning(_("this is already a gzcon connection"));
	return CAR(args);
    }
    m = incon->mode;
    if(streql(m, "r") || streqln(m, "rb", 2)) mode = "rb";
    else if (streql(m, "w") || streqln(m, "wb", 2)) mode = "wb";
    else error(_("can only use read- or write- binary connections"));
    if(streql(incon->connclass, "file") &&
       (streql(m, "r") || streql(m, "w")))
	warning(_("using a text-mode 'file' connection may not work correctly"));

    else if(streql(incon->connclass, "textConnection") && streql(m, "w"))
	error(_("cannot create a gzcon connection from a writable textConnection; maybe use rawConnection"));

    newconn = (Rconnection) malloc(sizeof(struct Rconn));
    if(!newconn) error(_("allocation of gzcon connection failed"));
    newconn->connclass = (char *) malloc(strlen("gzcon") + 1);
    if(!newconn->connclass) {
	free(newconn);
	error(_("allocation of gzcon connection failed"));
 	/* for Solaris 12.5 */ newconn = nullptr;
   }
    strcpy(newconn->connclass, "gzcon");
    Rsnprintf_mbcs(description, 1000, "gzcon(%s)", incon->description);
    newconn->description = (char *) malloc(strlen(description) + 1);
    if(!newconn->description) {
	free(newconn->connclass); free(newconn);
	error(_("allocation of gzcon connection failed"));
	/* for Solaris 12.5 */ newconn = nullptr;
    }
    init_con(newconn, description, CE_NATIVE, mode);
    newconn->text = text;
    newconn->isGzcon = TRUE;
    newconn->open = &gzcon_open;
    newconn->close = &gzcon_close;
    newconn->vfprintf = &dummy_vfprintf;
    newconn->fgetc = &gzcon_fgetc;
    newconn->read = &gzcon_read;
    newconn->write = &gzcon_write;
    newconn->connprivate = (void *) malloc(sizeof(struct gzconn));
    if(!newconn->connprivate) {
	free(newconn->description); free(newconn->connclass); free(newconn);
	error(_("allocation of 'gzcon' connection failed"));
	/* for Solaris 12.5 */ newconn = nullptr;
    }
    ((Rgzconn) (newconn->connprivate))->con = incon;
    ((Rgzconn) (newconn->connprivate))->cp = level;
    ((Rgzconn) (newconn->connprivate))->nsaved = -1;
    ((Rgzconn) (newconn->connprivate))->allow = (Rboolean) allow;

    /* as there might not be an R-level reference to the wrapped connection */
    R_PreserveObject((SEXP) incon->ex_ptr);

    Connections[icon] = newconn;
    // gcc 8 with sanitizers selected objects to truncation here with 99 or 100.
    strncpy(newconn->encname, incon->encname, 100);
    newconn->encname[100 - 1] = '\0';
    newconn->ex_ptr = PROTECT(R_MakeExternalPtr((void *)newconn->id, install("connection"),
					    R_NilValue));
    if(incon->isopen) newconn->open(newconn);

    PROTECT(ans = ScalarInteger(icon));
    PROTECT(classs = allocVector(STRSXP, 2));
    SET_STRING_ELT(classs, 0, mkChar("gzcon"));
    SET_STRING_ELT(classs, 1, mkChar("connection"));
    classgets(ans, classs);
    setAttrib(ans, R_ConnIdSymbol, (SEXP) newconn->ex_ptr);
    /* Disable, as e.g. load() leaves no reference to the new connection */
    //R_RegisterCFinalizerEx(new->ex_ptr, conFinalizer, FALSE);
    UNPROTECT(3);

    return ans;
}


/* code for in-memory (de)compression
   of data stored in a scalar string. Uses a 4-byte header of length,
   in XDR order. */

#ifndef WORDS_BIGENDIAN
static unsigned int uiSwap(unsigned int x)
{
    return ((x << 24) | ((x & 0xff00) << 8) | ((x & 0xff0000) >> 8) | (x >> 24));
}
#else
#define uiSwap(x) (x)
#endif

/* These are all hidden and used only in serialize.cpp,
   so managing R_alloc stack is prudence. */
HIDDEN SEXP R_compress1(SEXP in)
{
    const void *vmax = vmaxget();
    unsigned int inlen;
    uLong outlen;
    int res;
    Bytef *buf;
    SEXP ans;

    if(TYPEOF(in) != RAWSXP)
	error(_("'%s' function requires a raw vector"), "R_compress1()");
    inlen = LENGTH(in);
    outlen = (uLong)(1.001*inlen + 20);
    buf = (Bytef *) R_alloc(outlen + 4, sizeof(Bytef));
    /* we want this to be system-independent */
    *((unsigned int *)buf) = (unsigned int) uiSwap(inlen);
    res = compress(buf + 4, &outlen, (Bytef *) RAW(in), inlen);
    if(res != Z_OK) error(_("internal error %d in '%s' function"), res, "R_compress1()");
    ans = allocVector(RAWSXP, outlen + 4);
    memcpy(RAW(ans), buf, outlen + 4);
    vmaxset(vmax);
    return ans;
}

HIDDEN SEXP R_decompress1(SEXP in, bool &err)
{
    const void *vmax = vmaxget();
    uLong inlen, outlen;
    int res;
    Bytef *buf;
    unsigned char *p = RAW(in);
    SEXP ans;

    if(TYPEOF(in) != RAWSXP)
	error(_("'%s' function requires a raw vector"), "R_decompress1()");
    inlen = LENGTH(in);
    outlen = (uLong) uiSwap(*((unsigned int *) p));
    buf = (Bytef *) R_alloc(outlen, sizeof(Bytef));
    res = uncompress(buf, &outlen, (Bytef *) (p + 4), inlen - 4);
    if(res != Z_OK) {
	warning(_("internal error %d in '%s' function"), res, "R_decompress1()");
	err = true;
	return R_NilValue;
    }
    ans = allocVector(RAWSXP, outlen);
    memcpy(RAW(ans), buf, outlen);
    vmaxset(vmax);
    return ans;
}

HIDDEN SEXP R_compress2(SEXP in)
{
    const void *vmax = vmaxget();
    unsigned int inlen, outlen;
    int res;
    char *buf;
    SEXP ans;

    if(TYPEOF(in) != RAWSXP)
	error(_("'%s' function requires a raw vector"), "R_compress2()");
    inlen = LENGTH(in);
    outlen = (unsigned int) (1.01*inlen + 600);
    buf = R_alloc(outlen + 5, sizeof(char));
    /* we want this to be system-independent */
    *((unsigned int *) buf) = (unsigned int) uiSwap(inlen);
    buf[4] = '2';
    res = BZ2_bzBuffToBuffCompress(buf + 5, &outlen,
				   (char *) RAW(in), inlen,
				   9, 0, 0);
    if(res != BZ_OK) error(_("internal error %d in '%s' function"), res, "R_compress2()");
    /* printf("compressed %d to %d\n", inlen, outlen); */
    if (res != BZ_OK || outlen > inlen) {
	outlen = inlen;
	buf[4] = '0';
	memcpy(buf+5, (char *)RAW(in), inlen);
    }
    ans = allocVector(RAWSXP, outlen + 5);
    memcpy(RAW(ans), buf, outlen + 5);
    vmaxset(vmax);
    return ans;
}

HIDDEN SEXP R_decompress2(SEXP in, bool &err)
{
    const void *vmax = vmaxget();
    unsigned int inlen, outlen;
    int res;
    char *buf, *p = (char *) RAW(in), type;
    SEXP ans;

    if(TYPEOF(in) != RAWSXP)
	error(_("'%s' function requires a raw vector"), "R_decompress2()");
    inlen = LENGTH(in);
    outlen = uiSwap(*((unsigned int *) p));
    buf = R_alloc(outlen, sizeof(char));
    type = p[4];
    if (type == '2') {
	res = BZ2_bzBuffToBuffDecompress(buf, &outlen, p + 5, inlen - 5, 0, 0);
	if(res != BZ_OK) {
	    warning(_("internal error %d in '%s' function"), res, "R_decompress2()");
	    err = true;
	    return R_NilValue;
	}
    } else if (type == '1') {
	uLong outl;
	res = uncompress((unsigned char *) buf, &outl,
			 (Bytef *) (p + 5), inlen - 5);
	if(res != Z_OK) {
	    warning(_("internal error %d in '%s' function"), res, "R_decompress1()");
	    err = true;
	    return R_NilValue;
	}
    } else if (type == '0') {
	buf = p + 5;
    } else {
	warning(_("unknown type in '%s' function"), "R_decompress2()");
	err = true;
	return R_NilValue;
    }
    ans = allocVector(RAWSXP, outlen);
    memcpy(RAW(ans), buf, outlen);
    vmaxset(vmax);
    return ans;
}


HIDDEN SEXP do_sockselect(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    Rboolean immediate = FALSE;
    int nsock, i;
    SEXP insock, write, val, insockfd;
    double timeout;

    checkArity(op, args);

    insock = CAR(args);
    if (TYPEOF(insock) != VECSXP || LENGTH(insock) == 0)
	error(_("'%s' argument is not a list of sockets"), "socklist");
    nsock = LENGTH(insock);

    write = CADR(args);
    if (TYPEOF(write) != LGLSXP || LENGTH(write) != nsock)
	error(_("bad write indicators"));

    timeout = asReal(CADDR(args));

    PROTECT(insockfd = allocVector(INTSXP, nsock));
    PROTECT(val = allocVector(LGLSXP, nsock));

    for (i = 0; i < nsock; i++) {
	Rconnection conn = getConnection(asInteger(VECTOR_ELT(insock, i)));
	if (streql(conn->connclass, "sockconn")) {
	    Rsockconn scp = (Rsockconn) conn->connprivate;
	    INTEGER(insockfd)[i] = scp->fd;
	    if (! LOGICAL(write)[i] && scp->pstart < scp->pend) {
		LOGICAL(val)[i] = TRUE;
		immediate = TRUE;
	    }
	    else LOGICAL(val)[i] = FALSE;
	} else if (streql(conn->connclass, "servsockconn")) {
	    Rservsockconn sop = (Rservsockconn) conn->connprivate;
	    INTEGER(insockfd)[i] = sop->fd;
	    LOGICAL(val)[i] = FALSE;
	    if (LOGICAL(write)[i])
		warning(_("a server socket connection cannot be writeable"));
	} else
	    error(_("this is not a socket connection"));
    }

    if (! immediate)
	Rsockselect(nsock, INTEGER(insockfd), LOGICAL(val), LOGICAL(write),
		    timeout);

    UNPROTECT(2);
    return val;
}

HIDDEN SEXP do_serversocket(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, connclass;
    int ncon, port;
    Rconnection con = nullptr;

    checkArity(op, args);

    port = asInteger(CAR(args));
    if(port == NA_INTEGER || port < 0)
	error(_("invalid '%s' argument"), "port");

    ncon = Rconn::NextConnection();
    con = R_newservsock(port);
    Connections[ncon] = con;
    con->ex_ptr = PROTECT(R_MakeExternalPtr(con->id, install("connection"), R_NilValue));

    PROTECT(ans = ScalarInteger(ncon));
    PROTECT(connclass = allocVector(STRSXP, 2));
    SET_STRING_ELT(connclass, 0, mkChar("servsockconn"));
    SET_STRING_ELT(connclass, 1, mkChar("connection"));
    classgets(ans, connclass);
    setAttrib(ans, R_ConnIdSymbol, (SEXP) con->ex_ptr);
    R_RegisterCFinalizerEx((SEXP) con->ex_ptr, conFinalizer, FALSE);
    UNPROTECT(3);
    return ans;
}

/* socketTimeout(socket, timeout) */
HIDDEN SEXP do_socktimeout(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int tnew, told;
    Rsockconn scon;

    checkArity(op, args);

    if (!inherits(CAR(args), "sockconn"))
        error(_("invalid '%s' argument"), "socket");

    scon = (Rsockconn)getConnection(asInteger(CAR(args)))->connprivate;
    told = scon->timeout;

    tnew = asInteger(CADR(args));
    if (tnew == NA_INTEGER)
        error(_("invalid '%s' argument"), "timeout");

    if (tnew >= 0)
        scon->timeout = tnew;

    return ScalarInteger(told);
}

static lzma_filter filters[LZMA_FILTERS_MAX + 1];

static void init_filters(void)
{
    static uint32_t preset_number = 6; /* 9 | LZMA_PRESET_EXTREME; */
    static lzma_options_lzma opt_lzma;
    static Rboolean set = FALSE;
    if(set) return;
    if(lzma_lzma_preset(&opt_lzma, preset_number))
	error(_("problem with setting presets"));
    filters[0].id = LZMA_FILTER_LZMA2;
    filters[0].options = &opt_lzma;
    filters[1].id = LZMA_VLI_UNKNOWN;
    set = TRUE;
    /*
      printf("encoding memory usage %lu\n", lzma_raw_encoder_memusage(filters));
      printf("decoding memory usage %lu\n", lzma_raw_decoder_memusage(filters));
    */
}

HIDDEN SEXP R_compress3(SEXP in)
{
    const void *vmax = vmaxget();
    unsigned int inlen, outlen;
    unsigned char *buf;
    SEXP ans;
    lzma_stream strm = LZMA_STREAM_INIT;
    lzma_ret ret;

    if(TYPEOF(in) != RAWSXP)
	error(_("'%s' function requires a raw vector"), "R_compress3()");
    inlen = LENGTH(in);
    outlen = inlen + 5;  /* don't allow it to expand */
    buf = (unsigned char *) R_alloc(outlen + 5, sizeof(unsigned char));
    /* we want this to be system-independent */
    *((unsigned int *) buf) = (unsigned int) uiSwap(inlen);
    buf[4] = 'Z';

    init_filters();
    ret = lzma_raw_encoder(&strm, filters);
    if (ret != LZMA_OK) error(_("internal error %d in '%s' function"), ret, "R_compress3()");
    strm.next_in = RAW(in);
    strm.avail_in = inlen;
    strm.next_out = buf + 5;
    strm.avail_out = outlen;
    while(!ret) ret = lzma_code(&strm, LZMA_FINISH);
    if (ret != LZMA_STREAM_END || (strm.avail_in > 0)) {
	warning(_("internal error %d in '%s' function"), ret, "R_compress3()");
	outlen = inlen;
	buf[4] = '0';
	memcpy(buf+5, (char *) RAW(in), inlen);
    } else outlen = (unsigned int) strm.total_out;
    lzma_end(&strm);

    /* printf("compressed %d to %d\n", inlen, outlen); */
    ans = allocVector(RAWSXP, outlen + 5);
    memcpy(RAW(ans), buf, outlen + 5);
    vmaxset(vmax);
    return ans;
}

HIDDEN SEXP R_decompress3(SEXP in, bool &err)
{
    const void *vmax = vmaxget();
    unsigned int inlen, outlen;
    unsigned char *buf, *p = RAW(in), type = p[4];
    SEXP ans;

    if(TYPEOF(in) != RAWSXP)
	error(_("'%s' function requires a raw vector"), "R_decompress3()");
    inlen = LENGTH(in);
    outlen = (unsigned int) uiSwap(*((unsigned int *) p));
    buf = (unsigned char *) R_alloc(outlen, sizeof(unsigned char));

    if (type == 'Z') {
	lzma_stream strm = LZMA_STREAM_INIT;
	lzma_ret ret;
	init_filters();
	ret = lzma_raw_decoder(&strm, filters);
	if (ret != LZMA_OK) {
	    warning(_("internal error %d in '%s' function"), ret, "R_decompress3()");
	    err = true;
	    return R_NilValue;
	}
	strm.next_in = p + 5;
	strm.avail_in = inlen - 5;
	strm.next_out = buf;
	strm.avail_out = outlen;
	ret = lzma_code(&strm, LZMA_RUN);
	if (ret != LZMA_OK && (strm.avail_in > 0)) {
	    warning(_("internal error %d in '%s' function (%d)"), ret, "R_decompress3()", strm.avail_in);
	    err = true;
	    return R_NilValue;
	}
	lzma_end(&strm);
    } else if (type == '2') {
	int res;
	res = BZ2_bzBuffToBuffDecompress((char *) buf, &outlen,
					 (char *) (p + 5), inlen - 5, 0, 0);
	if(res != BZ_OK) {
	    warning(_("internal error %d in '%s' function"), res, "R_decompress2()");
	    err = true;
	    return R_NilValue;
	}
    } else if (type == '1') {
	uLong outl; int res;
	res = uncompress(buf, &outl, (Bytef *) (p + 5), inlen - 5);
	if(res != Z_OK) {
	    warning(_("internal error %d in '%s' function"), res, "R_decompress1()");
	    err = true;
	    return R_NilValue;
	}
    } else if (type == '0') {
	buf = p + 5;
    } else {
	warning(_("unknown type in '%s' function"), "R_decompress3()");
	err = true;
	return R_NilValue;
    }
    ans = allocVector(RAWSXP, outlen);
    memcpy(RAW(ans), buf, outlen);
    vmaxset(vmax);
    return ans;
}

HIDDEN SEXP do_memCompress(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, from;
    int type, res;

    checkArity(op, args);
    ans = from = CAR(args);
    if(TYPEOF(from) != RAWSXP) error(_("'from' argument must be raw or character"));
    type = asInteger(CADR(args));
    switch(type) {
    case 1: break; /* none */
    case 2: /*gzip */
    {
	Bytef *buf;
	/* could use outlen = compressBound(inlen) */
	uLong inlen = XLENGTH(from),
	    outlen = (uLong)(1.001*(double)inlen + 20);
	buf = (Bytef *) R_alloc(outlen, sizeof(Bytef));
	res = compress(buf, &outlen, (Bytef *) RAW(from), inlen);
	if(res != Z_OK) error(_("internal error %d in '%s' function"), res, "memCompress()");
	ans = allocVector(RAWSXP, outlen);
	memcpy(RAW(ans), buf, outlen);
	break;
    }
    case 3: /* bzip */
    {
	// bzlib does not support long inputs
	char *buf;
	unsigned int inlen = LENGTH(from),
	    outlen = (unsigned int) (1.01*inlen + 600);
	buf = R_alloc(outlen, sizeof(char));
	res = BZ2_bzBuffToBuffCompress(buf, &outlen, (char *) RAW(from),
				       inlen, 9, 0, 0);
	if(res != BZ_OK) error(_("internal error %d in '%s' function"), res, "memCompress()");
	ans = allocVector(RAWSXP, outlen);
	memcpy(RAW(ans), buf, outlen);
	break;
    }
    case 4: /* xz */
    {
	unsigned char *buf;
	size_t inlen = XLENGTH(from), outlen;
	lzma_stream strm = LZMA_STREAM_INIT;
	lzma_ret ret;
	lzma_filter filters[LZMA_FILTERS_MAX + 1];
	uint32_t preset_number = 9 | LZMA_PRESET_EXTREME;
	lzma_options_lzma opt_lzma;

	if(lzma_lzma_preset(&opt_lzma, preset_number))
	    error(_("problem with setting presets"));
	filters[0].id = LZMA_FILTER_LZMA2;
	filters[0].options = &opt_lzma;
	filters[1].id = LZMA_VLI_UNKNOWN;

	ret = lzma_stream_encoder(&strm, filters, LZMA_CHECK_CRC32);
	if (ret != LZMA_OK) error(_("internal error %d in '%s' function"), ret, "memCompress()");

	outlen = (unsigned int) (1.01 * inlen + 600); /* FIXME, copied
						        from bzip2 case */
	buf = (unsigned char *) R_alloc(outlen, sizeof(unsigned char));
	strm.next_in = RAW(from);
	strm.avail_in = inlen;
	strm.next_out = buf;
	strm.avail_out = outlen;
	while(!ret) ret = lzma_code(&strm, LZMA_FINISH);
	if (ret != LZMA_STREAM_END || (strm.avail_in > 0))
	    error(_("internal error %d in '%s' function"), ret, "memCompress()");
	/* If LZMZ_BUF_ERROR, could realloc and continue */
	outlen = (unsigned int) strm.total_out;
	lzma_end(&strm);
	ans = allocVector(RAWSXP, outlen);
	memcpy(RAW(ans), buf, outlen);
	break;
    }
    default:
	break;
    }

    return ans;
}

HIDDEN SEXP do_memDecompress(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, from;
    int type, subtype = 0;

    checkArity(op, args);
    ans = from = CAR(args);
    if(TYPEOF(from) != RAWSXP) error(_("'from' argument must be raw or character"));
    type = asInteger(CADR(args));
    if (type == 5) {/* type = 5 is "unknown" */
	char *p = (char *) RAW(from);
	if (streqln(p, "BZh", 3)) type = 3; /* bzip2 always uses a header */
	else if(p[0] == '\x1f' && p[1] == '\x8b') type = 2; /* gzip files */
	else if((p[0] == '\xFD') && streqln(p+1, "7zXZ", 4)) type = 4;
	else if((p[0] == '\xFF') && streqln(p+1, "LZMA", 4)) {
	    type = 4; subtype = 1;
	} else if(!memcmp(p, "]\0\0\200\0", 5)) {
	    type = 4; subtype = 1;
	} else {
	    warning(_("unknown compression, assuming none"));
	    type = 1;
	}
    }

    switch(type) {
    case 1: break; /* none */
    case 2: /* gzip */
    {
	uLong inlen = XLENGTH(from), outlen = 3*inlen;
	Bytef *buf, *p = (Bytef *) RAW(from);
	int opt = 0;

	if (p[0] == 0x1f && p[1] == 0x8b) { // in-memory gzip file
	    // in this case we could read outlen from the trailer
	    opt = 16;  // force gzip format
	}
	while(true) {
	    buf = (Bytef *) R_alloc(outlen, sizeof(Bytef));
	    int res = R_uncompress(buf, outlen, p, inlen, opt);
	    if(res == Z_BUF_ERROR) {
		if(outlen < ULONG_MAX/2) {
		    outlen *= 2; continue;
		} else break;
	    }
	    if(res >= 0) break;
	    error(_("internal error %d in '%s' function"), res,
		  "memDecompress(type = \"gzip\")");
	}

	/*
	if (p[0] == 0x1f && p[1] == 0x8b) { // in-memory gzip file
	    // in this case we could read outlen from the trailer
	    while(true) {
		buf = (Bytef *) R_alloc(outlen, sizeof(Bytef));
		int res = R_gzuncompress(buf, &outlen, p, inlen, 16); // force gzip format
		if(res == Z_BUF_ERROR) {
		    if(outlen < ULONG_MAX/2) {
			outlen *= 2; continue;
		    } else break;
		}
		if(res >= 0) break;
		error("internal error %d in memDecompress(%s)", res,
		      "type = \"gzip\"");
	    }
	} else {
	    while(true) {
		buf = (Bytef *) R_alloc(outlen, sizeof(Bytef));
		int res = uncompress(buf, &outlen, p, inlen);
		if(res == Z_BUF_ERROR) {
		    if(outlen < ULONG_MAX/2) {
			outlen *= 2; continue;
		    } else break;
		}
		if(res == Z_OK) break;
		error("internal error %d in memDecompress(%s)", res,
		      "type = \"gzip\"");
	    }
	}
	*/
	ans = allocVector(RAWSXP, outlen);
	memcpy(RAW(ans), buf, outlen);
	break;
    }
    case 3: /* bzip2 */
    {
	// bzlib does not support long inputs
	unsigned int inlen = LENGTH(from), outlen;
	double o0 = 3.0*inlen;
	outlen = (o0 > UINT_MAX)? UINT_MAX: (unsigned int)o0;
	int res;
	char *buf, *p = (char *) RAW(from);
	while(true) {
	    buf = R_alloc(outlen, sizeof(char));
	    res = BZ2_bzBuffToBuffDecompress(buf, &outlen, p, inlen, 0, 0);
	    if(res == BZ_OUTBUFF_FULL) {
		if(outlen < UINT_MAX/2) {
		    outlen *= 2; continue;
		} else break;
	    }
	    if(res == BZ_OK) break;
	    error(_("internal error %d in '%s' function"), res,
		  "memDecompress(type = \"bzip2\")");
	}
	ans = allocVector(RAWSXP, outlen);
	memcpy(RAW(ans), buf, outlen);
	break;
    }
    case 4: /* xz */
    {
	unsigned char *buf;
	size_t inlen = XLENGTH(from);
	size_t outlen = 3*inlen;
	lzma_stream strm = LZMA_STREAM_INIT;
	lzma_ret ret;
	while(true) {
	    /* Initialize lzma_stream in each iteration. */
	    /* probably at most 80Mb is required, but 512Mb seems OK as a limit */
	    if (subtype == 1)
		ret = lzma_alone_decoder(&strm, 536870912);
	    else
		ret = lzma_stream_decoder(&strm, 536870912, LZMA_CONCATENATED);
	    if (ret != LZMA_OK)
		error(_("cannot initialize lzma decoder, error %d"), ret);

	    buf = (unsigned char *) R_alloc(outlen, sizeof(unsigned char));
	    strm.avail_in = inlen;
	    strm.avail_out = outlen;
	    strm.next_in = (unsigned char *) RAW(from);
	    strm.next_out = buf;

	    ret = lzma_code(&strm, LZMA_FINISH);
	    /* Did lzma_code() leave some input? */
	    if (strm.avail_in > 0) {
		/* Decompression failed, free lzma_stream. */
		lzma_end(&strm);
		/* Because it ran out of output buffer?
		 *
		 * This used to only check if LZMA_BUF_ERROR was
		 * returned, but apparently XZ will also signal an out
		 * of buffer condition by returning LZMA_OK and
		 * leaving avail_in > 0 (i.e. not all input was
		 * consumed).
		 */
		if (ret == LZMA_BUF_ERROR || ret == LZMA_OK) {
		    outlen *= 2;
		    continue;
		} else {
		    error(_("internal error %d in '%s' function at %d"),
			  ret, "memDecompress(type = \"xz\")", strm.avail_in);
		}
	    } else {
		break;
	    }
	}
	outlen = strm.total_out;
	lzma_end(&strm);
	ans = allocVector(RAWSXP, outlen);
	memcpy(RAW(ans), buf, outlen);
	break;
    }
    default:
	break;
    }
    return ans;
}

/* --- C-level entry to create a custom connection object -- */
/* The returned value is the R-side instance. To avoid additional call to getConnection()
   the internal Rconnection pointer will be placed in ptr[0] if ptr is not NULL.
   It is the responsibility of the caller to customize callbacks in the structure,
   they are initialized to dummy_ (where available) and null_ (all others) callbacks.
   Also note that the resulting object has a finalizer, so any clean up (including after
   errors) is done by garbage collection - the caller may not free anything in the
   structure explicitly (that includes the con->connprivate pointer!).
 */
SEXP R_new_custom_connection(const char *description, const char *mode, const char *class_name, Rconnection *ptr)
{
    Rconnection newconn;
    SEXP ans, classs;

    int ncon = Rconn::NextConnection();

    /* built-in connections do this in a separate new<class>() function */
    newconn = (Rconnection) malloc(sizeof(struct Rconn));
    if(!newconn) error(_("allocation of %s connection failed"), class_name);
    newconn->connclass = (char *) malloc(strlen(class_name) + 1);
    if(!newconn->connclass) {
	free(newconn);
	error(_("allocation of %s connection failed"), class_name);
	/* for Solaris 12.5 */ newconn = nullptr;
    }
    strcpy(newconn->connclass, class_name);
    newconn->description = (char *) malloc(strlen(description) + 1);
    if(!newconn->description) {
	free(newconn->connclass); free(newconn);
	error(_("allocation of %s connection failed"), class_name);
	/* for Solaris 12.5 */ newconn = nullptr;
    }
    init_con(newconn, description, CE_NATIVE, mode);
    /* all ptrs are init'ed to null_* so no need to repeat that,
       but the following two are useful tools which could not be accessed otherwise */
    newconn->vfprintf = &dummy_vfprintf;
    newconn->fgetc = &dummy_fgetc;

    /* here we use the new connection to create a SEXP */
    Connections[ncon] = newconn;
    /* new->blocking = block; */
    newconn->encname[0] = 0; /* "" (should have the same effect as "native.enc") */
    newconn->ex_ptr = PROTECT(R_MakeExternalPtr(newconn->id, install("connection"), R_NilValue));

    PROTECT(ans = ScalarInteger(ncon));
    PROTECT(classs = allocVector(STRSXP, 2));
    SET_STRING_ELT(classs, 0, mkChar(class_name));
    SET_STRING_ELT(classs, 1, mkChar("connection"));
    classgets(ans, classs);
    setAttrib(ans, R_ConnIdSymbol, (SEXP) newconn->ex_ptr);
    R_RegisterCFinalizerEx((SEXP) newconn->ex_ptr, conFinalizer, FALSE);
    UNPROTECT(3);

    if (ptr) ptr[0] = newconn;

    return ans;
}
