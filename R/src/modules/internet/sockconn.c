/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C)  2001-12   The R Core Team.
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
 *  http://www.r-project.org/Licenses/
 */

/* <UTF8> chars are only handled as a whole */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef HAVE_SOCKETS


/* ------------------- socket connections  --------------------- */

#define R_USE_SIGNALS 1
#include <Localization.h>

#include <Defn.h>
#include <Rconnections.h>
#include <R_ext/R-ftp-http.h>
#include "sock.h"
#include <errno.h>

static void listencleanup(void *data)
{
    int *psock = data;
    R_SockClose(*psock);
}

static Rboolean sock_open(Rconnection con)
{
    Rsockconn thiscon = (Rsockconn)con->conprivate;
    int sock, sock1, mlen;
    int timeout = thiscon->timeout;
    char buf[256];

    if(timeout == NA_INTEGER || timeout <= 0) timeout = 60;
    thiscon->pend = thiscon->pstart = thiscon->inbuf;

    if(thiscon->server) {
	sock1 = R_SockOpen(thiscon->port);
	if(sock1 < 0) {
	    warning(_("port %d cannot be opened"), thiscon->port);
	    return FALSE;
	}
	{
	    RCNTXT cntxt;

	    /* set up a context which will close socket on jump. */
	    begincontext(&cntxt, CTXT_CCODE, R_NilValue, R_BaseEnv,
			 R_BaseEnv, R_NilValue, R_NilValue);
	    cntxt.cend = &listencleanup;
	    cntxt.cenddata = &sock1;
	    sock = R_SockListen(sock1, buf, 256, timeout);
	    endcontext(&cntxt);
	}
	if(sock < 0) {
	    warning(_("problem in listening on this socket"));
	    R_SockClose(sock1);
	    return FALSE;
	}
	free(con->description);
	con->description = (char *) malloc(strlen(buf) + 10);
	sprintf(con->description, "<-%s:%d", buf, thiscon->port);
	R_SockClose(sock1);
    } else {
	sock = R_SockConnect(thiscon->port, con->description, timeout);
	if(sock < 0) {
	    warning(_("%s:%d cannot be opened"), con->description, thiscon->port);
	    return FALSE;
	}
	sprintf(buf, "->%s:%d", con->description, thiscon->port);
	strcpy(con->description, buf);
    }
    thiscon->fd = sock;

    mlen = (int) strlen(con->mode);
    con->isopen = TRUE;
    if(mlen >= 2 && con->mode[mlen - 1] == 'b') con->text = FALSE;
    else con->text = TRUE;
    set_iconv(con); /* OK for output, at least */
    con->save = -1000;
    return TRUE;
}

static void sock_close(Rconnection con)
{
    Rsockconn thiscon = (Rsockconn)con->conprivate;
    R_SockClose(thiscon->fd);
    con->isopen = FALSE;
}

static size_t sock_read_helper(Rconnection con, void *ptr, size_t size)
{
    Rsockconn thiscon = (Rsockconn)con->conprivate;
    ssize_t res;
    size_t nread = 0, n;

    con->incomplete = FALSE;
    do {
	/* read data into the buffer if it's empty and size > 0 */
	if (size > 0 && thiscon->pstart == thiscon->pend) {
	    thiscon->pstart = thiscon->pend = thiscon->inbuf;
	    do
		res = R_SockRead(thiscon->fd, thiscon->inbuf, 4096,
				 con->blocking, thiscon->timeout);
	    while (-res == EINTR);
	    if (! con->blocking && -res == EAGAIN) {
		con->incomplete = TRUE;
		return nread;
	    }
	    else if (res == 0) /* should mean EOF */
		return nread;
	    else if (res < 0) return res;
	    else thiscon->pend = thiscon->inbuf + res;
	}

	/* copy data from buffer to ptr */
	if (thiscon->pstart + size <= thiscon->pend)
	    n = size;
	else
	    n = thiscon->pend - thiscon->pstart;
	memcpy(ptr, thiscon->pstart, n);
	ptr = ((char *) ptr) + n;
	thiscon->pstart += n;
	size -= n;
	nread += n;
    } while (size > 0);

    return nread;
}


static int sock_fgetc_internal(Rconnection con)
{
    unsigned char c;
    size_t n;

    n = sock_read_helper(con, (char *)&c, 1);
    return (n == 1) ? c : R_EOF;
}

static size_t sock_read(void *ptr, size_t size, size_t nitems,
			Rconnection con)
{
    return sock_read_helper(con, ptr, size * nitems)/size;
}

static size_t sock_write(const void *ptr, size_t size, size_t nitems,
			 Rconnection con)
{
    Rsockconn thiscon = (Rsockconn)con->conprivate;

    return R_SockWrite(thiscon->fd, ptr, (int)(size * nitems), thiscon->timeout)/size;
}

Rconnection in_R_newsock(const char *host, int port, int server,
			 const char * const mode, int timeout)
{
    Rconnection newcon;

    newcon = (Rconnection) malloc(sizeof(struct Rconn));
    if(!newcon) error(_("allocation of socket connection failed"));
    newcon->conclass = (char *) malloc(strlen("sockconn") + 1);
    if(!newcon->conclass) {
	free(newcon);
	error(_("allocation of socket connection failed"));
    }
    strcpy(newcon->conclass, "sockconn");
    newcon->description = (char *) malloc(strlen(host) + 10);
    if(!newcon->description) {
	free(newcon->conclass); free(newcon);
	error(_("allocation of socket connection failed"));
    }
    init_con(newcon, host, CE_NATIVE, mode);
    newcon->open = &sock_open;
    newcon->close = &sock_close;
    newcon->vfprintf = &dummy_vfprintf;
    newcon->fgetc_internal = &sock_fgetc_internal;
    newcon->fgetc = &dummy_fgetc;
    newcon->read = &sock_read;
    newcon->write = &sock_write;
    newcon->conprivate = (void *) malloc(sizeof(struct sockconn));
    if(!newcon->conprivate) {
	free(newcon->description); free(newcon->conclass); free(newcon);
	error(_("allocation of socket connection failed"));
    }
    ((Rsockconn)newcon->conprivate)-> port = port;
    ((Rsockconn)newcon->conprivate)-> server = server;
    ((Rsockconn)newcon->conprivate)-> timeout = timeout;
    return newcon;
}

#endif /* HAVE_SOCKETS */
