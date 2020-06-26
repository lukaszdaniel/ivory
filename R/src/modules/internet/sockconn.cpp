/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C)  2001-2020   The R Core Team.
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

/* <UTF8> chars are only handled as a whole */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif


/* ------------------- socket connections  --------------------- */

#define R_USE_SIGNALS 1
#include <Localization.h>

#include <Defn.h>
#include <Rconnections.h>
#include <R_ext/R-ftp-http.h>
#include "sock.h"
#include <cerrno>

#ifdef _WIN32
# ifndef EINTR
#  define EINTR                   WSAEINTR
# endif
# ifndef EWOULDBLOCK
#  define EWOULDBLOCK             WSAEWOULDBLOCK
# endif
#endif

static void listencleanup(void *data)
{
    int *psock = (int *) data;
    R_SockClose(*psock);
}

static Rboolean sock_open(Rconnection con)
{
    Rsockconn thisconn = (Rsockconn)con->connprivate;
    int sock, sock1, mlen;
    int timeout = thisconn->timeout;
    char buf[256];

    if(timeout == NA_INTEGER || timeout <= 0) timeout = 60;
    thisconn->pend = thisconn->pstart = thisconn->inbuf;

    if(thisconn->server) {
	if (thisconn->serverfd == -1) {
	    sock1 = R_SockOpen(thisconn->port); /* socket(), bind(), listen() */
	    if(sock1 < 0) {
		warning(_("port %d cannot be opened"), thisconn->port);
		return FALSE;
	    }
	    {
		RCNTXT cntxt;

		/* set up a context which will close socket on jump. */
		begincontext(&cntxt, CTXT_CCODE, R_NilValue, R_BaseEnv,
			     R_BaseEnv, R_NilValue, R_NilValue);
		cntxt.cend = &listencleanup;
		cntxt.cenddata = &sock1;
		sock = R_SockListen(sock1, buf, 256, timeout); /* accept() */
		endcontext(&cntxt);
	    }
	    R_SockClose(sock1);
	    if(sock < 0) {
		/* NOTE: potentially confusing as the error was in accept() */
		warning(_("problem in listening on this socket"));
		return FALSE;
	    }
	} else {
	    /* accept() */
	    sock = R_SockListen(thisconn->serverfd, buf, 256, timeout);
	    if(sock < 0) {
		/* "accepting" as this is used with socketAccept() */
		warning(_("problem in accepting connections on this socket"));
		return FALSE;
	    }
	}
	free(con->description);
	con->description = (char *) malloc(strlen(buf) + 10);
	sprintf(con->description, "<-%s:%d", buf, thisconn->port);
    } else {
	sock = R_SockConnect(thisconn->port, con->description, timeout);
	if(sock < 0) {
	    warning(_("%s:%d cannot be opened"), con->description, thisconn->port);
	    return FALSE;
	}
	sprintf(buf, "->%s:%d", con->description, thisconn->port);
	strcpy(con->description, buf);
    }
    thisconn->fd = sock;

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
    Rsockconn thisconn = (Rsockconn)con->connprivate;
    R_SockClose(thisconn->fd);
    con->isopen = FALSE;
}

static void servsock_close(Rconnection con)
{
    Rservsockconn this_ = (Rservsockconn)con->connprivate;
    R_SockClose(this_->fd);
    con->isopen = FALSE;
}

static ssize_t sock_read_helper(Rconnection con, void *ptr, size_t size)
{
    Rsockconn thisconn = (Rsockconn)con->connprivate;
    ssize_t res;
    size_t nread = 0, n;

    con->incomplete = FALSE;
    do {
	/* read data into the buffer if it's empty and size > 0 */
	if (size > 0 && thisconn->pstart == thisconn->pend) {
	    thisconn->pstart = thisconn->pend = thisconn->inbuf;
	    do
		res = R_SockRead(thisconn->fd, thisconn->inbuf, 4096,
				 con->blocking, thisconn->timeout);
	    while (-res == EINTR);
#ifdef _WIN32
	    if (! con->blocking && -res == EAGAIN) {
#else
	    if (! con->blocking && (-res == EAGAIN || -res == EWOULDBLOCK)) {
#endif
		con->incomplete = TRUE;
		return nread;
	    }
	    else if (res == 0) /* should mean EOF */
		return nread;
	    else if (res < 0) return res;
	    else thisconn->pend = thisconn->inbuf + res;
	}

	/* copy data from buffer to ptr */
	if (thisconn->pstart + size <= thisconn->pend)
	    n = size;
	else
	    n = thisconn->pend - thisconn->pstart;
	memcpy(ptr, thisconn->pstart, n);
	ptr = ((char *) ptr) + n;
	thisconn->pstart += n;
	size -= n;
	nread += n;
    } while (size > 0);

    return nread;
}


static int sock_fgetc_internal(Rconnection con)
{
    unsigned char c;
    ssize_t n;

    n = sock_read_helper(con, (char *)&c, 1);
    return (n == 1) ? c : R_EOF;
}

static size_t sock_read(void *ptr, size_t size, size_t nitems,
			Rconnection con)
{
    ssize_t n = sock_read_helper(con, ptr, size * nitems)/size;
    return n > 0 ? n : 0;
}

static size_t sock_write(const void *ptr, size_t size, size_t nitems,
			 Rconnection con)
{
    Rsockconn thisconn = (Rsockconn)con->connprivate;
    ssize_t n = R_SockWrite(thisconn->fd, ptr, (int)(size * nitems),
			    thisconn->timeout)/size;
    return n > 0 ? n : 0;
}

Rconnection in_R_newsock(const char *host, int port, int server, int serverfd,
			 const char * const mode, int timeout)
{
    Rconnection newconn;

    newconn = (Rconnection) malloc(sizeof(struct Rconn));
    if(!newconn) error(_("allocation of socket connection failed"));
    newconn->connclass = (char *) malloc(strlen("sockconn") + 1);
    if(!newconn->connclass) {
	free(newconn);
	error(_("allocation of socket connection failed"));
        /* for Solaris 12.5 */ newconn = NULL;
    }
    strcpy(newconn->connclass, "sockconn");
    newconn->description = (char *) malloc(strlen(host) + 10);
    if(!newconn->description) {
	free(newconn->connclass); free(newconn);
	error(_("allocation of socket connection failed"));
        /* for Solaris 12.5 */ newconn = NULL;
    }
    init_con(newconn, host, CE_NATIVE, mode);
    newconn->open = &sock_open;
    newconn->close = &sock_close;
    newconn->vfprintf = &dummy_vfprintf;
    newconn->fgetc_internal = &sock_fgetc_internal;
    newconn->fgetc = &dummy_fgetc;
    newconn->read = &sock_read;
    newconn->write = &sock_write;
    newconn->connprivate = (void *) malloc(sizeof(struct sockconn));
    if(!newconn->connprivate) {
	free(newconn->description); free(newconn->connclass); free(newconn);
	error(_("allocation of socket connection failed"));
	/* for Solaris 12.5 */ newconn = NULL;
    }
    ((Rsockconn)newconn->connprivate)-> port = port;
    ((Rsockconn)newconn->connprivate)-> server = server;
    ((Rsockconn)newconn->connprivate)-> timeout = timeout;
    ((Rsockconn)newconn->connprivate)-> serverfd = serverfd;
    return newconn;
}

Rconnection in_R_newservsock(int port)
{
    Rconnection newconn;

    newconn = (Rconnection) malloc(sizeof(struct Rconn));
    if(!newconn) error(_("allocation of server socket connection failed"));
    newconn->connclass = (char *) malloc(strlen("servsockconn") + 1);
    if(!newconn->connclass) {
	free(newconn);
	error(_("allocation of server socket connection failed"));
        /* for Solaris 12.5 */ newconn = NULL;
    }
    strcpy(newconn->connclass, "servsockconn");
    newconn->description = (char *) malloc(strlen("localhost") + 10);
    if(!newconn->description) {
	free(newconn->connclass); free(newconn);
	error(_("allocation of server socket connection failed"));
        /* for Solaris 12.5 */ newconn = NULL;
    }
    init_con(newconn, "localhost", CE_NATIVE, "a+");
    newconn->close = &servsock_close;
    newconn->connprivate = (void *) malloc(sizeof(struct servsockconn));
    if(!newconn->connprivate) {
	free(newconn->description); free(newconn->connclass); free(newconn);
	error(_("allocation of server socket connection failed"));
	/* for Solaris 12.5 */ newconn = NULL;
    }
    ((Rservsockconn)newconn->connprivate)-> port = port;

    /* socket(), bind(), listen() */
    int sock = R_SockOpen(port); 
    if(sock < 0) {
	free(newconn->connprivate); free(newconn->description); free(newconn->connclass); free(newconn);
	error(_("creation of server socket failed: port %d cannot be opened"),
	      port);
	/* for Solaris 12.5 */ newconn = NULL;
    }
    ((Rservsockconn)newconn->connprivate)-> fd = sock;
    newconn->isopen = TRUE;

    return newconn;
}

