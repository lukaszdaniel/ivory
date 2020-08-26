/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996, 1997,  Robert Gentleman and Ross Ihaka
 *                2007-2020 The R Core Team
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
 *  I/O Support Code
 *
 *  This is a general IO support package to provide R with a uniform
 *  interface to reading data from the console, files and internal
 *  text strings.
 *
 *  This is probably overkill, but it works much better than the
 *  previous anarchy.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "IOStuff.h"

using namespace R;

/* Move the iob->write_buf pointer to the next */
/* BufferListItem in the chain. If there no next */
/* buffer item, then one is added. */

static bool NextWriteBufferListItem(IoBuffer *iob)
{
    if (iob->write_buf->next)
    {
        iob->write_buf = iob->write_buf->next;
    }
    else
    {
        BufferListItem *_new;
        if (!(_new = (BufferListItem *)malloc(sizeof(BufferListItem))))
            return false;
        _new->next = nullptr;
        iob->write_buf->next = _new;
        iob->write_buf = iob->write_buf->next;
    }
    iob->write_ptr = iob->write_buf->buf;
    iob->write_offset = 0;
    return true;
}

/* Move the iob->read_buf pointer to the next */
/* BufferListItem in the chain. */

static bool NextReadBufferListItem(IoBuffer *iob)
{
    iob->read_buf = iob->read_buf->next;
    iob->read_ptr = iob->read_buf->buf;
    iob->read_offset = 0;
    return true;
}

/* Reset the read/write pointers of an IoBuffer */

HIDDEN bool R::R_IoBufferWriteReset(IoBuffer *iob)
{
    if (iob == nullptr || iob->start_buf == nullptr)
        return false;
    iob->write_buf = iob->start_buf;
    iob->write_ptr = iob->write_buf->buf;
    iob->write_offset = 0;
    iob->read_buf = iob->start_buf;
    iob->read_ptr = iob->read_buf->buf;
    iob->read_offset = 0;
    return true;
}

/* Reset the read pointer of an IoBuffer */

HIDDEN bool R::R_IoBufferReadReset(IoBuffer *iob)
{
    if (iob == nullptr || iob->start_buf == nullptr)
        return false;
    iob->read_buf = iob->start_buf;
    iob->read_ptr = iob->read_buf->buf;
    iob->read_offset = 0;
    return true;
}

/* Allocate an initial BufferListItem for IoBuffer */
/* Initialize the counts and pointers. */

HIDDEN bool R::R_IoBufferInit(IoBuffer *iob)
{
    if (iob == nullptr)
        return false;
    iob->start_buf = (BufferListItem *)malloc(sizeof(BufferListItem));
    if (iob->start_buf == nullptr)
        return false;
    iob->start_buf->next = nullptr;
    return R_IoBufferWriteReset(iob);
}

/* Free any BufferListItem's associated with an IoBuffer */
/* This resets pointers to nullptr, which could be detected */
/* in other calls. */

HIDDEN bool R::R_IoBufferFree(IoBuffer *iob)
{
    BufferListItem *thisItem, *nextItem;
    if (iob == nullptr || iob->start_buf == nullptr)
        return false;
    thisItem = iob->start_buf;
    while (thisItem)
    {
        nextItem = thisItem->next;
        free(thisItem);
        thisItem = nextItem;
    }
    return true;
}

/* Add a character to an IoBuffer */

HIDDEN bool R::R_IoBufferPutc(int c, IoBuffer *iob)
{
    if (iob->write_offset == IOBSIZE)
        NextWriteBufferListItem(iob);
    *(iob->write_ptr)++ = (char)c;
    iob->write_offset++;
    return false; /*not used*/
}

/* Add a (null terminated) string to an IoBuffer */

HIDDEN int R::R_IoBufferPuts(char *s, IoBuffer *iob)
{
    char *p;
    int n = 0;
    for (p = s; *p; p++)
    {
        R_IoBufferPutc(*p, iob);
        n++;
    }
    return n;
}

/* Read a character from an IoBuffer */

HIDDEN int R::R_IoBufferGetc(IoBuffer *iob)
{
    if (iob->read_buf == iob->write_buf &&
        iob->read_offset >= iob->write_offset)
        return EOF;
    if (iob->read_offset == IOBSIZE)
        NextReadBufferListItem(iob);
    iob->read_offset++;
    return *(iob->read_ptr)++;
}

/* What is our current offset, taking all blocks into account? */

HIDDEN int R::R_IoBufferReadOffset(IoBuffer *iob)
{
    int result = iob->read_offset;
    BufferListItem *buf = iob->start_buf;
    while (buf && buf != iob->read_buf)
    {
        result += IOBSIZE;
        buf = buf->next;
    }
    return result;
}

/* Initialization code for text buffers */

static void transferChars(unsigned char *p, const char *q)
{
    while (*q)
        *p++ = *q++;
    *p++ = '\n';
    *p++ = '\0';
}

/* respect encoding override from parser invocation - do_parse */
static const char *translateCharWithOverride(SEXP x)
{
    if (!IS_LATIN1(x) && !mbcslocale && known_to_be_utf8)
        /* A hack to allow UTF-8 string literals, comments when
	   parsing on Windows. Note that the parser cannot handle
	   invalid characters when running in UTF-8 locale. */
        return CHAR(x);

    return translateChar(x);
}

HIDDEN bool R::R_TextBufferInit(TextBuffer *txtb, SEXP text)
{
    int i, k, l, n;
    if (isString(text)) {
	// translateChar might allocate
	void *vmax = vmaxget();
	n = length(text);
	l = 0;
	for (i = 0; i < n; i++) {
	    if (STRING_ELT(text, i) != R_NilValue) {
		k = (int) strlen(translateCharWithOverride(STRING_ELT(text, i)));
		if (k > l)
		    l = k;
	    }
	}
	vmaxset(vmax);
	txtb->vmax = vmax;
	txtb->buf = (unsigned char *)R_alloc(l+2, sizeof(char)); /* '\n' and '\0' */
	txtb->bufp = txtb->buf;
	txtb->text = text;
	txtb->ntext = n;
	txtb->offset = 0;
	transferChars(txtb->buf,
		      translateCharWithOverride(STRING_ELT(txtb->text, txtb->offset)));
	txtb->offset++;
	return true;
    }
    else {
	txtb->vmax = vmaxget();
	txtb->buf = nullptr;
	txtb->bufp = nullptr;
	txtb->text = R_NilValue;
	txtb->ntext = 0;
	txtb->offset = 1;
	return false;
    }
}

/* Finalization code for text buffers */

HIDDEN bool R::R_TextBufferFree(TextBuffer *txtb)
{
    vmaxset(txtb->vmax);
    return false; /* not used */
}

/* Getc for text buffers */

HIDDEN int R::R_TextBufferGetc(TextBuffer *txtb)
{
    if (txtb->buf == nullptr)
        return EOF;
    if (*(txtb->bufp) == '\0')
    {
        if (txtb->offset == txtb->ntext)
        {
            txtb->buf = nullptr;
            return EOF;
        }
        else
        {
            const void *vmax = vmaxget();
            transferChars(txtb->buf,
                          translateCharWithOverride(STRING_ELT(txtb->text,
                                                               txtb->offset)));
            txtb->bufp = txtb->buf;
            txtb->offset++;
            vmaxset(vmax);
        }
    }
    return *txtb->bufp++;
}
