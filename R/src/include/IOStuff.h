/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1997  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 2005  R Core Team
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

/* Internal header, not installed */

#ifndef R_IOSTUFF_H
#define R_IOSTUFF_H

#ifndef __cplusplus
#error IOStuff.h can only be included in C++ files
#endif

/*
 *  I/O Support for Consoles and Character Vectors
 *
 *  This code provides analogues for the stdio routines "fgetc" and
 *  (formerly) "ungetc" for "consoles" and character vectors.  These routines
 *  are used for parsing input from the console window and character
 *  vectors.
 */

#include <Defn.h>
#include <cstdio>

#define IOBSIZE 4096

struct BufferListItem
{
	unsigned char buf[IOBSIZE];
	struct BufferListItem *next;
};

struct IoBuffer
{
	BufferListItem *start_buf; /* First buffer item */
	BufferListItem *write_buf; /* Write pointer location */
	unsigned char *write_ptr;  /* Write pointer location */
	int write_offset;		   /* Write pointer location */
	BufferListItem *read_buf;  /* Read pointer location */
	unsigned char *read_ptr;   /* Read pointer location */
	int read_offset;		   /* Read pointer location */
};

struct TextBuffer
{
	void *vmax;			 /* Memory stack top */
	unsigned char *buf;	 /* Line buffer */
	unsigned char *bufp; /* Line buffer location */
	SEXP text;			 /* String Vector */
	int ntext;			 /* Vector length */
	int offset;			 /* Offset within vector */
};

#ifndef __MAIN__
extern
#else
HIDDEN
#endif
	IoBuffer R_ConsoleIob; /* Console IO Buffer */

/*- some of these really could be void */
bool R_IoBufferInit(IoBuffer *);
bool R_IoBufferFree(IoBuffer *);
bool R_IoBufferReadReset(IoBuffer *);
bool R_IoBufferWriteReset(IoBuffer *);
int R_IoBufferGetc(IoBuffer *);
bool R_IoBufferPutc(int, IoBuffer *);
int R_IoBufferPuts(char *, IoBuffer *);
int R_IoBufferReadOffset(IoBuffer *);

bool R_TextBufferInit(TextBuffer *, SEXP);
bool R_TextBufferFree(TextBuffer *);
int R_TextBufferGetc(TextBuffer *);

#endif /* R_IOSTUFF_H */
