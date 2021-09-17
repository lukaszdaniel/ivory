/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998--2014	    The R Core Team.
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

#ifndef R_DUPLICATE_H
#define R_DUPLICATE_H

/*    
FILL_MATRIX_ITERATE
Iterator macro to fill a matrix from a vector with re-use of vector

    for(R_xlen_t i = 0; i < srows; i++) {
        R_xlen_t sidx = i;
        for(R_xlen_t j = 0; j < cols; j++, sidx += srows) {
            if (sidx >= nsrc) sidx -= nsrc;
            didx = dstart + i + (j * drows);
            ... "dst[didx] = src[sidx]"
        }
    }
*/

#define FILL_MATRIX_ITERATE(dstart, drows, srows, cols, nsrc) \
    for (R_xlen_t i = 0, sidx = 0; i < srows; i++, sidx = i)  \
        for (R_xlen_t j = 0, didx = dstart + i; j < cols;     \
             j++,                                             \
                      sidx += srows,                          \
                      (sidx >= nsrc) ? sidx -= nsrc : 0,      \
                      didx += drows)

void xcopyStringWithRecycle(SEXP dst, SEXP src, R_xlen_t dstart, R_xlen_t n, R_xlen_t nsrc);
void xcopyVectorWithRecycle(SEXP dst, SEXP src, R_xlen_t dstart, R_xlen_t n, R_xlen_t nsrc);
template <typename VALTYPE>
RHIDDEN void xcopyWithRecycle(VALTYPE *dst, VALTYPE *src, R_xlen_t dstart, R_xlen_t n, R_xlen_t nsrc)
{

	if (nsrc >= n)
	{ /* no recycle needed */
		for (R_xlen_t i = 0; i < n; i++)
			dst[dstart + i] = src[i];
		return;
	}
	if (nsrc == 1)
	{
		VALTYPE val = src[0];
		for (R_xlen_t i = 0; i < n; i++)
			dst[dstart + i] = val;
		return;
	}

	/* recycle needed */
	R_xlen_t sidx = 0;
	for (R_xlen_t i = 0; i < n; i++, sidx++)
	{
		if (sidx == nsrc)
			sidx = 0;
		dst[dstart + i] = src[sidx];
	}
}

void xfillStringMatrixWithRecycle(SEXP dst, SEXP src, R_xlen_t dstart, R_xlen_t drows, R_xlen_t srows, R_xlen_t cols, R_xlen_t nsrc);
void xfillVectorMatrixWithRecycle(SEXP dst, SEXP src, R_xlen_t dstart, R_xlen_t drows, R_xlen_t srows, R_xlen_t cols, R_xlen_t nsrc);
template <typename VALTYPE>
RHIDDEN void xfillMatrixWithRecycle(VALTYPE *dst, VALTYPE *src,
                                   R_xlen_t dstart, R_xlen_t drows, R_xlen_t srows,
                                   R_xlen_t cols, R_xlen_t nsrc)
{

    FILL_MATRIX_ITERATE(dstart, drows, srows, cols, nsrc)
    dst[didx] = src[sidx];
}

#define FILL_MATRIX_BYROW_ITERATE(dstart, drows, dcols, nsrc) \
    for (R_xlen_t i = 0, sidx = 0; i < drows; i++)            \
        for (R_xlen_t j = 0, didx = dstart + i; j < dcols;    \
             j++,                                             \
                      sidx++,                                 \
                      (sidx >= nsrc) ? sidx -= nsrc : 0,      \
                      didx += drows)

#endif /* R_DUPLICATE_H */
