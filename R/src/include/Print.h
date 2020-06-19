/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997-2019   The R Core Team.
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

#ifndef PRINT_H_
#define PRINT_H_

#include <Defn.h>
#include <R_ext/PrtUtil.h>
#include <R_ext/Print.h>

#define formatRaw           Rf_formatRaw
#define formatString        Rf_formatString
#define formatRawS          Rf_formatRawS
#define formatStringS       Rf_formatStringS
#define EncodeElement       Rf_EncodeElement
#define EncodeElement0      Rf_EncodeElement0
#define EncodeEnvironment   Rf_EncodeEnvironment
#define printArray          Rf_printArray
#define printMatrix         Rf_printMatrix
#define printNamedVector    Rf_printNamedVector
#define printVector         Rf_printVector

/* For backward compatibility */
#define R_print_par_t R_PrintData

extern R_print_par_t R_print;

#ifdef __cplusplus
extern "C" {
#endif

/* Computation of printing formats */
void Rf_formatRaw(const Rbyte *, R_xlen_t, int *);
void Rf_formatString(const SEXP *, R_xlen_t, int *, int);
void Rf_formatRawS(SEXP, R_xlen_t, int *);
void Rf_formatStringS(SEXP, R_xlen_t, int*, int);

/* Formating of values */
const char *Rf_EncodeElement0(SEXP, R_xlen_t, int, const char *);
const char *Rf_EncodeEnvironment(SEXP);
/* Legacy, for R.app */
const char *Rf_EncodeElement(SEXP, int, int, char);

/* In Rinternals.h (and MUST be there):
   CustomPrintValue,  PrintValue, PrintValueRec */
void Rf_printArray(SEXP, SEXP, int, int, SEXP);
void Rf_printMatrix(SEXP, int, SEXP, int, int, SEXP, SEXP,
		 const char*, const char*);
void Rf_printNamedVector(SEXP, SEXP, int, const char*);
void Rf_printVector(SEXP, int, int);
// void PrintClosure(SEXP, Rboolean);
// void PrintLanguage(SEXP, Rboolean);

/* Utilities for S compatibility and debuggging */
void R_PV(SEXP s);
#ifdef __cplusplus
} //extern "C"
#endif

/* Offset for rowlabels if there are named dimnames */
#define R_MIN_LBLOFF 2

#define R_MIN_WIDTH_OPT		10
#define R_MAX_WIDTH_OPT		10000
#define R_MIN_DIGITS_OPT	0
#define R_MAX_DIGITS_OPT	22

#endif /* PRINT_H_ */
