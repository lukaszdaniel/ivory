/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998-2014    The R Core Team
 *
 *  This header file is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This file is part of R. R is distributed under the terms of the
 *  GNU General Public License, either Version 2, June 1991 or Version 3,
 *  June 2007. See doc/COPYRIGHTS for details of the copyright status of R.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

/*
 *  These functions are not part of the API.
 */
#ifndef PRTUTIL_H_
#define PRTUTIL_H_

#include <Rinternals.h> // for R_xlen_t
#include <R_ext/Complex.h>

// for backcompatibility but not to stay (MM) :
#define formatComplex_USING_signif

#ifdef  __cplusplus
extern "C" {
#endif

/* Computation of printing formats */
void Rf_formatLogical(const int *, R_xlen_t, int *);
void Rf_formatInteger(const int *, R_xlen_t, int *);
void Rf_formatReal(const double *, R_xlen_t, int *, int *, int *, int);
void Rf_formatComplex(const Rcomplex *, R_xlen_t, int *, int *, int *, int *, int *, int *, int);
void formatLogicalS(SEXP, R_xlen_t, int *);
void formatIntegerS(SEXP, R_xlen_t, int *);
void formatRealS(SEXP, R_xlen_t, int *, int *, int *, int);
void formatComplexS(SEXP, R_xlen_t, int *, int *, int *, int *, int *, int *, int);

/* Formating of values */
const char *Rf_EncodeLogical(int, int);
const char *Rf_EncodeInteger(int, int);
const char *Rf_EncodeReal0(double, int, int, int, const char *);
const char *Rf_EncodeComplex(Rcomplex, int, int, int, int, int, int, const char *);

/* Legacy, misused by packages RGtk2 and qtbase */
const char *Rf_EncodeReal(double, int, int, int, char);


/* Printing */
int Rf_IndexWidth(R_xlen_t);
void Rf_VectorIndex(R_xlen_t, int);

//void printLogicalVector(int *, R_xlen_t, int);
void Rf_printIntegerVector(const int *, R_xlen_t, int);
void Rf_printRealVector(const double *, R_xlen_t, int);
void Rf_printComplexVector(const Rcomplex *, R_xlen_t, int);
void printIntegerVectorS(SEXP, R_xlen_t, int);
void printRealVectorS(SEXP, R_xlen_t, int);
void printComplexVectorS(SEXP, R_xlen_t, int);

#ifdef  __cplusplus
}
#endif

#ifdef __cplusplus
const auto formatLogical = Rf_formatLogical;
const auto formatInteger = Rf_formatInteger;
const auto formatReal = Rf_formatReal;
const auto formatComplex = Rf_formatComplex;
const auto EncodeLogical = Rf_EncodeLogical;
const auto EncodeInteger = Rf_EncodeInteger;
const auto EncodeReal = Rf_EncodeReal;
const auto EncodeReal0 = Rf_EncodeReal0;
const auto EncodeComplex = Rf_EncodeComplex;
const auto VectorIndex = Rf_VectorIndex;
const auto printIntegerVector = Rf_printIntegerVector;
const auto printRealVector = Rf_printRealVector;
const auto printComplexVector = Rf_printComplexVector;
#else
#define formatLogical      Rf_formatLogical
#define formatInteger      Rf_formatInteger
#define formatReal         Rf_formatReal
#define formatComplex      Rf_formatComplex
#define EncodeLogical      Rf_EncodeLogical
#define EncodeInteger      Rf_EncodeInteger
#define EncodeReal         Rf_EncodeReal
#define EncodeReal0        Rf_EncodeReal0
#define EncodeComplex      Rf_EncodeComplex
#define VectorIndex        Rf_VectorIndex
#define printIntegerVector Rf_printIntegerVector
#define printRealVector    Rf_printRealVector
#define printComplexVector Rf_printComplexVector
#endif

#endif /* PRTUTIL_H_ */
