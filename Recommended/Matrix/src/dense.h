#ifndef MATRIX_DENSE_H
#define MATRIX_DENSE_H

#include <Rinternals.h>
#include "Lapack-etc.h"

SEXP lsq_dense_Chol(SEXP X, SEXP y);
SEXP lsq_dense_QR(SEXP X, SEXP y);
SEXP lapack_qr(SEXP Xin, SEXP tl);
SEXP dense_to_Csparse(SEXP x);
SEXP dense_band(SEXP x, SEXP k1, SEXP k2);
SEXP dense_to_symmetric(SEXP x, SEXP uplo, SEXP symm_test);
SEXP ddense_symmpart(SEXP x);
SEXP ddense_skewpart(SEXP x);

#endif
