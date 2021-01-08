#include "dppMatrix.h"
#include "localization.h"

SEXP dppMatrix_validate(SEXP obj)
{
/*     int i, n = INTEGER(R_do_slot(obj, Matrix_DimSym))[0]; */
/*     double *x = REAL(R_do_slot(obj, Matrix_xSym)); */

    /* quick but nondefinitive check on positive definiteness */
/*     for (i = 0; i < n; i++) */
/* 	if (x[i * np1] < 0) */
/* 	    return mkString(_("dppMatrix is not positive definite")); */
    return dspMatrix_validate(obj);
}

SEXP dppMatrix_chol(SEXP x)
{
    SEXP val = get_factors(x, "pCholesky"),
	dimP = R_do_slot(x, Matrix_DimSym),
	uploP = R_do_slot(x, Matrix_uploSym);
    const char *uplo = CHAR(STRING_ELT(uploP, 0));
    int *dims = INTEGER(dimP), info;

    if (val != R_NilValue) return val;
    dims = INTEGER(dimP);
    val = PROTECT(NEW_OBJECT_OF_CLASS("pCholesky"));
    R_do_slot_assign(val, Matrix_uploSym, duplicate(uploP));
    R_do_slot_assign(val, Matrix_diagSym, mkString("N"));
    R_do_slot_assign(val, Matrix_DimSym, duplicate(dimP));
    slot_dup(val, x, Matrix_xSym);
    F77_CALL(dpptrf)(uplo, dims, REAL(R_do_slot(val, Matrix_xSym)), &info FCONE);
    if (info) {
	if(info > 0) /* e.g. x singular */
	    error(_("the leading minor of order %d is not positive definite"),
		    info);
	else /* should never happen! */
	    error(_("Lapack routine '%s' returned error code %d"), "dpptrf", info);
    }
    UNPROTECT(1);
    return set_factors(x, val, "pCholesky");
}

SEXP dppMatrix_rcond(SEXP obj, SEXP type)
{
    SEXP Chol = dppMatrix_chol(obj);
    char typnm[] = {'O', '\0'};	/* always use the one norm */
    int *dims = INTEGER(R_do_slot(Chol, Matrix_DimSym)), info;
    double anorm = get_norm_sp(obj, typnm), rcond;

    F77_CALL(dppcon)(uplo_P(Chol), dims,
		     REAL(R_do_slot(Chol, Matrix_xSym)), &anorm, &rcond,
		     (double *) R_alloc(3*dims[0], sizeof(double)),
		     (int *) R_alloc(dims[0], sizeof(int)), &info FCONE);
    return ScalarReal(rcond);
}

SEXP dppMatrix_solve(SEXP x)
{
    SEXP Chol = dppMatrix_chol(x);
    SEXP val = PROTECT(NEW_OBJECT_OF_CLASS("dppMatrix"));
    int *dims = INTEGER(R_do_slot(x, Matrix_DimSym)), info;

    slot_dup(val, Chol, Matrix_uploSym);
    slot_dup(val, Chol, Matrix_xSym);
    slot_dup(val, Chol, Matrix_DimSym);
    F77_CALL(dpptri)(uplo_P(val), dims,
		     REAL(R_do_slot(val, Matrix_xSym)), &info FCONE);
    UNPROTECT(1);
    return val;
}

SEXP dppMatrix_matrix_solve(SEXP a, SEXP b)
{
    SEXP val = PROTECT(dup_mMatrix_as_dgeMatrix(b));
    SEXP Chol = dppMatrix_chol(a);
    int *adims = INTEGER(R_do_slot(a, Matrix_DimSym)),
	*bdims = INTEGER(R_do_slot(val, Matrix_DimSym));
    int n = bdims[0], nrhs = bdims[1], info;

    if (*adims != *bdims || bdims[1] < 1 || *adims < 1)
	error(_("Dimensions of system to be solved are inconsistent"));
    F77_CALL(dpptrs)(uplo_P(Chol), &n, &nrhs,
		     REAL(R_do_slot(Chol, Matrix_xSym)),
		     REAL(R_do_slot(val, Matrix_xSym)), &n, &info FCONE);
    UNPROTECT(1);
    return val;
}
