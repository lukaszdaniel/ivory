#include "ldense.h"

/* dense logical Matrices "ldenseMatrix" classes --- almost identical to
 * dense nonzero-pattern: "ndenseMatrix" ones
 */

/* this is very close to dspMatrix_as_dsy* () in ./dspMatrix.c : */
SEXP lspMatrix_as_lsyMatrix(SEXP from, SEXP kind)
{
    SEXP val = PROTECT(NEW_OBJECT_OF_CLASS(
			   (asInteger(kind) == 1) ? "nsyMatrix" : "lsyMatrix")),
	uplo = R_do_slot(from, Matrix_uploSym),
	dimP = R_do_slot(from, Matrix_DimSym),
	dmnP = R_do_slot(from, Matrix_DimNamesSym);
    int n = *INTEGER(dimP);

    R_do_slot_assign(val, Matrix_DimSym, duplicate(dimP));
    R_do_slot_assign(val, Matrix_DimNamesSym, duplicate(dmnP));
    R_do_slot_assign(val, Matrix_uploSym, duplicate(uplo));
    packed_to_full_int(LOGICAL(ALLOC_SLOT(val, Matrix_xSym, LGLSXP, n*n)),
		       LOGICAL( R_do_slot(from, Matrix_xSym)), n,
		       *CHAR(STRING_ELT(uplo, 0)) == 'U' ? UPP : LOW);
    UNPROTECT(1);
    return val;
}

// this is very close to dsyMatrix_as_lsp*() in ./dsyMatrix.c  -- keep synced !
SEXP lsyMatrix_as_lspMatrix(SEXP from, SEXP kind)
{
    SEXP val = PROTECT(NEW_OBJECT_OF_CLASS(
			   (asInteger(kind) == 1) ? "nspMatrix" : "lspMatrix")),
	uplo = R_do_slot(from, Matrix_uploSym),
	dimP = R_do_slot(from, Matrix_DimSym);
    int n = *INTEGER(dimP);

    R_do_slot_assign(val, Matrix_DimSym, duplicate(dimP));
    R_do_slot_assign(val, Matrix_uploSym, duplicate(uplo));
    full_to_packed_int(
	LOGICAL(ALLOC_SLOT(val, Matrix_xSym, LGLSXP, (n*(n+1))/2)),
	LOGICAL( R_do_slot(from, Matrix_xSym)), n,
	*CHAR(STRING_ELT(uplo, 0)) == 'U' ? UPP : LOW, NUN);
    R_do_slot_assign(val, Matrix_DimNamesSym,
	     duplicate(R_do_slot(from, Matrix_DimNamesSym)));
    R_do_slot_assign(val, Matrix_factorSym,
	     duplicate(R_do_slot(from, Matrix_factorSym)));
    UNPROTECT(1);
    return val;
}

// this is very close to dtpMatrix_as_dtr*() in ./dtpMatrix.c -- keep synced!
SEXP ltpMatrix_as_ltrMatrix(SEXP from, SEXP kind)
{
    SEXP val = PROTECT(NEW_OBJECT_OF_CLASS(
			   (asInteger(kind) == 1) ? "ntrMatrix" : "ltrMatrix")),
	uplo = R_do_slot(from, Matrix_uploSym),
	diag = R_do_slot(from, Matrix_diagSym),
	dimP = R_do_slot(from, Matrix_DimSym),
	dmnP = R_do_slot(from, Matrix_DimNamesSym);
    int n = *INTEGER(dimP);

    R_do_slot_assign(val, Matrix_DimSym, duplicate(dimP));
    R_do_slot_assign(val, Matrix_DimNamesSym, duplicate(dmnP));
    R_do_slot_assign(val, Matrix_diagSym, duplicate(diag));
    R_do_slot_assign(val, Matrix_uploSym, duplicate(uplo));
    packed_to_full_int(LOGICAL(ALLOC_SLOT(val, Matrix_xSym, LGLSXP, n*n)),
		       LOGICAL(R_do_slot(from, Matrix_xSym)), n,
		       *CHAR(STRING_ELT(uplo, 0)) == 'U' ? UPP : LOW);
    R_do_slot_assign(val, Matrix_DimNamesSym,
	     duplicate(R_do_slot(from, Matrix_DimNamesSym)));
    UNPROTECT(1);
    return val;
}

/* this is very close to dtrMatrix_as_dtp* () in ./dtrMatrix.c : */
SEXP ltrMatrix_as_ltpMatrix(SEXP from, SEXP kind)
{
    SEXP val = PROTECT(NEW_OBJECT_OF_CLASS(
			   (asInteger(kind) == 1) ? "ntpMatrix" : "ltpMatrix")),
	uplo = R_do_slot(from, Matrix_uploSym),
	diag = R_do_slot(from, Matrix_diagSym),
	dimP = R_do_slot(from, Matrix_DimSym);
    int n = *INTEGER(dimP);

    R_do_slot_assign(val, Matrix_DimSym, duplicate(dimP));
    R_do_slot_assign(val, Matrix_diagSym, duplicate(diag));
    R_do_slot_assign(val, Matrix_uploSym, duplicate(uplo));
    full_to_packed_int(
	LOGICAL(ALLOC_SLOT(val, Matrix_xSym, LGLSXP, (n*(n+1))/2)),
	LOGICAL(R_do_slot(from, Matrix_xSym)), n,
	*CHAR(STRING_ELT(uplo, 0)) == 'U' ? UPP : LOW,
	*CHAR(STRING_ELT(diag, 0)) == 'U' ? UNT : NUN);
    R_do_slot_assign(val, Matrix_DimNamesSym,
	     duplicate(R_do_slot(from, Matrix_DimNamesSym)));
    UNPROTECT(1);
    return val;
}

/* this is very close to dtrMatrix_as_dge*() :*/
SEXP ltrMatrix_as_lgeMatrix(SEXP from, SEXP kind)
{
    SEXP val = PROTECT(NEW_OBJECT_OF_CLASS(
			   (asInteger(kind) == 1) ? "ngeMatrix" : "lgeMatrix"));
    slot_dup(val, from, Matrix_xSym);
    slot_dup(val, from, Matrix_DimSym);
    slot_dup(val, from, Matrix_DimNamesSym);
    R_do_slot_assign(val, Matrix_factorSym, allocVector(VECSXP, 0));

    make_i_matrix_triangular(LOGICAL(R_do_slot(val, Matrix_xSym)), from);
    UNPROTECT(1);
    return val;
}

/* this is very close to dsyMatrix_as_dge*() :*/
SEXP lsyMatrix_as_lgeMatrix(SEXP from, SEXP kind)
{
    SEXP val = PROTECT(NEW_OBJECT_OF_CLASS(
			   (asInteger(kind) == 1) ? "ngeMatrix" : "lgeMatrix"));
    slot_dup(val, from, Matrix_xSym);
    slot_dup(val, from, Matrix_DimSym);
    slot_dup(val, from, Matrix_DimNamesSym);
    R_do_slot_assign(val, Matrix_factorSym, allocVector(VECSXP, 0));

    make_i_matrix_symmetric(LOGICAL(R_do_slot(val, Matrix_xSym)), from);
    UNPROTECT(1);
    return val;
}

