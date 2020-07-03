#include "factorizations.h"

SEXP MatrixFactorization_validate(SEXP obj)
{
    SEXP val;
    if (isString(val = dim_validate(R_do_slot(obj, Matrix_DimSym),
				    "MatrixFactorization")))
	return(val);
    return ScalarLogical(1);
}

SEXP LU_validate(SEXP obj)
{
    SEXP x = R_do_slot(obj, Matrix_xSym),
	Dim = R_do_slot(obj, Matrix_DimSym);
    int m = INTEGER(Dim)[0], n = INTEGER(Dim)[1]; // checked already in MatrixF.._validate()
    if(TYPEOF(x) != REALSXP)
	return mkString(_("'x' slot is not \"double\""));
    if(XLENGTH(x) != ((double) m) * n)
	return mkString(_("'x' slot is not of correct length"));
    return dimNames_validate(obj);
}

SEXP BunchKaufman_validate(SEXP obj)
{
    // TODO
    return ScalarLogical(1);
}

SEXP pBunchKaufman_validate(SEXP obj)
{
    // TODO
    return ScalarLogical(1);
}

SEXP Cholesky_validate(SEXP obj)
{
    // TODO
    return ScalarLogical(1);
}

SEXP pCholesky_validate(SEXP obj)
{
    // TODO
    return ScalarLogical(1);
}

#ifdef _Matrix_has_SVD_
SEXP SVD_validate(SEXP obj)
{
    return ScalarLogical(1);
}
#endif

SEXP LU_expand(SEXP x)
{
    const char *nms[] = {"L", "U", "P", ""};
    // x[,] is  m x n    (using LAPACK dgetrf notation)
    SEXP L, U, P, val = PROTECT(Rf_mkNamed(VECSXP, nms)),
	lux = R_do_slot(x, Matrix_xSym),
	dd = R_do_slot(x, Matrix_DimSym);
    int *iperm, *perm, *pivot = INTEGER(R_do_slot(x, Matrix_permSym)),
	*dim = INTEGER(dd), m = dim[0], n = dim[1], nn = m, i;
    size_t m_ = (size_t) m; // to prevent integer (multiplication) overflow
    Rboolean is_sq = (n == m), L_is_tri = TRUE, U_is_tri = TRUE;

    // nn :=  min(n,m) ==  length(pivot[])
    if(!is_sq) {
	if(n < m) { // "long"
	    nn = n;
	    L_is_tri = FALSE;
	} else { // m < n : "wide"
	    U_is_tri = FALSE;
	}
    }

    SET_VECTOR_ELT(val, 0, NEW_OBJECT_OF_CLASS(L_is_tri ? "dtrMatrix":"dgeMatrix"));
    SET_VECTOR_ELT(val, 1, NEW_OBJECT_OF_CLASS(U_is_tri ? "dtrMatrix":"dgeMatrix"));
    SET_VECTOR_ELT(val, 2, NEW_OBJECT_OF_CLASS("pMatrix"));
    L = VECTOR_ELT(val, 0);
    U = VECTOR_ELT(val, 1);
    P = VECTOR_ELT(val, 2);
    if(is_sq || !L_is_tri) {
	R_do_slot_assign(L, Matrix_xSym, duplicate(lux));
	R_do_slot_assign(L, Matrix_DimSym, duplicate(dd));
    } else { // !is_sq && L_is_tri -- m < n -- "wide" -- L is  m x m
	size_t m2 = m_ * m;
	double *Lx = REAL(ALLOC_SLOT(L, Matrix_xSym, REALSXP, m2));
	int *dL = INTEGER(ALLOC_SLOT(L, Matrix_DimSym, INTSXP, 2));
	dL[0] = dL[1] = m;
	// fill lower-diagonal (non-{0,1}) part -- remainder by make_d_matrix*() below:
	Memcpy(Lx, REAL(lux), m2);
    }
    if(is_sq || !U_is_tri) {
	R_do_slot_assign(U, Matrix_xSym, duplicate(lux));
	R_do_slot_assign(U, Matrix_DimSym, duplicate(dd));
    } else { // !is_sq && U_is_tri -- m > n -- "long" -- U is  n x n
	double *Ux = REAL(ALLOC_SLOT(U, Matrix_xSym, REALSXP, ((size_t) n) * n)),
	       *xx = REAL(lux);
	int *dU = INTEGER(ALLOC_SLOT(U, Matrix_DimSym, INTSXP, 2));
	dU[0] = dU[1] = n;
	/* fill upper-diagonal (non-0) part -- remainder by make_d_matrix*() below:
	 * this is more complicated than in the L case, as the x / lux part we need
	 * is  *not*  continguous:  Memcpy(Ux, REAL(lux), n * n); -- is  WRONG */
	for (size_t j = 0; j < n; j++) {
	    Memcpy(Ux+j*n, xx+j*m, j+1);
	    // for (i = 0; i <= j; i++)
	    //   Ux[i + j*n] = xx[i + j*m];
	}
    }
    if(L_is_tri) {
	R_do_slot_assign(L, Matrix_uploSym, mkString("L"));
	R_do_slot_assign(L, Matrix_diagSym, mkString("U"));
	make_d_matrix_triangular(REAL(R_do_slot(L, Matrix_xSym)), L);
    } else { // L is "unit-diagonal" trapezoidal -- m > n -- "long"
	// fill the upper right part with 0  *and* the diagonal with 1
	double *Lx = REAL(R_do_slot(L, Matrix_xSym));
	size_t ii;
	for (i = 0, ii = 0; i < n; i++, ii+=(m+1)) { // ii = i*(m+1)
	    Lx[ii] = 1.;
	    for (size_t j = i*m_; j < ii; j++)
		Lx[j] = 0.;
	}
    }

    if(U_is_tri) {
	R_do_slot_assign(U, Matrix_uploSym, mkString("U"));
	R_do_slot_assign(U, Matrix_diagSym, mkString("N"));
	make_d_matrix_triangular(REAL(R_do_slot(U, Matrix_xSym)), U);
    } else { // U is trapezoidal -- m < n
	// fill the lower left part with 0
	double *Ux = REAL(R_do_slot(U, Matrix_xSym));
	for (i = 0; i < m; i++)
	    for (size_t j = i*(m_+1) +1; j < (i+1)*m_; j++)
		Ux[j] = 0.;
    }

    R_do_slot_assign(P, Matrix_DimSym, duplicate(dd));
    if(!is_sq) // m != n -- P is  m x m
	INTEGER(R_do_slot(P, Matrix_DimSym))[1] = m;
    perm = INTEGER(ALLOC_SLOT(P, Matrix_permSym, INTSXP, m));
    C_or_Alloca_TO(iperm, m, int);

    for (i = 0; i < m; i++) iperm[i] = i + 1; /* initialize permutation*/
    for (i = 0; i < nn; i++) {	/* generate inverse permutation */
	int newp = pivot[i] - 1;
	if (newp != i) { // swap
	    int tmp = iperm[i]; iperm[i] = iperm[newp]; iperm[newp] = tmp;
	}
    }
    // invert the inverse
    for (i = 0; i < m; i++) perm[iperm[i] - 1] = i + 1;

    if(m >= SMALL_4_Alloca) Free(iperm);
    UNPROTECT(1);
    return val;
}
