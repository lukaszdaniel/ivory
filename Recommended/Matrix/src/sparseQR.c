#include "sparseQR.h"

SEXP sparseQR_validate(SEXP x)
{
    CSP V = AS_CSP__(R_do_slot(x, Matrix_VSym)),
	R = AS_CSP__(R_do_slot(x, Matrix_RSym));
    SEXP beta = R_do_slot(x, Matrix_betaSym),
	p = R_do_slot(x, Matrix_pSym),
	q = R_do_slot(x, install("q"));
    R_CheckStack();

    if (LENGTH(p) != V->m)
	return mkString(_("'length(p)' must match 'nrow(V)'"));
    if (LENGTH(beta) != V->n)
	return mkString(_("'length(beta)' must match 'ncol(V)'"));
    int	lq = LENGTH(q);
    if (lq && lq != R->n)
	return mkString(_("'length(q)' must be zero or 'ncol(R)'"));
    if (V->n != R->n)
	return mkString("ncol(V) != ncol(R)");
    /* FIXME: Check that the permutations are permutations */
    return ScalarLogical(1);
}

/**
 * Apply Householder transformations and the row permutation P to y
 *
 * @param V sparse matrix containing the vectors defining the
 *        Householder transformations
 * @param dmns == dimnames(V) or "NULL" (R_NilValue)
 * @param beta scaling factors for the Householder transformations
 * @param p 0-based permutation vector of length V->m
 * @param trans logical value - if TRUE create Q'y[p] otherwise Qy[p]
 * @param ans : both function argument and result ("input and output")
 */
static
void sparseQR_Qmult(cs *V, SEXP dmns, double *beta, int *p, int trans,
		    /* --> */ SEXP ans)
{
    double *y  = REAL(   R_do_slot(ans, Matrix_xSym));
    int *ydims = INTEGER(R_do_slot(ans, Matrix_DimSym));
    /* y: contents of a V->m by nrhs, i.e. dim(y) == ydims[0:1], dense matrix
     * -- Note that V->m = m2 : V may contain "spurious 0 rows" (structural rank deficiency) */
    int m = V->m, n = V->n;
    if (ydims[0] != m)
	error(_("sparseQR_Qmult(): nrow(y) = %d != %d = nrow(V)"), ydims[0], m);
    double *x; // workspace
    C_or_Alloca_TO(x, m, double);
    if (trans) {
	for (int j = 0; j < ydims[1]; j++) {
	    double *yj = y + j * m;
	    cs_pvec(p, yj, x, m);	/* x(0:m-1) = y(p(0:m-1), j) */
	    Memcpy(yj, x, m);	/* replace it */
	    for (int k = 0 ; k < n ; k++)   /* apply H[1]...H[n] */
		cs_happly(V, k, beta[k], yj);
	}
    } else {
	for (int j = 0; j < ydims[1]; j++) {
	    double *yj = y + j * m;
	    for (int k = n - 1 ; k >= 0 ; k--) /* apply H[n]...H[1] */
		cs_happly(V, k, beta[k], yj);
	    cs_ipvec(p, yj, x, m); /* inverse permutation */
	    Memcpy(yj, x, m);
	}
    }
    if(m >= SMALL_4_Alloca) Free(x);
    if(!isNull(dmns)) { // assign rownames to 'ans' matrix
	// FIXME? colnames taken from 'y' ?!
	if(!isNull(VECTOR_ELT(dmns, 0))) {
	    SEXP nms_0 = PROTECT(duplicate(VECTOR_ELT(dmns, 0)));
	    SET_VECTOR_ELT(R_do_slot(ans, Matrix_DimNamesSym), 0, nms_0);
	    UNPROTECT(1);
	}
    }
} // sparseQR_Qmult


/**
* Given a sparse QR decomposition and y,  compute  Q y  or  Q'y
*
* @param qr a "sparseQR" object
* @param y a (dense) Matrix
* @param trans logical, if TRUE compute   Q'y   else  Q y
* @return Q'y ("qty")  or   Qy ("qy")
*/
SEXP sparseQR_qty(SEXP qr, SEXP y, SEXP trans, SEXP keep_dimnames)
{

//--- will be prepended also to other  sparseQR_..() functions below -----------
#define INIT_sparseQR_(_DM_NMS_)					\
    SEXP V_ = R_do_slot(qr, Matrix_VSym);				\
    CSP V = AS_CSP__(V_);						\
    R_CheckStack();							\
    SEXP ans, aa, dmns = R_NilValue;					\
    if(_DM_NMS_) dmns = R_do_slot(V_, Matrix_DimNamesSym);		\
    PROTECT_INDEX ipx;                                                  \
    PROTECT_WITH_INDEX(ans = dup_mMatrix_as_dgeMatrix(y), &ipx);	\
    int *ydims = INTEGER(R_do_slot(ans, Matrix_DimSym)),			\
	m = ydims[0], n = ydims[1], M = V->m, *d_a = NULL;			\
    Rboolean rank_def = (m < M);					\
    if(rank_def) { /* must add 0-rows to y, i.e. ans, and remove them *before* return */ \
	aa = PROTECT(NEW_OBJECT_OF_CLASS("dgeMatrix"));			\
	d_a = INTEGER(R_do_slot(aa, Matrix_DimSym)); d_a[0] = M; d_a[1] = n; \
	SEXP dn = R_do_slot(aa, Matrix_DimNamesSym);			\
	SET_VECTOR_ELT(dn, 1,						\
		       duplicate(VECTOR_ELT(R_do_slot(ans, Matrix_DimNamesSym), 1))); \
	R_do_slot_assign(aa, Matrix_DimNamesSym, dn);				\
	double *yy = REAL( R_do_slot(ans, Matrix_xSym));     /* m * n */	\
	double *xx = REAL(ALLOC_SLOT(aa, Matrix_xSym, REALSXP, M * n));	\
	for(int j = 0; j < n; j++) { /* j-th column */			\
	    Memcpy(xx + j*M, yy + j*m, m); /* copy          x[   1:m , j ] := yy[,j] */	\
	    for(int i = m; i < M; i++) xx[i + j*M] = 0.;/*  x[(m+1):M, j ] := 0	*/ \
	}								\
	REPROTECT(ans = duplicate(aa), ipx); /* is  M x n  now */	\
    }

//--- end {INIT_sparseQR_} -----------------------------------------------------

    INIT_sparseQR_(TRUE)
	;
    sparseQR_Qmult(V, dmns, REAL(R_do_slot(qr, Matrix_betaSym)),
		   INTEGER(R_do_slot(qr, Matrix_pSym)), asLogical(trans),
		   ans);
#define EXIT_sparseQR_							\
    /* remove the extra rows from ans */				\
	d_a[0] = m;/* -> @Dim is ok;  @Dimnames (i.e. colnames) still are */ \
	double *yy = REAL( R_do_slot(ans, Matrix_xSym)); /* is  M  x n */ \
	double *xx = REAL(ALLOC_SLOT(aa, Matrix_xSym, REALSXP, m * n));	\
	for(int j = 0; j < n; j++) { /*  j-th column */			\
	    Memcpy(xx + j*m, yy + j*M, m); /* copy    x[ 1:m, j ] := yy[,j] */ \
	}								\
	ans = duplicate(aa); /*  m x n  finally */			\
	UNPROTECT(1) // aa


    if(rank_def) {
	warning(_("%s(): structurally rank deficient case: possibly WRONG zeros"),
		"sparseQR_qty");
	EXIT_sparseQR_;
    }

    UNPROTECT(1);
    return ans;
}

// Compute  qr.coef(qr, y)  :=  R^{-1} Q' y   {modulo row and column permutations}
SEXP sparseQR_coef(SEXP qr, SEXP y)
{
    SEXP qslot = R_do_slot(qr, install("q")), R_ = R_do_slot(qr, Matrix_RSym);
    CSP	R = AS_CSP__(R_);
    // FIXME: check  n_R, M (= R->m)   vs  n, m
    int *q = INTEGER(qslot), lq = LENGTH(qslot), n_R = R->n; // = ncol(R)
    INIT_sparseQR_(FALSE); // <- FALSE: no dimnames from V
    // ans := R^{-1} Q' y ==>  rownames(ans) := rownames(R^{-1}) = colnames(R)
    dmns = PROTECT(duplicate(R_do_slot(R_, Matrix_DimNamesSym)));
    if(!isNull(VECTOR_ELT(dmns, 1))) { // need to correctly *permute* the colnames
	SEXP cns = PROTECT(duplicate(VECTOR_ELT(dmns, 1)));
	// *back* permute colnames from 'qslot' :
	for(int j=0; j < lq; j++)
	    SET_STRING_ELT(VECTOR_ELT(dmns, 1), q[j], STRING_ELT(cns, j));
	UNPROTECT(1);
    }

    // rownames(ans) := colnames(ans)
    SET_VECTOR_ELT(dmns, 0, VECTOR_ELT(dmns, 1));

    /* apply row permutation and multiply by Q' */
    sparseQR_Qmult(V, dmns, REAL(R_do_slot(qr, Matrix_betaSym)),
		   INTEGER(R_do_slot(qr, Matrix_pSym)), /* trans = */ TRUE,
		   ans);
    UNPROTECT(1); // dmns

    double *ax = REAL(R_do_slot(ans, Matrix_xSym)), *x;
    if(lq) { C_or_Alloca_TO(x, M, double); }
    for (int j = 0; j < n; j++) {
	double *aj = ax + j * M;
	cs_usolve(R, aj);
	if (lq) {
	    cs_ipvec(q, aj, x, n_R);
	    Memcpy(aj, x, n_R);
	}
    }
    if(lq && M >= SMALL_4_Alloca) Free(x);

    if(rank_def) {
	warning(_("%s(): structurally rank deficient case: possibly WRONG zeros"),
		"sparseQR_coef");
	EXIT_sparseQR_;
    }

    UNPROTECT(1);
    return ans;
}

/** Compute  qr.resid(qr, y)   or   qr.fitted(qr, y)
*/
SEXP sparseQR_resid_fitted(SEXP qr, SEXP y, SEXP want_resid)
{
    int *p = INTEGER(R_do_slot(qr, Matrix_pSym)),
	resid = asLogical(want_resid);
    double *beta = REAL(R_do_slot(qr, Matrix_betaSym));

    INIT_sparseQR_(FALSE);
    //             ..... ans should get rownames of 'y' ...
    /* apply row permutation and multiply by Q' */
    sparseQR_Qmult(V, dmns, beta, p, /* trans = */ TRUE, ans);

    double *ax = REAL(R_do_slot(ans, Matrix_xSym));
// FIXME   (n,m) := dim(y)   vs  (N,M) := dim(V)  -- ok ??
    int N = V->n; // M = V->m  (in INIT_.. above)
    for (int j = 0; j < n; j++) {
	if (resid) // qr.resid(): zero first N rows
	    for (int i = 0; i < N; i++) ax[i + j * M] = 0;
	else // qr.fitted(): zero last M - N rows
	    for (int i = N; i < M; i++) ax[i + j * M] = 0;
    }
    /* multiply by Q and apply inverse row permutation */
    sparseQR_Qmult(V, dmns, beta, p, /* trans = */ FALSE, ans);

    if(rank_def) {
	warning(_("%s(): structurally rank deficient case: possibly WRONG zeros"),
		"sparseQR_resid_fitted");
	EXIT_sparseQR_;
    }

    UNPROTECT(1);
    return ans;
}
