/*
 * The authors of this software are Cleveland, Grosse, and Shyu.
 * Copyright (c) 1989, 1992 by AT&T.
 * Permission to use, copy, modify, and distribute this software for any
 * purpose without fee is hereby granted, provided that this entire notice
 * is included in all copies of any software which is or includes a copy
 * or modification of this software and in all copies of the supporting
 * documentation for such software.
 * THIS SOFTWARE IS BEING PROVIDED "AS IS", WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTY.  IN PARTICULAR, NEITHER THE AUTHORS NOR AT&T MAKE ANY
 * REPRESENTATION OR WARRANTY OF ANY KIND CONCERNING THE MERCHANTABILITY
 * OF THIS SOFTWARE OR ITS FITNESS FOR ANY PARTICULAR PURPOSE.
 */

/* <UTF8> chars are handled as whole strings.
   They are passed from Fortran so had better be ASCII.
 */

/*
 *  Altered by B.D. Ripley to use F77_*, declare routines before use.
 *
 *  'protoize'd to ANSI C headers; indented: M.Maechler
 *
 *  Changes to the C/Fortran interface to support LTO in May 2019.
 */

#ifdef HAVE_CONFIG_H
// for FC_LEN_T
#include <config.h>
#endif

#include <string.h>
#include <stdio.h>
#include <math.h>
#include <limits.h>
#include <R.h>
#include <R_ext/Minmax.h>
#include "localization.h"

/* Forward declarations */
static
void loess_workspace(int *d, int *n, double *span, int *degree,
		     int *nonparametric, int *drop_square,
		     int *sum_drop_sqr, int *setLf);
static
void loess_prune(int *parameter, int *a,
		 double *xi, double *vert, double *vval);
static
void loess_grow (int *parameter, int *a,
		 double *xi, double *vert, double *vval);

/* These (and many more) are in ./loessf.f : */
void F77_NAME(lowesa)(double*, int*, int*, int*, int*, double*, double*);
void F77_NAME(lowesb)(double*, double*, double*, double*, int*, int*, int*,
		      int*, double*);
void F77_NAME(lowesc)(int*, double*, double*, double*, double*, double*);
void F77_NAME(lowesd)(int*, int*, int*, int*, double*, int*, int*,
		      double*, int*, int*, int*);
void F77_NAME(lowese)(int*, int*, int*, double*, int*, double*, double*);
void F77_NAME(lowesf)(double*, double*, double*, int*, int*, int*, double*,
		      int*, double*, double*, int*, double*);
void F77_NAME(lowesl)(int*, int*, int*, double*, int*, double*, double*);
void F77_NAME(ehg169)(int*, int*, int*, int*, int*, int*,
		      double*, int*, double*, int*, int*, int*);
void F77_NAME(ehg196)(int*, int*, double*, double*);
/* exported (for loessf.f) : */
void F77_SUB(ehg182)(int *i);
#ifdef FC_LEN_T
# include <stddef.h>
void F77_SUB(ehg183a)(char *s, int *nc,int *i,int *n,int *inc, FC_LEN_T c1);
void F77_SUB(ehg184a)(char *s, int *nc, double *x, int *n, int *inc, FC_LEN_T c1);
#else
void F77_SUB(ehg183a)(char *s, int *nc,int *i,int *n,int *inc);
void F77_SUB(ehg184a)(char *s, int *nc, double *x, int *n, int *inc);
#endif



#define	GAUSSIAN	1
#define SYMMETRIC	0

static int	*iv = NULL, liv, lv, tau;
static double	*v = NULL;

/* these are set in an earlier call to loess_workspace or loess_grow */
static void loess_free(void)
{
    Free(v);
    Free(iv);
}

void
loess_raw(double *y, double *x, double *weights, double *robust, int *d,
	  int *n, double *span, int *degree, int *nonparametric,
	  int *drop_square, int *sum_drop_sqr, double *cell,
	  char **surf_stat, double *surface, int *parameter,
	  int *a, double *xi, double *vert, double *vval, double *diagonal,
	  double *trL, double *one_delta, double *two_delta, int *setLf)
{
    int zero = 0, one = 1, two = 2, nsing, i, k;
    double *hat_matrix, *LL, dzero=0.0;

    *trL = 0;

    loess_workspace(d, n, span, degree, nonparametric, drop_square,
		    sum_drop_sqr, setLf);
    v[1] = *cell;/* = v(2) in Fortran (!) */

    /* NB:  surf_stat  =  (surface / statistics);
     *                               statistics = "none" for all robustness iterations
     */
    if(!strcmp(*surf_stat, "interpolate/none")) { // default for loess.smooth() and robustness iter.
	F77_CALL(lowesb)(x, y, robust, &dzero, &zero, iv, &liv, &lv, v);
	F77_CALL(lowese)(iv, &liv, &lv, v, n, x, surface);
	loess_prune(parameter, a, xi, vert, vval);
    }
    else if (!strcmp(*surf_stat, "direct/none")) {
	F77_CALL(lowesf)(x, y, robust, iv, &liv, &lv, v, n, x,
			 &dzero, &zero, surface);
    }
    else if (!strcmp(*surf_stat, "interpolate/1.approx")) { // default (trace.hat is "exact")
	F77_CALL(lowesb)(x, y, weights, diagonal, &one, iv, &liv, &lv, v);
	F77_CALL(lowese)(iv, &liv, &lv, v, n, x, surface);
	nsing = iv[29];
	for(i = 0; i < (*n); i++) *trL = *trL + diagonal[i];
	F77_CALL(lowesa)(trL, n, d, &tau, &nsing, one_delta, two_delta);
	loess_prune(parameter, a, xi, vert, vval);
    }
    else if (!strcmp(*surf_stat, "interpolate/2.approx")) { // default for trace.hat = "approximate"
	//                     vvvvvvv (had 'robust' in R <= 3.2.x)
	F77_CALL(lowesb)(x, y, weights, &dzero, &zero, iv, &liv, &lv, v);
	F77_CALL(lowese)(iv, &liv, &lv, v, n, x, surface);
	nsing = iv[29];
	F77_CALL(ehg196)(&tau, d, span, trL);
	F77_CALL(lowesa)(trL, n, d, &tau, &nsing, one_delta, two_delta);
	loess_prune(parameter, a, xi, vert, vval);
    }
    else if (!strcmp(*surf_stat, "direct/approximate")) {
	F77_CALL(lowesf)(x, y, weights, iv, &liv, &lv, v, n, x,
			diagonal, &one, surface);
	nsing = iv[29];
	for(i = 0; i < (*n); i++) *trL = *trL + diagonal[i];
	F77_CALL(lowesa)(trL, n, d, &tau, &nsing, one_delta, two_delta);
    }
    else if (!strcmp(*surf_stat, "interpolate/exact")) {
	hat_matrix = (double *) R_alloc((*n)*(*n), sizeof(double));
	LL = (double *) R_alloc((*n)*(*n), sizeof(double));
	F77_CALL(lowesb)(x, y, weights, diagonal, &one, iv, &liv, &lv, v);
	F77_CALL(lowesl)(iv, &liv, &lv, v, n, x, hat_matrix);
	F77_CALL(lowesc)(n, hat_matrix, LL, trL, one_delta, two_delta);
	F77_CALL(lowese)(iv, &liv, &lv, v, n, x, surface);
	loess_prune(parameter, a, xi, vert, vval);
    }
    else if (!strcmp(*surf_stat, "direct/exact")) {
	hat_matrix = (double *) R_alloc((*n)*(*n), sizeof(double));
	LL = (double *) R_alloc((*n)*(*n), sizeof(double));
	F77_CALL(lowesf)(x, y, weights, iv, &liv, &lv, v, n, x,
			hat_matrix, &two, surface);
	F77_CALL(lowesc)(n, hat_matrix, LL, trL, one_delta, two_delta);
	k = (*n) + 1;
	for(i = 0; i < (*n); i++)
	    diagonal[i] = hat_matrix[i * k];
    }
    loess_free();
}

void
loess_dfit(double *y, double *x, double *x_evaluate, double *weights,
	   double *span, int *degree, int *nonparametric,
	   int *drop_square, int *sum_drop_sqr,
	   int *d, int *n, int *m, double *fit)
{
    int zero = 0;
    double dzero = 0.0;

    loess_workspace(d, n, span, degree, nonparametric, drop_square,
		    sum_drop_sqr, &zero);
    F77_CALL(lowesf)(x, y, weights, iv, &liv, &lv, v, m, x_evaluate,
		    &dzero, &zero, fit);
    loess_free();
}

void
loess_dfitse(double *y, double *x, double *x_evaluate, double *weights,
	     double *robust, int *family, double *span, int *degree,
	     int *nonparametric, int *drop_square,
	     int *sum_drop_sqr,
	     int *d, int *n, int *m, double *fit, double *L)
{
    int zero = 0, two = 2;
    double dzero = 0.0;

    loess_workspace(d, n, span, degree, nonparametric, drop_square,
		    sum_drop_sqr, &zero);
    if(*family == GAUSSIAN)
	F77_CALL(lowesf)(x, y, weights, iv, &liv, &lv, v, m,
			x_evaluate, L, &two, fit);
    else if(*family == SYMMETRIC)
    {
	F77_CALL(lowesf)(x, y, weights, iv, &liv, &lv, v, m,
			x_evaluate, L, &two, fit);
	F77_CALL(lowesf)(x, y, robust, iv, &liv, &lv, v, m,
			x_evaluate, &dzero, &zero, fit);
    }
    loess_free();
}

void
loess_ifit(int *parameter, int *a, double *xi, double *vert,
	   double *vval, int *m, double *x_evaluate, double *fit)
{
    loess_grow(parameter, a, xi, vert, vval);
    F77_CALL(lowese)(iv, &liv, &lv, v, m, x_evaluate, fit);
    loess_free();
}

void
loess_ise(double *y, double *x, double *x_evaluate, double *weights,
	  double *span, int *degree, int *nonparametric,
	  int *drop_square, int *sum_drop_sqr, double *cell,
	  int *d, int *n, int *m, double *fit, double *L)
{
    int zero = 0, one = 1;
    double dzero = 0.0;

    loess_workspace(d, n, span, degree, nonparametric, drop_square,
		    sum_drop_sqr, &one);
    v[1] = *cell;
    F77_CALL(lowesb)(x, y, weights, &dzero, &zero, iv, &liv, &lv, v);
    F77_CALL(lowesl)(iv, &liv, &lv, v, m, x_evaluate, L);
    loess_free();
}

void
loess_workspace(int *d, int *n, double *span, int *degree,
		int *nonparametric, int *drop_square,
		int *sum_drop_sqr, int *setLf)
{
    int D = *d, N = *n, tau0, nvmax, nf, version = 106, i;

    nvmax = max(200, N);
    nf = min(N, (int) floor(N * (*span) + 1e-5));
    if(nf <= 0) error(_("'%s' argument is too small"), "span");
    tau0 = ((*degree) > 1) ? (int)((D + 2) * (D + 1) * 0.5) : (D + 1);
    tau = tau0 - (*sum_drop_sqr);
    lv = 50 + (3 * D + 3) * nvmax + N + (tau0 + 2) * nf;
    double dliv = 50 + (pow(2.0, (double)D) + 4.0) * nvmax + 2.0 * N;
    if (dliv < INT_MAX) liv = (int) dliv;
    else error(_("workspace required is too large"));
    if(*setLf) {
	lv = lv + (D + 1) * nf * nvmax;
	liv = liv + nf * nvmax;
    }
    iv = Calloc(liv, int);
    v = Calloc(lv, double);

    F77_CALL(lowesd)(&version, iv, &liv, &lv, v, d, n, span, degree,
		    &nvmax, setLf);
    iv[32] = *nonparametric;
    for(i = 0; i < D; i++)
	iv[i + 40] = drop_square[i];
}

static void
loess_prune(int *parameter, int *a, double *xi, double *vert,
	    double *vval)
{
    int d, vc, a1, v1, xi1, vv1, nc, nv, nvmax, i, k;

    d = iv[1];
    vc = iv[3] - 1;
    nc = iv[4];
    nv = iv[5];
    a1 = iv[6] - 1;
    v1 = iv[10] - 1;
    xi1 = iv[11] - 1;
    vv1 = iv[12] - 1;
    nvmax = iv[13];

    for(i = 0; i < 5; i++)
	parameter[i] = iv[i + 1];
    parameter[5] = iv[21] - 1;
    parameter[6] = iv[14] - 1;

    for(i = 0; i < d; i++) {
	k = nvmax * i;
	vert[i] = v[v1 + k];
	vert[i + d] = v[v1 + vc + k];
    }
    for(i = 0; i < nc; i++) {
	xi[i] = v[xi1 + i];
	a[i] = iv[a1 + i];
    }
    k = (d + 1) * nv;
    for(i = 0; i < k; i++)
	vval[i] = v[vv1 + i];
}

static void
loess_grow(int *parameter, int *a, double *xi,
	   double *vert, double *vval)
{
    int d, vc, nc, nv, a1, v1, xi1, vv1, i, k;

    d = parameter[0];
    vc = parameter[2];
    nc = parameter[3];
    nv = parameter[4];
    liv = parameter[5];
    lv = parameter[6];
    iv = Calloc(liv, int);
    v = Calloc(lv, double);

    iv[1] = d;
    iv[2] = parameter[1];
    iv[3] = vc;
    iv[5] = iv[13] = nv;
    iv[4] = iv[16] = nc;
    iv[6] = 50;
    iv[7] = iv[6] + nc;
    iv[8] = iv[7] + vc * nc;
    iv[9] = iv[8] + nc;
    iv[10] = 50;
    iv[12] = iv[10] + nv * d;
    iv[11] = iv[12] + (d + 1) * nv;
    iv[27] = 173;

    v1 = iv[10] - 1;
    xi1 = iv[11] - 1;
    a1 = iv[6] - 1;
    vv1 = iv[12] - 1;

    for(i = 0; i < d; i++) {
	k = nv * i;
	v[v1 + k] = vert[i];
	v[v1 + vc - 1 + k] = vert[i + d];
    }
    for(i = 0; i < nc; i++) {
	v[xi1 + i] = xi[i];
	iv[a1 + i] = a[i];
    }
    k = (d + 1) * nv;
    for(i = 0; i < k; i++)
	v[vv1 + i] = vval[i];

    F77_CALL(ehg169)(&d, &vc, &nc, &nc, &nv, &nv, v+v1, iv+a1,
		    v+xi1, iv+iv[7]-1, iv+iv[8]-1, iv+iv[9]-1);
}


/* begin ehg's FORTRAN-callable C-codes */

void F77_SUB(ehg182)(int *i)
{
    char *msg = NULL, msg2[50];

switch(*i){
 case 100:sprintf(msg, _("wrong version number in lowesd. Probably typo in caller.")); break ;
 case 101:sprintf(msg, _("d>dMAX in ehg131.  Need to recompile with increased dimensions.")); break ;
 case 102:sprintf(msg, _("'%s' argument is too small. (Discovered by 'lowesd()' function)"), "liv"); break ;
 case 103:sprintf(msg, _("'%s' argument is too small. (Discovered by 'lowesd()' function)"), "lv"); break ;
 case 104:sprintf(msg, _("'span' argument is too small. Fewer data values than degrees of freedom.")); break ;
 case 105:sprintf(msg, _("k>d2MAX in ehg136.  Need to recompile with increased dimensions.")); break ;
 case 106:sprintf(msg, _("'%s' argument is too small"), "lwork"); break ;
 case 107:sprintf(msg, _("invalid '%s' value"), "kernel"); break ;
 case 108:sprintf(msg, _("invalid '%s' value"), "ideg"); break ;
 case 109:sprintf(msg, _("lowstt only applies when 'kernel=1'.")); break ;
 case 110:sprintf(msg, _("not enough extra workspace for robustness calculation")); break ;
 case 120:sprintf(msg, _("zero-width neighborhood. make span bigger")); break ;
 case 121:sprintf(msg, _("all data on boundary of neighborhood. make span bigger")); break ;
 case 122:sprintf(msg, _("extrapolation not allowed with blending")); break ;
 case 123:sprintf(msg, _("ihat=1 (diag L) in 'l2fit()' function only makes sense if z=x (eval=data).")); break ;
 case 171:sprintf(msg, _("'lowesd()' function must be called first.")); break ;
 case 172:sprintf(msg, _("'lowesf()' function must not come between lowesb and lowese, lowesr, or lowesl.")); break ;
 case 173:sprintf(msg, _("'lowesb()' function must come before lowese, lowesr, or lowesl.")); break ;
 case 174:sprintf(msg, _("'lowesb()' function need not be called twice.")); break ;
 case 175:sprintf(msg, _("need setLf=.true. for lowesl.")); break ;
 case 180:sprintf(msg, _("nv>nvmax in 'cpvert()' function")); break ;
 case 181:sprintf(msg, _("nt>20 in 'eval()' function")); break ;
 case 182:sprintf(msg, _("svddc failed in 'l2fit()' function")); break ;
 case 183:sprintf(msg, _("didnt find edge in 'vleaf()' function")); break ;
 case 184:sprintf(msg, _("zero-width cell found in 'vleaf()' function")); break ;
 case 185:sprintf(msg, _("trouble descending to leaf in 'vleaf()' function")); break ;
 case 186:sprintf(msg, _("insufficient workspace for 'lowesf()' function")); break ;
 case 187:sprintf(msg, _("insufficient stack space")); break ;
 case 188:sprintf(msg, _("lv too small for computing explicit L")); break ;
 case 191:sprintf(msg, _("computed trace L was negative; something is wrong!")); break ;
 case 192:sprintf(msg, _("computed delta was negative; something is wrong!")); break ;
 case 193:sprintf(msg, _("workspace in loread appears to be corrupted")); break ;
 case 194:sprintf(msg, _("trouble in l2fit/l2tr")); break ;
 case 195:sprintf(msg, _("only constant, linear, or quadratic local models allowed")); break ;
 case 196:sprintf(msg, _("degree must be at least 1 for vertex influence matrix")); break ;
 case 999:sprintf(msg, _("not yet implemented")); break ;
 default: {
     snprintf(msg2, 50, _("Assert failed; error code %d\n"),*i);
     msg = msg2;
 }
}
warning(msg);
}

#ifdef FC_LEN_T
void F77_SUB(ehg183a)(char *s, int *nc,int *i,int *n,int *inc, FC_LEN_T c1)
#else
void F77_SUB(ehg183a)(char *s, int *nc,int *i,int *n,int *inc)
#endif
{
    char mess[4000], num[20];
    int j;
    strncpy(mess,s,*nc);
    mess[*nc] = '\0';
    for (j=0; j<*n; j++) {
	snprintf(num, 20, " %d",i[j * *inc]);
	strcat(mess,num);
    }
    strcat(mess,"\n");
    warning(mess);
}

#ifdef FC_LEN_T
void F77_SUB(ehg184a)(char *s, int *nc, double *x, int *n, int *inc, FC_LEN_T c1)
#else
void F77_SUB(ehg184a)(char *s, int *nc, double *x, int *n, int *inc)
#endif
{
    char mess[4000], num[30];
    int j;
    strncpy(mess,s,*nc);
    mess[*nc] = '\0';
    for (j=0; j<*n; j++) {
	snprintf(num,30," %.5g",x[j * *inc]);
	strcat(mess,num);
    }
    strcat(mess,"\n");
    warning(mess);
}
