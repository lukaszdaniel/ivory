/*
 *  Copyright (C) 2000-2015 The R Core Team
 *
 *  Algorithm AS 226 Appl. Statist. (1987) Vol. 36, No. 2
 *  by Russell V. Lenth
 *  Incorporates modification AS R84 from AS Vol. 39, pp311-2, 1990
 *                        and AS R95 from AS Vol. 44, pp551-2, 1995
 *  by H. Frick and Min Long Lam.
 *  original (C) Royal Statistical Society 1987, 1990, 1995
 *
 *  Returns the cumulative probability of x for the non-central
 *  beta distribution with parameters a, b and non-centrality ncp.
 *
 *  Auxiliary routines required:
 *	lgamma - log-gamma function
 *      pbeta  - incomplete-beta function {nowadays: Rf_pbeta_raw() -> Rf_bratio()}
 */

#include "nmath.h"
#include "dpq.h"

LDOUBLE HIDDEN Rf_pnbeta_raw(double x, double o_x, double a, double b, double ncp)
{
    /* o_x  == 1 - x  but maybe more accurate */

    /* change errmax and itrmax if desired;
     * original (AS 226, R84) had  (errmax; itrmax) = (1e-6; 100) */
    static constexpr double errmax = 1.0e-9;
    constexpr int    itrmax = 10000;  /* 100 is not enough for pf(ncp=200)
				     see PR#11277 */

    double a0, lbeta, c, errbd, x0, temp, tmp_c;
    int ierr;

    LDOUBLE ans, ax, gx, q, sumq;

    if (ncp < 0. || a <= 0. || b <= 0.) ML_WARN_return_NAN;

    if(x < 0. || o_x > 1. || (x == 0. && o_x == 1.)) return 0.;
    if(x > 1. || o_x < 0. || (x == 1. && o_x == 0.)) return 1.;

    c = ncp / 2.;

	/* initialize the series */

    x0 = floor(Rf_fmax2(c - 7. * sqrt(c), 0.));
    a0 = a + x0;
    lbeta = Rf_lgammafn(a0) + Rf_lgammafn(b) - Rf_lgammafn(a0 + b);
    /* temp = Rf_pbeta_raw(x, a0, b, TRUE, FALSE), but using (x, o_x): */
    Rf_bratio(a0, b, x, o_x, &temp, &tmp_c, &ierr, FALSE);

    gx = exp(a0 * log(x) + b * (x < .5 ? log1p(-x) : log(o_x))
	     - lbeta - log(a0));
    if (a0 > a)
	q = exp(-c + x0 * log(c) - Rf_lgammafn(x0 + 1.));
    else
	q = exp(-c);

    sumq = 1. - q;
    ans = ax = q * temp;

	/* recurse over subsequent terms until convergence is achieved */
    double j = floor(x0); // x0 could be billions, and is in package EnvStats
    do {
	j++;
	temp -= (double) gx;
	gx *= x * (a + b + j - 1.) / (a + j);
	q *= c / j;
	sumq -= q;
	ax = temp * q;
	ans += ax;
	errbd = (double)((temp - gx) * sumq);
    }
    while (errbd > errmax && j < itrmax + x0);

    if (errbd > errmax)
	ML_WARNING(ME_PRECISION, "Rf_pnbeta()");
    if (j >= itrmax + x0)
	ML_WARNING(ME_NOCONV, "Rf_pnbeta()");

    return ans;
}

HIDDEN double Rf_pnbeta2(double x, double o_x, double a, double b, double ncp,
	/* o_x  == 1 - x  but maybe more accurate */
	int lower_tail, int log_p)
{
    LDOUBLE ans = Rf_pnbeta_raw(x, o_x, a,b, ncp);

    /* return R_DT_val(ans), but we want to warn about cancellation here */
    if (lower_tail)
#ifdef HAVE_LONG_DOUBLE
	return (double) (log_p ? logl(ans) : ans);
#else
	return log_p ? log(ans) : ans;
#endif
    else {
	if (ans > 1. - 1e-10) ML_WARNING(ME_PRECISION, "pnbeta");
	if (ans > 1.0) ans = 1.0;  /* Precaution */
#if defined(HAVE_LONG_DOUBLE) && defined(HAVE_LOG1PL)
	return (double) (log_p ? log1pl(-ans) : (1. - ans));
#else
	/* include standalone case */
	return (double) (log_p ? log1p((double)-ans) : (1. - ans));
#endif
    }
}

double Rf_pnbeta(double x, double a, double b, double ncp,
	      int lower_tail, int log_p)
{
#ifdef IEEE_754
    if (ISNAN(x) || ISNAN(a) || ISNAN(b) || ISNAN(ncp))
	return x + a + b + ncp;
#endif

    R_P_bounds_01(x, 0., 1.);
    return Rf_pnbeta2(x, 1-x, a, b, ncp, lower_tail, log_p);
}