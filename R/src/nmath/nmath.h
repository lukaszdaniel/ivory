/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998-2020  The R Core Team
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

/* Private header file for use during compilation of Mathlib */
#ifndef MATHLIB_PRIVATE_H
#define MATHLIB_PRIVATE_H

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

/* Required by C99 but might be slow */
#include <R_ext/Ldouble.h>
#include <R_ext/Minmax.h>

/* To ensure atanpi, cospi,  sinpi, tanpi are defined */
# ifndef __STDC_WANT_IEC_60559_FUNCS_EXT__
#  define __STDC_WANT_IEC_60559_FUNCS_EXT__ 1
# endif

#include <math.h>
#include <float.h> /* DBL_MIN etc */

#include <Rconfig.h>
#include <Rmath.h>

/* Used internally only */
double  Rf_d1mach(int);
double	Rf_gamma_cody(double);

#include <R_ext/RS.h>

/* possibly needed for debugging */
#include <R_ext/Print.h>

/* moved from dpq.h */
#ifdef HAVE_NEARBYINT
# define R_forceint(x)   nearbyint(x)
#else
# define R_forceint(x)   round(x)
#endif
//R >= 3.1.0: # define R_nonint(x) 	  (fabs((x) - R_forceint(x)) > 1e-7)
# define R_nonint(x) 	  (fabs((x) - R_forceint(x)) > 1e-7*Rf_fmax2(1., fabs(x)))

#ifndef MATHLIB_STANDALONE

#include <R_ext/Error.h>
# define MATHLIB_ERROR(fmt,x)		Rf_error(fmt,x);
# define MATHLIB_WARNING(fmt,x)		Rf_warning(fmt,x)
# define MATHLIB_WARNING2(fmt,x,x2)	Rf_warning(fmt,x,x2)
# define MATHLIB_WARNING3(fmt,x,x2,x3)	Rf_warning(fmt,x,x2,x3)
# define MATHLIB_WARNING4(fmt,x,x2,x3,x4) Rf_warning(fmt,x,x2,x3,x4)
# define MATHLIB_WARNING5(fmt,x,x2,x3,x4,x5) Rf_warning(fmt,x,x2,x3,x4,x5)

#include <R_ext/Arith.h>
#define ML_POSINF	R_PosInf
#define ML_NEGINF	R_NegInf
#define ML_NAN		R_NaN

extern "C"
void R_CheckUserInterrupt(void);
/* Ei-ji Nakama reported that AIX 5.2 has calloc as a macro and objected
   to redefining it.  Tests added for 2.2.1 */
// #ifdef calloc
// # undef calloc
// #endif
// #define calloc R_chk_calloc
// #ifdef free
// # undef free
// #endif
// #define free R_chk_free

/* Localization */
#include <Localization.h>

#else
/* Mathlib standalone */

#include <stdio.h>
#include <stdlib.h> /* for exit */
#define MATHLIB_ERROR(fmt,x)	{ printf(fmt,x); exit(1); }
#define MATHLIB_WARNING(fmt,x)		printf(fmt,x)
#define MATHLIB_WARNING2(fmt,x,x2)	printf(fmt,x,x2)
#define MATHLIB_WARNING3(fmt,x,x2,x3)	printf(fmt,x,x2,x3)
#define MATHLIB_WARNING4(fmt,x,x2,x3,x4) printf(fmt,x,x2,x3,x4)
#define MATHLIB_WARNING5(fmt,x,x2,x3,x4,x5) printf(fmt,x,x2,x3,x4,x5)

#define ISNAN(x) (isnan(x)!=0)
// Arith.h defines it
#ifndef R_FINITE
#ifdef HAVE_WORKING_ISFINITE
/* isfinite is defined in <math.h> according to C99 */
# define R_FINITE(x)    isfinite(x)
#else
# define R_FINITE(x)    R_finite(x)
#endif
#endif
int R_finite(double);

#define ML_POSINF	(1.0 / 0.0)
#define ML_NEGINF	((-1.0) / 0.0)
#define ML_NAN		(0.0 / 0.0)

#define _(String) String
#endif /* standalone */

#define ML_VALID(x)	(!ISNAN(x))

#define ME_NONE		0
/*	no error */
#define ME_DOMAIN	1
/*	argument out of domain */
#define ME_RANGE	2
/*	value out of range */
#define ME_NOCONV	4
/*	process did not converge */
#define ME_PRECISION	8
/*	does not have "full" precision */
#define ME_UNDERFLOW	16
/*	and underflow occured (important for IEEE)*/


#define ML_WARN_return_NAN { ML_WARNING(ME_DOMAIN, ""); return ML_NAN; }

/* For a long time prior to R 2.3.0 ML_WARNING did nothing.
   We don't report ME_DOMAIN errors as the callers collect ML_NANs into
   a single warning.
 */
#define ML_WARNING(x, s)                                                           \
   {                                                                               \
      if (x > ME_DOMAIN)                                                           \
      {                                                                            \
         const char *msg = "";                                                     \
         switch (x)                                                                \
         {                                                                         \
         case ME_DOMAIN:                                                           \
            msg = _("argument out of domain in '%s' function");                    \
            break;                                                                 \
         case ME_RANGE:                                                            \
            msg = _("value out of range in '%s' function");                        \
            break;                                                                 \
         case ME_NOCONV:                                                           \
            msg = _("convergence failed in '%s' function");                        \
            break;                                                                 \
         case ME_PRECISION:                                                        \
            msg = _("full precision may not have been achieved in '%s' function"); \
            break;                                                                 \
         case ME_UNDERFLOW:                                                        \
            msg = _("underflow occurred in '%s' function");                        \
            break;                                                                 \
         }                                                                         \
         MATHLIB_WARNING(msg, s);                                                  \
      }                                                                            \
   }

/* Wilcoxon Rank Sum Distribution */

#define WILCOX_MAX 50

#include <R_ext/Visibility.h>

/* Formerly private part of Mathlib.h */

/* always remap internal functions */
#define bd0       	Rf_bd0
#define chebyshev_eval	Rf_chebyshev_eval
#define chebyshev_init	Rf_chebyshev_init
#define gammalims	Rf_gammalims
#define lfastchoose	Rf_lfastchoose
#define lgammacor	Rf_lgammacor
#define stirlerr       	Rf_stirlerr
#define pnchisq_raw   	Rf_pnchisq_raw
#define pgamma_raw   	Rf_pgamma_raw
#define pnbeta_raw   	Rf_pnbeta_raw
#define pnbeta2       	Rf_pnbeta2
#define bratio       	Rf_bratio

	/* Chebyshev Series */

HIDDEN int Rf_chebyshev_init(double*, int, double);
HIDDEN double Rf_chebyshev_eval(double, const double *, const int);

/* Gamma and Related Functions */

HIDDEN void Rf_gammalims(double*, double*);
HIDDEN double Rf_lgammacor(double); /* log(gamma) correction */
HIDDEN double Rf_stirlerr(double);  /* Stirling expansion "error" */

HIDDEN double Rf_lfastchoose(double, double);

HIDDEN double Rf_bd0(double, double);

HIDDEN double Rf_pnchisq_raw(double, double, double, double, double, int, Rboolean, Rboolean);
HIDDEN double Rf_pgamma_raw(double, double, int, int);
HIDDEN double Rf_pbeta_raw(double, double, double, int, int);
HIDDEN double Rf_qchisq_appr(double, double, double, int, int, double tol);
HIDDEN LDOUBLE Rf_pnbeta_raw(double, double, double, double, double);
HIDDEN double Rf_pnbeta2(double, double, double, double, double, int, int);

int Rf_i1mach(int);

/* From toms708.cpp */
HIDDEN void Rf_bratio(double a, double b, double x, double y,
	    		     double *w, double *w1, int *ierr, int log_p);


#endif /* MATHLIB_PRIVATE_H */
