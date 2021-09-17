/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996, 1997  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 2000-2018	    The R Core Team
 *  Copyright (C) 2005		    The R Foundation
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

#ifndef R_RCOMPLEX_H
#define R_RCOMPLEX_H

/* GCC has problems with header files on e.g. Solaris.
   That OS defines the imaginary type, but GCC does not.
   Probably needed elsewhere, e.g. AIX, HP-UX (PR#15083)
   And use on Win32/64 suppresses warnings.
   The warning was also seen on macOS 10.5, but not later.
*/
#ifdef __cplusplus
#include <complex>
#define I std::complex<double>(0,1)
#elif defined(__GNUC__) && (defined(__sun__) || defined(__hpux__) || defined(_WIN32))
# undef  I
# define I (__extension__ 1.0iF)
#endif

/*
   Note: this could use the C11 CMPLX() macro.
   As could mycpow, z_tan and some of the substitutes.
 */
#ifdef __cplusplus
R_INLINE static std::complex<double> toC99(const Rcomplex *x) {
    std::complex<double> val(x->r, x->i);
    return val;
}
#else
R_INLINE static double complex toC99(const Rcomplex *x)
{
#if __GNUC__
    double complex ans = (double complex) 0; /* -Wall */
    __real__ ans = x->r;
    __imag__ ans = x->i;
    return ans;
#else
    return x->r + x->i * I;
#endif
}
#endif

#ifdef __cplusplus
R_INLINE static void SET_C99_COMPLEX(Rcomplex *x, R_xlen_t i, std::complex<double> value)
{
    Rcomplex *ans = x + i;
    ans->r = value.real();
    ans->i = value.imag();
}
#else
R_INLINE static void SET_C99_COMPLEX(Rcomplex *x, R_xlen_t i, double complex value)
{
    Rcomplex *ans = x + i;
    ans->r = creal(value);
    ans->i = cimag(value);
}
#endif

#ifdef __cplusplus
extern "C" {
#endif

RHIDDEN void z_prec_r(Rcomplex *r, const Rcomplex *x, double digits);

#ifdef __cplusplus
}
#endif

#endif /* R_RCOMPLEX_H */
