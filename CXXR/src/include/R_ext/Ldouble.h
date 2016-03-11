/*
 * Ldouble.h
 *
 *  Created on: 5 wrz 2014
 *      Author: Lukasz Daniel
 */

#ifndef LDOUBLE_H_
#define LDOUBLE_H_


#ifdef HAVE_LONG_DOUBLE
#  define LDOUBLE long double
#else
#  define LDOUBLE double
#endif


#endif /* LDOUBLE_H_ */
