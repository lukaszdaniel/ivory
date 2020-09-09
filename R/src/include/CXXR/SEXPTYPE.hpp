/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999--2020  The R Core Team.
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *
 *  This header file is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This file is part of R. R is distributed under the terms of the
 *  GNU General Public License, either Version 2, June 1991 or Version 3,
 *  June 2007. See doc/COPYRIGHTS for details of the copyright status of R.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

#ifndef SEXPTYPE_HPP
#define SEXPTYPE_HPP

    /** @enum SEXPTYPE
     *
     * @brief CR's object type identification.
     *
     * This enumeration is used within CR to identify different types
     * of R object.
     * 
     * Note: when not compiling ivory, SEXPTYPE is a typedef for unsigned int.
     * This is done to support C++ packages that expect implicit int to
     * SEXPTYPE conversions.
     */
#ifndef COMPILING_IVORY
typedef unsigned int SEXPTYPE;
#else
typedef
#endif
enum
{
    NILSXP = 0,      /* nil = NULL */
    SYMSXP = 1,      /* symbols */
    LISTSXP = 2,     /* lists of dotted pairs */
    CLOSXP = 3,      /* closures */
    ENVSXP = 4,      /* environments */
    PROMSXP = 5,     /* promises: [un]evaluated closure arguments */
    LANGSXP = 6,     /* language constructs (special lists) */
    SPECIALSXP = 7,  /* special forms */
    BUILTINSXP = 8,  /* builtin non-special forms */
    CHARSXP = 9,     /* "scalar" string type (internal only)*/
    LGLSXP = 10,     /* logical vectors */
    INTSXP = 13,     /* integer vectors */
    REALSXP = 14,    /* real variables */
    CPLXSXP = 15,    /* complex variables */
    STRSXP = 16,     /* string vectors */
    DOTSXP = 17,     /* dot-dot-dot object */
    ANYSXP = 18,     /* make "any" args work */
    VECSXP = 19,     /* generic vectors */
    EXPRSXP = 20,    /* expressions vectors */
    BCODESXP = 21,   /* byte code */
    EXTPTRSXP = 22,  /* external pointer */
    WEAKREFSXP = 23, /* weak reference */
    RAWSXP = 24,     /* raw bytes */
    S4SXP = 25,      /* S4 non-vector */

    NEWSXP = 30,    /* fresh node created in new page */
    FREESXP = 31,   /* node released by GC */
    SINGLESXP = 47, /* For interfaces to objects created with as.single */
    intCHARSXP = 73,
    FUNSXP = 99, /* Closure or Builtin */
    ALTREP_SXP = 238,
    ATTRLISTSXP = 239,
    ATTRLANGSXP = 240,
    /* the following (241 - 246) are speculative--we may or may not need them soon */
    BASEENV_SXP = 241,
    EMPTYENV_SXP = 242,
    BCREPREF = 243,
    BCREPDEF = 244,
    GENERICREFSXP = 245,
    CLASSREFSXP = 246,
    PERSISTSXP = 247,
    PACKAGESXP = 248,
    NAMESPACESXP = 249,
    BASENAMESPACE_SXP = 250,
    MISSINGARG_SXP = 251,
    UNBOUNDVALUE_SXP = 252,
    GLOBALENV_SXP = 253,
    NILVALUE_SXP = 254,
    REFSXP = 255
}
#ifdef COMPILING_IVORY
SEXPTYPE
#endif
    ;

#endif /* SEXPTYPE_HPP */