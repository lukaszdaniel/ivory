/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999--2020  The R Core Team.
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
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

/**
 * @brief CR's object type identification.
 *
 * @enum SEXPTYPE This enumeration is used within CR to identify different types
 * of R object.
 *
 * @note when not compiling ivory, SEXPTYPE is a typedef for unsigned int.
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
    NILSXP = 0,      /**< NULL. In rho no CXXR::RObject has
                      * this type, but for backward
                      * compatibility TYPEOF will return ::NILSXP
                      * if passed a null pointer.
                      */
    SYMSXP = 1,      /**< symbols, implemented in class CXXR::Symbol. */
    LISTSXP = 2,     /**< lists of dotted pairs, implemented in class CXXR::PairList. */
    CLOSXP = 3,      /**< closures, implemented in class CXXR::Closure. */
    ENVSXP = 4,      /**< environments, implemented in class CXXR::Environment. */
    PROMSXP = 5,     /**< promises: [un]evaluated closure arguments, implemented in class CXXR::Promise. */
    LANGSXP = 6,     /**< language constructs (special lists), implemented in class CXXR::Expression. */
    SPECIALSXP = 7,  /**< special forms, implemented in class CXXR::BuiltInFunction. */
    BUILTINSXP = 8,  /**< builtin non-special forms, also implemented in class CXXR::BuiltInFunction. */
    CHARSXP = 9,     /**< "scalar" string type (internal only), implemented in class CXXR::String. */
    LGLSXP = 10,     /**< logical vectors, implemented in class CXXR::LogicalVector. */
    INTSXP = 13,     /**< integer vectors, implemented in class CXXR::IntVector. */
    REALSXP = 14,    /**< real variables, implemented in class CXXR::RealVector. */
    CPLXSXP = 15,    /**< complex variables, implemented in class CXXR::ComplexVector. */
    STRSXP = 16,     /**< string vectors, implemented in class CXXR::StringVector. */
    DOTSXP = 17,     /**< dot-dot-dot objects, implemented in class CXXR::DottedArgs. */
    ANYSXP = 18,     /**< Used to make "any" args work.  No CXXR::RObject has this type. */
    VECSXP = 19,     /**< generic vectors, implemented in class CXXR::ListVector. */
    EXPRSXP = 20,    /**< expression vectors, implemented in class CXXR::ExpressionVector. */
    BCODESXP = 21,   /**< byte code.  Unused in rho. */
    EXTPTRSXP = 22,  /**< external pointers, implemented in class CXXR::ExternalPointer. */
    WEAKREFSXP = 23, /**< weak references, implemented in class CXXR::WeakRef. */
    RAWSXP = 24,     /**< raw bytes, implemented in class CXXR::RawVector. */
    S4SXP = 25,      /**< S4 object not inheriting from another ::SEXPTYPE, implemented in class CXXR::S4Object. */

    NEWSXP = 30,  /* fresh node created in new page */
    FREESXP = 31, /* node released by GC */
    CXXSXP = 43,  /**< object types specific to rho.*/
                  /* (43 = ASCII +) */
    BAILSXP = 44, /**< Object used to implement indirect flow of control in R without using a C++ exception. */

    SINGLESXP = 47, /* For interfaces to objects created with as.single */
    FUNSXP = 99,    /**< Closure or Builtin.  No CXXR::RObject has this type. */
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

#ifdef __cplusplus
/* These are also used with the write barrier on, in attrib.cpp and util.cpp */
constexpr int BASIC_TYPE_BITS = 5;
constexpr int FULL_TYPE_BITS = 8;
constexpr int MAX_NUM_BASIC_SEXPTYPE = (1 << BASIC_TYPE_BITS);
constexpr int MAX_NUM_SEXPTYPE = (1 << FULL_TYPE_BITS);
#endif

#endif /* SEXPTYPE_HPP */
