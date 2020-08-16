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

#ifndef BUILTINFUNCTION_HPP
#define BUILTINFUNCTION_HPP

#include <RObject.hpp>

/* Primitive Access Macros */
#define PRIMOFFSET(x) (R::RObject::primoffset(x))
#define SET_PRIMOFFSET(x, v) (R::RObject::set_primoffset(x, v))
#define PRIMFUN(x) (R_FunTab[PRIMOFFSET(x)].cfun)
#define PRIMNAME(x) (R_FunTab[PRIMOFFSET(x)].name)
#define PRIMVAL(x) (R_FunTab[PRIMOFFSET(x)].code)
#define PRIMARITY(x) (R_FunTab[PRIMOFFSET(x)].arity)
#define PPINFO(x) (R_FunTab[PRIMOFFSET(x)].gram)
#define PRIMPRINT(x) (((R_FunTab[PRIMOFFSET(x)].eval) / 100) % 10)
#define PRIMINTERNAL(x) (((R_FunTab[PRIMOFFSET(x)].eval) % 100) / 10)

#endif