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

#ifndef LIST_HPP
#define LIST_HPP

#include <RObject.hpp>

/* List Access Macros */
/* These also work for ... objects */
#define TAG(e) (R::RObject::tag(e))
#define CAR0(e) (R::RObject::car0(e))
#define EXTPTR_PTR(e) (R::RObject::extptr_ptr(e))
#define CDR(e) (R::RObject::cdr(e))
#define CAAR(e) CAR(CAR(e))
#define CDAR(e) CDR(CAR(e))
#define CADR(e) CAR(CDR(e))
#define CDDR(e) CDR(CDR(e))
#define CDDDR(e) CDR(CDDR(e))
#define CD4R(x) CDR(CDR(CDR(CDR(x))))
#define CADDR(e) CAR(CDDR(e))
#define CADDDR(e) CAR(CDR(CDDR(e)))
#define CAD3R(e) CAR(CDR(CDDR(e)))
#define CAD4R(e) CAR(CDDR(CDDR(e)))
#define CAD5R(e) CAR(CDR(CDR(CDR(CDR(CDR(e))))))

#endif