/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2006   The R Development Core Team.
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
 *
 *  Rho is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the Rho website.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

/** @file Expression.hpp
 * @brief Class CXXR::Expression and associated C interface.
 */

#ifndef EXPRESSION_HPP
#define EXPRESSION_HPP

#include <CXXR/PairList.hpp>

namespace CXXR
{
   /** @brief Pointer to expression currently being evaluated.
    */
   // extern CXXR::GCRoot<> R_CurrentExpr;

   /** @brief Expression currently being evaluated.
    *
    * @return Pointer to the Expression currently being evaluated.
    */
   RObject *Rf_currentExpression();

   /** @brief Designate the Expression currently being evaluated.
    *
    * @param e Pointer to the Expression now to be evaluated.  (Not
    *          currently checked in any way.)
    */
   void Rf_setCurrentExpression(RObject *e);
} // namespace CXXR

#endif /* EXPRESSION_HPP */
