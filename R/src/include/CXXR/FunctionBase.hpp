/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2007   The R Development Core Team.
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

/** @file FunctionBase.hpp
 *
 * @brief Class R::FunctionBase and associated C interface functions.
 */

#ifndef FUNCTIONBASE_HPP
#define FUNCTIONBASE_HPP

#include <CXXR/RObject.hpp>
#include <CXXR/SEXP_downcast.hpp>

namespace R
{

    /** @brief Base class for function types.
     */
    class FunctionBase : public RObject
    {
    public:
        /** @brief The name by which this type is known in R.
         *
         * @return the name by which this type is known in R.
         */
        static const char *staticTypeName()
        {
            return "(function type)";
        }

        static bool rtrace(RObject *x);
        static void set_rtrace(RObject *x, bool v);

    protected:
        /**
         * @param stype Required type of the FunctionBase.
         */
        explicit FunctionBase(SEXPTYPE stype)
            : RObject(stype)
        {
        }

        virtual ~FunctionBase(){};
    };

} // namespace R

#endif // FUNCTIONBASE_HPP
