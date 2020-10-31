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

/** @file Closure.hpp
 * @brief Class R::Closure and associated C interface.
 */

#ifndef CLOSURE_HPP
#define CLOSURE_HPP

#include <CXXR/FunctionBase.hpp>
#include <CXXR/SEXP_downcast.hpp>

namespace R
{
    /** @brief Class representing a functional programming closure.
     *
     * A closure associates a function definition (the body) with a
     * list of formal arguments and an environment.  In evaluating the
     * function, non-local variables within the function definition
     * are interpreted by reference to the specified environment (and
     * its enclosing environments).
     */
    class Closure : public FunctionBase
    {
    private:
        RObject *m_formals;
        RObject *m_body;
        RObject *m_env;
        // Declared private to ensure that Closure objects are
        // created only using 'new':
        ~Closure();

    public:
        // Virtual functions of RObject:
        const char *typeName() const override;

        /** @brief The name by which this type is known in R.
         *
         * @return The name by which this type is known in R.
         */
        static const char *staticTypeName()
        {
            return "closure";
        }
        auto formals() const { return this->m_formals; }
        auto body() const { return this->m_body; }
        auto env() const { return this->m_env; }

        /* Closure Access Methods */
        static RObject *formals(RObject *x);
        static void set_formals(RObject *x, RObject *v);
        static RObject *body(RObject *x);
        static void set_body(RObject *x, RObject *v);
        static RObject *cloenv(RObject *x);
        static void set_cloenv(RObject *x, RObject *v);
        static bool rdebug(RObject *x);
        static void set_rdebug(RObject *x, bool v);
        static bool rstep(RObject *x);
        static void set_rstep(RObject *x, bool v);
    };
} // namespace R

#endif /* CLOSURE_HPP */
