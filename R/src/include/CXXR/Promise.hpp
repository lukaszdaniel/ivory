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

/** @file Promise.hpp
 * @brief Class R::Promise and associated C interface.
 */

#ifndef PROMISE_HPP
#define PROMISE_HPP

#include <CXXR/RObject.hpp>
#include <CXXR/SEXP_downcast.hpp>

namespace R
{
    /** @brief Mechanism for deferred evaluation.
     *
     * This class is used to handle function arguments within R's lazy
     * evaluation scheme.  A Promise object encapsulates a pointer to
     * an arbitrary RObject (typically a Symbol or an Expression), and
     * a pointer to an Environment.  When the Promise is first
     * evaluated, the RObject is evaluated within the Environment, and
     * the result of evaluation returned as the value of the Promise.
     *
     * After the first evaluation, the result of evaluation is cached
     * within the Promise object, and the Environment pointer is set
     * null (thus possibly allowing the Environment to be
     * garbage-collected).  Subsequent evaluations of the Promise
     * object simply return the cached value.
     *
     * @note When a Promise is evaluated \e via a call to evaluate()
     * (the virtual function defined in class RObject), the \a env
     * parameter to evaluate() is ignored: evaluation uses only the
     * Environment encapsulated within the Promise object.  When an
     * RObject is known to be a Promise, it is suggested that
     * evaluation be carried out using the function Promise::force(),
     * which lacks the redundant parameter and is consequently clearer
     * to readers of the code.
     */
    class Promise : public RObject
    {
    private:
        RObject *m_value;
        RObject *m_expr;
        RObject *m_env;

    public:
        const char *typeName() const override;

        /** @brief The name by which this type is known in R.
         *
         * @return The name by which this type is known in R.
         */
        static const char *staticTypeName()
        {
            return "promise";
        }
        auto value() const { return this->m_value; }
        auto expr() const { return this->m_expr; }
        auto env() const { return this->m_env; }

        /* Promise Access Methods */
        static RObject *prcode(RObject *x);
        static void set_prcode(RObject *x, RObject *v);
        static RObject *prenv(RObject *x);
        static RObject *prvalue(RObject *x);
        static void set_prvalue(RObject *x, RObject *v);
        static unsigned int prseen(RObject *x);
        static void set_prenv(RObject *x, RObject *v);
        static void set_prseen(RObject *x, unsigned int v);
    };
} // namespace R

#endif /* PROMISE_HPP */
