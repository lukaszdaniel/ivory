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
 * @brief Class CXXR::FunctionBase and associated C interface functions.
 */

#ifndef FUNCTIONBASE_HPP
#define FUNCTIONBASE_HPP

#include <CXXR/RObject.hpp>
#include <CXXR/SEXP_downcast.hpp>

namespace CXXR
{

    /** @brief Base class for function types.
     */
    class FunctionBase : public RObject
    {
    public:
        /** @brief Is debugging enabled?
         *
         * @return true iff debugging is currently enabled for this
         * Funtion.
         */
        bool debugging() const
        {
            return m_debug;
        }

        /** @brief Set debugging status.
         *
         * @param on The required new debugging status (true =
         *           enabled).
         */
        void setDebugging(bool on)
        {
            m_debug = on;
        }

        /** @brief The name by which this type is known in R.
         *
         * @return the name by which this type is known in R.
         */
        static const char *staticTypeName()
        {
            return "(function type)";
        }

        static bool rdebug(RObject *x);
        static void set_rdebug(RObject *x, bool v);
        static bool rtrace(RObject *x);
        static void set_rtrace(RObject *x, bool v);

    protected:
        /**
         * @param stype Required type of the FunctionBase.
         */
        explicit FunctionBase(SEXPTYPE stype)
            : RObject(stype), m_debug(false)
        {
        }

        /** @brief Copy constructor.
         *
         * @param pattern FunctionBase to be copied.
         *
         * @param deep Indicator whether to perform deep or shallow copy.
         */
        FunctionBase(const FunctionBase &pattern, bool deep)
            : RObject(pattern, deep), m_debug(false)
        {
        }

        virtual ~FunctionBase(){};

    private:
        bool m_debug;
    };

} // namespace CXXR

extern "C"
{
    /** @brief Get function tracing status.
     *
     * @param x Pointer to a CXXR::FunctionBase (checked), or a null
     *          pointer.
     *
     * @return Refer to 'R Internals' document.  Returns 0 if \a x is a
     *         null pointer.
     */
    int RTRACE(SEXP x);

    /** @brief Set function tracing status.
     *
     * @param x Pointer to a CXXR::FunctionBase (checked), or a null
     *          pointer.
     *
     * @param v The desired tracing status: non-zero if tracing is
     *          required.
     */
    void SET_RTRACE(SEXP x, int v);

    /** @brief Query debugging status.
     *
     * @param x Pointer to a CXXR::FunctionBase object.
     *
     * @return \c true if debugging is set, i.e. evaluations of the
     *         function should run under the browser.
     *
     * @note In CXXR, RDEBUG() is applicable only to FunctionBase; use
     * ENV_RDEBUG() to query the debugging (single-stepping) state
     * for environments.
     */
    int RDEBUG(SEXP x);

    /**
     * Set the debugging state of a CXXR::FunctionBase object.
     *
     * @param x Pointer a CXXR::FunctionBase object (checked).
     *
     * @param v The new debugging state.
     *
     * @note In CXXR, SET_RDEBUG() is applicable only to FunctionBase; use
     * SET_ENV_RDEBUG() to set the debugging (single-stepping) state
     * for environments.
     */
    void SET_RDEBUG(SEXP x, int v);
    Rboolean Rf_isPrimitive(SEXP s);
    Rboolean Rf_isFunction(SEXP s);
} // extern "C"

#if defined(R_NO_REMAP) && defined(COMPILING_IVORY) && defined(__cplusplus)
const auto isFunction = Rf_isFunction;
const auto isPrimitive = Rf_isPrimitive;
#endif

#endif // FUNCTIONBASE_HPP
