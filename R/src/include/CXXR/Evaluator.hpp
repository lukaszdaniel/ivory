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
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

/** @file Evaluator.hpp
 *
 * @brief Class CXXR::Evaluator
 */

#ifndef EVALUATOR_HPP
#define EVALUATOR_HPP

#include <R_ext/Boolean.h>
#include <CXXR/Environment.hpp>
#include <CXXR/PairList.hpp>
#include <utility>

namespace CXXR
{
    class RObject;
    class Environment;

    /** @brief Housekeeping services for R expression evaluation.
     * 
     * All members of this class are static.
     */
    class Evaluator
    {
    public:
        /** @brief (Not for general use.)
         *
         * Used in context.cpp to save the evaluation depth
         * associated with an RCNTXT.  Also used in do_Cstack_info()
         * in platform.cpp.
         *
         * @return The current evaluation depth.
         */
        static unsigned int depth()
        {
            return s_depth;
        }

        /** @brief Maximum depth of R expression nesting.
         *
         * @return The current maximum nesting depth (disregarding any
         * extra depth that may have been introduced by extraDepth()
         * ).
         */
        static unsigned int depthLimit()
        {
            return s_depth_limit;
        }

        /** @brief (Not for general use.)
         *
         * This function is for use by the profiling code in eval.cpp
         * to record whether profiling is currently enabled.
         *
         * @param on true iff printing is required.
         */
        static void enableProfiling(bool on)
        {
            s_profiling = on;
        }

        /** @brief Specify whether the result of top-level expression
         * be printed.
         *
         * @param on true iff printing is required.
         */
        static void enableResultPrinting(bool on);

        /** @brief Evaluate RObject in a specified Environment.
         *
         * For most types of RObject, this simply returns a pointer to
         * the RObject itself.
         *
         * @param object Pointer to the RObject to be evaluated.  May
         *          be null, in which case the function returns a null
         *          pointer.
         *
         * @param env Pointer to the environment in which evaluation
         *          is to take place.  May be null only if \a object
         *          is null.
         *
         * @return Pointer to the result of evaluation.
         */
        static RObject *evaluate(RObject *object, Environment *env);

        /** @brief (Not for general use.)
         *
         * @param on If true, an increase is applied to the
         *          permissible depth of nested evaluations to allow
         *          error reporting to be carried out.  If false, any
         *          such extra depth currently in force is removed.
         */
        static void extraDepth(bool on)
        {
            s_depth_threshold = s_depth_limit + (on ? 500 : 0);
        }

        /** @brief Is profiling currently enabled?
         *
         * @return true iff profiling is currently in progress.
         */
        static bool profiling()
        {
            return s_profiling;
        }

        /** @brief Is the result of top-level expression evaluation
         * printed?
         *
         * @return true iff it is currently specified that the result
         * of a top-level R expression evaluation should be printed.
         */
        static bool resultPrinted();

        /** @brief (Not for general use.)
         *
         * Used in context.cpp to restore the evaluation depth
         * associated with an RCNTXT.  Also used in main.cpp to reset
         * the evaluation depth to zero.
         *
         * @param depth The required depth.
         */
        static void setDepth(unsigned int depth)
        {
            s_depth = depth;
        }

        /** @brief Set maximum depth of R expression nesting.
         *
         * @param depth The required maximum nesting depth.  If the
         *          supplied value lies outside the permissible range,
         *          an error is reported and the nesting depth is left
         *          unchanged.
         *
         * @param The maximum nesting depth previously in force.
         */
        static void setDepthLimit(int depth);

    private:
        /** @brief Print expression value?
         *
         * If s_visible is true when the evaluation of a top-level R
         * expression completes, the value of the expression is printed.
         */
        static bool s_visible;
        static unsigned int s_depth;           // Current depth of expression evaluation
        static unsigned int s_depth_threshold; // An error will be
                                               // reported if s_depth exceeds this
                                               // value.  s_depth_threshold is normally
                                               // equal to s_depth_limit, but may be
                                               // temporarily increased above s_depth_limit
                                               // to allow error reporting.
        static unsigned int s_depth_limit;     // The value (controlled
                                               // by the 'expressions' R option) to
                                               // which s_depth_threshold is set except
                                               // during error reporting.
        static unsigned int s_countdown;       // Number of calls of
                                               // Evaluator::evaluate() to go before a
                                               // check is made for user interrupts.
        static unsigned int s_countdown_start; // Value from which
                                               // s_countdown starts counting down
        static Evaluator *s_current;           // The current (innermost) Evaluator
        static bool s_profiling;               // True iff profiling enabled

        Evaluator *m_next; // Next Evaluator down the stack
    };

    /** @brief Shorthand for Evaluator::evaluate().
     *
     * @param object Pointer to the RObject to be evaluated.  May be
     *          null, in which case the function returns a null
     *          pointer. 
     *
     * @param env Pointer to the environment in which evaluation
     *          is to take place.  May be null only if \a object is
     *          null.
     *
     * @return Pointer to the result of evaluation.
     */
    inline RObject *evaluate(RObject *object, Environment *env)
    {
        return Evaluator::evaluate(object, env);
    }
} // namespace CXXR

namespace R
{
    /** @brief Is a Symbol missing within an Environment?
     *
     * @param symbol Pointer to the Symbol whose missing status is
     * required.
     *
     * @param rho Pointer to the Environment in whose Frame \a symbol
     *          is to be sought.
     *
     * @return A non-zero value iff \a symbol is missing in the Frame
     * of \a rho.
     *
     * @note For more information, refer to the code, which is
     * surprisingly complicated.
     */
    bool R_isMissing(SEXP symbol, SEXP rho);
}

extern "C"
{
    /** @brief Are any user interrupts currently pending?
     *
     * If user interrupts are attempted while user interrupts are
     * suspended, this is set non-zero.  The interrupt is then
     * services when the period of suspension ends.
     */
    extern Rboolean R_interrupts_pending;

    /** @brief Are interrupts currently suspended?
     */
    extern Rboolean R_interrupts_suspended;

    /** @brief Evaluate an object in a specified Environment.
     *
     * @param e Pointer (possibly null) to the object to be evaluated.
     *
     * @param rho Pointer to an Environment (checked unless \a e is null).
     *
     * @return Pointer to the result of evaluating \a e in \a rho, or
     * a null pointer if \a e is null.
     */
    SEXP Rf_eval(SEXP e, SEXP rho);
} // extern "C"

#endif /* EVALUATOR_HPP */
