/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
 *
 *  Rho is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the Rho website.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

/** @file Evaluator.cpp
 *
 * Implementation of class Evaluator.
 */

#include <CXXR/Evaluator.hpp>
#include <CXXR/Environment.hpp>
#include <Localization.h>

using namespace std;
using namespace CXXR;

namespace CXXR
{
    // Force the creation of non-inline embodiments of functions callable
    // from C:
    namespace ForceNonInline
    {
        const auto &evalp = Rf_eval;
    } // namespace ForceNonInline

    bool Evaluator::s_visible = false; // R_Visible

    unsigned int Evaluator::s_depth = 0;              // R_EvalDepth, Evaluation recursion depth
    unsigned int Evaluator::s_depth_threshold = 5000; // R_Expressions, options(expressions)
    unsigned int Evaluator::s_depth_limit = 5000;     // R_Expressions_keep, options(expressions)
    unsigned int Evaluator::s_countdown = 1000;
    unsigned int Evaluator::s_countdown_start = 1000; // was 100 before 2.8.0

    namespace
    {
        constexpr int R_MIN_EXPRESSIONS_OPT = 25;
        constexpr int R_MAX_EXPRESSIONS_OPT = 500000;
    }

    // Implementation of Evaluator::evaluate() is in eval.cpp (for the time being)
    // Implementation of Evaluator::checkForUserInterrupts() is in eval.cpp (for the time being)

    void Evaluator::maybeCheckForUserInterrupts()
    {
        if (--s_countdown == 0)
        {
            checkForUserInterrupts();
        }
    }

    void Evaluator::setDepthLimit(int depth)
    {
        if (depth < R_MIN_EXPRESSIONS_OPT || depth > R_MAX_EXPRESSIONS_OPT)
            Rf_error(_("'expressions' parameter invalid, allowed %d...%d"),
                     R_MIN_EXPRESSIONS_OPT, R_MAX_EXPRESSIONS_OPT);
        s_depth_threshold = s_depth_limit = depth;
    }

    void Evaluator::enableResultPrinting(bool on)
    {
        s_visible = on;
    }

    bool Evaluator::resultPrinted()
    {
        return s_visible;
    }
} // namespace CXXR

namespace R
{
} // namespace R

// ***** C interface *****

extern "C"
{
    Rboolean R_interrupts_suspended = FALSE;
    Rboolean R_interrupts_pending = FALSE;
}
