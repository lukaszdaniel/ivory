/*
 *  R : A Computer Language for Statistical Data Analysis
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
 *  https://www.R-project.org/Licenses/
 */

#include <CXXR/Logical.hpp>

#include <CXXR/GCRoot.hpp>
#include <CXXR/LogicalVector.hpp>
#include <Defn.h>

namespace CXXR
{
    SEXP R_TrueValue = nullptr;
    SEXP R_FalseValue = nullptr;
    SEXP R_LogicalNAValue = nullptr;

    void Logical::initialize()
    {
        static GCRoot<LogicalVector> trueValue(GCNode::expose(new LogicalVector(1)));
        (*trueValue)[0] = true;
        MARK_NOT_MUTABLE(trueValue.get());

        static GCRoot<LogicalVector> falseValue(GCNode::expose(new LogicalVector(1)));
        (*falseValue)[0] = false;
        MARK_NOT_MUTABLE(falseValue.get());

        static GCRoot<LogicalVector> naValue(GCNode::expose(new LogicalVector(1)));
        (*naValue)[0] = NA();
        MARK_NOT_MUTABLE(naValue.get());

        R_TrueValue = trueValue.get();
        R_FalseValue = falseValue.get();
        R_LogicalNAValue = naValue.get();
    }
} // namespace CXXR
