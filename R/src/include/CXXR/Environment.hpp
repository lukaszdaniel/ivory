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

/** @file Environment.hpp
 * @brief Class rho::Environment and associated C interface.
 */

#ifndef ENVIRONMENT_HPP
#define ENVIRONMENT_HPP

#include <CXXR/RObject.hpp>

namespace R
{
    class Environment : public RObject
    {
    private:
        RObject *m_frame;
        RObject *m_enclos;
        RObject *m_hashtab;

    public:
        auto frame() const { return this->m_frame; }
        auto enclos() const { return this->m_enclos; }
        auto hashtab() const { return this->m_hashtab; }
    };
} // namespace R

#endif /* ENVIRONMENT_HPP */
