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
 * @brief Class R::Environment and associated C interface.
 */

#ifndef ENVIRONMENT_HPP
#define ENVIRONMENT_HPP

#include <CXXR/RObject.hpp>
#include <CXXR/SEXP_downcast.hpp>

namespace R
{
    /** @brief Mapping from Symbols to R objects.
     *
     * An Environment has an associated Frame, which defines a mapping
     * from (pointers to) R::Symbol objects to (pointers to)
     * arbitrary objects of classes derived from RObject.  An
     * Environment will normally have an 'enclosing environment', and
     * the Environment class provides facilities for searching for a
     * binding for a Symbol first in the Environment's own Frame, and
     * then successively in the Frames of enclosing Environments.
     *
     * @note This class does not in itself enforce the requirement
     * that the enclosing relationship must be acyclic.
     */
    class Environment : public RObject
    {
    private:
        RObject *m_frame;
        RObject *m_enclos;
        RObject *m_hashtab;
        // Declared private to ensure that Environment objects are
        // created only using 'new':
        ~Environment() {}

    public:
        // Virtual functions of RObject:
        const char *typeName() const override;

        /** @brief The name by which this type is known in R.
         *
         * @return The name by which this type is known in R.
         */
        static const char *staticTypeName()
        {
            return "environment";
        }
        auto frame() const { return this->m_frame; }
        auto enclos() const { return this->m_enclos; }
        auto hashtab() const { return this->m_hashtab; }

        /* Environment Access Methods */
        static constexpr int FRAME_LOCK_MASK = (1 << 14);
        static constexpr int GLOBAL_FRAME_MASK = (1 << 15);
        static RObject *frame(RObject *x);
        static RObject *enclos(RObject *x);
        static RObject *hashtab(RObject *x);
        static unsigned int envflags(RObject *x); /* for environments */
        static void set_envflags(RObject *x, unsigned int v);
        static void set_frame(RObject *x, RObject *v);
        static void set_enclos(RObject *x, RObject *v);
        static void set_hashtab(RObject *x, RObject *v);
        static unsigned int frame_is_locked(RObject *x);
        static void lock_frame(RObject *x);
        static bool is_global_frame(RObject *x);
        static void mark_as_global_frame(RObject *x);
        static void mark_as_local_frame(RObject *x);
    };
} // namespace R

#endif /* ENVIRONMENT_HPP */
