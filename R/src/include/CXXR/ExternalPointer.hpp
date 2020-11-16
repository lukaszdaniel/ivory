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

/** @file ExternalPointer.hpp
 * @brief Class CXXR::ExternalPointer and associated C interface.
 */

#ifndef EXTERNALPOINTER_HPP
#define EXTERNALPOINTER_HPP

#include <CXXR/RObject.hpp>
#include <CXXR/GCEdge.hpp>
#include <CXXR/SEXP_downcast.hpp>

namespace CXXR
{
    /** @brief External pointer.
     *
     * RObject encapsulating a pointer to some entity that is not an
     * RObject.  An ExternalPointer also comprises two pointers to
     * objects of a type derived from RObject, here designated the
     * 'tag' and the 'protege', each of which is protected from
     * garbage collection for the lifetime of the ExternalPointer
     * object.  The tag and the protege are treated identically by the
     * ExternalPointer class, but the 'Writing R Extensions' document
     * (in recent revisions) suggests that the tag be used for some
     * sort of type identification, and that the protege be used for
     * protecting memory (or other resources) used by the entity
     * pointed to by the ExternalPointer.
     *
     * @note Writers of C++ packages are recommended to derive their
     * own classes from RObject rather than use this class, which is
     * provided primarily to support the established C interface.
     */
    class ExternalPointer : public RObject
    {
    public:
        // Virtual function of RObject:
        const char *typeName() const override;

        /** @brief The name by which this type is known in R.
         *
         * @return the name by which this type is known in R.
         */
        static const char *staticTypeName()
        {
            return "externalptr";
        }
        /* External pointer access methods */
        static RObject *extptr_prot(RObject *x);
        static RObject *extptr_tag(RObject *x);
        static void set_extptr_tag(RObject *x, RObject *v);
        static void set_extptr_prot(RObject *x, RObject *v);
        static RObject *extptr_ptr(RObject *x);
        static void set_extptr_ptr(RObject *x, RObject *v);

    protected:
    private:
    };
} // namespace CXXR

#endif // EXTERNALPOINTER_HPP
