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
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

/** @file S4Object.cpp
 *
 * @brief Class S4Object and associated C interface.
 */

#include <CXXR/S4Object.hpp>
#include <Localization.h>

using namespace CXXR;

namespace CXXR
{
    // Force the creation of non-inline embodiments of functions callable
    // from C:
    namespace ForceNonInline
    {
        const auto &IS_S4_OBJECTptr = IS_S4_OBJECT;
        const auto &SET_S4_OBJECTptr = SET_S4_OBJECT;
        const auto &UNSET_S4_OBJECTptr = UNSET_S4_OBJECT;

    } // namespace ForceNonInline

    S4Object *S4Object::clone(bool deep) const
    {
        // return GCNode::expose(new S4Object(*this, deep));
        return new S4Object(*this, deep);
    }

    const char *S4Object::typeName() const
    {
        return S4Object::staticTypeName();
    }

    void S4Object::visitChildren(const_visitor *v) const
    {
        RObject::visitChildren(v);
        if (tag())
            tag()->conductVisitor(v);
    }
} // namespace CXXR

// ***** C interface *****

/* S4 object testing */
int IS_S4_OBJECT(SEXP x)
{
    return x ? x->isS4Object() : 0;
}

void SET_S4_OBJECT(SEXP x)
{
    if (!x)
        return;
    x->setS4Object(true);
}

void UNSET_S4_OBJECT(SEXP x)
{
    if (!x)
        return;
    x->setS4Object(false);
}

/* S4Object Accessors */
SEXP S4TAG(SEXP e)
{
    return e ? SEXP_downcast<S4Object *>(e)->tag() : nullptr;
}

void SET_S4TAG(SEXP x, SEXP v)
{
    if (!x)
        Rf_error(_("incorrect value"));
    S4Object *s4 = SEXP_downcast<S4Object *>(x);
    s4->setTag(v);
}

SEXP Rf_allocS4Object()
{
    return GCNode::expose(new CXXR::S4Object());
}
