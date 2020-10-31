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
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

/** @file ConsCell.cpp
 *
 * @brief Class ConsCell and associated C interface.
 */

#include <CXXR/ConsCell.hpp>
#include <Defn.h>
#include <Rinternals.h>

namespace R
{
    // Force the creation of non-inline embodiments of functions callable
    // from C:
    namespace ForceNonInline
    {
        const auto &CAD4Rptr = CAD4R;
        const auto &CADDDRptr = CADDDR;
        const auto &CADDRptr = CADDR;
        const auto &CADRptr = CADR;
        const auto &CDARptr = CDAR;
        const auto &CDDRptr = CDDR;
        const auto &CDDDRptr = CDDDR;
        const auto &CDRptr = CDR;
        const auto &BINDING_IS_LOCKEDptr = BINDING_IS_LOCKED;
        const auto &IS_ACTIVE_BINDINGptr = IS_ACTIVE_BINDING;
        const auto &LOCK_BINDINGptr = LOCK_BINDING;
        const auto &SET_ACTIVE_BINDING_BITptr = SET_ACTIVE_BINDING_BIT;
        const auto &UNLOCK_BINDINGptr = UNLOCK_BINDING;
    } // namespace ForceNonInline

    /* List Access Methods */
    RObject *RObject::tag(RObject *e) { return e ? e->u.listsxp.m_tagval : nullptr; }

    void RObject::set_tag(RObject *x, RObject *v)
    {
        if (!x)
            return;
        x->u.listsxp.m_tagval = v;
    }

    RObject *RObject::car0(RObject *e) { return e ? e->u.listsxp.m_carval : nullptr; }

    void RObject::set_car0(RObject *x, RObject *v)
    {
        if (!x)
            return;
        x->u.listsxp.m_carval = v;
    }

    RObject *RObject::cdr(RObject *e) { return e ? e->u.listsxp.m_cdrval : nullptr; }

    void RObject::set_cdr(RObject *x, RObject *v)
    {
        if (!x)
            return;
        x->u.listsxp.m_cdrval = v;
    }

} // namespace R
