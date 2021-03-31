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

/** @file ExternalPointer.cpp
 *
 * @brief Class ExternalPointer and associated C interface.
 */

#include <CXXR/ExternalPointer.hpp>
#include <CXXR/GCStackRoot.hpp>
#include <Rinternals.h>
#include <Localization.h>

using namespace std;
using namespace CXXR;

namespace CXXR
{
    // Force the creation of non-inline embodiments of functions callable
    // from C:
    namespace ForceNonInline
    {
        const auto &R_ExternalPtrAddrptr = R_ExternalPtrAddr;
        const auto &R_ExternalPtrTagptr = R_ExternalPtrTag;
        const auto &R_ExternalPtrProtectedptr = R_ExternalPtrProtected;
        const auto &R_ClearExternalPtrptr = R_ClearExternalPtr;
    } // namespace ForceNonInline

    const char *ExternalPointer::typeName() const
    {
        return ExternalPointer::staticTypeName();
    }

    void ExternalPointer::visitChildren(const_visitor *v) const
    {
        RObject::visitChildren(v);
        if (m_protege)
            m_protege->conductVisitor(v);
        if (m_tag)
            m_tag->conductVisitor(v);
    }
} // namespace CXXR

// ***** C interface *****

void *EXTPTR_PTR(SEXP x)
{
    if (!x)
        return nullptr;
    ExternalPointer *ep = SEXP_downcast<ExternalPointer *>(x, false);
    return ep->ptr();
}

/* External Pointer Objects */
SEXP R_MakeExternalPtr(void *p, SEXP tag, SEXP prot)
{
    GCStackRoot<> tagr(tag);
    if (tag)
        tag->incrementRefCount();
    GCStackRoot<> protr(prot);
    if (prot)
        prot->incrementRefCount();

    return GCNode::expose(new ExternalPointer(p, tag, prot));
}

void *R_ExternalPtrAddr(SEXP s)
{
    if (!s)
        return nullptr;
    ExternalPointer *ep = SEXP_downcast<ExternalPointer *>(s, false);
    return ep->ptr();
}

SEXP R_ExternalPtrTag(SEXP s)
{
    if (!s)
        return nullptr;
    ExternalPointer *ep = SEXP_downcast<ExternalPointer *>(s, false);
    return ep->tag();
}

SEXP R_ExternalPtrProtected(SEXP s)
{
    if (!s)
        return nullptr;
    ExternalPointer *ep = SEXP_downcast<ExternalPointer *>(s, false);
    return ep->protege();
}

void R_ClearExternalPtr(SEXP s)
{
    R_SetExternalPtrAddr(s, nullptr);
}

void R_SetExternalPtrAddr(SEXP s, void *p)
{
    if (!s)
        return;
    ExternalPointer *ep = SEXP_downcast<ExternalPointer *>(s, false);
    ep->setPtr(p);
}

void R_SetExternalPtrTag(SEXP s, SEXP tag)
{
    if (!s)
        return;
    ExternalPointer *ep = SEXP_downcast<ExternalPointer *>(s, false);
    ep->setTag(tag);
}

void R_SetExternalPtrProtected(SEXP s, SEXP p)
{
    if (!s)
        return;
    ExternalPointer *ep = SEXP_downcast<ExternalPointer *>(s, false);
    ep->setProtege(p);
}
