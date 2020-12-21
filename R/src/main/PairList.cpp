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

/** @file PairList.cpp
 *
 * @brief Class PairList and associated C interface.
 */

#include <CXXR/PairList.hpp>
#include <CXXR/StringVector.hpp>
#include <CXXR/Symbol.hpp>
#include <Localization.h>
#include <iostream>

using namespace std;
using namespace CXXR;

namespace CXXR
{
    // Force the creation of non-inline embodiments of functions callable
    // from C:
    namespace ForceNonInline
    {
        // const auto &BINDING_IS_LOCKEDptr = BINDING_IS_LOCKED;
        // const auto &CAD4Rp = CAD4R;
        // const auto &CADDDRp = CADDDR;
        // const auto &CADDRp = CADDR;
        // const auto &CADRp = CADR;
        // const auto &CDARp = CDAR;
        // const auto &CDDRp = CDDR;
        // const auto &CDDDRp = CDDDR;
        // const auto &CDRp = CDR;
        // const auto &allocListp = Rf_allocList;
        // const auto &allocSExpp = Rf_allocSExp;
        // const auto &consp = Rf_cons;
        // const auto &lconsp = Rf_lcons;
        // const auto &IS_ACTIVE_BINDINGptr = IS_ACTIVE_BINDING;
        // const auto &LOCK_BINDINGptr = LOCK_BINDING;
        // const auto &SET_ACTIVE_BINDING_BITptr = SET_ACTIVE_BINDING_BIT;
        // const auto &UNLOCK_BINDINGptr = UNLOCK_BINDING;
    } // namespace ForceNonInline

    /* List Access Methods */
    RObject *PairList::tag(RObject *e)
    {
        if (!e)
            return nullptr;
        return e->u.listsxp.m_tagval;
    }

    void PairList::set_tag(RObject *x, RObject *v)
    {
        if (!x)
            Rf_error(_("incorrect value"));
        x->u.listsxp.m_tagval = v;
    }

    RObject *PairList::car0(RObject *e)
    {
        if (!e)
            return nullptr;
        return e->u.listsxp.m_carval;
    }

    void PairList::set_car0(RObject *x, RObject *v)
    {
        if (!x)
            Rf_error(_("incorrect value"));
        x->u.listsxp.m_carval = v;
    }

    RObject *PairList::cdr(RObject *e)
    {
        if (!e)
            return nullptr;
        return e->u.listsxp.m_cdrval;
    }

    void PairList::set_cdr(RObject *x, RObject *v)
    {
        if (!x)
            Rf_error(_("incorrect value"));
        x->u.listsxp.m_cdrval = v;
    }

    double PairList::bndcell_dval(RObject *x)
    {
        if (!x)
            return 0.0;
        return ((R_bndval_t *)&(x->u.listsxp.m_carval))->dval;
    }

    int PairList::bndcell_ival(RObject *x)
    {
        if (!x)
            return 0;
        return ((R_bndval_t *)&(x->u.listsxp.m_carval))->ival;
    }

    int PairList::bndcell_lval(RObject *x)
    {
        if (!x)
            return 0;
        return ((R_bndval_t *)&(x->u.listsxp.m_carval))->ival;
    }

    void PairList::set_bndcell_dval(RObject *x, double v)
    {
        if (!x)
            return;
        ((R_bndval_t *)&(x->u.listsxp.m_carval))->dval = v;
    }

    void PairList::set_bndcell_ival(RObject *x, int v)
    {
        if (!x)
            return;
        ((R_bndval_t *)&(x->u.listsxp.m_carval))->ival = v;
    }

    void PairList::set_bndcell_lval(RObject *x, int v)
    {
        if (!x)
            return;
        ((R_bndval_t *)&(x->u.listsxp.m_carval))->ival = v;
    }
} // namespace CXXR

PairList::PairList(SEXPTYPE st, size_t sz)
    : RObject(st)
{
    checkST(st);
#ifdef LONG_VECTOR_SUPPORT
    if (sz > R_SHORT_LEN_MAX)
        Rf_error(_("invalid length for pairlist"));
#endif
    if (sz == 0)
        throw out_of_range(_("Cannot construct PairList of zero length."));
    try
    {
        while (--sz)
            m_tail = new PairList(st, nullptr, m_tail, nullptr);
    }
    catch (...)
    {
        if (m_tail)
            m_tail->expose();
        throw;
    }
}

void PairList::checkST(SEXPTYPE st)
{
    switch (st)
    {
    case LISTSXP:
    case LANGSXP:
    case DOTSXP:
    case BCODESXP:
        break;
    default:
        throw invalid_argument("Inappropriate SEXPTYPE for PairList.");
    }
}

const char *PairList::typeName() const
{
    switch (sexptype())
    {
    case LISTSXP:
        return "pairlist";
    case LANGSXP:
        return "language";
    case DOTSXP:
        return "...";
    case BCODESXP:
        return "bytecode";
    default:
        throw logic_error(_("PairList has illegal SEXPTYPE."));
    }
}

void PairList::visitChildren(const_visitor *v) const
{
    const PairList *p = this;
    do
    {
        p->RObject::visitChildren(v);
        if ((p->sexptype() != LISTSXP || BOXED_BINDING_CELLS || RObject::bndcell_tag(p) == 0) && p->m_car)
            p->m_car->conductVisitor(v);
        if (p->m_tag)
            p->m_tag->conductVisitor(v);
        p = p->m_tail;
    } while (p && (*v)(p));
}

namespace
{
    void indent(ostream &os, size_t margin)
    {
        while (margin--)
            os << " ";
    }

    const char *sympname(const RObject *sym)
    {
        const RObject *pname = sym->u.symsxp.m_pname;
        if (!pname)
            return "(Symbol has no PRINTNAME)";
        const String *pstr = dynamic_cast<const String *>(pname);
        if (!pstr)
            return "(PRINTNAME not a String)";
        return pstr->c_str();
    }
} // namespace

void CXXR::pldump(ostream &os, const PairList &pl, size_t margin)
{
    indent(os, margin);
    os << Rf_type2char(pl.sexptype()) << "\n";
    for (const PairList *p = &pl; p; p = p->tail())
    {
        // Print tag:
        indent(os, margin);
        os << "- ";
        const RObject *tag = p->tag();
        if (!tag)
            os << "(No tag):\n";
        else if (tag->sexptype() != SYMSXP)
            os << "(Tag not a SYMSXP):\n";
        else
            os << sympname(tag) << ":\n";
        // Print car:
        const RObject *car = p->car();
        if (const PairList *plinner = dynamic_cast<const PairList *>(car))
            pldump(os, *plinner, margin + 2);
        else if (const StringVector *sv = dynamic_cast<const StringVector *>(car))
            strdump(os, *sv, margin + 2);
        else
        {
            indent(os, margin + 2);
            if (!car)
                os << "NILSXP\n";
            else
            {
                SEXPTYPE st = car->sexptype();
                os << Rf_type2char(st);
                if (st == SYMSXP)
                    os << ": " << sympname(car);
                os << "\n";
            }
        }
    }
}
