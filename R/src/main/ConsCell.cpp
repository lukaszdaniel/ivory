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
 * @brief Class CXXR::ConsCell and associated C interface.
 */

#include <CXXR/ConsCell.hpp>
#include <iostream>
#include <CXXR/ByteCode.hpp>
#include <CXXR/DottedArgs.hpp>
#include <CXXR/Expression.hpp>
#include <CXXR/PairList.hpp>
#include <CXXR/StringVector.hpp>
#include <CXXR/Symbol.hpp>
#include <Defn.h>
// #include <Rinternals.h>

using namespace std;
using namespace CXXR;

namespace CXXR
{
    // Force the creation of non-inline embodiments of functions callable
    // from C:
    namespace ForceNonInline
    {
        // const auto &CAD4Rptr = CAD4R;
        // const auto &CADDDRptr = CADDDR;
        // const auto &CADDRptr = CADDR;
        // const auto &CADRptr = CADR;
        // const auto &CDARptr = CDAR;
        // const auto &CDDRptr = CDDR;
        // const auto &CDDDRptr = CDDDR;
        // const auto &CDRptr = CDR;
        // const auto &BINDING_IS_LOCKEDptr = BINDING_IS_LOCKED;
        // const auto &IS_ACTIVE_BINDINGptr = IS_ACTIVE_BINDING;
        // const auto &LOCK_BINDINGptr = LOCK_BINDING;
        // const auto &SET_ACTIVE_BINDING_BITptr = SET_ACTIVE_BINDING_BIT;
        // const auto &UNLOCK_BINDINGptr = UNLOCK_BINDING;
    } // namespace ForceNonInline

    /* List Access Methods */
    RObject *ConsCell::tag(RObject *e)
    {
        if (!e)
            return nullptr;
        return e->u.listsxp.m_tagval;
    }

    void ConsCell::set_tag(RObject *x, RObject *v)
    {
        if (!x)
            Rf_error(_("incorrect value"));
        x->u.listsxp.m_tagval = v;
    }

    RObject *ConsCell::car0(RObject *e)
    {
        if (!e)
            return nullptr;
        return e->u.listsxp.m_carval;
    }

    void ConsCell::set_car0(RObject *x, RObject *v)
    {
        if (!x)
            Rf_error(_("incorrect value"));
        x->u.listsxp.m_carval = v;
    }

    RObject *ConsCell::cdr(RObject *e)
    {
        if (!e)
            return nullptr;
        return e->u.listsxp.m_cdrval;
    }

    void ConsCell::set_cdr(RObject *x, RObject *v)
    {
        if (!x)
            Rf_error(_("incorrect value"));
        x->u.listsxp.m_cdrval = v;
    }

    double ConsCell::bndcell_dval(RObject *x)
    {
        if (!x)
            return 0.0;
        return ((R_bndval_t *)&(x->u.listsxp.m_carval))->dval;
    }

    int ConsCell::bndcell_ival(RObject *x)
    {
        if (!x)
            return 0;
        return ((R_bndval_t *)&(x->u.listsxp.m_carval))->ival;
    }

    int ConsCell::bndcell_lval(RObject *x)
    {
        if (!x)
            return 0;
        return ((R_bndval_t *)&(x->u.listsxp.m_carval))->ival;
    }

    void ConsCell::set_bndcell_dval(RObject *x, double v)
    {
        if (!x)
            return;
        ((R_bndval_t *)&(x->u.listsxp.m_carval))->dval = v;
    }

    void ConsCell::set_bndcell_ival(RObject *x, int v)
    {
        if (!x)
            return;
        ((R_bndval_t *)&(x->u.listsxp.m_carval))->ival = v;
    }

    void ConsCell::set_bndcell_lval(RObject *x, int v)
    {
        if (!x)
            return;
        ((R_bndval_t *)&(x->u.listsxp.m_carval))->ival = v;
    }

    ConsCell::ConsCell(SEXPTYPE st, size_t sz)
        : RObject(st)
    {
        checkST(st);
#ifdef LONG_VECTOR_SUPPORT
        if (sz > R_SHORT_LEN_MAX)
            Rf_error(_("invalid length for pairlist"));
#endif
        if (sz == 0)
            throw std::out_of_range(_("Cannot construct PairList of zero length."));
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

    void ConsCell::checkST(SEXPTYPE st)
    {
        switch (st)
        {
        case LISTSXP:
        case LANGSXP:
        case DOTSXP:
        case BCODESXP:
            break;
        default:
            throw std::invalid_argument(_("Inappropriate SEXPTYPE for PairList."));
        }
    }

    void ConsCell::visitChildren(const_visitor *v) const
    {
        const ConsCell *p = this;
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

    void ccdump(std::ostream &os, const ConsCell &cc, size_t margin)
    {
        indent(os, margin);
        os << Rf_type2char(cc.sexptype()) << "\n";
        for (const ConsCell *p = &cc; p; p = p->tail())
        {
            // Print tag:
            indent(os, margin);
            os << "- ";
            const RObject *tag = p->tag();
            os << "Tag = ";
            if (!tag)
                os << "(No tag):\n";
            else if (tag->sexptype() != SYMSXP)
                os << "(Tag not a SYMSXP):\n";
            else
                os << sympname(tag) << ":\n";
            // Print car:
            const RObject *car = p->car();
            indent(os, margin + 2);
            os << "Car = ";
            if (const ConsCell *ccinner = dynamic_cast<const ConsCell *>(car))
                ccdump(os, *ccinner, margin + 2);
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
} // namespace CXXR
