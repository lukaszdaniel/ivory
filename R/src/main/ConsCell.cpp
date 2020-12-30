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
#ifdef ENABLE_ST_CHECKS
        switch (e->sexptype())
        {
        case LISTSXP:
        case LANGSXP:
        case DOTSXP:
        case BCODESXP:
            break;
        default:
            std::cerr << LOCATION << "Inappropriate SEXPTYPE (" << e->sexptype() << ") for PairList." << std::endl;
            abort();
        }
#endif
        ConsCell &cc = *SEXP_downcast<ConsCell *>(e, false);
        return cc.tag();
    }

    void ConsCell::set_tag(RObject *x, RObject *v)
    {
        if (!x)
            Rf_error(_("incorrect value"));
#ifdef ENABLE_ST_CHECKS
        switch (x->sexptype())
        {
        case LISTSXP:
        case LANGSXP:
        case DOTSXP:
        case BCODESXP:
            break;
        default:
            std::cerr << LOCATION << "Inappropriate SEXPTYPE (" << x->sexptype() << ") for PairList." << std::endl;
            abort();
        }
#endif
        ConsCell &cc = *SEXP_downcast<ConsCell *>(x, false);
        cc.setTag(v);
    }

    RObject *ConsCell::car0(RObject *e)
    {
        if (!e)
            return nullptr;
#ifdef ENABLE_ST_CHECKS
        switch (e->sexptype())
        {
        case LISTSXP:
        case LANGSXP:
        case DOTSXP:
        case BCODESXP:
            break;
        default:
            std::cerr << LOCATION << "Inappropriate SEXPTYPE (" << e->sexptype() << ") for PairList." << std::endl;
            abort();
        }
#endif
        ConsCell &cc = *SEXP_downcast<ConsCell *>(e, false);
        return cc.car();
    }

    void ConsCell::set_car0(RObject *x, RObject *v)
    {
        if (!x)
            Rf_error(_("incorrect value"));
#ifdef ENABLE_ST_CHECKS
        switch (x->sexptype())
        {
        case LISTSXP:
        case LANGSXP:
        case DOTSXP:
        case BCODESXP:
            break;
        default:
            std::cerr << LOCATION << "Inappropriate SEXPTYPE (" << x->sexptype() << ") for PairList." << std::endl;
            abort();
        }
#endif
        ConsCell &cc = *SEXP_downcast<ConsCell *>(x, false);
        cc.setCar(v);
    }

    RObject *ConsCell::cdr(RObject *e)
    {
        if (!e)
            return nullptr;
#ifdef ENABLE_ST_CHECKS
        switch (e->sexptype())
        {
        case LISTSXP:
        case LANGSXP:
        case DOTSXP:
        case BCODESXP:
            break;
        default:
            std::cerr << LOCATION << "Inappropriate SEXPTYPE (" << e->sexptype() << ") for PairList." << std::endl;
            abort();
        }
#endif
        ConsCell &cc = *SEXP_downcast<ConsCell *>(e, false);
        return cc.tail();
    }

    void ConsCell::set_cdr(RObject *x, RObject *v)
    {
        if (!x)
            Rf_error(_("incorrect value"));
#ifdef ENABLE_ST_CHECKS
        switch (x->sexptype())
        {
        case LISTSXP:
        case LANGSXP:
        case DOTSXP:
        case BCODESXP:
            break;
        default:
            std::cerr << LOCATION << "Inappropriate SEXPTYPE (" << x->sexptype() << ") for PairList." << std::endl;
            abort();
        }
#endif
        ConsCell &cc = *SEXP_downcast<ConsCell *>(x, false);
        PairList *tl = SEXP_downcast<PairList *>(v);
        cc.setTail(tl);
    }

    double ConsCell::bndcell_dval(RObject *x)
    {
        if (!x)
            return 0.0;
#ifdef ENABLE_ST_CHECKS
        switch (x->sexptype())
        {
        case LISTSXP:
        case LANGSXP:
        case DOTSXP:
        case BCODESXP:
            break;
        default:
            std::cerr << LOCATION << "Inappropriate SEXPTYPE (" << x->sexptype() << ") for PairList." << std::endl;
            abort();
        }
#endif
#ifdef CXXR_OLD_PAIRLIST_IMPL
        return ((R_bndval_t *)&(SEXP_downcast<ConsCell *>(x, false)->u.listsxp.m_carval))->dval;
#else
        return ((R_bndval_t *)&(SEXP_downcast<ConsCell *>(x, false)->m_car))->dval;
#endif
    }

    int ConsCell::bndcell_ival(RObject *x)
    {
        if (!x)
            return 0;
#ifdef ENABLE_ST_CHECKS
        switch (x->sexptype())
        {
        case LISTSXP:
        case LANGSXP:
        case DOTSXP:
        case BCODESXP:
            break;
        default:
            std::cerr << LOCATION << "Inappropriate SEXPTYPE (" << x->sexptype() << ") for PairList." << std::endl;
            abort();
        }
#endif
#ifdef CXXR_OLD_PAIRLIST_IMPL
        return ((R_bndval_t *)&(SEXP_downcast<ConsCell *>(x, false)->u.listsxp.m_carval))->ival;
#else
        return ((R_bndval_t *)&(SEXP_downcast<ConsCell *>(x, false)->m_car))->ival;
#endif
    }

    int ConsCell::bndcell_lval(RObject *x)
    {
        if (!x)
            return 0;
#ifdef ENABLE_ST_CHECKS
        switch (x->sexptype())
        {
        case LISTSXP:
        case LANGSXP:
        case DOTSXP:
        case BCODESXP:
            break;
        default:
            std::cerr << LOCATION << "Inappropriate SEXPTYPE (" << x->sexptype() << ") for PairList." << std::endl;
            abort();
        }
#endif
#ifdef CXXR_OLD_PAIRLIST_IMPL
        return ((R_bndval_t *)&(SEXP_downcast<ConsCell *>(x, false)->u.listsxp.m_carval))->ival;
#else
        return ((R_bndval_t *)&(SEXP_downcast<ConsCell *>(x, false)->m_car))->ival;
#endif
    }

    void ConsCell::set_bndcell_dval(RObject *x, double v)
    {
        if (!x)
            return;
#ifdef ENABLE_ST_CHECKS
        switch (x->sexptype())
        {
        case LISTSXP:
        case LANGSXP:
        case DOTSXP:
        case BCODESXP:
            break;
        default:
            std::cerr << LOCATION << "Inappropriate SEXPTYPE (" << x->sexptype() << ") for PairList." << std::endl;
            abort();
        }
#endif
#ifdef CXXR_OLD_PAIRLIST_IMPL
        ((R_bndval_t *)&(SEXP_downcast<ConsCell *>(x, false)->u.listsxp.m_carval))->dval = v;
#else
        ((R_bndval_t *)&(SEXP_downcast<ConsCell *>(x, false)->m_car))->dval = v;
#endif
    }

    void ConsCell::set_bndcell_ival(RObject *x, int v)
    {
        if (!x)
            return;
#ifdef ENABLE_ST_CHECKS
        switch (x->sexptype())
        {
        case LISTSXP:
        case LANGSXP:
        case DOTSXP:
        case BCODESXP:
            break;
        default:
            std::cerr << LOCATION << "Inappropriate SEXPTYPE (" << x->sexptype() << ") for PairList." << std::endl;
            abort();
        }
#endif
#ifdef CXXR_OLD_PAIRLIST_IMPL
        ((R_bndval_t *)&(SEXP_downcast<ConsCell *>(x, false)->u.listsxp.m_carval))->ival = v;
#else
        ((R_bndval_t *)&(SEXP_downcast<ConsCell *>(x, false)->m_car))->ival = v;
#endif
    }

    void ConsCell::set_bndcell_lval(RObject *x, int v)
    {
        if (!x)
            return;
#ifdef ENABLE_ST_CHECKS
        switch (x->sexptype())
        {
        case LISTSXP:
        case LANGSXP:
        case DOTSXP:
        case BCODESXP:
            break;
        default:
            std::cerr << LOCATION << "Inappropriate SEXPTYPE (" << x->sexptype() << ") for PairList." << std::endl;
            abort();
        }
#endif
#ifdef CXXR_OLD_PAIRLIST_IMPL
        ((R_bndval_t *)&(SEXP_downcast<ConsCell *>(x, false)->u.listsxp.m_carval))->ival = v;
#else
        ((R_bndval_t *)&(SEXP_downcast<ConsCell *>(x, false)->m_car))->ival = v;
#endif
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
#ifdef CXXR_OLD_PAIRLIST_IMPL
                u.listsxp.m_cdrval = new PairList(nullptr, SEXP_downcast<PairList *>(u.listsxp.m_cdrval), nullptr);
#else
                m_tail = new PairList(nullptr, m_tail, nullptr);
#endif
        }
        catch (...)
        {
#ifndef CXXR_OLD_PAIRLIST_IMPL
            if (m_tail)
                m_tail->expose();
#endif
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
#if CXXR_TRUE
        RObject::visitChildren(v);
        if (altrep())
        {
            if (RObject::bndcell_tag(this))
            {
                Rf_error("bad binding access");
            }
            if (car())
                car()->conductVisitor(v);
            if (tag())
                tag()->conductVisitor(v);
            if (tail())
                tail()->conductVisitor(v);
        }
        else
        {
            if (car() && (sexptype() != LISTSXP || BOXED_BINDING_CELLS || RObject::bndcell_tag(this) == 0))
                car()->conductVisitor(v);
            if (tag())
                tag()->conductVisitor(v);
            if (tail())
                tail()->conductVisitor(v);
        }
#else
        const ConsCell *p = this;
        do
        {
            ccdump(cerr, p, 1);
            std::cerr << std::endl;
            p->RObject::visitChildren(v);
            if (p->altrep())
            {
                if (RObject::bndcell_tag(p))
                {
                    Rf_error("bad binding access");
                }
                if (p->car())
                    p->car()->conductVisitor(v);
                if (p->tag())
                    p->tag()->conductVisitor(v);
            }
            else
            {
                if (p->car() && (p->sexptype() != LISTSXP || BOXED_BINDING_CELLS || RObject::bndcell_tag(p) == 0))
                    p->car()->conductVisitor(v);
                if (p->tag())
                    p->tag()->conductVisitor(v);
            }
            p = p->m_tail;
        } while (p && (*v)(p));
#endif
    }
#ifndef CXXR_OLD_PAIRLIST_IMPL
    namespace
    {
        void indent(ostream &os, size_t margin)
        {
            while (margin--)
                os << " ";
        }

        const char *sympname(const RObject *sym)
        {
            const Symbol *symb = dynamic_cast<const Symbol *>(sym);
            if (!symb)
                return "(SYMSXP is not a Symbol)";
            const String *pname = symb->name();
            if (!pname)
                return "(Symbol has no PRINTNAME)";
            return pname->c_str();
        }
    } // namespace

    void ccdump(std::ostream &os, const ConsCell *cc, size_t margin)
    {
        if (!cc)
        {
            os << "cc is nullptr" << std::endl;
            return;
        }
        // indent(os, margin);
        if (cc->altrep())
            os << "altrep ";
        os << Rf_type2char(cc->sexptype()) << "\n";
        for (const ConsCell *p = cc; p; p = p->tail())
        {
            // Print tag:
            indent(os, margin);
            os << "- ";
            const RObject *tag = p->tag();
            if (tag && tag->altrep())
                os << "altrep ";
            os << "Tag = ";
            if (!tag)
                os << "(No tag):\n";
            else if (tag->sexptype() != SYMSXP)
                os << "(Tag not a SYMSXP but " << R::sexptype2char(tag->sexptype()) << "):\n";
            else
                os << sympname(tag) << ":\n";
            // Print car:
            const RObject *car = p->car();
            indent(os, margin + 2);
            // if (car && car->altrep()) os << "altrep ";
            os << "Car (p = " << Rf_type2char(RObject::typeof_(p)) << ", " << car << ") = ";
            if (const ConsCell *ccinner = dynamic_cast<const ConsCell *>(car))
            {
                ccdump(os, ccinner, margin + 2);
            }
            else if (const StringVector *sv = dynamic_cast<const StringVector *>(car))
            {
                strdump(os, sv, margin + 2);
            }
            else
            {
                // indent(os, margin + 2);
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
            if (p->tail())
            {
                indent(os, margin + 2);
                os << "------" << std::endl;
            }
        }
    }
#endif
} // namespace CXXR
