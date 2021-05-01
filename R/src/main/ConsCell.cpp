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

#include <iostream>
#include <CXXR/ConsCell.hpp>
#include <CXXR/ByteCode.hpp>
#include <CXXR/DottedArgs.hpp>
#include <CXXR/Expression.hpp>
#include <CXXR/PairList.hpp>
#include <CXXR/StringVector.hpp>
#include <CXXR/Symbol.hpp>
#include <Defn.h>
#include <Rinternals.h>

using namespace std;
using namespace CXXR;

namespace CXXR
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

    ConsCell::ConsCell(SEXPTYPE st, RObject *cr, PairList *tl, RObject *tg)
        : RObject(st), m_missing(0)
    {
        checkST(st);
        m_car.retarget(this, cr);
        m_tail = tl;
        m_tag = tg;
    }

    ConsCell::ConsCell(const ConsCell &pattern, bool deep)
        : RObject(pattern, deep), m_car(pattern.m_car, deep),
          m_missing(0)
    {
        m_tail = clone(pattern.tail(), deep);
        m_tag = pattern.tag();
    }

    ConsCell::ConsCell(const ConsCell &pattern, bool deep, int)
        : RObject(pattern, deep), m_car(pattern.m_car, deep), m_missing(0)
    {
        m_tail = nullptr;
        m_tag = pattern.tag();
    }

    namespace
    {
        // Used in {,un}packGPBits():
        /* reserve 4 bits -- m_missing uses only 2 bit now */
        /* MISSING_MASK = 15 */
        constexpr unsigned int MISSING_MASK = ((1 << 4) - 1);
    } // namespace

    unsigned int ConsCell::packGPBits() const
    {
        unsigned int ans = RObject::packGPBits();
        if (m_missing)
        {
            ans &= ~MISSING_MASK; // erase 4 rightmost bits
            ans |= m_missing;
        }
        return ans;
    }

    void ConsCell::unpackGPBits(unsigned int gpbits)
    {
        RObject::unpackGPBits(gpbits);
        m_missing = gpbits;
        // m_missing = ((gpbits & MISSING_MASK) != 0);
    }

#if !BOXED_BINDING_CELLS
    double ConsCell::bndcell_dval(const RObject *x)
    {
        if (!x)
            return 0.0;
        ConsCell::checkST(x);
        return ((R_bndval_t *)&(SEXP_downcast<const ConsCell *>(x, false)->m_car))->dval;
    }

    int ConsCell::bndcell_ival(const RObject *x)
    {
        if (!x)
            return 0;
        ConsCell::checkST(x);
        return ((R_bndval_t *)&(SEXP_downcast<const ConsCell *>(x, false)->m_car))->ival;
    }

    int ConsCell::bndcell_lval(const RObject *x)
    {
        if (!x)
            return 0;
        ConsCell::checkST(x);
        return ((R_bndval_t *)&(SEXP_downcast<const ConsCell *>(x, false)->m_car))->ival;
    }

    void ConsCell::set_bndcell_dval(RObject *x, double v)
    {
        if (!x)
            return;
        ConsCell::checkST(x);
        ((R_bndval_t *)&(SEXP_downcast<ConsCell *>(x, false)->m_car))->dval = v;
    }

    void ConsCell::set_bndcell_ival(RObject *x, int v)
    {
        if (!x)
            return;
        ConsCell::checkST(x);
        ((R_bndval_t *)&(SEXP_downcast<ConsCell *>(x, false)->m_car))->ival = v;
    }

    void ConsCell::set_bndcell_lval(RObject *x, int v)
    {
        if (!x)
            return;
        ConsCell::checkST(x);
        ((R_bndval_t *)&(SEXP_downcast<ConsCell *>(x, false)->m_car))->ival = v;
    }
#endif
    void ConsCell::clear_bndcell_tag(SEXP cell)
    {
        if (!cell)
            return;
        ConsCell *cc = SEXP_downcast<ConsCell *>(cell);

        if (cc->bndcellTag())
        {
            cc->clearCar();
            cc->setBndCellTag(0);
        }
    }

    void ConsCell::setMissing(unsigned int v)
    {
        int __other_flags__ = packGPBits() & ~MISSING_MASK; // erase 4 rightmost bits
        // m_gpbits = __other_flags__ | v;
        m_missing = __other_flags__ | v;
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

    void ConsCell::visitReferents(const_visitor *v) const
    {
#if CXXR_TRUE
        RObject::visitReferents(v);
        if (altrep())
        {
            if (bndcellTag())
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
            if (car() && (sexptype() != LISTSXP || BOXED_BINDING_CELLS || bndcellTag() == 0))
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
            p->RObject::visitReferents(v);
            if (p->m_tail && !(p->m_tail->sexptype() == LISTSXP || p->m_tail->sexptype() == LANGSXP || p->m_tail->sexptype() == DOTSXP || p->m_tail->sexptype() == BCODESXP))
            {
                std::cerr << LOCATION << Rf_type2char(p->m_tail->sexptype()) << " : " << R::typeName(p->m_tail) << std::endl;
                ccdump(std::cerr, p, 0);
                abort();
            }
            if (p->altrep())
            {
                if (p->bndcellTag())
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
                if (p->car() && (p->sexptype() != LISTSXP || BOXED_BINDING_CELLS || p->bndcellTag() == 0))
                    p->car()->conductVisitor(v);
                if (p->tag())
                    p->tag()->conductVisitor(v);
            }
            p = p->m_tail;
        } while (p && (*v)(p));
#endif
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
            const Symbol *symb = dynamic_cast<const Symbol *>(sym);
            if (!symb)
                return "(SYMSXP is not a Symbol)";
            return symb->name()->c_str();
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
        os << R::sexptype2char(cc->sexptype()) << "\n";
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
            os << "Car (p = " << R::sexptype2char(p->sexptype()) << ", " << car << ") = ";
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
                    os << R::sexptype2char(st);
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
    void ConsCell::checkST(const RObject *x)
    {
#ifdef ENABLE_ST_CHECKS
        switch (e->sexptype())
        {
        case LISTSXP:
        case LANGSXP:
        case DOTSXP:
        case BCODESXP:
            break;
        default:
            std::cerr << "Inappropriate SEXPTYPE (" << e->sexptype() << ") for PairList." << std::endl;
            abort();
        }
#endif
    }
} // namespace CXXR

// ***** C interface *****

SEXP TAG(SEXP e)
{
    if (!e)
        return nullptr;
    if (TYPEOF(e) == S4SXP)
    {
        return S4TAG(e);
    }
    else
    {
        ConsCell::checkST(e);
        const ConsCell *cc = SEXP_downcast<const ConsCell *>(e);
        return cc->tag();
    }
}

void SET_TAG(SEXP x, SEXP v)
{
    if (!x)
        Rf_error(_("incorrect value"));
    if (TYPEOF(x) == S4SXP)
    {
        SET_S4TAG(x, v);
    }
    else
    {
        ConsCell::checkST(x);
        ConsCell *cc = SEXP_downcast<ConsCell *>(x, false);
        cc->setTag(v);
    }
}

SEXP SETCAR(SEXP x, SEXP y)
{
    if (!x)
        Rf_error(_("incorrect value"));

    ConsCell *cc = SEXP_downcast<ConsCell *>(x, false);
    cc->setCar(y);
    return y;
}

SEXP SETCDR(SEXP x, SEXP y)
{
    if (!x)
        Rf_error(_("incorrect value"));
    ConsCell::checkST(x);
    ConsCell *cc = SEXP_downcast<ConsCell *>(x, false);
    PairList *tl = SEXP_downcast<PairList *>(y);
    cc->setTail(tl);
    return y;
}

SEXP SETCADR(SEXP x, SEXP y)
{
    if (!x ||
        CDR(x) == nullptr)
        Rf_error(_("incorrect value"));
    SEXP cell = CDR(x);

    ConsCell *cc = SEXP_downcast<ConsCell *>(cell, false);
    cc->setCar(y);
    return y;
}

SEXP SETCADDR(SEXP x, SEXP y)
{
    if (!x ||
        CDR(x) == nullptr ||
        CDDR(x) == nullptr)
        Rf_error(_("incorrect value"));
    SEXP cell = CDDR(x);

    ConsCell *cc = SEXP_downcast<ConsCell *>(cell, false);
    cc->setCar(y);
    return y;
}

SEXP SETCADDDR(SEXP x, SEXP y)
{
    if (!x ||
        CDR(x) == nullptr ||
        CDDR(x) == nullptr ||
        CDDDR(x) == nullptr)
        Rf_error(_("incorrect value"));
    SEXP cell = CDDDR(x);

    ConsCell *cc = SEXP_downcast<ConsCell *>(cell, false);
    cc->setCar(y);
    return y;
}

SEXP SETCAD4R(SEXP x, SEXP y)
{
    if (!x ||
        CDR(x) == nullptr ||
        CDDR(x) == nullptr ||
        CDDDR(x) == nullptr ||
        CD4R(x) == nullptr)
        Rf_error(_("incorrect value"));
    SEXP cell = CD4R(x);

    ConsCell *cc = SEXP_downcast<ConsCell *>(cell, false);
    cc->setCar(y);
    return y;
}

int MISSING(SEXP x)
{
    if (!x)
        return 0;
    return SEXP_downcast<const ConsCell *>(x)->missing();
}

void SET_MISSING(SEXP x, int v)
{
    if (x)
        SEXP_downcast<ConsCell *>(x)->setMissing(v);
}

int BNDCELL_TAG(SEXP cell)
{
    return cell ? SEXP_downcast<ConsCell *>(cell)->bndcellTag() : 0;
}

void SET_BNDCELL_TAG(SEXP cell, int val)
{
    if (cell)
        SEXP_downcast<ConsCell *>(cell)->setBndCellTag(val);
}

double BNDCELL_DVAL(SEXP cell)
{
    return BNDCELL_DVAL_MACRO(cell);
}

int BNDCELL_IVAL(SEXP cell)
{
    return BNDCELL_IVAL_MACRO(cell);
}

int BNDCELL_LVAL(SEXP cell)
{
    return BNDCELL_LVAL_MACRO(cell);
}

void SET_BNDCELL_DVAL(SEXP cell, double v)
{
    SET_BNDCELL_DVAL_MACRO(cell, v);
}

void SET_BNDCELL_IVAL(SEXP cell, int v)
{
    SET_BNDCELL_IVAL_MACRO(cell, v);
}

void SET_BNDCELL_LVAL(SEXP cell, int v)
{
    SET_BNDCELL_LVAL_MACRO(cell, v);
}

void INIT_BNDCELL(SEXP cell, int type)
{
    INIT_BNDCELL_MACRO(cell, type);
}

void SET_BNDCELL(SEXP cell, SEXP val)
{
    CXXR::ConsCell::clear_bndcell_tag(cell);
    SETCAR(cell, val);
}
