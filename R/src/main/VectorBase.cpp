/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999--2020  The R Core Team.
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
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

/** @file VectorBase.cpp
 *
 * @brief Implementation of class VectorBase and related functions.
 */

#include <CXXR/VectorBase.hpp>
#include <R_ext/Error.h>
#include <Rinternals.h>
#include <Localization.h>

namespace CXXR
{
    // Force the creation of non-inline embodiments of functions callable
    // from C:
    namespace ForceNonInline
    {
        const auto &STDVEC_LENGTHptr = STDVEC_LENGTH;
        const auto &SET_TRUELENGTHptr = SET_TRUELENGTH;
        const auto &STDVEC_TRUELENGTHptr = STDVEC_TRUELENGTH;
    } // namespace ForceNonInline

    namespace
    {
        // Used in {,un}packGPBits():
        constexpr unsigned int GROWABLE_MASK = 1 << 5;
    } // namespace

    unsigned int VectorBase::packGPBits() const
    {
        unsigned int ans = RObject::packGPBits();
        if (m_growable)
            ans |= GROWABLE_MASK;
        return ans;
    }

    void VectorBase::unpackGPBits(unsigned int gpbits)
    {
        RObject::unpackGPBits(gpbits);
        m_growable = ((gpbits & GROWABLE_MASK) != 0);
    }

    void VectorBase::resize(R_xlen_t new_size)
    {
        // if (new_size > m_size)
        // {
        //     Rf_error("VectorBase::resize() : requested size exceeds current size.");
        //     return;
        // }
        m_size = new_size;
    }

    // The error messages here match those used by CR (as of 3.0.2),
    // not including the malformed unit abbreviations.
    void VectorBase::tooBig(std::size_t bytes)
    {
        double dsize = double(bytes) / 1024.0;
        if (dsize > 1024.0 * 1024.0)
        {
            Rf_errorcall(nullptr, _("cannot allocate vector of size %0.1f GB"), dsize / 1024.0 / 1024.0);
        }
        else if (dsize > 1024.0)
        {
            Rf_errorcall(nullptr, _("cannot allocate vector of size %0.1f MB"), dsize / 1024.0);
        }
        else
        {
            Rf_errorcall(nullptr, _("cannot allocate vector of size %0.1f KB"), dsize);
        }
    }

    /* Growable vector support */
    unsigned int VectorBase::growable_bit_set(RObject *x)
    {
        if (!x)
            return 0;
        return SEXP_downcast<VectorBase *>(x)->growable();
    }

    void VectorBase::set_growable_bit(RObject *x)
    {
        if (!x)
            return;
        // x->m_gpbits |= GROWABLE_MASK;
        SEXP_downcast<VectorBase *>(x)->setGrowable(true);
    }
} // namespace CXXR
