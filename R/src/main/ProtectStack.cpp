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

/** @file ProtectStack.cpp
 *
 * Implementation of class ProtectStack and associated C
 * interface.
 */

#include <CXXR/ProtectStack.hpp>

#include <stdexcept>

using namespace std;
using namespace CXXR;

namespace CXXR
{
    // Force the creation of non-inline embodiments of functions callable
    // from C:
    namespace ForceNonInline
    {
        const auto &protectp = Rf_protect;
        const auto &unprotectp = Rf_unprotect;
        const auto &unprotect_ptrp = Rf_unprotect_ptr;
        const auto &ProtectWithIndexp = R_ProtectWithIndex;
        const auto &Reprotectp = R_Reprotect;
        const auto &cxxrprotectp = CXXR_protect;
        const auto &cxxrunprotectp = CXXR_unprotect;
        const auto &cxxrunprotect_ptrp = CXXR_unprotect_ptr;
        const auto &cxxrProtectWithIndexp = CXXR_ProtectWithIndex;
        const auto &cxxrReprotectp = CXXR_Reprotect;
    } // namespace ForceNonInline

#ifdef NDEBUG
    vector<RObject *> *ProtectStack::s_stack;
#else
    vector<tuple<RObject *, RCNTXT *, const char *>> *ProtectStack::s_stack;
#endif

    void ProtectStack::initialize()
    {
#ifdef NDEBUG
        s_stack = new vector<RObject *>;
#else
        s_stack = new vector<tuple<RObject *, RCNTXT *, const char *>>;
#endif
    }

    void ProtectStack::restoreSize(size_t new_size)
    {
        if (new_size > s_stack->size())
            throw out_of_range("ProtectStack::restoreSize: requested size greater than current size.");
        s_stack->resize(new_size);
    }

    void ProtectStack::reprotect(RObject *node, unsigned int index, const char *function_name)
    {
        if (index >= s_stack->size())
            throw out_of_range("ProtectStack::reprotect: index out of range.");
#ifdef NDEBUG
        (*s_stack)[index] = node;
#else
        auto &pr = (*s_stack)[index];
        // if (get<1>(pr) != R_GlobalContext)
        // {
        //     std::cerr << __LINE__ << " Expected function '" << get<2>(pr) << "', got '" << function_name << "'" << std::endl;
        //     throw logic_error("ProtectStack::reprotect: not in same context as the corresponding call of protect().");
        // }
        // if (get<2>(pr) != function_name)
        // {
        //     std::cerr << __LINE__ << " Expected function '" << get<2>(pr) << "', got '" << function_name << "'" << std::endl;
        //     throw logic_error("ProtectStack::reprotect: not in same function as the corresponding call of protect().");
        // }
        pr = std::make_tuple(node, R_GlobalContext, function_name);
#endif
    }

    unsigned int ProtectStack::protect_(RObject *node, const char *function_name)
    {
        unsigned int index = s_stack->size();
#ifdef NDEBUG
        s_stack->push_back(node);
#else
        s_stack->push_back(std::make_tuple(node, R_GlobalContext, function_name));
#endif
        return index;
    }

    void ProtectStack::unprotect_(unsigned int count, const char *function_name)
    {
        size_t sz = s_stack->size();
        if (count > sz)
        {
            throw out_of_range("ProtectStack::unprotect: count greater than current stack size.");
        }
#ifdef NDEBUG
        s_stack->resize(sz - count);
#else
        for (unsigned int i = 0; i < count; ++i)
        {
            const auto &pr = s_stack->back();
            if (get<1>(pr) != R_GlobalContext)
            {
                std::cerr << __LINE__ << " Expected function '" << get<2>(pr) << "', got '" << function_name << "'" << std::endl;
                throw logic_error("ProtectStack::unprotect: not in same context as the corresponding call of protect().");
            }
            // if (get<2>(pr) != function_name)
            // {
            //     std::cerr << __LINE__ << " Expected function '" << get<2>(pr) << "', got '" << function_name << "'" << std::endl;
            //     throw logic_error("ProtectStack::unprotect: not in same function as the corresponding call of protect().");
            // }
            s_stack->pop_back();
        }
#endif
    }

    void ProtectStack::unprotectPtr(RObject *node, const char *function_name)
    {
#ifdef NDEBUG
        auto rit = find(s_stack->rbegin(), s_stack->rend(), node);
#else
        auto rit = s_stack->rbegin();
        while (rit != s_stack->rend() && get<0>(*rit) != node)
            ++rit;
#endif
        if (rit == s_stack->rend())
            throw invalid_argument("ProtectStack::unprotectPtr: pointer not found.");
        // See Josuttis p.267 for the need for -- :
        const auto &pr = (--(rit.base()));
        // if (get<1>(*pr) != R_GlobalContext)
        // {
        //     std::cerr << __LINE__ << " Expected function '" << get<2>(*pr) << "', got '" << function_name << "'" << std::endl;
        //     throw logic_error("ProtectStack::unprotect: not in same context as the corresponding call of protect().");
        // }
        // if (get<2>(*pr) != function_name)
        // {
        //     std::cerr << __LINE__ << " Expected function '" << get<2>(*pr) << "', got '" << function_name << "'" << std::endl;
        //     throw logic_error("ProtectStack::unprotect: not in same function as the corresponding call of protect().");
        // }
        s_stack->erase(pr);
    }

    void ProtectStack::visitRoots(GCNode::const_visitor *v)
    {
#ifdef NDEBUG
        for (auto &n : *s_stack)
        {
            if (n)
                n->conductVisitor(v);
        }
#else
        for (auto &n : *s_stack)
        {
            if (get<0>(n))
                (get<0>(n))->conductVisitor(v);
        }
#endif
    }
} // namespace CXXR

// ***** C interface *****

void Rf_ppsRestoreSize(size_t new_size)
{
    ProtectStack::restoreSize(new_size);
}

size_t Rf_ppsSize()
{
    return ProtectStack::size();
}
