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
 *  https://www.R-project.org/Licenses/
 */

/** @file DebugMacros.hpp
 *
 * @brief Various macros to help debugging CR/CXXR code.
 */

#ifndef DEBUGMACROS_HPP
#define DEBUGMACROS_HPP

#include <CXXR/PairList.hpp>
#include <CXXR/Symbol.hpp>

#define LOCATION __FILE__ << ":" << __LINE__ << " " << __func__ << ": "

#define TEST_PL(pp)                                                                                                                   \
    {                                                                                                                                 \
        ConsCell *p = SEXP_downcast<ConsCell *>(pp);                                                                                  \
        do                                                                                                                            \
        {                                                                                                                             \
            if (p && !(p->sexptype() == LISTSXP || p->sexptype() == LANGSXP || p->sexptype() == DOTSXP || p->sexptype() == BCODESXP)) \
            {                                                                                                                         \
                std::cerr << LOCATION << p->sexptype() << std::endl;                                                                  \
                std::abort();                                                                                                         \
            }                                                                                                                         \
            if (p)                                                                                                                    \
                p = p->tail();                                                                                                        \
        } while (p);                                                                                                                  \
    }

#define PRINT_PL(call)                                                                                                                      \
    if (call && (call->sexptype() == LISTSXP || call->sexptype() == DOTSXP || call->sexptype() == LANGSXP || call->sexptype() == BCODESXP)) \
    {                                                                                                                                       \
        std::cerr << LOCATION << "Begin ccdump for " << call->sexptype() << " ...\n";                                                       \
        ccdump(std::cerr, SEXP_downcast<const CXXR::PairList *>(call), 0);                                                                  \
        std::cerr << LOCATION << "Done ccdump ...\n\n";                                                                                     \
    }                                                                                                                                       \
    else if (call)                                                                                                                          \
    {                                                                                                                                       \
        std::cerr << LOCATION << "Not a pairlist but " << call->sexptype() << std::endl;                                                    \
    }                                                                                                                                       \
    else                                                                                                                                    \
    {                                                                                                                                       \
        std::cerr << LOCATION << "object = nullptr" << std::endl;                                                                           \
    }

#define CHECK_OBJECT_FLAG(x, v)                                                                                       \
    {                                                                                                                 \
        RObject *__x__ = x;                                                                                           \
        bool __v__ = v;                                                                                               \
        bool found = false;                                                                                           \
        PairList *new_attributes = __x__->attributes();                                                               \
        while (new_attributes)                                                                                        \
        {                                                                                                             \
            Symbol *name = SEXP_downcast<Symbol *>(new_attributes->tag());                                            \
            if (name == R_ClassSymbol)                                                                                \
            {                                                                                                         \
                found = true;                                                                                         \
                break;                                                                                                \
            }                                                                                                         \
            new_attributes = new_attributes->tail();                                                                  \
        }                                                                                                             \
        if (found && !__v__)                                                                                          \
        {                                                                                                             \
            std::cerr << LOCATION << "Attempt to set object to false, while R_ClassSymbol exists" << std::endl;       \
            abort();                                                                                                  \
        }                                                                                                             \
        else if (!found && __v__)                                                                                     \
        {                                                                                                             \
            std::cerr << LOCATION << "Attempt to set object to true, while R_ClassSymbol doesn't exist" << std::endl; \
            abort();                                                                                                  \
        }                                                                                                             \
    }

#define CHECK_EMPTY_ATTRIB_NAME(x)                                                   \
    {                                                                                \
        if (x)                                                                       \
        {                                                                            \
            RObject *__x__ = x;                                                      \
            PairList *new_attributes = __x__->attributes();                          \
            while (new_attributes)                                                   \
            {                                                                        \
                Symbol *name = SEXP_downcast<Symbol *>(new_attributes->tag());       \
                if (!name)                                                           \
                {                                                                    \
                    std::cerr << LOCATION << "Found empty symbol name" << std::endl; \
                    abort();                                                         \
                }                                                                    \
                new_attributes = new_attributes->tail();                             \
            }                                                                        \
        }                                                                            \
    }

#endif // DEBUGMACROS_HPP
