/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2016 and onwards the Rho Project Authors.
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

/** @file strutil.hpp
 * @brief Various string utilities.
 */

#ifndef CXXR_STRUTIL_HPP
#define CXXR_STRUTIL_HPP

#include <sstream>
#include <ostream>

namespace CXXR
{
    namespace internal
    {
        inline void strcat_helper(std::ostringstream &stream) {}

        template <typename Arg, typename... Args>
        inline void strcat_helper(std::ostringstream &stream,
                                  Arg arg, Args... args)
        {
            stream << arg;
            strcat_helper(stream, args...);
        }
    }

    template <typename... Args>
    std::string StrCat(Args... args)
    {
        std::ostringstream stream;
        internal::strcat_helper(stream, args...);
        return stream.str();
    }

    namespace Color
    {
        class Modifier
        {
        public:
            enum Code
            {
                FG_DEFAULT = 39,
                FG_BLACK = 30,
                FG_RED = 31,
                FG_GREEN = 32,
                FG_YELLOW = 33,
                FG_BLUE = 34,
                FG_MAGENTA = 35,
                FG_CYAN = 36,
                FG_LIGHT_GRAY = 37,
                FG_DARK_GRAY = 90,
                FG_LIGHT_RED = 91,
                FG_LIGHT_GREEN = 92,
                FG_LIGHT_YELLOW = 93,
                FG_LIGHT_BLUE = 94,
                FG_LIGHT_MAGENTA = 95,
                FG_LIGHT_CYAN = 96,
                FG_WHITE = 97,
                BG_RED = 41,
                BG_GREEN = 42,
                BG_BLUE = 44,
                BG_DEFAULT = 49
            };

            Code code() const
            {
                return m_code;
            }

        private:
            Code m_code;

        public:
            explicit Modifier(Code pCode) : m_code(pCode) {}
            friend std::ostream &operator<<(std::ostream &os, const Modifier &mod)
            {
                return os << "\033[" << mod.m_code << "m";
            }
        };
    } // namespace Color
} // namespace CXXR

#endif // CXXR_STRUTIL_HPP
