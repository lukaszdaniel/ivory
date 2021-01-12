/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2007  Andrew Runnalls
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

/** @file JMPException.hpp
 * @brief Class CXXR::JMPException.
 */

#ifndef JMPEXCEPTION_HPP
#define JMPEXCEPTION_HPP

#include <RContext.h>

namespace CXXR
{
    /** @brief Exception class to replace setjmp/longjmp.
     *
     * This class is intended as far as possible as a drop-in
     * replacement for the use of setjmp/longjmp within R.  The
     * replacement is necessary to ensure that the destructors of
     * automatic variables are invoked as the stack is unwound.
     *
     * @note This class is an interim measure: in due course it would
     * be desirable to replace it and RCNTXT with something more in
     * line with conventional C++ exception handling idioms.
     */
    class JMPException
    {
    public:
        /** @brief Constructor.
         *
         * @param the_context Pointer to the context within which the
         *          exception is to be caught.  (catch blocks within
         *          other contexts should rethrow the exception.)
         * @param the_mask Context mask, or zero.
         */
        JMPException(RContext *the_context = nullptr, int the_mask = 0)
            : m_context(the_context), m_mask(the_mask)
        {
        }

        /** @brief Target Context of this JMPException.
         *
         * @return pointer to the Context within which this
         * JMPException should be caught.
         */
        RContext *context() const
        {
            return m_context;
        }

        int mask() const
        {
            return m_mask;
        }

    private:
        RContext *m_context;
        int m_mask;
    };
} // namespace CXXR

#endif // JMPEXCEPTION_HPP
