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
 *  http://www.r-project.org/Licenses/
 */

/** @file SchwarzCounter.hpp
 *
 * @brief Schwarz counter class template.
 */

#ifndef SCHWARZCOUNTER_HPP
#define SCHWARZCOUNTER_HPP

namespace CXXR
{
    /** @brief Schwarz counter.
     *
     * The Schwarz counter (see for example Stephen C. Dewhurst's book
     * 'C++ Gotchas') is a programming idiom to ensure that a class \a
     * T (including particularly its static members) is initialized
     * before any client of the class requires to use it, and that on
     * program exit the class's static resources are not cleaned up
     * prematurely (e.g. while the class is still in use by another
     * class's static members).  Devices such as this are necessitated
     * by the fact that the standard does not prescribe the order in
     * which objects of file and global scope in different compilation
     * units are initialized: it specifies only that the order of
     * destruction must be the reverse of the order of initialization.
     *
     * The necessary control is achieved by the unusual stratagem of
     * including the \e definition of a lightweight data item within
     * the header file of class \a T.  This data item is of type
     * SchwarzCounter<T>, and is declared within an anonymous
     * namespace.  Each file that <tt>\#include</tt>s this header file
     * will therefore include a definition of a SchwarzCounter object,
     * and this definition will precede any data definitions within
     * the enclosing file that depend on class \a T.  Consequently,
     * the SchwarzCounter object will be constructed before any data
     * objects of the client file.  The constructor of SchwarzCounter
     * is so defined that when the first such object is created, the
     * class MemoryBank will itself be initialized.
     *
     * Conversely, when the program exits, data items within each
     * client file will have their destructors invoked before the
     * file's SchwarzCounter object has its destructor invoked.  This
     * SchwarzCounter destructor is so defined that only when the last
     * SchwarzCounter object is destroyed is class \a T itself cleaned
     * up.
     *
     * @param T class whose initialization and clean-up are to be
     *          controlled by the Schwarz counter.  This class must
     *          defined static methods <tt>initialize()</tt> and
     *          <tt>cleanup()</tt>, capable of being invoked with no
     *          parameters.
     */
    template <class T>
    class SchwarzCounter
    {
    public:
        SchwarzCounter()
        {
            if (!s_count++)
                T::initialize();
        }

        ~SchwarzCounter()
        {
            if (!--s_count)
                T::cleanup();
        }

    private:
        static unsigned int s_count;
    };

    template <class T>
    unsigned int SchwarzCounter<T>::s_count = 0;
} // namespace CXXR

#endif // SCHWARZCOUNTER_HPP
