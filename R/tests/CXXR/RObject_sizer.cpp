/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2007 Andrew Runnalls.
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
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street Fifth Floor, Boston, MA 02110-1301 USA
 */

#include <iostream>
#include <R_ext/Boolean.h>
#include <CXXR/RObject.hpp>
#include <CXXR/ListVector.hpp>

using namespace std;
using namespace CXXR;

// Stubs for members of GCNode:
unsigned int GCNode::SchwarzCtr::s_count = 0;

GCNode::SchwarzCtr::SchwarzCtr()
{
    if (!s_count++)
        GCNode::initialize();
}

GCNode::SchwarzCtr::~SchwarzCtr()
{
    if (!--s_count)
        GCNode::cleanup();
}

void GCNode::cleanup()
{
    cout << "GCNode::cleanup()\n";
}

void CXXR::GCNode::initialize()
{
    cout << "GCNode::initialize()\n";
}

int main()
{
    cout << "Size of various types used in R (in bytes):\n\n";
    cout << "sizeof(bool): " << sizeof(bool)
         << "\nsizeof(char): " << sizeof(char)
         << "\nsizeof(Rboolean): " << sizeof(Rboolean)
         << "\nsizeof(int): " << sizeof(int)
         << "\nsizeof(unsigned): " << sizeof(unsigned int)
         << "\nsizeof(R_len_t): " << sizeof(R_len_t)
         << "\nsizeof(size_t): " << sizeof(size_t)
         << "\nsizeof(R_xlen_t): " << sizeof(R_xlen_t)
         << "\nsizeof(double): " << sizeof(double)
         << "\nsizeof(RObject*): " << sizeof(RObject *)
         << "\nsizeof(ListVector*): " << sizeof(ListVector *)
         << "\nsizeof(ListVector): " << sizeof(ListVector)
         << "\nsizeof(RObject): " << sizeof(RObject) << "\n";
    return 0;
}
