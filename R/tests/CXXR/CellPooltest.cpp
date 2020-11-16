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

/** @file CellPooltest.cpp
 *
 * Test of class CXXR::CellPool
 */

#include <CXXR/CellPool.hpp>

#include <iostream>

using namespace std;
using namespace CXXR;

namespace
{
    double *dptrs[16];

    void out_of_memory(CellPool *);

    CellPool pool(1, 5, out_of_memory);

    void out_of_memory(CellPool * /*ignored*/)
    {
        cout << "Out of memory";
        if (dptrs[1])
        {
            cout << ": freeing dptrs[1]";
            pool.deallocate(dptrs[1]);
            dptrs[1] = 0;
        }
        cout << endl;
    }
} // namespace

int main()
{

    for (int i = 0; i < 16; ++i)
        dptrs[i] = 0;
    pool.check();
    cout << "Cell size: " << pool.cellSize()
         << "\nSuperblock size: " << pool.superblockSize() << endl;
    for (int i = 0; i < 10; ++i)
    {
        cout << "Allocating dptrs[" << i << "]\n";
        dptrs[i] = reinterpret_cast<double *>(pool.allocate());
    }
    pool.check();
    cout << "Cells allocated: " << pool.cellsAllocated() << endl;
    for (int i = 3; i < 10; i += 2)
    {
        cout << "Deallocating dptrs[" << i << "]\n";
        pool.deallocate(dptrs[i]);
    }
    pool.check();
    cout << "Cells allocated: " << pool.cellsAllocated() << endl;
    for (int i = 1;
         (dptrs[i] = reinterpret_cast<double *>(pool.easyAllocate()));
         i += 2)
        cout << "Allocated dptrs[" << i << "]\n";
    cout << "easyAllocate() failed\n";
    pool.check();
    for (int i = 11; i < 16; i += 2)
    {
        cout << "Allocating dptrs[" << i << "]\n";
        dptrs[i] = reinterpret_cast<double *>(pool.allocate());
    }
    pool.check();
    cout << "Cells allocated: " << pool.cellsAllocated() << endl;

    return 0;
}
