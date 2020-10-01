/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
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

/** @file MemoryBank.cpp
 *
 * Implementation of class MemoryBank
 */

#include <CXXR/MemoryBank.hpp>

#include <iostream>
#include <limits>
#include <iterator>

using namespace std;
using namespace R;

// If NO_CELLPOOLS is defined, all memory blocks are allocated
// directly via ::operator new.
#ifdef NO_CELLPOOLS
const size_t MemoryBank::s_new_threshold = 1;
#else
const size_t MemoryBank::s_new_threshold = 128;
#endif

size_t MemoryBank::s_blocks_allocated = 0;
size_t MemoryBank::s_bytes_allocated = 0;
bool (*MemoryBank::s_cue_gc)(size_t, bool) = nullptr;

void MemoryBank::pool_out_of_memory(CellPool* pool)
{
    if (s_cue_gc) s_cue_gc(pool->superblockSize(), false);
}

CellPool MemoryBank::s_pools[] = {CellPool(1, 512, pool_out_of_memory),
                                  CellPool(2, 256, pool_out_of_memory),
                                  CellPool(4, 128, pool_out_of_memory),
                                  CellPool(8, 64, pool_out_of_memory),
                                  CellPool(16, 32, pool_out_of_memory)};

// Note that the C++ standard requires that an operator new returns a
// valid pointer even when 0 bytes are requested.  The entry at
// s_pooltab[0] ensures this.  This table assumes sizeof(double) == 8.
const unsigned char MemoryBank::s_pooltab[]
= {0, 0, 0, 0, 0, 0, 0, 0, 0,
   1, 1, 1, 1, 1, 1, 1, 1,
   2, 2, 2, 2, 2, 2, 2, 2,
   2, 2, 2, 2, 2, 2, 2, 2,
   3, 3, 3, 3, 3, 3, 3, 3,
   3, 3, 3, 3, 3, 3, 3, 3,
   3, 3, 3, 3, 3, 3, 3, 3,
   3, 3, 3, 3, 3, 3, 3, 3,
   4, 4, 4, 4, 4, 4, 4, 4,
   4, 4, 4, 4, 4, 4, 4, 4,
   4, 4, 4, 4, 4, 4, 4, 4,
   4, 4, 4, 4, 4, 4, 4, 4,
   4, 4, 4, 4, 4, 4, 4, 4,
   4, 4, 4, 4, 4, 4, 4, 4,
   4, 4, 4, 4, 4, 4, 4, 4,
   4, 4, 4, 4, 4, 4, 4, 4};
    
void* MemoryBank::alloc2(size_t bytes)
{
    void* p = nullptr;
    bool joy = false;  // true if GC succeeds after bad_alloc
    try {
	if (bytes > s_new_threshold) {
	    if (s_cue_gc) s_cue_gc(bytes, false);
	    p = ::operator new(bytes);
	}
	else p = s_pools[s_pooltab[bytes]].allocate();
    }
    catch (bad_alloc) {
	if (s_cue_gc) {
	    // Try to force garbage collection if available:
	    size_t sought_bytes = bytes;
	    if (bytes < s_new_threshold)
		sought_bytes = s_pools[s_pooltab[bytes]].superblockSize();
	    joy = s_cue_gc(sought_bytes, true);
	}
	else throw;
    }
    if (!p && joy) {
	// Try once more:
	try {
	    if (bytes > s_new_threshold) p = ::operator new(bytes);
	    else p = s_pools[s_pooltab[bytes]].allocate();
	}
	catch (bad_alloc) {
	    throw;
	}
    }
    notifyAllocation(bytes);
    return p;
}

void* MemoryBank::allocate(size_t bytes)
{
    // notifyAllocation(bytes);
    void* p;
    // Assumes sizeof(double) == 8:
    return (bytes > s_new_threshold || !(p = alloc1(bytes)))
	? alloc2(bytes) : p;
}

void MemoryBank::check()
{
#ifndef NO_CELLPOOLS
    for (unsigned int i = 0; i < s_num_pools; ++i)
	s_pools[i].check();
#endif
}

void MemoryBank::notifyAllocation(size_t bytes)
{
    ++s_blocks_allocated;
    s_bytes_allocated += bytes;
}

void MemoryBank::notifyDeallocation(size_t bytes)
{
    s_bytes_allocated -= bytes;
    --s_blocks_allocated;
}

