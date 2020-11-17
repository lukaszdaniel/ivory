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

/** @file MemoryBank.cpp
 *
 * Implementation of class MemoryBank
 */

#include <CXXR/MemoryBank.hpp>

#include <iostream>
#include <limits>
#include <iterator>

#ifdef R_MEMORY_PROFILING
#include <limits>
#endif

using namespace std;
using namespace CXXR;

unsigned int MemoryBank::SchwarzCtr::s_count = 0;
size_t MemoryBank::s_blocks_allocated = 0;
size_t MemoryBank::s_bytes_allocated = 0;
bool (*MemoryBank::s_cue_gc)(size_t, bool) = nullptr;
#ifdef R_MEMORY_PROFILING
void (*MemoryBank::s_monitor)(size_t) = 0;
size_t MemoryBank::s_monitor_threshold = numeric_limits<size_t>::max();
#endif

void MemoryBank::pool_out_of_memory(CellPool *pool)
{
    if (s_cue_gc)
        s_cue_gc(pool->superblockSize(), false);
}

CellPool *MemoryBank::s_pools[5];

// Note that the C++ standard requires that an operator new returns a
// valid pointer even when 0 bytes are requested.  The entry at
// s_pooltab[0] ensures this.  This table assumes sizeof(double) == 8.
unsigned int MemoryBank::s_pooltab[] = {0, 0, 0, 0, 0, 0, 0, 0, 0,
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

void *MemoryBank::alloc2(size_t bytes)
{
    CellPool *pool = nullptr;
    void *p = nullptr;
    bool joy = false; // true if GC succeeds after bad_alloc
    try
    {
        if (bytes > s_max_cell_size)
        {
            if (s_cue_gc)
                s_cue_gc(bytes, false);
            p = ::operator new(bytes);
        }
        else
        {
            pool = s_pools[s_pooltab[bytes]];
            p = pool->allocate();
        }
    }
    catch (bad_alloc)
    {
        if (s_cue_gc)
        {
            // Try to force garbage collection if available:
            size_t sought_bytes = (pool ? pool->superblockSize() : bytes);
            joy = s_cue_gc(sought_bytes, true);
        }
        else
            throw;
    }
    if (!p && joy)
    {
        // Try once more:
        p = (pool ? pool->allocate() : ::operator new(bytes));
    }
    notifyAllocation(bytes);
#if VALGRIND_LEVEL >= 2
    if (pool)
    {
        // Fence off supernumerary bytes:
        size_t surplus = pool->cellSize() - bytes;
        if (surplus > 0)
        {
            char *tail = reinterpret_cast<char *>(p) + bytes;
            VALGRIND_MAKE_MEM_NOACCESS(tail, surplus);
        }
    }
#endif
    return p;
}

void MemoryBank::check()
{
    for (unsigned int i = 0; i < 5; ++i)
        s_pools[i]->check();
}

// Deleting heap objects on program exit is not strictly necessary,
// but doing so makes bugs more conspicuous when using valgrind.
void MemoryBank::cleanup()
{
    for (unsigned int i = 0; i < 5; ++i)
        delete s_pools[i];
}

void MemoryBank::initialize()
{
    s_pools[0] = new CellPool(1, 512, pool_out_of_memory);
    s_pools[1] = new CellPool(2, 256, pool_out_of_memory);
    s_pools[2] = new CellPool(4, 128, pool_out_of_memory);
    s_pools[3] = new CellPool(8, 64, pool_out_of_memory);
    s_pools[4] = new CellPool(16, 32, pool_out_of_memory);
}

void MemoryBank::notifyAllocation(size_t bytes)
{
#ifdef R_MEMORY_PROFILING
    if (s_monitor && bytes >= s_monitor_threshold)
        s_monitor(bytes);
#endif
    ++s_blocks_allocated;
    s_bytes_allocated += bytes;
}

void MemoryBank::notifyDeallocation(size_t bytes)
{
    s_bytes_allocated -= bytes;
    --s_blocks_allocated;
}

#ifdef R_MEMORY_PROFILING
void MemoryBank::setMonitor(void (*monitor)(size_t), size_t threshold)
{
    s_monitor = monitor;
    s_monitor_threshold = (monitor ? threshold : numeric_limits<size_t>::max());
}
#endif

void *MemoryBank::custom_node_alloc(R_allocator_t *allocator, size_t bytes)
{
    if (!allocator || !allocator->mem_alloc)
        return nullptr;
    void *ptr = allocator->mem_alloc(allocator, bytes + sizeof(R_allocator_t));
    if (ptr)
    {
        notifyAllocation(bytes);
        R_allocator_t *ca = (R_allocator_t *)ptr;
        *ca = *allocator;
        return (void *)(ca + 1);
    }
    return nullptr;
}

void MemoryBank::custom_node_free(void *ptr)
{
    if (ptr)
    {
        R_allocator_t *allocator = ((R_allocator_t *)ptr) - 1;
        allocator->mem_free(allocator, (void *)allocator);
    }
}