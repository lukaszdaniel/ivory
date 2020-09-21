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

/** @file MemoryBank.hpp
 *
 * @brief Class R::MemoryBank
 */

#ifndef MEMORYBANK_HPP
#define MEMORYBANK_HPP

#include <cstring>
#include <CXXR/CellPool.hpp>
#include <CXXR/SEXPTYPE.hpp>

namespace R {
    /** @brief Class to manage memory allocation and deallocation for R.
     * 
     * Small objects are quickly allocated from pools of various cell
     * sizes; large objects are obtained directly from the main heap.
     */
    class MemoryBank {
    public:
	/** @brief Allocate a block of memory.
	 *
	 * @param bytes Required size in bytes of the block.
	 *
	 * @return a pointer to the allocated cell.
	 *
	 * @throws bad_alloc if a cell cannot be allocated.
	 */
	static void* allocate(size_t bytes);

	/** @brief Number of blocks currently allocated.
	 *
	 * @return the number of blocks of memory currently allocated.
	 */
	static size_t blocksAllocated() { return s_blocks_allocated; }

	/** @brief Number of bytes currently allocated.
	 *
	 * @return the number of bytes of memory currently allocated.
	 *
	 * @note This refers to the total number of bytes \e requested
	 * in blocks that have been allocated but not subsequently
	 * deallocated.  Actual utilisation of memory in the main heap
	 * may be greater than this, possibly by as much as a factor
	 * of 2.
	 */
	static size_t bytesAllocated() { return s_bytes_allocated; }

	/** @brief Integrity check.
	 *
	 * Aborts the program with an error message if the class is
	 * found to be internally inconsistent.
	 */
	static void check();

	/** @brief Deallocate a block
	 *
	 * @param p Pointer to a block of memory previously allocated
	 *          by MemoryBank::allocate(), or a null pointer (in which
	 *          case method does nothing).
	 *
	 * @param bytes The number of bytes in the memory block,
	 *          i.e. the number of bytes requested in the
	 *          corresponding call to allocate().
	 */
	static void deallocate(void* p, size_t bytes)
	{
	    if (!p) return;
	    // Assumes sizeof(double) == 8:
	    if (bytes > s_new_threshold)
		::operator delete(p);
	    else s_pools[s_pooltab[bytes]].deallocate(p);
	    notifyDeallocation(bytes);
	}

	/** Set a callback to cue garbage collection.
	 *
	 * @param cue_gc This is a pointer to a function that this
	 *         class will call before it attempts to allocate
	 *         memory from the main heap (second argument set to
	 *         false), or has just failed to allocate memory from
	 *         the heap (second argument set to true).  The first
	 *         argument is the amount of memory sought (in bytes).
	 *         The function should return true iff a release of memory 
	 *         took place.
	 */
	static void setGCCuer(bool (*cue_gc)(size_t, bool) = 0)
	{
	    s_cue_gc = cue_gc;
	}
    private:
	typedef CellPool Pool;
	static const size_t s_num_pools = 5;
	// We use ::operator new directly for allocations at least this big:
	static const size_t s_new_threshold;
	static size_t s_blocks_allocated;
	static size_t s_bytes_allocated;
	static bool (*s_cue_gc)(size_t, bool);
	static CellPool s_pools[];
	static const unsigned char s_pooltab[];
	static void notifyAllocation(size_t bytes);

	static void notifyDeallocation(size_t bytes);
	// First-line allocation attempt for small objects:
	static void* alloc1(size_t bytes) throw()
	{
	    void* p = s_pools[s_pooltab[bytes]].easyAllocate();
	    if (p) {
		notifyAllocation(bytes);
	    }
	    return p;
	}

	// Allocation of large objects, and second-line allocation
	// attempt for small objects:
	static void* alloc2(size_t bytes);

	static void pool_out_of_memory(CellPool *pool);
	MemoryBank() = delete;
    };
}

#endif /* MEMORYBANK_HPP */
