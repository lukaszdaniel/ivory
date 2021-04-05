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
 * @brief Class CXXR::MemoryBank
 */

#ifndef MEMORYBANK_HPP
#define MEMORYBANK_HPP

#include <cstring>
#include <config.h>
#include <CXXR/RTypes.hpp>
#include <CXXR/CellPool.hpp>
#include <CXXR/SEXPTYPE.hpp>
#include <CXXR/SchwarzCounter.hpp>
#include <R_ext/Rallocators.h>

#ifdef __GNUC__
#ifdef __i386__
#define HOT_FUNCTION __attribute__((hot, fastcall))
#else
#define HOT_FUNCTION __attribute__((hot))
#endif
#else
#define HOT_FUNCTION
#endif

#define ALLOC_STATS // TODO(joqvist): Make this a configure option?

namespace CXXR
{
	/** @brief Class to manage memory allocation and deallocation for R.
     *
     * Small objects are quickly allocated from pools of various cell
     * sizes; large objects are obtained directly from the main heap.
     */
	class MemoryBank
	{
	public:
		/** @brief Allocate a block of memory.
		 *
		 * @param bytes Required size in bytes of the block.
		 *
		 * @param alloc_gc If false, the call will under no
		 *          circumstances cue garbage collection.
		 *
		 * @param allocator Pointer to the custom allocator.
		 *
		 * @return a pointer to the allocated cell.
		 *
		 * @throws bad_alloc if a cell cannot be allocated.
		 */
		static void *allocate(size_t bytes = 0, bool allow_gc = true, R_allocator_t *allocator = nullptr) HOT_FUNCTION;

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
		static void deallocate(void *p = nullptr, size_t bytes = 0, bool allocator = false)
		{
			if (!p)
				return;
			// Uncommenting this helps to diagnose premature GC:
			// memset(p, 0x55, bytes);
			if (allocator)
				custom_node_free(p);
			// Assumes sizeof(double) == 8:
			if (bytes >= s_new_threshold)
				::operator delete(p);
			else
				s_pools[s_pooltab[(bytes + 7) >> 3]].deallocate(p);
			notifyDeallocation(bytes);
		}

		/** @brief Reorganise lists of free cells.
		 *
		 * This is done with a view to increasing the probability that
		 * successive allocations will lie within the same cache line
		 * or (less importantly nowadays) memory page.
		 */
		static void defragment();

		/** @brief Set a callback to cue garbage collection.
		 *
		 * @param cue_gc This is a pointer, possibly null, to a
		 *          function that this class will call to cue garbage 
		 *          collection, either because the garbage collection
		 *          threshold has been exceeded, or because the class
		 *          has just failed to allocate memory from the main
		 *          heap.  The argument is set to the number of bytes
		 *          of memory currently being sought.  The function
		 *          should return the new value of the garbage
		 *          collection threshold.  If \a cue_gc is a null
		 *          pointer, then such callbacks are discontinued.
		 *
		 * @param initial_threshold The initial threshold for garbage
		 *          collection.  If garbage collection is allowed,
		 *          allocate() will call \a cue_gc when it looks as if
		 *          the number of bytes allocated via MemoryBank is
		 *          about to exceed the threshold.  The parameter is
		 *          ignored if \a cue_gc is a null pointer.
		 */
		static void setGCCuer(size_t (*cue_gc)(size_t), size_t initial_threshold);

#ifdef R_MEMORY_PROFILING
		/** Set a callback to monitor allocations exceeding a threshold size.
		 *
		 * @param monitor This is a pointer to a function that this
		 *          class will call when it allocates a block
		 *          exceeding a threshold size.  The function is
		 *          called with the first argument set to the number
		 *          of bytes allocated.  Alternatively, monitor can be
		 *          set to a null pointer to discontinue monitoring.
		 *
		 * @param threshold The monitor will only be called for
		 *          allocations of at least this many bytes.  Ignored
		 *          if monitor is a null pointer.
		 *
		 * @note This function is available only if R_MEMORY_PROFILING
		 * is defined, e.g. by specifying option
		 * --enable-memory-profiling to the configure script.
		 */
		static void setMonitor(void (*monitor)(size_t) = nullptr,
							   size_t threshold = 0);

		static void R_ReportAllocation(R_size_t size);
#endif
	private:
		typedef CellPool Pool;
		static const size_t s_num_pools = 10;
		// We use ::operator new directly for allocations at least this big:
		static const size_t s_new_threshold;
		static size_t s_blocks_allocated;
		static size_t s_bytes_allocated;
		static size_t s_gc_threshold;
		static size_t (*s_cue_gc)(size_t);
		static Pool *s_pools;
		static const unsigned char s_pooltab[];
#ifdef R_MEMORY_PROFILING
		static void (*s_monitor)(size_t);
		static size_t s_monitor_threshold;
#endif

		/* support for custom allocators that allow vectors to be allocated
		 using non-standard means such as COW mmap() */
		static void *custom_node_alloc(R_allocator_t *allocator, size_t bytes);
		static void custom_node_free(void *ptr);

		friend class GCNode;
		static void notifyAllocation(size_t bytes);

		static void notifyDeallocation(size_t bytes);

		friend class CachedString;
		template <typename, SEXPTYPE>
		friend class FixedVector;

		/** @brief Adjust the freed block statistics.
         *
         * This should be used when the entire size of a freed block is not
         * passed to the delete operator, causing inconsistent statistics for
         * free counts.
         *
         * @param original The previously logged freed block size.
         *
         * @param actual The actual size of the freed block.
         */
		static void adjustFreedSize(size_t original, size_t actual);

		static void adjustBytesAllocated(size_t bytes)
		{
			s_bytes_allocated += bytes;
		}

		// Free memory used by the static data members:
		static void cleanup();

		// Initialize the static data members:
		friend void initializeMemorySubsystem();
		static void initialize();

		friend class SchwarzCounter<MemoryBank>;
		MemoryBank() = delete;
	};
} // namespace CXXR

namespace
{
	CXXR::SchwarzCounter<CXXR::MemoryBank> memorybank_schwarz_ctr;
}

#endif /* MEMORYBANK_HPP */
