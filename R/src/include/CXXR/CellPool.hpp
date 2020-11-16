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

/** @file CellPool.hpp
 *
 * @brief Class CXXR::CellPool.
 */

#ifndef CELLPOOL_HPP
#define CELLPOOL_HPP

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cstddef>
#include <new>
#include <vector>

// TODO: Similar predefines also in memory.cpp
// TODO: Move to a separate header.
#ifndef VALGRIND_LEVEL
#define VALGRIND_LEVEL 0
#endif

#ifndef NVALGRIND
#ifdef HAVE_VALGRIND_MEMCHECK_H
#include "valgrind/memcheck.h"
#else
// internal version of headers
#include "vg/memcheck.h"
#endif
#endif

namespace CXXR
{
	/** @brief Class to manage a pool of memory cells of a fixed size.
     * 
     * This class, based closely on Item 10 of Scott Meyers' 'Effective
     * C++ (2nd edition)' manages a collection of memory cells of a
     * specified size, and is intended as a back-end to implementations of
     * operator new and operator delete to enable the allocation and
     * deallocation of small objects quickly.
     */
	class CellPool
	{
	public:
		/**
		 * @param dbls_per_cell (must be >= 1). Size of cells,
		 *         expressed as a multiple of sizeof(double).  For
		 *         example, if you require cells large enough to
		 *         contain one double, put dbls_per_cell as 1.  (NB:
		 *         cells can contain anything, not just doubles; we
		 *         work in doubles because these are likely to have
		 *         the most stringent address alignment requirements.)
		 *
		 * @param cells_per_superblock (must be >= 1).  Memory for cells is
		 *         obtained from the main heap in 'superblocks'
		 *         sufficient to contain this many cells.
		 *
		 * @param out_of_cells This function (if specified) is called
		 *         by a CellPool when an allocation attempt finds that
		 *         there are no cells available within the currently
		 *         allocated superblocks; the function's argument is set to
		 *         point to the CellPool concerned.  The function may
		 *         for example initiate garbage collection.  If when
		 *         this function returns there are still no free
		 *         cells, only then will the CellPool allocate a new
		 *         superblock.
		 */
		CellPool(size_t dbls_per_cell, size_t cells_per_superblock,
				 void (*out_of_cells)(CellPool *) = 0)
			: m_cellsize(dbls_per_cell * sizeof(double)),
			  m_cells_per_superblock(cells_per_superblock),
			  m_superblocksize(m_cellsize * cells_per_superblock),
			  m_out_of_cells(out_of_cells),
			  m_free_cells(0),
			  m_cells_allocated(0)
		{
#if VALGRIND_LEVEL >= 2
			VALGRIND_CREATE_MEMPOOL(this, 0, 0);
#endif
		}

		/** Destructor
		 *
		 * It is up to the user to check that any cells allocated from
		 * the pool have been freed before this destructor is
		 * invoked.  (Although the destructor could check this for
		 * itself and issue an error message, this message would
		 * probably be a nuisance if it occurred during program shutdown.)
		 */
		~CellPool();

		/**
		 * @brief Allocate a cell from the pool.
		 *
		 * @return a pointer to the allocated cell.
		 *
		 * @throws bad_alloc if a cell cannot be allocated.
		 */
		void *allocate()
		{
			if (!m_free_cells)
				seekMemory();
			Cell *c = m_free_cells;
			m_free_cells = c->m_next;
			++m_cells_allocated;
#if VALGRIND_LEVEL >= 2
			VALGRIND_MEMPOOL_ALLOC(this, c, cellSize());
#endif
			return c;
		}

		/** @brief Size of cells.
		 *
		 * @return the size of each cell in bytes (well, strictly as a
		 * multiple of sizeof(char)).
		 */
		size_t cellSize() const
		{
			return m_cellsize;
		}

		/** @brief Number of cells allocated from this CellPool.
		 *
		 * @return the number of cells currently allocated from this
		 * pool.
		 */
		auto cellsAllocated() const
		{
			return m_cells_allocated;
		}

		/** @brief Integrity check.
		 *
		 * Aborts the program with an error message if the object is
		 * found to be internally inconsistent.
		 *
		 * @return true, if it returns at all.  The return value is to
		 * facilitate use with \c assert .
		 */
		bool check() const;

		/** @brief Deallocate a cell
		 *
		 * @param p Pointer to a block of memory previously allocated
		 * from this pool, or a null pointer (in which case method
		 * does nothing).
		 */
		void deallocate(void *p)
		{
			if (!p)
				return;
#ifdef DEBUG_RELEASE_MEM
			checkAllocatedCell(p);
#endif
#if VALGRIND_LEVEL >= 2
			VALGRIND_MEMPOOL_FREE(this, p);
			VALGRIND_MAKE_MEM_UNDEFINED(p, sizeof(Cell));
#endif
			Cell *c = reinterpret_cast<Cell *>(p);
			c->m_next = m_free_cells;
			m_free_cells = c;
			--m_cells_allocated;
		}

		/**
		 * Allocate a cell from the pool, provided it can be allocated
		 * 'from stock'.  Can be useful when called from other inlined
		 * functions in that it doesn't throw any exceptions.
		 *
		 * @return a pointer to the allocated cell, or 0 if the cell
		 * cannot be allocated from the current memory superblocks.
		 */
		void *easyAllocate() throw()
		{
			if (!m_free_cells)
				return nullptr;
			Cell *c = m_free_cells;
			m_free_cells = c->m_next;
			++m_cells_allocated;
#if VALGRIND_LEVEL >= 2
			VALGRIND_MEMPOOL_ALLOC(this, c, cellSize());
#endif
			return c;
		}

		/**
		 * @return The size in bytes of the superblocks from which
		 *         cells are allocated.
		 */
		auto superblockSize() const
		{
			return m_superblocksize;
		}

	private:
		struct Cell
		{
			Cell *m_next;

			Cell(Cell *next = nullptr) : m_next(next) {}
		};

		const size_t m_cellsize;
		const size_t m_cells_per_superblock;
		const size_t m_superblocksize;
		void (*m_out_of_cells)(CellPool *);
		std::vector<void *> m_superblocks;
		Cell *m_free_cells;
		size_t m_cells_allocated;

		// Checks that p is either null or points to a cell belonging
		// to this pool; aborts if not.
		void checkCell(const void *p) const;

		// Calls checkCell, and further checks that the cell is not on
		// the free list:
		void checkAllocatedCell(const void *p) const;

		void seekMemory();
	};
} // namespace CXXR

#endif /* CELLPOOL_HPP */
