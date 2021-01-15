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
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street Fifth Floor, Boston, MA 02110-1301 USA
 */

/** @file CellHeap.cpp
 *
 * Implementation of class CellHeap
 */

#include <CXXR/CellHeap.hpp>

// For posix_memalign:
#ifndef __APPLE__
#define HAVE_POSIX_MEMALIGN
#endif

#ifndef __APPLE__
#include <features.h>
#endif

#include <algorithm>
#include <cstdlib>
#include <iostream>

using namespace std;
using namespace CXXR;

CellHeap::~CellHeap()
{
#if VALGRIND_LEVEL >= 2
	VALGRIND_DESTROY_MEMPOOL(this);
#endif
	for (vector<void *>::iterator it = m_superblocks.begin();
		 it != m_superblocks.end(); ++it)
		free(*it);
}

bool CellHeap::check() const
{
	unsigned int free_cells = countFreeCells(m_free_cells);
	if (m_cells_allocated + free_cells != m_cells_per_superblock * m_superblocks.size())
	{
		cerr << "CellHeap::check(): internal inconsistency\n";
		cerr << "cells allocated     = " << m_cells_allocated << "\n";
		cerr << "free cells          = " << free_cells << "\n";
		cerr << "cells per uperblock = " << m_cells_per_superblock << "\n";
		cerr << "superblocks size    = " << m_superblocks.size() << "\n";
		abort();
	}
	return true;
}

void CellHeap::checkAllocatedCell(const void *p) const
{
	checkCell(p);
	const Cell *cp = static_cast<const Cell *>(p);
	if (isFreeCell(m_free_cells, cp))
	{
		cerr << "CellHeap::checkCell : designated block is (already) free.\n";
		abort();
	}
}

void CellHeap::checkCell(const void *p) const
{
	if (!p)
		return;
	const char *pc = static_cast<const char *>(p);
	bool found = false;
	for (vector<void *>::const_iterator it = m_superblocks.begin();
		 !found && it != m_superblocks.end(); ++it)
	{
		ptrdiff_t offset = pc - static_cast<const char *>(*it);
		if (offset >= 0 && offset < static_cast<long>(m_superblocksize))
		{
			found = true;
			if (offset % m_cellsize != 0)
			{
				cerr << "CellHeap::checkCell : designated block is misaligned\n";
				abort();
			}
		}
	}
	if (!found)
	{
		cerr << "CellHeap::checkCell : designated block doesn't belong to this CellHeap\n";
		abort();
	}
	const Cell *c = reinterpret_cast<const Cell *>(p);
	if ((c->m_l && c->m_l < c) || (c->m_r && c->m_r < c))
	{
		cerr << "CellHeap::checkCell : child with lower address than parent.\n";
		abort();
	}
}

unsigned int CellHeap::countFreeCells(const Cell *root)
{
	if (!root)
		return 0;
	unsigned int ans = 1;
	if (root->m_l)
	{
		if (root >= root->m_l)
			abort();
		ans += countFreeCells(root->m_l);
	}
	if (root->m_r)
	{
		if (!root->m_l)
			abort();
		if (root >= root->m_r)
			abort();
		ans += countFreeCells(root->m_r);
	}
	return ans;
}

// Having this inlined was causing errors when optimising at -O2 with
// gcc (4.1.2 and 4.2.1) leading up to 2008/07/17.
void CellHeap::deallocate(void *p)
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
	check();
	Cell *c = new (p) Cell;
	m_free_cells = meld(c, m_free_cells);
	--m_cells_allocated;
	check();
}

bool CellHeap::isFreeCell(const Cell *root, const Cell *c)
{
	if (!root || !c)
		return false;
	return c == root || isFreeCell(root->m_l, c) || isFreeCell(root->m_r, c);
}

void CellHeap::meld_aux(Cell *host, Cell *guest)
{
	swap(host->m_l, host->m_r);
	while (host->m_l)
	{
		// if (host >= host->m_l) abort();
		// if (host->m_r && host >= host->m_r) abort();
		// if (guest->m_l && guest >= guest->m_l) abort();
		// if (guest->m_r && guest >= guest->m_r) abort();
		if (guest < host->m_l)
			swap(host->m_l, guest);
		host = host->m_l;
		swap(host->m_l, host->m_r);
	}
	host->m_l = guest;
}

void CellHeap::seekMemory()
{
	if (m_out_of_cells)
		(*m_out_of_cells)(this);
	if (!m_free_cells)
	{
#ifdef HAVE_POSIX_MEMALIGN
		void *memblock;
		// We assume the memory page size is some multiple of 4096 bytes:
		if (0 != posix_memalign(&memblock, 4096, m_superblocksize))
		{
			cerr << "Unable to allocate CellHeap memory.\n";
			abort();
		}
		char *superblock = reinterpret_cast<char *>(memblock);
#else
		char *superblock = reinterpret_cast<char *>(malloc(m_superblocksize));
		if (!superblock)
		{
			cerr << "Unable to allocate CellHeap memory.\n";
			abort();
		}
#endif
		//	cout << "Superblock at " << memblock << " for cell size "
		//     << m_cellsize << endl;
		m_superblocks.push_back(superblock);
		// Initialise cells:
		{
			ptrdiff_t offset = ptrdiff_t(m_superblocksize - m_cellsize);
			Cell *next = nullptr;
			while (offset >= 0)
			{
				next = new (superblock + offset) Cell(next);
				// cout << "Cell created at " << next << "\n";
#if VALGRIND_LEVEL >= 2
				VALGRIND_MAKE_MEM_NOACCESS(next + 1, m_cellsize - sizeof(Cell));
#endif
				offset -= ptrdiff_t(m_cellsize);
			}
			m_free_cells = next;
		}
	}
}
