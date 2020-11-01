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

/** @file CellPool.cpp
 *
 * Implementation of class CellPool
 */

#include <CXXR/CellPool.hpp>

#ifndef __APPLE__
#include <features.h>
#endif

#include <algorithm>
#include <cstdlib>
#include <iostream>

using namespace std;
using namespace R;

CellPool::~CellPool()
{
    for (vector<void*>::iterator it = m_superblocks.begin();
	 it != m_superblocks.end(); ++it)
	::operator delete(*it);
}

bool CellPool::check() const
{
	unsigned int free_cells = 0;
	for (Cell *c = m_free_cells; c; c = c->m_next)
	{
		checkCell(c);
		++free_cells;
	}
	if (m_cells_allocated + free_cells != m_cells_per_superblock * m_superblocks.size())
	{
		cerr << "CellPool::check(): internal inconsistency\n";
		cerr << "cells allocated     = " << m_cells_allocated << "\n";
		cerr << "free cells          = " << free_cells << "\n";
		cerr << "cells per uperblock = " << m_cells_per_superblock << "\n";
		cerr << "superblocks size    = " << m_superblocks.size() << "\n";
		abort();
	}
	return true;
}

void CellPool::checkAllocatedCell(const void* p) const 
{
    checkCell(p);
    const Cell* cp = static_cast<const Cell*>(p);
    bool found = false;
    for (Cell* c = m_free_cells; !found && c; c = c->m_next)
	found = (c == cp);
    if (found) {
	cerr << "CellPool::checkCell : designated block is (already) free.\n";
	abort();
    }
}

void CellPool::checkCell(const void* p) const
{
    if (!p) return;
    const char* pc = static_cast<const char*>(p);
    bool found = false;
    for (vector<void*>::const_iterator it = m_superblocks.begin();
	 !found && it != m_superblocks.end(); ++it) {
	ptrdiff_t offset = pc - static_cast<const char*>(*it);
	if (offset >= 0 && offset < static_cast<long>(m_superblocksize)) {
	    found = true;
	    if (offset%m_cellsize != 0) {
		cerr << "CellPool::checkCell : designated block is misaligned\n";
		abort();
	    }
	}
    }
    if (!found) {
	cerr << "CellPool::checkCell : designated block doesn't belong to this CellPool\n";
	abort();
    }
}

void CellPool::seekMemory()
{
    if (m_out_of_cells) (*m_out_of_cells)(this);
    if (!m_free_cells) {
	char* superblock
	    = static_cast<char*>(::operator new(m_superblocksize));
	m_superblocks.push_back(superblock);
	// Initialise cells:
	{
	    ptrdiff_t offset = ptrdiff_t(m_superblocksize - m_cellsize);
	    Cell* next = nullptr;
	    while (offset >= 0) {
		next = new (superblock + offset) Cell(next);
#if VALGRIND_LEVEL > 1
		VALGRIND_MAKE_MEM_NOACCESS(next + 1, m_cellsize - sizeof(Cell));
#endif
		offset -= ptrdiff_t(m_cellsize);
	    }
	    m_free_cells = next;
	}
    }
}
