/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998-2007   The R Development Core Team.
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

/** @file GCManager.cpp
 *
 * Class GCManager and associated C-callable functions.
 */

#include <CXXR/GCManager.hpp>
#include <CXXR/RTypes.hpp>
#include <cmath>
#include <iomanip>
#include <iostream>
#include <limits>
#include <R_ext/Print.h>
#include <CXXR/GCNode.hpp>
#include <CXXR/WeakRef.hpp>
#include <Defn.h>
#include <Rinterface.h>

using namespace std;
using namespace CXXR;

constexpr int LEVEL_0_FREQ = 20;
constexpr int LEVEL_1_FREQ = 5;
const unsigned int GCManager::s_collect_counts_max[s_num_old_generations] = {LEVEL_0_FREQ, LEVEL_1_FREQ};
unsigned int GCManager::s_gen_gc_counts[s_num_old_generations + 1];

size_t GCManager::s_threshold = R_VSIZE;
size_t GCManager::s_max_threshold = R_SIZE_T_MAX;
size_t GCManager::s_node_threshold = R_NSIZE;
size_t GCManager::s_max_node_threshold = R_SIZE_T_MAX;
size_t GCManager::s_min_threshold = s_threshold;
size_t GCManager::s_min_node_threshold = s_node_threshold;
bool GCManager::s_gc_fail_on_error = false;
bool GCManager::s_gc_pending = false;
int GCManager::s_gc_force_wait = 0;
int GCManager::s_gc_force_gap = 0;
bool GCManager::s_gc_inhibit_release = false;

size_t GCManager::s_max_bytes = 0;
size_t GCManager::s_max_nodes = 0;
bool GCManager::s_tortured = false;
std::ostream *GCManager::s_os = nullptr;

void (*GCManager::s_pre_gc)() = nullptr;
void (*GCManager::s_post_gc)() = nullptr;

namespace
{
    unsigned int gc_count;
} // namespace

/* Tuning Constants. Most of these could be made settable from R,
   within some reasonable constraints at least.  Since there are quite
   a lot of constants it would probably make sense to put together
   several "packages" representing different space/speed tradeoffs
   (e.g. very aggressive freeing and small increments to conserve
   memory; much less frequent releasing and larger increments to
   increase speed). */

/* When a level N collection fails to produce at least R_MinFreeFrac *
   s_node_threshold free nodes and R_MinFreeFrac * s_threshold free vector space, the
   next collection will be a level N + 1 collection.

   This constant is also used in heap size adjustment as a minimal
   fraction of the minimal heap size levels that should be available
   for allocation. */
static const double R_MinFreeFrac = 0.2;

/* The heap size constants s_node_threshold and s_threshold are used for triggering
   collections.  The initial values set by defaults or command line
   arguments are used as minimal values.  After full collections these
   levels are adjusted up or down, though not below the minimal values
   or above the maximum values, towards maintain heap occupancy within
   a specified range.  When the number of nodes in use reaches
   R_NGrowFrac * s_node_threshold, the value of s_node_threshold is incremented by
   R_NGrowIncrMin + R_NGrowIncrFrac * s_node_threshold.  When the number of
   nodes in use falls below R_NShrinkFrac, s_node_threshold is decremented by
   R_NShrinkIncrMin + R_NShrinkFrac * s_node_threshold.  Analogous adjustments
   are made to s_threshold.

   This mechanism for adjusting the heap size constants is very
   primitive but hopefully adequate for now.  Some modeling and
   experimentation would be useful.  We want the heap sizes to get set
   at levels adequate for the current computations.  The present
   mechanism uses only the size of the current live heap to provide
   information about the current needs; since the current live heap
   size can be very volatile, the adjustment mechanism only makes
   gradual adjustments.  A more sophisticated strategy would use more
   of the live heap history.

   Some of the settings can now be adjusted by environment variables.
*/

double GCManager::R_NGrowFrac = 0.70;
double GCManager::R_NShrinkFrac = 0.30;

double GCManager::R_VGrowFrac = 0.70;
double GCManager::R_VShrinkFrac = 0.30;

#ifdef SMALL_MEMORY
/* On machines with only 32M of memory (or on a classic Mac OS port)
   it might be a good idea to use settings like these that are more
   aggressive at keeping memory usage down. */
double GCManager::R_NGrowIncrFrac = 0.0, GCManager::R_NShrinkIncrFrac = 0.2;
int GCManager::R_NGrowIncrMin = 50000, GCManager::R_NShrinkIncrMin = 0;
double GCManager::R_VGrowIncrFrac = 0.0, GCManager::R_VShrinkIncrFrac = 0.2;
int GCManager::R_VGrowIncrMin = 100000, GCManager::R_VShrinkIncrMin = 0;
#else
double GCManager::R_NGrowIncrFrac = 0.2, GCManager::R_NShrinkIncrFrac = 0.2;
int GCManager::R_NGrowIncrMin = 40000, GCManager::R_NShrinkIncrMin = 0;
double GCManager::R_VGrowIncrFrac = 0.2, GCManager::R_VShrinkIncrFrac = 0.2;
int GCManager::R_VGrowIncrMin = 80000, GCManager::R_VShrinkIncrMin = 0;
#endif
namespace
{

#ifdef DEBUG_GC
    // This ought to go in GCNode.
    void DEBUG_GC_SUMMARY(int full_gc)
    {
        int OldCount;
        if (full_gc)
            REprintf("\nFull, VSize = %lu", MemoryBank::bytesAllocated());
        else
            REprintf("\nMinor, VSize = %lu", MemoryBank::bytesAllocated());
        for (unsigned int gen = 0, OldCount = 0; gen < s_num_old_generations; ++gen)
            OldCount += GCNode::s_gencount[gen];
        REprintf(", %d", OldCount);
    }
#else
#define DEBUG_GC_SUMMARY(x)
#endif /* DEBUG_GC */
} // namespace

/* Heap Size Adjustment. */
void GCManager::adjustThreshold(R_size_t bytes_wanted)
{
    size_t R_MinVFree = size_t(s_min_threshold * R_MinFreeFrac);
    size_t VNeeded = MemoryBank::bytesAllocated() + bytes_wanted + R_MinVFree;
    size_t R_MinNFree = size_t(s_min_node_threshold * R_MinFreeFrac);
    size_t NNeeded = GCNode::numNodes() + R_MinNFree;

    double vect_occup = ((double)VNeeded) / s_threshold;
    double node_occup = ((double)NNeeded) / s_node_threshold;

    if (node_occup > R_NGrowFrac)
    {
        size_t change =
            size_t(R_NGrowIncrMin + R_NGrowIncrFrac * s_node_threshold);

        /* for early andjustments grow more agressively */
        static R_size_t last_in_use = 0;
        static int adjust_count = 1;
        if (adjust_count < 50)
        {
            adjust_count++;

            /* estimate next in-use count by assuming linear growth */
            R_size_t next_in_use = GCNode::numNodes() + (GCNode::numNodes() - last_in_use);
            last_in_use = GCNode::numNodes();

            /* try to achieve and occupancy rate of R_NGrowFrac */
            R_size_t next_nsize = (R_size_t)(next_in_use / R_NGrowFrac);
            if (next_nsize > s_node_threshold + change)
                change = next_nsize - s_node_threshold;
        }

        if (s_max_node_threshold >= s_node_threshold + change)
            s_node_threshold += change;
    }
    else if (node_occup < R_NShrinkFrac)
    {
        s_node_threshold -= (R_size_t)(R_NShrinkIncrMin + R_NShrinkIncrFrac * s_node_threshold);
        if (s_node_threshold < NNeeded)
            s_node_threshold = min(NNeeded, s_max_node_threshold);
        s_node_threshold = max(s_node_threshold, s_min_node_threshold);
    }

    if (vect_occup > 1.0 && VNeeded < s_max_threshold)
    {
        s_threshold = VNeeded;
    }
    else if (vect_occup > R_VGrowFrac)
    {
        size_t change = size_t(R_VGrowIncrMin + R_VGrowIncrFrac * s_threshold);
        if (s_max_threshold >= change + s_threshold)
            s_threshold += change;
    }
    else if (vect_occup < R_VShrinkFrac)
    {
        s_threshold = size_t(s_threshold - R_VShrinkIncrMin - R_VShrinkIncrFrac * s_threshold);
        s_threshold = max(VNeeded, s_threshold);
        s_threshold = max(s_threshold, s_min_threshold);
    }
#ifdef DEBUG_ADJUST_HEAP
    if (s_os)
    {
        *s_os << "Node occupancy: " << fixed << setprecision(0) << 100.0 * node_occup << "\n";
        *s_os << "Memory occupancy: " << fixed << setprecision(0) << 100.0 * vect_occup << "\n";
        *s_os << "Total allocation: " << MemoryBank::bytesAllocated() << "\n";
        *s_os << "Node threshold: " << s_node_threshold << "\n";
        *s_os << "Memory threshold: " << s_threshold << "\n";
        *s_os << "Nodes needed: " << NNeeded << "\n";
        *s_os << "Bytes needed: " << VNeeded << endl;
    }
#endif
}

bool GCManager::FORCE_GC()
{
    if (s_gc_pending)
    {
        return true;
    }
    else if (s_gc_force_wait > 0)
    {
        --s_gc_force_wait;
        if (s_gc_force_wait > 0)
        {
            return false;
        }
        else
        {
            s_gc_force_wait = s_gc_force_gap;
            return true;
        }
    }
    return false;
}

void GCManager::setTortureParameters(int gap, int wait, bool inhibitor)
{
    s_gc_force_wait = wait;
    s_gc_force_gap = gap;
    setInhibitor(inhibitor);
}

void GCManager::setInhibitor(bool inhibitor)
{
    s_gc_inhibit_release = inhibitor;
}

int GCManager::gc_force_wait()
{
    return s_gc_force_wait;
}

int GCManager::gc_force_gap()
{
    return s_gc_force_gap;
}

bool GCManager::gc_inhibit_release()
{
    return s_gc_inhibit_release;
}

bool GCManager::cue(size_t bytes_wanted, bool force)
{
    if (force || FORCE_GC() || /*(GCNode::numNodes() >= s_node_threshold) ||*/ (s_threshold < bytes_wanted + MemoryBank::bytesAllocated()))
    {
        gc(bytes_wanted, false);
        return true;
    }

    return false;
}

void GCManager::gc_error(const char *msg)
{
    if (s_gc_fail_on_error)
        R_Suicide(msg);
    else if (R_in_gc)
        REprintf(msg);
    else
        Rf_error(msg);
}

void GCManager::enableGC(size_t initial_threshold, size_t initial_node_threshold)
{
    s_min_threshold = s_threshold = initial_threshold;
    s_min_node_threshold = s_node_threshold = initial_node_threshold;
    gc_count = 0;
    for (unsigned int i = 0; i <= s_num_old_generations; ++i)
        s_gen_gc_counts[i] = 0;
    MemoryBank::setGCCuer(cue);
}

void GCManager::gc(size_t bytes_wanted, bool full)
{
    static bool running_finalizers = false;
    // Prevent recursion:
    if (running_finalizers)
        return;

    R_CHECK_THREAD;
    if (!R_GCEnabled || R_in_gc)
    {
        if (R_in_gc)
            gc_error("*** recursive gc invocation\n");
        // if (GCNode::numNodes() >= s_node_threshold)
        //     GCManager::s_node_threshold = GCNode::numNodes() + 1;

        if (bytes_wanted + MemoryBank::bytesAllocated() > s_threshold)
        {
            R_size_t expand = bytes_wanted - s_threshold + MemoryBank::bytesAllocated();
            if (s_threshold + expand > s_max_threshold)
            {
                Rf_errorcall(nullptr, "vector memory exhausted (limit reached?)");
            }
            s_threshold += expand;
        }

        s_gc_pending = true;
        return;
    }
    s_gc_pending = false;

    gcGenController(bytes_wanted, full);

#ifdef IMMEDIATE_FINALIZERS
    running_finalizers = true;
    bool any_finalizers_run = WeakRef::runFinalizers();
    running_finalizers = false;
    /* Run any eligible finalizers.  The return result of
	   RunFinalizers is TRUE if any finalizers are actually run.
	   There is a small chance that running finalizers here may
	   chew up enough memory to make another immediate collection
	   necessary.  If so, we jump back to the beginning and run
	   the collection, but on this second pass we do not run
	   finalizers. */
    if (any_finalizers_run && (/*(GCNode::numNodes() >= s_node_threshold) ||*/ (bytes_wanted + MemoryBank::bytesAllocated() > s_threshold)))
        gcGenController(bytes_wanted, full);

#endif
}

void GCManager::gcGenController(size_t bytes_wanted, bool full)
{
    static unsigned int level = 0;
    double ncells, nfrac;
    if (full)
        level = s_num_old_generations;
    level = genRota(level);

    unsigned int gens_collected;

    gc_count++;

    s_max_nodes = max(s_max_nodes, GCNode::numNodes());
    s_max_bytes = max(s_max_bytes, MemoryBank::bytesAllocated());

    /* BEGIN_SUSPEND_INTERRUPTS { */
    R_in_gc = true;
    if (s_pre_gc)
        (*s_pre_gc)();

    bool ok = false;
    while (!ok)
    {
        ok = true;
        GCNode::gc(level);
        gens_collected = level;

        /* update heap statistics */
        if (level < s_num_old_generations)
        {
            if (/*GCNode::numNodes() ||*/
                MemoryBank::bytesAllocated() + bytes_wanted > (1.0 - R_MinFreeFrac) * s_threshold)
            {
                level++;
                if (GCNode::numNodes() >= s_node_threshold || MemoryBank::bytesAllocated() + bytes_wanted >= s_threshold)
                    ok = false;
            }
            else
                level = 0;
        }
        else
            level = 0;
    }

    s_gen_gc_counts[gens_collected]++;

    if (gens_collected == s_num_old_generations)
    {
        /**** do some adjustment for intermediate collections? */
        adjustThreshold(bytes_wanted);
    }
    if (s_post_gc)
        (*s_post_gc)();
    R_in_gc = false;
    /* } END_SUSPEND_INTERRUPTS; */

    if (R_check_constants > 2 ||
        (R_check_constants > 1 && gens_collected == s_num_old_generations))
        R_checkConstants(TRUE);

    if (s_os)
    {
        *s_os << "Garbage collection " << gc_count << " = " << s_gen_gc_counts[0];
        for (unsigned int i = 0; i < s_num_old_generations; ++i)
            *s_os << "+" << s_gen_gc_counts[i + 1];
        *s_os << " (level " << gens_collected << ") ... ";
        DEBUG_GC_SUMMARY(gens_collected == s_num_old_generations);

        ncells = GCNode::numNodes();
        nfrac = (100.0 * ncells) / s_node_threshold;
        /* We try to make this consistent with the results returned by gc */
        ncells = 0.1 * ceil(10.0 * ncells * sizeof(RObject) / Mega);
        *s_os << "\n"
              << std::fixed << std::setprecision(1) << ncells << " Mbytes of cons cells used (" << int(nfrac + 0.5) << "%)";
        double bytes = MemoryBank::bytesAllocated();
        double bfrac = (100.0 * bytes) / s_threshold;
        double mbytes = 0.1 * ceil(10.0 * bytes / Mega);
        *s_os << "\n"
              << std::fixed << std::setprecision(1) << mbytes << " Mbytes used (" << int(bfrac + 0.5) << "%)\n";
    }
}

unsigned int GCManager::genRota(unsigned int minlevel)
{
    static unsigned int collect_counts[s_num_old_generations];
    unsigned int level = minlevel;
    for (unsigned int i = 0; i < level; ++i)
        collect_counts[i] = 0;
    while (level < s_num_old_generations && ++collect_counts[level] > s_collect_counts_max[level])
    {
        collect_counts[level] = 0;
        ++level;
    }
    return level;
}

void GCManager::resetMaxTallies()
{
    s_max_bytes = MemoryBank::bytesAllocated();
    s_max_nodes = GCNode::numNodes();
}

std::ostream *GCManager::setReporting(std::ostream *os)
{
    std::ostream *ans = s_os;
    s_os = os;
    return ans;
}
