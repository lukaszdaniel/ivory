/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2006   The R Development Core Team.
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

/** @file GCManager.hpp
 * @brief Class CXXR::GCManager.
 *
 * @todo Reinstate garbage collection timing.
 * @todo Update DEBUG_GC_SUMMARY etc.
 */

#ifndef GCMANAGER_HPP
#define GCMANAGER_HPP

#include <cstddef>
#include <iosfwd>
#include <CXXR/RTypes.hpp>
#include <CXXR/MemoryBank.hpp>

namespace CXXR
{
	/** @brief Class for managing garbage collection.
	 *
	 * This class only has static members.  When CXXR::MemoryBank indicates
	 * that it is on the point of requesting additional memory from
	 * the operating system, the class decides whether to initiate a
	 * garbage collection, and if so how many levels to collect.
	 *
	 * In the current implementation of GCManager, when cued by R
	 * as above, a garbage collection will be carried out if the
	 * number of bytes currently allocated via CXXR::MemoryBank is at least
	 * as great as a threshold value.  This threshold value varies
	 * during the run, subject to a minimum value specified in the
	 * enableGC() method.
	 */
	class GCManager
	{
	public:
		/** @brief Not for general use.
		 *
		 * All garbage collection will be inhibited while any object
		 * of this type exists.
		 *
		 * @deprecated This class is provided for use in implementing
		 * functions (such as SET_ATTRIB()) in the Rinternals.h
		 * interface which would not give rise to any memory
		 * allocations as implemented in CR but may do so as
		 * implemented in rho.  It is also used within the GCNode
		 * class to handle reentrant calls to gclite() and gc().  Its
		 * use for other purposes is deprecated: use instead more
		 * selective protection against garbage collection such as
		 * that provided by class GCStackRoot<T>.
		 *
		 * @note GC inhibition is implemented as an object type to
		 * facilitate reinstatement of garbage collection when an
		 * exception is thrown.
		 */
		struct GCInhibitor
		{
			GCInhibitor()
			{
				++s_inhibitor_count;
			}

			~GCInhibitor()
			{
				--s_inhibitor_count;
			}

			/** @brief Is inhibition currently in effect?
			 *
			 * @return true iff garbage collection is currently inhibited.
			 */
			static bool active()
			{
				return s_inhibitor_count != 0;
			}
		};

		/** @brief Adjust the garbage collection threshold.
		 *
		 *  Adjust the garbage collection threshold in the light of
		 *  current allocations, and the space demand currently being
		 *  addressed.
		 *
		 * @param bytes_wanted If specified, the number of bytes
		 *          currently being sought by CXXR::MemoryBank.
		 */
		static void adjustThreshold(R_size_t bytes_wanted = 0);

		/** @brief Initiate a garbage collection.
		 *
		 * Note that enableGC() must have been called before this
		 * method is used.
		 *
		 * @param bytes_wanted Bytes required to be freed.
		 *
		 * @param full If this is true, a garbage collection of all
		 *          generations of nodes is forced.  Otherwise
		 *          GCManager decides for itself how many generations
		 *          should be collected.
		 */
		static void gc(size_t bytes_wanted, bool full = false);

		/** @brief Enable garbage collection.
		 *
		 * No automatic garbage collection of GCNode objects will take
		 * place until this method has been called; nor may a
		 * collection may be initiated 'manually' by calling gc().
		 * The effect of calling enableGC() more than once during a
		 * single program run is undefined.
		 *
		 * @param initial_threshold  Initial value for the collection
		 *          threshold.  The threshold will never be made less
		 *          than this value during the run.
		 *
		 * @param initial_node_threshold Initial value for the node collection
		 *          threshold.  The threshold will never be made less
		 *          than this value during the run.
		 */
		static void enableGC(size_t initial_threshold, size_t initial_node_threshold);

		/**
		 * @return true iff garbage collection torture is enabled.
		 */
		static bool isTortured() { return s_tortured; }

		/** @brief Maximum number of bytes used.
		 *
		 * @return the maximum number of bytes used (up to the time of
		 *         the most recent garbage collection.)
		 */
		static size_t maxBytes() { return s_max_bytes; }

		/** @brief Maximum number of GCNode objects allocated.
		 *
		 * @return the maximum number of GCNode objects allocated (up
		 * to the time of the most recent garbage collection.)
		 *
		 * @note This method is provided for compatibility with CR.
		 * The number of GCNode objects doesn't directly affect the
		 * operation of garbage collection in R.
		 */
		static size_t maxNodes() { return s_max_nodes; }

		/** @brief Number of generations used by garbage collector.
		 *
		 * This will be at least 2, since one generation (Generation
		 * 0) is for newly created nodes still enjoying infant
		 * immunity.
		 * @return The number of generations into which GCNode objects
		 * are ranked by the garbage collector.
		 */
		static size_t numGenerations() { return s_num_old_generations + 2; }

		/** @brief Reset the tallies of the maximum numbers of bytes and
		 *  GCNode objects.
		 *
		 * This method resets the record of the maximum number of
		 * bytes allocated to the current number of bytes allocated,
		 * and similarly for the maximum number of GCNode objects.
		 */
		static void resetMaxTallies();

		/** @brief Set/unset monitors on mark-sweep garbage collection.
		 *
		 * @param pre_gc If not a null pointer, this function will be
		 *          called just before garbage collection begins,
		 *          e.g. to carry out timing.  It must not itself give
		 *          rise to a garbage collection.
		 *
		 * @param post_gc If not a null pointer, this function will be
		 *          called just after garbage collection is completed.
		 *          It  must not itself give rise to a garbage
		 *          collection.
		 */
		static void setMonitors(void (*pre_gc)() = nullptr,
								void (*post_gc)() = nullptr)
		{
			s_pre_gc = pre_gc;
			s_post_gc = post_gc;
		}

		/** @brief Set the output stream for garbage collection reporting.
		 *
		 * @param os Pointer to the output stream to which reporting
		 *          should be directed.  If NULL, suppresses reporting.
		 *
		 * @return The previous value of the output stream pointer.
		 */
		static std::ostream *setReporting(std::ostream *os = nullptr);

		/** @brief Turn garbage collection torture on or off.
		 *
		 * If enabled, every time that CXXR::Heap indicates that it is
		 * about to request additional memory from the operating
		 * system, a garbage collection is carried out.
		 *
		 * @param on The required torturing status.
		 */
		static void torture(bool on) { s_tortured = on; }

		/** @brief Current threshold level for mark-sweep garbage
		 * collection.
		 *
		 * @return The current threshold level.  When GCNode::operator
		 * new is on the point of requesting memory from MemoryBank,
		 * if it finds that the number of bytes already allocated via
		 * MemoryBank is at least as great as this threshold level, it
		 * may initiate a mark-sweep garbage collection.
		 */
		static size_t triggerLevel() { return s_threshold; }
		static size_t nodeTriggerLevel() { return s_node_threshold; }
		static size_t maxTriggerLevel() { return s_max_threshold; }
		static void setMaxTriggerLevel(size_t max_trigger) { s_max_threshold = max_trigger; }
		static size_t maxNodeTriggerLevel() { return s_max_node_threshold; }
		static void setMaxNodeTriggerLevel(size_t max_trigger) { s_max_node_threshold = max_trigger; }
		static bool gc_pending() { return s_gc_pending; }
		static bool gc_fail_on_error() { return s_gc_fail_on_error; }
		static void set_gc_fail_on_error(bool status) { s_gc_fail_on_error = status; }

		/** @brief Report error encountered during garbage collection.
		 *
		 * Report error encountered during garbage collection where for detecting
		 * problems it is better to abort, but for debugging (or some production runs,
		 * where external validation of results is possible) it may be preferred to
		 * continue. Configurable via _R_GC_FAIL_ON_ERROR_. Typically these problems
		 * are due to memory corruption.
		 *
		 * @param msg Error message to be displayed.
		 */
		static void gc_error(const char *msg);
		static void setGCGrowParameters(double node_grow_frac = 0.70, double mem_grow_frac = 0.70)
		{
			R_NGrowFrac = node_grow_frac;
			R_VGrowFrac = mem_grow_frac;
		};
		static void setGCGrowIncrParameters(double node_grow_incr_frac = 0.2, double mem_grow_incr_frac = 0.2)
		{
			R_NGrowIncrFrac = node_grow_incr_frac;
			R_VGrowIncrFrac = mem_grow_incr_frac;
		};
		static bool FORCE_GC();
		static void setTortureParameters(int gap, int wait, bool inhibitor);
		static void setInhibitor(bool inhibitor);
		static int gc_force_wait();
		static int gc_force_gap();
		static bool gc_inhibit_release();

	private:
		static const size_t s_num_old_generations = 2;
		static const unsigned int s_collect_counts_max[s_num_old_generations];
		static unsigned int s_gen_gc_counts[s_num_old_generations + 1];
		static size_t s_threshold;
		static size_t s_min_threshold;
		static size_t s_max_threshold;
		static size_t s_node_threshold;
		static size_t s_min_node_threshold;
		static size_t s_max_node_threshold;
		static bool s_gc_pending;
		static bool s_gc_fail_on_error;
		/* **** if the user specified a wait before starting to force
   		**** collections it might make sense to also wait before starting
   		**** to inhibit releases */
		static int s_gc_force_wait;
		static int s_gc_force_gap;
		static bool s_gc_inhibit_release;

		static double R_NGrowFrac;
		static double R_NShrinkFrac;
		static double R_VGrowFrac;
		static double R_VShrinkFrac;
		static double R_NGrowIncrFrac;
		static double R_NShrinkIncrFrac;
		static int R_NGrowIncrMin;
		static int R_NShrinkIncrMin;
		static double R_VGrowIncrFrac;
		static double R_VShrinkIncrFrac;
		static int R_VGrowIncrMin;
		static int R_VShrinkIncrMin;

		static size_t s_max_bytes;
		static size_t s_max_nodes;

		static bool s_tortured; // If this is true, every cue from
								// CXXR::MemoryBank leads to a garbage
								// collection.

		// Clean up static data associated with garbage collection.
		static void cleanup() {}

		// Callback for CXXR::MemoryBank to cue a garbage collection:
		static size_t cue(size_t bytes_wanted);

		// Detailed control of the garbage collection, in particular
		// choosing how many generations to collect, is carried out
		// here.
		static void gcGenController(size_t bytes_wanted, bool full);

		/** Choose how many generations to collect according to a rota.
		 *
		 * There are three levels of collections.  Level 0 collects only
		 * the youngest generation, Level 1 collects the two youngest
		 * generations, and Level 2 collects all generations.  This
		 * function decides how many old generations to collect according
		 * to a rota.  Most collections are Level 0.  However, after every
		 * collect_counts_max[0] Level 0 collections, a Level 1 collection
		 * will be carried out; similarly after every
		 * collect_counts_max[1] Level 1 collections a Level 2 collection
		 * will be carried out.
		 *
		 * @param minlevel (<= 2, not checked) This parameter places a
		 *          minimum on the number of old generations to be
		 *          collected.  If minlevel is higher than the number of
		 *          generations that genRota would have chosen for itself,
		 *          the position in the rota is advanced accordingly.
		 */
		static unsigned int genRota(unsigned int minlevel);

		static unsigned int s_inhibitor_count; // Number of GCInhibitor
											   // objects in existence.

		static std::ostream *s_os; // Pointer to output stream for GC
								   // reporting, or NULL.

		// Callbacks e.g. for timing:
		static void (*s_pre_gc)();
		static void (*s_post_gc)();

		GCManager() = delete;
	};
} // namespace CXXR

#endif /* GCMANAGER_HPP */
