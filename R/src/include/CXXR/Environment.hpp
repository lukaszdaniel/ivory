/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999--2020  The R Core Team.
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
 *
 *  This header file is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This file is part of R. R is distributed under the terms of the
 *  GNU General Public License, either Version 2, June 1991 or Version 3,
 *  June 2007. See doc/COPYRIGHTS for details of the copyright status of R.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

/** @file Environment.hpp
 * @brief Class CXXR::Environment and associated C interface.
 */

#ifndef ENVIRONMENT_HPP
#define ENVIRONMENT_HPP

#include <CXXR/RObject.hpp>
#include <CXXR/ListVector.hpp>
#include <CXXR/PairList.hpp>
#include <CXXR/SEXP_downcast.hpp>

namespace R
{
   extern void InitGlobalEnv();
}

namespace CXXR
{
   /** @brief Mapping from Symbols to R objects.
    *
    * An Environment has an associated Frame, which defines a mapping
    * from (pointers to) CXXR::Symbol objects to (pointers to)
    * arbitrary objects of classes derived from RObject.  An
    * Environment will normally have an 'enclosing environment', and
    * the Environment class provides facilities for searching for a
    * binding for a Symbol first in the Environment's own Frame, and
    * then successively in the Frames of enclosing Environments.
    *
    * @note This class does not in itself enforce the requirement
    * that the enclosing relationship must be acyclic.
    */
   class Environment : public RObject
   {
   public:
      /**
       * @param enclosing Pointer to the enclosing environment.
       *
       * @param namevals List of name-value pairs used to initialize
       *          the environment.  Every element of this list must have
       *          a tag (not checked), and these tags must be
       *          distinct (not checked).  As presently implemented,
       *          the constructed Environment takes ownership of
       *          this list, so the calling code should not
       *          subsequently modify it.
       *
       * @todo Probably the default for \a enclosing should be the
       * empty environment.  \a namevals ought to be thoroughly
       * checked.
       */
      explicit Environment(Environment *enclosing = nullptr, PairList *namevals = nullptr)
          : RObject(ENVSXP), m_enclosing(enclosing), m_frame(namevals), m_hashtable(nullptr), m_single_stepping(false)
      {
         INCREMENT_REFCNT(namevals);

         if (enclosing)
            INCREMENT_REFCNT(enclosing);
      }

      /** @brief Base environment.
       *
       * @return pointer to the base environment.
       */
      static Environment *base()
      {
         return s_base_env;
      }

      /** @brief Access the enclosing environment.
       *
       * @return pointer to the enclosing environment.
       */
      Environment *enclosingEnvironment() const
      {
         return m_enclosing;
      }

      /** @brief Empty environment.
       *
       * @return const pointer to the standard empty environment.
       */
      static const Environment *emptyEnvironment()
      {
         return s_empty_env;
      }

      /** @brief Access the frame.
       *
       * @return pointer to the frame of this environment.
       */
      PairList *frame()
      {
         return m_frame;
      }

      /** @brief Access the frame (const variant).
       *
       * @return pointer to the frame of this environment.
       */
      const PairList *frame() const
      {
         return m_frame;
      }

      /** @brief Global environment.
       *
       * @return pointer to the global environment.
       */
      static Environment *global()
      {
         return s_global_env;
      }

      /** @brief Access the hash table.
       *
       * @return pointer to the hash table of this environment.
       */
      ListVector *hashTable()
      {
         return m_hashtable;
      }

      /** @brief Access the hash table (const variant).
       *
       * @return pointer to the hash table of this environment.
       */
      const ListVector *hashTable() const
      {
         return m_hashtable;
      }

      /** @brief Replace the enclosing environment.
       *
       * @param new_enclos Pointer to the environment now to be
       *          considered to enclose this Environment.
       *
       * @todo This ought to check that the chain of ancestors
       * is free of loops and terminates with the empty environment.
       */
      void setEnclosingEnvironment(Environment *new_enclos)
      {
         m_enclosing = new_enclos;
         propagateAge(m_enclosing);
      }

      /** @brief Replace the frame.
       *
       * @param new_frame Pointer to the new frame of this
       *          environment.  Every element of this list must have
       *          a tag (not checked), and these tags must be
       *          distinct (not checked).
       */
      void setFrame(PairList *new_frame)
      {
         m_frame = new_frame;
         propagateAge(m_frame);
      }

      /** @brief Replace the hash table.
       *
       * @param new_hash_table Pointer to the new hash table.
       *          (Because this member function will soon be
       *          replaced, we won't go into the detailed
       *          requirements for a hash table.)
       */
      void setHashTable(ListVector *new_hash_table)
      {
         m_hashtable = new_hash_table;
         propagateAge(m_hashtable);
      }

      /** @brief Set single-stepping status
	 *
	 * @param on The required single-stepping status (true =
	 *           enabled).
	 */
      void setSingleStepping(bool on)
      {
         m_single_stepping = on;
      }

      /** @brief Get single-stepping status.
	 *
	 * @return true if debugger should single-step within this
	 * Environment.
	 */
      bool singleStepping() const
      {
         return m_single_stepping;
      }

      /** @brief The name by which this type is known in R.
       *
       * @return The name by which this type is known in R.
       */
      static const char *staticTypeName()
      {
         return "environment";
      }

      // Virtual function of RObject:
      const char *typeName() const override;

      // Virtual function of GCNode:
      void visitChildren(const_visitor *v) const override;

      /* Environment Access Methods */
      static constexpr int FRAME_LOCK_MASK = (1 << 14);
      static constexpr int GLOBAL_FRAME_MASK = (1 << 15);
      static RObject *frame(RObject *x);
      static RObject *enclos(RObject *x);
      static RObject *hashtab(RObject *x);
      static unsigned int envflags(RObject *x); /* for environments */
      static void set_envflags(RObject *x, unsigned int v);
      static void set_frame(RObject *x, RObject *v);
      static void set_enclos(RObject *x, RObject *v);
      static void set_hashtab(RObject *x, RObject *v);
      static unsigned int frame_is_locked(RObject *x);
      static void lock_frame(RObject *x);
      static bool is_global_frame(RObject *x);
      static void mark_as_global_frame(RObject *x);
      static void mark_as_local_frame(RObject *x);
      static bool env_rdebug(RObject *x);
      static void set_env_rdebug(RObject *x, bool v);

   private:
      static GCRoot<Environment> s_empty_env;
      static GCRoot<Environment> s_base_env;
      static GCRoot<Environment> s_global_env;

      Environment *m_enclosing;
      PairList *m_frame;
      ListVector *m_hashtable;
      bool m_single_stepping;

      // Declared private to ensure that Environment objects are
      // created only using 'new':
      ~Environment() {}

      static void initialize();
      friend void ::R::InitGlobalEnv();
      // Not (yet) implemented.  Declared to prevent
      // compiler-generated versions:
      Environment(const Environment &);
      Environment &operator=(const Environment &);
   };
} // namespace CXXR

extern "C"
{
   extern SEXP R_EmptyEnv;
   extern SEXP R_BaseEnv;
   extern SEXP R_GlobalEnv;

   /** @brief Is this a CXXR::Environment?
    *
    * @param s Pointer to an RObject.
    *
    * @return TRUE iff the RObject pointed to by s is an environment.
    */
   Rboolean Rf_isEnvironment(SEXP s);

   /* Environment Access Functions */

   /** @brief Access an environment's frame.
    *
    * @param x Pointer to a CXXR::Environment (checked).
    *
    * @return Pointer to the frame of \a x (may be null).
    */
   SEXP FRAME(SEXP x);

   /**
    * @param x Pointer to an \c Environment.
    * @return Pointer to \a x 's enclosing environment.
    */
   SEXP ENCLOS(SEXP x);

   /** @brief Access an environment's hash table.
    *
    * @param x Pointer to a CXXR::Environment (checked).
    *
    * @return Pointer to the hash table of \a x (may be null).
    */
   SEXP HASHTAB(SEXP x);

   /** @brief Access an environment's flags.
    *
    * @param x Pointer to a CXXR::Environment (not currently checked).
    *
    * @return the environment flags of \a x .
    */
   int ENVFLAGS(SEXP x);

   /** @brief Should the debugger single-step?
     *
     * @param x Pointer to a CXXR::Environment object (checked).
     *
     * @return \c true if single-stepping is set, i.e. the debugger
     * should single-step within this environment.
     */
   int ENV_RDEBUG(SEXP x);

   /** @brief Enable/disable single-stepping of the debugger.
     *
     * @param x Pointer a CXXR::Environment object (checked).
     *
     * @param v The new single-stepping state (true = enabled).
     */
   void SET_ENV_RDEBUG(SEXP x, int v);

   /** @brief Set environment flags.
    *
    * @param x Pointer to a CXXR::Environment (not currently checked).
    *
    * @param v The new flags.
    *
    * @deprecated
    */
   void SET_ENVFLAGS(SEXP x, int v);

   /** @brief Set environment's frame.
    *
    * @param x Pointer to a CXXR::Environment (checked).
    *
    * @param v Pointer to the new frame.  This must be a CXXR::PairList
    *          (checked), and every element of this list must have a tag
    *          (not checked), and these tags must be distinct (not
    *          checked).
    *
    * @todo Probably should be private.
    */
   void SET_FRAME(SEXP x, SEXP v);

   /** @brief Set an environment's enclosing environment.
    *
    * @param x Pointer to a CXXR::Environment (checked).
    *
    * @param v Pointer to a CXXR::Environment (checked) intended to be
    *          the new enclosing environment of \a x.
    */
   void SET_ENCLOS(SEXP x, SEXP v);

   /** @brief Set environment's hash table.
    *
    * @param x Pointer to a CXXR::Environment (checked).
    *
    * @param v Pointer to the new hash table, which must be a
    * CXXR::ListVector (checked), and satisfy other conditions.
    *
    * @todo To be removed quite soon.
    */
   void SET_HASHTAB(SEXP x, SEXP v);
} // extern "C"

#endif /* ENVIRONMENT_HPP */
