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

#include <CXXR/GCRoot.hpp>
#include <CXXR/GCStackRoot.hpp>
#include <CXXR/RObject.hpp>
#include <CXXR/Frame.hpp>
#include <CXXR/ListVector.hpp>
#include <CXXR/PairList.hpp>
#include <CXXR/StringVector.hpp>
#include <CXXR/Symbol.hpp>
#include <CXXR/SEXP_downcast.hpp>

namespace R
{
   extern void InitGlobalEnv();
} // namespace R

namespace CXXR
{
   class FunctionBase;

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
      /** @brief Constructor.
       *
       * @param enclosing Pointer to the enclosing environment.
       *
       * @param frame List of name-value pairs used to initialize
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
      explicit Environment(Environment *enclosing = nullptr, PairList *frame = nullptr)
          : RObject(ENVSXP), m_single_stepping(false),
            m_globally_cached(false), m_locked(false)
      {
         m_hashtable = nullptr;
         m_enclosing = enclosing;
         m_frame = frame;
      }

      /** @brief Base environment.
       *
       * @return Pointer to the base environment.
       */
      static Environment *base()
      {
         return s_base_env;
      }

      /** @brief Base namespace.
       *
       * @return Pointer to the base namespace.
       */
      static Environment *baseNamespace()
      {
         return s_base_namespace;
      }

      /** @brief Empty environment.
       *
       * CR accords a special status to the empty environment,
       * R_EmptyEnv, which is an Environment whose Frame contains no
       * Bindings, and which has no enclosing Environment.  In CR
       * the search for a Symbol Binding terminates when it reaches
       * the empty environment, without looking inside it.  In rho,
       * although the empty environment still exists (for backwards
       * compatibility)), it is not handled specially.  If the
       * search for a Symbol reaches the empty environment, rho
       * will look for the Symbol inside it - unsuccessfully of
       * course - and the search then terminates because there is no
       * enclosing Environment.
       *
       * @return Pointer to the empty environment.
       *
       * @note rho's own code does not include tests to prohibit
       * the creation of bindings within the empty environment, but
       * the effect of doing so is undefined.
       */
      static Environment *empty()
      {
         return s_empty_env;
      }

      /** @brief Access the enclosing Environment.
       *
       * @return Pointer to the enclosing Environment.
       */
      Environment *enclosingEnvironment() const
      {
         return m_enclosing;
      }

      /** @brief Access the Environment's Frame.
       *
       * @return Pointer to the Environment's Frame.
       */
      PairList *frame()
      {
         return m_frame;
      }

      /** @brief Access the Environment's Frame (const variant).
       *
       * @return const pointer to the Environment's Frame.
       */
      const PairList *frame() const
      {
         return m_frame;
      }

      /** @brief Global environment.
       *
       * @return Pointer to the global environment.
       */
      static Environment *global()
      {
         return s_global_env;
      }

      /** @brief Is this frame in the global cache?
       *
       * @return true iff this frame is included in the global cache.
       */
      bool inGlobalCache() const
      {
         return m_globally_cached;
      }

      /** @brief Is the frame locked?
       *
       * @return true iff the frame is locked.
       */
      bool isLocked() const
      {
         return m_locked;
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
         // m_enclosing = new_enclos;
         // m_enclosing.propagateAge(this);
         m_enclosing.retarget(this, new_enclos);
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
         // m_frame = new_frame;
         // m_frame.propagateAge(this);
         m_frame.retarget(this, new_frame);
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
         // m_hashtable = new_hash_table;
         // m_hashtable.propagateAge(this);
         m_hashtable.retarget(this, new_hash_table);
      }

      /** @brief Set the frame's status as globally cached.
       *
       * @param cached The required status.
       *
       * @note At present this function just toggles a flag: it
       * doesn't insert or remove the frame from the global cache.
       */
      void setGlobalCaching(bool cached)
      {
         // if (cached)
         //    m_gpbits |= GLOBAL_FRAME_MASK;
         // else
         //    m_gpbits &= ~(GLOBAL_FRAME_MASK);
         m_globally_cached = cached;
      }

      /** @brief Set locking status.
       *
       * @param on The required locking status (true = locked).
       *
       * @note Possibly replace by a plain lock(): unlocking doesn't
       * seem to happen.
       */
      void setLocking(bool on)
      {
         // m_gpbits |= FRAME_LOCK_MASK;
         m_locked = on;
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

      static void checkST(const RObject *);

      // Virtual functions of RObject:
      unsigned int packGPBits() const override;
      void unpackGPBits(unsigned int gpbits) override;
      const char *typeName() const override;

      // Virtual function of GCNode:
      void visitReferents(const_visitor *v) const override;

      /** @brief Not for general use.
       *
       * (Used by downcast_to_env to report use of NULL environment.)
       */
      NORET static void nullEnvironmentError();

   protected:
      // Declared private to ensure that Environment objects are
      // created only using 'new':
      ~Environment() {}

   private:
      static GCRoot<Environment> s_empty_env;
      static GCRoot<Environment> s_base_env;
      static GCRoot<Environment> s_base_namespace;
      static GCRoot<Environment> s_global_env;

      GCEdge<Environment> m_enclosing;
      GCEdge<PairList> m_frame;
      GCEdge<ListVector> m_hashtable;
      bool m_single_stepping;
      bool m_globally_cached;
      bool m_locked;

      static void initialize();
      friend void ::R::InitGlobalEnv();
      // Not (yet) implemented.  Declared to prevent
      // compiler-generated versions:
      Environment(const Environment &);
      Environment &operator=(const Environment &);
   };

   template <typename PtrIn>
   Environment *downcast_to_env(PtrIn s, bool allow_null = false)
   {
      if (!s && !allow_null)
      {
         Environment::nullEnvironmentError();
      }
      return SEXP_downcast<Environment *>(s);
   }

} // namespace CXXR

extern "C"
{
   /** @brief An empty environment at the root of the environment tree
    */
   extern SEXP R_EmptyEnv;

   /** @brief The base environment; formerly R_NilValue
    */
   extern SEXP R_BaseEnv;

   /** @brief The "global" environment
    */
   extern SEXP R_GlobalEnv;

   /** @brief The (fake) namespace for base
    */
   extern SEXP R_BaseNamespace;

   /** @brief Is this a CXXR::Environment?
    *
    * @param s Pointer to an RObject.
    *
    * @return TRUE iff the RObject pointed to by s is an environment.
    */
   Rboolean Rf_isEnvironment(SEXP s);

   /** @brief Access an environment's Frame, represented as a PairList.
    *
    * @param x Pointer to a CXXR::Environment (checked).
    *
    * @return Pointer to a PairList representing the contents of the
    * Frame of \a x (may be null).  This PairList is generated on the
    * fly, so this is a relatively expensive operation.  Alterations
    * to the returned PairList will not alter the Environment's Frame.
    *
    * @note Beware that since (unlike CR) this isn't a simple
    * accessor function, its return value will need protection from
    * garbage collection.
    */
   SEXP FRAME(SEXP x);

   /** @brief Access enclosing environment.
    *
    * @param x Pointer to a CXXR::Environment (checked).
    *
    * @return Pointer to the enclosing environment of \a x.
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
    * @todo Probably should be private.
    */
   void SET_HASHTAB(SEXP x, SEXP v);

   SEXP R_NewEnv(SEXP enclos, int hash, int size);
   void R_RestoreHashCount(SEXP rho);
   Rboolean R_IsPackageEnv(SEXP rho);
   SEXP R_PackageEnvName(SEXP rho);
   SEXP R_FindPackageEnv(SEXP info);
   Rboolean R_IsNamespaceEnv(SEXP rho);
   SEXP R_NamespaceEnvSpec(SEXP rho);
   SEXP R_FindNamespace(SEXP info);
   void R_LockEnvironment(SEXP env, Rboolean bindings);
   Rboolean R_EnvironmentIsLocked(SEXP env);
   void R_LockBinding(SEXP sym, SEXP env);
   void R_unLockBinding(SEXP sym, SEXP env);
   void R_MakeActiveBinding(SEXP sym, SEXP fun, SEXP env);
   Rboolean R_BindingIsLocked(SEXP sym, SEXP env);
   Rboolean R_BindingIsActive(SEXP sym, SEXP env);
   SEXP R_ActiveBindingFunction(SEXP sym, SEXP env);
   Rboolean R_HasFancyBindings(SEXP rho);
} // extern "C"

#endif /* ENVIRONMENT_HPP */
