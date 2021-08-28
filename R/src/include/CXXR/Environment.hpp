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
      /** @brief Object authorising R 'break' and 'next' commands.
       *
       * LoopScope objects must be declared on the processor stack
       * (i.e. as C++ automatic variables).  Each Environment object
       * keeps track of the number of LoopScope objects associated
       * with it.  The R commands 'break' and 'next' are legal only
       * when the evaluation Environment has at least one LoopScope
       * in existence; this can be determined by calling
       * Environment::loopActive().
       */
      class LoopScope
      {
      public:
         /** @brief Constructor.
	       *
	       * @param env Pointer to the Environment with which this
	       *          LoopScope is to be associated.
	       */
         LoopScope(Environment *env)
             : m_environment(env), m_prev_state(env->m_in_loop)
         {
            env->m_in_loop = true;
         }

         ~LoopScope()
         {
            m_environment->m_in_loop = m_prev_state;
         }

      private:
         GCStackRoot<Environment> m_environment;
         bool m_prev_state;
      };

      /** @brief Object authorising R 'return' command.
       *
       * ReturnScope objects must be declared on the processor stack
       * (i.e. as C++ automatic variables).  Each Environment object
       * keeps track of the number of ReturnScope objects associated
       * with it.  The R command 'return' is legal only when the
       * evaluation Environment has at least one ReturnScope in
       * existence; this can be determined by calling
       * Environment::canReturn().  More generally, a transfer of
       * control to a specified Environment using ReturnException
       * will succeed only if canReturn() is true.
       */
      class ReturnScope
      {
      public:
         /** @brief Constructor.
          *
          * @param env Pointer to the Environment with which this
          *          ReturnScope is to be associated.
          */
         ReturnScope(Environment *env)
             : m_environment(env), m_prev_state(env->m_can_return)
         {
            env->m_can_return = true;
         }

         ~ReturnScope()
         {
            m_environment->m_can_return = m_prev_state;
         }

      private:
         GCStackRoot<Environment> m_environment;
         bool m_prev_state;
      };

      /** @brief Constructor with specified Frame.
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
            m_locked(false), m_in_loop(false), m_can_return(false)
      {
         m_enclosing = enclosing;
         m_frame = frame;
      }

      /** @brief Access binding of an already-defined Symbol.
       *
       * This function provides a pointer to the Binding of a
       * Symbol.  In this variant the pointer is non-const, and
       * consequently the calling code can use it to modify the
       * binding (provided the Binding is not locked).
       *
       * @param symbol The Symbol for which a mapping is sought.
       *
       * @param recursive If false, a mapping is sought only in this
       *          environment.  If true, the search works up through
       *          enclosing environments.
       *
       * @return A pointer to the required binding, or a null
       * pointer if it was not found..
       */
      // Binding *binding(const Symbol *symbol, bool recursive = true);

      /** @brief Access const binding of an already-defined Symbol.
       *
       * This function provides a pointer to a PairList element
       * representing the binding of a symbol.  In this variant the
       * pointer is const, and consequently the calling code can use
       * it only to examine the binding.
       *
       * @param symbol The Symbol for which a mapping is sought.
       *
       * @param recursive If false, a mapping is sought only in this
       *          environment.  If true, the search works up through
       *          enclosing environments.
       *
       * @return A pointer to the required binding, or a null
       * pointer if it was not found..
       */
      // const Binding *binding(const Symbol *symbol, bool recursive = true) const;

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

      /** @brief Is R 'return' currently legal?
       *
       * @return true iff there is currently at least one ReturnScope
       * object in existence associated with this Environment, so
       * that a transfer of control using ReturnException will
       * succeed.
       */
      bool canReturn() const
      {
         return m_can_return;
      }

      /** @brief Is R 'break' or 'next' currently legal?
       *
       * @return true iff there is currently at least one LoopScope
       * object in existence associated with this Environment.
       */
      bool loopActive() const
      {
         return m_in_loop;
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

      /** @brief Locate a namespace environment from its
       *   specification.
       *
       * @param spec Non-null pointer to the specification of a
       * namespace environment (as returned by namespaceSpec() ).
       *
       * @return A pointer to the namespace environment
       * corresponding to \a spec .  This namespace is loaded if
       * necessary, and deserialization fails if loading is
       * unsuccessful.
       *
       * @todo Having deserialization fail entirely in the event that
       * the namespace cannot be loaded seems insufficiently robust,
       * but follows CR practice.
       */
      static Environment *findNamespace(const StringVector *spec);

      /** @brief Locate a package environment from its name.
       *
       * @param name Name of a package, prefixed by <tt>package:</tt>.
       *
       * @return A pointer to the package environment corresponding
       * to \a name .  This package is loaded if necessary.  If
       * loading fails, the function returns a pointer to the global
       * Environment (<tt>Environment::global()</tt>): this follows
       * CR practice.
       */
      static Environment *findPackage(const std::string &name);

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

      /** @brief Is the frame locked?
       *
       * @return true iff the frame is locked.
       */
      bool isLocked() const
      {
         return m_locked;
      }

      bool noSpecialSymbols() const
      {
         return m_no_special_symbols;
      }

      void setNoSpecialSymbols(bool on)
      {
         m_no_special_symbols = on;
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

      /** @brief Set locking status.
       *
       * @param on The required locking status (true = locked).
       *
       * @note Possibly replace by a plain lock(): unlocking doesn't
       * seem to happen.
       */
      void setLocking(bool on)
      {
         m_locked = on;
      }

      /** @brief Get namespace spec (if applicable).
       *
       * @return If this Environment is a namespace environment,
       * this function returns the namespace specification.
       * Otherwise returns a null pointer.
       */
      const StringVector *namespaceSpec() const;

      /** @brief Get package name (if any).
       *
       * @return If this Environment is the Environment of a package, this
       * function returns the name of the package (of the form
       * "package:foo") as the first element of a StringVector.
       * Otherwise returns a null pointer.
       */
      const StringVector *packageName() const;

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
      // Declared protected to ensure that Environment objects are
      // created only using 'new':
      ~Environment() {}

   private:
      static GCRoot<Environment> s_empty_env;
      static GCRoot<Environment> s_base_env;
      static GCRoot<Environment> s_base_namespace;
      static GCRoot<Environment> s_global_env;

      GCEdge<Environment> m_enclosing;
      GCEdge<PairList> m_frame;
      bool m_single_stepping;
      bool m_locked;
      bool m_no_special_symbols;
      bool m_in_loop;
      bool m_can_return;

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

namespace R
{
   /** @brief Set symbol's value in the base environment.
    *
    * @param x Pointer to a CXXR::Symbol (checked).
    *
    * @param val Pointer to the RObject now to be considered as
    *            the value of this symbol.  A null pointer or
    *            R_UnboundValue are permissible values of \a val.
    *
    * @todo No binding to R_UnboundValue ought to be created.
    */
   void SET_SYMVALUE(SEXP x, SEXP v);

   void R_RestoreHashCount(SEXP rho);

   /** @brief Enable/disable single-stepping of the debugger.
    *
    * @param x Pointer to a CXXR::Environment object (checked).
    *
    * @param v The new single-stepping state (true = enabled).
    */
   void SET_ENV_RDEBUG(SEXP x, int v);
} // namespace R

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

   /** @brief Is this an environment?
    *
    * @param s Pointer to CXXR::RObject.
    *
    * @return TRUE iff the RObject pointed to by \a s is an environment.
    */
   Rboolean Rf_isEnvironment(SEXP s);

   /** @brief Symbol's value in the base environment.
    *
    * @param x Pointer to a CXXR::Symbol (checked).
    *
    * @return Pointer to a CXXR::RObject representing \a x's value.
    *         Returns R_UnboundValue if no value is currently
    *         associated with the Symbol.
    */
   SEXP SYMVALUE(SEXP x);

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
   void SET_NO_SPECIAL_SYMBOLS(SEXP b);
   void UNSET_NO_SPECIAL_SYMBOLS(SEXP b);
   Rboolean NO_SPECIAL_SYMBOLS(SEXP b);
} // extern "C"

#endif /* ENVIRONMENT_HPP */
