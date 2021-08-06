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

/** @file Closure.hpp
 * @brief Class CXXR::Closure and associated C interface.
 */

#ifndef CLOSURE_HPP
#define CLOSURE_HPP

#include <CXXR/FunctionBase.hpp>
#include <CXXR/Environment.hpp>
#include <CXXR/PairList.hpp>
#include <CXXR/SEXP_downcast.hpp>

namespace CXXR
{
   /** @brief Class representing a functional programming closure.
    *
    * A closure associates a function definition (the body) with a
    * list of formal arguments and an environment.  In evaluating the
    * function, non-local variables within the function definition
    * are interpreted by reference to the specified environment (and
    * its enclosing environments).
    */
   class Closure : public FunctionBase
   {
   public:
      /**
       * @param formal_args List of formal arguments.
       *
       * @param body Pointer to the body of the Closure.  This must
       *          be either a null pointer or a pointer to an object
       *          of one of the following types: PairList,
       *          Expression, Symbol, ExpressionVector, ListVector
       *          or ByteCode (checked).
       *
       * @param env pointer to the environment in which the Closure
       *          is to be evaluated.
       */
      Closure(const PairList *formal_args = nullptr, const RObject *body = nullptr,
              Environment *env = Environment::global());

      /** @brief Copy constructor.
       *
       * @param pattern Closure to be copied.
       *
       * @param deep Indicator whether to perform deep or shallow copy.
       */
      Closure(const Closure &pattern, Duplicate deep)
          : FunctionBase(pattern, deep),
            m_no_jit(pattern.m_no_jit), m_maybe_jit(pattern.m_maybe_jit)
      {
         m_formals = pattern.m_formals;
         m_body = pattern.m_body;
         m_environment = pattern.m_environment ? pattern.m_environment : Environment::global();
      }

      /** @brief Access the body of the Closure.
       *
       * @return Pointer to the body of the Closure.
       */
      const RObject *body() const
      {
         return m_body;
      }

      /** @brief Access the environment of the Closure.
       *
       * @return Pointer to the environment of the Closure.
       */
      Environment *environment() const
      {
         return m_environment;
      }

      /** @brief Access the formal argument list of the Closure.
       *
       * @return Pointer to the formal argument list of the Closure.
       */
      const PairList *formalArgs() const
      {
         return m_formals;
      }

      /** @brief Replace the environment of the closure.
       *
       * @param new_env Pointer to the environment now to be
       *          considered as the environment of this Closure.  A
       *          null pointer is not permissible (not checked).
       */
      void setEnvironment(Environment *new_env)
      {
         // m_environment = new_env;
         // m_environment.propagateAge(this);
         m_environment.retarget(this, new_env);
      }

      /** @brief Replace the formals of the closure.
       *
       * @param formal_args Pointer to a pairlist containing the new
       *          formal arguments to use for this closure.
       */
      void setFormalArgs(PairList *formal_args)
      {
         // m_formals = formal_args;
         // m_formals.propagateAge(this);
         m_formals.retarget(this, formal_args);
      }

      /** @brief Replace the body of the closure.
       *
       * @param body Pointer to the new body to use for this closure.
       */
      void setBody(RObject *body)
      {
         // m_body = body;
         // m_body.propagateAge(this);
         m_body.retarget(this, body);
      }

      bool noJIT() const
      {
         return m_no_jit;
      }

      void setNoJIT()
      {
         m_no_jit = true;
      }

      bool maybeJIT() const
      {
         return m_maybe_jit;
      }

      void setMaybeJIT(bool on)
      {
         m_maybe_jit = on;
      }

      /** @brief The name by which this type is known in R.
       *
       * @return The name by which this type is known in R.
       */
      static const char *staticTypeName()
      {
         return "closure";
      }

      static void checkST(const RObject *);

      // Virtual functions of RObject:
      unsigned int packGPBits() const override;
      void unpackGPBits(unsigned int gpbits) override;
      Closure *clone(Duplicate deep) const override;
      const char *typeName() const override;

      // Virtual function of GCNode:
      void visitReferents(const_visitor *v) const override;

   private:
      GCEdge<const PairList> m_formals;
      GCEdge<const RObject> m_body;
      GCEdge<Environment> m_environment;
      bool m_no_jit;
      bool m_maybe_jit;

      // Declared private to ensure that Closure objects are
      // created only using 'new':
      ~Closure() {}

      // Not (yet) implemented.  Declared to prevent
      // compiler-generated versions:
      Closure &operator=(const Closure &);
   };
} // namespace CXXR

namespace R
{
   /** @brief Create a CXXR::Closure object.
    *
    * @param formal_args Pointer to a CXXR::PairList (checked) of
    *          formal arguments.
    *
    * @param body Pointer to the body of the CXXR::Closure.  This must be
    *          either a null pointer or a pointer to an object of one
    *          of the following types: LISTSXP, LANGSXP, SYMSXP,
    *          EXPRSXP, VECSXP or BCODESXP (checked).
    *
    * @param env pointer to the CXXR::Environment (checked) in which the
    *          closure is to be evaluated.
    *
    * @return pointer to the created closure object.
    *
    * @note This is called by function() {}, where an invalid
    *       body should be impossible. When called from
    *       other places (eg do_asfunction) they
    *       should do this checking in advance.
    */
   SEXP mkCLOSXP(SEXP formal_args, SEXP body, SEXP env);

   /** @brief Get the JIT state
    *
    * @param x Pointer to \c RObject.
    *
    * @return true iff \a x is not meant to be JIT-compiled.  Returns false if \a x
    * is nullptr.
    */
   int NOJIT(SEXP x);

   /** @brief Can this object be JIT-compiled?
    *
    * @param x Pointer to \c RObject.
    *
    * @return true iff \a x can be JIT-compiled.  Returns false if \a x
    * is nullptr.
    */
   int MAYBEJIT(SEXP x);

   /** @brief Do not allow JIT compilation for this object
    *
    * @param x Pointer to \c RObject.
    */
   void SET_NOJIT(SEXP x);

   /** @brief Mark object as available for JIT compilation
    *
    * @param x Pointer to \c RObject.
    */
   void SET_MAYBEJIT(SEXP x);

   /** @brief Remove availabilty flag for JIT compilation
    *
    * @param x Pointer to \c RObject.
    */
   void UNSET_MAYBEJIT(SEXP x);
} // namespace R

extern "C"
{
   /** @brief Access the body of a CXXR::Closure.
    *
    * @param x Pointer to a CXXR::Closure object (checked).
    *
    * @return Pointer to the body of \a x.
    */
   SEXP BODY(SEXP x);

   /** @brief Access the environment of a CXXR::Closure.
    *
    * @param x Pointer to a CXXR::Closure object (checked).
    *
    * @return Pointer to the environment of x.
    */
   SEXP CLOENV(SEXP x);

   /** @brief Access formal arguments of a CXXR::Closure.
    *
    * @param x Pointer to a CXXR::Closure object (checked).
    *
    * @return Pointer to the formal argument list of \a x.
    */
   SEXP FORMALS(SEXP x);

   /** @brief Set the formal arguments of a CXXR::Closure.
    *
    * @param x Pointer to a CXXR::Closure object (checked).
    *
    * @param v Pointer to the formal argument list.
    */
   void SET_FORMALS(SEXP x, SEXP v);

   /** @brief Set the body of a CXXR::Closure.
    *
    * @param x Pointer to a CXXR::Closure object (checked).
    *
    * @param v Pointer to the body of this CXXR::Closure.
    */
   void SET_BODY(SEXP x, SEXP v);

   /** @brief Replace the environment of a CXXR::Closure.
    *
    * @param x Pointer to a CXXR::Closure object (checked).
    *
    * @param v Pointer to the environment now to be
    *          considered as the environment of this CXXR::Closure.
    *          A null pointer is not permissible (not checked).
    */
   void SET_CLOENV(SEXP x, SEXP v);

   /** @brief Get debugging state
    *
    * @param x Pointer to \c RObject.
    *
    * @return true iff \a x is in debugging state.  Returns false if \a x
    * is nullptr.
    */
   int RSTEP(SEXP x);

   /** @brief Set debugging state
    *
    * @param x Pointer to \c RObject.
    */
   void SET_RSTEP(SEXP x, int v);
} // extern "C"

#endif /* CLOSURE_HPP */
