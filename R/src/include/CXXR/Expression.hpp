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

/** @file Expression.hpp
 * @brief Class CXXR::Expression and associated C interface.
 */

#ifndef EXPRESSION_HPP
#define EXPRESSION_HPP

#include <CXXR/PairList.hpp>
#include <CXXR/GCRoot.hpp>

namespace CXXR
{
   /** @brief Singly linked list representing an R expression.
    *
    * R expression, represented as a LISP-like singly-linked list,
    * each element containing pointers to a 'car' object and to a
    * 'tag' object, as well as a pointer to the next element of the
    * list.  (Any of these pointers may be null.)  A Expression
    * object is considered to 'own' its car, its tag, and all its
    * successors.
    */
   class Expression : public ConsCell
   {
   public:
      /**
       * @param cr Pointer to the 'car' of the element to be
       *           constructed.
       * @param tl Pointer to the 'tail' (LISP cdr) of the element
       *           to be constructed.
       * @param tg Pointer to the 'tag' of the element to be constructed.
       */
      explicit Expression(RObject *cr = nullptr, PairList *tl = nullptr, const RObject *tg = nullptr)
          : ConsCell(LANGSXP, cr, tl, tg)
      {
      }

      Expression(RObject *function, std::initializer_list<RObject *> unnamed_args);

      /** @brief Copy constructor.
       *
       * @param pattern Expression to be copied.
       */
      Expression(const Expression &pattern, Duplicate deep)
          : ConsCell(pattern, deep)
      {
      }

      const RObject *getFunction() const
      {
         return car();
      }

      const PairList *getArgs() const
      {
         return tail();
      }

      /** @brief The name by which this type is known in R.
       *
       * @return The name by which this type is known in R.
       */
      static const char *staticTypeName()
      {
         return "language";
      }

      // Virtual functions of RObject:
      Expression *clone(Duplicate deep) const override;
      RObject *evaluate(Environment *env) override;
      const char *typeName() const override;

   protected:
   private:
      // Declared private to ensure that Expression objects are
      // allocated only using 'new':
      ~Expression() {}

      Expression &operator=(const Expression &) = delete;
   };

   /** @brief Pointer to expression currently being evaluated.
    */
   extern GCRoot<> R_CurrentExpr;

   /** @brief Slot for return-ing values.
    */
   extern GCRoot<> R_ReturnedValue;

   /** @brief Expression currently being evaluated.
    *
    * @return Pointer to the Expression currently being evaluated.
    */
   RObject *Rf_currentExpression();

   /** @brief Designate the Expression currently being evaluated.
    *
    * @param e Pointer to the Expression now to be evaluated.  (Not
    *          currently checked in any way.)
    */
   void Rf_setCurrentExpression(RObject *e);

   /** @brief Deparse an Expression.
    *
    * Utility intended to be called from a debugger.  Prints out the
    * deparse of an RObject.
    *
    * @param expr Pointer to a CXXR::Expression (checked) to be deparsed.
    */
   void DEPARSE(RObject *expr);
} // namespace CXXR

namespace R
{
} // namespace R

extern "C"
{
   /** @brief Create a CXXR::Expression with a specified car and tail.
    *
    * This function protects its arguments from the garbage collector.
    *
    * @param cr Pointer to the 'car' of the element to be created.
    *
    * @param tl Pointer to the 'tail' of the element to be created,
    *          which must be of a CXXR::PairList type (checked).
    *
    * @return Pointer to the constructed list.
    */
   SEXP Rf_lcons(SEXP cr, SEXP tl);

   /** @note Language based list constructs.  These are identical to the list
    * constructs, but the results can be evaluated.
    */
   SEXP Rf_lang1(SEXP s);
   SEXP Rf_lang2(SEXP s, SEXP t);
   SEXP Rf_lang3(SEXP s, SEXP t, SEXP u);
   SEXP Rf_lang4(SEXP s, SEXP t, SEXP u, SEXP v);
   SEXP Rf_lang5(SEXP s, SEXP t, SEXP u, SEXP v, SEXP w);
   SEXP Rf_lang6(SEXP s, SEXP t, SEXP u, SEXP v, SEXP w, SEXP x);

   /** @brief Is an object "language" expression?
    *
    * @param s Pointer to an CXXR::RObject.
    *
    * @return \c true if s is a null pointer or a language expression.
    *
    * @note DIFFERENT than R's  is.language(.) in ../main/coerce.cpp [do_is(), case 301:]
    *                                    which is   <=>  SYMSXP || LANGSXP || EXPRSXP
    */
   Rboolean Rf_isLanguage(SEXP s);
} // extern "C"

#if (defined(R_NO_REMAP) && defined(COMPILING_IVORY)) && defined(__cplusplus)
const auto lang1 = Rf_lang1;
const auto lang2 = Rf_lang2;
const auto lang3 = Rf_lang3;
const auto lang4 = Rf_lang4;
const auto lang5 = Rf_lang5;
const auto lang6 = Rf_lang6;
const auto lcons = Rf_lcons;
const auto isLanguage = Rf_isLanguage;
#endif

#endif /* EXPRESSION_HPP */
