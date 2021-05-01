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

/** @file Promise.hpp
 * @brief Class CXXR::Promise and associated C interface.
 */

#ifndef PROMISE_HPP
#define PROMISE_HPP

#include <CXXR/RObject.hpp>
#include <CXXR/Expression.hpp>
#include <CXXR/Environment.hpp>
#include <CXXR/Symbol.hpp>
#include <CXXR/SEXP_downcast.hpp>

namespace CXXR
{
    /** @brief Mechanism for deferred evaluation.
     *
     * This class is used to handle function arguments within R's lazy
     * evaluation scheme.  A Promise object encapsulates a pointer to
     * an arbitrary RObject (typically a Symbol or an Expression), and
     * a pointer to an Environment.  When the Promise is first
     * evaluated, the RObject is evaluated within the Environment, and
     * the result of evaluation returned as the value of the Promise.
     *
     * After the first evaluation, the result of evaluation is cached
     * within the Promise object, and the Environment pointer is set
     * null (thus possibly allowing the Environment to be
     * garbage-collected).  Subsequent evaluations of the Promise
     * object simply return the cached value.
     *
     * @note When a Promise is evaluated \e via a call to evaluate()
     * (the virtual function defined in class RObject), the \a env
     * parameter to evaluate() is ignored: evaluation uses only the
     * Environment encapsulated within the Promise object.  When an
     * RObject is known to be a Promise, it is suggested that
     * evaluation be carried out using the function Promise::force(),
     * which lacks the redundant parameter and is consequently clearer
     * to readers of the code.
     */
    class Promise : public RObject
    {
    public:
        /**
         * @param valgen pointer to RObject to be evaluated to provide
         *          the value of the Promise.  Can be null.
         *
         * @param env Environment in which \a valgen is to be evaluated.
         */
        Promise(const RObject *valgen, Environment *env)
            : RObject(PROMSXP), m_under_evaluation(false),
              m_interrupted(false)
        {
            m_value = Symbol::unboundValue();
            m_valgen = valgen;
            m_environment = env;
        }

        /** @brief Access the environment of the Promise.
         *
         * @return Pointer to the environment of the Promise.  This
         * will be a null pointer after the promise has been
         * evaluated.
         */
        Environment *environment() const
        {
            return m_environment;
        }

        /** @brief Has evaluation been interrupted by a jump?
         *
         * @return true iff evaluation of this Promise has been
         * interrupted by a jump (JMPException).
         */
        bool evaluationInterrupted() const
        {
            return m_interrupted;
        }

        /** @brief Force the Promise.
         *
         * i.e. evaluate the Promise within its environment.
         * Following this, the environment pointer is set null, thus
         * possibly allowing the Environment to be garbage-collected.
         *
         * @return The result of evaluating the promise.
         */
        RObject *force()
        {
            return evaluate(nullptr);
        }

        /** @brief Indicate whether evaluation has been interrupted.
         *
         * @param on true to indicate that evaluation of this promise
         *           has been interrupted by a JMPException.
         *
         * @note To be removed from public interface in due course.
         */
        void markEvaluationInterrupted(bool on)
        {
            m_interrupted = on;
        }

        /** @brief Indicate whether this promise is under evaluation.
         *
         * @param on true to indicate that this promise is currently
         *           under evaluation; otherwise false.
         *
         * @note To be removed from public interface in due course.
         */
        void markUnderEvaluation(bool on)
        {
            m_under_evaluation = on;
        }

        /** @brief RObject to be evaluated by the Promise.
         *
         * @return const pointer to the RObject to be evaluated by
         * the Promise.
         */
        const RObject *valueGenerator() const
        {
            return m_valgen;
        }

        /** @brief Set value of the Promise.
         *
         * Once the value is set to something other than
         * Symbol::unboundValue(), the environment pointer is
         * set null.
         *
         * @param val Value to be associated with the Promise.
         *
         * @todo Should be private (or removed entirely), but currently
         * still used in saveload.cpp.
         */
        void setValue(RObject *val);

        void setEnvironment(Environment *val);
        void setValueGenerator(RObject *val);

        /** @brief The name by which this type is known in R.
         *
         * @return The name by which this type is known in R.
         */
        static const char *staticTypeName()
        {
            return "promise";
        }

        /** @brief Is this promise currently under evaluation?
         *
         * @return true iff this promise is currently under evaluation.
         */
        bool underEvaluation() const
        {
            return m_under_evaluation;
        }

        /** @brief Access the value of a Promise.
         *
         * @return pointer to the value of the Promise, or to
         * Symbol::unboundValue() if it has not yet been evaluated.
         */
        const RObject *value() const
        {
            return m_value;
        }

        static void checkST(const RObject *);

        /** @brief Has this promise been evaluated yet?
         */
        bool evaluated() const
        {
            return m_environment == nullptr;
        }

        // Virtual function of RObject:
        const char *typeName() const;
        RObject *evaluate(Environment *env) override;

        // Virtual function of GCNode:
        void visitReferents(const_visitor *v) const;

    private:
        GCEdge<> m_value;
        GCEdge<const RObject> m_valgen;
        GCEdge<Environment> m_environment;
        bool m_under_evaluation;
        bool m_interrupted;
        // Declared private to ensure that Environment objects are
        // created only using 'new':
        ~Promise() {}

        // Not (yet) implemented.  Declared to prevent
        // compiler-generated versions:
        Promise(const Promise &);
        Promise &operator=(const Promise &);
    };

    /* Stack entry for pending promises */
    struct RPRSTACK
    {
        RObject *promise;
        RPRSTACK *next;
    };

    extern struct RPRSTACK *R_PendingPromises; // INI_as(nullptr); /* Pending promise stack */
} // namespace CXXR

namespace R
{
    /** @brief Create a CXXR::Promise object.
     *
     * @param expr Expression to be evaluated to provide the value
     *          of the CXXR::Promise.
     *
     * @param env CXXR::Environment in which \a expr is to be evaluated.
     */
    SEXP mkPROMISE(SEXP expr, SEXP rho);
} // namespace R

extern "C"
{
    /** @brief Access the expression of a CXXR::Promise.
     *
     * @param x Pointer to a CXXR::Promise (checked).
     *
     * @return Pointer to the expression to be evaluated by the
     *         CXXR::Promise. 
     */
    SEXP PRCODE(SEXP x);

    /** @brief Access the environment of a CXXR::Promise.
     *
     * @param x Pointer to a CXXR::Promise (checked).
     *
     * @return Pointer to the environment in which the CXXR::Promise
     *         is to be  evaluated.  Set to a null pointer when the
     *         CXXR::Promise has been evaluated.
     */
    SEXP PRENV(SEXP x);

    /** @brief Access the value of a CXXR::Promise.
     *
     * @param x Pointer to a CXXR::Promise (checked).
     *
     * @return Pointer to the value of the CXXR::Promise, or to
     *         R_UnboundValue if it has not yet been evaluated..
     */
    SEXP PRVALUE(SEXP x);

    /**
     * @param x Pointer to a CXXR::Promise.
     *
     * @return ?
     *
     * @deprecated Will need to be fixed.
     */
    int PRSEEN(SEXP x);

    /**
     * @param x Pointer to a CXXR::Promise.
     *
     * @deprecated Will need to be fixed.
     */
    void SET_PRSEEN(SEXP x, int v);

    /** @brief Set the environment of a CXXR::Promise.
     *
     * @param x Pointer to a CXXR::Promise (checked).
     *
     * @param v Pointer to the environment in which the expression is to
     *          be evaluated.
     *
     * @todo Probably ought to be private or done in the constructor.
     */
    void SET_PRENV(SEXP x, SEXP v);

    /** @brief Set the value of a CXXR::Promise.
     *
     * Once the value is set to something other than R_UnboundValue,
     * the environment pointer is set null.
     *
     * @param x Pointer to a CXXR::Promise (checked).
     *
     * @param v Pointer to the value to be assigned to the CXXR::Promise.
     *
     * @todo Replace this with a method call to evaluate the CXXR::Promise.
     */
    void SET_PRVALUE(SEXP x, SEXP v);

    /** @brief Set the expression of a CXXR::Promise.
     *
     * @param x Pointer to a CXXR::Promise (checked).
     *
     * @return Pointer to the expression to be evaluated by the
     *         CXXR::Promise. 
     */
    void SET_PRCODE(SEXP x, SEXP v);
} // extern "C"

#endif /* PROMISE_HPP */
