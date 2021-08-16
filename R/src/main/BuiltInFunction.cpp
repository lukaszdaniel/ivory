/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999--2020  The R Core Team.
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
 *
 *  This program is free software; you can redistribute it and/or modify
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

/** @file BuiltInFunction.cpp
 *
 * Implementation of class BuiltInFunction and associated
 * C interface.
 */

#include <CXXR/BuiltInFunction.hpp>
#include <CXXR/Symbol.hpp>
#include <CXXR/SEXP_downcast.hpp>
#include <Internal.h>
#include <Defn.h>

namespace CXXR
{
    // Force the creation of non-inline embodiments of functions callable
    // from C:
    namespace ForceNonInline
    {
        const auto &PRIMOFFSETptr = PRIMOFFSET;
    } // namespace ForceNonInline
#ifndef CXXR_USE_OLD_R_FUNTAB_IMPL
    // BuiltInFunction::getFunctionTable() is in names.cpp

    unsigned int BuiltInFunction::s_next_offset = 0;

    // BuiltInFunction::apply() creates a FunctionContext only if
    // m_transparent is false.  This affects the location at which
    // Rf_error() reports an error as having occurred, and also determines
    // whether a function is reported within traceback().
    //
    // Since functions called via .Internal are not visible to the R user,
    // it seems clear that such functions should be 'transparent'.  One
    // approach would be to leave it at that, so that errors within
    // internal functions would be attributed to the surrounding call of
    // .Internal.  However, rho (currently at least) goes further than
    // this, with a view to attributing an error arising within an
    // internal function to the documented R function which it implements.
    // To this end do_internal itself and various 'syntactical' functions
    // such as do_begin are also flagged as transparent.
    //
    // The flip side of this is that if an error really does occur within
    // one of the 'syntactical' functions (rather than within some inner
    // but transparent scope), it may be desirable to report the error
    // using Rf_errorcall() rather than Rf_error(), so that it can
    // specifically be attributed to the 'syntactical' function.

    BuiltInFunction::BuiltInFunction(const char *name,
                                     CCODE cfun,
                                     unsigned int variant,
                                     unsigned int flags,
                                     int arity,
                                     PPinfo ppinfo,
                                     const char *first_arg_name,
                                     DispatchType dispatch)
        : BuiltInFunction(name, variant, flags, arity, ppinfo,
                          first_arg_name, dispatch)
    {
        m_calling_convention = CallingConvention::PairList;
        m_function.pairlist = cfun;
        if (cfun == do_External || cfun == do_Externalgr || cfun == do_begin || cfun == do_break || cfun == do_dotcall || cfun == do_for || cfun == do_if || cfun == do_internal || cfun == do_repeat || cfun == do_return || cfun == do_while)
        {
            m_transparent = true;
        }
        if (cfun == do_set)
        {
            m_transparent = false;
        }
    }

    BuiltInFunction::BuiltInFunction(const char *name,
                                     ArgumentArrayFn fun,
                                     unsigned int variant,
                                     unsigned int flags,
                                     int arity,
                                     PPinfo ppinfo,
                                     const char *first_arg_name,
                                     DispatchType dispatch)
        : BuiltInFunction(name, variant, flags, arity, ppinfo,
                          first_arg_name, dispatch)
    {
        m_calling_convention = CallingConvention::ArgumentArray;
        m_function.arg_array = fun;
    }

    BuiltInFunction::BuiltInFunction(const char *name,
                                     FixedNativeFnStorage cfun,
                                     unsigned int variant,
                                     unsigned int flags,
                                     int arity,
                                     PPinfo ppinfo,
                                     const char *first_arg_name,
                                     DispatchType dispatch)
        : BuiltInFunction(name, variant, flags, arity, ppinfo,
                          first_arg_name, dispatch)
    {
        m_calling_convention = CallingConvention::FixedNative;
        m_function.fixed_native = cfun;

        if (cfun == reinterpret_cast<FixedNativeFnStorage>(do_paren))
            m_transparent = true;
    }

    BuiltInFunction::BuiltInFunction(const char *name,
                                     VarArgsNativeFn function,
                                     unsigned int variant,
                                     unsigned int flags,
                                     int arity,
                                     PPinfo ppinfo,
                                     const char *first_arg_name,
                                     DispatchType dispatch)
        : BuiltInFunction(name, variant, flags, arity, ppinfo,
                          first_arg_name, dispatch)
    {
        m_calling_convention = CallingConvention::VarArgsNative;
        m_function.varargs_native = function;
    }

    BuiltInFunction::BuiltInFunction(const char *name,
                                     unsigned int variant,
                                     unsigned int flags,
                                     int arity,
                                     PPinfo ppinfo,
                                     const char *first_arg_name,
                                     DispatchType dispatch)
        : FunctionBase(flags % 10 ? BUILTINSXP : SPECIALSXP),
          m_offset(s_next_offset++), m_name(name), m_variant(variant),
          m_via_dot_internal((flags % 100) / 10 == 1), m_arity(arity),
          m_first_arg_name(first_arg_name), m_dispatch_type(dispatch),
          m_gram(ppinfo)
    {
        unsigned int pmdigit = (flags / 100) % 10;
        m_result_printing_mode = ResultPrintingMode(pmdigit);
        m_transparent = (viaDotInternal() || (m_name.length() > 2 && m_name.substr(m_name.length() - 2) == "<-"));
    }
#endif
    BuiltInFunction::~BuiltInFunction()
    {
        assert(0 && "BuiltInFunction's destructor should never be called");
    }
#ifndef CXXR_USE_OLD_R_FUNTAB_IMPL
    BuiltInFunction *BuiltInFunction::obtainInternal(const std::string &name)
    {
        return obtainInternal(Symbol::obtain(name));
    }

    void BuiltInFunction::badArgumentCountError(int nargs, int arity, const Expression *call) const
    {
        if (viaDotInternal())
            Rf_error(
                n_("%d argument passed to .Internal(%s) which requires %d",
                   "%d arguments passed to .Internal(%s) which requires %d",
                   nargs),
                nargs, name(), arity);
        else
            Rf_errorcall(const_cast<Expression *>(call),
                         n_("%d argument passed to '%s' which requires %d",
                            "%d arguments passed to '%s' which requires %d",
                            nargs),
                         nargs, name(), arity);
    }
#endif
    const char *BuiltInFunction::typeName() const
    {
        return sexptype() == SPECIALSXP ? "special" : "builtin";
    }
#ifndef CXXR_USE_OLD_R_FUNTAB_IMPL
    // BuiltInFunction::createLookupTables() is in names.cpp

    std::pair<BuiltInFunction::map *, BuiltInFunction::map *>
    BuiltInFunction::getLookupTables()
    {
        static std::pair<map *, map *> tables = createLookupTables();
        return tables;
    }

    BuiltInFunction::map *BuiltInFunction::getPrimitiveFunctionLookupTable()
    {
        return getLookupTables().first;
    }

    BuiltInFunction::map *BuiltInFunction::getInternalFunctionLookupTable()
    {
        return getLookupTables().second;
    }

    BuiltInFunction *BuiltInFunction::obtainPrimitive(unsigned int offset, bool evaluate)
    {
        if (offset < 0 || offset >= getFunctionTable().size())
            Rf_error(_("offset is out of range"));
        for (BuiltInFunction *function : getFunctionTable())
        {
            if (function->offset() == offset)
            {
                SEXPTYPE type = evaluate ? BUILTINSXP : SPECIALSXP;
                if (TYPEOF(function) != type)
                    Rf_error(_("requested primitive type is not consistent with cached value"));
                return function;
            }
        }
        return nullptr;
    }

    BuiltInFunction *BuiltInFunction::obtainPrimitive(const Symbol *symbol)
    {
        auto location = getPrimitiveFunctionLookupTable()->find(symbol);
        if (location == getPrimitiveFunctionLookupTable()->end())
        {
            Rf_warning(_("%s is not the name of a built-in or special function"), symbol->name()->c_str());
            return nullptr;
        }
        return location->second;
    }

    BuiltInFunction *BuiltInFunction::obtainPrimitive(const std::string &name)
    {
        return obtainPrimitive(Symbol::obtain(name));
    }

    void BuiltInFunction::addPrimitivesToEnvironment(Environment *environment)
    {
        for (const auto &entry : *getPrimitiveFunctionLookupTable())
        {
            const Symbol *symbol = entry.first;
            BuiltInFunction *function = entry.second;
            environment->frame()->bind(symbol, function);
        }
    }

    BuiltInFunction *BuiltInFunction::obtainInternal(const Symbol *name)
    {
        auto location = getInternalFunctionLookupTable()->find(name);
        if (location == getInternalFunctionLookupTable()->end())
        {
            return nullptr;
        }
        return location->second;
    }
#if CXXR_FALSE
    RObject *BuiltInFunction::callBuiltInWithCApi(CCODE builtin,
                                                  const Expression *call,
                                                  const FunctionBase *op,
                                                  const ArgList &args,
                                                  Environment *env)
    {
        return (*builtin)(const_cast<Expression *>(call),
                          const_cast<FunctionBase *>(op),
                          const_cast<PairList *>(args.list()),
                          env);
    }
#endif
    const char *BuiltInFunction::GetInternalGroupDispatchName() const
    {
        switch (m_dispatch_type)
        {
        case DispatchType::GROUP_MATH:
            return "Math";
        case DispatchType::GROUP_OPS:
            return "Ops";
        case DispatchType::GROUP_COMPLEX:
            return "Complex";
        case DispatchType::GROUP_SUMMARY:
            return "Summary";
        default:
            // Ought to be unreachable.
            Rf_error("Attempting to do group dispatch without a group");
        }
    }
#if CXXR_FALSE
    std::pair<bool, RObject *>
    BuiltInFunction::RealInternalDispatch(const Expression *call,
                                          int num_args,
                                          RObject *const *evaluated_args,
                                          const PairList *tags,
                                          Environment *env) const
    {
        PairList *pargs = PairList::make(num_args, evaluated_args);
        pargs->copyTagsFrom(tags);
        return RealInternalDispatch(call, env, ArgList(pargs, ArgList::EVALUATED));
    }

    std::pair<bool, RObject *>
    BuiltInFunction::RealInternalDispatch(const Expression *call,
                                          Environment *env,
                                          ArgList &&args) const
    {
        switch (m_dispatch_type)
        {
        case DispatchType::INTERNAL:
            return R::Dispatch(call, this, args, env);
        case DispatchType::GROUP_MATH:
        case DispatchType::GROUP_OPS:
        case DispatchType::GROUP_COMPLEX:
        case DispatchType::GROUP_SUMMARY:
            return R::DispatchGroup(GetInternalGroupDispatchName(),
                                    call, this, std::move(args), env);
            break;
        default:
            Rf_error("Internal error: Unexepcted group dispatch type");
        }
        return std::make_pair(false, nullptr);
    }
#endif
#endif
#ifdef CXXR_USE_OLD_R_FUNTAB_IMPL
    CCODE PRIMFUN(RObject *x) { return R_FunTab[PRIMOFFSET(x)].cfun(); }
    const char *PRIMNAME(RObject *x) { return R_FunTab[PRIMOFFSET(x)].name(); }
    int PRIMVAL(RObject *x) { return R_FunTab[PRIMOFFSET(x)].code(); }
    int PRIMARITY(RObject *x) { return R_FunTab[PRIMOFFSET(x)].arity(); }
    PPinfo PPINFO(RObject *x) { return R_FunTab[PRIMOFFSET(x)].gram(); }
    int PRIMPRINT(RObject *x) { return ((R_FunTab[PRIMOFFSET(x)].evalargs()) / 100) % 10; }
    int PRIMINTERNAL(RObject *x) { return ((R_FunTab[PRIMOFFSET(x)].evalargs()) % 100) / 10; }
#else
    CCODE PRIMFUN(RObject *x)
    {
        return SEXP_downcast<BuiltInFunction *>(x)->fun();
    }

    const char *PRIMNAME(SEXP x)
    {
        BuiltInFunction *bif = SEXP_downcast<BuiltInFunction *>(x);
        return bif->name();
    }

    int PRIMVAL(SEXP x)
    {
        BuiltInFunction *bif = SEXP_downcast<BuiltInFunction *>(x);
        return bif->variant();
    }

    int PRIMARITY(RObject *x)
    {
        return SEXP_downcast<BuiltInFunction *>(x)->arity();
    }

    PPinfo PPINFO(RObject *x)
    {
        BuiltInFunction *bif = SEXP_downcast<BuiltInFunction *>(x);
        PPinfo ans = {bif->kind(), bif->precedence(), bif->rightAssociative()};
        return ans;
    }

    int PRIMPRINT(RObject *x)
    {
        return SEXP_downcast<BuiltInFunction *>(x)->printHandling();
    }

    int PRIMINTERNAL(RObject *x)
    {
        return SEXP_downcast<BuiltInFunction *>(x)->viaDotInternal();
    }
#endif
} // namespace CXXR

// ***** C interface *****

int CXXR::PRIMOFFSET(SEXP x)
{
    if (!x)
        return 0;
    const BuiltInFunction *bif = SEXP_downcast<const BuiltInFunction *>(x);
    return bif->offset();
}
