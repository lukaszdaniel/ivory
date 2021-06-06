/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999--2021  The R Core Team.
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
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
 *
 *
 *
 *  Environments:
 *
 *  All the action of associating values with symbols happens
 *  in this code.  An environment is (essentially) a list of
 *  environment "frames" of the form
 *
 *	FRAME(envir) = environment frame
 *	ENCLOS(envir) = parent environment
 *	HASHTAB(envir) = (optional) hash table
 *
 *  Each frame is a (tagged) list with
 *
 *	TAG(item) = symbol
 *	CAR(item) = value bound to symbol in this frame
 *	CDR(item) = next value on the list
 *
 *  When the value of a symbol is required, the environment is
 *  traversed frame-by-frame until a value is found.
 *
 *  If a value is not found during the traversal, the symbol's
 *  "value" slot is inspected for a value.  This "top-level"
 *  environment is where system functions and variables reside.
 *
 *  Environments with the NO_SPECIAL_SYMBOLS flag set are known to not
 *  contain any special symbols, as indicated by the IS_SPECIAL_SYMBOL
 *  macro.  Lookup for such a symbol can then bypass this environment
 *  without searching it.
 */

/* R 1.8.0: namespaces are no longer experimental, so the following
 *  are no longer 'experimental options', but rather three sections
 *  describing the API:
 *
 * NAMESPACES:
 *     R_BaseNamespace holds an environment that has R_GlobalEnv as
 *     its parent.  This environment does not actually contain any
 *     bindings of its own.  Instead, it redirects all fetches and
 *     assignments to the SYMVALUE fields of the base (R_BaseEnv)
 *     environment.  If evaluation occurs in R_BaseNamespace, then
 *     base is searched before R_GlobalEnv.
 *
 * ENVIRONMENT_LOCKING: Locking an environment prevents new bindings
 *     from being created and existing bindings from being removed.
 *
 * FANCY_BINDINGS: We have binding locking and "active bindings".
 *     When a binding is locked, its value cannot be changed.  It may
 *     still be removed from the environment if the environment is not
 *     locked.
 *
 *     Active bindings contain a function in their value cell.
 *     Getting the value of an active binding calls this function with
 *     no arguments and returns the result.  Assigning to an active
 *     binding calls this function with one argument, the new value.
 *     Active bindings may be useful for mapping external variables,
 *     such as C variables or data base entries, to R variables.  They
 *     may also be useful for making some globals thread-safe.
 *
 *     Bindings are marked as locked or active using bits 14 and 15 in
 *     their gp fields.  Since the save/load code writes out this
 *     field it means the value will be preserved across save/load.
 *     But older versions of R will interpret the entire gp field as
 *     the MISSING field, which may cause confusion.  If we keep this
 *     code, then we will need to make sure that there are no
 *     locked/active bindings in workspaces written for older versions
 *     of R to read.
 *
 * LT */

/** @file envir.cpp
 *
 * Environments: all the action of associating values with symbols
 * happens in this code.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#define R_NO_REMAP
#define R_USE_SIGNALS 1

#include <CXXR/Frame.hpp>
#include <CXXR/Environment.hpp>
#include <CXXR/CachedString.hpp>
#include <CXXR/BuiltInFunction.hpp>
#include <CXXR/Expression.hpp>
#include <CXXR/FixedVector.hpp>
#include <CXXR/StringVector.hpp>
#include <CXXR/IntVector.hpp>
#include <CXXR/LogicalVector.hpp>
#include <CXXR/Symbol.hpp>
#include <CXXR/Promise.hpp>
#include <Localization.h>
#include <RContext.h>
#include <Defn.h>
#include <Internal.h>
#include <R_ext/Callbacks.h>

using namespace R;
using namespace CXXR;

#define FAST_BASE_CACHE_LOOKUP  /* Define to enable fast lookups of symbols */
				/*    in global cache from base environment */

static SEXP getActiveValue(SEXP fun)
{
    GCStackRoot<Expression> expr(GCNode::expose(new Expression(fun, {})));
    return expr->evaluate(Environment::global());
}

static void setActiveValue(SEXP fun, SEXP val)
{
    static Symbol *s_dcolon = Symbol::obtain("::");
    static Symbol *s_base = Symbol::obtain("base");
    static Symbol *s_quote = Symbol::obtain("quote");
    GCStackRoot<Expression> qfun(GCNode::expose(new Expression(s_dcolon, {s_base, s_quote})));
    GCStackRoot<Expression> arg(GCNode::expose(new Expression(qfun, {val})));
    GCStackRoot<Expression> expr(GCNode::expose(new Expression(fun, {arg})));
    expr->evaluate(Environment::global());
}

inline static bool IS_USER_DATABASE(SEXP rho)
{
    // original CXXR always returns false (rev. 375)
    return (OBJECT((rho)) && inherits((rho), "UserDefinedDatabase"));
}

namespace
{
    inline bool FRAME_IS_LOCKED(SEXP e)
    {
        return e && SEXP_downcast<const Environment *>(e)->isLocked();
    }

    inline void LOCK_FRAME(SEXP e)
    {
        if (!e)
            return;
        SEXP_downcast<Environment *>(e)->setLocking(true);
    }
}
/* use the same bits (15 and 14) in symbols and bindings */
static SEXP getActiveValue(SEXP);
inline static SEXP BINDING_VALUE(SEXP b)
{
    if (BNDCELL_TAG(b))
    {
        R_expand_binding_value(b);
        return CAR0(b);
    }
    if (IS_ACTIVE_BINDING(b))
        return getActiveValue(CAR(b));
    else
        return CAR(b);
}

RObject *Frame::Binding::unforcedValue() const
{
    RObject *ans = nullptr;
    if (bndcellTag())
    {
        R_expand_binding_value(const_cast<Frame::Binding *>(this));
        ans = m_value;
    }
    if (isActive())
        ans = getActiveValue(m_value);
    else
    {
        if (bndcellTag())
            Rf_error(_("bad binding access"));
        ans = m_value;
    }
    m_frame->monitorRead(*this);
    return ans;
}

inline static SEXP SYMBOL_BINDING_VALUE(SEXP s)
{
    return IS_ACTIVE_BINDING(s) ? getActiveValue(SYMVALUE(s)) : SYMVALUE(s);
}

inline static Rboolean SYMBOL_HAS_BINDING(SEXP s)
{
    return Rboolean(IS_ACTIVE_BINDING(s) || (SYMVALUE(s) != R_UnboundValue));
}

inline static void SET_BINDING_VALUE(SEXP b, SEXP val)
{
    if (BINDING_IS_LOCKED(b))
        error(_("cannot change value of locked binding for '%s'"), CHAR(PRINTNAME(TAG(b))));
    if (IS_ACTIVE_BINDING(b))
        setActiveValue(CAR(b), val);
    else
        SET_BNDCELL(b, val);
}

void Frame::Binding::assignSlow(RObject *new_value, Origin origin)
{
    if (isLocked())
        Rf_error(_("cannot change value of locked binding for '%s'"), symbol()->name()->c_str());
    m_origin = origin;
    if (isActive())
    {
        setActiveValue(m_value, new_value);
        m_frame->monitorRead(*this);
    }
    else
    {
        if (bndcellTag())
        {
            m_value.clearCar(); // m_value = nullptr;
            setBndCellTag(NILSXP);
        }
        m_value.retarget(m_frame, new_value);
        m_frame->monitorWrite(*this);
    }
}

inline static void SET_SYMBOL_BINDING_VALUE(SEXP sym, SEXP new_value)
{
    if (BINDING_IS_LOCKED(sym))
        error(_("cannot change value of locked binding for '%s'"), CHAR(PRINTNAME(sym)));
    if (IS_ACTIVE_BINDING(sym))
    {
        PROTECT(new_value);
        setActiveValue(SYMVALUE(sym), new_value);
        UNPROTECT(1);
    }
    else
        SET_SYMVALUE(sym, new_value);
}

/* Macro version of isNull for only the test against nullptr */
inline static bool ISNULL(SEXP x)
{
    return (x == nullptr);
}

/* Function to determine whethr an environment contains special symbols */
Rboolean R_envHasNoSpecialSymbols(SEXP env)
{
    SEXP frame;

    if (HASHTAB(env) != nullptr)
	return FALSE;

    for (frame = FRAME(env); frame != nullptr; frame = CDR(frame))
	if (IS_SPECIAL_SYMBOL(TAG(frame)))
	    return FALSE;

    return TRUE;
}

/*----------------------------------------------------------------------

  Hash Tables

  We use a basic separate chaining algorithm.	A hash table consists
  of SEXP (vector) which contains a number of SEXPs (lists).

  The only non-static function is R_NewHashedEnv, which allows code to
  request a hashed environment.  All others are static to allow
  internal changes of implementation without affecting client code.
*/

R_INLINE static int HASHSIZE(SEXP x)
{
    return (int)CXXR::VectorBase::stdvec_length(x);
}
R_INLINE static int HASHPRI(SEXP x)
{
    return (int)CXXR::VectorBase::stdvec_truelength(x);
}
#define HASHTABLEGROWTHRATE 1.2
#define HASHMINSIZE 29
R_INLINE static void SET_HASHPRI(SEXP x, int v)
{
    SET_TRUELENGTH(x, v);
}
#define HASHCHAIN(table, i) (CXXR::stdvec_dataptr<SEXP>(table))[i]

R_INLINE static bool IS_HASHED(SEXP x)
{
    return (HASHTAB(x) != nullptr);
}

/*----------------------------------------------------------------------

  R_HashSet

  Hashtable set function.  Sets 'symbol' in 'table' to be 'value'.
  'hashcode' must be provided by user.	Allocates some memory for list
  entries.

*/

static void R_HashSet(int hashcode, SEXP symbol, SEXP table, SEXP value,
		      int frame_locked)
{
    SEXP chain;

    /* Grab the chain from the hashtable */
    chain = VECTOR_ELT(table, hashcode);

    /* Search for the value in the chain */
    for (; !ISNULL(chain); chain = CDR(chain))
	if (TAG(chain) == symbol) {
	    SET_BINDING_VALUE(chain, value);
	    SET_MISSING(chain, 0);	/* Over-ride for new value */
	    return;
	}
    if (frame_locked)
	error(_("cannot add bindings to a locked environment"));
    if (ISNULL(chain))
	SET_HASHPRI(table, HASHPRI(table) + 1);
    /* Add the value into the chain */
    SET_VECTOR_ELT(table, hashcode, CONS(value, VECTOR_ELT(table, hashcode)));
    SET_TAG(VECTOR_ELT(table, hashcode), symbol);
    return;
}



/*----------------------------------------------------------------------

  R_HashGet

  Hashtable get function.  Returns 'value' from 'table' indexed by
  'symbol'.  'hashcode' must be provided by user.  Returns
  'R_UnboundValue' if value is not present.

*/

static SEXP R_HashGet(int hashcode, SEXP symbol, SEXP table)
{
    SEXP chain;

    /* Grab the chain from the hashtable */
    chain = HASHCHAIN(table, hashcode);
    /* Retrieve the value from the chain */
    for (; chain != nullptr ; chain = CDR(chain))
	if (TAG(chain) == symbol) return BINDING_VALUE(chain);
    /* If not found */
    return R_UnboundValue;
}

static bool R_HashExists(int hashcode, SEXP symbol, SEXP table)
{
    SEXP chain;

    /* Grab the chain from the hashtable */
    chain = VECTOR_ELT(table, hashcode);
    /* Find the binding in the chain */
    for (; chain != nullptr; chain = CDR(chain))
        if (TAG(chain) == symbol)
            return true;
    /* If not found */
    return false;
}

/*----------------------------------------------------------------------

  R_HashGetLoc

  Hashtable get location function. Just like R_HashGet, but returns
  location of variable, rather than its value. Returns nullptr
  if not found.

*/

static SEXP R_HashGetLoc(int hashcode, SEXP symbol, SEXP table)
{
    SEXP chain;

    /* Grab the chain from the hashtable */
    chain = VECTOR_ELT(table, hashcode);
    /* Retrieve the value from the chain */
    for (; !ISNULL(chain); chain = CDR(chain))
	if (TAG(chain) == symbol) return chain;
    /* If not found */
    return nullptr;
}



/** @brief Hash table initialisation function.
 * 
 * Creates a table of size 'size' that increases in size
 * by 'growth_rate' after a threshold is met.
 */
static SEXP R_NewHashTable(int size)
{
    if (size <= 0) size = HASHMINSIZE;

    /* Allocate hash table in the form of a vector */
    GCStackRoot<> table(allocVector(VECSXP, size));
    SET_HASHPRI(table, 0);

    return table;
}

/**
 * @return Returns a new Environment with a hash table initialized with default
 * size.
 * 
 * @note The only non-static hash table function.
 */
SEXP R::R_NewHashedEnv(SEXP enclos, SEXP size)
{
    SEXP s;

    PROTECT(enclos);
    PROTECT(size);
    PROTECT(s = NewEnvironment(nullptr, nullptr, enclos));
    SET_HASHTAB(s, R_NewHashTable(asInteger(size)));
    UNPROTECT(3);
    return s;
}


/** @brief Hash table delete function.
 * 
 * Symbols are completely removed from the table;
 * there is no way to mark a symbol as not present without actually removing
 * it.
 */
static std::pair<SEXP, bool> RemoveFromList(SEXP thing, SEXP list);

static bool R_HashDelete(int hashcode, SEXP symbol, SEXP env)
{
    int idx;
    SEXP hashtab;

    hashtab = HASHTAB(env);
    idx = hashcode % HASHSIZE(hashtab);
    auto list = RemoveFromList(symbol, VECTOR_ELT(hashtab, idx));
    if (list.second) {
	if (env == R_GlobalEnv)
	    R_DirtyImage = 1;
	if (list.first == nullptr)
	    SET_HASHPRI(hashtab, HASHPRI(hashtab) - 1);
	SET_VECTOR_ELT(hashtab, idx, list.first);
    }
    return list.second;
}




/** @brief Hash table resizing function.
 * 
 * Increase the size of the hash table by
 * the growth_rate of the table. The vector is reallocated, however
 * the lists with in the hash table have their pointers shuffled around
 * so that they are not reallocated.
 */
static SEXP R_HashResize(SEXP table)
{
    SEXP new_table, chain, new_chain, tmp_chain;
    int counter, new_hashcode;

    /* Do some checking */
    if (TYPEOF(table) != VECSXP)
	error(_("first argument ('table') is not of type '%s', from '%s'"), "VECSXP", "R_HashResize()");

    /* This may have to change.	 The growth rate should
       be independent of the size (not implemented yet) */
    /* hash_grow = HASHSIZE(table); */

    /* Allocate the new hash table */
    new_table = R_NewHashTable((int)(HASHSIZE(table) * HASHTABLEGROWTHRATE));
    for (counter = 0; counter < length(table); counter++) {
	chain = VECTOR_ELT(table, counter);
	while (!ISNULL(chain)) {
	    new_hashcode = R_Newhashpjw(CHAR(PRINTNAME(TAG(chain)))) %
		HASHSIZE(new_table);
	    new_chain = VECTOR_ELT(new_table, new_hashcode);
	    /* If using a primary slot then increase HASHPRI */
	    if (ISNULL(new_chain))
		SET_HASHPRI(new_table, HASHPRI(new_table) + 1);
	    tmp_chain = chain;
	    chain = CDR(chain);
	    SETCDR(tmp_chain, new_chain);
	    SET_VECTOR_ELT(new_table, new_hashcode,  tmp_chain);
#ifdef MIKE_DEBUG
	    fprintf(stdout, "HASHSIZE = %d\nHASHPRI = %d\ncounter = %d\nHASHCODE = %d\n",
		    HASHSIZE(table), HASHPRI(table), counter, new_hashcode);
#endif
	}
    }
    /* Some debugging statements */
#ifdef MIKE_DEBUG
    fprintf(stdout, "Resized O.K.\n");
    fprintf(stdout, "Old size: %d, New size: %d\n",
	    HASHSIZE(table), HASHSIZE(new_table));
    fprintf(stdout, "Old pri: %d, New pri: %d\n",
	    HASHPRI(table), HASHPRI(new_table));
#endif
    return new_table;
} /* end R_HashResize */



/** @brief Hash table size rechecking function.
 * 
 * Compares the load factor (size/# of primary slots used)
 * to a particular threshhold value.
 * 
 * @return true if the table needs to be resized.
 */
static int R_HashSizeCheck(SEXP table)
{
    int resize;
    double thresh_val;

    /* Do some checking */
    if (TYPEOF(table) != VECSXP)
	error(_("first argument ('table') is not of type '%s', from '%s'"), "VECSXP", "R_HashSizeCheck()");
    resize = 0; thresh_val = 0.85;
    if ((double)HASHPRI(table) > (double)HASHSIZE(table) * thresh_val)
	resize = 1;
    return resize;
}



/** @brief Hashing for environment frames.
 * 
 * This function ensures that the 
 * first frame in the given environment has been hashed. Ultimately
 * all enironments should be created in hashed form.  At that point
 * this function will be redundant.
 */
static SEXP R_HashFrame(SEXP rho)
{
    int hashcode;
    SEXP frame, chain, tmp_chain, table;

    /* Do some checking */
    if (TYPEOF(rho) != ENVSXP)
	error(_("first argument ('table') is not of type '%s', from '%s'"), "ENVSXP", "R_HashVector2Hash()");
    table = HASHTAB(rho);
    frame = FRAME(rho);
    while (!ISNULL(frame)) {
	hashcode = HASHVALUE(PRINTNAME(TAG(frame))) % HASHSIZE(table);
	chain = VECTOR_ELT(table, hashcode);
	/* If using a primary slot then increase HASHPRI */
	if (ISNULL(chain)) SET_HASHPRI(table, HASHPRI(table) + 1);
	tmp_chain = frame;
	frame = CDR(frame);
	SETCDR(tmp_chain, chain);
	SET_VECTOR_ELT(table, hashcode, tmp_chain);
    }
    SET_FRAME(rho, nullptr);
    return rho;
}


/* ---------------------------------------------------------------------

   R_HashProfile

   Profiling tool for analyzing hash table performance.  Returns a
   three element list with components:

   size: the total size of the hash table

   nchains: the number of non-null chains in the table (as reported by
	    HASHPRI())

   counts: an integer vector the same length as size giving the length of
	   each chain (or zero if no chain is present).  This allows
	   for assessing collisions in the hash table.
 */

static SEXP R_HashProfile(SEXP table)
{
    SEXP chain, ans, chain_counts, nms;
    int i, count;

    PROTECT(ans = allocVector(VECSXP, 3));
    PROTECT(nms = allocVector(STRSXP, 3));
    SET_STRING_ELT(nms, 0, mkChar("size"));    /* size of hashtable */
    SET_STRING_ELT(nms, 1, mkChar("nchains")); /* number of non-null chains */
    SET_STRING_ELT(nms, 2, mkChar("counts"));  /* length of each chain */
    setAttrib(ans, R_NamesSymbol, nms);
    UNPROTECT(1);

    SET_VECTOR_ELT(ans, 0, ScalarInteger(length(table)));
    SET_VECTOR_ELT(ans, 1, ScalarInteger(HASHPRI(table)));

    PROTECT(chain_counts = allocVector(INTSXP, length(table)));
    for (i = 0; i < length(table); i++) {
	chain = VECTOR_ELT(table, i);
	count = 0;
	for (; chain != nullptr ; chain = CDR(chain)) {
	    count++;
	}
	INTEGER(chain_counts)[i] = count;
    }

    SET_VECTOR_ELT(ans, 2, chain_counts);

    UNPROTECT(2);
    return ans;
}



/*----------------------------------------------------------------------

  Environments

  The following code implements variable searching for environments.

*/


/*----------------------------------------------------------------------

  InitGlobalEnv

  Create the initial global environment.  The global environment is
  no longer a linked list of environment frames.  Instead it is a
  vector of environments which is searched from beginning to end.

  Note that only the first frame of each of these environments is
  searched.  This is intended to make it possible to implement
  namespaces at some (indeterminate) point in the future.

  We hash the initial environment.  100 is a magic number discovered
  by Ross.  Change it if you feel inclined.

*/

#ifdef USE_GLOBAL_CACHE  /* NB leave in place: see below */
/* Global variable caching.  A cache is maintained in a hash table,
   R_GlobalCache.  The entry values are either R_UnboundValue (a
   flushed cache entry), the binding LISTSXP cell from the environment
   containing the binding found in a search from R_GlobalEnv, or a
   symbol if the globally visible binding lives in the base package.
   The cache for a variable is flushed if a new binding for it is
   created in a global frame or if the variable is removed from any
   global frame.

   Symbols in the global cache with values from the base environment
   are flagged with BASE_SYM_CACHED, so that their value can be
   returned immediately without needing to look in the hash table.
   They must still have entries in the hash table, however, so that
   they can be flushed as needed.

   To make sure the cache is valid, all binding creations and removals
   from global frames must go through the interface functions in this
   file.

   Initially only the R_GlobalEnv frame is a global frame.  Additional
   global frames can only be created by attach.  All other frames are
   considered local.  Whether a frame is local or not is recorded in
   the highest order bit of the ENVFLAGS field (the gp field of
   sxpinfo).

   It is possible that the benefit of caching may be significantly
   reduced if we introduce namespace management.  Since maintaining
   cache integrity is a bit tricky and since it might complicate
   threading a bit (I'm not sure it will but it needs to be thought
   through if nothing else) it might make sense to remove caching at
   that time.  To make that easier, the ifdef's should probably be
   left in place.

   L. T. */
#ifdef USE_GLOBAL_CACHE
namespace
{
    inline bool IS_GLOBAL_FRAME(SEXP e)
    {
        return e && SEXP_downcast<const Environment *>(e)->inGlobalCache();
    }

    inline void MARK_AS_GLOBAL_FRAME(SEXP e)
    {
        if (!e)
            return;
        SEXP_downcast<Environment *>(e)->setGlobalCaching(true);
    }

    inline void MARK_AS_LOCAL_FRAME(SEXP e)
    {
        if (!e)
            return;
        SEXP_downcast<Environment *>(e)->setGlobalCaching(false);
    }
}
#endif

#define INITIAL_CACHE_SIZE 1000

static GCRoot<> R_GlobalCache, R_GlobalCachePreserve;
#endif
static GCRoot<> R_BaseNamespaceName;
static GCRoot<Symbol> R_NamespaceSymbol;

HIDDEN void R::InitGlobalEnv()
{
    Environment::initialize();
    R_NamespaceSymbol = Symbol::obtain(".__NAMESPACE__.");

    R_MethodsNamespace = R_GlobalEnv; // so it is initialized.

#ifdef USE_GLOBAL_CACHE
    MARK_AS_GLOBAL_FRAME(R_GlobalEnv);
    R_GlobalCache = R_NewHashTable(INITIAL_CACHE_SIZE);
    R_GlobalCachePreserve = CXXR_cons(R_GlobalCache, nullptr);
    R_PreserveObject(R_GlobalCachePreserve);
#endif
    R_PreserveObject(R_BaseNamespace);
    SET_SYMVALUE(Symbol::obtain(".BaseNamespaceEnv"), R_BaseNamespace);
    R_BaseNamespaceName = ScalarString(CachedString::obtain("base"));
    R_PreserveObject(R_BaseNamespaceName);
    GCStackRoot<> zero(Rf_ScalarInteger(0));
    R_NamespaceRegistry = R_NewHashedEnv(nullptr, zero);
    R_PreserveObject(R_NamespaceRegistry);
    Rf_defineVar(R_BaseSymbol, R_BaseNamespace, R_NamespaceRegistry);
    /**** needed to properly initialize the base namespace */
    Rf_gsetVar(R_LastWarningSymbol, nullptr, R_BaseEnv);  // CXXR addition
    Rf_gsetVar(R_DotTracebackSymbol, nullptr, R_BaseEnv);  // CXXR addition
}

#ifdef USE_GLOBAL_CACHE
static int hashIndex(SEXP symbol, SEXP table)
{
    SEXP c = PRINTNAME(symbol);
    return HASHVALUE(c) % HASHSIZE(table);
}

static void R_FlushGlobalCache(SEXP sym)
{
    SEXP entry = R_HashGetLoc(hashIndex(sym, R_GlobalCache), sym,
			      R_GlobalCache);
    if (entry != nullptr) {
	SETCAR(entry, R_UnboundValue);
#ifdef FAST_BASE_CACHE_LOOKUP
	UNSET_BASE_SYM_CACHED(sym);
#endif
    }
}

static void R_FlushGlobalCacheFromTable(SEXP table)
{
    int i, size;
    SEXP chain;
    size = HASHSIZE(table);
    for (i = 0; i < size; i++) {
	for (chain = VECTOR_ELT(table, i); chain != nullptr; chain = CDR(chain))
	    R_FlushGlobalCache(TAG(chain));
    }
}

/**
 Flush the cache based on the names provided by the user defined
 table, specifically returned from calling objects() for that
 table.
 */
static void R_FlushGlobalCacheFromUserTable(SEXP udb)
{
    int n, i;
    R_ObjectTable *tb;
    SEXP names;
    tb = (R_ObjectTable*) R_ExternalPtrAddr(udb);
    names = tb->objects(tb);
    n = length(names);
    for(i = 0; i < n ; i++)
	R_FlushGlobalCache(Rf_installTrChar(STRING_ELT(names,i)));
}

static void R_AddGlobalCache(SEXP symbol, SEXP place)
{
    int oldpri = HASHPRI(R_GlobalCache);
    R_HashSet(hashIndex(symbol, R_GlobalCache), symbol, R_GlobalCache, place,
	      FALSE);
#ifdef FAST_BASE_CACHE_LOOKUP
    if (symbol == place)
	SET_BASE_SYM_CACHED(symbol);
    else
	UNSET_BASE_SYM_CACHED(symbol);
#endif
    if (oldpri != HASHPRI(R_GlobalCache) &&
	HASHPRI(R_GlobalCache) > 0.85 * HASHSIZE(R_GlobalCache)) {
	R_GlobalCache = R_HashResize(R_GlobalCache);
	SETCAR(R_GlobalCachePreserve, R_GlobalCache);
    }
}

static SEXP R_GetGlobalCacheLoc(SEXP symbol)
{
#ifdef FAST_BASE_CACHE_LOOKUP
    if (BASE_SYM_CACHED(symbol))
	return symbol;
#endif

    return R_HashGet(hashIndex(symbol, R_GlobalCache), symbol, R_GlobalCache);
}
#endif /* USE_GLOBAL_CACHE */

static std::pair<SEXP, bool> RemoveFromList(SEXP thing, SEXP list)
{
    if (list == nullptr) {
        return std::make_pair(nullptr, false);
    }
    else if (TAG(list) == thing) {
	SET_BNDCELL(list, R_UnboundValue); /* in case binding is cached */
	LOCK_BINDING(list);                /* in case binding is cached */
	SEXP rest = CDR(list);
	SETCDR(list, nullptr);          /* to fix refcnt on 'rest' */
	return std::make_pair(rest, true);
    }
    else {
	SEXP last = list;
	SEXP next = CDR(list);
	while (next != nullptr) {
	    if (TAG(next) == thing) {
		SETCAR(next, R_UnboundValue); /* in case binding is cached */
		LOCK_BINDING(next);           /* in case binding is cached */
		SETCDR(last, CDR(next));
		SETCDR(next, nullptr);     /* to fix refcnt on 'list' */
		return std::make_pair(list, true);
	    }
	    else {
		last = next;
		next = CDR(next);
	    }
	}
	return std::make_pair(list, false);
    }
}

/** @brief Remove a value from an environment.
 *
 *       This happens only in the frame of the specified environment.
 * 
 * @todo should this also unbind the symbol value slot when rho is R_BaseEnv.
 *       This is only called from eval.cpp in applydefine and bcEval
 *       (and applydefine only works for unhashed environments, so not base).
*/
HIDDEN void R::unbindVar(SEXP symbol, SEXP rho)
{
    if (rho == R_BaseNamespace)
        error(_("cannot unbind in the base namespace"));
    if (rho == R_BaseEnv)
        error(_("unbind in the base environment is unimplemented"));
    if (FRAME_IS_LOCKED(rho))
	error(_("cannot remove bindings from a locked environment"));
    if (HASHTAB(rho) == nullptr) {
	auto list = RemoveFromList(symbol, FRAME(rho));
	if (list.second) {
	    if (rho == R_GlobalEnv) R_DirtyImage = 1;
	    SET_FRAME(rho, list.first);
#ifdef USE_GLOBAL_CACHE
	    if (IS_GLOBAL_FRAME(rho))
		R_FlushGlobalCache(symbol);
#endif
	}
    }
    else {
	/* This branch is used e.g. via sys.source, utils::data */
	SEXP c = PRINTNAME(symbol);
	int hashcode = HASHVALUE(c) % HASHSIZE(HASHTAB(rho));
#ifdef USE_GLOBAL_CACHE
    bool found = R_HashDelete(hashcode, symbol, rho);
	if (found && IS_GLOBAL_FRAME(rho))
	     R_FlushGlobalCache(symbol);
#else
    R_HashDelete(hashcode, symbol, rho);
#endif
    }
}

/** @brief Look up the location of the value of a symbol in a single environment frame.
 * 
 *        Almost like findVarInFrame, but does not return the value. nullptr if not found.
 *        Callers set *canCache = TRUE or NULL
*/
static SEXP findVarLocInFrame(SEXP rho, SEXP symbol, Rboolean *canCache)
{
    int hashcode;
    SEXP frame, c;

    if (rho == R_BaseEnv || rho == R_BaseNamespace)
	return (SYMVALUE(symbol) == R_UnboundValue) ? nullptr : symbol;

    if (!rho || rho == R_EmptyEnv)
        return nullptr;

    if(IS_USER_DATABASE(rho)) {
	R_ObjectTable *table;
	SEXP val, tmp = nullptr;
	table = (R_ObjectTable *) R_ExternalPtrAddr(HASHTAB(rho));
	/* Better to use exists() here if we don't actually need the value! */
	val = table->get(CHAR(PRINTNAME(symbol)), canCache, table);
	if(val != R_UnboundValue) {
	    /* The result should probably be identified as being from
	       a user database, or maybe use an active binding
	       mechanism to allow setting a new value to get back to
	       the data base. */
	    tmp = GCNode::expose(new PairList(val, nullptr, symbol));
	    /* If the database has a canCache method, then call that.
	       Otherwise, we believe the setting for canCache. */
	    if(canCache && table->canCache) {
		PROTECT(tmp);
		*canCache = table->canCache(CHAR(PRINTNAME(symbol)), table);
		UNPROTECT(1);
	    }
	    MARK_NOT_MUTABLE(val); /* to keep complex assignment code sane */
	}
	return tmp;
    }

    if (HASHTAB(rho) == nullptr) {
	frame = FRAME(rho);
	while (frame != nullptr && TAG(frame) != symbol)
	    frame = CDR(frame);
	return frame;
    }
    else {
	c = PRINTNAME(symbol);
	hashcode = HASHVALUE(c) % HASHSIZE(HASHTAB(rho));
	/* Will return 'nullptr' if not found */
	return R_HashGetLoc(hashcode, symbol, HASHTAB(rho));
    }
}

/** @brief External version and accessor functions.
 * 
 * @return Returned value is cast as an opaque pointer to ensure it is only used by routines in this
 * group. This allows the implementation to be changed without needing to change other files.
 */
R_varloc_t R::R_findVarLocInFrame(SEXP rho, SEXP symbol)
{
    SEXP binding = findVarLocInFrame(rho, symbol, nullptr);
    R_varloc_t val;
    val.fromPairList(binding);
    return val;
}

HIDDEN
SEXP R::R_GetVarLocValue(R_varloc_t vl)
{
    SEXP cell = vl.asPairList();
    if (cell == nullptr || cell == R_UnboundValue)
        return R_UnboundValue;
    else if (TYPEOF(cell) == SYMSXP)
        return SYMBOL_BINDING_VALUE(cell);
    return BINDING_VALUE(cell);
}

HIDDEN
SEXP R::R_GetVarLocSymbol(R_varloc_t vl)
{
    return TAG(vl.asPairList());
}

/* used in methods */
Rboolean R::R_GetVarLocMISSING(R_varloc_t vl)
{
    return Rboolean(MISSING(vl.asPairList()));
}

HIDDEN
void R::R_SetVarLocValue(R_varloc_t vl, SEXP value)
{
    SET_BINDING_VALUE(vl.asPairList(), value);
}

/*----------------------------------------------------------------------

  findVarInFrame

  Look up the value of a symbol in a single environment frame.	This
  is the basic building block of all variable lookups.

  It is important that this be as efficient as possible.

  The final argument is usually TRUE and indicates whether the
  lookup is being done in order to get the value (TRUE) or
  simply to check whether there is a value bound to the specified
  symbol in this frame (FALSE).  This is used for get() and exists().
*/

SEXP Rf_findVarInFrame3(SEXP rho, SEXP symbol, Rboolean doGet)
{
    int hashcode;
    SEXP frame, c;

    if (TYPEOF(rho) == NILSXP)
        error(_("use of NULL environment is defunct"));

    if (rho == R_BaseNamespace || rho == R_BaseEnv)
	return SYMBOL_BINDING_VALUE(symbol);

    if (!rho || rho == R_EmptyEnv)
        return R_UnboundValue;

    if(IS_USER_DATABASE(rho)) {
	/* Use the objects function pointer for this symbol. */
	R_ObjectTable *table;
	SEXP val = R_UnboundValue;
	table = (R_ObjectTable *) R_ExternalPtrAddr(HASHTAB(rho));
	if(table->active) {
	    if(doGet)
		val = table->get(CHAR(PRINTNAME(symbol)), nullptr, table);
	    else {
		if(table->exists(CHAR(PRINTNAME(symbol)), nullptr, table))
		    val = table->get(CHAR(PRINTNAME(symbol)), nullptr, table);
		else
		    val = R_UnboundValue;
	    }
	    MARK_NOT_MUTABLE(val); /* to keep complex assignment code sane */
	}
	return val;
    } else if (HASHTAB(rho) == nullptr) {
	frame = FRAME(rho);
	while (frame != nullptr) {
	    if (TAG(frame) == symbol)
		return BINDING_VALUE(frame);
	    frame = CDR(frame);
	}
    }
    else {
	c = PRINTNAME(symbol);
	hashcode = HASHVALUE(c) % HASHSIZE(HASHTAB(rho));
	/* Will return 'R_UnboundValue' if not found */
	return R_HashGet(hashcode, symbol, HASHTAB(rho));
    }
    return R_UnboundValue;
}

/* This variant of findVarinFrame3 is needed to avoid running active
   binding functions in calls to exists() with mode = "any" */
static bool existsVarInFrame(SEXP rho, SEXP symbol)
{
    int hashcode;
    SEXP frame, c;

    if (TYPEOF(rho) == NILSXP)
        error(_("use of NULL environment is defunct"));

    if (rho == R_BaseNamespace || rho == R_BaseEnv)
	return SYMBOL_HAS_BINDING(symbol);

    if (!rho || rho == R_EmptyEnv)
        return false;

    if(IS_USER_DATABASE(rho)) {
	/* Use the objects function pointer for this symbol. */
	R_ObjectTable *table;
	bool val = false;
	table = (R_ObjectTable *) R_ExternalPtrAddr(HASHTAB(rho));
	if(table->active) {
	    if(table->exists(CHAR(PRINTNAME(symbol)), nullptr, table))
		val = true;
	    else
		val = false;
	}
	return (val);
    } else if (HASHTAB(rho) == nullptr) {
	frame = FRAME(rho);
	while (frame != nullptr) {
	    if (TAG(frame) == symbol)
		return true;
	    frame = CDR(frame);
	}
    }
    else {
	c = PRINTNAME(symbol);
	hashcode = HASHVALUE(c) % HASHSIZE(HASHTAB(rho));
	/* Will return 'R_UnboundValue' if not found */
	return R_HashExists(hashcode, symbol, HASHTAB(rho));
    }
    return false;
}

inline SEXP Rf_findVarInFrame(SEXP rho, SEXP symbol)
{
    return findVarInFrame3(rho, symbol, TRUE);
}

/** @brief Read the S3 meta-variables from a given (single) frame.
 * R_UnboundValue marks that respective variable is not present.
 * This function is optimized to be fast in the common case when the
 * S3 meta-variables are in the expected order and that the frame is
 * represented by a pairlist.
 */
void Rf_readS3VarsFromFrame(SEXP rho,
    SEXP *dotGeneric, SEXP *dotGroup, SEXP *dotClass, SEXP *dotMethod,
    SEXP *dotGenericCallEnv, SEXP *dotGenericDefEnv) {

    SEXP frame = nullptr;

    if (TYPEOF(rho) == NILSXP ||
	rho == R_BaseNamespace || rho == R_BaseEnv || rho == R_EmptyEnv ||
	IS_USER_DATABASE(rho) || HASHTAB(rho) != nullptr) goto slowpath;

    frame = FRAME(rho);

    /*
    This code speculates there is a specific order of S3 meta-variables.  It
    holds in most (perhaps all non-fabricated) cases.  If at any time this
    ceased to hold, this code will fall back to the slowpath, which may be
    slow but still correct.
    */

    for(;TAG(frame) != R_dot_Generic; frame = CDR(frame))
	if (frame == nullptr) goto slowpath;
    *dotGeneric = BINDING_VALUE(frame);
    frame = CDR(frame);

    if (TAG(frame) != R_dot_Class) goto slowpath;
    *dotClass = BINDING_VALUE(frame);
    frame = CDR(frame);

    if (TAG(frame) != R_dot_Method) goto slowpath;
    *dotMethod = BINDING_VALUE(frame);
    frame = CDR(frame);

    if (TAG(frame) != R_dot_Group) goto slowpath;
    *dotGroup = BINDING_VALUE(frame);
    frame = CDR(frame);

    if (TAG(frame) != R_dot_GenericCallEnv) goto slowpath;
    *dotGenericCallEnv = BINDING_VALUE(frame);
    frame = CDR(frame);

    if (TAG(frame) != R_dot_GenericDefEnv) goto slowpath;
    *dotGenericDefEnv = BINDING_VALUE(frame);

    return;

slowpath:
    /* fall back to the slow but general implementation */

    *dotGeneric = Rf_findVarInFrame3(rho, R_dot_Generic, TRUE);
    *dotClass = Rf_findVarInFrame3(rho, R_dot_Class, TRUE);
    *dotMethod = Rf_findVarInFrame3(rho, R_dot_Method, TRUE);
    *dotGroup = Rf_findVarInFrame3(rho, R_dot_Group, TRUE);
    *dotGenericCallEnv = Rf_findVarInFrame3(rho, R_dot_GenericCallEnv, TRUE);
    *dotGenericDefEnv = Rf_findVarInFrame3(rho, R_dot_GenericDefEnv, TRUE);
}


#ifdef USE_GLOBAL_CACHE
/* findGlobalVar searches for a symbol value starting at R_GlobalEnv,
   so the cache can be used. */
static SEXP findGlobalVarLoc(SEXP symbol)
{
    SEXP vl, rho;
    Rboolean canCache = TRUE;
    vl = R_GetGlobalCacheLoc(symbol);
    if (vl != R_UnboundValue)
	return vl;
    for (rho = R_GlobalEnv; rho != R_EmptyEnv; rho = ENCLOS(rho)) {
	if (rho != R_BaseEnv) { /* we won't have R_BaseNamespace */
	    vl = findVarLocInFrame(rho, symbol, &canCache);
	    if (vl != nullptr) {
		if(canCache)
		    R_AddGlobalCache(symbol, vl);
		return vl;
	    }
	}
	else {
	    if (SYMVALUE(symbol) != R_UnboundValue)
		R_AddGlobalCache(symbol, symbol);
	    return symbol;
	}
    }
    return nullptr;
}

inline static SEXP findGlobalVar(SEXP symbol)
{
    SEXP loc = findGlobalVarLoc(symbol);
    switch (TYPEOF(loc)) {
    case NILSXP: return R_UnboundValue;
    case SYMSXP: return SYMBOL_BINDING_VALUE(symbol);
    default: return BINDING_VALUE(loc);
                    /* loc is protected by callee when needed */
    }
}
#endif

/** @brief Look up a symbol in an environment.
 */
SEXP Rf_findVar(SEXP symbol, SEXP rho)
{
    SEXP vl;

    if (TYPEOF(rho) == NILSXP)
	error(_("use of NULL environment is defunct"));

    if (!isEnvironment(rho))
	error(_("argument passed to '%s' function is not an environment"), "findVar()");

#ifdef USE_GLOBAL_CACHE
    /* This first loop handles local frames, if there are any.  It
       will also handle all frames if rho is a global frame other than
       R_GlobalEnv */
    while (rho != R_GlobalEnv && rho != R_EmptyEnv) {
	vl = findVarInFrame3(rho, symbol, TRUE /* get rather than exists */);
	if (vl != R_UnboundValue) return (vl);
	rho = ENCLOS(rho);
    }
    if (rho == R_GlobalEnv)
	return findGlobalVar(symbol);
    else
	return R_UnboundValue;
#else
    while (rho != R_EmptyEnv) {
	vl = findVarInFrame3(rho, symbol, TRUE);
	if (vl != R_UnboundValue) return (vl);
	rho = ENCLOS(rho);
    }
    return R_UnboundValue;
#endif
}

static SEXP findVarLoc(SEXP symbol, SEXP rho)
{
    SEXP vl;

    if (TYPEOF(rho) == NILSXP)
        error(_("use of NULL environment is defunct"));

    if (!isEnvironment(rho))
        error(_("argument passed to '%s' function is not an environment"), "findVarLoc()");

#ifdef USE_GLOBAL_CACHE
    /* This first loop handles local frames, if there are any.  It
       will also handle all frames if rho is a global frame other than
       R_GlobalEnv */
    while (rho != R_GlobalEnv && rho != R_EmptyEnv) {
	vl = findVarLocInFrame(rho, symbol, nullptr);
	if (vl != nullptr) return vl;
	rho = ENCLOS(rho);
    }
    if (rho == R_GlobalEnv)
	return findGlobalVarLoc(symbol);
    else
	return nullptr;
#else
    while (rho != R_EmptyEnv) {
	vl = findVarLocInFrame(rho, symbol, nullptr);
	if (vl != nullptr) return vl;
	rho = ENCLOS(rho);
    }
    return nullptr;
#endif
}

R_varloc_t R::R_findVarLoc(SEXP rho, SEXP symbol)
{
    SEXP binding = findVarLoc(rho, symbol);
    R_varloc_t val;
    val.fromPairList(binding);
    return val;
}

/** @brief Look up a symbol in an environment.
 * 
 * Ignore any values which are not of the specified type.
 */
HIDDEN SEXP R::findVar1(SEXP symbol, SEXP rho, SEXPTYPE mode, int inherits_)
{
    SEXP vl;
    while (rho != R_EmptyEnv) {
	vl = findVarInFrame3(rho, symbol, TRUE);
	if (vl != R_UnboundValue) {
	    if (mode == ANYSXP) return vl;
	    if (TYPEOF(vl) == PROMSXP) {
		PROTECT(vl);
		vl = eval(vl, rho);
		UNPROTECT(1);
	    }
	    if (TYPEOF(vl) == mode) return vl;
	    if (mode == FUNSXP && (TYPEOF(vl) == CLOSXP ||
				   TYPEOF(vl) == BUILTINSXP ||
				   TYPEOF(vl) == SPECIALSXP))
		return (vl);
	}
	if (inherits_)
	    rho = ENCLOS(rho);
	else
	    return R_UnboundValue;
    }
    return R_UnboundValue;
}

/** @brief Look up a symbol in an environment.
 * 
 * Ignore any values which are not of the specified mode.
 */
static SEXP findVar1mode(SEXP symbol, SEXP rho, SEXPTYPE mode, int inherits,
	     Rboolean doGet)
{
    SEXP vl;
    SEXPTYPE tl;
    if (mode == INTSXP) mode = REALSXP;
    if (mode == FUNSXP || mode ==  BUILTINSXP || mode == SPECIALSXP)
	mode = CLOSXP;
    while (rho != R_EmptyEnv) {
	if (! doGet && mode == ANYSXP)
	    vl = existsVarInFrame(rho, symbol) ? nullptr : R_UnboundValue;
	else
	    vl = findVarInFrame3(rho, symbol, doGet);

	if (vl != R_UnboundValue) {
	    if (mode == ANYSXP) return vl;
	    if (TYPEOF(vl) == PROMSXP) {
		PROTECT(vl);
		vl = eval(vl, rho);
		UNPROTECT(1);
	    }
	    tl = TYPEOF(vl);
	    if (tl == INTSXP) tl = REALSXP;
	    if (tl == FUNSXP || tl ==  BUILTINSXP || tl == SPECIALSXP)
		tl = CLOSXP;
	    if (tl == mode) return vl;
	}
	if (inherits)
	    rho = ENCLOS(rho);
	else
	    return R_UnboundValue;
    }
    return R_UnboundValue;
}


/*
   ddVal ("dot-dot-value"):
   a function to take a name and determine if it is of the form
   ..x where x is an integer; if so x is returned otherwise 0 is returned
*/
static int ddVal(SEXP symbol)
{
    Symbol *sym = SEXP_downcast<Symbol *>(symbol);
    return sym->dotDotIndex();
}

inline static R_len_t length_DOTS(SEXP _v_) { return (TYPEOF(_v_) == DOTSXP ? Rf_length(_v_) : 0); }

SEXP ddfind(int i, SEXP rho)
{
    if(i <= 0)
	error(_("indexing '...' with non-positive index %d"), i);
    /* first look for ... symbol  */
    SEXP vl = findVar(R_DotsSymbol, rho);
    if (vl != R_UnboundValue) {
	if (length_DOTS(vl) >= i) {
	    vl = nthcdr(vl, i - 1);
	    return CAR(vl);
	}
	else // length(...) < i
	    error(n_("the ... list contains fewer than %d element",
			   "the ... list contains fewer than %d elements", i),
                  i);
    }
    else error(_("'..%d' used in an incorrect context, no ... to look in"), i);

    return nullptr;
}

/** @brief This function fetches the variables ..1, ..2, etc from the first
 * frame of the environment passed as the second argument to ddfindVar.
 * 
 * These variables are implicitly defined whenever a ... object is
 * created.
 * 
 * To determine values for the variables we first search for an
 * explicit definition of the symbol, them we look for a ... object in
 * the frame and then walk through it to find the appropriate values.
 * If no value is obtained we return R_UnboundValue.
 * 
 * It is an error to specify a .. index longer than the length of the
 * ... object the value is sought in.
 */
HIDDEN
SEXP R::ddfindVar(SEXP symbol, SEXP rho)
{
    int i = ddVal(symbol);
    return ddfind(i, rho);
}

HIDDEN SEXP do_dotsElt(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    check1arg(args, call, "n");

    SEXP si = CAR(args);
    if (! isNumeric(si) || XLENGTH(si) != 1)
	errorcall(call, _("indexing '...' with an invalid index"));
    int i = asInteger(si);
    return eval(ddfind(i, env), env);
}

HIDDEN SEXP do_dotsLength(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    SEXP vl = findVar(R_DotsSymbol, env);
    if (vl == R_UnboundValue)
	error(_("incorrect context: the current call has no '...' to look in"));
    // else
    return ScalarInteger(length_DOTS(vl));
}

HIDDEN SEXP do_dotsNames(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    GCStackRoot<> vl(findVar(R_DotsSymbol, env));

    if (vl == R_UnboundValue)
	error(_("incorrect context: the current call has no '...' to look in"));
    // else
    GCStackRoot<> out(allocVector(STRSXP, length_DOTS(vl)));
    for(int i = 0; i < LENGTH(out); i++) {
        SEXP tag = TAG(vl);
        SET_STRING_ELT(out, i, tag == nullptr ? NA_STRING : PRINTNAME(tag));
        vl = CDR(vl);
    }

    return out;
}

#undef length_DOTS

#ifdef UNUSED
/** @brief This function does a variable lookup, but uses dynamic scoping rules
 * rather than the lexical scoping rules used in findVar.
 * 
 * @return return R_UnboundValue if the symbol isn't located and the calling
 * function needs to handle the errors.
 */
SEXP RCNTXT::dynamicfindVar(SEXP symbol, RCNTXT *cptr)
{
    SEXP vl;
    while (cptr != R_ToplevelContext) {
	if (cptr->getCallFlag() & CTXT_FUNCTION) {
	    vl = findVarInFrame3(cptr->workingEnvironment(), symbol, TRUE);
	    if (vl != R_UnboundValue) return vl;
	}
	cptr = cptr->nextContext();
    }
    return R_UnboundValue;
}
#endif



/*----------------------------------------------------------------------

  findFun

  Search for a function in an environment This is a specially modified
  version of findVar which ignores values its finds if they are not
  functions.

 [ NEEDED: This needs to be modified so that a search for an arbitrary mode can
  be made.  Then findVar and findFun could become same function.]

  This could call findVar1.  NB: they behave differently on failure.
*/

/*HIDDEN*/
SEXP Rf_findFun3(SEXP symbol, SEXP rho, SEXP call)
{
    SEXP vl;

    /* If the symbol is marked as special, skip to the first
       environment that might contain such a symbol. */
       // TODO: Deal with CXXR_FALSE later.
    if (CXXR_FALSE && IS_SPECIAL_SYMBOL(symbol)) {
	while (rho != R_EmptyEnv && NO_SPECIAL_SYMBOLS(rho))
	    rho = ENCLOS(rho);
    }

    while (rho != R_EmptyEnv) {
	/* This is not really right.  Any variable can mask a function */
#ifdef USE_GLOBAL_CACHE
	if (rho == R_GlobalEnv)
#ifdef FAST_BASE_CACHE_LOOKUP
	    if (BASE_SYM_CACHED(symbol))
		vl = SYMBOL_BINDING_VALUE(symbol);
	    else
		vl = findGlobalVar(symbol);
#else
	    vl = findGlobalVar(symbol);
#endif
	else
	    vl = findVarInFrame3(rho, symbol, TRUE);
#else
	vl = findVarInFrame3(rho, symbol, TRUE);
#endif
	if (vl != R_UnboundValue) {
	    if (TYPEOF(vl) == PROMSXP) {
		SEXP pv = PRVALUE(vl);
		if (pv != R_UnboundValue)
		    vl = pv;
		else {
		    PROTECT(vl);
		    vl = eval(vl, rho);
		    UNPROTECT(1);
		}
	    }
	    if (TYPEOF(vl) == CLOSXP || TYPEOF(vl) == BUILTINSXP ||
		TYPEOF(vl) == SPECIALSXP)
		return (vl);
	    if (vl == R_MissingArg)
		errorcall(call, _("'%s' argument is missing, with no default"), CHAR(PRINTNAME(symbol)));
	}
	rho = ENCLOS(rho);
    }
    errorcall_cpy(call, _("could not find function '%s'"), EncodeChar(PRINTNAME(symbol)));
    /* NOT REACHED */
    return R_UnboundValue;
}

SEXP Rf_findFun(SEXP symbol, SEXP rho)
{
    return Rf_findFun3(symbol, rho, R_CurrentExpression);
}

/** @brief Assign a value in a specific environment frame.
 */
void Rf_defineVar(SEXP symbol, SEXP value, SEXP rho)
{
    int hashcode;
    SEXP frame, c;

    if (value == R_UnboundValue)
        error(_("attempt to bind a variable to R_UnboundValue"));
    /* R_DirtyImage should only be set if assigning to R_GlobalEnv. */
    if (rho == R_GlobalEnv)
        R_DirtyImage = 1;

    if (!rho || rho == R_EmptyEnv)
        error(_("cannot assign values in the empty environment"));

    if(IS_USER_DATABASE(rho)) {
	R_ObjectTable *table;
	table = (R_ObjectTable *) R_ExternalPtrAddr(HASHTAB(rho));
	if(table->assign == nullptr)
	    error(_("cannot assign variables to this database"));
	PROTECT(value);
	table->assign(CHAR(PRINTNAME(symbol)), value, table);
	UNPROTECT(1);
#ifdef USE_GLOBAL_CACHE
	if (IS_GLOBAL_FRAME(rho)) R_FlushGlobalCache(symbol);
#endif
	return;
    }

    if (rho == R_BaseNamespace || rho == R_BaseEnv) {
	gsetVar(symbol, value, rho);
    } else {
#ifdef USE_GLOBAL_CACHE
	if (IS_GLOBAL_FRAME(rho)) R_FlushGlobalCache(symbol);
#endif

	if (IS_SPECIAL_SYMBOL(symbol))
	    UNSET_NO_SPECIAL_SYMBOLS(rho);

	if (HASHTAB(rho) == nullptr) {
	    /* First check for an existing binding */
	    frame = FRAME(rho);
	    while (frame != nullptr) {
		if (TAG(frame) == symbol) {
		    SET_BINDING_VALUE(frame, value);
		    SET_MISSING(frame, 0);	/* Over-ride */
		    return;
		}
		frame = CDR(frame);
	    }
	    if (FRAME_IS_LOCKED(rho))
		error(_("cannot add bindings to a locked environment"));
	    SET_FRAME(rho, CONS(value, FRAME(rho)));
	    SET_TAG(FRAME(rho), symbol);
	}
	else {
	    c = PRINTNAME(symbol);
	    hashcode = HASHVALUE(c) % HASHSIZE(HASHTAB(rho));
	    R_HashSet(hashcode, symbol, HASHTAB(rho), value,
		      FRAME_IS_LOCKED(rho));
	    if (R_HashSizeCheck(HASHTAB(rho)))
		SET_HASHTAB(rho, R_HashResize(HASHTAB(rho)));
	}
    }
}

/** @brief Add given variables (addVars - list) to given environment (env) unless
 * they are already there.
 * 
 * Env is a "new" environment, created by NewEnvironment, as in applyClosure (so it list-based).
 * Slots for vars are re-used.  The addVars list itself can have duplicit variables.
 * 
 * The implementation is performance optimized towards the common case that
 * the variables from addVars are not present in env and that addVars does
 * not have duplicit variables.
 */
void Rf_addMissingVarsToNewEnv(SEXP env, SEXP addVars)
{
    if (!addVars)
        return;

    /* temporary sanity check */
    if (TYPEOF(addVars) == ENVSXP)
        Rf_error(_("additional variables should now be passed as a list, not an environment"));

    /* append variables from env after addVars */
    SEXP aprev = addVars;
    SEXP a = CDR(addVars);
    while (a) {
	aprev = a;
	a = CDR(a);
    }
    SETCDR(aprev, FRAME(env));
    SET_FRAME(env, addVars);

    /* remove duplicates - a variable listed later has precedence over a
       variable listed sooner */
    SEXP end;
    for(end = CDR(addVars); end; end = CDR(end)) {
	SEXP endTag = TAG(end);
	SEXP sprev = nullptr;
	SEXP s;
	for(s = addVars; s != end; s = CDR(s)) {
	    if (TAG(s) == endTag) {
		/* remove variable s from the list, because it is overridden by "end" */
		if (sprev == nullptr) {
		    addVars = CDR(s);
		    SET_FRAME(env, addVars);
		} else
		    SETCDR(sprev, CDR(s));
	    } else
		sprev = s;
	}
    }
}

/** @brief Assign a new value to an existing symbol in a frame.
 * 
 * @return Return the symbol if successful and nullptr if not.
 * 
 * @note Taken static in 2.4.0: not called for emptyenv or baseenv.
 */
static SEXP setVarInFrame(SEXP rho, SEXP symbol, SEXP value)
{
    int hashcode;
    SEXP frame, c;

    /* R_DirtyImage should only be set if assigning to R_GlobalEnv. */
    if (rho == R_GlobalEnv)
        R_DirtyImage = 1;
    if (!rho || rho == R_EmptyEnv)
        return nullptr;

    if(IS_USER_DATABASE(rho)) {
	/* FIXME: This does not behave as described */
	R_ObjectTable *table;
	table = (R_ObjectTable *) R_ExternalPtrAddr(HASHTAB(rho));
	if(table->assign == nullptr)
	    error(_("cannot assign variables to this database"));
	PROTECT(value);
	SEXP result = table->assign(CHAR(PRINTNAME(symbol)), value, table);
	UNPROTECT(1);
	return result;
    }

    if (rho == R_BaseNamespace || rho == R_BaseEnv) {
	if (SYMVALUE(symbol) == R_UnboundValue) return nullptr;
	SET_SYMBOL_BINDING_VALUE(symbol, value);
	return symbol;
    }

    if (HASHTAB(rho) == nullptr) {
	frame = FRAME(rho);
	while (frame != nullptr) {
	    if (TAG(frame) == symbol) {
		SET_BINDING_VALUE(frame, value);
		SET_MISSING(frame, 0);	/* same as defineVar */
		return symbol;
	    }
	    frame = CDR(frame);
	}
    } else {
	/* Do the hash table thing */
	c = PRINTNAME(symbol);
	hashcode = HASHVALUE(c) % HASHSIZE(HASHTAB(rho));
	frame = R_HashGetLoc(hashcode, symbol, HASHTAB(rho));
	if (frame != nullptr) {
	    SET_BINDING_VALUE(frame, value);
	    SET_MISSING(frame, 0);	/* same as defineVar */
	    return symbol;
	}
    }
    return nullptr; /* -Wall */
}

/** @brief Assign a new value to bound symbol.
 * 
 * Note this does the "inherits" case.
 * I.e. it searches frame-by-frame for a symbol and binds the
 * given value to the first symbol encountered.  If no symbol is
 * found then a binding is created in the global environment.
 * 
 * Changed in R 2.4.0 to look in the base environment (previously the
 * search stopped befor the base environment, but would (and still
 * does) assign into the base namespace if that is on the search and
 * the symbol existed there).
 */
void Rf_setVar(SEXP symbol, SEXP value, SEXP rho)
{
    SEXP vl;
    while (rho != R_EmptyEnv)
    {
        vl = setVarInFrame(rho, symbol, value);
        if (vl)
            return;
        rho = ENCLOS(rho);
    }
    defineVar(symbol, value, R_GlobalEnv);
}

/** @brief Assignment in the base environment.
 * 
 * Here we assign directly into the base environment.
 */
void Rf_gsetVar(SEXP symbol, SEXP value, SEXP rho)
{
    if (FRAME_IS_LOCKED(rho)) {
	if(SYMVALUE(symbol) == R_UnboundValue)
	    error(_("cannot add binding of '%s' to the base environment"), CHAR(PRINTNAME(symbol)));
    }
#ifdef USE_GLOBAL_CACHE
    R_FlushGlobalCache(symbol);
#endif
    SET_SYMBOL_BINDING_VALUE(symbol, value);
}

/** @brief Get environment from a subclass if possible; else return nullptr.
 */
inline static SEXP simple_as_environment(SEXP arg)
{
	return (IS_S4_OBJECT(arg) && (TYPEOF(arg) == S4SXP) ? R_getS4DataSlot(arg, ENVSXP) : nullptr);
}

/**
 * @example .Internal(assign(x, value, envir, inherits))
 */
HIDDEN SEXP do_assign(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP name=nullptr, val, aenv;
    int ginherits = 0;
    checkArity(op, args);

    if (!isString(CAR(args)) || length(CAR(args)) == 0)
	error(_("invalid first argument"));
    else {
	if (length(CAR(args)) > 1)
	    warning(_("only the first element is used as variable name"));
	name = installTrChar(STRING_ELT(CAR(args), 0));
    }
    PROTECT(val = CADR(args));
    aenv = CADDR(args);
    if (TYPEOF(aenv) == NILSXP)
	error(_("use of NULL environment is defunct"));
    if (TYPEOF(aenv) != ENVSXP &&
	TYPEOF((aenv = simple_as_environment(aenv))) != ENVSXP)
	error(_("invalid '%s' argument"), "envir");
    ginherits = asLogical(CADDDR(args));
    if (ginherits == NA_LOGICAL)
	error(_("invalid '%s' argument"), "inherits");
    if (ginherits)
	setVar(name, val, aenv);
    else
	defineVar(name, val, aenv);
    UNPROTECT(1);
    return val;
}


/**
 * @example .Internal(list2env(x, envir))
 */
HIDDEN SEXP do_list2env(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP x, xnms, envir;
    int n;
    checkArity(op, args);

    if (TYPEOF(CAR(args)) != VECSXP)
	error(_("first argument must be a named list"));
    x = CAR(args);
    n = LENGTH(x);
    xnms = getAttrib(x, R_NamesSymbol);
    PROTECT(xnms);
    if (n && (TYPEOF(xnms) != STRSXP || LENGTH(xnms) != n))
	error(_("'names(x)' must be a character vector of the same length as 'x'"));
    envir = CADR(args);
    if (TYPEOF(envir) != ENVSXP)
	error(_("'%s' argument must be an environment"), "envir");

    for(int i = 0; i < n; i++) {
	SEXP name = installTrChar(STRING_ELT(xnms, i));
	defineVar(name, lazy_duplicate(VECTOR_ELT(x, i)), envir);
    }
    UNPROTECT(1); /* xnms */

    return envir;
}

static int RemoveVariable(SEXP name, int hashcode, SEXP env)
{
    bool found = false;

    if (env == R_BaseNamespace)
        error(_("cannot remove variables from base namespace"));
    if (env == R_BaseEnv)
        error(_("cannot remove variables from the base environment"));
    if (env == R_EmptyEnv)
        error(_("cannot remove variables from the empty environment"));
    if (FRAME_IS_LOCKED(env))
        error(_("cannot remove bindings from a locked environment"));

    if(IS_USER_DATABASE(env)) {
	R_ObjectTable *table;
	table = (R_ObjectTable *) R_ExternalPtrAddr(HASHTAB(env));
	if(table->remove == nullptr)
	    error(_("cannot remove variables from this database"));
	return table->remove(CHAR(PRINTNAME(name)), table);
    }

    if (IS_HASHED(env)) {
	found = R_HashDelete(hashcode, name, env);
#ifdef USE_GLOBAL_CACHE
	if (found && IS_GLOBAL_FRAME(env))
	    R_FlushGlobalCache(name);
#endif
    } else {
	auto list = RemoveFromList(name, FRAME(env));
    found = list.second;
	if (list.second) {
	    if(env == R_GlobalEnv) R_DirtyImage = 1;
	    SET_FRAME(env, list.first);
#ifdef USE_GLOBAL_CACHE
	    if (IS_GLOBAL_FRAME(env))
		R_FlushGlobalCache(name);
#endif
	}
    }
    return found;
}

/** @brief Remove elements for an environment
 * 
 * There are three arguments to do_remove; a list of names to remove,
 * an optional environment (if missing set it to R_GlobalEnv) and
 * inherits, a logical indicating whether to look in the parent env if
 * a symbol is not found in the supplied env.  This is ignored if
 * environment is not specified.
 * 
 * @example .Internal(remove(list, envir, inherits))
 */
HIDDEN SEXP do_remove(SEXP call, SEXP op, SEXP args, SEXP rho)
{

    SEXP name, envarg, tsym, tenv;
    int ginherits = 0;
    int done, i, hashcode;
    checkArity(op, args);

    name = CAR(args);
    if (!isString(name))
	error(_("invalid first argument"));
    args = CDR(args);

    envarg = CAR(args);
    if (TYPEOF(envarg) == NILSXP)
	error(_("use of NULL environment is defunct"));
    if (TYPEOF(envarg) != ENVSXP &&
	TYPEOF((envarg = simple_as_environment(envarg))) != ENVSXP)
	error(_("invalid '%s' argument"), "envir");
    args = CDR(args);

    ginherits = asLogical(CAR(args));
    if (ginherits == NA_LOGICAL)
	error(_("invalid '%s' argument"), "inherits");

    for (i = 0; i < LENGTH(name); i++) {
	done = 0;
	tsym = installTrChar(STRING_ELT(name, i));
	hashcode = HASHVALUE(PRINTNAME(tsym));
	tenv = envarg;
	while (tenv != R_EmptyEnv) {
	    done = RemoveVariable(tsym, hashcode, tenv);
	    if (done || !ginherits)
		break;
	    tenv = ENCLOS(tenv);
	}
	if (!done)
	    warning(_("object '%s' was not found"), EncodeChar(PRINTNAME(tsym)));
    }
    return nullptr;
}

void R_removeVarFromFrame(SEXP name, SEXP env)
{
    int hashcode = -1;

    if (TYPEOF(env) == NILSXP)
        error(_("use of NULL environment is defunct"));

    if (!isEnvironment(env))
        error(_("argument to '%s' is not an environment"), "R_removeVarFromFrame");

    if (TYPEOF(name) != SYMSXP)
        error(_("not a symbol"));

    if (IS_HASHED(env)) {
	    hashcode = HASHVALUE(PRINTNAME(name));
    }
    RemoveVariable(name, hashcode, env);
}

/** @brief This function returns the SEXP associated with the character
 * argument.
 * 
 * It needs the environment of the calling function as a default.
 * 
 * @example exists (x, envir, mode, inherits)
 * @example get    (x, envir, mode, inherits)
 * @example get0   (x, envir, mode, inherits, value_if_not_exists)
 */
HIDDEN SEXP do_get(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP rval, genv, t1 = nullptr;
    SEXPTYPE gmode;
    int ginherits = 0, where;
    checkArity(op, args);

    /* The first arg is the object name */
    /* It must be present and a non-empty string */

    if (TYPEOF(CAR(args)) == SYMSXP)
	t1 = CAR(args);
    else if (isValidStringF(CAR(args))) {
	if (XLENGTH(CAR(args)) > 1)
	    error(_("first argument has length > 1"));
	t1 = installTrChar(STRING_ELT(CAR(args), 0));
    }
    else
	error(_("invalid first argument"));

    /* envir :	originally, the "where=" argument */

    if (TYPEOF(CADR(args)) == REALSXP || TYPEOF(CADR(args)) == INTSXP) {
	where = asInteger(CADR(args));
	genv = R_GlobalContext->R_sysframe(where);
    }
    else if (TYPEOF(CADR(args)) == NILSXP) {
	error(_("use of NULL environment is defunct"));
	genv = nullptr;  /* -Wall */
    }
    else if (TYPEOF(CADR(args)) == ENVSXP)
	genv = CADR(args);
    else if(TYPEOF((genv = simple_as_environment(CADR(args)))) != ENVSXP) {
	error(_("invalid '%s' argument"), "envir");
	genv = nullptr;  /* -Wall */
    }

    /* mode :  The mode of the object being sought */

    /* as from R 1.2.0, this is the *mode*, not the *typeof* aka
       storage.mode.
    */

    if (isString(CADDR(args))) {
	if (streql(CHAR(STRING_ELT(CADDR(args), 0)), "function")) /* ASCII */
	    gmode = FUNSXP;
	else
	    gmode = str2type(CHAR(STRING_ELT(CADDR(args), 0))); /* ASCII */
    } else {
	error(_("invalid '%s' argument"), "mode");
	gmode = FUNSXP;/* -Wall */
    }

    ginherits = asLogical(CADDDR(args));
    if (ginherits == NA_LOGICAL)
	error(_("invalid '%s' argument"), "inherits");

    /* Search for the object */
    rval = findVar1mode(t1, genv, gmode, ginherits, Rboolean(PRIMVAL(op)));
    if (rval == R_MissingArg)
	error(_("'%s' argument is missing, with no default"),
	      CHAR(PRINTNAME(t1)));

    switch (PRIMVAL(op) ) {
    case 0: // exists(.) :
	return ScalarLogical(rval != R_UnboundValue);
	break;

    case 1: // have get(.)
	if (rval == R_UnboundValue) {
	    if (gmode == ANYSXP)
		error(_("object '%s' was not found"), EncodeChar(PRINTNAME(t1)));
	    else
		error(_("object '%s' of mode '%s' was not found"),
		      CHAR(PRINTNAME(t1)),
		      CHAR(STRING_ELT(CADDR(args), 0))); /* ASCII */
	}

#define GET_VALUE(rval)                              \
    do                                               \
    {                                                \
        /* We need to evaluate if it is a promise */ \
        if (TYPEOF(rval) == PROMSXP)                 \
        {                                            \
            PROTECT(rval);                           \
            rval = eval(rval, genv);                 \
            UNPROTECT(1);                            \
        }                                            \
        ENSURE_NAMED(rval);                          \
    } while (0)

    GET_VALUE(rval);
	break;

    case 2: // get0(.)
	if (rval == R_UnboundValue)
	    return CAD4R(args);// i.e.  value_if_not_exists
	GET_VALUE(rval);
	break;
    }
    return rval;
}
#undef GET_VALUE

static SEXP gfind(const char *name, SEXP env, SEXPTYPE mode,
		  SEXP ifnotfound, int inherits, SEXP enclos)
{
    SEXP rval, t1, R_fcall, var;

    t1 = install(name);

    /* Search for the object - last arg is 1 to 'get' */
    rval = findVar1mode(t1, env, mode, inherits, TRUE);

    if (rval == R_UnboundValue) {
	if( isFunction(ifnotfound) ) {
	    PROTECT(var = mkString(name));
	    PROTECT(R_fcall = GCNode::expose(new Expression(ifnotfound, {var})));
	    rval = eval(R_fcall, enclos);
	    UNPROTECT(2);
	} else
	    rval = ifnotfound;
    }

    /* We need to evaluate if it is a promise */
    if (TYPEOF(rval) == PROMSXP) {
	PROTECT(rval);
	rval = eval(rval, env);
	UNPROTECT(1);
    }
    ENSURE_NAMED(rval);
    return rval;
}


/** @brief Get multiple values from an environment
 * 
 * @example .Internal(mget(x, envir, mode, ifnotfound, inherits))
 * 
 * @return  a list of the same length as x, a character vector (of names).
 */
HIDDEN SEXP do_mget(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, env, x, mode, ifnotfound;
    int ginherits = 0, nvals, nmode, nifnfnd;

    checkArity(op, args);

    x = CAR(args);

    nvals = length(x);

    /* The first arg is the object name */
    /* It must be present and a string */
    if (!isString(x) )
	error(_("invalid first argument"));
    for(int i = 0; i < nvals; i++)
	if( isNull(STRING_ELT(x, i)) || !CHAR(STRING_ELT(x, 0))[0] )
	    error(_("invalid name in position %d"), i+1);

    env = CADR(args);
    if (ISNULL(env)) {
	error(_("use of NULL environment is defunct"));
    } else if( !isEnvironment(env) )
	error(_("second argument must be an environment"));

    mode = CADDR(args);
    nmode = length(mode);
    if( !isString(mode) )
	error(_("invalid '%s' argument"), "mode");

    if( nmode != nvals && nmode != 1 )
	error(_("wrong length for '%s' argument"), "mode");

    PROTECT(ifnotfound = coerceVector(CADDDR(args), VECSXP));
    nifnfnd = length(ifnotfound);
    if( !isVector(ifnotfound) )
	error(_("invalid '%s' argument"), "ifnotfound");

    if( nifnfnd != nvals && nifnfnd != 1 )
	error(_("wrong length for '%s' argument"), "ifnotfound");

    ginherits = asLogical(CAD4R(args));
    if (ginherits == NA_LOGICAL)
	error(_("invalid '%s' argument"), "inherits");

    PROTECT(ans = allocVector(VECSXP, nvals));

    for(int i = 0; i < nvals; i++) {
	SEXPTYPE gmode;
	if (streql(CHAR(STRING_ELT(CADDR(args), i % nmode)), "function"))
	    gmode = FUNSXP;
	else {
	    gmode = Rf_str2type(CHAR(STRING_ELT(CADDR(args), i % nmode)));
	    if(gmode == (SEXPTYPE) (-1))
		error(_("invalid '%s' argument"), "mode");
	}
	SEXP ans_i = gfind(translateChar(STRING_ELT(x, i % nvals)), env,
			   gmode, VECTOR_ELT(ifnotfound, i % nifnfnd),
			   ginherits, rho);
	SET_VECTOR_ELT(ans, i, lazy_duplicate(ans_i));
    }

    setAttrib(ans, R_NamesSymbol, lazy_duplicate(x));
    UNPROTECT(2);
    return ans;
}

static SEXP findRootPromise(SEXP p)
{
    if (TYPEOF(p) == PROMSXP)
    {
        while (TYPEOF(PREXPR(p)) == PROMSXP)
        {
            p = PREXPR(p);
        }
    }
    return p;
}

HIDDEN bool R::R_isMissing(SEXP symbol, SEXP rho)
{
    int ddv = 0;
    SEXP s;
    GCRoot<> vl; // Binding defined in PairList form

    if (symbol == R_MissingArg) /* Yes, this can happen */
	return true;

    /* check for infinite recursion */
    R_CheckStack();

    if (DDVAL(symbol)) {
	s = R_DotsSymbol;
	ddv = ddVal(symbol);
    }
    else
	s = symbol;

    if (rho == R_BaseEnv || rho == R_BaseNamespace)
        return false; /* is this really the right thing to do? LT */

    vl = findVarLocInFrame(rho, s, nullptr);
    if (vl) {
	if (DDVAL(symbol)) {
	    if (length(CAR(vl)) < ddv || CAR(vl) == R_MissingArg)
		return true;
	    /* defineVar(symbol, value, R_GlobalEnv); */
	    else
		vl = nthcdr(CAR(vl), ddv-1);
	}
	if (MISSING(vl) == 1 ||
	    (BNDCELL_TAG(vl) == NILSXP && CAR(vl) == R_MissingArg))
	    return 1;
	if (IS_ACTIVE_BINDING(vl))
	    return false;
	if (BNDCELL_TAG(vl))
	    return false;
	SETCAR(vl, findRootPromise(CAR(vl)));
	if (TYPEOF(CAR(vl)) == PROMSXP &&
	    PRVALUE(CAR(vl)) == R_UnboundValue &&
	    TYPEOF(PREXPR(CAR(vl))) == SYMSXP) {
	    /* This code uses the PRSEEN value to detect cycles.  If a
	       cycle occurs then a missing argument was encountered,
	       so the return value is TRUE.  It would be a little
	       safer to use the promise stack to ensure unsetting of
	       the bits in the event of a longjump, but doing so would
	       require distinguishing between evaluating promises and
	       checking for missingness.  Because of the test above
	       for an active binding a longjmp should only happen if
	       the stack check fails.  LT */
	    if (PRSEEN(CAR(vl)) == Promise::EvaluationStatus::UNDER_EVALUATION)
		return true;
	    else {
		bool val;
		int oldseen = PRSEEN(CAR(vl));
		SET_PRSEEN(CAR(vl), Promise::EvaluationStatus::UNDER_EVALUATION);
		PROTECT(vl);
		val = R_isMissing(PREXPR(CAR(vl)), PRENV(CAR(vl)));
		UNPROTECT(1); /* vl */
		/* The oldseen value will usually be DEFAULT, but might be INTERRUPTED
		   from an interrupted evaluation. LT */
		SET_PRSEEN(CAR(vl), oldseen);
		return val;
	    }
	}
	else
	    return false;
    }
    return false;
}

/** @brief This function tests whether the symbol passed as its first argument
 * is a missing argument to the current closure.  rho is the
 * environment that missing was called from.
 * 
 * This is primitive and a SPECIALSXP
 * 
 * R_isMissing is called on the not-yet-evaluated value of an argument,
 * if this is a symbol, as it could be a missing argument that has been
 * passed down.  So 'symbol' is the promise value, and 'rho' its
 * evaluation argument.
 * 
 * @note It is also called in arithmetic.cpp. for e.g. do_log
 */
HIDDEN SEXP do_missing(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int ddv=0;
    SEXP sym, s;
    GCStackRoot<> rval;
    GCStackRoot<> t; // Binding defined in PairList form

    checkArity(op, args);
    check1arg(args, call, "x");
    s = sym = CAR(args);
    if( isString(sym) && length(sym)==1 )
	s = sym = installTrChar(STRING_ELT(CAR(args), 0));
    if (!isSymbol(sym))
	errorcall(call, _("invalid use of 'missing'"));

    if (DDVAL(sym)) {
	ddv = ddVal(sym);
	sym = R_DotsSymbol;
    }

    t = findVarLocInFrame(rho, sym, nullptr);
    rval = allocVector(LGLSXP,1);
    LOGICAL(rval)[0] = 0;

    if (t) {
	if (DDVAL(s)) {
	    if (length(CAR(t)) < ddv  || CAR(t) == R_MissingArg) {
		LOGICAL(rval)[0] = 1;
		return rval;
	    }
	    else
		t = nthcdr(CAR(t), ddv-1);
	}
	if (BNDCELL_TAG(t)) return rval;
	if (MISSING(t) || CAR(t) == R_MissingArg) {
	    LOGICAL(rval)[0] = 1;
	    return rval;
	}
	else goto havebinding;
    }
    else  /* it wasn't an argument to the function */
	errorcall(call, _("'missing' can only be used for arguments"));

 havebinding:

    t = CAR(t);
    if (TYPEOF(t) != PROMSXP) {
	LOGICAL(rval)[0] = 0;
	return rval;
    }

    t = findRootPromise(t);
    if (!isSymbol(PREXPR(t))) LOGICAL(rval)[0] = 0;
    else {
	PROTECT(rval);
	LOGICAL(rval)[0] = R_isMissing(PREXPR(t), PRENV(t));
	UNPROTECT(1);
    }
    return rval;
}

/**
 * @return the current global environment.
 */
HIDDEN SEXP do_globalenv(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    return R_GlobalEnv;
}

/**
 * @return the current base environment.
 */
HIDDEN SEXP do_baseenv(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    return R_BaseEnv;
}

/**
 * @return the current empty environment.
 */
HIDDEN SEXP do_emptyenv(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    return R_EmptyEnv;
}

/*----------------------------------------------------------------------

  do_attach

  To attach a list we make up an environment and insert components
  of the list in as the values of this env and install the tags from
  the list as the names.

*/
HIDDEN SEXP do_attach(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP name, s, t, x;
    int pos, hsize;
    bool isSpecial;

    checkArity(op, args);

    pos = asInteger(CADR(args));
    if (pos == NA_INTEGER)
        error(_("'%s' argument must be an integer"), "pos");

    name = CADDR(args);
    if (!isValidStringF(name))
        error(_("invalid '%s' argument"), "name");

    isSpecial = IS_USER_DATABASE(CAR(args));

    if(!isSpecial) {
	if (isNewList(CAR(args))) {
	    SETCAR(args, VectorToPairList(CAR(args)));

	    for (x = CAR(args); x != nullptr; x = CDR(x))
		if (TAG(x) == nullptr)
		    error(_("all elements of a list must be named"));
	    GCStackRoot<PairList> dupcar(SEXP_downcast<PairList*>(shallow_duplicate(CAR(args))));
        PROTECT(s = GCNode::expose(new Environment(nullptr, dupcar)));
	} else if (isEnvironment(CAR(args))) {
	    SEXP p, loadenv = CAR(args);

	    PROTECT(s = GCNode::expose(new Environment()));
	    if (HASHTAB(loadenv) != nullptr) {
		int i, n;
		n = length(HASHTAB(loadenv));
		for (i = 0; i < n; i++) {
		    p = VECTOR_ELT(HASHTAB(loadenv), i);
		    while (p != nullptr) {
			defineVar(TAG(p), lazy_duplicate(CAR(p)), s);
			p = CDR(p);
		    }
		}
		/* FIXME: duplicate the hash table and assign here */
	    } else {
            GCStackRoot<> framelist(FRAME(loadenv));
            for (p = framelist; p != nullptr; p = CDR(p))
                defineVar(TAG(p), lazy_duplicate(CAR(p)), s);
	    }
	} else {
	    error(_("'attach()' function only works for lists, data frames and environments"));
	    s = nullptr; /* -Wall */
	}

	/* Connect FRAME(s) into HASHTAB(s) */
	if (length(s) < HASHMINSIZE)
	    hsize = HASHMINSIZE;
	else
	    hsize = length(s);

	SET_HASHTAB(s, R_NewHashTable(hsize));
	s = R_HashFrame(s);

	/* FIXME: A little inefficient */
	while (R_HashSizeCheck(HASHTAB(s)))
	    SET_HASHTAB(s, R_HashResize(HASHTAB(s)));

    } else { /* is a user object */
	/* Having this here (rather than below) means that the onAttach routine
	   is called before the table is attached. This may not be necessary or
	   desirable. */
	R_ObjectTable *tb = (R_ObjectTable*) R_ExternalPtrAddr(CAR(args));
	if(tb->onAttach)
	    tb->onAttach(tb);
	PROTECT(s = GCNode::expose(new Environment()));
	SET_HASHTAB(s, CAR(args));
	setAttrib(s, R_ClassSymbol, getAttrib(HASHTAB(s), R_ClassSymbol));
    }

    setAttrib(s, R_NameSymbol, name);
    for (t = R_GlobalEnv; ENCLOS(t) != R_BaseEnv && pos > 2; t = ENCLOS(t))
	pos--;

    if (ENCLOS(t) == R_BaseEnv)
    {
        SET_ENCLOS(t, s);
        SET_ENCLOS(s, R_BaseEnv);
    }
    else
    {
        x = ENCLOS(t);
        SET_ENCLOS(t, s);
        SET_ENCLOS(s, x);
    }
#ifdef USE_GLOBAL_CACHE
    if(!isSpecial) { /* Temporary: need to remove the elements identified by objects(CAR(args)) */
	R_FlushGlobalCacheFromTable(HASHTAB(s));
	MARK_AS_GLOBAL_FRAME(s);
    } else {
	R_FlushGlobalCacheFromUserTable(HASHTAB(s));
	MARK_AS_GLOBAL_FRAME(s);
    }
#endif
    UNPROTECT(1); /* s */
    return s;
}



/** @brief Detach the specified environment.
 * 
 * Detachment only takes place by position.
 */
HIDDEN SEXP do_detach(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP s, t, x;
    int pos, n;
    bool isSpecial = false;

    checkArity(op, args);
    pos = asInteger(CAR(args));

    for (n = 2, t = ENCLOS(R_GlobalEnv); t != R_BaseEnv; t = ENCLOS(t))
	n++;

    if (pos == n) /* n is the length of the search list */
	error(_("detaching \"package:base\" is not allowed"));

    for (t = R_GlobalEnv ; ENCLOS(t) != R_BaseEnv && pos > 2 ; t = ENCLOS(t))
	pos--;
    if (pos != 2) {
	error(_("invalid '%s' argument"), "pos");
	s = t;	/* for -Wall */
    }
    else {
	PROTECT(s = ENCLOS(t));
	x = ENCLOS(s);
	SET_ENCLOS(t, x);
	isSpecial = IS_USER_DATABASE(s);
	if(isSpecial) {
	    R_ObjectTable *tb = (R_ObjectTable*) R_ExternalPtrAddr(HASHTAB(s));
	    if(tb->onDetach) tb->onDetach(tb);
	}

	SET_ENCLOS(s, R_BaseEnv);
    }
#ifdef USE_GLOBAL_CACHE
    if(!isSpecial) {
	R_FlushGlobalCacheFromTable(HASHTAB(s));
	MARK_AS_LOCAL_FRAME(s);
    } else {
	R_FlushGlobalCacheFromUserTable(HASHTAB(s));
	MARK_AS_LOCAL_FRAME(s); /* was _GLOBAL_ prior to 2.4.0 */
    }
#endif
    UNPROTECT(1);
    return s;
}



/** @brief Print out the current search path.
 */
HIDDEN SEXP do_search(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, name, t;
    int i, n;

    checkArity(op, args);
    n = 2;
    for (t = ENCLOS(R_GlobalEnv); t != R_BaseEnv ; t = ENCLOS(t))
	n++;
    PROTECT(ans = allocVector(STRSXP, n));
    /* TODO - what should the name of this be? */
    SET_STRING_ELT(ans, 0, mkChar(".GlobalEnv"));
    SET_STRING_ELT(ans, n-1, mkChar("package:base"));
    i = 1;
    for (t = ENCLOS(R_GlobalEnv); t != R_BaseEnv ; t = ENCLOS(t)) {
	name = getAttrib(t, R_NameSymbol);
	if (!isString(name) || length(name) < 1)
	    SET_STRING_ELT(ans, i, mkChar("(unknown)"));
	else
	    SET_STRING_ELT(ans, i, STRING_ELT(name, 0));
	i++;
    }
    UNPROTECT(1);
    return ans;
}


/** @brief This code implements the functionality of the "ls" and "objects"
 * functions.
 * 
 * @example ls(envir, all.names, sorted)
 */

static int FrameSize(SEXP frame, bool all)
{
    int count = 0;
    while (frame)
    {
        if (all || !isDotSymbol(SEXP_downcast<Symbol *>(TAG(frame))))
            count += 1;
        frame = CDR(frame);
    }
    return count;
}

static void FrameNames(SEXP frame, bool all, SEXP names, int *indx)
{
    while (frame)
    {
        if (all || !isDotSymbol(SEXP_downcast<Symbol *>(TAG(frame))))
        {
            SET_STRING_ELT(names, *indx, PRINTNAME(TAG(frame)));
            (*indx)++;
        }
        frame = CDR(frame);
    }
}

static void FrameValues(SEXP frame, bool all, SEXP values, int *indx)
{
    while (frame)
    {
        if (all || !isDotSymbol(SEXP_downcast<Symbol *>(TAG(frame))))
        {
            SEXP value = BINDING_VALUE(frame);
            if (TYPEOF(value) == PROMSXP)
            {
                PROTECT(value);
                value = Rf_eval(value, R_GlobalEnv);
                UNPROTECT(1);
            }
            SET_VECTOR_ELT(values, *indx, Rf_lazy_duplicate(value));
            (*indx)++;
        }
        frame = CDR(frame);
    }
}

#define CHECK_HASH_TABLE(table)                  \
    do                                           \
    {                                            \
        if (TYPEOF(table) != VECSXP)             \
            error(_("bad hash table contents")); \
    } while (0)

static int HashTableSize(SEXP table, int all)
{
    CHECK_HASH_TABLE(table);
    int count = 0;
    int n = length(table);
    int i;
    for (i = 0; i < n; i++)
	count += FrameSize(VECTOR_ELT(table, i), all);
    return count;
}

static void HashTableNames(SEXP table, int all, SEXP names, int *indx)
{
    CHECK_HASH_TABLE(table);
    int n = length(table);
    int i;
    for (i = 0; i < n; i++)
	FrameNames(VECTOR_ELT(table, i), all, names, indx);
}

static void HashTableValues(SEXP table, int all, SEXP values, int *indx)
{
    CHECK_HASH_TABLE(table);
    int n = length(table);
    int i;
    for (i = 0; i < n; i++)
	FrameValues(VECTOR_ELT(table, i), all, values, indx);
}

static bool BuiltinTest(const Symbol *sym, bool all, bool intern)
{
    if (!sym)
        return false;
    if (intern && sym->internalFunction())
        return true;
    if ((all || !isDotSymbol(sym)) && SYMVALUE(const_cast<Symbol*>(sym)) != R_UnboundValue)
        return true;
    return false;
}

static int BuiltinSize(bool all, bool intern)
{
    int count = 0;
    for (Symbol::const_iterator it = Symbol::begin(); it != Symbol::end(); ++it)
    {
        const Symbol *sym = (*it).second;
        if (BuiltinTest(sym, all, intern))
            ++count;
    }
    return count;
}

static void BuiltinNames(bool all, bool intern, SEXP names, int *indx)
{
    // StringVector *sv = SEXP_downcast<StringVector *>(names);
    for (Symbol::const_iterator it = Symbol::begin(); it != Symbol::end(); ++it)
    {
        const Symbol *sym = (*it).second;
        if (BuiltinTest(sym, all, intern))
            SET_STRING_ELT(names, (*indx)++, const_cast<CachedString *>(sym->name()));
    }
}

static void BuiltinValues(bool all, bool intern, SEXP values, int *indx)
{
    // ListVector *lv = SEXP_downcast<ListVector *>(values);
    for (Symbol::const_iterator it = Symbol::begin(); it != Symbol::end(); ++it)
    {
        Symbol *sym = (*it).second;
        if (BuiltinTest(sym, all, intern))
        {
            RObject *vl = SYMVALUE(sym);
            if (vl && vl->sexptype() == PROMSXP)
            {
                GCStackRoot<> vlr(vl);
                vl = eval(vl, R_BaseEnv);
            }
            SET_VECTOR_ELT(values, (*indx)++, lazy_duplicate(vl));
        }
    }
}

/**
 * @example .Internal(ls(envir, all.names, sorted))
 */
HIDDEN SEXP do_ls(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);

    if(IS_USER_DATABASE(CAR(args))) {
	R_ObjectTable *tb = (R_ObjectTable*)
	    R_ExternalPtrAddr(HASHTAB(CAR(args)));
	return tb->objects(tb);
    }

    SEXP env = CAR(args);

    /* if (env == R_BaseNamespace) env = R_BaseEnv; */

    int all = asLogical(CADR(args));
    if (all == NA_LOGICAL) all = 0;

    int sort_nms = asLogical(CADDR(args)); /* sorted = TRUE/FALSE */
    if (sort_nms == NA_LOGICAL) sort_nms = 0;

    return R_lsInternal3(env, Rboolean(all), Rboolean(sort_nms));
}

/** @brief Takes an environment, a boolean indicating whether to get all
 * names and a boolean if sorted is desired
 */
SEXP R_lsInternal3(SEXP env, Rboolean all, Rboolean sorted)
{
    if(IS_USER_DATABASE(env)) {
	R_ObjectTable *tb = (R_ObjectTable*)
	    R_ExternalPtrAddr(HASHTAB(env));
	return tb->objects(tb);
    }

    /* Step 1 : Compute the Vector Size */
    int k = 0;
    if (env == R_BaseEnv || env == R_BaseNamespace)
	k += BuiltinSize(all, false);
    else if (isEnvironment(env) ||
	isEnvironment(env = simple_as_environment(env))) {
	if (HASHTAB(env) != nullptr)
	    k += HashTableSize(HASHTAB(env), all);
	else
	    k += FrameSize(FRAME(env), all);
    }
    else
	error(_("invalid '%s' argument"), "envir");

    /* Step 2 : Allocate and Fill the Result */
    SEXP ans = PROTECT(allocVector(STRSXP, k));
    k = 0;
    if (env == R_BaseEnv || env == R_BaseNamespace)
	BuiltinNames(all, 0, ans, &k);
    else if (isEnvironment(env)) {
	if (HASHTAB(env) != nullptr)
	    HashTableNames(HASHTAB(env), all, ans, &k);
	else
	    FrameNames(FRAME(env), all, ans, &k);
    }

    if(sorted) sortVector(ans, false);
    UNPROTECT(1);
    return ans;
}

/** @brief non-API version used in several packages
 */
SEXP R_lsInternal(SEXP env, Rboolean all)
{
    return R_lsInternal3(env, all, TRUE);
}

/** @brief transform an environment into a named list
 * 
 * @example as.list.environment(.)
 */
HIDDEN SEXP do_env2list(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP env, ans, names;
    int k, all;

    checkArity(op, args);

    env = CAR(args);
    if (ISNULL(env))
	error(_("use of NULL environment is defunct"));
    if( !isEnvironment(env) ) {
	SEXP xdata;
	if( IS_S4_OBJECT(env) && TYPEOF(env) == S4SXP &&
	    (xdata = R_getS4DataSlot(env, ENVSXP)) != nullptr)
	    env = xdata;
	else
	    error(_("'%s' argument must be an environment"), "env");
    }

    GCStackRoot<> framelist(FRAME(env));
    all = asLogical(CADR(args)); /* all.names = TRUE/FALSE */
    if (all == NA_LOGICAL) all = 0;

    int sort_nms = asLogical(CADDR(args)); /* sorted = TRUE/FALSE */
    if (sort_nms == NA_LOGICAL) sort_nms = 0;

    // k := length(env) = envxlength(env) :
    if (env == R_BaseEnv || env == R_BaseNamespace)
	k = BuiltinSize(all, false);
    else if (HASHTAB(env) != nullptr)
	k = HashTableSize(HASHTAB(env), all);
    else
	k = FrameSize(framelist, all);

    PROTECT(names = allocVector(STRSXP, k));
    PROTECT(ans = allocVector(VECSXP, k));

    k = 0;
    if (env == R_BaseEnv || env == R_BaseNamespace)
	BuiltinValues(all, 0, ans, &k);
    else if (HASHTAB(env) != nullptr)
	HashTableValues(HASHTAB(env), all, ans, &k);
    else
	FrameValues(framelist, all, ans, &k);

    k = 0;
    if (env == R_BaseEnv || env == R_BaseNamespace)
	BuiltinNames(all, 0, names, &k);
    else if (HASHTAB(env) != nullptr)
	HashTableNames(HASHTAB(env), all, names, &k);
    else
	FrameNames(framelist, all, names, &k);

    if(k == 0) { // no sorting, keep NULL names
	UNPROTECT(2);
	return ans;
    }
    if(sort_nms) {
	// return list with *sorted* names
	SEXP sind = PROTECT(allocVector(INTSXP, k));
	int *indx = INTEGER(sind);
	for (int i = 0; i < k; i++) indx[i] = i;
	orderVector1(indx, k, names, /* nalast */ TRUE, /* decreasing */ FALSE,
		     nullptr);
	SEXP ans2   = PROTECT(allocVector(VECSXP, k));
	SEXP names2 = PROTECT(allocVector(STRSXP, k));
	for(int i = 0; i < k; i++) {
	    SET_STRING_ELT(names2, i, STRING_ELT(names, indx[i]));
	    SET_VECTOR_ELT(ans2,   i, VECTOR_ELT(ans,   indx[i]));
	}
	setAttrib(ans2, R_NamesSymbol, names2);
	UNPROTECT(5);
	return ans2;
    }
    else {
	setAttrib(ans, R_NamesSymbol, names);
	UNPROTECT(2);
	return ans;
    }
}

/** @brief apply a function to all objects in an environment
 * 
 * @return return the results in a list.
 * 
 * @example lapply(as.list(env, all.names=all.names), FUN, ...)
 * 
 * @note This is a special .Internal
 */
HIDDEN SEXP do_eapply(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP env, ans, R_fcall, FUN, tmp, tmp2, ind;
    int i, k, k2;
    int /* boolean */ all, useNms;

    checkArity(op, args);

    PROTECT(env = eval(CAR(args), rho));
    if (ISNULL(env))
	error(_("use of NULL environment is defunct"));
    if( !isEnvironment(env) )
	error(_("'%s' argument must be an environment"), "env");

    FUN = CADR(args);
    if (!isSymbol(FUN))
	error(_("arguments must be symbolic"));

    /* 'all.names' : */
    all = asLogical(PROTECT(eval(CADDR(args), rho)));
    UNPROTECT(1);
    if (all == NA_LOGICAL) all = 0;

    GCStackRoot<> framelist(FRAME(env));

    /* 'USE.NAMES' : */
    useNms = asLogical(PROTECT(eval(CADDDR(args), rho)));
    UNPROTECT(1);
    if (useNms == NA_LOGICAL) useNms = 0;

    if (env == R_BaseEnv || env == R_BaseNamespace)
	k = BuiltinSize(all, false);
    else if (HASHTAB(env) != nullptr)
	k = HashTableSize(HASHTAB(env), all);
    else
	k = FrameSize(framelist, all);

    PROTECT(ans  = allocVector(VECSXP, k));
    PROTECT(tmp2 = allocVector(VECSXP, k));

    k2 = 0;
    if (env == R_BaseEnv || env == R_BaseNamespace)
	BuiltinValues(all, 0, tmp2, &k2);
    else if (HASHTAB(env) != nullptr)
	HashTableValues(HASHTAB(env), all, tmp2, &k2);
    else
	FrameValues(framelist, all, tmp2, &k2);

    SEXP Xsym = Symbol::obtain("X");
    SEXP isym = Symbol::obtain("i");
    PROTECT(ind = allocVector(INTSXP, 1));
    /* tmp :=  `[`(<elist>, i) */
    PROTECT(tmp = GCNode::expose(new Expression(R_Bracket2Symbol, {Xsym, isym})));
    /* fcall :=  <FUN>( tmp, ... ) */
    PROTECT(R_fcall = GCNode::expose(new Expression(FUN, {tmp, R_DotsSymbol})));

    defineVar(Xsym, tmp2, rho);
    defineVar(isym, ind, rho);

    for(i = 0; i < k2; i++) {
	INTEGER(ind)[0] = i+1;
	SEXP tmp = R_forceAndCall(R_fcall, 1, rho);
	if (MAYBE_REFERENCED(tmp))
	    tmp = lazy_duplicate(tmp);
	SET_VECTOR_ELT(ans, i, tmp);
    }

    if (useNms) {
	SEXP names;
	PROTECT(names = allocVector(STRSXP, k));
	k = 0;
	if (env == R_BaseEnv || env == R_BaseNamespace)
	    BuiltinNames(all, 0, names, &k);
	else if(HASHTAB(env) != nullptr)
	    HashTableNames(HASHTAB(env), all, names, &k);
	else
	    FrameNames(framelist, all, names, &k);

	setAttrib(ans, R_NamesSymbol, names);
	UNPROTECT(1);
    }
    UNPROTECT(6);
    return ans;
}

/* Leaks out via inlining in ../library/tools/src/ */
int Rf_envlength(SEXP rho)
{
    if (IS_USER_DATABASE(rho))
    {
        R_ObjectTable *tb = (R_ObjectTable *)R_ExternalPtrAddr(HASHTAB(rho));
        return Rf_length(tb->objects(tb));
    }
    else if (HASHTAB(rho) != nullptr)
        return HashTableSize(HASHTAB(rho), 1);
    else if (rho == R_BaseEnv || rho == R_BaseNamespace)
        return BuiltinSize(true, false);
    else
        return FrameSize(FRAME(rho), 1);
}

R_xlen_t Rf_envxlength(SEXP rho)
{
    if (IS_USER_DATABASE(rho))
    {
        R_ObjectTable *tb = (R_ObjectTable *)R_ExternalPtrAddr(HASHTAB(rho));
        return Rf_xlength(tb->objects(tb));
    }
    else if (HASHTAB(rho) != nullptr)
        return HashTableSize(HASHTAB(rho), 1);
    else if (rho == R_BaseEnv || rho == R_BaseNamespace)
        return BuiltinSize(true, false);
    else
        return FrameSize(FRAME(rho), 1);
}

/**
 * @return Return the names of all the built in functions.  These are fetched
 * directly from the symbol table.
 */
HIDDEN SEXP do_builtins(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int intern, nelts;
    checkArity(op, args);
    intern = asLogical(CAR(args));
    if (intern == NA_INTEGER)
        intern = 0;
    nelts = BuiltinSize(true, intern);
    GCStackRoot<> ans(allocVector(STRSXP, nelts));
    nelts = 0;
    BuiltinNames(1, intern, ans, &nelts);
    sortVector(ans, true);
    return ans;
}

/**
 * This function returns the environment at a specified position in the
 * search path or the environment of the caller of
 * pos.to.env (? but pos.to.env is usually used in arg lists and hence
 * is evaluated in the calling environment so this is one higher).
 * 
 * When pos = -1 the environment of the closure that pos2env is
 * evaluated in is obtained. Note: this relies on pos.to.env being
 * a primitive.
 */

static SEXP pos2env(int pos, SEXP call)
{
    SEXP env;

    if (pos == NA_INTEGER || pos < -1 || pos == 0) {
	errorcall(call, _("invalid '%s' argument"), "pos");
	env = call;/* just for -Wall */
    }
    else if (pos == -1) {
	/* make sure the context is a funcall */
	RCNTXT *cptr = R_GlobalContext;
	while( !(cptr->getCallFlag() & CTXT_FUNCTION) && cptr->nextContext())
	    cptr = cptr->nextContext();
	if( !(cptr->getCallFlag() & CTXT_FUNCTION) )
	    errorcall(call, _("no enclosing environment"));

	env = cptr->getSysParent();
	if (R_GlobalEnv != nullptr && env == nullptr)
	    errorcall(call, _("invalid '%s' argument"), "pos");
    }
    else {
	for (env = R_GlobalEnv; env != R_EmptyEnv && pos > 1;
	     env = ENCLOS(env))
	    pos--;
	if (pos != 1)
	    errorcall(call, _("invalid '%s' argument"), "pos");
    }
    return env;
}

/**
 * @note this is primitive
 */

HIDDEN SEXP do_pos2env(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int i, npos;
    checkArity(op, args);
    check1arg(args, call, "x");

    GCStackRoot<> pos(Rf_coerceVector(CAR(args), INTSXP));
    npos = length(pos);
    if (npos <= 0)
        Rf_errorcall(call, _("invalid '%s' argument"), "pos");
    if (npos == 1)
    {
        SEXP env = pos2env(INTEGER(pos)[0], call);
        return env;
    }
    else
    {
        GCStackRoot<> env(Rf_allocVector(VECSXP, npos));
        for (i = 0; i < npos; i++)
        {
            SET_VECTOR_ELT(env, i, pos2env(INTEGER(pos)[i], call));
        }
        return env;
    }
}

static SEXP matchEnvir(SEXP call, const char *what)
{
    SEXP t, name;
    const void *vmax = vmaxget();
    if(streql(".GlobalEnv", what))
	return R_GlobalEnv;
    if(streql("package:base", what))
	return R_BaseEnv;
    for (t = ENCLOS(R_GlobalEnv); t != R_EmptyEnv ; t = ENCLOS(t)) {
	name = getAttrib(t, R_NameSymbol);
	if(Rf_isString(name) && length(name) > 0 &&
	   streql(translateChar(STRING_ELT(name, 0)), what)) {
	    vmaxset(vmax);
	    return t;
	}
    }
    errorcall(call, _("no item called '%s' on the search list"), what);
    /* not reached */
    vmaxset(vmax);
    return nullptr;
}

/**
 * @note this is primitive
 */

HIDDEN SEXP do_as_environment(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP arg = CAR(args), ans;
    checkArity(op, args);
    check1arg(args, call, "object");
    if(Rf_isEnvironment(arg))
	return arg;
    /* DispatchOrEval internal generic: as.environment */
    if(Rf_isObject(arg) &&
       DispatchOrEval(call, op, "as.environment", args, rho, &ans, 0, 1))
	return ans;
    switch(TYPEOF(arg)) {
    case STRSXP:
	return matchEnvir(call, translateChar(asChar(arg)));
    case REALSXP:
    case INTSXP:
	return do_pos2env(call, op, args, rho);
    case NILSXP:
	errorcall(call,_("using 'as.environment(NULL)' is defunct"));
	return R_BaseEnv;	/* -Wall */
    case S4SXP: {
	/* dispatch was tried above already */
	SEXP dot_xData = R_getS4DataSlot(arg, ENVSXP);
	if(!Rf_isEnvironment(dot_xData))
	    errorcall(call, _("S4 object does not extend class \"environment\""));
	else
	    return dot_xData;
    }
    case VECSXP: {
	/* implement as.environment.list() {isObject(.) is false for a list} */
	SEXP val;
	GCStackRoot<> call(lang4(Symbol::obtain("list2env"), arg,
			     /* envir = */nullptr,
			     /* parent = */R_EmptyEnv));
	val = eval(call, rho);
	return val;
    }
    default:
	errorcall(call, _("invalid object for 'as.environment()' function"));
	return nullptr;	/* -Wall */
    }
}

void R_LockEnvironment(SEXP env, Rboolean bindings)
{
    if(IS_S4_OBJECT(env) && (TYPEOF(env) == S4SXP))
	env = R_getS4DataSlot(env, ANYSXP); /* better be an ENVSXP */
    if (env == R_BaseEnv || env == R_BaseNamespace) {
        if (bindings)
        {
            for (Symbol::const_iterator it = Symbol::begin(); it != Symbol::end(); ++it)
            {
                Symbol *sym = (*it).second;
                if (sym && sym->value() != R_UnboundValue)
                    LOCK_BINDING(sym);
            }
        }
	LOCK_FRAME(env);
	return;
    }

    if (TYPEOF(env) != ENVSXP)
	error(_("'%s' argument is not an environment"), "env");
    if (bindings) {
	if (IS_HASHED(env)) {
	    SEXP table, chain;
	    int i, size;
	    table = HASHTAB(env);
	    size = HASHSIZE(table);
	    for (i = 0; i < size; i++)
		for (chain = VECTOR_ELT(table, i);
		     chain != nullptr;
		     chain = CDR(chain))
		    LOCK_BINDING(chain);
	}
	else {
	    SEXP frame;
	    for (frame = FRAME(env); frame != nullptr; frame = CDR(frame))
		LOCK_BINDING(frame);
	}
    }
    LOCK_FRAME(env);
}

Rboolean R_EnvironmentIsLocked(SEXP env)
{
    if (TYPEOF(env) == NILSXP)
	error(_("use of NULL environment is defunct"));
    if (TYPEOF(env) != ENVSXP &&
	TYPEOF((env = simple_as_environment(env))) != ENVSXP)
	error(_("'%s' argument is not an environment"), "env");
    return Rboolean(FRAME_IS_LOCKED(env) != 0);
}

HIDDEN SEXP do_lockEnv(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    SEXP frame = CAR(args);
    Rboolean bindings = Rboolean(asLogical(CADR(args)));
    R_LockEnvironment(frame, bindings);
    return nullptr;
}

HIDDEN SEXP do_envIsLocked(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    return ScalarLogical(R_EnvironmentIsLocked(CAR(args)));
}

void R_LockBinding(SEXP sym, SEXP env)
{
    if (TYPEOF(sym) != SYMSXP)
        error(_("'%s' argument is not a symbol"), "sym");
    if (TYPEOF(env) == NILSXP)
        error(_("use of NULL environment is defunct"));
    if (TYPEOF(env) != ENVSXP &&
        TYPEOF((env = simple_as_environment(env))) != ENVSXP)
        error(_("'%s' argument is not an environment"), "env");
    if (env == R_BaseEnv || env == R_BaseNamespace)
	/* It is a symbol, so must have a binding even if it is
	   R_UnboundSymbol */
	LOCK_BINDING(sym);
    else {
	SEXP binding = findVarLocInFrame(env, sym, nullptr);
	if (binding == nullptr)
	    error(_("no binding for '%s'"), EncodeChar(PRINTNAME(sym)));
	LOCK_BINDING(binding);
    }
}

void R_unLockBinding(SEXP sym, SEXP env)
{
    if (TYPEOF(sym) != SYMSXP)
        error(_("'%s' argument is not a symbol"), "sym");
    if (TYPEOF(env) == NILSXP)
        error(_("use of NULL environment is defunct"));
    if (TYPEOF(env) != ENVSXP &&
        TYPEOF((env = simple_as_environment(env))) != ENVSXP)
        error(_("'%s' argument is not an environment"), "env");
    if (env == R_BaseEnv || env == R_BaseNamespace)
	/* It is a symbol, so must have a binding even if it is
	   R_UnboundSymbol */
	UNLOCK_BINDING(sym);
    else {
	SEXP binding = findVarLocInFrame(env, sym, nullptr);
	if (binding == nullptr)
	    error(_("no binding for '%s'"), EncodeChar(PRINTNAME(sym)));
	UNLOCK_BINDING(binding);
    }
}

void R_MakeActiveBinding(SEXP sym, SEXP fun, SEXP env)
{
    if (TYPEOF(sym) != SYMSXP)
        error(_("'%s' argument is not a symbol"), "sym");
    if (!isFunction(fun))
        error(_("'%s' argument is not a function"), "fun");
    if (TYPEOF(env) == NILSXP)
        error(_("use of NULL environment is defunct"));
    if (TYPEOF(env) != ENVSXP && TYPEOF((env = simple_as_environment(env))) != ENVSXP)
        error(_("'%s' argument is not an environment"), "env");
    if (env == R_BaseEnv || env == R_BaseNamespace) {
	if (SYMVALUE(sym) != R_UnboundValue && ! IS_ACTIVE_BINDING(sym))
	    error(_("symbol already has a regular binding"));
	else if (BINDING_IS_LOCKED(sym))
	    error(_("cannot change active binding if binding is locked"));
	SET_SYMVALUE(sym, fun);
	SET_ACTIVE_BINDING_BIT(sym);
	/* we don't need to worry about the global cache here as
	   a regular binding cannot be changed */
    }
    else {
	SEXP binding = findVarLocInFrame(env, sym, nullptr);
	if (binding == nullptr) {
	    defineVar(sym, fun, env); /* fails if env is locked */
	    binding = findVarLocInFrame(env, sym, nullptr);
	    SET_ACTIVE_BINDING_BIT(binding);
	}
	else if (binding && ! IS_ACTIVE_BINDING(binding))
	    error(_("symbol already has a regular binding"));
	else if (BINDING_IS_LOCKED(binding))
	    error(_("cannot change active binding if binding is locked"));
	else
	    SETCAR(binding, fun);
    }
}

Rboolean R_BindingIsLocked(SEXP sym, SEXP env)
{
    if (TYPEOF(sym) != SYMSXP)
        error(_("'%s' argument is not a symbol"), "sym");
    if (TYPEOF(env) == NILSXP)
        error(_("use of NULL environment is defunct"));
    if (TYPEOF(env) != ENVSXP && TYPEOF((env = simple_as_environment(env))) != ENVSXP)
        error(_("'%s' argument is not an environment"), "env");
    if (env == R_BaseEnv || env == R_BaseNamespace)
	/* It is a symbol, so must have a binding even if it is
	   R_UnboundSymbol */
	return (Rboolean) (BINDING_IS_LOCKED(sym) != 0);
    else {
	SEXP binding = findVarLocInFrame(env, sym, nullptr);
	if (binding == nullptr)
	    error(_("no binding for '%s'"), EncodeChar(PRINTNAME(sym)));
	return (Rboolean) (BINDING_IS_LOCKED(binding) != 0);
    }
}

Rboolean R_BindingIsActive(SEXP sym, SEXP env)
{
    if (TYPEOF(sym) != SYMSXP)
        error(_("'%s' argument is not a symbol"), "sym");
    if (TYPEOF(env) == NILSXP)
        error(_("use of NULL environment is defunct"));
    if (TYPEOF(env) != ENVSXP && TYPEOF((env = simple_as_environment(env))) != ENVSXP)
        error(_("'%s' argument is not an environment"), "env");
    if (env == R_BaseEnv || env == R_BaseNamespace)
        /* It is a symbol, so must have a binding even if it is
	   R_UnboundSymbol */
        return Rboolean(IS_ACTIVE_BINDING(sym) != 0);
    else
    {
        SEXP binding = findVarLocInFrame(env, sym, nullptr);
        if (!binding)
            error(_("no binding for '%s'"), EncodeChar(PRINTNAME(sym)));
        return Rboolean(IS_ACTIVE_BINDING(binding) != 0);
    }
}

Rboolean R_HasFancyBindings(SEXP rho)
{
    if (IS_HASHED(rho)) {
	SEXP table, chain;
	int i, size;

	table = HASHTAB(rho);
	size = HASHSIZE(table);
	for (i = 0; i < size; i++)
	    for (chain = VECTOR_ELT(table, i);
		 chain != nullptr;
		 chain = CDR(chain))
		if (IS_ACTIVE_BINDING(chain) || BINDING_IS_LOCKED(chain))
		    return TRUE;
	return FALSE;
    }
    else {
	for (SEXP frame = FRAME(rho); frame; frame = CDR(frame))
	    if (IS_ACTIVE_BINDING(frame) || BINDING_IS_LOCKED(frame))
		return TRUE;
	return FALSE;
    }
}

SEXP R_ActiveBindingFunction(SEXP sym, SEXP env)
{
    if (TYPEOF(sym) != SYMSXP)
        error(_("not a symbol"));
    if (TYPEOF(env) == NILSXP)
        error(_("use of NULL environment is defunct"));
    if (TYPEOF(env) != ENVSXP &&
        TYPEOF((env = simple_as_environment(env))) != ENVSXP)
        error(_("not an environment"));
    if (env == R_BaseEnv || env == R_BaseNamespace) {
	SEXP val = SYMVALUE(sym);
	if (val == R_UnboundValue)
	    error(_("no binding for \"%s\""), EncodeChar(PRINTNAME(sym)));
	if (sym && !IS_ACTIVE_BINDING(sym))
	    error(_("no active binding for \"%s\""),
		  EncodeChar(PRINTNAME(sym)));
	return val;
    }
    else {
	SEXP binding = findVarLocInFrame(env, sym, nullptr);
	if (binding == nullptr)
	    error(_("no binding for \"%s\""), EncodeChar(PRINTNAME(sym)));
	if (! IS_ACTIVE_BINDING(binding))
	    error(_("no active binding for \"%s\""),
		  EncodeChar(PRINTNAME(sym)));
	return CAR(binding);
    }
}

HIDDEN SEXP do_lockBnd(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP sym, env;
    checkArity(op, args);
    sym = CAR(args);
    env = CADR(args);
    switch(PRIMVAL(op)) {
    case 0:
	R_LockBinding(sym, env);
	break;
    case 1:
	R_unLockBinding(sym, env);
	break;
    default:
	error(_("unknown op"));
    }
    return nullptr;
}

HIDDEN SEXP do_bndIsLocked(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP sym, env;
    checkArity(op, args);
    sym = CAR(args);
    env = CADR(args);
    return ScalarLogical(R_BindingIsLocked(sym, env));
}

HIDDEN SEXP do_mkActiveBnd(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP sym, fun, env;
    checkArity(op, args);
    sym = CAR(args);
    fun = CADR(args);
    env = CADDR(args);
    R_MakeActiveBinding(sym, fun, env);
    return nullptr;
}

HIDDEN SEXP do_bndIsActive(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP sym, env;
    checkArity(op, args);
    sym = CAR(args);
    env = CADR(args);
    return ScalarLogical(R_BindingIsActive(sym, env));
}

HIDDEN SEXP do_activeBndFun(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP sym, env;
    checkArity(op, args);
    sym = CAR(args);
    env = CADR(args);
    return R_ActiveBindingFunction(sym, env);
}

/**
 * @note This is a .Internal with no wrapper, currently unused in base R
 */

HIDDEN SEXP do_mkUnbound(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP sym;
    checkArity(op, args);
    sym = CAR(args);

    if (TYPEOF(sym) != SYMSXP)
        error(_("'%s' argument is not a symbol"), "sym");
    /* This is not quite the same as SET_SYMBOL_BINDING_VALUE as it
       does not allow active bindings to be unbound */
    if (R_BindingIsLocked(sym, R_BaseEnv))
        error(_("cannot unbind a locked binding"));
    if (R_BindingIsActive(sym, R_BaseEnv))
        error(_("cannot unbind an active binding"));
    SET_SYMVALUE(sym, R_UnboundValue);
#ifdef USE_GLOBAL_CACHE
    R_FlushGlobalCache(sym);
#endif
    return nullptr;
}

/* C version of new.env */
SEXP R_NewEnv(SEXP enclos, int hash, int size)
{
    if (hash) {
	SEXP ssize = PROTECT(ScalarInteger(size));
	SEXP ans = R_NewHashedEnv(enclos, ssize);
	UNPROTECT(1); /* ssize */
	return ans;
    }
    else
	return NewEnvironment(nullptr, nullptr, enclos);
}

void R_RestoreHashCount(SEXP rho)
{
    if (IS_HASHED(rho)) {
	SEXP table;
	int i, count, size;

	table = HASHTAB(rho);
	size = HASHSIZE(table);
	for (i = 0, count = 0; i < size; i++)
	    if (VECTOR_ELT(table, i) != nullptr)
		count++;
	SET_HASHPRI(table, count);
    }
}

Rboolean R_IsPackageEnv(SEXP rho)
{
    if (TYPEOF(rho) == ENVSXP) {
	SEXP name = getAttrib(rho, R_NameSymbol);
	const char *packprefix = "package:";
	size_t pplen = strlen(packprefix);
	if(Rf_isString(name) && length(name) > 0 &&
	   streqln(packprefix, CHAR(STRING_ELT(name, 0)), pplen)) /* ASCII */
	    return TRUE;
	else
	    return FALSE;
    }
    else
	return FALSE;
}

SEXP R_PackageEnvName(SEXP rho)
{
    if (TYPEOF(rho) == ENVSXP) {
	SEXP name = getAttrib(rho, R_NameSymbol);
	const char *packprefix = "package:";
	size_t pplen = strlen(packprefix);
	if(Rf_isString(name) && length(name) > 0 &&
	   streqln(packprefix, CHAR(STRING_ELT(name, 0)), pplen)) /* ASCII */
	    return name;
	else
	    return nullptr;
    }
    else
	return nullptr;
}

SEXP R_FindPackageEnv(SEXP info)
{
    static Symbol *s_findPackageEnv = Symbol::obtain("findPackageEnv");
    GCStackRoot<Expression> expr(GCNode::expose(new Expression(s_findPackageEnv, {info})));

    return expr->evaluate(Environment::global());
}

Rboolean R_IsNamespaceEnv(SEXP rho)
{
    if (rho == R_BaseNamespace)
        return TRUE;
    else if (TYPEOF(rho) == ENVSXP)
    {
        SEXP info = Rf_findVarInFrame3(rho, R_NamespaceSymbol, TRUE);
        if (info != R_UnboundValue && TYPEOF(info) == ENVSXP)
        {
            GCStackRoot<> infor(info);
            SEXP spec = findVarInFrame3(info, Symbol::obtain("spec"), TRUE);
            if (spec != R_UnboundValue &&
                TYPEOF(spec) == STRSXP && LENGTH(spec) > 0)
                return TRUE;
            else
                return FALSE;
        }
        else
            return FALSE;
    }
    else
        return FALSE;
}

HIDDEN SEXP do_isNSEnv(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    return R_IsNamespaceEnv(CAR(args)) ? mkTrue() : mkFalse();
}

SEXP R_NamespaceEnvSpec(SEXP rho)
{
    /* The namespace spec is a character vector that specifies the
       namespace.  The first element is the namespace name.  The
       second element, if present, is the namespace version.  Further
       elements may be added later. */
    if (rho == R_BaseNamespace)
        return R_BaseNamespaceName;
    else if (TYPEOF(rho) == ENVSXP)
    {
        SEXP info = findVarInFrame3(rho, R_NamespaceSymbol, TRUE);
        if (info != R_UnboundValue && TYPEOF(info) == ENVSXP)
        {
            PROTECT(info);
            SEXP spec = findVarInFrame3(info, Symbol::obtain("spec"), TRUE);
            UNPROTECT(1);
            if (spec != R_UnboundValue &&
                TYPEOF(spec) == STRSXP && LENGTH(spec) > 0)
                return spec;
            else
                return nullptr;
        }
        else
            return nullptr;
    }
    else
        return nullptr;
}

SEXP R_FindNamespace(SEXP info)
{
    SEXP val;
    GCStackRoot<> infor(info);
    SEXP s_getNamespace = Symbol::obtain("getNamespace");
    GCStackRoot<PairList> tail(CXXR_cons(info, nullptr));
    GCStackRoot<> expr(LCONS(s_getNamespace, tail));
    val = eval(expr, R_GlobalEnv);
    return val;
}

static SEXP checkNSname(SEXP call, SEXP name)
{
    switch (TYPEOF(name))
    {
    case SYMSXP:
        break;
    case STRSXP:
        if (LENGTH(name) >= 1)
        {
            name = installTrChar(STRING_ELT(name, 0));
            break;
        }
    /* else fall through */
    default:
        errorcall(call, _("bad namespace name"));
    }
    return name;
}

HIDDEN SEXP do_regNS(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP name, val;
    checkArity(op, args);
    name = checkNSname(call, CAR(args));
    val = CADR(args);
    if (findVarInFrame(R_NamespaceRegistry, name) != R_UnboundValue)
        errorcall(call, _("namespace is already registered"));
    defineVar(name, val, R_NamespaceRegistry);
    return nullptr;
}

HIDDEN SEXP do_unregNS(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP name;
    int hashcode;
    checkArity(op, args);
    name = checkNSname(call, CAR(args));
    if (findVarInFrame(R_NamespaceRegistry, name) == R_UnboundValue)
        errorcall(call, _("namespace is not registered"));
    hashcode = HASHVALUE(PRINTNAME(name));
    RemoveVariable(name, hashcode, R_NamespaceRegistry);
    return nullptr;
}

HIDDEN SEXP do_getRegNS(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP name, val;
    checkArity(op, args);
    name = checkNSname(call, PROTECT(coerceVector(CAR(args), SYMSXP)));
    UNPROTECT(1);
    val = findVarInFrame(R_NamespaceRegistry, name);

    switch (PRIMVAL(op))
    {
    case 0: // get..()
        if (val == R_UnboundValue)
            return nullptr;
        else
            return val;
    case 1: // is..()
        return ScalarLogical(val == R_UnboundValue ? FALSE : TRUE);

    default:
        error(_("unknown op"));
    }
    return nullptr; // -Wall
}

HIDDEN SEXP do_getNSRegistry(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    return R_NamespaceRegistry;
}

static SEXP getVarValInFrame(SEXP rho, SEXP sym, int unbound_ok)
{
    SEXP val = findVarInFrame(rho, sym);
    if (! unbound_ok && val == R_UnboundValue)
	error(_("object '%s' not found"), EncodeChar(PRINTNAME(sym)));
    if (TYPEOF(val) == PROMSXP) {
	PROTECT(val);
	val = eval(val, R_EmptyEnv);
	UNPROTECT(1);
    }
    return val;
}

static SEXP checkVarName(SEXP call, SEXP name)
{
    switch(TYPEOF(name)) {
    case SYMSXP: break;
    case STRSXP:
	if (LENGTH(name) >= 1) {
	    name = installTrChar(STRING_ELT(name, 0));
	    break;
	}
	/* else fall through */
    default:
	errorcall(call, _("bad variable name"));
    }
    return name;
}

static SEXP callR1(SEXP fun, SEXP arg)
{
    static GCRoot<Symbol> R_xSymbol(nullptr);
    if (R_xSymbol == nullptr)
	R_xSymbol = Symbol::obtain("x");

    SEXP rho = PROTECT(NewEnvironment(nullptr, nullptr, R_BaseNamespace));
    defineVar(R_xSymbol, arg, rho);
    SEXP expr = PROTECT(lang2(fun, R_xSymbol));
    SEXP val = eval(expr, rho);
    /**** ideally this should clear out rho if it isn't captured - LT */
    UNPROTECT(2); /* rho, expr */
    return val;
}

SEXP attribute_hidden R_getNSValue(SEXP call, SEXP ns, SEXP name, int exported)
{
    static GCRoot<Symbol> R_loadNamespaceSymbol(nullptr);
    static GCRoot<Symbol> R_exportsSymbol(nullptr);
    static GCRoot<Symbol> R_lazydataSymbol(nullptr);
    static GCRoot<Symbol> R_getNamespaceNameSymbol(nullptr);
    if (R_loadNamespaceSymbol == nullptr) {
	R_loadNamespaceSymbol = Symbol::obtain("loadNamespace");
	R_exportsSymbol = Symbol::obtain("exports");
	R_lazydataSymbol = Symbol::obtain("lazydata");
	R_getNamespaceNameSymbol = Symbol::obtain("getNamespaceName");
    }

    if (R_IsNamespaceEnv(ns))
	PROTECT(ns);
    else {
	SEXP pkg = checkNSname(call, ns);
	ns = findVarInFrame(R_NamespaceRegistry, pkg);
	if (ns == R_UnboundValue)
	    ns = callR1(R_loadNamespaceSymbol, pkg);
	PROTECT(ns);
	if (! R_IsNamespaceEnv(ns))
	    errorcall(call, _("bad namespace"));
    }

    name = checkVarName(call, name);

    SEXP val;

    /* base or non-exported variables */
    if (ns == R_BaseNamespace || ! exported) {
	val = getVarValInFrame(ns, name, FALSE);
	UNPROTECT(1); /* ns */
	return val;
    }

    /* exported variables */
    SEXP info = PROTECT(getVarValInFrame(ns, R_NamespaceSymbol, FALSE));
    SEXP exports = PROTECT(getVarValInFrame(info, R_exportsSymbol, FALSE));
    SEXP exportName = PROTECT(getVarValInFrame(exports, name, TRUE));
    if (exportName != R_UnboundValue) {
	val = eval(checkVarName(call, exportName), ns);	
	UNPROTECT(4);  /* ns, info, exports, exportName */
	return val;
    }

    /* lazydata */
    SEXP ld = PROTECT(getVarValInFrame(info, R_lazydataSymbol, FALSE));
    val = getVarValInFrame(ld, name, TRUE);
    if (val != R_UnboundValue) {
	UNPROTECT(5); /* ns, info, exports, exportName, ld */
	return val;
    }

    SEXP nsname = PROTECT(callR1(R_getNamespaceNameSymbol, ns));
    if (TYPEOF(nsname) != STRSXP || LENGTH(nsname) != 1)
	errorcall(call, "bad value returned by `getNamespaceName'");
    errorcall(call,
	      _("'%s' is not an exported object from 'namespace:%s'"),
	      EncodeChar(PRINTNAME(name)),
	      CHAR(STRING_ELT(nsname, 0)));
    return NULL; /* not reached */
}

SEXP do_getNSValue(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    SEXP ns = CAR(args);
    SEXP name = CADR(args);
    int exported = asLogical(CADDR(args));

    return R_getNSValue(nullptr, ns, name, exported);
}

SEXP do_colon2(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    /* use nullptr for the call to avoid changing the error message */
    return R_getNSValue(nullptr, CAR(args), CADR(args), TRUE);
}

SEXP do_colon3(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    return R_getNSValue(call, CAR(args), CADR(args), FALSE);
}

HIDDEN SEXP do_importIntoEnv(SEXP call, SEXP op, SEXP args, SEXP env)
{
    /* This function copies values of variables from one environment
       to another environment, possibly with different names.
       Promises are not forced and active bindings are preserved. */
    SEXP impenv, impnames, expenv, expnames;
    SEXP impsym, expsym, val;
    int n;

    checkArity(op, args);

    impenv = CAR(args); args = CDR(args);
    impnames = CAR(args); args = CDR(args);
    expenv = CAR(args); args = CDR(args);
    expnames = CAR(args); args = CDR(args);

    if (TYPEOF(impenv) == NILSXP)
        error(_("use of NULL environment is defunct"));
    if (TYPEOF(impenv) != ENVSXP &&
        TYPEOF((impenv = simple_as_environment(impenv))) != ENVSXP)
        error(_("bad import environment argument"));
    if (TYPEOF(expenv) == NILSXP)
        error(_("use of NULL environment is defunct"));
    if (TYPEOF(expenv) != ENVSXP &&
        TYPEOF((expenv = simple_as_environment(expenv))) != ENVSXP)
        error(_("bad export environment argument"));
    if (TYPEOF(impnames) != STRSXP || TYPEOF(expnames) != STRSXP)
        error(_("invalid '%s' argument"), "names");
    if (LENGTH(impnames) != LENGTH(expnames))
        error(_("length of import and export names must match"));

    n = LENGTH(impnames);
    for (int i = 0; i < n; i++) {
	impsym = installTrChar(STRING_ELT(impnames, i));
	expsym = installTrChar(STRING_ELT(expnames, i));

	/* find the binding--may be a CONS cell or a symbol */
	GCRoot<> binding;  // represented in PairList form.

	for (SEXP env = expenv;
	     env != R_EmptyEnv && binding == nullptr;
	     env = ENCLOS(env))
	    if (env == R_BaseNamespace) {
		if (SYMVALUE(expsym) != R_UnboundValue)
		    binding = expsym;
        }
        else
        {
            binding = findVarLocInFrame(env, expsym, nullptr);
        }
    if (!binding)
	    binding = expsym;

	/* get value of the binding; do not force promises */
	if (TYPEOF(binding) == SYMSXP) {
	    if (SYMVALUE(expsym) == R_UnboundValue)
		error(_("exported symbol '%s' has no value"), R_CHAR(PRINTNAME(expsym)));
	    val = SYMVALUE(expsym);
	}
	else val = CAR(binding);

	/* import the binding */
	if (IS_ACTIVE_BINDING(binding))
	    R_MakeActiveBinding(impsym, val, impenv);
	/* This is just a tiny optimization */
	else if (impenv == R_BaseNamespace || impenv == R_BaseEnv)
	    gsetVar(impsym, val, impenv);
	else
	    defineVar(impsym, val, impenv);
    }
    return nullptr;
}

HIDDEN SEXP do_envprofile(SEXP call, SEXP op, SEXP args, SEXP env)
{
    /* Return a list containing profiling information given a hashed
       environment.  For non-hashed environments, this function
       returns nullptr.  This seems appropriate since there is no
       way to test whether an environment is hashed at the R level.
    */
    checkArity(op, args);
    SEXP env2, ans = nullptr /* -Wall */;
    env2 = CAR(args);
    if (isEnvironment(env2))
    {
        if (IS_HASHED(env2))
            ans = R_HashProfile(HASHTAB(env2));
    }
    else
        error(_("'%s' argument must be a hashed environment"), "env");
    return ans;
}

SEXP Rf_mkCharCE(const char *name, cetype_t enc)
{
    size_t len = strlen(name);
    if (len > R_INT_MAX)
        Rf_error(_("R character strings are limited to 2^31-1 bytes"));
    return Rf_mkCharLenCE(name, (int)len, enc);
}

/* no longer used in R but documented in 2.7.x */
SEXP Rf_mkCharLen(const char *name, int len)
{
    return Rf_mkCharLenCE(name, len, CE_NATIVE);
}

/** @brief Make a character (CHARSXP) variable.
 * 
 * @param name character string to be used when creating character variable
 * 
 * @return CHARXSP object
 * 
 * @note See Rinlinedfuns.h
 */

SEXP Rf_mkChar(const char *const name)
{
    size_t len = strlen(name);
    if (len > R_INT_MAX)
        Rf_error(_("R character strings are limited to 2^31-1 bytes"));
    return Rf_mkCharLenCE(name, (int)len, CE_NATIVE);
}

/** @brief Make a character (CHARSXP) variable and set its
 * encoding bit.
 * 
 * @note If a CHARSXP with the same string already exists it is returned.
 * Otherwise, a new CHARSXP is created and then returned.
 */
SEXP Rf_mkCharLenCE(const char *name, int len, cetype_t enc)
{
    if (!name)
        name = "";

    switch (enc)
    {
    case CE_NATIVE:
    case CE_UTF8:
    case CE_LATIN1:
    case CE_BYTES:
    case CE_SYMBOL:
    case CE_ANY:
        break;
    default:
        Rf_error(_("unknown encoding: %d"), enc);
    }
    std::string str(name, len);
    return CachedString::obtain(str, enc);
}

// topenv

SEXP Rf_topenv(SEXP target, SEXP envir)
{
    SEXP env = envir;
    while (env != R_EmptyEnv)
    {
        if (env == target || env == R_GlobalEnv ||
            env == R_BaseEnv || env == R_BaseNamespace ||
            R_IsPackageEnv(env) || R_IsNamespaceEnv(env) ||
            existsVarInFrame(env, R_dot_packageName))
            return env;
        env = ENCLOS(env);
    }
    return R_GlobalEnv;
}

/**
 *
 * @example .Internal(topenv(envir, matchThisEnv))
 *
 * @return
 */
HIDDEN SEXP do_topenv(SEXP call, SEXP op, SEXP args, SEXP rho) {
    checkArity(op, args);
    SEXP envir = CAR(args);
    SEXP target = CADR(args); // = matchThisEnv, typically NULL (nullptr)
    if (TYPEOF(envir) != ENVSXP) envir = rho; // envir = parent.frame()
    if (target != nullptr && TYPEOF(target) != ENVSXP)  target = nullptr;
    return topenv(target, envir);
}

/*HIDDEN*/
Rboolean Rf_isUnmodifiedSpecSym(SEXP sym, SEXP env)
{
    if (sym && !IS_SPECIAL_SYMBOL(sym))
        return FALSE;
    while (env != R_EmptyEnv)
    {
        if (env && !NO_SPECIAL_SYMBOLS(env) && env != R_BaseEnv && env != R_BaseNamespace && existsVarInFrame(env, sym))
            return FALSE;
        env = ENCLOS(env);
    }
    return TRUE;
}

void findFunctionForBodyInNamespace(SEXP body, SEXP nsenv, SEXP nsname) {
    if (R_IsNamespaceEnv(nsenv) != TRUE)
	error(_("'%s' argument is not a namespace"), "nsenv");
    SEXP args = PROTECT(list3(nsenv /* x */,
	R_TrueValue /* all.names */,
	R_FalseValue /* sorted */));
    SEXP env2listOp = INTERNAL(Symbol::obtain("env2list"));

    SEXP elist = do_env2list(nullptr, env2listOp, args, nullptr);
    PROTECT(elist);
    R_xlen_t n = xlength(elist);
    R_xlen_t i;
    SEXP names = PROTECT(getAttrib(elist, R_NamesSymbol));
    for(i = 0; i < n; i++) {
	SEXP value = VECTOR_ELT(elist, i);
	const char *vname = CHAR(STRING_ELT(names, i));
	/* the constants checking requires shallow comparison */
	if (TYPEOF(value) == CLOSXP && R_ClosureExpr(value) == body)
	    REprintf(_("Function %s in namespace %s has this body.\n"),
		vname,
		CHAR(PRINTNAME(nsname)));
	/* search S4 registry */
	const char *s4prefix = ".__T__";
	if (TYPEOF(value) == ENVSXP &&
		streqln(vname, s4prefix, strlen(s4prefix))) {
	    SETCAR(args, value); /* re-use args */
	    SEXP rlist = do_env2list(nullptr, env2listOp, args, nullptr);
	    PROTECT(rlist);
	    R_xlen_t rn = xlength(rlist);
	    R_xlen_t ri;
	    SEXP rnames = PROTECT(getAttrib(rlist, R_NamesSymbol));
	    for(ri = 0; ri < rn; ri++) {
		SEXP rvalue = VECTOR_ELT(rlist, ri);
		/* the constants checking requires shallow comparison */
		if (TYPEOF(rvalue) == CLOSXP &&
			R_ClosureExpr(rvalue) == body)
		    REprintf(_("S4 Method %s defined in namespace %s with signature %s has this body.\n"),
			vname + strlen(s4prefix),
			CHAR(PRINTNAME(nsname)),
			CHAR(STRING_ELT(rnames, ri)));
	    }
	    UNPROTECT(2); /* rlist, rnames */
	}
    }
    UNPROTECT(3); /* names, elist, args */
}

/** @brief For a given function body, try to find a closure and
 * the name of its binding (and the name of the package).
 * 
 * @note For debugging.
 * */
/*HIDDEN*/
void Rf_findFunctionForBody(SEXP body)
{
    SEXP nstable = HASHTAB(R_NamespaceRegistry);
    CHECK_HASH_TABLE(nstable);
    int n = Rf_length(nstable);
    int i;
    for (i = 0; i < n; i++)
    {
        SEXP frame = VECTOR_ELT(nstable, i);
        while (frame)
        {
            findFunctionForBodyInNamespace(body, CAR(frame), TAG(frame));
            frame = CDR(frame);
        }
    }
}

