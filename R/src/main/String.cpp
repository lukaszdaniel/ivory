/*
 *  R : A Computer Language for Statistical Data Analysis
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

/** @file String.cpp
 *
 * Implementation of class String and related functions.
 */

#include <CXXR/String.hpp>
#include <CXXR/CachedString.hpp>
#include <CXXR/UncachedString.hpp>

namespace CXXR
{
    // Force the creation of non-inline embodiments of functions callable
    // from C:
    namespace ForceNonInline
    {
        // const auto &ENC_KNOWNptr = ENC_KNOWN;
        // const auto &IS_ASCIIptr = IS_ASCII;
        // const auto &IS_BYTESptr = IS_BYTES;
        const auto &IS_LATIN1ptr = IS_LATIN1;
        const auto &IS_UTF8ptr = IS_UTF8;
        const auto &R_CHARptr = R_CHAR;
        const auto &mkCharCEptr = Rf_mkCharCE;
        const auto &mkCharLenptr = Rf_mkCharLen;
    } // namespace ForceNonInline

    namespace
    {
        // Used in GPBits2Encoding() and packGPBits():
        constexpr unsigned int BYTES_MASK = 1 << 1;
        constexpr unsigned int LATIN1_MASK = 1 << 2;
        constexpr unsigned int UTF8_MASK = 1 << 3;
        constexpr unsigned int ASCII_MASK = 1 << 6;
        constexpr unsigned int CACHED_MASK = 1 << 5;
        constexpr unsigned int HASHASH_MASK = 1;
    } // namespace

    unsigned int String::packGPBits() const
    {
        unsigned int ans = VectorBase::packGPBits();
        switch (m_encoding)
        {
        case CE_UTF8:
            ans |= UTF8_MASK;
            break;
        case CE_LATIN1:
            ans |= LATIN1_MASK;
            break;
        case CE_BYTES:
            ans |= BYTES_MASK;
            break;
        default:
            break;
        }
        if (m_cached)
            ans |= CACHED_MASK;
        if (m_ascii)
            ans |= ASCII_MASK;
        // if (m_hash != -1)
        //     ans |= HASHASH_MASK;
        return ans;
    }

    cetype_t String::GPBits2Encoding(unsigned int gpbits)
    {
        if ((gpbits & LATIN1_MASK) != 0)
            return CE_LATIN1;
        if ((gpbits & UTF8_MASK) != 0)
            return CE_UTF8;
        if ((gpbits & BYTES_MASK) != 0)
            return CE_BYTES;
        return CE_NATIVE;
    }

    int String::clearCacheAndHashMasks(int levels)
    {
        return levels & (~(CACHED_MASK | HASHASH_MASK));
    }

    void String::setGPBits()
    {
#if CXXR_FALSE
        switch (m_encoding)
        {
        case CE_NATIVE:
            break;
        case CE_UTF8:
            m_gpbits |= UTF8_MASK;
            break;
        case CE_LATIN1:
            m_gpbits |= LATIN1_MASK;
            break;
        case CE_BYTES:
            m_gpbits |= BYTES_MASK;
            break;
        default:
            break;
        }
        if (m_ascii)
            m_gpbits |= ASCII_MASK;
        if (m_cached)
            m_gpbits |= CACHED_MASK;
#endif
    }

    GCRoot<const String> String::s_na(UncachedString::obtain("NA"), true);
    GCRoot<const String> String::s_blank(CachedString::obtain(""), true);

    // String::Comparator::operator()(const String&, const String&) is in
    // sort.cpp

    void String::initialize()
    {
        R_NaString = const_cast<String *>(String::NA());
        set_cached(R_NaString); /* Mark it */
        R_BlankString = const_cast<String *>(String::blank());
    }

    void String::checkEncoding(cetype_t encoding)
    {
        switch (encoding)
        {
        case CE_NATIVE:
        case CE_UTF8:
        case CE_LATIN1:
        case CE_BYTES:
            break;
        default:
            Rf_error(_("unknown encoding: %d"), encoding);
        }
    }

    /* Hashing Methods */
    unsigned int String::hashash(RObject *x)
    {
        return x ? SEXP_downcast<String *>(x, false)->m_hash != -1 : 0;
    }

    unsigned int String::is_bytes(RObject *x)
    {
        if (!x)
            return false;
        const String *str = SEXP_downcast<const String *>(x);
        return str->encoding() == CE_BYTES;
    }

    void String::set_bytes(RObject *x)
    {
    }

    unsigned int String::is_latin1(RObject *x)
    {
        if (!x)
            return false;
        const String *str = SEXP_downcast<const String *>(x);
        return str->encoding() == CE_LATIN1;
    }

    void String::set_latin1(RObject *x)
    {
    }

    unsigned int String::is_ascii(RObject *x)
    {
        if (!x)
            return false;
        const String *str = SEXP_downcast<const String *>(x);
        return str->encoding() == CE_NATIVE;
    }

    void String::set_ascii(RObject *x)
    {
    }

    unsigned int String::is_utf8(RObject *x)
    {
        if (!x)
            return false;
        const String *str = SEXP_downcast<const String *>(x);
        return str->encoding() == CE_UTF8;
    }

    void String::set_utf8(RObject *x)
    {
    }

    unsigned int String::enc_known(RObject *x)
    {
        if (!x)
            return CE_NATIVE;
        const String *str = SEXP_downcast<const String *>(x);
        cetype_t enc = str->encoding();
        if (enc == CE_LATIN1)
            return CE_LATIN1;
        else if (enc == CE_UTF8)
            return CE_UTF8;

        return CE_NATIVE;
    }

    void String::set_cached(RObject *x)
    {
        // x->m_gpbits |= CACHED_MASK;
        SEXP_downcast<String *>(x)->m_cached = true;
    }

    unsigned int String::is_cached(RObject *x)
    {
        return x && SEXP_downcast<String *>(x)->m_cached;
    }
} // namespace CXXR

namespace R
{
    bool streql(const char *s, const char *t)
    {
        return (strcmp(s, t) == 0);
    }
    bool streqln(const char *s, const char *t, size_t n)
    {
        return (strncmp(s, t, n) == 0);
    }

    /* this has NA_STRING = NA_STRING */
    HIDDEN bool Seql(SEXP a, SEXP b)
    {
        /* The only case where pointer comparisons do not suffice is where
      we have two strings in different encodings (which must be
      non-ASCII strings). Note that one of the strings could be marked
      as unknown. */
        if (a == b)
            return true;
        /* Leave this to compiler to optimize */
        if (IS_CACHED(a) && IS_CACHED(b) && ENC_KNOWN(a) == ENC_KNOWN(b))
            return false;
        else
        {
            auto vmax = vmaxget();
            bool result = streql(translateCharUTF8(a), translateCharUTF8(b));
            vmaxset(vmax); /* discard any memory used by translateCharUTF8 */
            return result;
        }
    }
}

// ***** C interface *****

int HASHASH(SEXP x)
{
    return CXXR::String::hashash(x);
}

int HASHVALUE(SEXP x)
{
    const CXXR::String *str = CXXR::SEXP_downcast<CXXR::String *>(x, false);
    return str->hash();
}

void SET_HASHASH(SEXP x, int v)
{
    /* does nothing in CXXR */
}

void SET_HASHVALUE(SEXP x, int v)
{
    const CXXR::String *str = CXXR::SEXP_downcast<CXXR::String *>(x, false);
    str->hash();
}

const char *R_CHAR(SEXP x)
{
    if (TYPEOF(x) != CHARSXP) // Han-Tak proposes to prepend  'x && '
        Rf_error(_("'%s' function can only be applied to a charecter, not a '%s'"), "CHAR()",
                 Rf_type2char(TYPEOF(x)));
    return CXXR::r_char(x);
}

int IS_BYTES(SEXP x)
{
    return CXXR::String::is_bytes(x);
}

int IS_LATIN1(SEXP x)
{
    return CXXR::String::is_latin1(x);
}

int IS_ASCII(SEXP x)
{
    return CXXR::String::is_ascii(x);
}

int IS_UTF8(SEXP x)
{
    return CXXR::String::is_utf8(x);
}

void SET_BYTES(SEXP x)
{
    CXXR::String::set_bytes(x);
}

void SET_LATIN1(SEXP x)
{
    CXXR::String::set_latin1(x);
}

void SET_UTF8(SEXP x)
{
    CXXR::String::set_utf8(x);
}

void SET_ASCII(SEXP x)
{
    CXXR::String::set_ascii(x);
}

int ENC_KNOWN(SEXP x)
{
    return CXXR::String::enc_known(x);
}

void SET_CACHED(SEXP x)
{
    CXXR::String::set_cached(x);
}

int IS_CACHED(SEXP x)
{
    return CXXR::String::is_cached(x);
}