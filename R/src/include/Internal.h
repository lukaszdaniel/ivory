/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1997--2020  The R Core Team
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
 */

/* Names of  .Internal(.) and .Primitive(.)  R functions
 *
 * Must all return SEXP because of CCODE in Defn.h.
 * do_math*() and do_cmathfuns are in ../main/arithmetic.h
 */

#ifndef R_INTERNAL_H
#define R_INTERNAL_H


#ifndef __cplusplus
#error Internal.h can only be included in C++ files
#endif

extern "C" {

/* Function Names */

#if _WIN32
SEXP do_mkjunction(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_shellexec(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_syswhich(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_tzone_name(SEXP call, SEXP op, SEXP args, SEXP env);
#else
SEXP do_X11(SEXP call, SEXP op, SEXP args, SEXP env);
#endif

SEXP do_abbrev(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_abs(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_addCondHands(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_addGlobHands(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_address(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_addRestart(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_addTryHandlers(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_adist(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_agrep(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_allnames(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_altrep_class(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_anyNA(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_aperm(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_aregexec(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_args(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_arith(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_array(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_asPOSIXct(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_asPOSIXlt(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_ascall(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_as_environment(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_asatomic(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_asfunction(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_asmatrixdf(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_assign(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_asvector(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_asCharacterFactor(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_AT(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_attach(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_attr(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_attrgets(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_attributes(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_attributesgets(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_backsolve(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_baseenv(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_basename(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_bcprofcounts(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_bcprofstart(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_bcprofstop(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_begin(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_bincode(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_bind(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_bindtextdomain(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_bitwise(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_body(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_bodyCode(SEXP call, SEXP op, SEXP args, SEXP env);
NORET SEXP do_break(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_browser(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_builtins(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_c(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_c_dflt(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_call(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_str2lang(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_capabilities(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_capabilitiesX11(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_cat(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_charmatch(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_charToRaw(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_chartr(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_class(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_classgets(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_colon(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_colsum(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_commandArgs(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_comment(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_commentgets(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_complex(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_contourLines(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_copyDFattr(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_crc64(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_Cstack_info(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_cum(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_curlDownload(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_curlGetHeaders(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_curlVersion(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_D2POSIXlt(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_date(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_debug(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_delayed(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_deparse(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_detach(SEXP call, SEXP op, SEXP args, SEXP env);
NORET SEXP do_dfltStop(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_dfltWarn(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_diag(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_dim(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_dimgets(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_dimnames(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_dimnamesgets(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_dircreate(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_direxists(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_dirname(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_docall(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_dotcall(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_dotsElt(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_dotsLength(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_dotsNames(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_dotcallgr(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_dotCode(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_dput(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_drop(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_dump(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_duplicated(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_dynload(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_dynunload(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_eapply(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_edit(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_emptyenv(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_encoding(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_encodeString(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_enc2(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_envir(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_envirgets(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_envirName(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_env2list(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_eSoftVersion(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_External(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_Externalgr(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_eval(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_expression(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_fileaccess(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_fileappend(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_filechoose(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_filecopy(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_filecreate(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_fileexists(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_fileinfo(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_filelink(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_filepath(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_fileremove(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_filerename(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_fileshow(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_filesymlink(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_findinterval(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_first_min(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_flush(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_for(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_forceAndCall(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_format(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_formatC(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_formatinfo(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_formatPOSIXlt(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_formals(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_function(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_gc(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_gcinfo(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_gctime(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_gctorture(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_gctorture2(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_get(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_getDllTable(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_getVarsFromFrame(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_getenv(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_geterrmessage(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_getGraphicsEvent(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_getGraphicsEventEnv(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_getlocale(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_getOption(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_getRegisteredRoutines(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_getSymbolInfo(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_getRestart(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_gettext(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_getwd(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_glob(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_globalenv(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_grep(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_grepraw(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_gsub(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_iconv(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_ICUget(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_ICUset(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_identical(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_if(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_inherits(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_inspect(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_intToUtf8(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_interactive(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_internal(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_internalsID(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_interruptsSuspended(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_numToBits(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_numToInts(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_intToBits(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_invisible(SEXP call, SEXP op, SEXP args, SEXP env);
NORET SEXP do_invokeRestart(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_is(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_isatty(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_isfinite(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_isinfinite(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_islistfactor(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_isloaded(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_isna(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_isnan(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_isunsorted(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_isvector(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_lapack(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_lapply(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_lazyLoadDBfetch(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_lazyLoadDBflush(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_lazyLoadDBinsertValue(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_length(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_lengthgets(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_lengths(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_levelsgets(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_listdirs(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_listfiles(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_list2env(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_load(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_loadFromConn2(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_loadInfoFromConn2(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_localeconv(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_log(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_log1arg(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_logic(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_logic2(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_logic3(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_ls(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_l10n_info(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_machine(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_makelazy(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_makelist(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_makenames(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_makeunique(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_makevector(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_mapply(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_match(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_matchcall(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_matprod(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_Math2(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_matrix(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_maxcol(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_maxVSize(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_maxNSize(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_memlimits(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_memoryprofile(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_merge(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_mget(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_missing(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_mmap_file(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_munmap_file(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_named(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_names(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_namesgets(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_nargs(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_nchar(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_newenv(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_nextmethod(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_ngettext(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_normalizepath(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_nzchar(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_onexit(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_options(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_order(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_packBits(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_paren(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_parentenv(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_parentenvgets(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_parentframe(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_parse(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_paste(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_pathexpand(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_pcre_config(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_pmatch(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_pmin(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_polyroot(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_pos2env(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_POSIXlt2D(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_pretty(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_primitive(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_printdefault(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_printDeferredWarnings(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_printfunction(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_prmatrix(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_proctime(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_psort(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_qsort(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_quit(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_quote(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_radixsort(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_random1(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_random2(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_random3(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_range(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_rank(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_rapply(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_rawShift(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_rawToBits(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_rawToChar(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_readDCF(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_readEnviron(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_readlink(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_readLines(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_readln(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_recall(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_refcnt(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_recordGraphics(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_regexec(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_regexpr(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_regFinaliz(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_relop(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_relop_dflt(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_remove(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_rep(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_rep_int(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_rep_len(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_repeat(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_resetCondHands(SEXP call, SEXP op, SEXP args, SEXP env);
NORET SEXP do_return(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_returnValue(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_rgb(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_Rhome(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_RNGkind(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_rowsum(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_rowscols(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_S4on(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_sample(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_sample2(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_save(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_saveToConn(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_saveplot(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_scan(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_search(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_seq(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_seq_along(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_seq_len(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_sequence(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_serialize(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_serializeToConn(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_serializeInfoFromConn(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_set(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_setS4Object(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_setFileTime(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_setencoding(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_setenv(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_seterrmessage(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_setmaxnumthreads(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_setnumthreads(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_setGraphicsEventEnv(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_setlocale(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_setseed(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_setSessionTimeLimit(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_setTimeLimit(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_setwd(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_shortRowNames(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_signalCondition(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_sink(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_sinknumber(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_sort(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_split(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_sprintf(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_standardGeneric(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_startsWith(SEXP call, SEXP op, SEXP args, SEXP env);
NORET SEXP do_stop(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_storage_mode(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_strrep(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_strsplit(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_strptime(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_strtrim(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_strtoi(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_syschmod(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_sysinfo(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_syssleep(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_sysumask(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_subassign(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_subassign_dflt(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_subassign2(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_subassign2_dflt(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_subassign3(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_subset(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_subset_dflt(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_subset2(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_subset2_dflt(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_subset3(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_substitute(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_substr(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_substrgets(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_summary(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_switch(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_sys(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_sysbrowser(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_sysgetpid(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_system(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_systime(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_tabulate(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_tempdir(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_tempfile(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_tilde(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_tolower(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_topenv(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_trace(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_traceOnOff(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_traceback(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_transpose(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_trunc(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_tryCatchHelper(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_tryWrap(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_typeof(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_unclass(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_unlink(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_unlist(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_unserializeFromConn(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_unsetenv(SEXP call, SEXP op, SEXP args, SEXP env);
NORET SEXP do_usemethod(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_utf8ToInt(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_validEnc(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_validUTF8(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_vapply(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_version(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_warning(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_while(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_which(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_withVisible(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_wrap_meta(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_xtfrm(SEXP call, SEXP op, SEXP args, SEXP env);

SEXP do_getSnapshot(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_playSnapshot(SEXP call, SEXP op, SEXP args, SEXP env);

SEXP R_do_data_class(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP R_do_set_class(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP R_getS4DataSlot(SEXP obj, SEXPTYPE type);

/* bytecode */
SEXP do_mkcode(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_bcclose(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_is_builtin_internal(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_disassemble(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_bcversion(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_loadfile(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_savefile(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_growconst(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_putconst(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_getconst(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_enablejit(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_compilepkgs(SEXP call, SEXP op, SEXP args, SEXP env);

/* Connections */
SEXP do_stdin(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_stdout(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_stderr(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_writelines(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_readbin(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_writebin(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_readchar(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_writechar(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_open(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_isopen(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_isincomplete(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_isseekable(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_close(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_fifo(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_pipe(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_url(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_gzfile(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_unz(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_seek(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_truncate(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_pushback(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_pushbacklength(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_clearpushback(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_rawconnection(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_rawconvalue(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_textconnection(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_textconvalue(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_getconnection(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_getallconnections(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_sumconnection(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_sockconn(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_serversocket(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_socktimeout(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_sockselect(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_gzcon(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_memCompress(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_memDecompress(SEXP call, SEXP op, SEXP args, SEXP env);

SEXP do_lockEnv(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_envIsLocked(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_lockBnd(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_bndIsLocked(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_mkActiveBnd(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_bndIsActive(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_activeBndFun(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_mkUnbound(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_isNSEnv(SEXP call, SEXP op, SEXP args, SEXP rho);
SEXP do_regNS(SEXP call, SEXP op, SEXP args, SEXP rho);
SEXP do_unregNS(SEXP call, SEXP op, SEXP args, SEXP rho);
SEXP do_getRegNS(SEXP call, SEXP op, SEXP args, SEXP rho);
SEXP do_getNSRegistry(SEXP call, SEXP op, SEXP args, SEXP rho);
SEXP do_importIntoEnv(SEXP call, SEXP op, SEXP args, SEXP rho);
SEXP do_envprofile(SEXP call, SEXP op, SEXP args, SEXP rho);

SEXP do_tracemem(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_retracemem(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_untracemem(SEXP call, SEXP op, SEXP args, SEXP env);

/* ALTREP-related */

SEXP do_sorted_fpass(SEXP call, SEXP op, SEXP args, SEXP env);

} //extern "C"


#endif /* not R_INTERNAL_H */
