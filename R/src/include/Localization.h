/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999--2020  The R Core Team.
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
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

/** @file Localization.h
 *
 * @brief Macro definitions related to localization.
 */

#ifndef LOCALIZATION_H
#define LOCALIZATION_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext("R", String)
#define n_(String, StringP, N) dngettext("R", String, StringP, N)
#define gettext_noop(String) String
#define N_(String) gettext_noop(String)
#define G_(String) dgettext("RGui", String)
#define GN_(String) gettext_noop(String)
#else /* not NLS */
#define _(String) (String)
#define n_(String, StringP, N) (N > 1 ? StringP : String)
#define N_(String) String
#define G_(String) (String)
#define GN_(String) String
#endif

#endif /* LOCALIZATION_H */
