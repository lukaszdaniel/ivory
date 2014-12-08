/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 2005  The R Core Team
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
 *  http://www.r-project.org/Licenses/
 */

#ifdef ENABLE_NLS
 #include <libintl.h>
 #define _(String) dgettext("R", String)
 #define n_(String, StringP, N) dngettext("R", String, StringP, N)
 #define gettext_noop(String) String
 #define N_(String) gettext_noop (String)
 #define G_(String) dgettext("RGui", String)
 #define GN_(String) gettext_noop (String)
#else /* not NLS */
 #define _(String) (String)
 #define N_(String) String
 #define G_(String) (String)
 #define GN_(String) String
 #define ngettext(String, StringP, N) (N > 1 ? StringP: String)
 #define dngettext(Domain, String, StringP, N) ngettext(String, StringP, N)
 #define n_(String, StringP, N) ngettext(String, StringP, N)
#endif
