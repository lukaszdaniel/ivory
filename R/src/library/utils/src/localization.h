/* Localization */

#ifndef LOCALIZATION_H
#define LOCALIZATION_H

#ifdef ENABLE_NLS
 #include <libintl.h>
 #ifdef Win32
  #define _(String) libintl_dgettext ("utils", String)
  #undef gettext /* needed for graphapp */
  #define G_(String) libintl_dgettext("RGui", String)
 #else
  #define _(String) dgettext ("utils", String)
 #endif
 #define gettext_noop(String) String
 #define N_(String) gettext_noop (String)
 #define GN_(String) gettext_noop (String)
#else /* not NLS */
 #define _(String) (String)
 #define N_(String) String
 #define G_(String) (String)
 #define GN_(String) String
 #define ngettext(String, StringP, N) (N > 1 ? StringP: String)
 #define dngettext(Domain, String, StringP, N) ngettext(String, StringP, N)
#endif

#endif /* LOCALIZATION_H */
