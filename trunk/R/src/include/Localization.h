/* Localization */

#ifndef LOCALIZATION_H
#define LOCALIZATION_H

#ifdef ENABLE_NLS
 #include <libintl.h>
 #define _(String) gettext(String)
 #define gettext_noop(String) String
 #define N_(String) gettext_noop (String)
#else /* not NLS */
 #define _(String) (String)
 #define N_(String) String
 #define ngettext(String, StringP, N) (N > 1 ? StringP: String)
 #define dngettext(Domain, String, StringP, N) ngettext(String, StringP, N)
#endif

#endif /* LOCALIZATION_H */
