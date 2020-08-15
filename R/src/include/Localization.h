/* Localization */

/** @file Localization.h
 *
 * @brief Macro definitions relating to localization.
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
 #define N_(String) gettext_noop (String)
 #define G_(String) dgettext("RGui", String)
 #define GN_(String) gettext_noop (String)
#else /* not NLS */
 #define _(String) (String)
 #define n_(String, StringP, N) (N > 1 ? StringP: String)
 #define N_(String) String
 #define G_(String) (String)
 #define GN_(String) String
#endif

#endif /* LOCALIZATION_H */
