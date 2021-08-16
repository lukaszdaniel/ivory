![Build](https://github.com/lukaszdaniel/ivory/actions/workflows/build-ivory.yml/badge.svg)

# What is IVORY? #
IVORY is a project based on R-devel platform. Its aim is to deliver internationalization of R and its recommended packages.

Work began since R-3.0.0 and is being continued.

R to some extent supports internationalization, yet the scope in my opinion isn't sufficient (i.e. right now mostly error and warning messages are translatable).


IVORY was tested on Ubuntu 16.04 (64 bit) and Windows 7 (64 bit).

# Goals #
Work on IVORY focuses on delivering solution that is functionally 100% compatible with original R-devel, and nonfunctional aspects (such as messages) differ only when it's necessary or it increases usability.



# Changes in R-devel and recommended packages #
Notice that modifications listed below should not change functional side of R, but its nonfunctional only.

Significant changes:

1. C source code in R and recommended packages
    * Where possible, similar messages have been standardized. New messages have been enabled for translation.
2. R and its recommended packages
    * Where possible, similar messages have been standardized. Consistent usage of sQuote()/dQuote() in R files. New messages have been enabled for translation.
3. R and its recommended packages
    * Where possible, text informations printed via 'cat()' have been embedded in gettext/gettextf/ngettext, in order to enable translation.
4. Stats/htest.R
    * Class „htest” gains an extra component (alt.name), in which complete statement for alternative hypotesis is being kept.
5. Utils/help.search.R
    * In order to get translatable format, message informing that no demo/help/vignette has been found, has been split into 3 separate messages.
6. Tools/xgettext.R
    * R-\*.pot files for each translatable message indicate the file where the message is defined.
7. Include/R\_ext/Minmax.h
    * Multiple definitions of min/max spread in \*.c and \*.h files have been gathered, merged and put in one header file named 'R\_ext/Minmax.h'.
8. Include/R\_ext/Itermacros.h
    * 'mod\_iterate\*(…)' definitions located in arithmetic.c, complex.c, relop.c files have been moved to existing 'Itermacros.h' header file.
9. Include/Localization.h
    * NLS declaration has been taken out from Defn.h file and put in a separate header named 'Localization.h'. From now on it should be added separately in R-core \*.c files.
10. Recommended packages/localization.h
    * Multiple NLS declarations have been replaced by one stored in 'localization.h' file. From now on it should be added separately in \*.c files.
11. Tests in R and its recommended packages
    * Due to changes in R and recommended packages, all '\*.Rout.save' files have been updated. Of course numerical results for obvious reasons were not updated.
12. main/errors.c
    * 'PrintWarnings' function gains an argument. Now texts "Additional warning messages" and "Warning messages during startup" present in R are translatable.

Complete list of changes in R and its recommended packages can be found in 'ChangeLog' file for R and 'Changes' for each package.
