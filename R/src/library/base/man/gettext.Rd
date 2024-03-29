% File src/library/base/man/gettext.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2021 R Core Team
% Distributed under GPL 2 or later

\name{gettext}
\alias{gettext}
\alias{ngettext}
\alias{bindtextdomain}
\title{Translate Text Messages}
\description{
  If Native Language Support (NLS) was enabled in this build of \R (see
  the \code{bindtextdomain()} example), attempt to
  translate character vectors or set where the translations are to be found.
}
\usage{
gettext(\dots, domain = NULL)

ngettext(n, msg1, msg2, domain = NULL)

bindtextdomain(domain, dirname = NULL)
}
\arguments{
  \item{\dots}{One or more character vectors.}
  \item{domain}{The \sQuote{domain} for the translation.}
  \item{n}{a non-negative integer.}
  \item{msg1}{the message to be used in English for \code{n = 1}.}
  \item{msg2}{the message to be used in English for \code{n = 0, 2, 3, \dots}.}
  \item{dirname}{The directory in which to find translated message
    catalogs for the domain.}
}
\details{
  If \code{domain} is \code{NULL} or \code{""}, and \code{gettext}
  or \code{ngettext}  is called from a function in the namespace of
  package \pkg{pkg} the domain is set to \code{"R-pkg"}.  Otherwise
  there is no default domain.  In \R versions up to 4.1.x, \code{"R-base"}
  has been chosen in spite of this.  Note in particular that \code{.onAttach}
  and \code{\link{.onLoad}} are not called from the package namespace, so
  messages there should always provide a domain explicitly.

  If a suitable domain is found, each character string is offered for
  translation, and replaced by its translation into the current language
  if one is found.  The value (logical) \code{NA} suppresses any
  translation.

  The \emph{language} to be used for message translation is determined by
  your OS default and/or the locale setting at \R's startup, see
  \code{\link{Sys.getlocale}()}, and notably the \env{LANGUAGE} environment
  variable.

  Conventionally the domain for \R warning/error messages in package
  \pkg{pkg} is \code{"R-pkg"}, and that for C-level messages is \code{"pkg"}.

  For \code{gettext}, leading and trailing whitespace is ignored when
  looking for the translation.

  \code{ngettext} is used where the message needs to vary by a single
  integer.  Translating such messages is subject to very specific rules
  for different languages: see the GNU Gettext Manual.  The string
  will often contain a single instance of \code{\%d} to be used in
  \code{\link{sprintf}}.  If English is used, \code{msg1} is returned if
  \code{n == 1} and \code{msg2} in all other cases.

  \code{bindtextdomain} is a wrapper for the C function of the same
  name: your system may have a \command{man} page for it.  With a
  non-\code{NULL} \code{dirname} it specifies where to look for message
  catalogues: with \code{domain = NULL} it returns the current location.
}
\value{
  For \code{gettext}, a character vector, one element per string in
  \code{\dots}.  If translation is not enabled or no domain is found or
  no translation is found in that domain, the original strings are
  returned.

  For \code{ngettext}, a character string.

  For \code{bindtextdomain}, a character string giving the current base
  directory, or \code{NULL} if setting it failed.
}
\seealso{
  \code{\link{stop}} and \code{\link{warning}} make use of \code{gettext} to
  translate messages.

  \code{\link{xgettext}} (package \pkg{tools}) for extracting translatable
  strings from \R source files.
}
\examples{
bindtextdomain("R")  # non-null if and only if NLS is enabled

for(n in 0:3)
    print(sprintf(ngettext(n, "\%d variable has missing values",
                              "\%d variables have missing values"),
                  n))

\dontrun{
## for translation, those strings should appear in R-pkg.pot as
msgid        "\%d variable has missing values"
msgid_plural "\%d variables have missing values"
msgstr[0] ""
msgstr[1] ""
}

miss <- c("one", "or", "another")
cat(ngettext(length(miss), "variable", "variables"),
    paste(sQuote(miss), collapse = ", "),
    ngettext(length(miss), "contains", "contain"), "missing values\n")

## better for translators would be to use
cat(sprintf(ngettext(length(miss),
                     "variable \%s contains missing values\n",
                     "variables \%s contain missing values\n"),
            paste(sQuote(miss), collapse = ", ")))
}
\keyword{ utilities }
\keyword{ character }
