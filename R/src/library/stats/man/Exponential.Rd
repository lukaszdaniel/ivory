% File src/library/stats/man/Exponential.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{Exponential}
\alias{Exponential}
\alias{dexp}
\alias{pexp}
\alias{qexp}
\alias{rexp}
\title{The Exponential Distribution}
\description{
  Density, distribution function, quantile function and random
  generation for the exponential distribution with rate \code{rate}
  (i.e., mean \code{1/rate}).
}
\usage{
dexp(x, rate = 1, log = FALSE)
pexp(q, rate = 1, lower.tail = TRUE, log.p = FALSE)
qexp(p, rate = 1, lower.tail = TRUE, log.p = FALSE)
rexp(n, rate = 1)
}
\arguments{
  \item{x, q}{vector of quantiles.}
  \item{p}{vector of probabilities.}
  \item{n}{number of observations. If \code{length(n) > 1}, the length
    is taken to be the number required.}
  \item{rate}{vector of rates.}
  \item{log, log.p}{logical; if TRUE, probabilities p are given as log(p).}
  \item{lower.tail}{logical; if TRUE (default), probabilities are
    \eqn{P[X \le x]}, otherwise, \eqn{P[X > x]}.}
}
\value{
  \code{dexp} gives the density,
  \code{pexp} gives the distribution function,
  \code{qexp} gives the quantile function, and
  \code{rexp} generates random deviates.

  The length of the result is determined by \code{n} for
  \code{rexp}, and is the maximum of the lengths of the
  numerical arguments for the other functions.  

  The numerical arguments other than \code{n} are recycled to the
  length of the result.  Only the first elements of the logical
  arguments are used.
}
\details{
  If \code{rate} is not specified, it assumes the default value of
  \code{1}.

  The exponential distribution with rate \eqn{\lambda} has density
  \deqn{f(x) = \lambda {e}^{- \lambda x}} for \eqn{x \ge 0}.
}
\source{
  \code{dexp}, \code{pexp} and \code{qexp} are all calculated
  from numerically stable versions of the definitions.

  \code{rexp} uses

  Ahrens, J. H. and Dieter, U. (1972).
  Computer methods for sampling from the exponential and normal distributions.
  \emph{Communications of the ACM}, \bold{15}, 873--882.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.

  Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995)
  \emph{Continuous Univariate Distributions}, volume 1, chapter 19.
  Wiley, New York.
}
\seealso{
  \code{\link{exp}} for the exponential function.

  \link{Distributions} for other standard distributions, including
  \code{\link{dgamma}} for the gamma distribution and
  \code{\link{dweibull}} for the Weibull distribution, both of which
  generalize the exponential.
}
\note{
  The cumulative hazard \eqn{H(t) = - \log(1 - F(t))}{H(t) = - log(1 - F(t))}
  is \code{-pexp(t, r, lower = FALSE, log = TRUE)}.
}
\examples{
dexp(1) - exp(-1) #-> 0

## a fast way to generate *sorted*  U[0,1]  random numbers:
rsunif <- function(n) { n1 <- n+1
   cE <- cumsum(rexp(n1)); cE[seq_len(n)]/cE[n1] }
plot(rsunif(1000), ylim=0:1, pch=".")
abline(0,1/(1000+1), col=adjustcolor(1, 0.5))
}
\keyword{distribution}
