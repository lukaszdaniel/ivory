# file MASS/R/rms.curv.R
# copyright (C) 1994-2002 W. N. Venables and B. D. Ripley
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 or 3 of the License
#  (at your option).
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/
#
"rms.curv"<-
function(obj)
{
  fit.val <- obj$m$fitted()
  v <- attr(fit.val, "gradient")
  if(is.null(v)) stop(gettextf("%s attribute is missing", dQuote("gradient")))
  a <- attr(fit.val, "hessian")
  if(is.null(a)) stop(gettextf("%s attribute is missing", dQuote("hessian")))
  p <- ncol(v)
  n <- nrow(v)
  s <- sqrt(deviance(obj)/(n - p))
  sp <- s * sqrt(p)
  D <- v
  for(j in seq_len(p)) D <- cbind(D, a[, seq_len(j), j])
  qrd <- qr(D)
  Q <- qr.Q(qrd)
  rnk <- qrd$rank
  if(rnk <= p) warning("regression is apparently linear")
  Q1 <- Q[, seq_len(rnk)]
  C <- array(0, c(rnk, p, p))
  for(j in seq_len(p)) C[,  , j] <- crossprod(Q1, a[,  , j])
  C <- aperm(C, c(2, 3, 1))
  r11i <- solve(qr.R(qrd)[seq_len(p), seq_len(p)])
  ct <- 0
  for(j in seq_len(p)) {
    C[,  , j] <- crossprod(r11i, C[,  , j]) %*% r11i * sp
    ct <- ct + 2 * sum(C[,  , j]^2) + sum(diag(C[,  , j]))^2
  }
  ci <- 0
  for(j in (p + 1):rnk) {
    C[,  , j] <- crossprod(r11i, C[,  , j]) %*% r11i * sp
    ci <- ci + 2 * sum(C[,  , j]^2) + sum(diag(C[,  , j]))^2
  }
  ct <- sqrt(ct/(p * (p + 2)))
  ci <- sqrt(ci/(p * (p + 2)))
  pe <- ct * sqrt(qf(19/20, p, n - p))
  ic <- ci * sqrt(qf(19/20, p, n - p))
  val <- list(pe = pe, ic = ic, ct = ct, ci = ci, C = C)
  class(val) <- "rms.curv"
  val
}
"print.rms.curv"<- function(x, ...)
{
  cat(gettextf("Parameter effects: c^theta x sqrt(F) = %s", round(x$pe, 4), domain = "R-MASS"), "\n",
      "       ", gettextf("Intrinsic: c^iota  x sqrt(F) = %s", round(x$ic, 4), domain = "R-MASS"), "\n",
      ..., sep = "")
  invisible(x)
}
