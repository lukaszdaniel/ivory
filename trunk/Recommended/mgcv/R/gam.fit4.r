## (c) Simon N. Wood (2013,2014). Provided under GPL 2.
## Routines for gam estimation beyond exponential family.



dDeta <- function(y,mu,wt,theta,fam,deriv=0) {
## What is available directly from the family are derivatives of the 
## deviance and link w.r.t. mu. This routine converts these to the
## required derivatives of the deviance w.r.t. eta.
## deriv is the order of derivative of the smoothing parameter score 
## required.
## This version is based on ratios of derivatives of links rather 
## than raw derivatives of links. g2g = g''/g'^2, g3g = g'''/g'^3 etc 
   r <- fam$Dd(y, mu, theta, wt, level=deriv)  
   d <- list(Deta=0,Dth=0,Dth2=0,Deta2=0,EDeta2=0,Detath=0,
             Deta3=0,Deta2th=0,Detath2=0,
             Deta4=0,Deta3th=0,Deta2th2=0)
   if (fam$link=="identity") { ## don't waste time on transformation
      d$Deta <- r$Dmu;d$Deta2 <- r$Dmu2
      d$EDeta2 <- r$EDmu2;d$Deta.Deta2 <- r$Dmu/r$Dmu2
      d$Deta.EDeta2 <- r$Dmu/r$EDmu2
      if (deriv>0) {
        d$Dth <- r$Dth; d$Detath <- r$Dmuth
        d$Deta3 <- r$Dmu3; d$Deta2th <- r$Dmu2th
      }
      if (deriv>1) {
        d$Deta4 <- r$Dmu4; d$Dth2 <- r$Dth2; d$Detath2 <- r$Dmuth2
        d$Deta2th2 <- r$Dmu2th2; d$Deta3th <- r$Dmu3th
      }
      return(d)
   }

   ig1 <- fam$mu.eta(fam$linkfun(mu)) 
   ig12 <- ig1^2
   
   g2g <- fam$g2g(mu)

##   ig12 <- ig1^2;ig13 <- ig12 * ig1

   d$Deta <- r$Dmu * ig1
   d$Deta2 <- r$Dmu2*ig12 - r$Dmu*g2g*ig1
   d$EDeta2 <- r$EDmu2*ig12
   d$Deta.Deta2 <- r$Dmu/(r$Dmu2*ig1 - r$Dmu*g2g)
   d$Deta.EDeta2 <- r$Dmu/(r$EDmu2*ig1)
   if (deriv>0) {
      ig13 <- ig12 * ig1
      d$Dth <- r$Dth 
      d$Detath <- r$Dmuth * ig1
      g3g <- fam$g3g(mu)
      d$Deta3 <- r$Dmu3*ig13 - 3*r$Dmu2 * g2g * ig12 + r$Dmu * (3*g2g^2 - g3g)*ig1
      d$Deta2th <- r$Dmu2th*ig12 - r$Dmuth*g2g*ig1
   }
   if (deriv>1) {
     g4g <- fam$g4g(mu)
     d$Deta4 <- ig12^2*r$Dmu4 - 6*r$Dmu3*ig13*g2g + r$Dmu2*(15*g2g^2-4*g3g)*ig12 - 
                       r$Dmu*(15*g2g^3-10*g2g*g3g  +g4g)*ig1
     d$Dth2 <- r$Dth2
     d$Detath2 <- r$Dmuth2 * ig1 
     d$Deta2th2 <- ig12*r$Dmu2th2 - r$Dmuth2*g2g*ig1
     d$Deta3th <-  ig13*r$Dmu3th - 3 *r$Dmu2th*g2g*ig12 + r$Dmuth*(3*g2g^2-g3g)*ig1
   }
   d
} ## dDmu

fetad.test <- function(y,mu,wt,theta,fam,eps = 1e-7,plot=TRUE) {
## test family derivatives w.r.t. eta
  
  dd <- dDeta(y,mu,wt,theta,fam,deriv=2)
  dev <- fam$dev.resids(y, mu, wt,theta)
  mu1 <- fam$linkinv(fam$linkfun(mu)+eps)
  dev1 <- fam$dev.resids(y,mu1, wt,theta)
  Deta.fd <- (dev1-dev)/eps
  cat(gettextf("Deta: rdiff = %s cor = %s", paste(range(dd$Deta-Deta.fd), collapse = " "), cor(dd$Deta,Deta.fd), domain = "R-mgcv"), "\n", sep = "")
  plot(dd$Deta,Deta.fd);abline(0,1)
  nt <- length(theta)
  for (i in seq_len(nt)) {
    th1 <- theta;th1[i] <- th1[i] + eps
    dev1 <- fam$dev.resids(y, mu, wt,th1)
    Dth.fd <- (dev1-dev)/eps
    um <- if (nt>1) dd$Dth[,i] else dd$Dth
    cat(gettextf("Dth[%d]: rdiff = %s cor = %s", i, paste(range(um-Dth.fd), collapse = " "), cor(um,Dth.fd), domain = "R-mgcv"), "\n", sep = "")
    plot(um,Dth.fd);abline(0,1)
  }
  ## second order up...
  dd1 <- dDeta(y,mu1,wt,theta,fam,deriv=2)
  Deta2.fd <- (dd1$Deta - dd$Deta)/eps
  cat(gettextf("Deta2: rdiff = %s cor = %s", paste(range(dd$Deta2-Deta2.fd), collapse = " "), cor(dd$Deta2,Deta2.fd), domain = "R-mgcv"), "\n", sep = "")
  plot(dd$Deta2,Deta2.fd);abline(0,1)
  Deta3.fd <- (dd1$Deta2 - dd$Deta2)/eps
  cat(gettextf("Deta3: rdiff = %s cor = %s", paste(range(dd$Deta3-Deta3.fd), collapse = " "), cor(dd$Deta3,Deta3.fd), domain = "R-mgcv"), "\n", sep = "")
  plot(dd$Deta3,Deta3.fd);abline(0,1)
  Deta4.fd <- (dd1$Deta3 - dd$Deta3)/eps
  cat(gettextf("Deta4: rdiff = %s cor = %s", paste(range(dd$Deta4-Deta4.fd), collapse = " "), cor(dd$Deta4,Deta4.fd), domain = "R-mgcv"), "\n", sep = "")
  plot(dd$Deta4,Deta4.fd);abline(0,1)
  ## and now the higher derivs wrt theta...
  ind <- seq_len(nt)
  for (i in seq_len(nt)) {
    th1 <- theta;th1[i] <- th1[i] + eps
    dd1 <- dDeta(y,mu,wt,th1,fam,deriv=2)
    Detath.fd <- (dd1$Deta - dd$Deta)/eps
    um <- if (nt>1) dd$Detath[,i] else dd$Detath
    cat(gettextf("Detath[%d]: rdiff = %s cor = %s", i, paste(range(um-Detath.fd), collapse = " "), cor(um,Detath.fd), domain = "R-mgcv"), "\n", sep = "")
    plot(um,Detath.fd);abline(0,1)
    Deta2th.fd <- (dd1$Deta2 - dd$Deta2)/eps
    cat(gettextf("Deta2th[%d]: rdiff = %s cor = %s", i, paste(range(dd$Deta2th-Deta2th.fd), collapse = " "), cor(dd$Deta2th,Deta2th.fd), domain = "R-mgcv"), "\n", sep = "")
    Deta3th.fd <- (dd1$Deta3 - dd$Deta3)/eps
    cat(gettextf("Deta3th[%d]: rdiff = %s cor = %s", i, paste(range(dd$Deta3th-Deta3th.fd), collapse = " "), cor(dd$Deta3th,Deta3th.fd), domain = "R-mgcv"), "\n", sep = "")
    ## now the 3 second derivative w.r.t. theta terms

    Dth2.fd <- (dd1$Dth - dd$Dth)/eps
    cat(gettextf("Dth2[%d]: rdiff = %s cor = %s", i, paste(range(dd$Dth2-Dth2.fd), collapse = " "), cor(dd$Dth2,Dth2.fd), domain = "R-mgcv"), "\n", sep = "")
    Detath2.fd <- (dd1$Detath - dd$Detath)/eps
    cat(gettextf("Deta2th[%d]: rdiff = %s cor = %s", i, paste(range(dd$Deta2th-Deta2th.fd), collapse = " "), cor(dd$Deta2th,Deta2th.fd), domain = "R-mgcv"), "\n", sep = "")
    Deta2th2.fd <- (dd1$Deta2th - dd$Deta2th)/eps
    cat(gettextf("Deta2th2[%d]: rdiff = %s cor = %s", i, paste(range(dd$Deta2th2-Deta2th2.fd), collapse = " "), cor(dd$Deta2th2,Deta2th2.fd), domain = "R-mgcv"), "\n", sep = "")
  }
} ## fetad.test

fmud.test <- function(y,mu,wt,theta,fam,eps = 1e-7) {
## test family deviance derivatives w.r.t. mu
  dd <- fam$Dd(y, mu, theta, wt, level=2) 
  dev <- fam$dev.resids(y, mu, wt,theta)
  dev1 <- fam$dev.resids(y, mu+eps, wt,theta)
  Dmu.fd <- (dev1-dev)/eps
  cat(gettextf("Dmu: rdiff = %s cor = %s", paste(range(dd$Dmu-Dmu.fd), collapse = " "), cor(dd$Dmu,Dmu.fd), domain = "R-mgcv"), "\n", sep = "")
  for (i in seq_len(length(theta))) {
    th1 <- theta;th1[i] <- th1[i] + eps
    dev1 <- fam$dev.resids(y, mu, wt,th1)
    Dth.fd <- (dev1-dev)/eps
    cat(gettextf("Dth[%d]: rdiff = %s cor = %s", i, paste(range(dd$Dth-Dth.fd), collapse = " "), cor(dd$Dth,Dth.fd), domain = "R-mgcv"), "\n", sep = "")
  }
  ## second order up...
  dd1 <- fam$Dd(y, mu+eps, theta, wt, level=2)
  Dmu2.fd <- (dd1$Dmu - dd$Dmu)/eps
  cat(gettextf("Dmu2: rdiff = %s cor = %s", paste(range(dd$Dmu2-Dmu2.fd), collapse = " "), cor(dd$Dmu2,Dmu2.fd), domain = "R-mgcv"), "\n", sep = "")
  Dmu3.fd <- (dd1$Dmu2 - dd$Dmu2)/eps
  cat(gettextf("Dmu3: rdiff = %s cor = %s", paste(range(dd$Dmu3-Dmu3.fd), collapse = " "), cor(dd$Dmu3,Dmu3.fd), domain = "R-mgcv"), "\n", sep = "")
  Dmu4.fd <- (dd1$Dmu3 - dd$Dmu3)/eps
  cat(gettextf("Dmu4: rdiff = %s cor = %s", paste(range(dd$Dmu4-Dmu4.fd), collapse = " "), cor(dd$Dmu4,Dmu4.fd), domain = "R-mgcv"), "\n", sep = "")
  ## and now the higher derivs wrt theta 
  for (i in seq_len(length(theta))) {
    th1 <- theta;th1[i] <- th1[i] + eps
    dd1 <- fam$Dd(y, mu, th1, wt, level=2)
    Dmuth.fd <- (dd1$Dmu - dd$Dmu)/eps
    cat(gettextf("Dmuth[%d]: rdiff = %s cor = %s", i, paste(range(dd$Dmuth-Dmuth.fd), collapse = " "), cor(dd$Dmuth,Dmuth.fd), domain = "R-mgcv"), "\n", sep = "")
    Dmu2th.fd <- (dd1$Dmu2 - dd$Dmu2)/eps
    cat(gettextf("Dmu2th[%d]: rdiff = %s cor = %s", i, paste(range(dd$Dmu2th-Dmu2th.fd), collapse = " "), cor(dd$Dmu2th,Dmu2th.fd), domain = "R-mgcv"), "\n", sep = "")
    Dmu3th.fd <- (dd1$Dmu3 - dd$Dmu3)/eps
    cat(gettextf("Dmu3th[%d]: rdiff = %s cor = %s", i, paste(range(dd$Dmu3th-Dmu3th.fd), collapse = " "), cor(dd$Dmu3th,Dmu3th.fd), domain = "R-mgcv"), "\n", sep = "")
    ## now the 3 second derivative w.r.t. theta terms
    Dth2.fd <- (dd1$Dth - dd$Dth)/eps
    cat(gettextf("Dth2[%d]: rdiff = %s cor = %s", i, paste(range(dd$Dth2-Dth2.fd), collapse = " "), cor(dd$Dth2,Dth2.fd), domain = "R-mgcv"), "\n", sep = "")
    Dmuth2.fd <- (dd1$Dmuth - dd$Dmuth)/eps
    cat(gettextf("Dmu2th[%d]: rdiff = %s cor = %s", i, paste(range(dd$Dmu2th-Dmu2th.fd), collapse = " "), cor(dd$Dmu2th,Dmu2th.fd), domain = "R-mgcv"), "\n", sep = "")
    Dmu2th2.fd <- (dd1$Dmu2th - dd$Dmu2th)/eps
    cat(gettextf("Dmu2th2[%d]: rdiff = %s cor = %s", i, paste(range(dd$Dmu2th2-Dmu2th2.fd), collapse = " "), cor(dd$Dmu2th2,Dmu2th2.fd), domain = "R-mgcv"), "\n", sep = "")
  }
}



gam.fit4 <- function(x, y, sp, Eb,UrS=list(),
            weights = rep(1, nobs), start = NULL, etastart = NULL, 
            mustart = NULL, offset = rep(0, nobs),U1=diag(ncol(x)), Mp=-1, family = gaussian(), 
            control = gam.control(), deriv=2,
            scale=1,scoreType="REML",null.coef=rep(0,ncol(x)),...) {
## Routine for fitting GAMs beyond exponential family.
## Inputs as gam.fit3 except that family is of class "extended.family", while
## sp contains the vector of extended family parameters, followed by the log smoothing parameters,
## followed by the log scale parameter if scale < 0

  if (family$n.theta>0) { ## there are extra parameters to estimate
    ind <- seq_len(family$n.theta)
    theta <- sp[ind] ## parameters of the family
    family$putTheta(theta)
    sp <- sp[-ind]   ## log smoothing parameters
  } else theta <- family$getTheta() ## fixed value

  penalized <- if (length(UrS)>0) TRUE else FALSE

  if (scale>0) scale.known <- TRUE else {
    ## unknown scale parameter, trial value supplied as 
    ## final element of sp. 
    scale.known <- FALSE
    nsp <- length(sp)
    scale <- exp(sp[nsp])
    sp <- sp[-nsp]
  }
  
  x <- as.matrix(x)  
  nSp <- length(sp) 
  rank.tol <- .Machine$double.eps*100 ## tolerance to use for rank deficiency
  q <- ncol(x)
  n <- nobs <- nrow(x)  
  
  xnames <- dimnames(x)[[2]]
  ynames <- if (is.matrix(y)) rownames(y) else names(y)
  ## Now a stable re-parameterization is needed....

  if (length(UrS)) {
      rp <- gam.reparam(UrS,sp,deriv)
      T <- diag(q)
      T[seq_len(ncol(rp$Qs)),seq_len(ncol(rp$Qs))] <- rp$Qs
      T <- U1%*%T ## new params b'=T'b old params
    
      null.coef <- t(T)%*%null.coef  
     
      if (!is.null(start)) start <- t(T)%*%start

      ## form x%*%T in parallel 
      x <- .Call(C_mgcv_pmmult2,x,T,0,0,control$nthreads)
      rS <- list()
      for (i in seq_len(length(UrS))) {
        rS[[i]] <- rbind(rp$rS[[i]],matrix(0,Mp,ncol(rp$rS[[i]])))
      } ## square roots of penalty matrices in current parameterization
      Eb <- Eb%*%T ## balanced penalty matrix
      rows.E <- q-Mp
      Sr <- cbind(rp$E,matrix(0,nrow(rp$E),Mp))
      St <- rbind(cbind(rp$S,matrix(0,nrow(rp$S),Mp)),matrix(0,Mp,q))
  } else { 
      T <- diag(q); 
      St <- matrix(0,q,q) 
      rSncol <- rows.E <- Eb <- Sr <- 0   
      rS <- list(0)
      rp <- list(det=0,det1 = 0,det2 = 0,fixed.penalty=FALSE)
  }

  ## re-parameterization complete. Initialization....

  nvars <- ncol(x)
  if (nvars==0) stop("empty models are not available")
  if (is.null(weights)) weights <- rep.int(1, nobs)
  if (is.null(offset)) offset <- rep.int(0, nobs)

  ## call the families initialization code...

  if (is.null(mustart)) {
    eval(family$initialize)
  } else {
    mukeep <- mustart
    eval(family$initialize)
    mustart <- mukeep
  }
  
  ## and now finalize initialization of mu and eta...

  eta <- if (!is.null(etastart)) etastart
         else if (!is.null(start)) 
              if (length(start) != nvars) 
                  stop(gettextf("Length of start should equal %d and correspond to initial coefs for %s", nvars, deparse(xnames)))
              else {
                  coefold <- start
                  etaold <- offset + as.vector(if (NCOL(x) == 1) 
                  x * start
                  else x %*% start)
              }
              else family$linkfun(mustart)
 

   mu.eta <- family$mu.eta
   Dd <- family$Dd

   linkinv <- family$linkinv
   valideta <- family$valideta
   validmu <- family$validmu
   dev.resids <- family$dev.resids
 
   mu <- linkinv(eta);etaold <- eta
     
   ## need an initial `null deviance' to test for initial divergence...
   ## if (!is.null(start)) null.coef <- start - can be on edge of feasible - not good
   coefold <- null.coef
   null.eta <- as.numeric(x%*%null.coef + as.numeric(offset))
   old.pdev <- sum(dev.resids(y, linkinv(null.eta), weights,theta)) + t(null.coef)%*%St%*%null.coef 
   conv <-  boundary <- FALSE
 
   for (iter in seq_len(control$maxit)) { ## start of main fitting iteration 
      if (control$trace) cat(iter," ")
      dd <- dDeta(y,mu,weights,theta,family,0) ## derivatives of deviance w.r.t. eta
     
      good <- is.finite(dd$Deta.Deta2)
      if (control$trace&sum(!good)>0) cat("\n",sum(!good)," not good\n") 
      w <- dd$Deta2 * .5;
      if (sum(!good)) {
        good1 <- is.finite(w)&good ## make sure w finite too
        w[!is.finite(w)] <- 0      ## clear infinite w
        w[!good1&w==0] <- max(w)*.Machine$double.eps^.5 ## reset zero value weights for problem elements
        dd$Deta.Deta2[!good] <- .5*dd$Deta[!good]/w[!good] ## reset problem elements to finite
        good <- is.finite(dd$Deta.Deta2) ## check in case Deta not finite, for example
      }
      z <- (eta-offset)[good] - dd$Deta.Deta2[good] ## - .5 * dd$Deta[good] / w
      
      oo <- .C(C_pls_fit1,   ##C_pls_fit1, reinstate for use in mgcv
               y=as.double(z),X=as.double(x[good,]),w=as.double(w),
                     E=as.double(Sr),Es=as.double(Eb),n=as.integer(sum(good)),
                     q=as.integer(ncol(x)),rE=as.integer(rows.E),eta=as.double(z),
                     penalty=as.double(1),rank.tol=as.double(rank.tol),
                     nt=as.integer(control$nthreads))
      if (oo$n<0) { ## then problem is indefinite - switch to +ve weights for this step
        if (control$trace) cat(gettext("**using positive weights", domain = "R-mgcv"), "\n", sep = "")
        # problem is that Fisher can be very poor for zeroes  

        ## index weights that are finite and positive 
        good <- is.finite(dd$Deta2)
        good[good] <- dd$Deta2[good]>0 
        w <- dd$Deta2*.5; w[!good] <- 0
        thresh <- max(w[good])*.Machine$double.eps^.5
        w[w < thresh] <- thresh
        good <- is.finite(dd$Deta)
        z <- (eta-offset)[good] - .5 * dd$Deta[good] / w[good]
       
        oo <- .C(C_pls_fit1, ##C_pls_fit1,
                  y=as.double(z),X=as.double(x[good,]),w=as.double(w),
                     E=as.double(Sr),Es=as.double(Eb),n=as.integer(sum(good)),
                     q=as.integer(ncol(x)),rE=as.integer(rows.E),eta=as.double(z),
                     penalty=as.double(1),rank.tol=as.double(rank.tol),
                     nt=as.integer(control$nthreads))
      }
      start <- oo$y[seq_len(ncol(x))] ## current coefficient estimates
      penalty <- oo$penalty ## size of penalty

      eta <- drop(x%*%start) ## the linear predictor (less offset)

      if (any(!is.finite(start))) { ## test for breakdown
          conv <- FALSE
          warning(gettextf("Non-finite coefficients at iteration %d", iter))
          break
      }        
     
      mu <- linkinv(eta <- eta + offset)
      dev <- sum(dev.resids(y, mu, weights,theta)) 

      ## now step halve under non-finite deviance...
      if (!is.finite(dev)) {
         if (is.null(coefold)) {
            if (is.null(null.coef)) 
              stop("no valid set of coefficients has been found:please supply starting values", call. = FALSE)
            ## Try to find feasible coefficients from the null.coef and null.eta
            coefold <- null.coef
            etaold <- null.eta
         }
         warning("Step size truncated due to divergence", call. = FALSE)
         ii <- 1
         while (!is.finite(dev)) {
               if (ii > control$maxit) 
                    stop(gettextf("inner loop %d; can't correct step size", 1))
               ii <- ii + 1
               start <- (start + coefold)/2
               eta <- (eta + etaold)/2               
               mu <- linkinv(eta)
               dev <- sum(dev.resids(y, mu, weights,theta))
              
         }
         boundary <- TRUE
         penalty <- t(start)%*%St%*%start ## reset penalty too
         if (control$trace) 
                  cat(gettextf("Step halved: new deviance = %g", dev, domain = "R-mgcv"), "\n", sep = "")
      } ## end of infinite deviance correction

      ## now step halve if mu or eta are out of bounds... 
      if (!(valideta(eta) && validmu(mu))) {
         warning("Step size truncated: out of bounds", call. = FALSE)
         ii <- 1
         while (!(valideta(eta) && validmu(mu))) {
                  if (ii > control$maxit) 
                    stop(gettextf("inner loop %d; can't correct step size", 2))
                  ii <- ii + 1
                  start <- (start + coefold)/2
                  eta <- (eta + etaold)/2 
                  mu <- linkinv(eta)
         }
         boundary <- TRUE
         dev <- sum(dev.resids(y, mu, weights))
         penalty <- t(start)%*%St%*%start ## need to reset penalty too
         if (control$trace) 
                  cat(gettextf("Step halved: new deviance = %g", dev, domain = "R-mgcv"), "\n", sep = "")
      } ## end of invalid mu/eta handling

      ## now check for divergence of penalized deviance....
  
      pdev <- dev + penalty  ## the penalized deviance 
      if (control$trace) cat(gettextf("penalized deviance = %s", pdev, domain = "R-mgcv"), "\n", sep = "")
     
      div.thresh <- 10*(.1+abs(old.pdev))*.Machine$double.eps^.5

      if (pdev-old.pdev>div.thresh) { ## solution diverging
         ii <- 1 ## step halving counter
         if (iter==1) { ## immediate divergence, need to shrink towards zero 
               etaold <- null.eta; coefold <- null.coef
         }
         while (pdev -old.pdev > div.thresh)  { ## step halve until pdev <= old.pdev
           if (ii > 100) 
              stop(gettextf("inner loop %d; can't correct step size", 3))
           ii <- ii + 1
           start <- (start + coefold)/2 
           eta <- (eta + etaold)/2               
           mu <- linkinv(eta)
           dev <- sum(dev.resids(y, mu, weights,theta))
       
           pdev <- dev + t(start)%*%St%*%start ## the penalized deviance
           if (control$trace) 
                  cat(gettextf("Step halved: new penalized deviance = %g", pdev, domain = "R-mgcv"), "\n", sep = "")
        }
     } ## end of pdev divergence

     ## convergence testing...

     if (abs(pdev - old.pdev)/(0.1 + abs(pdev)) < control$epsilon) {
       if (max(abs(start-coefold))>control$epsilon*max(abs(start+coefold))/2) {
         old.pdev <- pdev  ## not converged quite enough
         coef <- coefold <- start
         etaold <- eta 
         muold <- mu
       } else { ## converged
         conv <- TRUE
         coef <- start
         break 
       }
     } else { ## not converged
       old.pdev <- pdev
       coef <- coefold <- start
       etaold <- eta 
     }
   } ## end of main loop
   
   ## so at this stage the model has been fully estimated
   coef <- as.numeric(T %*% coef)
 
   ## now obtain derivatives, if these are needed...
   check.derivs <- FALSE
   while (check.derivs) { ## debugging code to check derivatives
     eps <- 1e-7
     fmud.test(y,mu,weights,theta,family,eps = eps)
     fetad.test(y,mu,weights,theta,family,eps = eps)
   }   

   dd <- dDeta(y,mu,weights,theta,family,deriv)
   w <- dd$Deta2 * .5

   ## exclude points for which gradient and second deriv are effectively zero and 
   ## points with non finite second deriv or deriv ratio... 
   min.Deta <- mean(abs(dd$Deta[is.finite(dd$Deta)]))*.Machine$double.eps*.001
   min.Deta2 <- mean(abs(dd$Deta2[is.finite(dd$Deta2)]))*.Machine$double.eps*.001
   good <- is.finite(dd$Deta.Deta2)&is.finite(w)&!(abs(dd$Deta2) < min.Deta2 & abs(dd$Deta) < min.Deta) 
   if (control$trace&sum(!good)>0) cat("\n",sum(!good)," not good\n")
   w <- w[good] 

   z <- (eta-offset)[good] - dd$Deta.Deta2[good] ## - .5 * dd$Deta[good] / w
   wf <- dd$EDeta2[good] * .5 ## Fisher type weights 

   residuals <- rep.int(NA, nobs)
   residuals[good] <- z - (eta - offset)[good]

   ntot <- length(theta) + length(sp)
   if (deriv>1) n2d <- ntot*(1+ntot)/2 else n2d <- 0 
   rSncol <- unlist(lapply(UrS,ncol))
   ## Now drop any elements of dd that have been dropped in fitting...
   if (sum(!good)>0) { ## drop !good from fields of dd
     dd$Deta <- dd$Deta[good];dd$Deta2 <- dd$Deta2[good] 
     dd$EDeta2 <- dd$EDeta2[good]
     if (deriv>0) dd$Deta3 <- dd$Deta3[good]
     if (deriv>1) dd$Deta4 <- dd$Deta4[good]
     if (length(theta)>1) {
         if (deriv>0) {  
         dd$Dth <- dd$Dth[good,]; 
         dd$Detath <- dd$Detath[good,]; dd$Deta2th <- dd$Deta2th[good,]
         if (deriv>1) {  
           dd$Detath2 <- dd$Detath2[good,]; dd$Deta3th <- dd$Deta3th[good,]
           dd$Deta2th2 <- dd$Deta2th2[good,];dd$Dth2 <- dd$Dth2[good,]
         }
       }
     } else {
       if (deriv>0) { 
         dd$Dth <- dd$Dth[good]; 
         dd$Detath <- dd$Detath[good]; dd$Deta2th <- dd$Deta2th[good]
         if (deriv>1) {
           dd$Detath2 <- dd$Detath2[good]; dd$Deta3th <- dd$Deta3th[good]
           dd$Deta2th2 <- dd$Deta2th2[good]; dd$Dth2 <- dd$Dth2[good]
         }
       } 
     }
   }
   ## can't have zero weights in gdi2 call
   mwb <- max(abs(w))*.Machine$double.eps
   mwa <- min(abs(w[w!=0]))*.0001; if (mwa==0) mwa <- mwb
   w[w==0] <- min(mwa,mwb);
   oo <- .C(C_gdi2,
            X=as.double(x[good,]),E=as.double(Sr),Es=as.double(Eb),rS=as.double(unlist(rS)),
            U1 = as.double(U1),sp=as.double(exp(sp)),theta=as.double(theta),
            z=as.double(z),w=as.double(w),wf=as.double(wf),Dth=as.double(dd$Dth),Det=as.double(dd$Deta),
            Det2=as.double(dd$Deta2),Dth2=as.double(dd$Dth2),Det.th=as.double(dd$Detath),
            Det2.th=as.double(dd$Deta2th),Det3=as.double(dd$Deta3),Det.th2 = as.double(dd$Detath2),
            Det4 = as.double(dd$Deta4),Det3.th=as.double(dd$Deta3th), Deta2.th2=as.double(dd$Deta2th2),
            beta=as.double(coef),b1=as.double(rep(0,ntot*ncol(x))),
            D1=as.double(rep(0,ntot)),D2=as.double(rep(0,ntot^2)),
            P=as.double(0),P1=as.double(rep(0,ntot)),P2 = as.double(rep(0,ntot^2)),
            ldet=as.double(1-2*(scoreType=="ML")),ldet1 = as.double(rep(0,ntot)), 
            ldet2 = as.double(rep(0,ntot^2)),
            rV=as.double(rep(0,ncol(x)^2)),
            rank.tol=as.double(.Machine$double.eps^.75),rank.est=as.integer(0),
	    n=as.integer(sum(good)),q=as.integer(ncol(x)),M=as.integer(nSp),
            n.theta=as.integer(length(theta)), Mp=as.integer(Mp),Enrow=as.integer(rows.E),
            rSncol=as.integer(rSncol),deriv=as.integer(deriv),
	    fixed.penalty = as.integer(rp$fixed.penalty),nt=as.integer(control$nthreads))

   rV <- matrix(oo$rV,ncol(x),ncol(x)) ## rV%*%t(rV)*scale gives covariance matrix 
   rV <- T %*% rV   
   ## derivatives of coefs w.r.t. sps etc...
   db.drho <- if (deriv) T %*% matrix(oo$b1,ncol(x),ntot) else NULL 
   Kmat <- matrix(0,nrow(x),ncol(x)) 
   Kmat[good,] <- oo$X                    ## rV%*%t(K)%*%(sqrt(wf)*X) = F; diag(F) is edf array 

   D2 <- matrix(oo$D2,ntot,ntot); ldet2 <- matrix(oo$ldet2,ntot,ntot)
   bSb2 <- matrix(oo$P2,ntot,ntot)
   ## compute the REML score...
   ls <- family$ls(y,weights,n,theta,scale)
   nt <- length(theta)
   lsth1 <- ls$lsth1[seq_len(nt)];
   lsth2 <- as.matrix(ls$lsth2)[seq_len(nt),seq_len(nt)] ## exclude any derivs w.r.t log scale here
   REML <- (dev+oo$P)/(2*scale) - ls$ls + (oo$ldet - rp$det)/2 - 
           as.numeric(scoreType=="REML") * Mp * log(2*pi*scale)/2
   REML1 <- REML2 <- NULL
   if (deriv) {
     det1 <- oo$ldet1
     if (nSp) {
       ind <- seq_len(nSp) + length(theta)
       det1[ind] <- det1[ind] - rp$det1
     }
     REML1 <- (oo$D1+oo$P1)/(2*scale) - c(lsth1,rep(0,length(sp))) + (det1)/2
     if (deriv>1) {
       ls2 <- D2*0;ls2[seq_len(nt),seq_len(nt)] <- lsth2 
       if (nSp) ldet2[ind,ind] <- ldet2[ind,ind] - rp$det2
       REML2 <- (D2+bSb2)/(2*scale) - ls2 + ldet2/2
     }
   } 

   if (!scale.known&&deriv) { ## need derivatives wrt log scale, too 
      Dp <- dev + oo$P
      dlr.dlphi <- -Dp/(2 *scale) - ls$lsth1[nt+1] - Mp/2
      d2lr.d2lphi <- Dp/(2*scale) - ls$lsth2[nt+1,nt+1] 
      d2lr.dspphi <- -(oo$D1+oo$P1)/(2*scale) 
      d2lr.dspphi[seq_len(nt)] <- d2lr.dspphi[seq_len(nt)] - ls$lsth2[nt+1,seq_len(nt)]
      REML1 <- c(REML1,dlr.dlphi)
      if (deriv==2) {
              REML2 <- rbind(REML2,as.numeric(d2lr.dspphi))
              REML2 <- cbind(REML2,c(as.numeric(d2lr.dspphi),d2lr.d2lphi))
      }
   }
   
   nth <- length(theta)
   if (deriv>0&&family$n.theta==0&&nth>0) { ## need to drop derivs for fixed theta
     REML1 <- REML1[-seq_len(nth)]
     if (deriv>1) REML2 <- REML2[-seq_len(nth),-seq_len(nth)]
     db.drho <- db.drho[,-seq_len(nth),drop=FALSE]
   }  

   names(coef) <- xnames
   names(residuals) <- ynames
   wtdmu <- sum(weights * y)/sum(weights)
   nulldev <- sum(dev.resids(y, rep(wtdmu,length(y)), weights))
   n.ok <- nobs - sum(weights == 0)
   nulldf <- n.ok
   ww <- wt <- rep.int(0, nobs)
   wt[good] <- wf 
   ww[good] <- w
   aic.model <- family$aic(y, mu, theta, weights, dev) # note: incomplete 2*edf needs to be added
 

   list(coefficients = coef,residuals=residuals,fitted.values = mu,
        family=family, linear.predictors = eta,deviance=dev,
        null.deviance=nulldev,iter=iter,
        weights=wt, ## note that these are Fisher type weights 
        prior.weights=weights,
        working.weights = ww, ## working weights
        df.null = nulldf, y = y, converged = conv,
        boundary = boundary,
        REML=REML,REML1=REML1,REML2=REML2,
        rV=rV,db.drho=db.drho,
        scale.est=scale,reml.scale=scale,
        aic=aic.model,
        rank=oo$rank.est,
        K=Kmat,control=control
        #,D1=oo$D1,D2=D2,
        #ldet=oo$ldet,ldet1=oo$ldet1,ldet2=ldet2,
        #bSb=oo$P,bSb1=oo$P1,bSb2=bSb2,
        #ls=ls$ls,ls1=ls$lsth1,ls2=ls$lsth2
       )
 
} ## gam.fit4



gam.fit5 <- function(x,y,lsp,Sl,weights=NULL,offset=NULL,deriv=2,family,
                     control=gam.control(),Mp=-1,start=NULL){
## NOTE: offset handling - needs to be passed to ll code
## fit models by general penalized likelihood method, 
## given doubly extended family in family. lsp is log smoothing parameters
## Stabilization strategy:
## 1. Sl.repara
## 2. Hessian diagonally pre-conditioned if +ve diagonal elements
##    (otherwise indefinite anyway)
## 3. Newton fit with perturbation of any indefinite hessian
## 4. At convergence test fundamental rank on balanced version of 
##    penalized Hessian. Drop unidentifiable parameters and 
##    continue iteration to adjust others.
## 5. All remaining computations in reduced space.
##    
## Idea is that rank detection takes care of structural co-linearity,
## while preconditioning and step 1 take care of extreme smoothing parameters
## related problems. 

  penalized <- if (length(Sl)>0) TRUE else FALSE

  nSp <- length(lsp)
  sp <- exp(lsp) 
  rank.tol <- .Machine$double.eps*100 ## tolerance to use for rank deficiency
  q <- ncol(x)
  n <- nobs <- length(y)
  
  if (penalized) {
    Eb <- attr(Sl,"E") ## balanced penalty sqrt
 
    ## the stability reparameterization + log|S|_+ and derivs... 
    rp <- ldetS(Sl,rho=lsp,fixed=rep(FALSE,length(lsp)),np=q,root=TRUE) 
    x <- Sl.repara(rp$rp,x) ## apply re-parameterization to x
    Eb <- Sl.repara(rp$rp,Eb) ## root balanced penalty 
    St <- crossprod(rp$E) ## total penalty matrix
    E <- rp$E ## root total penalty
    attr(E,"use.unscaled") <- TRUE ## signal initialization code that E not to be further scaled   
    if (!is.null(start)) start  <- Sl.repara(rp$rp,start) ## re-para start
    ## NOTE: it can be that other attributes need re-parameterization here
    ##       this should be done in 'family$initialize' - see mvn for an example. 

  } else { ## unpenalized so no derivatives required
    deriv <- 0 
    rp <- list(ldetS=0,rp=list())
    St <- matrix(0,q,q)
    E <- matrix(0,0,q) ## can be needed by initialization code
  }
  ## now call initialization code, but make sure that any 
  ## supplied 'start' vector is not overwritten...
  start0 <- start
  
  ## Assumption here is that the initialization code is fine with
  ##  re-parameterized x...

  eval(family$initialize) 
   
  if (!is.null(start0)) start <- start0 
  coef <- as.numeric(start)


  if (is.null(weights)) weights <- rep.int(1, nobs)
  if (is.null(offset)) offset <- rep.int(0, nobs)
 

  ## get log likelihood, grad and Hessian (w.r.t. coefs - not s.p.s) ...
  ll <- family$ll(y,x,coef,weights,family,deriv=1) 
  ll0 <- ll$l - t(coef)%*%St%*%coef/2
  rank.checked <- FALSE ## not yet checked the intrinsic rank of problem 
  rank <- q;drop <- NULL
  eigen.fix <- FALSE
  converged <- FALSE
  check.deriv <- FALSE; eps <- 1e-5 
  drop <- NULL;bdrop <- rep(FALSE,q) ## by default nothing dropped
  perturbed <- 0 ## counter for number of times perturbation tried on possible saddle
  for (iter in seq_len(2*control$maxit)) { ## main iteration
    ## get Newton step... 
    if (check.deriv) {
      fdg <- ll$lb*0; fdh <- ll$lbb*0
      for (k in seq_len(length(coef))) {
        coef1 <- coef;coef1[k] <- coef[k] + eps
        ll.fd <- family$ll(y,x,coef1,weights,family,deriv=1)
        fdg[k] <- (ll.fd$l-ll$l)/eps
        fdh[,k] <- (ll.fd$lb-ll$lb)/eps
      }
    }
    grad <- ll$lb - St%*%coef 
    Hp <- -ll$lbb+St
    D <- diag(Hp)
    indefinite <- FALSE
    if (sum(D <= 0)) { ## Hessian indefinite, for sure
      D <- rep(1,ncol(Hp))
      if (eigen.fix) {
        eh <- eigen(Hp,symmetric=TRUE);
        ev <- abs(eh$values)
        #thresh <- min(ev[ev>0])
        #ev[ev<thresh] <- thresh
        Hp <- eh$vectors%*%(ev*t(eh$vectors))
      } else {
        Ib <- diag(rank)*abs(min(D))
        Ip <- diag(rank)*abs(max(D)*.Machine$double.eps^.5)
        Hp <- Hp  + Ip + Ib
      }
      indefinite <- TRUE
    } else { ## Hessian could be +ve def in which case Choleski is cheap!
      D <- D^-.5 ## diagonal pre-conditioner
      Hp <- D*t(D*Hp) ## pre-condition Hp
      Ip <- diag(rank)*.Machine$double.eps^.5   
    }
    L <- suppressWarnings(chol(Hp,pivot=TRUE))
    mult <- 1
    while (attr(L,"rank") < rank) { ## rank deficient - add ridge penalty 
      if (eigen.fix) {
        eh <- eigen(Hp,symmetric=TRUE);ev <- eh$values
        thresh <- max(min(ev[ev>0]),max(ev)*1e-6)*mult
        mult <- mult*10
        ev[ev<thresh] <- thresh
        Hp <- eh$vectors%*%(ev*t(eh$vectors)) 
        L <- suppressWarnings(chol(Hp,pivot=TRUE))
      } else {
        L <- suppressWarnings(chol(Hp+Ip,pivot=TRUE))
        Ip <- Ip * 100 ## increase regularization penalty
      }
      indefinite <- TRUE
    }

    piv <- attr(L,"pivot")
    ipiv <- piv;ipiv[piv] <- seq_len(ncol(L))
    step <- D*(backsolve(L,forwardsolve(t(L),(D*grad)[piv]))[ipiv])

    c.norm <- sum(coef^2)
    if (c.norm>0) { ## limit step length to .1 of coef length
      s.norm <- sqrt(sum(step^2))
      c.norm <- sqrt(c.norm)
      if (s.norm > .1*c.norm) step <- step*0.1*c.norm/s.norm
    }
    ## try the Newton step...
    coef1 <- coef + step 
    ll <- family$ll(y,x,coef1,weights,family,deriv=1) 
    ll1 <- ll$l - (t(coef1)%*%St%*%coef1)/2
    khalf <- 0
    while (ll1 < ll0 && khalf < 50) { ## step halve until it succeeds...
      step <- step/2;coef1 <- coef + step
      ll <- family$ll(y,x,coef1,weights,family,deriv=0)
      ll1 <- ll$l - (t(coef1)%*%St%*%coef1)/2
      if (ll1>=ll0) {
        ll <- family$ll(y,x,coef1,weights,family,deriv=1)
      }
      khalf <- khalf + 1
    } ## end step halve

    if (ll1 >= ll0||iter==control$maxit) { ## step ok. Accept and test
      coef <- coef + step
      ## convergence test...
      ok <- (iter==control$maxit||(abs(ll1-ll0) < control$epsilon*abs(ll0) 
          && max(abs(grad)) < .Machine$double.eps^.5*abs(ll0))) 
      if (ok) { ## appears to have converged
        if (indefinite) { ## not a well defined maximum
          if (perturbed==5) stop("indefinite penalized likelihood in 'gam.fit5()' function")
          if (iter<4||rank.checked) {
            perturbed <- perturbed + 1
            coef <- coef*(1+(runif(length(coef))*.02-.01)*perturbed) + 
                    (runif(length(coef)) - 0.5 ) * mean(abs(coef))*1e-5*perturbed 
            ll <- family$ll(y,x,coef,weights,family,deriv=1) 
            ll0 <- ll$l - (t(coef)%*%St%*%coef)/2
          } else {        
            rank.checked <- TRUE
            if (penalized) {
              Sb <- crossprod(Eb) ## balanced penalty
              Hb <- -ll$lbb/norm(ll$lbb,"F")+Sb/norm(Sb,"F") ## balanced penalized hessian
            } else Hb <- -ll$lbb/norm(ll$lbb,"F")
            ## apply pre-conditioning, otherwise badly scaled problems can result in
            ## wrong coefs being dropped...
            D <- abs(diag(Hb))
            D[D<1e-50] <- 1;D <- D^-.5
            Hb <- t(D*Hb)*D
            qrh <- qr(Hb,LAPACK=TRUE)
            rank <- Rrank(qr.R(qrh))
            if (rank < q) { ## rank deficient. need to drop and continue to adjust other params
              drop <- sort(qrh$pivot[(rank+1):q]) ## set these params to zero 
              bdrop <- seq_len(q) %in% drop ## TRUE FALSE version
              ## now drop the parameters and recompute ll0...
              lpi <- attr(x,"lpi")
              coef <- coef[-drop]
              St <- St[-drop,-drop]
              x <- x[,-drop] ## dropping columns from model matrix
              if (!is.null(lpi)) { ## need to adjust column indexes as well
                k <- 0
                for (i in seq_len(length(lpi))) {
                  kk <- sum(lpi[[i]]%in%drop==FALSE) ## how many left undropped?
                  lpi[[i]] <- seq_len(kk) + k ## new index - note strong assumptions on structure here
                  k <- k + kk
                }
              } ## lpi adjustment done
              attr(x,"lpi") <- lpi
              ll <- family$ll(y,x,coef,weights,family,deriv=1) 
              ll0 <- ll$l - (t(coef)%*%St%*%coef)/2
            } 
          }

        } else { ## not indefinite really converged
          converged <- TRUE
          break
        }
      } else ll0 <- ll1 ## step ok but not converged yet
    } else { ## step failed.
      converged  <- FALSE
      if (is.null(drop)) bdrop <- rep(FALSE,q)
      warning(gettextf("step failed: max(|grad|) = %s", max(abs(grad))))
      break
    }
  } ## end of main fitting iteration

  ## at this stage the Hessian (of pen lik. w.r.t. coefs) should be +ve definite,
  ## so that the pivoted Choleski factor should exist...
  if (iter == 2*control$maxit&&converged==FALSE) 
    warning(gettextf("iteration limit reached: max(|grad|) = %g",max(abs(grad))))

  ldetHp <- 2*sum(log(diag(L))) - 2 * sum(log(D)) ## log |Hp|

  if (!is.null(drop)) { ## create full version of coef with zeros for unidentifiable 
    fcoef <- rep(0,length(bdrop));fcoef[!bdrop] <- coef
  } else fcoef <- coef

  d1l <- d2l <- d1bSb <- d2bSb <- d1b <- d2b <- d1ldetH <- d2ldetH <- d1b <- d2b <- NULL

  if (deriv>0) {  ## Implicit differentiation for derivs...

    m <- nSp
    d1b <- matrix(0,rank,m)
    Sib <- Sl.termMult(rp$Sl,fcoef,full=TRUE) ## list of penalties times coefs
    if (nSp) for (i in seq_len(m)) d1b[,i] <- 
       -D*(backsolve(L,forwardsolve(t(L),(D*Sib[[i]][!bdrop])[piv]))[ipiv])
  
    if (!is.null(drop)) { ## create full version of d1b with zeros for unidentifiable 
      fd1b <-  matrix(0,q,m)
      fd1b[!bdrop,] <- d1b
    } else fd1b <- d1b

    ## Now call the family again to get first derivative of Hessian w.r.t
    ## smoothing parameters, in list d1H...

    ll <- family$ll(y,x,coef,weights,family,deriv=3,d1b=d1b)
    d1l <- colSums(ll$lb*d1b)
    

    if (deriv>1) { ## Implicit differentiation for the second derivatives is now possible...

      d2b <- matrix(0,rank,m*(m+1)/2)
      k <- 0
      for (i in seq_len(m)) for (j in i:m) {
        k <- k + 1
        v <- -ll$d1H[[i]]%*%d1b[,j] + Sl.mult(rp$Sl,fd1b[,j],i)[!bdrop] + Sl.mult(rp$Sl,fd1b[,i],j)[!bdrop]
        d2b[,k] <- -D*(backsolve(L,forwardsolve(t(L),(D*v)[piv]))[ipiv])
        if (i==j) d2b[,k] <- d2b[,k] + d1b[,i]
      } 
  
      ## Now call family for last time to get trHid2H the tr(H^{-1} d^2 H / drho_i drho_j)...

      llr <- family$ll(y,x,coef,weights,family,deriv=4,d1b=d1b,d2b=d2b,
                       Hp=Hp,rank=rank,fh = L,D=D)

      ## Now compute Hessian of log lik w.r.t. log sps using chain rule
       
      d2la <- colSums(ll$lb*d2b)
      k <- 0
      d2l <- matrix(0,m,m)
      for (i in seq_len(m)) for (j in i:m) {
        k <- k + 1
        d2l[j,i] <- d2l[i,j] <- d2la[k] + t(d1b[,i])%*%ll$lbb%*%d1b[,j] 
      }
    } ## if (deriv > 1)
  } ## if (deriv > 0)

  ## Compute the derivatives of log|H+S|... 
  if (deriv > 0) {
    d1ldetH <- rep(0,m)
    d1Hp <- list()
    for (i in seq_len(m)) {
      A <- -ll$d1H[[i]] + Sl.mult(rp$Sl,diag(q),i)[!bdrop,!bdrop]
      d1Hp[[i]] <- D*(backsolve(L,forwardsolve(t(L),(D*A)[piv,]))[ipiv,])  
      d1ldetH[i] <- sum(diag(d1Hp[[i]]))
    }
  } ## if (deriv > 0)

  if (deriv > 1) {
    d2ldetH <- matrix(0,m,m)
    k <- 0
    for (i in seq_len(m)) for (j in i:m) {
      k <- k + 1
      d2ldetH[i,j] <- -sum(d1Hp[[i]]*t(d1Hp[[j]])) - llr$trHid2H[k] 
      if (i==j) { ## need to add term relating to smoothing penalty
        A <- t(Sl.mult(rp$Sl,diag(q),i,full=FALSE))
        bind <- rowSums(A)!=0
        ind <- which(bind)
        bind <- bind[!bdrop]
        A <- A[!bdrop,!bdrop[ind]]
        A <- D*(backsolve(L,forwardsolve(t(L),(D*A)[piv,]))[ipiv,])
        d2ldetH[i,j] <- d2ldetH[i,j] + sum(diag(A[bind,]))
      } else d2ldetH[j,i] <- d2ldetH[i,j]
    }
  } ## if (deriv > 1)

  ## Compute derivs of b'Sb...

  if (deriv>0) {
    Sb <- St%*%coef
    Skb <- Sl.termMult(rp$Sl,fcoef,full=TRUE)
    d1bSb <- rep(0,m)
    for (i in seq_len(m)) { 
      Skb[[i]] <- Skb[[i]][!bdrop]
      d1bSb[i] <- 2*sum(d1b[,i]*Sb) + sum(coef*Skb[[i]])
    }
  }
 
  if (deriv>1) {
    d2bSb <- matrix(0,m,m)
    k <- 0
    for (i in seq_len(m)) {
      Sd1b <- St%*%d1b[,i] 
      for (j in i:m) {
        k <- k + 1
        d2bSb[j,i] <- d2bSb[i,j] <- 2*sum(d2b[,k]*Sb + 
         d1b[,i]*Skb[[j]] + d1b[,j]*Skb[[i]] + d1b[,j]*Sd1b)
      }
      d2bSb[i,i] <-  d2bSb[i,i] + sum(coef*Skb[[i]]) 
    }
  }

  ## get grad and Hessian of REML score...
  REML <- -as.numeric(ll$l - t(coef)%*%St%*%coef/2 + rp$ldetS/2 - ldetHp/2 + Mp*log(2*pi)/2)
 
  REML1 <- if (deriv>0) -as.numeric(d1l - d1bSb/2 + rp$ldet1/2 - d1ldetH/2) else NULL 
  if (control$trace) {
    cat("\niter =",iter,"  ll =",ll$l,"  REML =",REML,"  bSb =",t(coef)%*%St%*%coef/2,"\n")
    cat("log|S| =",rp$ldetS,"  log|H+S| =",ldetHp,"  n.drop =",length(drop),"\n")
    if (!is.null(REML1)) cat("REML1 =",REML1,"\n")
  }
  REML2 <- if (deriv>1) -(d2l - d2bSb/2 + rp$ldet2/2 - d2ldetH/2) else NULL 
  bSb <- t(coef)%*%St%*%coef
  lpi <- attr(x,"lpi")
  if (is.null(lpi)) { 
    linear.predictors <- as.numeric(x%*%coef)
    fitted.values <- family$linkinv(linear.predictors) 
  } else {
    fitted.values <- linear.predictors <- matrix(0,nrow(x),length(lpi))
    for (j in 1:length(lpi)) {
      linear.predictors[,j] <- as.numeric(x[,lpi[[j]],drop=FALSE] %*% coef[lpi[[j]]])
      fitted.values[,j] <- family$linfo[[j]]$linkinv( linear.predictors[,j]) 
    }
  }
  coef <- Sl.repara(rp$rp,fcoef,inverse=TRUE) ## undo re-parameterization of coef 
 
  if (!is.null(drop)) { ## create full version of d1b with zeros for unidentifiable 
    db.drho <- matrix(0,length(bdrop),ncol(d1b));db.drho[!bdrop,] <- d1b
  } else db.drho <- d1b
  ## and undo re-para...
  if (!is.null(d1b)) db.drho <- t(Sl.repara(rp$p,t(db.drho),inverse=TRUE,both.sides=FALSE)) 

  ret <- list(coefficients=coef,family=family,y=y,prior.weights=weights,
       fitted.values=fitted.values, linear.predictors=linear.predictors,
       scale.est=1, ### NOTE: needed by newton, but what is sensible here? 
       REML= REML,REML1= REML1,REML2=REML2,
       rank=rank,aic = -2*ll$l, ## 2*edf needs to be added
       l= ll$l,l1 =d1l,l2 =d2l,
       lbb = ll$lbb, ## Hessian of log likelihood
       L=L, ## chol factor of pre-conditioned penalized hessian
       bdrop=bdrop, ## logical index of dropped parameters
       D=D, ## diagonal preconditioning matrix
       St=St, ## total penalty matrix
       rp = rp$rp,
       db.drho = db.drho, ## derivative of penalty coefs w.r.t. log sps.
       #bSb = bSb, bSb1 =  d1bSb,bSb2 =  d2bSb,
       #S=rp$ldetS,S1=rp$ldet1,S2=rp$ldet2,
       #Hp=ldetHp,Hp1=d1ldetH,Hp2=d2ldetH,
       #b2 = d2b)
       H = ll$lbb,dH = ll$d1H)#,d2H=llr$d2H)
    ret
} ## end of gam.fit5

gam.fit5.post.proc <- function(object,Sl,L) {
## object is object returned by gam.fit5, Sl is penalty object, L maps working sp
## vector to full sp vector 
## Computes:
## R - unpivoted Choleski of estimated expected hessian of ll 
## Vb - the Bayesian cov matrix,
## Ve - "frequentist" alternative
## F - the EDF matrix
## edf = diag(F) and edf2 = diag(2F-FF)
## Main issue is that lbb and lbb + St must be +ve definite for
## F to make sense.
## NOTE: what comes in is in stabilizing parameterization from 
##       gam.fit5, and may have had parameters dropped. 
##       possibly initial reparam needs to be undone here as well
##       before formation of F....
  lbb <- -object$lbb ## Hessian of log likelihood in fit parameterization
  p <- ncol(lbb)
  ipiv <- piv <- attr(object$L,"pivot")
  ipiv[piv] <- seq_len(p)
  ##  Vb0 <- crossprod(forwardsolve(t(object$L),diag(object$D,nrow=p)[piv,])[ipiv,])

  ## need to pre-condition lbb before testing rank...
  lbb <- object$D*t(object$D*lbb)
 
  R <- suppressWarnings(chol(lbb,pivot=TRUE)) 
  
  if (attr(R,"rank") < ncol(R)) { 
    ## The hessian of the -ve log likelihood is not +ve definite
    ## Find the "nearest" +ve semi-definite version and use that
    retry <- TRUE;tol <- 0
    eh <- eigen(lbb,symmetric=TRUE)
    mev <- max(eh$values);dtol <- 1e-7
    while (retry) {
      eh$values[eh$values<tol*mev] <- tol*mev
      R <- sqrt(eh$values)*t(eh$vectors)
      lbb <- crossprod(R)
      Hp <- lbb + object$D*t(object$D*object$St) ## pre-conditioned Hp
      ## Now try to invert it by Choleski with diagonal pre-cond,
      ## to get Vb
      object$L <- suppressWarnings(chol(Hp,pivot=TRUE))
      if (attr(object$L,"rank")==ncol(Hp)) {
        R <- t(t(R)/object$D) ## so R'R = lbb (original)
        retry <- FALSE
      } else { ##  failure: make more +ve def
        tol <- tol + dtol;dtol <- dtol*10
      }
    } ## retry  
  } else { ## hessian +ve def, so can simply use what comes from fit directly
    ipiv <- piv <- attr(R,"pivot")
    ipiv[piv] <- seq_len(p)
    R <- t(t(R[,ipiv])/object$D) ## so now t(R)%*%R = lbb (original)
  } 
  ## DL'LD = penalized Hessian, which needs to be inverted
  ## to DiLiLi'Di = Vb, the Bayesian cov matrix...
  ipiv <- piv <- attr(object$L,"pivot")
  ipiv[piv] <- seq_len(p)
  Vb <- crossprod(forwardsolve(t(object$L),diag(object$D,nrow=p)[piv,,drop=FALSE])[ipiv,,drop=FALSE])

  ## Insert any zeroes required as a result of dropping 
  ## unidentifiable parameters...
  if (sum(object$bdrop)) { ## some coefficients were dropped...
    q <- length(object$bdrop)
    ibd <- !object$bdrop
    Vtemp <- Vb; Vb <- matrix(0,q,q)
    Vb[ibd,ibd] <- Vtemp
    Rtemp <- R; R <- matrix(0,q,q)
    R[ibd,ibd] <- Rtemp
    lbbt <- lbb;lbb <- matrix(0,q,q)
    lbb[ibd,ibd] <- lbbt
  }  

  ## compute the smoothing parameter uncertainty correction...
  if (!is.null(object$outer.info$hess)) {
    ev <- eigen(object$outer.info$hess,symmetric=TRUE)
    ind <- ev$values <= 0
    ev$values[ind] <- 0;ev$values[!ind] <- 1/sqrt(ev$values[!ind])
    if (!is.null(L)) object$db.drho <- object$db.drho%*%L ## transform to derivs w.r.t. working
    Vc <- crossprod((ev$values*t(ev$vectors))%*%t(object$db.drho))
    Vc <- Vb + Vc  ## Bayesian cov matrix with sp uncertainty
    ## reverse the various re-parameterizations...
  } else Vc <- Vb
  Vc <- Sl.repara(object$rp,Vc,inverse=TRUE) 
  Vc <-  Sl.initial.repara(Sl,Vc,inverse=TRUE)
  Vb <- Sl.repara(object$rp,Vb,inverse=TRUE)
  Vb <-  Sl.initial.repara(Sl,Vb,inverse=TRUE)
  R <- Sl.repara(object$rp,R,inverse=TRUE,both.sides=FALSE)
  R <-  Sl.initial.repara(Sl,R,inverse=TRUE,both.sides=FALSE,cov=FALSE)
  F <- Vb%*%crossprod(R)
  Ve <- F%*%Vb ## 'frequentist' cov matrix
  edf <- diag(F)
  ## note that edf1 is a heuristic upper bound on EDF - it's the 
  ## df of the best unpenalized approx to the 1st order bias corrected
  ## model. This is larger than edf2 should be, because of bias correction variability,
  ##  but is bounded in a way that is not *guaranteed* for edf2. Note that 
  ## justification only applies to sum(edf1/2) not elementwise   
  edf1 <- 2*edf - rowSums(t(F)*F)
  edf2 <- diag(Vc%*%crossprod(R)) 
  if (sum(edf2)>sum(edf1)) edf2 <- edf1 
  ## note hat not possible here...
  list(Vc=Vc,Vb=Vb,Ve=Ve,edf=edf,edf1=edf1,edf2=edf2,F=F,R=R)
} ## gam.fit5.post.proc


deriv.check5 <- function(x, y, sp, 
            weights = rep(1, length(y)), start = NULL,
            offset = rep(0, length(y)),Mp,family = gaussian(), 
            control = gam.control(),deriv=2,eps=1e-7,spe=1e-3,
            Sl,...)
## FD checking of derivatives for gam.fit5: a debugging routine
{  if (!deriv%in%c(1,2)) stop(gettextf("'%s' argument should be 1 or 2", "deriv"))
   if (control$epsilon>1e-9) control$epsilon <- 1e-9 
   ## first obtain the fit corresponding to sp...
   b <- gam.fit5(x=x,y=y,lsp=sp,Sl=Sl,weights=weights,offset=offset,deriv=2,
        family=family,control=control,Mp=Mp,start=start)
   ## now get the derivatives of the likelihood w.r.t. coefs...
   ll <- family$ll(y=y,X=x,coef=b$coefficients,wt=weights,family=family,
                   deriv=1,d1b=0,d2b=0,Hp=NULL,rank=0,fh=NULL,D=NULL)
   ## and finite difference versions of these...
   p <- length(b$coefficients)
   fdg <- rep(0,p)
   fdh <- matrix(0,p,p)
   for (i in seq_len(p)) {
     coef1 <- b$coefficients;coef1[i] <- coef1[i] + eps
     ll1 <- family$ll(y=y,X=x,coef=coef1,wt=weights,family=family,
                      deriv=1,d1b=0,d2b=0,Hp=NULL,rank=0,fh=NULL,D=NULL)
     fdg[i] <- (ll1$l - ll$l)/eps
     fdh[,i] <- (ll1$lb - ll$lb)/eps
   }
   ## display them... 
   oask <- devAskNewPage(TRUE)
   on.exit(devAskNewPage(oask))
   plot(ll$lb,fdg,xlab="computed",ylab="FD",main="grad of log lik");abline(0,1)
   cat(gettextf("log lik grad cor. = %s", cor(ll$lb,fdg), domain = "R-mgcv"), "\n", sep = "")
   plot(ll$lbb,fdh,xlab="computed",ylab="FD",main="hess of log lik");abline(0,1)
   cat(gettextf("log lik hess cor. = %s", cor(as.numeric(ll$lbb),as.numeric(fdh)), domain = "R-mgcv"), "\n", sep = "")
   ## now we need to investigate the derivatives w.r.t. the log smoothing parameters.    
   M <- length(sp) ## number of smoothing parameters
   fd.br <- matrix(0,p,M)
   REML1 <- rep(0,M)
   fd.dH <- list()
   for (i in seq_len(M)) { ## the smoothing parameter loop
     sp0 <- sp1 <- sp;sp1[i] <- sp[i] + spe/2;sp0[i] <- sp[i] - spe/2
     b0 <- gam.fit5(x=x,y=y,lsp=sp0,Sl=Sl,weights=weights,offset=offset,deriv=0,
          family=family,control=control,Mp=Mp,start=start)
     b1 <- gam.fit5(x=x,y=y,lsp=sp1,Sl=Sl,weights=weights,offset=offset,deriv=0,
          family=family,control=control,Mp=Mp,start=start)
     fd.br[,i] <- (b1$coefficients - b0$coefficients)/spe
     REML1[i] <- (b1$REML-b0$REML)/spe
     fd.dH[[i]] <- (b1$lbb - b0$lbb)/spe
   }
   ## plot db.drho against fd versions...
   for (i in seq_len(M)) {
     plot(b$db.drho[,i],fd.br[,i],xlab="computed",ylab="FD",main="db/drho");abline(0,1)
     cat(gettextf("cor db/drho[%d] = %s", i, cor(b$db.drho[,i],fd.br[,i]), domain = "R-mgcv"), "\n", sep = "")
   }
   ## plot first deriv Hessian against FD version
   for (i in seq_len(M)) {
     plot(b$dH[[i]],fd.dH[[i]],xlab="computed",ylab="FD",main="dH/drho");abline(0,1)
     cat(gettextf("cor dH/drho[%d] = %s", i, cor(as.numeric(b$dH[[i]]),as.numeric(fd.dH[[i]])), domain = "R-mgcv"),"\n", sep = "")
   }
   list(fd=list(lb=fdg,lbb=fdh,REML1=REML1,db.drho=fd.br,dH=fd.dH),
        lb=ll$lb,lbb=ll$lbb,REML1=b$REML1,db.drho=b$db.drho,dH=b$dH)
} ## deriv.check5
