## (c) Simon N. Wood 2011-2019
## Many of the following are simple wrappers for C functions

"%.%" <- function(a,b) {
  tensor.prod.model.matrix(list(as.matrix(a),as.matrix(b)))
}

blas.thread.test <- function(n=1000,nt=4) {
  inter <- interactive()
  if (inter) prg <- txtProgressBar(min = 0, max = n, initial = 0,
              char = "=",width = NA, title="Progress", style = 3)
  for (i in seq_len(n)) {
    m <- sample(213:654,1)
    p <- sample(3:153,1)
    X <- matrix(runif(m*p),m,p)
    er <- pqr(X,nt=nt) 
    rr <- range(pqr.qy(er,pqr.R(er))-X[,er$pivot])
    if (rr[1] < -1e-4||rr[2] > 1e-4) {
      break;
    }
    if (inter) setTxtProgressBar(prg, i)
  }
  if (inter) close(prg)
  if (rr[1] < -1e-4||rr[2] > 1e-4) {
    cat(gettextf("BLAS thread safety problem at iteration %d",i, domain = "R-mgcv"),"\n", sep = "")
  } else cat(gettextf("No problem encountered in %d iterations",i, domain = "R-mgcv"), "\n", sep = "")
} ## blas.thread.test



rmvn <- function(n,mu,V) {
## generate multivariate normal deviates. e.g.
## V <- matrix(c(2,1,1,2),2,2); mu <- c(1,1);n <- 1000;z <- rmvn(n,mu,V);crossprod(sweep(z,2,colMeans(z)))/n
  p <- ncol(V)
  R <- mroot(V,rank=ncol(V)) ## RR' = V
  if (is.matrix(mu)) {
    if (ncol(mu)!=p||nrow(mu)!=n) stop("'mu' dimensions are wrong")
    z <- matrix(rnorm(p*n),n,p)%*%t(R) + mu
  } else { 
    if (length(mu)!=p) stop("'mu' dimensions are wrong")
    z <- t(R%*% matrix(rnorm(p*n),p,n) + mu)
    if (n==1) z <- as.numeric(z)
  }
  z
} ## rmvn

sdiag <- function(A,k=0) {
## extract sub or super diagonal of matrix (k=0 is leading)  
 p <- ncol(A)
 n <- nrow(A)
 if (k>p-1||-k > n-1) return()
 if (k >= 0) {
   i <- seq_len(n)
   j <- (k+1):p
 } else {
   i <- (-k+1):n
   j <- seq_len(p)
 }
 if (length(i)>length(j)) i <- i[seq_len(length(j))] else j <- j[1:length(i)]
 ii <- i + (j-1) * n 
 A[ii]
} ## sdiag

"sdiag<-" <- function(A,k=0,value) {
 p <- ncol(A)
 n <- nrow(A)
 if (k>p-1||-k > n-1) return()
 if (k >= 0) {
   i <- seq_len(n)
   j <- (k+1):p
 } else {
   i <- (-k+1):n
   j <- seq_len(p)
 }
 if (length(i)>length(j)) i <- i[seq_len(length(j))] else j <- j[seq_len(length(i))]
 ii <- i + (j-1) * n 
 A[ii] <- value
 A
} ## "sdiag<-"

bandchol <- function(B) {
## obtain R such that R'R = A. Where A is banded matrix contained in R.
  n <- ncol(B)
  k <- 0
  if (n==nrow(B)) { ## square matrix. Extract the diagonals
    A <- B*0
    for (i in seq_len(n)) {
      b <- sdiag(B,i-1)
      if (sum(b!=0)!=0) {
        k <- i ## largest index of a non-zero band
        A[i,seq_len(length(b))] <- b
      }
    } 
    B <- A[seq_len(k),]
  }
  oo <- .C(C_band_chol,B=as.double(B),n=as.integer(n),k=as.integer(nrow(B)),info=as.integer(0))
  if (oo$info<0) stop("something wrong with inputs to LAPACK routine")
  if (oo$info>0) stop("matrix is not positive definite")
  B <- matrix(oo$B,nrow(B),n)
  if (k>0) { ## was square on entry, so also on exit...
    A <- A * 0
    for (i in seq_len(k)) sdiag(A,i-1) <- B[i,seq_len(n-i+1)]
    B <- A
  }
  B
} ## bandchol

trichol <- function(ld,sd) {
## obtain chol factor R of symm tridiag matrix, A, with leading diag
## ld and sub/super diags sd. R'R = A. On exit ld is diag of R and
## sd its super diagonal.
  n <- length(ld)
  if (n<2) stop("don't be silly")
  if (n!=length(sd)+1) stop("sd should have exactly one less entry than ld")
  oo <- .C(C_tri_chol,ld=as.double(ld),sd=as.double(sd),n=as.integer(n),info=as.integer(0))
  if (oo$info<0) stop("something wrong with inputs to LAPACK routine")
  if (oo$info>0) stop("matrix is not positive definite")
  ld <- sqrt(oo$ld)
  sd <- oo$sd*ld[seq_len(n-1)]
  list(ld=ld,sd=sd)
}

mgcv.omp <- function() {
## does open MP appear to be available?
  oo <- .C(C_mgcv_omp,a=as.integer(-1))
  if (oo$a==1) TRUE else FALSE
}

mvn.ll <- function(y,X,beta,dbeta=NULL) {
## to facilitate testing of MVN routine mvn_ll.
## X is a sequence of m model matrices bound columnwise, with m dim attribute lpi
##   indicating where the next starts in all cases.
## beta is parameter vector - last m*(m+1)/2 elements are chol factor of precision params.
## y is m by n data matrix.
  lpi <- attr(X,"lpi")-1;m <- length(lpi)
  nb <- length(beta)
  if (is.null(dbeta)) {
    nsp = 0;dbeta <- dH <- 0
  } else {
    nsp = ncol(dbeta)
    dH = rep(0,nsp*nb*nb)
  }
  oo <- .C(C_mvn_ll,y=as.double(y),X=as.double(X),XX=as.double(crossprod(X)),beta=as.double(beta),n=as.integer(nrow(X)),
                  lpi=as.integer(lpi),m=as.integer(m),ll=as.double(0),lb=as.double(beta*0),
                  lbb=as.double(rep(0,nb*nb)), dbeta = as.double(dbeta), dH = as.double(dH), 
                  deriv = as.integer(nsp>0),nsp = as.integer(nsp),nt=as.integer(1))
  if (nsp==0) dH <- NULL else {
    dH <- list();ind <- seq_len(nb*nb)
    for (i in seq_len(nsp)) { 
      dH[[i]] <- matrix(oo$dH[ind],nb,nb)
      ind <- ind + nb*nb
    }
  }
  list(l=oo$ll,lb=oo$lb,lbb=matrix(oo$lbb,nb,nb),dH=dH)
} ## mvn.ll

## discretized covariate routines...

XWXd <- function(X,w,k,ks,ts,dt,v,qc,nthreads=1,drop=NULL,ar.stop=-1,ar.row=-1,ar.w=-1,lt=NULL,rt=NULL) {
## Form X'WX given weights in w and X in compressed form in list X.
## each element of X is a (marginal) model submatrix. Full version 
## is given by X[[i]][k[,i],] (see below for summation convention).
## list X relates to length(ts) separate
## terms. ith term starts at matrix ts[i] and has dt[i] marginal matrices.
## For summation convention, k[,ks[j,1]:ks[j,2]] gives index columns
## for matrix j, thereby allowing summation over matrix covariates....
## i.e. for q in ks[j,1]:ks[j,2] sum up X[[j]][k[,q],] 
## Terms with several marginals are tensor products and may have 
## constraints (if qc[i]>1), stored as a householder vector in v[[i]]. 
## check ts and k index start (assumed 1 here)
## if drop is non-NULL it contains index of rows/cols to drop from result
## * lt is array of terms to include in left matrix (assumed in ascending coef index order)
## * rt is array of terms to include in right matrix (assumed in ascending coef index order)
## * if both NULL all are terms are included, if only one is NULL then used for left and right. 
  m <- unlist(lapply(X,nrow));p <- unlist(lapply(X,ncol))
  nx <- length(X);nt <- length(ts)
  n <- length(w);pt <- 0;
  for (i in seq_len(nt)) pt <- pt + prod(p[ts[i]:(ts[i]+dt[i]-1)]) - as.numeric(qc[i]>0)
  ## block oriented code...
  if (is.null(lt)&&is.null(lt)) {
    #t0 <- system.time(
    oo <- .C(C_XWXd0,XWX =as.double(rep(0,(pt+nt)^2)),X= as.double(unlist(X)),w=as.double(w),
           k=as.integer(k-1),ks=as.integer(ks-1),m=as.integer(m),p=as.integer(p), n=as.integer(n), 
           ns=as.integer(nx), ts=as.integer(ts-1), as.integer(dt), nt=as.integer(nt),
           v = as.double(unlist(v)),qc=as.integer(qc),nthreads=as.integer(nthreads),
           ar.stop=as.integer(ar.stop-1),ar.row=as.integer(ar.row-1),ar.weights=as.double(ar.w))#)
    XWX <- if (is.null(drop)) matrix(oo$XWX[1:pt^2],pt,pt) else matrix(oo$XWX[1:pt^2],pt,pt)[-drop,-drop]
  } else {
    lpip <- attr(X,"lpip") ## list of coefs for each term
    rpi <- unlist(lpip[rt])
    lpi <- unlist(lpip[lt])
    if (is.null(lt)) {
      lpi <- rpi
      nrs <- lt <- 0
      ncs <- length(rt)
    } else {
      nrs <- length(lt)
      if (is.null(rt)) {
        rt <- ncs <- 0
	rpi <- lpi
      } else ncs <- length(rt)
    }  
    #t0 <- system.time(
    oo <- .C(C_XWXd1,XWX =as.double(rep(0,(pt+nt)^2)),X= as.double(unlist(X)),w=as.double(w),
           k=as.integer(k-1),ks=as.integer(ks-1),m=as.integer(m),p=as.integer(p), n=as.integer(n), 
           ns=as.integer(nx), ts=as.integer(ts-1), as.integer(dt), nt=as.integer(nt),
           v = as.double(unlist(v)),qc=as.integer(qc),nthreads=as.integer(nthreads),
           ar.stop=as.integer(ar.stop-1),ar.row=as.integer(ar.row-1),ar.weights=as.double(ar.w),rs=as.integer(lt-1),
	   cs=as.integer(rt-1),nrs=as.integer(nrs),ncs=as.integer(ncs))#)
    XWX <- matrix(oo$XWX[seq_len(length(lpi)*length(rpi))],length(lpi),length(rpi))
    if (!is.null(drop)) {
      ldrop <- which(lpi %in% drop)
      rdrop <- which(lpi %in% drop)
      if (length(ldrop)>0||length(rdrop)>0) XWX <-
        if (length(ldrop==0)) XWX[,-rdrop] else if (length(rdrop)==0) XWX[-ldrop,] else XWX[-ldrop,-rdrop]
    }
  }
## old strictly level 2 code for comparison...	   
#  t1 <- system.time(ooo <- .C(C_XWXd,XWX =as.double(rep(0,pt^2)),X= as.double(unlist(X)),w=as.double(w),
#           k=as.integer(k-1),ks=as.integer(ks-1),m=as.integer(m),p=as.integer(p), n=as.integer(n), 
#           ns=as.integer(nx), ts=as.integer(ts-1), as.integer(dt), nt=as.integer(nt),
#           v = as.double(unlist(v)),qc=as.integer(qc),nthreads=as.integer(nthreads),
#           ar.stop=as.integer(ar.stop-1),ar.row=as.integer(ar.row-1),ar.weights=as.double(ar.w)))
  
#  XWX <- matrix(oo$XWX[1:pt^2],pt,pt)
#  XWX0 <- matrix(ooo$XWX[1:pt^2],pt,pt)
#  plot(XWX0,XWX,pch=".",main=range(XWX-XWX0));abline(0,1,col=2)
  #if (is.null(drop)) matrix(oo$XWX[1:pt^2],pt,pt) else matrix(oo$XWX[1:pt^2],pt,pt)[-drop,-drop]
  XWX
} ## XWXd

XWyd <- function(X,w,y,k,ks,ts,dt,v,qc,drop=NULL,ar.stop=-1,ar.row=-1,ar.w=-1,lt=NULL) {
## X'Wy...
## if lt if not NULL then it lists the discrete terms to include (from X)
## returned vector/matrix only includes rows for selected terms
  m <- unlist(lapply(X,nrow));p <- unlist(lapply(X,ncol))
  nx <- length(X);nt <- length(ts)
  n <- length(w);##pt <- 0;
  ##for (i in 1:nt) pt <- pt + prod(p[ts[i]:(ts[i]+dt[i]-1)]) - as.numeric(qc[i]>0)
  if (is.null(lt)) {
    pt <- 0
    for (i in seq_len(nt)) pt <- pt + prod(p[ts[i]:(ts[i]+dt[i]-1)]) - as.numeric(qc[i]>0)
    lt <- seq_len(nt)
  } else {
    lpip <- attr(X,"lpip") ## list of coefs for each term 
    lpi <- unlist(lpip[lt]) ## coefs corresponding to terms selected by lt
    if (!is.null(drop)) drop <- which(lpi %in% drop) ## rebase drop 
    pt <- length(lpi)
  }
  cy <- if (is.matrix(y)) ncol(y) else 1
  oo <- .C(C_XWyd,XWy=rep(0,pt*cy),y=as.double(y),X=as.double(unlist(X)),w=as.double(w),k=as.integer(k-1), 
           ks=as.integer(ks-1),
           m=as.integer(m),p=as.integer(p),n=as.integer(n),cy=as.integer(cy), nx=as.integer(nx), ts=as.integer(ts-1), 
           dt=as.integer(dt),nt=as.integer(nt),v=as.double(unlist(v)),qc=as.integer(qc),
           ar.stop=as.integer(ar.stop-1),ar.row=as.integer(ar.row-1),ar.weights=as.double(ar.w),
	   cs=as.integer(lt-1),ncs=as.integer(length(lt)))
  if (cy>1) XWy <- if (is.null(drop)) matrix(oo$XWy,pt,cy) else matrix(oo$XWy,pt,cy)[-drop,] else
  XWy <- if (is.null(drop)) oo$XWy else oo$XWy[-drop] 
  XWy
} ## XWyd 

Xbd <- function(X,beta,k,ks,ts,dt,v,qc,drop=NULL,lt=NULL) {
## note that drop may contain the index of columns of X to drop before multiplying by beta.
## equivalently we can insert zero elements into beta in the appropriate places.
## if lt if not NULL then it lists the discrete terms to include (from X)
  n <- if (is.matrix(k)) nrow(k) else length(k) ## number of data
  m <- unlist(lapply(X,nrow)) ## number of rows in each discrete model matrix
  p <- unlist(lapply(X,ncol)) ## number of cols in each discrete model matrix
  nx <- length(X) ## number of model matrices
  nt <- length(ts) ## number of terms
  if (!is.null(drop)) { 
    b <- if (is.matrix(beta)) matrix(0,nrow(beta)+length(drop),ncol(beta)) else rep(0,length(beta)+length(drop))
    if (is.matrix(beta)) b[-drop,] <- beta else b[-drop] <- beta
    beta <- b
  }
  if (is.null(lt)) {
    lt <- seq_len(nt)
  }
  lpip <- attr(X,"lpip")
  if (!is.null(lpip)) { ## then X list may not be in coef order...
    lpip <- unlist(lpip[lt])
    beta <- if (is.matrix(beta)) beta[lpip,] else beta[lpip] ## select params required in correct order
  }
  bc <- if (is.matrix(beta)) ncol(beta) else 1 ## number of columns in beta
  oo <- .C(C_Xbd,f=as.double(rep(0,n*bc)),beta=as.double(beta),X=as.double(unlist(X)),k=as.integer(k-1),
           ks = as.integer(ks-1), 
           m=as.integer(m),p=as.integer(p), n=as.integer(n), nx=as.integer(nx), ts=as.integer(ts-1), 
           as.integer(dt), as.integer(nt),as.double(unlist(v)),as.integer(qc),as.integer(bc),as.integer(lt-1),as.integer(length(lt)))
  if (is.matrix(beta)) matrix(oo$f,n,bc) else oo$f
} ## Xbd

diagXVXd <- function(X,V,k,ks,ts,dt,v,qc,drop=NULL,nthreads=1,lt=NULL,rt=NULL) {
## discrete computation of diag(XVX')
## BUGS: 1. X list may not be in coefficient order - in which case V has to be re-ordered according to lpip
##          attribute of X (if non-null)
##       2. It doesn't seem to work if terms are dropped, not from end, although Xbd seems to work in these cases.

  n <- if (is.matrix(k)) nrow(k) else length(k)
  m <- unlist(lapply(X,nrow));p <- unlist(lapply(X,ncol))
  nx <- length(X);nt <- length(ts)
  if (!is.null(drop)) { 
    pv <- ncol(V)+length(drop)
    V0 <- matrix(0,pv,pv)
    V0[-drop,-drop] <- V
    V <- V0;rm(V0)
  } else pv <- ncol(V)
  if (is.null(lt)) lt <- seq_len(nt)
  if (is.null(rt)) rt <- seq_len(nt)
  lpip <- attr(X,"lpip")
  if (!is.null(lpip)) { ## then X list may not be in coef order...
    lpi <- unlist(lpip[lt])
    rpi <- unlist(lpip[rt])
    V <- V[lpi,rpi] ## select part of V required required in correct order
  }
  oo <- .C(C_diagXVXt,diag=as.double(rep(0,n)),V=as.double(V),X=as.double(unlist(X)),k=as.integer(k-1), 
           ks=as.integer(ks-1),m=as.integer(m),p=as.integer(p), n=as.integer(n), nx=as.integer(nx),
	   ts=as.integer(ts-1), as.integer(dt), as.integer(nt),as.double(unlist(v)),as.integer(qc),as.integer(nrow(V)),as.integer(ncol(V)),
	   as.integer(nthreads),as.integer(lt-1),as.integer(length(lt)),as.integer(rt-1),as.integer(length(rt)))
  oo$diag
} ## diagXVXd

dchol <- function(dA,R) {
## if dA contains matrix dA/dx where R is chol factor s.t. R'R = A
## then this routine returns dR/dx...
  p <- ncol(R)
  oo <- .C(C_dchol,dA=as.double(dA),R=as.double(R),dR=as.double(R*0),p=as.integer(ncol(R)))
  return(matrix(oo$dR,p,p))
} ## dchol

choldrop <- function(R,k) {
## routine to update Cholesky factor R of A on dropping row/col k of A.
## R can be upper triangular, in which case (R'R=A) or lower triangular in
## which case RR'=A...
  n <- as.integer(ncol(R))
  k1 <- as.integer(k-1)
  ut <- as.integer(as.numeric(R[1,2]!=0))
  if (k<1||k>n) return(R)
  Rup <- matrix(0,n-1,n-1)
  #oo <- .C(C_chol_down,R=as.double(R),Rup=as.double(Rup),n=as.integer(n),k=as.integer(k-1),ut=as.integer(ut))
  .Call(C_mgcv_chol_down,R,Rup,n,k1,ut)
  #matrix(oo$Rup,n-1,n-1)
  Rup
} ## choldrop

cholup <- function(R,u,up=TRUE) {
## routine to update Cholesky factor R to the factor of R'R + uu' (up == TRUE)
## or R'R - uu' (up=FALSE). 
  n <- as.integer(ncol(R))
  up <- as.integer(up)
  eps <- as.double(.Machine$double.eps)
  R1 <- R * 1.0
  .Call(C_mgcv_chol_up,R1,u,n,up,eps)
  if (up==0) if ((n>1 && R1[2,1] < -1)||(n==1&&u[1]>R[1])) stop("update not positive definite")
  R1
} ## cholup


vcorr <- function(dR,Vr,trans=TRUE) {
## Suppose b = sum_k op(dR[[k]])%*%z*r_k, z ~ N(0,Ip), r ~ N(0,Vr). vcorr returns cov(b).
## dR is a list of p by p matrices. 'op' is 't' if trans=TRUE and I() otherwise.
  p <- ncol(dR[[1]])
  M <- if (trans) ncol(Vr) else -ncol(Vr) ## sign signals transpose or not to C code
  if (abs(M)!=length(dR)) stop("internal error in vcorr, please report to simon.wood@r-project.org")
  oo <- .C(C_vcorr,dR=as.double(unlist(dR)),Vr=as.double(Vr),Vb=as.double(rep(0,p*p)),
           p=as.integer(p),M=as.integer(M))
  return(matrix(oo$Vb,p,p))
} ## vcorr


pinv <- function(X,svd=FALSE) {
## a pseudoinverse for n by p, n>p matrices
  qrx <- qr(X,tol=0,LAPACK=TRUE)
  R <- qr.R(qrx);Q <- qr.Q(qrx) 
  rr <- Rrank(R) 
  if (svd&&rr<ncol(R)) {
    piv <- seq_len(ncol(X)); piv[qrx$pivot] <- seq_len(ncol(X))
    er <- svd(R[,piv])
    d <- er$d*0;d[seq_len(rr)] <- 1/er$d[seq_len(rr)]
    X <- Q%*%er$u%*%(d*t(er$v))
  } else {
    Ri <- R*0 
    Ri[seq_len(rr),seq_len(rr)] <- backsolve(R[seq_len(rr),seq_len(rr)],diag(rr))
    X[,qrx$pivot] <- Q%*%t(Ri)
  }
  X
} ## end pinv

pqr2 <- function(x,nt=1,nb=30) {
## Function for parallel pivoted qr decomposition of a matrix using LAPACK
## householder routines. Currently uses a block algorithm.
## library(mgcv); n <- 4000;p<-3000;x <- matrix(runif(n*p),n,p)
## system.time(qrx <- qr(x,LAPACK=TRUE))
## system.time(qrx2 <- mgcv:::pqr2(x,2)) 
## system.time(qrx3 <- mgcv:::pqr(x,2)) 
## range(qrx2$qr-qrx$qr)
  p <- ncol(x)
  beta <- rep(0.0,p)
  piv <- as.integer(rep(0,p))
  ## need to force a copy of x, otherwise x will be over-written 
  ## by .Call *in environment from which function is called*
  x <- x*1  
  rank <- .Call(C_mgcv_Rpiqr,x,beta,piv,nt,nb)
  ret <- list(qr=x,rank=rank,qraux=beta,pivot=piv+1)
  attr(ret,"useLAPACK") <- TRUE
  class(ret) <- "qr"
  ret
} ## pqr2

pbsi <- function(R,nt=1,copy=TRUE) {
## parallel back substitution inversion of upper triangular R
## library(mgcv); n <- 500;p<-400;x <- matrix(runif(n*p),n,p)
## qrx <- qr(x);R <- qr.R(qrx)
## system.time(Ri <- mgcv:::pbsi(R,2))
## system.time(Ri2 <- backsolve(R,diag(p)));range(Ri-Ri2)
  if (copy) R <- R * 1 ## ensure that R modified only within pbsi
 .Call(C_mgcv_Rpbsi,R,nt)
 R
} ## pbsi

pchol <- function(A,nt=1,nb=40) {
## parallel Choleski factorization.
## library(mgcv);
## set.seed(2);n <- 200;r <- 190;A <- tcrossprod(matrix(runif(n*r),n,r))
## system.time(R <- chol(A,pivot=TRUE));system.time(L <- mgcv:::pchol(A));range(R[1:r,]-L[1:r,])
## k <- 30;range(R[1:k,1:k]-L[1:k,1:k])
## system.time(L <- mgcv:::pchol(A,nt=2,nb=30))
## piv <- attr(L,"pivot");attr(L,"rank");range(crossprod(L)-A[piv,piv])
## should nb be obtained from 'ILAENV' as page 23 of Lucas 2004??
  piv <- as.integer(rep(0,ncol(A)))
  A <- A*1 ## otherwise over-write in calling env!
  rank <- .Call(C_mgcv_Rpchol,A,piv,nt,nb)
  attr(A,"pivot") <- piv+1;attr(A,"rank") <- rank
  A
}

pforwardsolve <- function(R,B,nt=1) {
## parallel forward solve via simple col splitting...
 if (!is.matrix(B)) B <- as.matrix(B)
 .Call(C_mgcv_Rpforwardsolve,R,B,nt)

}

pcrossprod <- function(A,trans=FALSE,nt=1,nb=30) {
## parallel cross prod A'A or AA' if trans==TRUE...
 if (!is.matrix(A)) A <- as.matrix(A)
 if (trans) A <- t(A)
 .Call(C_mgcv_Rpcross,A,nt,nb)
}

pRRt <- function(R,nt=1) {
## parallel RR' for upper triangular R
## following creates index of lower triangular elements...
## n <- 4000;a <- rep(1:n,n);b <- rep(1:n,each=n);which(a>=b) -> ii;a[ii]+(b[ii]-1)*n->ii ## lower
## n <- 4000;a <- rep(1:n,n);b <- rep(1:n,each=n);which(a<=b) -> ii;a[ii]+(b[ii]-1)*n->ii ## upper
## library(mgcv);R <- matrix(0,n,n);R[ii] <- runif(n*(n+1)/2)
## Note: A[a-b<=0] <- 0 zeroes upper triangle 
## system.time(A <- mgcv:::pRRt(R,2))
## system.time(A2 <- tcrossprod(R));range(A-A2)
  n <- nrow(R)
  A <- matrix(0,n,n)
  .Call(C_mgcv_RPPt,A,R,nt)
  A
}

block.reorder <- function(x,n.blocks=1,reverse=FALSE) {
## takes a matrix x divides it into n.blocks row-wise blocks, and re-orders 
## so that the blocks are stored one after the other. 
## e.g. library(mgcv); x <- matrix(1:18,6,3);xb <- mgcv:::block.reorder(x,2)
## x;xb;mgcv:::block.reorder(xb,2,TRUE)

 r = nrow(x);cols = ncol(x);
 if (n.blocks <= 1) return(x);
 if (r%%n.blocks) { 
   nb = ceiling(r/n.blocks)
 } else nb = r/n.blocks;
 oo <- .C(C_row_block_reorder,x=as.double(x),as.integer(r),as.integer(cols),
          as.integer(nb),as.integer(reverse));
 matrix(oo$x,r,cols)
} ## block.reorder


pqr <- function(x,nt=1) {
## parallel QR decomposition, using openMP in C, and up to nt threads (only if worthwhile)
## library(mgcv);n <- 20;p<-4;X <- matrix(runif(n*p),n,p);er <- mgcv:::pqr(X,nt=2)
## range(mgcv:::pqr.qy(er,mgcv:::pqr.R(er))-X[,er$pivot])
  x.c <- ncol(x);r <- nrow(x)
  oo <- .C(C_mgcv_pqr,x=as.double(c(x,rep(0,nt*x.c^2))),as.integer(r),as.integer(x.c),
           pivot=as.integer(rep(0,x.c)), tau=as.double(rep(0,(nt+1)*x.c)),as.integer(nt)) 
  list(x=oo$x,r=r,c=x.c,tau=oo$tau,pivot=oo$pivot+1,nt=nt)
}

pqr.R <- function(x) {
## x is an object returned by pqr. This extracts the R factor...
## e.g. as pqr then...
## R <- mgcv:::pqr.R(er); R0 <- qr.R(qr(X,tol=0))
## svd(R)$d;svd(R0)$d
  oo <- .C(C_getRpqr,R=as.double(rep(0,x$c^2)),as.double(x$x),as.integer(x$r),as.integer(x$c),
           as.integer(x$c),as.integer(x$nt))
  matrix(oo$R,x$c,x$c)
}

pqr.qy <- function(x,a,tr=FALSE) {
## x contains a parallel QR decomp as computed by pqr. a is a matrix. computes
## Qa or Q'a depending on tr.
## e.g. as above, then...
## a <- diag(p);Q <- mgcv:::pqr.qy(er,a);crossprod(Q)
## X[,er$pivot+1];Q%*%R
## Qt <- mgcv:::pqr.qy(er,diag(n),TRUE);Qt%*%t(Qt);range(Q-t(Qt))
## Q <- qr.Q(qr(X,tol=0));z <- runif(n);y0<-t(Q)%*%z
## mgcv:::pqr.qy(er,z,TRUE)->y
## z <- runif(p);y0<-Q%*%z;mgcv:::pqr.qy(er,z)->y
  if (is.matrix(a)) a.c <- ncol(a) else a.c <- 1
  if (tr) {
    if (is.matrix(a)) { if (nrow(a) != x$r) stop("'a' argument has wrong number of rows") }
    else if (length(a) != x$r) stop("'a' argument has wrong number of rows")
  } else {
    if (is.matrix(a)) { if (nrow(a) != x$c) stop("'a' argument has wrong number of rows") }
    else if (length(a) != x$c)  stop("'a' argument has wrong number of rows")
    a <- c(a,rep(0,a.c*(x$r-x$c)))
  }
  oo <- .C(C_mgcv_pqrqy,a=as.double(a),as.double(x$x),as.double(x$tau),as.integer(x$r),
                         as.integer(x$c),as.integer(a.c),as.integer(tr),as.integer(x$nt))
  if (tr) return(matrix(oo$a[seq_len(a.c*x$c)],x$c,a.c)) else
  return(matrix(oo$a,x$r,a.c))
}

pmmult <- function(A,B,tA=FALSE,tB=FALSE,nt=1) {
## parallel matrix multiplication (not for use on vectors or thin matrices)
## library(mgcv);r <- 10;c <- 5;n <- 8
## A <- matrix(runif(r*n),r,n);B <- matrix(runif(n*c),n,c);range(A%*%B-mgcv:::pmmult(A,B,nt=1))
## A <- matrix(runif(r*n),n,r);B <- matrix(runif(n*c),n,c);range(t(A)%*%B-mgcv:::pmmult(A,B,TRUE,FALSE,nt=1))
## A <- matrix(runif(r*n),n,r);B <- matrix(runif(n*c),c,n);range(t(A)%*%t(B)-mgcv:::pmmult(A,B,TRUE,TRUE,nt=1))
## A <- matrix(runif(r*n),r,n);B <- matrix(runif(n*c),c,n);range(A%*%t(B)-mgcv:::pmmult(A,B,FALSE,TRUE,nt=1))

 if (tA) { n = nrow(A);r = ncol(A)} else {n = ncol(A);r = nrow(A)}
 if (tB) { c = nrow(B)} else {c = ncol(B)}
 C <- rep(0,r * c) 
 oo <- .C(C_mgcv_pmmult,C=as.double(C),as.double(A),as.double(B),as.integer(tA),as.integer(tB),as.integer(r),
          as.integer(c),as.integer(n),as.integer(nt));
 matrix(oo$C,r,c)
}
