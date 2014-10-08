## (c) Simon N. Wood 2011-2014
## Many of the following are simple wrappers for C functions, used largely 
## for testing purposes


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
    dH <- list();ind <- 1:(nb*nb)
    for (i in 1:nsp) { 
      dH[[i]] <- matrix(oo$dH[ind],nb,nb)
      ind <- ind + nb*nb
    }
  }
  list(l=oo$ll,lb=oo$lb,lbb=matrix(oo$lbb,nb,nb),dH=dH)
}

pinv <- function(X,svd=FALSE) {
## a pseudoinverse for n by p, n>p matrices
  qrx <- qr(X,tol=0,LAPACK=TRUE)
  R <- qr.R(qrx);Q <- qr.Q(qrx) 
  rr <- Rrank(R) 
  if (svd&&rr<ncol(R)) {
    piv <- 1:ncol(X); piv[qrx$pivot] <- 1:ncol(X)
    er <- svd(R[,piv])
    d <- er$d*0;d[1:rr] <- 1/er$d[1:rr]
    X <- Q%*%er$u%*%(d*t(er$v))
  } else {
    Ri <- R*0 
    Ri[1:rr,1:rr] <- backsolve(R[1:rr,1:rr],diag(rr))
    X[,qrx$pivot] <- Q%*%t(Ri)
  }
  X
} ## end pinv

pqr2 <- function(x,nt=1) {
## Function for parallel pivoted qr decomposition of a matrix using LAPACK
## householder routines...
## library(mgcv); n <- 10000;p<-1000;x <- matrix(runif(n*p),n,p)
## system.time(qrx <- qr(x,LAPACK=TRUE))
## system.time(qrx2 <- mgcv:::pqr2(x,2)) 
## system.time(qrx3 <- mgcv:::pqr(x,2)) 
## range(qrx2$qr-qrx$qr)
  p <- ncol(x)
  beta <- rep(0.0,p)
  piv <- as.integer(rep(0,p))
  xc <- x*1
  rank <- .Call(C_mgcv_Rpiqr,xc,beta,piv,nt)
  ret <- list(qr=xc,rank=rank,qraux=beta,pivot=piv+1)
  attr(ret,"useLAPACK") <- TRUE
  class(ret) <- "qr"
  ret
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
}

pqr <- function(x,nt=1) {
## parallel QR decomposition, using openMP in C, and up to nt threads (only if worthwhile)
## library(mgcv);n <- 20;p<-4;X <- matrix(runif(n*p),n,p);er <- mgcv:::pqr(X,nt=2)
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
  if (tr) return(matrix(oo$a[1:(a.c*x$c)],x$c,a.c)) else
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