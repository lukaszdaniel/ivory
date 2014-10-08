c  Part of R package KernSmooth
c  Copyright (C) 1995  M. P. Wand
c
c  Unlimited use and distribution (see LICENCE).

cccccccccc FORTRAN subroutine sstdg cccccccccc

c For computing the diagonal entries of the "binned"
c version of SS^T, where S is a smoother matrix for
c local polynomial fitting.

c Last changed: 10/02/95

      subroutine sstdg(xcnts,delta,hdisc,Lvec,indic,
     +                 midpts,M,iQ,fkap,ipp,ippp,ss,uu,Smat,
     +                 Umat,work,det,ipvt,SSTd)
      integer i,j,k,Lvec(*),M,iQ,mid,indic(*),midpts(*),
     +        ipvt(*),info,ii,ipp,ippp,indss
      double precision xcnts(*),fkap(*),hdisc(*),
     +                 delta,ss(M,ippp),uu(M,ippp),Smat(ipp,ipp),
     +                 Umat(ipp,ipp),SSTd(*),fac,work(*),det(2)

c Obtain kernel weights

      mid = Lvec(1) + 1
      do 10 i=1,(iQ-1)
         midpts(i) = mid
         fkap(mid) = 1.0d0
         do 20 j=1,Lvec(i)
            fkap(mid+j) = exp(-(delta*j/hdisc(i))**2/2)
            fkap(mid-j) = fkap(mid+j)
20       continue
         mid = mid + Lvec(i) + Lvec(i+1) + 1
10       continue
      midpts(iQ) = mid
      fkap(mid) = 1.0d0
      do 30 j=1,Lvec(iQ)
         fkap(mid+j) = exp(-(delta*j/hdisc(iQ))**2/2)
         fkap(mid-j) = fkap(mid+j)
30    continue

c Combine kernel weights and grid counts

      do 40 k = 1,M
         if (xcnts(k).ne.0) then
            do 50 i = 1,iQ
               do 60 j = max(1,k-Lvec(i)),min(M,k+Lvec(i))
                  if (indic(j).eq.i) then
                     fac = 1.0d0
                     ss(j,1) = ss(j,1) + xcnts(k)*fkap(k-j+midpts(i))
                     uu(j,1) = uu(j,1) +
     +                         xcnts(k)*fkap(k-j+midpts(i))**2
                     do 70 ii = 2,ippp
                        fac = fac*delta*(k-j)
                        ss(j,ii) = ss(j,ii)
     +                  + xcnts(k)*fkap(k-j+midpts(i))*fac
                        uu(j,ii) = uu(j,ii)
     +                  + xcnts(k)*(fkap(k-j+midpts(i))**2)*fac
70                   continue
                  endif
60             continue
50          continue
         endif
40    continue

      do 80 k = 1,M
         SSTd(k) = dble(0)
         do 90 i = 1,ipp
            do 100 j = 1,ipp
               indss = i + j - 1
               Smat(i,j) = ss(k,indss)
               Umat(i,j) = uu(k,indss)
100         continue
90       continue

         call dgefa(Smat,ipp,ipp,ipvt,info)
         call dgedi(Smat,ipp,ipp,ipvt,det,work,01)

         do 110 i = 1,ipp
            do 120 j = 1,ipp
               SSTd(k) =  SSTd(k) + Smat(1,i)*Umat(i,j)*Smat(j,1)
120         continue
110      continue

80    continue

      return
      end

cccccccccc End of sstdg cccccccccc
