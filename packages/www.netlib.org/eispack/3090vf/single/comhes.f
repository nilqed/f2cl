@process directive('"    (')
      subroutine comhes(nm,n,low,igh,ar,ai,int)
c
      integer i,j,m,n,nm,igh,low,mm1,mp1,ip
      real ar(nm,n),ai(nm,n)
      real xr,xi,yr,yi
      real s,t,ais,ars,bis,brs
      integer int(igh)
c
c     this subroutine is a translation of the algol procedure comhes,
c     num. math. 12, 349-368(1968) by martin and wilkinson.
c     handbook for auto. comp., vol.ii-linear algebra, 339-358(1971).
c
c     given a complex general matrix, this subroutine
c     reduces a submatrix situated in rows and columns
c     low through igh to upper hessenberg form by
c     stabilized elementary similarity transformations.
c
c     on input
c
c        nm must be set to the row dimension of two-dimensional
c          array parameters as declared in the calling program
c          dimension statement.
c
c        n is the order of the matrix.
c
c        low and igh are integers determined by the balancing
c          subroutine  cbal.  if  cbal  has not been used,
c          set low=1, igh=n.
c
c        ar and ai contain the real and imaginary parts,
c          respectively, of the complex input matrix.
c
c     on output
c
c        ar and ai contain the real and imaginary parts,
c          respectively, of the hessenberg matrix.  the
c          multipliers which were used in the reduction
c          are stored in the remaining triangles under the
c          hessenberg matrix.
c
c        int contains information on the rows and columns
c          interchanged in the reduction.
c          only elements low through igh are used.
c
c     calls cdiv for complex division.
c
c     Questions and comments should be directed to Alan K. Cline,
c     Pleasant Valley Software, 8603 Altus Cove, Austin, TX 78759.
c     Electronic mail to cline@cs.utexas.edu.
c
c     this version dated january 1989. (for the IBM 3090vf)
c
c     ------------------------------------------------------------------
c
      call xuflow(0)
      do 180 m = low+1, igh-1
         mm1 = m - 1
         xr = 0.0e0
         xi = 0.0e0
         ip = m
c
         do 100 j = m, igh
            if (abs(ar(j,mm1)) + abs(ai(j,mm1))
     x         .le. abs(xr) + abs(xi)) go to 100
            xr = ar(j,mm1)
            xi = ai(j,mm1)
            ip = j
  100    continue
c
         int(m) = ip
c     .......... interchange columns of ar and ai ..........
c
         do 120 j = 1, igh
            yr = ar(j,ip)
            ar(j,ip) = ar(j,m)
            ar(j,m) = yr
            yi = ai(j,ip)
            ai(j,ip) = ai(j,m)
            ai(j,m) = yi
  120    continue
c
         yr = ar(ip,mm1)
         ar(ip,mm1) = ar(m,mm1)
         ar(m,mm1) = yr
c
         yi = ai(ip,mm1)
         ai(ip,mm1) = ai(m,mm1)
         ai(m,mm1) = yi
c
         if (xr .eq. 0.0e0 .and. xi .eq. 0.0e0) then
c"    ( prefer vector
            do 131 j = m, n
               yr = ar(ip,j)
               ar(ip,j) = ar(m,j)
               ar(m,j) = yr
               yi = ai(ip,j)
               ai(ip,j) = ai(m,j)
               ai(m,j) = yi
  131       continue
            goto 180
         endif
         mp1 = m + 1
c
c Scale column m-1 by <xr,xi>
c
         s = abs(xr) + abs(xi)
         brs = xr/s
         bis = xi/s
         t = brs**2 + bis**2

         do 135 i=mp1,igh
            ars = ar(i,mm1)/s
            ais = ai(i,mm1)/s
            ar(i,mm1) = (ars*brs + ais*bis) / t
            ai(i,mm1) = (ais*brs - ars*bis) / t
  135    continue
c
c Exchange elements a(ip,m) and a(m,m). Then update column m.
c
         yr = ar(ip,m)
         ar(ip,m) = ar(m,m)
         ar(m,m) = yr
         yi = ai(ip,m)
         ai(ip,m) = ai(m,m)
         ai(m,m) = yi
         do 137 i=mp1, igh
            ar(i,m) = ar(i,m) - yr * ar(i,mm1) + yi * ai(i,mm1)
            ai(i,m) = ai(i,m) - yr * ai(i,mm1) - yi * ar(i,mm1)
  137    continue
c
c Update columns m+1 through igh.
c
         do 140 j = mp1, igh
            yr = ar(ip,j)
            ar(ip,j) = ar(m,j)
            ar(m,j) = yr
            yi = ai(ip,j)
            ai(ip,j) = ai(m,j)
            ai(m,j) = yi
            do 138 i = mp1, igh
               ar(i,j) = ar(i,j) - yr * ar(i,mm1) + yi * ai(i,mm1)
               ai(i,j) = ai(i,j) - yr * ai(i,mm1) - yi * ar(i,mm1)
  138       continue

c"    ( ignore recrdeps
            do 139 i = 1, igh
               ar(i,m)=ar(i,m)+ar(j,mm1)*ar(i,j)-ai(j,mm1)*ai(i,j)
               ai(i,m)=ai(i,m)+ar(j,mm1)*ai(i,j)+ai(j,mm1)*ar(i,j)
  139       continue
  140    continue
c
c Update columns igh through n.
c
         do 150 j = igh+1,n
            yr = ar(ip,j)
            ar(ip,j) = ar(m,j)
            ar(m,j) = yr
            yi = ai(ip,j)
            ai(ip,j) = ai(m,j)
            ai(m,j) = yi
            do 145 i = mp1, igh
               ar(i,j) = ar(i,j) - yr * ar(i,mm1) + yi * ai(i,mm1)
               ai(i,j) = ai(i,j) - yr * ai(i,mm1) - yi * ar(i,mm1)
  145       continue
  150    continue
c
c
  180 continue
c
      return
      end
