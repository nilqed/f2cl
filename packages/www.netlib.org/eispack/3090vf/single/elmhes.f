@process directive('"    (')
      subroutine elmhes(nm,n,low,igh,a,int)
c
      integer i,j,m,n,nm,igh,low,mm1,mp1,ip
      real a(nm,n)
      real x,y
      integer int(igh)
c
c     this subroutine is a translation of the algol procedure elmhes,
c     num. math. 12, 349-368(1968) by martin and wilkinson.
c     handbook for auto. comp., vol.ii-linear algebra, 339-358(1971).
c
c     given a real general matrix, this subroutine
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
c          subroutine  balanc.  if  balanc  has not been used,
c          set low=1, igh=n.
c
c        a contains the input matrix.
c
c     on output
c
c        a contains the hessenberg matrix.  the multipliers
c          which were used in the reduction are stored in the
c          remaining triangle under the hessenberg matrix.
c
c        int contains information on the rows and columns
c          interchanged in the reduction.
c          only elements low through igh are used.
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
         x = 0.0e0
         ip = m
c
         do 100 j = m, igh
            if (abs(a(j,mm1)) .le. abs(x)) go to 100
            x = a(j,mm1)
            ip = j
  100    continue
c
         int(m) = ip
         if (x .eq. 0.0e0) goto 180
c
c     .......... interchange columns of a ..........
         do 120 j = 1, igh
            y = a(j,ip)
            a(j,ip) = a(j,m)
            a(j,m) = y
  120    continue
c
         y = a(ip,mm1)
         a(ip,mm1) = a(m,mm1)
         a(m,mm1) = y
c
         mp1 = m + 1
c
         do 135 i=mp1,igh
  135       a(i,mm1) = a(i,mm1)/x
c
c To avoid passing through the address space of  a  two times
c (as would be done if the LHS and RHS updates were performed
c one after the other) these updates are interleaved.
c Note that the inner loop, with label 138, must be executed
c for j=m,n, while the other iner loop, 139, is executed only
c for j=m+1,igh.)  To accomplish this, the outer loop (on j=m,n below)
c must be broken into three separate cases: j=m | j=m+1,igh | j=igh+1,n.
c
         y = a(ip,m)
         a(ip,m) = a(m,m)
         a(m,m) = y
         y = -y
         do 137 i = mp1, igh
  137       a(i,m) = a(i,m) + y * a(i,mm1)
c
         do 140 j = mp1, igh
            y = a(ip,j)
            a(ip,j) = a(m,j)
            a(m,j) = y
            y = -y
            do 138 i = mp1, igh
  138          a(i,j) = a(i,j) + y * a(i,mm1)
c"    ( ignore recrdeps
            do 139 i = 1, igh
  139          a(i,m) = a(i,m) + a(j,mm1)*a(i,j)
  140    continue
c
         do 150 j = igh+1, n
            y = a(ip,j)
            a(ip,j) = a(m,j)
            a(m,j) = y
            y = -y
            do 145 i = mp1, igh
  145          a(i,j) = a(i,j) + y * a(i,mm1)
  150    continue
c
  180 continue
c
      return
      end
