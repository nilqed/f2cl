@process directive('"    (')
      subroutine ortran(nm,n,low,igh,a,ort,z)
c
      integer i,j,n,mp,nm,igh,low
      real a(nm,igh),ort(igh),z(nm,n)
      real g
c
c     this subroutine is a translation of the algol procedure ortrans,
c     num. math. 16, 181-204(1970) by peters and wilkinson.
c     handbook for auto. comp., vol.ii-linear algebra, 372-395(1971).
c
c     this subroutine accumulates the orthogonal similarity
c     transformations used in the reduction of a real general
c     matrix to upper hessenberg form by  orthes.
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
c        a contains information about the orthogonal trans-
c          formations used in the reduction by  orthes
c          in its strict lower triangle.
c
c        ort contains further information about the trans-
c          formations used in the reduction by  orthes.
c          only elements low through igh are used.
c
c     on output
c
c        z contains the transformation matrix produced in the
c          reduction by  orthes.
c
c        ort has been altered.
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
c     .......... initialize z to identity matrix ..........
      do 80 j = 1, n
c
         do 60 i = 1, n
   60    z(i,j) = 0.0e0
c
         z(j,j) = 1.0e0
   80 continue
c
c     .......... for mp=igh-1 step -1 until low+1 do --
      do 140 mp = igh-1, low+1, -1
         if (a(mp,mp-1) .eq. 0.0e0) go to 140
c
         do 100 i = mp+1, igh
  100    ort(i) = a(i,mp-1)
c
c"    ( prefer vector
         do 130 j = mp, igh
            g = 0.0e0
c
            do 110 i = mp, igh
  110       g = g + ort(i) * z(i,j)
c     .......... divisor below is negative of h formed in orthes.
c                double division avoids possible underflow ..........
            g = (g / ort(mp)) / a(mp,mp-1)
c
            do 120 i = mp, igh
  120       z(i,j) = z(i,j) + g * ort(i)
c
  130    continue
c
  140 continue
c
      return
      end
