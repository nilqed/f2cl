@process directive('"    (')
      subroutine orthes(nm,n,low,igh,a,ort)
c
      integer i,j,m,n,mp,nm,igh,low
      real a(nm,n),ort(igh)
      real f,g,h,scale
c
c     this subroutine is a translation of the algol procedure orthes,
c     num. math. 12, 349-368(1968) by martin and wilkinson.
c     handbook for auto. comp., vol.ii-linear algebra, 339-358(1971).
c
c     given a real general matrix, this subroutine
c     reduces a submatrix situated in rows and columns
c     low through igh to upper hessenberg form by
c     orthogonal similarity transformations.
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
c        a contains the hessenberg matrix.  information about
c          the orthogonal transformations used in the reduction
c          is stored in the remaining triangle under the
c          hessenberg matrix.
c
c        ort contains further information about the transformations.
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
         h = 0.0e0
         ort(m) = 0.0e0
         scale = 0.0e0
c     .......... scale column (algol tol then not needed) ..........
         do 90 i = m, igh
   90    scale = scale + abs(a(i,m-1))
c
         if (scale .eq. 0.0e0) go to 180
         scale = 1.0e0/scale
         mp = m + igh
c     .......... for i=igh step -1 until m do -- This has been reversed
         do 100 i = m, igh
            ort(i) = a(i,m-1) * scale
            h = h + ort(i) * ort(i)
  100    continue
c
         g = -sign(sqrt(h),ort(m))
         h = 1.0e0/(h - ort(m) * g)
         ort(m) = ort(m) - g
c     .......... form (i-(u*ut)/h) * a ..........
c"    ( prefer vector
         do 130 j = m, n
            f = 0.0e0
c     .......... for i=igh step -1 until m do -- This has been reversed
            do 110 i = m, igh
               f = f + ort(i) * a(i,j)
  110       continue
c
            f = -f * h
c
            do 120 i = m, igh
  120       a(i,j) = a(i,j) + f * ort(i)
c
  130    continue
c     .......... form (i-(u*ut)/h)*a*(i-(u*ut)/h) ..........
c"    ( prefer vector
         do 160 i = 1, igh
            f = 0.0e0
c     .......... for j=igh step -1 until m do -- This has been reversed
            do 140 j = m, igh
               f = f + ort(j) * a(i,j)
  140       continue
c
            f = -f * h
c
            do 150 j = m, igh
  150       a(i,j) = a(i,j) + f * ort(j)
c
  160    continue
c
         ort(m) = ort(m)/scale
         a(m,m-1) = g/scale
  180 continue
c
      return
      end
