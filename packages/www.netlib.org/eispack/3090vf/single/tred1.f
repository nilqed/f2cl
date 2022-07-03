@process directive('"    (')
      subroutine tred1(nm,n,a,d,e,e2)
c
      integer i,j,k,l,n,nm
      real a(nm,n),d(n),e(n),e2(n)
      real f,g,h,scale
c
c     this subroutine is a translation of the algol procedure tred1,
c     num. math. 11, 181-195(1968) by martin, reinsch, and wilkinson.
c     handbook for auto. comp., vol.ii-linear algebra, 212-226(1971).
c
c     this subroutine reduces a real symmetric matrix
c     to a symmetric tridiagonal matrix using
c     orthogonal similarity transformations.
c
c     on input
c
c     nm must be set to the row dimension of two-dimensional
c       array parameters as declared in the calling program
c       dimension statement.
c
c     n is the order of the matrix.
c
c     a contains the real symmetric input matrix.  only the
c       lower triangle of the matrix need be supplied.
c
c     on output
c
c     a contains information about the orthogonal trans-
c       formations used in the reduction in its strict lower
c       triangle.  the full upper triangle of a is unaltered.
c
c     d contains the diagonal elements of the tridiagonal matrix.
c
c     e contains the subdiagonal elements of the tridiagonal
c       matrix in its last n-1 positions.  e(1) is set to zero.
c
c     e2 contains the squares of the corresponding elements of e.
c       e2 may coincide with e if the squares are not needed.
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
      do 100 i = 1, n
         d(i) = a(n,i)
         a(n,i) = a(i,i)
  100 continue
      do 300 i = n, 1, -1
         l = i - 1
         h = 0.0e0
         scale = 0.0e0
c     .......... scale row (algol tol then not needed) ..........
         do 120 k = 1, l
  120       scale = scale + abs(d(k))
c
         if (scale .ne. 0.0e0) go to 140
c
c"    ( prefer vector
         do 125 j = 1, l
            d(j) = a(l,j)
            a(l,j) = a(i,j)
            a(i,j) = 0.0e0
  125    continue
c
         e(i) = 0.0e0
         e2(i) = 0.0e0
         go to 300
c
  140    do 150 k = 1, l
            d(k) = d(k) / scale
            h = h + d(k) * d(k)
  150    continue
c
         e2(i) = scale * scale * h
         f = d(l)
         g = -sign(sqrt(h),f)
         e(i) = scale * g
         h = h - f * g
         d(l) = f - g
         if (l .eq. 1) go to 285
c     .......... form a*u ..........
         do 170 j = 1, l
  170       e(j) = 0.0e0
c
         do 240 j = 1, l
            f = d(j)
            g = e(j) + a(j,j) * f
c
            do 200 k = j+1, l
               g = g + a(k,j) * d(k)
               e(k) = e(k) + a(k,j) * f
  200       continue
c
            e(j) = g
  240    continue
c     .......... form p ..........
         f = 0.0e0
c
         do 245 j = 1, l
            e(j) = e(j) / h
            f = f + e(j) * d(j)
  245    continue
c
         h = -f / (h + h)
c     .......... form q ..........
         do 250 j = 1, l
  250       e(j) = e(j) + h * d(j)
c     .......... form reduced a ..........
         do 280 j = 1, l
            f = -d(j)
            g = -e(j)
c
            do 260 k = j, l
  260          a(k,j) = a(k,j) + f * e(k) + g * d(k)
c
  280    continue
c
c"    ( prefer vector
  285    do 290 j = 1, l
            f = d(j)
            d(j) = a(l,j)
            a(l,j) = a(i,j)
            a(i,j) = f * scale
  290    continue
c
  300 continue
c
      return
      end
