@process directive('"    (')
      subroutine reduc2(nm,n,a,b,dl,ierr)
c
      integer i,j,k,n,nm,nn,ierr
      real a(nm,n),b(nm,n),dl(n)
      real x,y
c
c     this subroutine is a translation of the algol procedure reduc2,
c     num. math. 11, 99-110(1968) by martin and wilkinson.
c     handbook for auto. comp., vol.ii-linear algebra, 303-314(1971).
c
c     this subroutine reduces the generalized symmetric eigenproblems
c     abx=(lambda)x or bay=(lambda)y, where b is positive definite,
c     to the standard symmetric eigenproblem using the cholesky
c     factorization of b.
c
c     on input
c
c        nm must be set to the row dimension of two-dimensional
c          array parameters as declared in the calling program
c          dimension statement.
c
c        n is the order of the matrices a and b.  if the cholesky
c          factor l of b is already available, n should be prefixed
c          with a minus sign.
c
c        a and b contain the real symmetric input matrices.  only the
c          full upper triangles of the matrices need be supplied.  if
c          n is negative, the strict lower triangle of b contains,
c          instead, the strict lower triangle of its cholesky factor l.
c
c        dl contains, if n is negative, the diagonal elements of l.
c
c     on output
c
c        a contains in its full lower triangle the full lower triangle
c          of the symmetric matrix derived from the reduction to the
c          standard form.  the strict upper triangle of a is unaltered.
c
c        b contains in its strict lower triangle the strict lower
c          triangle of its cholesky factor l.  the full upper
c          triangle of b is unaltered.
c
c        dl contains the diagonal elements of l.
c
c        ierr is set to
c          zero       for normal return,
c          7*n+1      if b is not positive definite.
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
      ierr = 0
      nn = iabs(n)
c     .......... form l in the arrays b and dl ..........
      do 80 i = 1, n
         do 80 j = i, n
            x = b(i,j)
c"    ( prefer vector
            do 20 k = 1, i-1
   20          x = x - b(i,k) * b(j,k)
            if (j .ne. i) go to 60
            if (x .le. 0.0e0) go to 1000
            y = sqrt(x)
            dl(i) = y
            go to 80
   60       b(j,i) = x / y
   80 continue
c     .......... form the lower triangle of a*l
c                in the lower triangle of the array a ..........
      do 200 i = 1, nn
         do 200 j = 1, i
            x = a(j,i) * dl(j)
            do 120 k = j+1, i
  120          x = x + a(k,i) * b(k,j)
            do 160 k = i+1, nn
  160          x = x + a(i,k) * b(k,j)
            a(i,j) = x
  200 continue
c     .......... pre-multiply by transpose(l) and overwrite ..........
      do 300 i = 1, nn
         y = dl(i)
         do 300 j = 1, i
            x = y * a(i,j)
            do 260 k = i+1, nn
  260          x = x + a(k,j) * b(k,i)
            a(i,j) = x
  300 continue
c
      go to 1001
c     .......... set error -- b is not positive definite ..........
 1000 ierr = 7 * n + 1
 1001 return
      end
