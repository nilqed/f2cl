@process directive('"    (')
      subroutine reduc(nm,n,a,b,dl,ierr)
c
      integer i,j,k,n,nm,nn,ierr
      real a(nm,n),b(nm,n),dl(n)
      real x,y
c
c     this subroutine is a translation of the algol procedure reduc1,
c     num. math. 11, 99-110(1968) by martin and wilkinson.
c     handbook for auto. comp., vol.ii-linear algebra, 303-314(1971).
c
c     this subroutine reduces the generalized symmetric eigenproblem
c     ax=(lambda)bx, where b is positive definite, to the standard
c     symmetric eigenproblem using the cholesky factorization of b.
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
c     .......... form the transpose of the upper triangle of inv(l)*a
c                in the lower triangle of the array a ..........
      do 200 i = 1, nn
         y = dl(i)
         do 200 j = i, nn
            x = a(i,j)
            do 160 k = 1, i-1
  160          x = x - b(i,k) * a(j,k)
            a(j,i) = x / y
  200 continue
c     .......... pre-multiply by inv(l) and overwrite ..........
      do 300 j = 1, nn
         do 300 i = j, nn
            x = a(i,j)
            do 220 k = j, i-1
  220          x = x - a(k,j) * b(i,k)
            do 260 k = 1, j-1
  260          x = x - a(j,k) * b(i,k)
            a(i,j) = x / dl(i)
  300 continue
c
      go to 1001
c     .......... set error -- b is not positive definite ..........
 1000 ierr = 7 * n + 1
 1001 return
      end
