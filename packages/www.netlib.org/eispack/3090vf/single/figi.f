      subroutine figi(nm,n,t,d,e,e2,ierr)
c
      integer i,n,nm,ierr
      real t(nm,3),d(n),e(n),e2(n)
c
c     given a nonsymmetric tridiagonal matrix such that the products
c     of corresponding pairs of off-diagonal elements are all
c     non-negative, this subroutine reduces it to a symmetric
c     tridiagonal matrix with the same eigenvalues.  if, further,
c     a zero product only occurs when both factors are zero,
c     the reduced matrix is similar to the original matrix.
c
c     on input
c
c        nm must be set to the row dimension of two-dimensional
c          array parameters as declared in the calling program
c          dimension statement.
c
c        n is the order of the matrix.
c
c        t contains the input matrix.  its subdiagonal is
c          stored in the last n-1 positions of the first column,
c          its diagonal in the n positions of the second column,
c          and its superdiagonal in the first n-1 positions of
c          the third column.  t(1,1) and t(n,3) are arbitrary.
c
c     on output
c
c        t is unaltered.
c
c        d contains the diagonal elements of the symmetric matrix.
c
c        e contains the subdiagonal elements of the symmetric
c          matrix in its last n-1 positions.  e(1) is not set.
c
c        e2 contains the squares of the corresponding elements of e.
c          e2 may coincide with e if the squares are not needed.
c
c        ierr is set to
c          zero       for normal return,
c          n+i        if t(i,1)*t(i-1,3) is negative,
c          -(3*n+i)   if t(i,1)*t(i-1,3) is zero with one factor
c                     non-zero.  in this case, the eigenvectors of
c                     the symmetric matrix are not simply related
c                     to those of  t  and should not be sought.
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
c
      d(1) = t(1,2)
      do 100 i = 2, n
         e2(i) = t(i,1) * t(i-1,3)
         e(i) = sqrt(abs(e2(i)))
         d(i) = t(i,2)
  100 continue

      do 200 i = 2, n
         if (e2(i) .gt. 0.e0) go to 200
            if (e2(i) .lt. 0.e0) go to 1000
               if (t(i,1) .eq. 0.e0 .and. t(i-1,3) .eq. 0.e0) go to 200
c     .......... set error -- product of some pair of off-diagonal
c                elements is zero with one member non-zero ..........
               ierr = -(3 * n + i)
  200 continue
c
      go to 1001
c     .......... set error -- product of some pair of off-diagonal
c                elements is negative ..........
 1000 ierr = n + i
 1001 return
      end
