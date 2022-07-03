      subroutine figi2(nm,n,t,d,e,z,ierr)
c
      integer i,j,n,nm,ierr
      real t(nm,3),d(n),e(n),z(nm,n)
c
c     given a nonsymmetric tridiagonal matrix such that the products
c     of corresponding pairs of off-diagonal elements are all
c     non-negative, and zero only when both factors are zero, this
c     subroutine reduces it to a symmetric tridiagonal matrix
c     using and accumulating diagonal similarity transformations.
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
c        z contains the transformation matrix produced in
c          the reduction.
c
c        ierr is set to
c          zero       for normal return,
c          n+i        if t(i,1)*t(i-1,3) is negative,
c          2*n+i      if t(i,1)*t(i-1,3) is zero with
c                     one factor non-zero.
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
      do 100 j = 2, n
         z(j,1) = 0.e0
         e(j) = t(j,1) * t(j-1,3)
  100 continue
c
      do 200 j = 2, n
         if (e(j).gt.0.e0) go to 200
            if (e(j).lt.0.e0) go to 900
               if (t(j,1).ne.0.e0 .or. t(j-1,3).ne.0.e0) go to 1000
  200 continue
c
      z(1,1) = 1.e0
      d(1) = t(1,2)
      do 320 j = 2, n
         d(j) = t(j,2)
         e(j) = sqrt(e(j))
         do 300 i = 1, n
  300       z(i,j) = 0.e0
         z(j,j) = 1.e0
         if (t(j-1,3).ne.0.e0) z(j,j) = z(j-1,j-1) * e(j) / t(j-1,3)
  320    continue
c
      go to 1001
c     .......... set error -- product of some pair of off-diagonal
c                elements is negative ..........
  900 ierr = n + i
      go to 1001
c     .......... set error -- product of some pair of off-diagonal
c                elements is zero with one member non-zero ..........
 1000 ierr = 2 * n + i
 1001 return
      end
