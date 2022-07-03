      subroutine trbak3(nm,n,nv,a,m,z)
c
      integer i,j,k,l,m,n,ik,iz,nm,nv
      real a(nv),z(nm,m)
      real h,s
c
c     this subroutine is a translation of the algol procedure trbak3,
c     num. math. 11, 181-195(1968) by martin, reinsch, and wilkinson.
c     handbook for auto. comp., vol.ii-linear algebra, 212-226(1971).
c
c     this subroutine forms the eigenvectors of a real symmetric
c     matrix by back transforming those of the corresponding
c     symmetric tridiagonal matrix determined by  tred3.
c
c     on input
c
c        nm must be set to the row dimension of two-dimensional
c          array parameters as declared in the calling program
c          dimension statement.
c
c        n is the order of the matrix.
c
c        nv must be set to the dimension of the array parameter a
c          as declared in the calling program dimension statement.
c
c        a contains information about the orthogonal transformations
c          used in the reduction by  tred3  in its first
c          n*(n+1)/2 positions.
c
c        m is the number of eigenvectors to be back transformed.
c
c        z contains the eigenvectors to be back transformed
c          in its first m columns.
c
c     on output
c
c        z contains the transformed eigenvectors
c          in its first m columns.
c
c     note that trbak3 preserves vector euclidean norms.
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
      if (m .eq. 0) go to 200
      if (n .eq. 1) go to 200
c
      do 140 i = 2, n
         l = i - 1
         iz = (i * l) / 2
         ik = iz + i
         h = a(ik)
         if (h .eq. 0.0e0) go to 140
c
         do 130 j = 1, m
            s = 0.0e0
            ik = iz
c
            do 110 k = 1, l
               ik = ik + 1
               s = s + a(ik) * z(k,j)
  110       continue
c     .......... double division avoids possible underflow ..........
            s = -(s / h) / h
            ik = iz
c
            do 120 k = 1, l
               ik = ik + 1
               z(k,j) = z(k,j) + s * a(ik)
  120       continue
c
  130    continue
c
  140 continue
c
  200 return
      end
