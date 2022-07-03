@process directive('"    (')
      subroutine qzhes(nm,n,a,b,matz,z)
c
      integer i,j,k,l,n,lb,l1,nm,nk1
      real a(nm,n),b(nm,n),z(nm,n)
      real r,s,t,u1,u2,v1,v2,rho
      logical matz
c
c     this subroutine is the first step of the qz algorithm
c     for solving generalized matrix eigenvalue problems,
c     siam j. numer. anal. 10, 241-256(1973) by moler and stewart.
c
c     this subroutine accepts a pair of real general matrices and
c     reduces one of them to upper hessenberg form and the other
c     to upper triangular form using orthogonal transformations.
c     it is usually followed by  qzit,  qzval  and, possibly,  qzvec.
c
c     on input
c
c        nm must be set to the row dimension of two-dimensional
c          array parameters as declared in the calling program
c          dimension statement.
c
c        n is the order of the matrices.
c
c        a contains a real general matrix.
c
c        b contains a real general matrix.
c
c        matz should be set to .true. if the right hand transformations
c          are to be accumulated for later use in computing
c          eigenvectors, and to .false. otherwise.
c
c     on output
c
c        a has been reduced to upper hessenberg form.  the elements
c          below the first subdiagonal have been set to zero.
c
c        b has been reduced to upper triangular form.  the elements
c          below the main diagonal have been set to zero.
c
c        z contains the product of the right hand transformations if
c          matz has been set to .true.  otherwise, z is not referenced.
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
c     .......... initialize z ..........
      if (.not. matz) go to 10
c
      do 3 j = 1, n
c
         do 2 i = 1, n
            z(i,j) = 0.0e0
    2    continue
c
         z(j,j) = 1.0e0
    3 continue
c     .......... reduce b to upper triangular form ..........
   10 if (n .le. 1) go to 170
c
      do 100 l = 1, n-1
         l1 = l + 1
         s = 0.0e0
c
         do 20 i = l1, n
            s = s + abs(b(i,l))
   20    continue
c
         if (s .eq. 0.0e0) go to 100
         s = s + abs(b(l,l))
         r = 0.0e0
c
         do 25 i = l, n
            b(i,l) = b(i,l) / s
            r = r + b(i,l)**2
   25    continue
c
         r = sign(sqrt(r),b(l,l))
         b(l,l) = b(l,l) + r
         rho = r * b(l,l)
c
         do 50 j = l1, n
            t = 0.0e0
c
            do 30 i = l, n
               t = t + b(i,l) * b(i,j)
   30       continue
c
            t = -t / rho
c
            do 40 i = l, n
               b(i,j) = b(i,j) + t * b(i,l)
   40       continue
c
   50    continue
c
         do 80 j = 1, n
            t = 0.0e0
c
            do 60 i = l, n
               t = t + b(i,l) * a(i,j)
   60       continue
c
            t = -t / rho
c
            do 70 i = l, n
               a(i,j) = a(i,j) + t * b(i,l)
   70       continue
c
   80    continue
c
         b(l,l) = -s * r
c
         do 90 i = l1, n
            b(i,l) = 0.0e0
   90    continue
c
  100 continue
c     .......... reduce a to upper hessenberg form, while
c                keeping b triangular ..........
      if (n .eq. 2) go to 170
      do 160 k = 1, n-2
         nk1 = n-1 - k
c     .......... for l=n-1 step -1 until k+1 do -- ..........
         do 150 lb = 1, nk1
            l = n - lb
            l1 = l + 1
c     .......... zero a(l+1,k) ..........
            s = abs(a(l,k)) + abs(a(l1,k))
            if (s .eq. 0.0e0) go to 150
            u1 = a(l,k) / s
            u2 = a(l1,k) / s
            r = sign(sqrt(u1*u1+u2*u2),u1)
            v1 =  -(u1 + r) / r
            v2 = -u2 / r
            u2 = v2 / v1
c
c"    ( prefer vector
            do 110 j = k, n
               t = a(l,j) + u2 * a(l1,j)
               a(l,j) = a(l,j) + t * v1
               a(l1,j) = a(l1,j) + t * v2
  110       continue
c
            a(l1,k) = 0.0e0
c
c"    ( prefer vector
            do 120 j = l, n
               t = b(l,j) + u2 * b(l1,j)
               b(l,j) = b(l,j) + t * v1
               b(l1,j) = b(l1,j) + t * v2
  120       continue
c     .......... zero b(l+1,l) ..........
            s = abs(b(l1,l1)) + abs(b(l1,l))
            if (s .eq. 0.0e0) go to 150
            u1 = b(l1,l1) / s
            u2 = b(l1,l) / s
            r = sign(sqrt(u1*u1+u2*u2),u1)
            v1 =  -(u1 + r) / r
            v2 = -u2 / r
            u2 = v2 / v1
c
            do 130 i = 1, l1
               t = b(i,l1) + u2 * b(i,l)
               b(i,l1) = b(i,l1) + t * v1
               b(i,l) = b(i,l) + t * v2
  130       continue
c
            b(l1,l) = 0.0e0
c
            do 140 i = 1, n
               t = a(i,l1) + u2 * a(i,l)
               a(i,l1) = a(i,l1) + t * v1
               a(i,l) = a(i,l) + t * v2
  140       continue
c
            if (.not. matz) go to 150
c
            do 145 i = 1, n
               t = z(i,l1) + u2 * z(i,l)
               z(i,l1) = z(i,l1) + t * v1
               z(i,l) = z(i,l) + t * v2
  145       continue
c
  150    continue
c
  160 continue
c
  170 return
      end
