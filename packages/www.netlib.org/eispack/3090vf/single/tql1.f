      subroutine tql1(n,d,e,ierr)
c
      integer i,j,l,m,n,l1,l2,ierr
      real d(n),e(n)
      real c,c2,c3,dl1,el1,f,g,h,p,r,s,s2,tst1,tst2
c
c     this subroutine is a translation of the algol procedure tql1,
c     num. math. 11, 293-306(1968) by bowdler, martin, reinsch, and
c     wilkinson.
c     handbook for auto. comp., vol.ii-linear algebra, 227-240(1971).
c
c     this subroutine finds the eigenvalues of a symmetric
c     tridiagonal matrix by the ql method.
c
c     on input
c
c        n is the order of the matrix.
c
c        d contains the diagonal elements of the input matrix.
c
c        e contains the subdiagonal elements of the input matrix
c          in its last n-1 positions.  e(1) is arbitrary.
c
c      on output
c
c        d contains the eigenvalues in ascending order.  if an
c          error exit is made, the eigenvalues are correct and
c          ordered for indices 1,2,...ierr-1, but may not be
c          the smallest eigenvalues.
c
c        e has been destroyed.
c
c        ierr is set to
c          zero       for normal return,
c          j          if the j-th eigenvalue has not been
c                     determined after 30 iterations.
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
      if (n .eq. 1) go to 1001
c
      do 100 i = 2, n
  100 e(i-1) = e(i)
c
      f = 0.0e0
      tst1 = 0.0e0
      e(n) = 0.0e0
c
      do 290 l = 1, n
         j = 0
         h = abs(d(l)) + abs(e(l))
         if (tst1 .lt. h) tst1 = h
c     .......... look for small sub-diagonal element ..........
         do 110 m = l, n
            tst2 = tst1 + abs(e(m))
            if (tst2 .eq. tst1) go to 120
c     .......... e(n) is always zero, so there is no exit
c                through the bottom of the loop ..........
  110    continue
c
  120    if (m .eq. l) go to 210
  130    if (j .eq. 30) go to 1000
         j = j + 1
c     .......... form shift ..........
         l1 = l + 1
         l2 = l1 + 1
         g = d(l)
         p = (d(l1) - g) / (2.0e0 * e(l))
cccccccccccccccccccccccccccccccccccccccccccc
c *      r = pythag(p,1.0d0)
c *      d(l) = e(l) / (p + dsign(r,p))
c *      d(l1) = e(l) * (p + dsign(r,p))
cccccccccccccccccccccccccccccccccccccccccccc
         if (abs(p).le.1.0e0) then
            p = p + sign(sqrt(1.0e0 + p*p),p)
         else
            p = p * (1.0e0 + sqrt(1.0e0 + (1.0e0/p)**2))
         endif
         d(l) = e(l) / p
         d(l1) = e(l) * p
cccccccccccccccccccccccccccccccccccccccccccc
         dl1 = d(l1)
         h = g - d(l)
         if (l2 .gt. n) go to 145
c
         do 140 i = l2, n
  140    d(i) = d(i) - h
c
  145    f = f + h
c     .......... ql transformation ..........
         p = d(m)
         c = 1.0e0
         c2 = c
         el1 = e(l1)
         s = 0.0e0
c     .......... for i=m-1 step -1 until l do -- ..........
         do 200 i = m-1, l, -1
            c3 = c2
            c2 = c
            s2 = s
            g = c * e(i)
            h = c * p
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c *         r = pythag(p,e(i))
c *         e(i+1) = s * r
c *         s = e(i) / r
c *         c = p / r
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
            if (abs(p).ge.abs(e(i))) then
               s = e(i)/p
               r = sqrt(1.0e0 + s*s)
               e(i+1) = s2 * p * r
               c = 1.0e0 / r
               s = s * c
            else
               c = p/e(i)
               r = sqrt(1.0e0 + c*c)
               e(i+1) = s2 * e(i) * r
               s = 1.0e0 / r
               c = c * s
            endif
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
            p = c * d(i) - s * g
            d(i+1) = h + s * (c * g + s * d(i))
  200    continue
c
         p = -s * s2 * c3 * el1 * e(l) / dl1
         e(l) = s * p
         d(l) = c * p
         tst2 = tst1 + abs(e(l))
         if (tst2 .gt. tst1) go to 130
  210    p = d(l) + f
c     .......... order eigenvalues ..........
c     .......... for i=l step -1 until 2 do -- ..........
         do 230 i = l, 2, -1
            if (p .ge. d(i-1)) go to 270
            d(i) = d(i-1)
  230    continue
c
         i = 1
  270    d(i) = p
  290 continue
c
      go to 1001
c     .......... set error -- no convergence to an
c                eigenvalue after 30 iterations ..........
 1000 ierr = l
 1001 return
      end
