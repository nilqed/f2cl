@process directive('"    (')
      subroutine hqr(nm,n,low,igh,h,wr,wi,ierr)
c
      integer i,j,k,l,m,n,en,na,nm,igh,itn,its,low,enm2,ierr
      real h(nm,n),wr(n),wi(n)
      real p,q,r,s,t,w,x,y,zz,norm,tst1,tst2,foo
      logical notlas
c
c     this subroutine is a translation of the algol procedure hqr,
c     num. math. 14, 219-231(1970) by martin, peters, and wilkinson.
c     handbook for auto. comp., vol.ii-linear algebra, 359-371(1971).
c
c     this subroutine finds the eigenvalues of a real
c     upper hessenberg matrix by the qr method.
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
c        h contains the upper hessenberg matrix.  information about
c          the transformations used in the reduction to hessenberg
c          form by  elmhes  or  orthes, if performed, is stored
c          in the remaining triangle under the hessenberg matrix.
c
c     on output
c
c        h has been destroyed.  therefore, it must be saved
c          before calling  hqr  if subsequent calculation and
c          back transformation of eigenvectors is to be performed.
c
c        wr and wi contain the real and imaginary parts,
c          respectively, of the eigenvalues.  the eigenvalues
c          are unordered except that complex conjugate pairs
c          of values appear consecutively with the eigenvalue
c          having the positive imaginary part first.  if an
c          error exit is made, the eigenvalues should be correct
c          for indices ierr+1,...,n.
c
c        ierr is set to
c          zero       for normal return,
c          j          if the limit of 30*n iterations is exhausted
c                     while the j-th eigenvalue is being sought.
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
      norm = 0.0e0
c     .......... store roots isolated by balanc
c                and compute matrix norm ..........
      do 50 j = 1, n
         do 50 i = 1,min0(n,j+1)
   50       norm = norm + abs(h(i,j))
c
c"    ( prefer vector
      do 52 i = 1, low-1
         wr(i) = h(i,i)
         wi(i) = 0.0e0
   52 continue
c
c"    ( prefer vector
      do 54 i = igh+1,n
         wr(i) = h(i,i)
         wi(i) = 0.0e0
   54 continue
c
      en = igh
      t = 0.0e0
      itn = 30*n
c     .......... search for next eigenvalues ..........
   60 if (en .lt. low) go to 1001
      its = 0
      na = en - 1
      enm2 = na - 1
c     .......... look for single small sub-diagonal element
c                for l=en step -1 until low do -- ..........
c     .......... changed lower bound to "low+1" and removed
c                test "if (l.eq.low) goto 100" [JMM] -- ..........
   70 do 80 l = en, low+1, -1
         s = abs(h(l-1,l-1)) + abs(h(l,l))
         if (s .eq. 0.0e0) s = norm
         tst1 = s
         tst2 = tst1 + abs(h(l,l-1))
         if (tst2 .eq. tst1) go to 100
   80 continue
c     .......... form shift ..........
  100 x = h(en,en)
      if (l .eq. en) go to 270
      y = h(na,na)
      w = h(en,na) * h(na,en)
      if (l .eq. na) go to 280
      if (itn .eq. 0) go to 1000
      if (its .ne. 10 .and. its .ne. 20) go to 130
c     .......... form exceptional shift ..........
      t = t + x
c
c"    ( prefer vector
      do 120 i = low, en
  120 h(i,i) = h(i,i) - x
c
      s = abs(h(en,na)) + abs(h(na,enm2))
      x = 0.75e0 * s
      y = x
      w = -0.4375e0 * s * s
  130 its = its + 1
      itn = itn - 1
c     .......... look for two consecutive small
c                sub-diagonal elements.
c                for m=en-2 step -1 until l do -- ..........
      do 140 m = en-2, l, -1
         zz = h(m,m)
         r = x - zz
         s = y - zz
         p = (r * s - w) / h(m+1,m) + h(m,m+1)
         q = h(m+1,m+1) - zz - r - s
         r = h(m+2,m+1)
         s = abs(p) + abs(q) + abs(r)
         p = p / s
         q = q / s
         r = r / s
         if (m .eq. l) go to 150
         tst1 = abs(p)*(abs(h(m-1,m-1)) + abs(zz) + abs(h(m+1,m+1)))
         tst2 = tst1 + abs(h(m,m-1))*(abs(q) + abs(r))
         if (tst2 .eq. tst1) go to 150
  140 continue
c
  150 h(m+2,m) = 0.0e0
c"    ( prefer vector
      do 160 i = m+3, en
         h(i,i-2) = 0.0e0
         h(i,i-3) = 0.0e0
  160 continue
c     .......... double qr step involving rows l to en and
c                columns m to en ..........
      do 260 k = m, na
         notlas = k .ne. na
         if (k .eq. m) go to 170
         p = h(k,k-1)
         q = h(k+1,k-1)
         r = 0.0e0
         if (notlas) r = h(k+2,k-1)
         x = abs(p) + abs(q) + abs(r)
         if (x .eq. 0.0e0) go to 260
         p = p / x
         q = q / x
         r = r / x
  170    s = sign(sqrt(p*p+q*q+r*r),p)
         if (k .eq. m) go to 180
         h(k,k-1) = -s * x
         go to 190
  180    if (l .ne. m) h(k,k-1) = -h(k,k-1)
  190    p = p + s
         x = p / s
         y = q / s
         zz = r / s
         q = q / p
         r = r / p
         if (notlas) go to 225
c     .......... row modification ..........
c"    ( prefer vector
         do 200 j = k, en
            foo = h(k,j) + q * h(k+1,j)
            h(k,j) = h(k,j) - foo * x
            h(k+1,j) = h(k+1,j) - foo * y
  200    continue
c
         j = min0(en,k+3)
c     .......... column modification ..........
         do 210 i = l, j
            foo = x * h(i,k) + y * h(i,k+1)
            h(i,k) = h(i,k) - foo
            h(i,k+1) = h(i,k+1) - foo * q
  210    continue
         go to 255
  225    continue
c     .......... row modification ..........
c"    ( prefer vector
         do 230 j = k, en
            foo = h(k,j) + q * h(k+1,j) + r * h(k+2,j)
            h(k,j) = h(k,j) - foo * x
            h(k+1,j) = h(k+1,j) - foo * y
            h(k+2,j) = h(k+2,j) - foo * zz
  230    continue
c
         j = min0(en,k+3)
c     .......... column modification ..........
         do 240 i = l, j
            foo = x * h(i,k) + y * h(i,k+1) + zz * h(i,k+2)
            h(i,k) = h(i,k) - foo
            h(i,k+1) = h(i,k+1) - foo * q
            h(i,k+2) = h(i,k+2) - foo * r
  240    continue
  255    continue
c
  260 continue
c
      go to 70
c     .......... one root found ..........
  270 wr(en) = x + t
      wi(en) = 0.0e0
      en = na
      go to 60
c     .......... two roots found ..........
  280 p = (y - x) / 2.0e0
      q = p * p + w
      zz = sqrt(abs(q))
      x = x + t
      if (q .lt. 0.0e0) go to 320
c     .......... real pair ..........
      zz = p + sign(zz,p)
      wr(na) = x + zz
      wr(en) = wr(na)
      if (zz .ne. 0.0e0) wr(en) = x - w / zz
      wi(na) = 0.0e0
      wi(en) = 0.0e0
      go to 330
c     .......... complex pair ..........
  320 wr(na) = x + p
      wr(en) = x + p
      wi(na) = zz
      wi(en) = -zz
  330 en = enm2
      go to 60
c     .......... set error -- all eigenvalues have not
c                converged after 30*n iterations ..........
 1000 ierr = en
 1001 return
      end
