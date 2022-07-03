@process directive('"    (')
      subroutine minfit(nm,m,n,a,w,ip,b,ierr,rv1)
c
      integer i,j,k,l,m,n,ip,i1,l1,nm,its,ierr
      real a(nm,n),w(n),b(nm,ip),rv1(n)
      real c,f,g,h,s,x,y,z,tst1,tst2,scale
c
c     this subroutine is a translation of the algol procedure minfit,
c     num. math. 14, 403-420(1970) by golub and reinsch.
c     handbook for auto. comp., vol ii-linear algebra, 134-151(1971).
c
c     this subroutine determines, towards the solution of the linear
c                                                        t
c     system ax=b, the singular value decomposition a=usv  of a real
c                                         t
c     m by n rectangular matrix, forming u b rather than u.  householder
c     bidiagonalization and a variant of the qr algorithm are used.
c
c     on input
c
c        nm must be set to the row dimension of two-dimensional
c          array parameters as declared in the calling program
c          dimension statement.  note that nm must be at least
c          as large as the maximum of m and n.
c
c        m is the number of rows of a and b.
c
c        n is the number of columns of a and the order of v.
c
c        a contains the rectangular coefficient matrix of the system.
c
c        ip is the number of columns of b.  ip can be zero.
c
c        b contains the constant column matrix of the system
c          if ip is not zero.  otherwise b is not referenced.
c
c     on output
c
c        a has been overwritten by the matrix v (orthogonal) of the
c          decomposition in its first n rows and columns.  if an
c          error exit is made, the columns of v corresponding to
c          indices of correct singular values should be correct.
c
c        w contains the n (non-negative) singular values of a (the
c          diagonal elements of s).  they are unordered.  if an
c          error exit is made, the singular values should be correct
c          for indices ierr+1,ierr+2,...,n.
c
c                                   t
c        b has been overwritten by u b.  if an error exit is made,
c                       t
c          the rows of u b corresponding to indices of correct
c          singular values should be correct.
c
c        ierr is set to
c          zero       for normal return,
c          k          if the k-th singular value has not been
c                     determined after 30 iterations.
c
c        rv1 is a temporary storage array.
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
c     .......... householder reduction to bidiagonal form ..........
      g = 0.0e0
      scale = 0.0e0
      x = 0.0e0
c
      do 300 i = 1, n
         l = i + 1
         rv1(i) = scale * g
         g = 0.0e0
         s = 0.0e0
         scale = 0.0e0
c
         do 120 k = i, m
  120    scale = scale + abs(a(k,i))
c
         if (scale .eq. 0.0e0) go to 210
c
         do 130 k = i, m
            a(k,i) = a(k,i) / scale
            s = s + a(k,i)**2
  130    continue
c
         f = a(i,i)
         g = -sign(sqrt(s),f)
         h = f * g - s
         a(i,i) = f - g
c"    ( ignore recrdeps
         do 150 j = l, n
            s = 0.0e0
c
            do 140 k = i, m
  140       s = s + a(k,i) * a(k,j)
c
            f = s / h
c
            do 150 k = i, m
               a(k,j) = a(k,j) + f * a(k,i)
  150    continue
c
         do 180 j = 1, ip
            s = 0.0e0
c
            do 170 k = i, m
  170       s = s + a(k,i) * b(k,j)
c
            f = s / h
c
            do 180 k = i, m
               b(k,j) = b(k,j) + f * a(k,i)
  180    continue
c
         do 200 k = i, m
  200    a(k,i) = scale * a(k,i)
c
  210    w(i) = scale * g
         g = 0.0e0
         s = 0.0e0
         scale = 0.0e0
         if (i .gt. m .or. i .eq. n) go to 290
c
c"    ( prefer vector
         do 220 k = l, n
  220    scale = scale + abs(a(i,k))
c
         if (scale .eq. 0.0e0) go to 290
c
         do 230 k = l, n
            a(i,k) = a(i,k) / scale
            s = s + a(i,k)**2
  230    continue
c
         f = a(i,l)
         g = -sign(sqrt(s),f)
         h = f * g - s
         a(i,l) = f - g
c
         do 240 k = l, n
  240    rv1(k) = a(i,k) / h
c
c"    ( ignore recrdeps
         do 260 j = l, m
            s = 0.0e0
c
            do 250 k = l, n
  250       s = s + a(j,k) * a(i,k)
c
            do 260 k = l, n
               a(j,k) = a(j,k) + s * rv1(k)
  260    continue
c
c"    ( prefer vector
         do 280 k = l, n
  280    a(i,k) = scale * a(i,k)
c
  290    x = amax1(x,abs(w(i))+abs(rv1(i)))
  300 continue
c     .......... accumulation of right-hand transformations.
c                for i=n step -1 until 1 do -- ..........
      do 400 i = n, 1, -1
         if (i .eq. n) go to 390
         if (g .eq. 0.0e0) go to 360
c
         do 320 j = l, n
c     .......... double division avoids possible underflow ..........
  320    a(j,i) = (a(i,j) / a(i,l)) / g
c
c"    ( ignore recrdeps
         do 350 j = l, n
            s = 0.0e0
c
            do 340 k = l, n
  340       s = s + a(i,k) * a(k,j)
c
            do 350 k = l, n
               a(k,j) = a(k,j) + s * a(k,i)
  350    continue
c
c"    ( ignore recrdeps
c"    ( prefer vector
  360    do 380 j = l, n
            a(i,j) = 0.0e0
            a(j,i) = 0.0e0
  380    continue
c
  390    a(i,i) = 1.0e0
         g = rv1(i)
         l = i
  400 continue
c
      do 500 j = 1, ip
         do 500 i = m+1, n
            b(i,j) = 0.0e0
  500 continue
c     .......... diagonalization of the bidiagonal form ..........
      tst1 = x
c     .......... for k=n step -1 until 1 do -- ..........
      do 700 k = n, 1, -1
         its = 0
c     .......... test for splitting.
c                for l=k step -1 until 1 do -- ..........
  520    do 530 l = k, 1, -1
            l1 = l - 1
            tst2 = tst1 + abs(rv1(l))
            if (tst2 .eq. tst1) go to 565
c     .......... rv1(1) is always zero, so there is no exit
c                through the bottom of the loop ..........
            tst2 = tst1 + abs(w(l1))
            if (tst2 .eq. tst1) go to 540
  530    continue
c     .......... cancellation of rv1(l) if l greater than 1 ..........
  540    c = 0.0e0
         s = 1.0e0
c
         do 560 i = l, k
            f = s * rv1(i)
            rv1(i) = c * rv1(i)
            tst2 = tst1 + abs(f)
            if (tst2 .eq. tst1) go to 565
            g = w(i)
c--            h = pythag(f,g)
            if (abs(f).gt.abs(g)) then
               h = abs(f)*sqrt(1e0+(g/f)**2)
            else if (g.ne.0e0) then
               h = abs(g)*sqrt((f/g)**2+1e0)
            else
               h = abs(f)
            endif
            w(i) = h
            c = g / h
            s = -f / h
c"    ( prefer vector
            do 550 j = 1, ip
               y = b(l1,j)
               z = b(i,j)
               b(l1,j) = y * c + z * s
               b(i,j) = -y * s + z * c
  550       continue
c
  560    continue
c     .......... test for convergence ..........
  565    z = w(k)
         if (l .eq. k) go to 650
c     .......... shift from bottom 2 by 2 minor ..........
         if (its .eq. 30) go to 1000
         its = its + 1
         x = w(l)
         y = w(k-1)
         g = rv1(k-1)
         h = rv1(k)
         f = 0.5e0 * (((g + z) / h) * ((g - z) / y) + y / h - h / y)
c**         g = pythag(f,1.0d0)
         if (abs(f).gt.1.0e0) then
            g = abs(f)*sqrt(1e0+(1.0e0/f)**2)
         else
            g = sqrt(f**2+1e0)
         endif
         f = x - (z / x) * z + (h / x) * (y / (f + sign(g,f)) - h)
c     .......... next qr transformation ..........
         c = 1.0e0
         s = 1.0e0
c
         do 600 i1 = l, k-1
            i = i1 + 1
            g = rv1(i)
            y = w(i)
            h = s * g
            g = c * g
c--            z = pythag(f,h)
            if (abs(f).gt.abs(h)) then
               z = abs(f)*sqrt(1e0+(h/f)**2)
            else if (h.ne.0e0) then
               z = abs(h)*sqrt((f/h)**2+1e0)
            else
               z = abs(f)
            endif
            rv1(i1) = z
            c = f / z
            s = h / z
            f = x * c + g * s
            g = -x * s + g * c
            h = y * s
            y = y * c
c
            do 570 j = 1, n
               x = a(j,i1)
               z = a(j,i)
               a(j,i1) = x * c + z * s
               a(j,i) = -x * s + z * c
  570       continue
c
c--            z = pythag(f,h)
            if (abs(f).gt.abs(h)) then
               z = abs(f)*sqrt(1e0+(h/f)**2)
            else if (h.ne.0e0) then
               z = abs(h)*sqrt((f/h)**2+1e0)
            else
               z = abs(f)
            endif
            w(i1) = z
c     .......... rotation can be arbitrary if z is zero ..........
            if (z .eq. 0.0e0) go to 580
            c = f / z
            s = h / z
  580       f = c * g + s * y
            x = -s * g + c * y
            do 590 j = 1, ip
               y = b(i1,j)
               z = b(i,j)
               b(i1,j) = y * c + z * s
               b(i,j) = -y * s + z * c
  590       continue
c
  600    continue
c
         rv1(l) = 0.0e0
         rv1(k) = f
         w(k) = x
         go to 520
c     .......... convergence ..........
  650    if (z .ge. 0.0e0) go to 700
c     .......... w(k) is made non-negative ..........
         w(k) = -z
c
         do 690 j = 1, n
  690    a(j,k) = -a(j,k)
c
  700 continue
c
      go to 1001
c     .......... set error -- no convergence to a
c                singular value after 30 iterations ..........
 1000 ierr = k
 1001 return
      end
