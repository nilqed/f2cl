@process directive('"    (')
      subroutine comqr2(nm,n,low,igh,ortr,orti,hr,hi,wr,wi,zr,zi,ierr)
c
      integer i,j,k,l,m,n,en,nm,nn,igh,
     x        itn,its,low,lp1,enm1,iend,ierr
      real hr(nm,n),hi(nm,n),wr(n),wi(n),zr(nm,n),zi(nm,n),
     x       ortr(igh),orti(igh)
      real si,sr,ti,tr,xi,xr,yi,yr,zzi,zzr,norm,tst1,tst2,
     x       foo,foo2,tti,ttr
c
c     this subroutine is a translation of a unitary analogue of the
c     algol procedure  comlr2, num. math. 16, 181-204(1970) by peters
c     and wilkinson.
c     handbook for auto. comp., vol.ii-linear algebra, 372-395(1971).
c     the unitary analogue substitutes the qr algorithm of francis
c     (comp. jour. 4, 332-345(1962)) for the lr algorithm.
c
c     this subroutine finds the eigenvalues and eigenvectors
c     of a complex upper hessenberg matrix by the qr
c     method.  the eigenvectors of a complex general matrix
c     can also be found if  corth  has been used to reduce
c     this general matrix to hessenberg form.
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
c          subroutine  cbal.  if  cbal  has not been used,
c          set low=1, igh=n.
c
c        ortr and orti contain information about the unitary trans-
c          formations used in the reduction by  corth, if performed.
c          only elements low through igh are used.  if the eigenvectors
c          of the hessenberg matrix are desired, set ortr(j) and
c          orti(j) to 0.0d0 for these elements.
c
c        hr and hi contain the real and imaginary parts,
c          respectively, of the complex upper hessenberg matrix.
c          their lower triangles below the subdiagonal contain further
c          information about the transformations which were used in the
c          reduction by  corth, if performed.  if the eigenvectors of
c          the hessenberg matrix are desired, these elements may be
c          arbitrary.
c
c     on output
c
c        ortr, orti, and the upper hessenberg portions of hr and hi
c          have been destroyed.
c
c        wr and wi contain the real and imaginary parts,
c          respectively, of the eigenvalues.  if an error
c          exit is made, the eigenvalues should be correct
c          for indices ierr+1,...,n.
c
c        zr and zi contain the real and imaginary parts,
c          respectively, of the eigenvectors.  the eigenvectors
c          are unnormalized.  if an error exit is made, none of
c          the eigenvectors has been found.
c
c        ierr is set to
c          zero       for normal return,
c          j          if the limit of 30*n iterations is exhausted
c                     while the j-th eigenvalue is being sought.
c
c     calls cdiv for complex division.
c     calls csroot for complex square root.
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
c     .......... initialize eigenvector matrix ..........
      do 101 j = 1, n
c
         do 100 i = 1, n
            zr(i,j) = 0.0e0
            zi(i,j) = 0.0e0
  100    continue
         zr(j,j) = 1.0e0
  101 continue
c     .......... form the matrix of accumulated transformations
c                from the information left by corth ..........
      iend = igh - low - 1
      if (iend) 180, 150, 105
c     .......... for i=igh-1 step -1 until low+1 do -- ..........
  105 do 140 i = igh-1, low+1, -1
         if (ortr(i) .eq. 0.0e0 .and. orti(i) .eq. 0.0e0) go to 140
         if (hr(i,i-1) .eq. 0.0e0 .and. hi(i,i-1) .eq. 0.0e0) go to 140
c     .......... norm below is negative of h formed in corth ..........
         norm = hr(i,i-1) * ortr(i) + hi(i,i-1) * orti(i)
c
         do 110 k = i+1, igh
            ortr(k) = hr(k,i-1)
            orti(k) = hi(k,i-1)
  110    continue
c
         do 130 j = i, igh
            sr = 0.0e0
            si = 0.0e0
c
            do 115 k = i, igh
               sr = sr + ortr(k) * zr(k,j) + orti(k) * zi(k,j)
               si = si + ortr(k) * zi(k,j) - orti(k) * zr(k,j)
  115       continue
c
            sr = sr / norm
            si = si / norm
c
            do 120 k = i, igh
               zr(k,j) = zr(k,j) + sr * ortr(k) - si * orti(k)
               zi(k,j) = zi(k,j) + sr * orti(k) + si * ortr(k)
  120       continue
c
  130    continue
c
  140 continue
c     .......... create real subdiagonal elements ..........
  150 l = low + 1
c
      do 170 i = l, igh
         if (hi(i,i-1) .eq. 0.0e0) go to 170
c--         norm = pythag(hr(i,i-1),hi(i,i-1))
         if (abs(hr(i,i-1)).gt.abs(hi(i,i-1))) then
            norm = abs(hr(i,i-1))*sqrt(1e0+(hi(i,i-1)/hr(i,i-1))**2)
         else if (hi(i,i-1).ne.0e0) then
            norm = abs(hi(i,i-1))*sqrt((hr(i,i-1)/hi(i,i-1))**2+1e0)
         else
            norm = abs(hr(i,i-1))
         endif
         yr = hr(i,i-1) / norm
         yi = hi(i,i-1) / norm
         hr(i,i-1) = norm
         hi(i,i-1) = 0.0e0
c
c"    ( prefer vector
         do 155 j = i, n
            si = yr * hi(i,j) - yi * hr(i,j)
            hr(i,j) = yr * hr(i,j) + yi * hi(i,j)
            hi(i,j) = si
  155    continue
c
         do 160 j = 1, min0(i+1,igh)
            si = yr * hi(j,i) + yi * hr(j,i)
            hr(j,i) = yr * hr(j,i) - yi * hi(j,i)
            hi(j,i) = si
  160    continue
c
         do 165 j = low, igh
            si = yr * zi(j,i) + yi * zr(j,i)
            zr(j,i) = yr * zr(j,i) - yi * zi(j,i)
            zi(j,i) = si
  165    continue
c
  170 continue
c     .......... store roots isolated by cbal ..........
  180 do 200 i = 1, n
         if (i .lt. low .or. i .gt. igh) wr(i) = hr(i,i)
         if (i .lt. low .or. i .gt. igh) wi(i) = hi(i,i)
  200 continue
c
      en = igh
      tr = 0.0e0
      ti = 0.0e0
      itn = 30*n
c     .......... search for next eigenvalue ..........
  220 if (en .lt. low) go to 680
      its = 0
      enm1 = en - 1
c     .......... look for single small sub-diagonal element
c                for l=en step -1 until low do -- ..........
  240 do 260 l = en, low, -1
         if (l .eq. low) go to 300
         tst1 = abs(hr(l-1,l-1)) + abs(hi(l-1,l-1))
     x            + abs(hr(l,l)) + abs(hi(l,l))
         tst2 = tst1 + abs(hr(l,l-1))
         if (tst2 .eq. tst1) go to 300
  260 continue
c     .......... form shift ..........
  300 if (l .eq. en) go to 660
      if (itn .eq. 0) go to 1000
      if (its .eq. 10 .or. its .eq. 20) go to 320
      sr = hr(en,en)
      si = hi(en,en)
      xr = hr(enm1,en) * hr(en,enm1)
      xi = hi(enm1,en) * hr(en,enm1)
      if (xr .eq. 0.0e0 .and. xi .eq. 0.0e0) go to 340
      yr = (hr(enm1,enm1) - sr) / 2.0e0
      yi = (hi(enm1,enm1) - si) / 2.0e0
      call csroot(yr**2-yi**2+xr,2.0e0*yr*yi+xi,zzr,zzi)
      if (yr * zzr + yi * zzi .ge. 0.0e0) go to 310
      zzr = -zzr
      zzi = -zzi
  310 call cdiv(xr,xi,yr+zzr,yi+zzi,xr,xi)
      sr = sr - xr
      si = si - xi
      go to 340
c     .......... form exceptional shift ..........
  320 sr = abs(hr(en,enm1)) + abs(hr(enm1,en-2))
      si = 0.0e0
c
c"    ( prefer vector
  340 do 360 i = low, en
         hr(i,i) = hr(i,i) - sr
         hi(i,i) = hi(i,i) - si
  360 continue
c
      tr = tr + sr
      ti = ti + si
      its = its + 1
      itn = itn - 1
c     .......... reduce to triangle (rows) ..........
      lp1 = l + 1
c
      do 500 i = lp1, en
         sr = hr(i,i-1)
         hr(i,i-1) = 0.0e0
c--         foo2 = pythag(hr(i-1,i-1),hi(i-1,i-1))
         if (abs(hr(i-1,i-1)).gt.abs(hi(i-1,i-1))) then
          foo2=abs(hr(i-1,i-1))*sqrt(1e0+(hi(i-1,i-1)/hr(i-1,i-1))**2)
         else if (hi(i-1,i-1).ne.0e0) then
          foo2=abs(hi(i-1,i-1))*sqrt((hr(i-1,i-1)/hi(i-1,i-1))**2+1e0)
         else
            foo2 = abs(hr(i-1,i-1))
         endif
c--         norm = pythag(foo2,sr)
         if (abs(foo2).gt.abs(sr)) then
            norm = abs(foo2)*sqrt(1e0+(sr/foo2)**2)
         else if (sr.ne.0e0) then
            norm = abs(sr)*sqrt((foo2/sr)**2+1e0)
         else
            norm = abs(foo2)
         endif
         xr = hr(i-1,i-1) / norm
         wr(i-1) = xr
         xi = hi(i-1,i-1) / norm
         wi(i-1) = xi
         hr(i-1,i-1) = norm
         hi(i-1,i-1) = 0.0e0
         hi(i,i-1) = sr / norm
c
         foo = hi(i,i-1)
c
c"    ( prefer vector
         do 490 j = i, n
            yr = hr(i-1,j)
            yi = hi(i-1,j)
            ttr = hr(i,j)
            tti = hi(i,j)
            hr(i-1,j) = xr * yr + xi * yi + foo * ttr
            hi(i-1,j) = xr * yi - xi * yr + foo * tti
            hr(i,j) = xr * ttr - xi * tti - foo * yr
            hi(i,j) = xr * tti + xi * ttr - foo * yi
  490    continue
c
  500 continue
c
      si = hi(en,en)
      if (si .eq. 0.0e0) go to 540
c--      norm = pythag(hr(en,en),si)
      if (abs(hr(en,en)).gt.abs(si)) then
         norm = abs(hr(en,en))*sqrt(1e0+(si/hr(en,en))**2)
      else if (si.ne.0e0) then
         norm = abs(si)*sqrt((hr(en,en)/si)**2+1e0)
      else
         norm = abs(hr(en,en))
      endif
      sr = hr(en,en) / norm
      si = si / norm
      hr(en,en) = norm
      hi(en,en) = 0.0e0
      if (en .eq. n) go to 540
c"    ( prefer vector
      do 520 j = en+1, n
         yr = hr(en,j)
         yi = hi(en,j)
         hr(en,j) = sr * yr + si * yi
         hi(en,j) = sr * yi - si * yr
  520 continue
c     .......... inverse operation (columns) ..........
  540 do 600 j = lp1, en
         xr = wr(j-1)
         xi = wi(j-1)
c
         do 580 i = 1, j
            yr = hr(i,j-1)
            yi = 0.0e0
            ttr = hr(i,j)
            tti = hi(i,j)
            if (i .ne. j) yi = hi(i,j-1)
            if (i .ne. j) hi(i,j-1) = xr*yi + xi*yr + hi(j,j-1)*tti
            hr(i,j-1) = xr * yr - xi * yi + hi(j,j-1) * ttr
            hr(i,j) = xr * ttr + xi * tti - hi(j,j-1) * yr
            hi(i,j) = xr * tti - xi * ttr - hi(j,j-1) * yi
  580    continue
c
         do 590 i = low, igh
            yr = zr(i,j-1)
            yi = zi(i,j-1)
            ttr = zr(i,j)
            tti = zi(i,j)
            zr(i,j-1) = xr * yr - xi * yi + hi(j,j-1) * ttr
            zi(i,j-1) = xr * yi + xi * yr + hi(j,j-1) * tti
            zr(i,j) = xr * ttr + xi * tti - hi(j,j-1) * yr
            zi(i,j) = xr * tti - xi * ttr - hi(j,j-1) * yi
  590    continue
c
  600 continue
c
      if (si .eq. 0.0e0) go to 240
c
      do 630 i = 1, en
         yr = hr(i,en)
         yi = hi(i,en)
         hr(i,en) = sr * yr - si * yi
         hi(i,en) = sr * yi + si * yr
  630 continue
c
      do 640 i = low, igh
         yr = zr(i,en)
         yi = zi(i,en)
         zr(i,en) = sr * yr - si * yi
         zi(i,en) = sr * yi + si * yr
  640 continue
c
      go to 240
c     .......... a root found ..........
  660 hr(en,en) = hr(en,en) + tr
      wr(en) = hr(en,en)
      hi(en,en) = hi(en,en) + ti
      wi(en) = hi(en,en)
      en = enm1
      go to 220
c     .......... all roots found.  backsubstitute to find
c                vectors of upper triangular form ..........
  680 norm = 0.0e0
c
      do 720 j = 1, n
         do 720 i = 1, j
            norm = amax1(norm,abs(hr(i,j)) + abs(hi(i,j)))
  720 continue
c
      if (n .eq. 1 .or. norm .eq. 0.0e0) go to 1001
c     .......... for en=n step -1 until 2 do -- ..........
      do 800 nn = 2, n
         en = n + 2 - nn
         xr = wr(en)
         xi = wi(en)
         hr(en,en) = 1.0e0
         hi(en,en) = 0.0e0
         enm1 = en - 1
c     .......... for i=en-1 step -1 until 1 do -- ..........
         do 780 i = en-1, 1, -1
            ttr = 0.0e0
            tti = 0.0e0
c
            do 740 j = i+1, en
               ttr = ttr + hr(i,j) * hr(j,en) - hi(i,j) * hi(j,en)
               tti = tti + hr(i,j) * hi(j,en) + hi(i,j) * hr(j,en)
  740       continue
c
            yr = xr - wr(i)
            yi = xi - wi(i)
            if (yr .ne. 0.0e0 .or. yi .ne. 0.0e0) go to 765
               tst1 = norm
               yr = tst1
  760          yr = 0.01e0 * yr
               tst2 = norm + yr
               if (tst2 .gt. tst1) go to 760
  765       continue
            call cdiv(ttr,tti,yr,yi,hr(i,en),hi(i,en))
c     .......... overflow control ..........
            tr = abs(hr(i,en)) + abs(hi(i,en))
            if (tr .eq. 0.0e0) go to 780
            tst1 = tr
            tst2 = tst1 + 1.0e0/tst1
            if (tst2 .gt. tst1) go to 780
            do 770 j = i, en
               hr(j,en) = hr(j,en)/tr
               hi(j,en) = hi(j,en)/tr
  770       continue
c
  780    continue
c
  800 continue
c     .......... end backsubstitution ..........
      enm1 = n - 1
c     .......... vectors of isolated roots ..........
      do  840 i = 1, n
         if (i .ge. low .and. i .le. igh) go to 840
         do 820 j = i, n
            zr(i,j) = hr(i,j)
            zi(i,j) = hi(i,j)
  820    continue
c
  840 continue
c     .......... multiply by transformation matrix to give
c                vectors of original full matrix.
c                for j=n step -1 until low do -- ..........
      do 880 j = n, low, -1
         m = min0(j,igh)
c
         do 880 i = low, igh
            ttr = 0.0e0
            tti = 0.0e0
c
            do 860 k = low, m
                    ttr = ttr + zr(i,k) * hr(k,j) - zi(i,k) * hi(k,j)
                    tti = tti + zr(i,k) * hi(k,j) + zi(i,k) * hr(k,j)
  860       continue
c
            zr(i,j) = ttr
            zi(i,j) = tti
  880 continue
c
      go to 1001
c     .......... set error -- all eigenvalues have not
c                converged after 30*n iterations ..........
 1000 ierr = en
 1001 return
      end
