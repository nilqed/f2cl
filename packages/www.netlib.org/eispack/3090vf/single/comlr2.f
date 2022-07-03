@process directive('"    (')
      subroutine comlr2(nm,n,low,igh,int,hr,hi,wr,wi,zr,zi,ierr)
c
      integer i,j,k,l,m,n,en,nm,nn,igh,im1,
     x        itn,its,low,mp1,enm1,ierr
      real hr(nm,n),hi(nm,n),wr(n),wi(n),zr(nm,n),zi(nm,n)
      real si,sr,ti,tr,xi,xr,yi,yr,zzi,zzr,xnorm,tst1,tst2
      real tti,ttr
      integer int(igh)
c
c     this subroutine is a translation of the algol procedure comlr2,
c     num. math. 16, 181-204(1970) by peters and wilkinson.
c     handbook for auto. comp., vol.ii-linear algebra, 372-395(1971).
c
c     this subroutine finds the eigenvalues and eigenvectors
c     of a complex upper hessenberg matrix by the modified lr
c     method.  the eigenvectors of a complex general matrix
c     can also be found if  comhes  has been used to reduce
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
c        int contains information on the rows and columns interchanged
c          in the reduction by  comhes, if performed.  only elements
c          low through igh are used.  if the eigenvectors of the hessen-
c          berg matrix are desired, set int(j)=j for these elements.
c
c        hr and hi contain the real and imaginary parts,
c          respectively, of the complex upper hessenberg matrix.
c          their lower triangles below the subdiagonal contain the
c          multipliers which were used in the reduction by  comhes,
c          if performed.  if the eigenvectors of the hessenberg
c          matrix are desired, these elements must be set to zero.
c
c     on output
c
c        the upper hessenberg portions of hr and hi have been
c          destroyed, but the location hr(1,1) contains the norm
c          of the triangularized matrix.
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
      do 110 j = 1, n
         do 100 i = 1, n
            zr(i,j) = 0.0e0
            zi(i,j) = 0.0e0
  100    continue
         zr(j,j) = 1.0e0
  110 continue
c     .......... form the matrix of accumulated transformations
c                from the information left by comhes ..........
c     .......... for i=igh-1 step -1 until low+1 do -- ..........
      do 160 i = igh-1, low+1, -1
         do 120 k = i+1, igh
            zr(k,i) = hr(k,i-1)
            zi(k,i) = hi(k,i-1)
  120    continue
c
         j = int(i)
c
         do 140 k = i, igh
            zr(i,k) = zr(j,k)
            zi(i,k) = zi(j,k)
  140    continue

c"    ( prefer vector
         do 150 k = i, igh
            zr(j,k) = 0.0e0
            zi(j,k) = 0.0e0
  150    continue
c
         zr(j,i) = 1.0e0
  160 continue
c     .......... store roots isolated by cbal ..........
      do 200 i = 1, n
         if (i .ge. low .and. i .le. igh) go to 200
         wr(i) = hr(i,i)
         wi(i) = hi(i,i)
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
         tst2 = tst1 + abs(hr(l,l-1)) + abs(hi(l,l-1))
         if (tst2 .eq. tst1) go to 300
  260 continue
c     .......... form shift ..........
  300 if (l .eq. en) go to 660
      if (itn .eq. 0) go to 1000
      if (its .eq. 10 .or. its .eq. 20) go to 320
      sr = hr(en,en)
      si = hi(en,en)
      xr = hr(enm1,en) * hr(en,enm1) - hi(enm1,en) * hi(en,enm1)
      xi = hr(enm1,en) * hi(en,enm1) + hi(enm1,en) * hr(en,enm1)
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
      si = abs(hi(en,enm1)) + abs(hi(enm1,en-2))
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
c     .......... look for two consecutive small
c                sub-diagonal elements ..........
      xr = abs(hr(enm1,enm1)) + abs(hi(enm1,enm1))
      yr = abs(hr(en,enm1)) + abs(hi(en,enm1))
      zzr = abs(hr(en,en)) + abs(hi(en,en))
c     .......... for m=en-1 step -1 until l do -- ..........
      do 380 m = en-1, l, -1
         if (m .eq. l) go to 420
         yi = yr
         yr = abs(hr(m,m-1)) + abs(hi(m,m-1))
         xi = zzr
         zzr = xr
         xr = abs(hr(m-1,m-1)) + abs(hi(m-1,m-1))
         tst1 = zzr / yi * (zzr + xr + xi)
         tst2 = tst1 + yr
         if (tst2 .eq. tst1) go to 420
  380 continue
c     .......... triangular decomposition h=l*r ..........
  420 mp1 = m + 1
c
      do 520 i = mp1, en
         im1 = i - 1
         xr = hr(im1,im1)
         xi = hi(im1,im1)
         yr = hr(i,im1)
         yi = hi(i,im1)
         if (abs(xr) + abs(xi) .ge. abs(yr) + abs(yi)) go to 460
c     .......... interchange rows of hr and hi ..........
c"    ( prefer vector
         do 440 j = im1, n
            ttr = hr(im1,j)
            hr(im1,j) = hr(i,j)
            hr(i,j) = ttr
            tti = hi(im1,j)
            hi(im1,j) = hi(i,j)
            hi(i,j) = tti
  440    continue
c
         call cdiv(xr,xi,yr,yi,zzr,zzi)
         wr(i) = 1.0e0
         go to 480
  460    call cdiv(yr,yi,xr,xi,zzr,zzi)
         wr(i) = -1.0e0
  480    hr(i,im1) = zzr
         hi(i,im1) = zzi
c
c"    ( prefer vector
         do 500 j = i, n
            hr(i,j) = hr(i,j) - zzr * hr(im1,j) + zzi * hi(im1,j)
            hi(i,j) = hi(i,j) - zzr * hi(im1,j) - zzi * hr(im1,j)
  500    continue
c
  520 continue
c     .......... composition r*l=h ..........
      do 640 j = mp1, en
         xr = hr(j,j-1)
         xi = hi(j,j-1)
         hr(j,j-1) = 0.0e0
         hi(j,j-1) = 0.0e0
c     .......... interchange columns of hr, hi, zr, and zi,
c                if necessary ..........
         if (wr(j) .le. 0.0e0) go to 580
c
         do 540 i = 1, j
            ttr = hr(i,j-1)
            hr(i,j-1) = hr(i,j)
            hr(i,j) = ttr
            tti = hi(i,j-1)
            hi(i,j-1) = hi(i,j)
            hi(i,j) = tti
  540    continue
c
         do 560 i = low, igh
            ttr = zr(i,j-1)
            zr(i,j-1) = zr(i,j)
            zr(i,j) = ttr
            tti = zi(i,j-1)
            zi(i,j-1) = zi(i,j)
            zi(i,j) = tti
  560    continue
c
  580    do 600 i = 1, j
            hr(i,j-1) = hr(i,j-1) + xr * hr(i,j) - xi * hi(i,j)
            hi(i,j-1) = hi(i,j-1) + xr * hi(i,j) + xi * hr(i,j)
  600    continue
c     .......... accumulate transformations ..........
         do 620 i = low, igh
            zr(i,j-1) = zr(i,j-1) + xr * zr(i,j) - xi * zi(i,j)
            zi(i,j-1) = zi(i,j-1) + xr * zi(i,j) + xi * zr(i,j)
  620    continue
c
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
  680 xnorm = 0.0e0
c
      do 720 i = 1, n
         do 720 j = i, n
            xnorm = amax1(xnorm, abs(hr(i,j)) + abs(hi(i,j)))
  720 continue
c
      hr(1,1) = xnorm
      if (n .eq. 1 .or. xnorm .eq. 0.0e0) go to 1001
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
            zzr = 0.0e0
            zzi = 0.0e0
c"    ( prefer vector
            do 740 j = i+1, en
               zzr = zzr + hr(i,j) * hr(j,en) - hi(i,j) * hi(j,en)
               zzi = zzi + hr(i,j) * hi(j,en) + hi(i,j) * hr(j,en)
  740       continue
c
            yr = xr - wr(i)
            yi = xi - wi(i)
            if (yr .ne. 0.0e0 .or. yi .ne. 0.0e0) go to 765
               tst1 = xnorm
               yr = tst1
  760          yr = 0.01e0 * yr
               tst2 = xnorm + yr
               if (tst2 .gt. tst1) go to 760
  765       continue
            call cdiv(zzr,zzi,yr,yi,hr(i,en),hi(i,en))
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
