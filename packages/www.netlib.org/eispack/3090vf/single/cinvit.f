@process directive('"    (')
      subroutine cinvit(nm,n,ar,ai,wr,wi,select,mm,m,zr,zi,
     x                  ierr,rm1,rm2,rv1,rv2)
c
      integer i,j,k,m,n,s,mm,mp,nm,uk,its,km1,ierr
      real ar(nm,n),ai(nm,n),wr(n),wi(n),zr(nm,mm),
     x       zi(nm,mm),rm1(n,n),rm2(n,n),rv1(n),rv2(n)
      real x,y,eps3,norm,normv,epslon,growto,ilambd,pythag,
     x       rlambd,ukroot,tmp,foo
      logical select(n)
c
c     this subroutine is a translation of the algol procedure cx invit
c     by peters and wilkinson.
c     handbook for auto. comp. vol.ii-linear algebra, 418-439(1971).
c
c     this subroutine finds those eigenvectors of a complex upper
c     hessenberg matrix corresponding to specified eigenvalues,
c     using inverse iteration.
c
c     on input
c
c        nm must be set to the row dimension of two-dimensional
c          array parameters as declared in the calling program
c          dimension statement.
c
c        n is the order of the matrix.
c
c        ar and ai contain the real and imaginary parts,
c          respectively, of the hessenberg matrix.
c
c        wr and wi contain the real and imaginary parts, respectively,
c          of the eigenvalues of the matrix.  the eigenvalues must be
c          stored in a manner identical to that of subroutine  comlr,
c          which recognizes possible splitting of the matrix.
c
c        select specifies the eigenvectors to be found.  the
c          eigenvector corresponding to the j-th eigenvalue is
c          specified by setting select(j) to .true..
c
c        mm should be set to an upper bound for the number of
c          eigenvectors to be found.
c
c     on output
c
c        ar, ai, wi, and select are unaltered.
c
c        wr may have been altered since close eigenvalues are perturbed
c          slightly in searching for independent eigenvectors.
c
c        m is the number of eigenvectors actually found.
c
c        zr and zi contain the real and imaginary parts, respectively,
c          of the eigenvectors.  the eigenvectors are normalized
c          so that the component of largest magnitude is 1.
c          any vector which fails the acceptance test is set to zero.
c
c        ierr is set to
c          zero       for normal return,
c          -(2*n+1)   if more than mm eigenvectors have been specified,
c          -k         if the iteration corresponding to the k-th
c                     value fails,
c          -(n+k)     if both error situations occur.
c
c        rm1, rm2, rv1, and rv2 are temporary storage arrays.
c
c     the algol procedure guessvec appears in cinvit in line.
c
c     calls cdiv for complex division.
c     calls pythag for  dsqrt(a*a + b*b) .
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
      uk = 0
      s = 1
c
      do 980 k = 1, n
         if (.not. select(k)) go to 980
         if (s .gt. mm) go to 1000
         if (uk .ge. k) go to 200
c     .......... check for possible splitting ..........
         do 120 uk = k, n
            if (uk .eq. n) go to 140
            if (ar(uk+1,uk) .eq. 0.0e0 .and. ai(uk+1,uk) .eq. 0.0e0)
     x         go to 140
  120    continue
c     .......... compute infinity norm of leading uk by uk
c                (hessenberg) matrix ..........
  140    norm = 0.0e0
         do 180 i = 1, uk
            x = 0.0e0
c
            do 160 j = max0(1,i-1), uk
c--            tmp = pythag(ar(i,j),ai(i,j))
            if (abs(ar(i,j)).gt.abs(ai(i,j))) then
               tmp = abs(ar(i,j))*sqrt(1e0+(ai(i,j)/ar(i,j))**2)
            else if (ai(i,j).ne.0e0) then
               tmp = abs(ai(i,j))*sqrt((ar(i,j)/ai(i,j))**2+1e0)
            else
               tmp = abs(ar(i,j))
            endif
            x = x + tmp
  160       continue
c
            norm = amax1(norm,x)
  180    continue
c     .......... eps3 replaces zero pivot in decomposition
c                and close roots are modified by eps3 ..........
         if (norm .eq. 0.0e0) norm = 1.0e0
         eps3 = epslon(norm)
c     .......... growto is the criterion for growth ..........
         ukroot = uk
         ukroot = sqrt(ukroot)
         growto = 0.1e0 / ukroot
  200    rlambd = wr(k)
         ilambd = wi(k)
         if (k .eq. 1) go to 280
         km1 = k - 1
         go to 240
c     .......... perturb eigenvalue if it is close
c                to any previous eigenvalue ..........
  220    rlambd = rlambd + eps3
c     .......... for i=k-1 step -1 until 1 do -- ..........
  240    do 260 i = k-1,1,-1
            if (select(i) .and. abs(wr(i)-rlambd) .lt. eps3 .and.
     x         abs(wi(i)-ilambd) .lt. eps3) go to 220
  260    continue
c
         wr(k) = rlambd
c     .......... form upper hessenberg (ar,ai)-(rlambd,ilambd)*i
c                and initial complex vector ..........
  280    do 320 i = 1, uk
            do 300 j = max0(1,i-1), uk
               rm1(i,j) = ar(i,j)
               rm2(i,j) = ai(i,j)
  300       continue
c
            rm1(i,i) = rm1(i,i) - rlambd
            rm2(i,i) = rm2(i,i) - ilambd
            rv1(i) = eps3
  320    continue
c     .......... triangular decomposition with interchanges,
c                replacing zero pivots by eps3 ..........
         if (uk .eq. 1) go to 420
c
         do 400 i = 2, uk
            mp = i - 1
            if (pythag(rm1(i,mp),rm2(i,mp)) .le.
     x          pythag(rm1(mp,mp),rm2(mp,mp))) go to 360
c
c"    ( prefer vector
            do 340 j = mp, uk
               foo = rm1(i,j)
               rm1(i,j) = rm1(mp,j)
               rm1(mp,j) = foo
               foo = rm2(i,j)
               rm2(i,j) = rm2(mp,j)
               rm2(mp,j) = foo
  340       continue
c
  360       if (rm1(mp,mp) .eq. 0.0e0 .and. rm2(mp,mp) .eq. 0.0e0)
     x         rm1(mp,mp) = eps3
            call cdiv(rm1(i,mp),rm2(i,mp),rm1(mp,mp),rm2(mp,mp),x,y)
            if (x .eq. 0.0e0 .and. y .eq. 0.0e0) go to 400
c
            x = -x
c"    ( prefer vector
            do 380 j = i, uk
               rm1(i,j) = rm1(i,j) + x * rm1(mp,j) + y * rm2(mp,j)
               rm2(i,j) = rm2(i,j) + x * rm2(mp,j) - y * rm1(mp,j)
  380       continue
c
  400    continue
c
  420    if (rm1(uk,uk) .eq. 0.0e0 .and. rm2(uk,uk) .eq. 0.0e0)
     x      rm1(uk,uk) = eps3
         its = 0
c     .......... back substitution
c                for i=uk step -1 until 1 do -- ..........
  660    do 720 i = uk, 1, -1
            x = rv1(i)
            y = 0.0e0
c
            do 680 j = i+1, uk
               x = x - rm1(i,j) * rv1(j) + rm2(i,j) * rv2(j)
               y = y - rm1(i,j) * rv2(j) - rm2(i,j) * rv1(j)
  680       continue
c
            call cdiv(x,y,rm1(i,i),rm2(i,i),rv1(i),rv2(i))
  720    continue
c     .......... acceptance test for eigenvector
c                and normalization ..........
         its = its + 1
         norm = 0.0e0
         normv = 0.0e0
c
         do 780 i = 1, uk
c--            foo = pythag(rv1(i),rv2(i))
            if (abs(rv1(i)).gt.abs(rv2(i))) then
               foo = abs(rv1(i))*sqrt(1e0+(rv2(i)/rv1(i))**2)
            else if (rv2(i).ne.0e0) then
               foo = abs(rv2(i))*sqrt((rv1(i)/rv2(i))**2+1e0)
            else
               foo = abs(rv1(i))
            endif
            if (normv .ge. foo) go to 760
            normv = foo
            j = i
  760       norm = norm + foo
  780    continue
c
         if (norm .lt. growto) go to 840
c     .......... accept vector ..........
         x = rv1(j)
         y = rv2(j)
c
         do 820 i = 1, uk
            call cdiv(rv1(i),rv2(i),x,y,zr(i,s),zi(i,s))
  820    continue
c
         if (uk .eq. n) go to 940
         j = uk + 1
         go to 900
c     .......... in-line procedure for choosing
c                a new starting vector ..........
  840    if (its .ge. uk) go to 880
         x = ukroot
         y = eps3 / (x + 1.0e0)
         rv1(1) = eps3
c
         do 860 i = 2, uk
  860    rv1(i) = y
c
         j = uk - its + 1
         rv1(j) = rv1(j) - eps3 * x
         go to 660
c     .......... set error -- unaccepted eigenvector ..........
  880    j = 1
         ierr = -k
c     .......... set remaining vector components to zero ..........
  900    do 920 i = j, n
            zr(i,s) = 0.0e0
            zi(i,s) = 0.0e0
  920    continue
c
  940    s = s + 1
  980 continue
c
      go to 1001
c     .......... set error -- underestimate of eigenvector
c                space required ..........
 1000 if (ierr .ne. 0) ierr = ierr - n
      if (ierr .eq. 0) ierr = -(2 * n + 1)
 1001 m = s - 1
      return
      end
