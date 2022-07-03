@process directive('"    (')
      subroutine combak(nm,low,igh,ar,ai,int,m,zr,zi)
c
      integer i,j,m,mp,nm,igh,low
      real ar(nm,igh),ai(nm,igh),zr(nm,m),zi(nm,m)
      real xr,xi
      integer int(igh)
c
c     this subroutine is a translation of the algol procedure combak,
c     num. math. 12, 349-368(1968) by martin and wilkinson.
c     handbook for auto. comp., vol.ii-linear algebra, 339-358(1971).
c
c     this subroutine forms the eigenvectors of a complex general
c     matrix by back transforming those of the corresponding
c     upper hessenberg matrix determined by  comhes.
c
c     on input
c
c        nm must be set to the row dimension of two-dimensional
c          array parameters as declared in the calling program
c          dimension statement.
c
c        low and igh are integers determined by the balancing
c          subroutine  cbal.  if  cbal  has not been used,
c          set low=1 and igh equal to the order of the matrix.
c
c        ar and ai contain the multipliers which were used in the
c          reduction by  comhes  in their lower triangles
c          below the subdiagonal.
c
c        int contains information on the rows and columns
c          interchanged in the reduction by  comhes.
c          only elements low through igh are used.
c
c        m is the number of eigenvectors to be back transformed.
c
c        zr and zi contain the real and imaginary parts,
c          respectively, of the eigenvectors to be
c          back transformed in their first m columns.
c
c     on output
c
c        zr and zi contain the real and imaginary parts,
c          respectively, of the transformed eigenvectors
c          in their first m columns.
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
c     .......... for mp=igh-1 step -1 until low+1 do -- ..........
      do 140 mp = igh-1, low+1, -1
c"    ( ignore recrdeps
         do 110 i = mp+1, igh
            xr = ar(i,mp-1)
            xi = ai(i,mp-1)
c
            do 100 j = 1, m
               zr(i,j) = zr(i,j) + xr * zr(mp,j) - xi * zi(mp,j)
               zi(i,j) = zi(i,j) + xr * zi(mp,j) + xi * zr(mp,j)
  100       continue
c
  110    continue
c
         i = int(mp)
c
c"    ( prefer vector
         do 130 j = 1, m
            xr = zr(i,j)
            zr(i,j) = zr(mp,j)
            zr(mp,j) = xr
            xi = zi(i,j)
            zi(i,j) = zi(mp,j)
            zi(mp,j) = xi
  130    continue
c
  140 continue
c
  200 return
      end
