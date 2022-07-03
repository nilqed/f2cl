C-----------------------------------------------------------------------SYM11700
C EIGENVALUES OF A SYMMETRIC TRIDIAGONAL MATRIX AND THEIR ASSOCIATIONS  SYM11710
C WITH PRINCIPAL SUBMATRICES ISOLATED BY SMALL SUBDIAGONAL ELEMENTS     SYM11720
C (QL ALGORITHM WITH IMPLICIT SHIFT).                                   SYM11730
C                                                                       SYM11740
      SUBROUTINE  IMTQLV ( N, D, E, E2, W, IND, IERR, V )               SYM11750
C                                                                       SYM11760
      INTEGER     IND(N), N, IERR                                       SYM11770
      REAL*8      D(N), E(N), E2(N), W(N), V(N)                         SYM11780
C                                                                       SYM11790
C N      E  ORDER OF THE MATRIX.                                        SYM11800
C                                                                       SYM11810
C D      E  DIAGONAL ELEMENTS OF THE TRIDIAGONAL MATRIX.                SYM11820
C                                                                       SYM11830
C E      E  SUBDIAGONAL ELEMENTS OF THE MATRIX IN CELLS 2,...,N.        SYM11840
C                                                                       SYM11850
C E2     E  SQUARES OF THE CORRESPONDING ELEMENTS OF E.                 SYM11860
C        R  THE ELEMENTS OF E2 CORRESPONDING TO THOSE OF E REGARDED AS  SYM11870
C           NEGLIGIBLE ARE REPLACED BY ZEROS.  THESE CAUSE THE MATRIX   SYM11880
C           TO SPLIT INTO A DIRECT SUM OF PRINCIPAL SUBMATRICES NUMBEREDSYM11890
C           FROM TOP TO BOTTOM.                                         SYM11900
C           E2(1) IS ALSO SET TO ZERO.                                  SYM11910
C                                                                       SYM11920
C W      R  EIGENVALUES IN ASCENDING ORDER.  IF AN ERROR EXIT OCCURS,   SYM11930
C           THE EIGENVALUES OF INDICES 1,2,...IERR-1 ARE CORRECT AND    SYM11940
C           ORDERED, BUT THEY MAY NOT BE THE SMALLEST EIGENVALUES.      SYM11950
C                                                                       SYM11960
C IND    R  INDICES OF THE SUBMATRICES ASSOCIATED WITH THE EIGENVALUES. SYM11970
C                                                                       SYM11980
C IERR   R  ZERO     FOR NORMAL RETURN,                                 SYM11990
C           J        IF THE J-TH EIGENVALUE IS NOT COMPUTED WITHIN 30   SYM12000
C                    ITERATIONS.                                        SYM12010
C                                                                       SYM12020
C V      -  ANCILLARY STORAGE.                                          SYM12030
C                                                                       SYM12040
C     THIS SUBROUTINE IS BASED ON A VARIANT OF THE ALGOL PROCEDURE      SYM12050
C     IMTQL1, NUM. MATH. 12, 377-383(1968) BY MARTIN AND WILKINSON,     SYM12060
C     AS MODIFIED IN NUM. MATH. 15, 450(1970) BY DUBRULLE.              SYM12070
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 241-248(1971).   SYM12080
C                                                                       SYM12090
C     VERSION FOR THE IBM 3090VF DATED NOVEMBER 1987.                   SYM12100
C     ------------------------------------------------------------------SYM12110
      INTEGER     I, J, K, L, M, ITAG                                   SYM12120
      REAL*8      B, C, F, G, P, R, S, TST1, TST2                       SYM12130
C                                                                       SYM12140
      CALL XUFLOW(0)                                                    SYM12150
      IERR=0                                                            SYM12160
      K=0                                                               SYM12170
      ITAG=0                                                            SYM12180
      W(1)=D(1)                                                         SYM12190
         DO 100 I=2,N                                                   SYM12200
         W(I)=D(I)                                                      SYM12210
         V(I-1)=E(I)                                                    SYM12220
100      CONTINUE                                                       SYM12230
      E2(1)=0D0                                                         SYM12240
      V(N)=0D0                                                          SYM12250
         DO 400 L=1,N                                                   SYM12260
         J=0                                                            SYM12270
C SEARCH FOR A SMALL SUB-DIAGONAL ELEMENT                               SYM12280
120         DO 140 M=L,N                                                SYM12290
            IF (M.EQ.N) GO TO 160                                       SYM12300
            TST1=DABS(W(M))+DABS(W(M+1))                                SYM12310
            TST2=TST1+DABS(V(M))                                        SYM12320
            IF (TST2.EQ.TST1) GO TO 160                                 SYM12330
C PROTECTION AGAINST FLOATING-POINT UNDERFLOW OF AN ELEMENT OF E2       SYM12340
            IF (E2(M+1).EQ.0D0) GO TO 180                               SYM12350
140         CONTINUE                                                    SYM12360
160      IF (M.LE.K) GO TO 200                                          SYM12370
         IF (M .NE. N) E2(M+1)=0D0                                      SYM12380
180      K=M                                                            SYM12390
         ITAG=ITAG+1                                                    SYM12400
200      P=W(L)                                                         SYM12410
C AN EIGENVALUE IS FOUND                                                SYM12420
         IF (M.EQ.L) GO TO 340                                          SYM12430
C ITERATION                                                             SYM12440
         IF (J.EQ.30) GO TO  999                                        SYM12450
         J=J+1                                                          SYM12460
C SHIFT                                                                 SYM12470
         G=(W(L+1)-P)/(2D0*V(L))                                        SYM12480
         IF(DABS(G).GT.1D0) GO TO 220                                   SYM12490
            G=G+DSIGN(DSQRT(1D0+G*G),G)                                 SYM12500
            GO TO 240                                                   SYM12510
220      G=G*(1D0+DSQRT(1D0+1D0/(G*G)))                                 SYM12520
240      G=W(M)-P+V(L)/G                                                SYM12530
         S=1D0                                                          SYM12540
         C=1D0                                                          SYM12550
         P=0D0                                                          SYM12560
C QL SWEEP                                                              SYM12570
            DO 300 I=M-1,L,-1                                           SYM12580
            F=S*V(I)                                                    SYM12590
            B=C*V(I)                                                    SYM12600
            IF(DABS(F).LT.DABS(G)) GO TO 260                            SYM12610
               IF(F.NE.0D0) GO TO 250                                   SYM12620
                  V(I+1)=0D0                                            SYM12630
                  W(I+1)=W(I+1)-P                                       SYM12640
                  V(M)=0D0                                              SYM12650
                  GO TO 120                                             SYM12660
250            C=G/F                                                    SYM12670
               R=DSQRT(1D0+C*C)                                         SYM12680
               V(I+1)=F*R                                               SYM12690
               S=1D0/R                                                  SYM12700
               C=C*S                                                    SYM12710
               GO TO 280                                                SYM12720
260         S=F/G                                                       SYM12730
            R=DSQRT(1D0+S*S)                                            SYM12740
            V(I+1)=G*R                                                  SYM12750
            C=1D0/R                                                     SYM12760
            S=S*C                                                       SYM12770
280         G=W(I+1)-P                                                  SYM12780
            R=(W(I)-G)*S+(C+C)*B                                        SYM12790
            P=S*R                                                       SYM12800
            W(I+1)=G+P                                                  SYM12810
            G=C*R-B                                                     SYM12820
300         CONTINUE                                                    SYM12830
         W(L)=W(L)-P                                                    SYM12840
         V(L)=G                                                         SYM12850
         V(M)=0D0                                                       SYM12860
         GO TO 120                                                      SYM12870
C ORDERING OF THE EIGENVALUES                                           SYM12880
340         DO 360 I=L,2,-1                                             SYM12890
            IF (P.GE.W(I-1)) GO TO 380                                  SYM12900
            W(I)=W(I-1)                                                 SYM12910
            IND(I)=IND(I-1)                                             SYM12920
360         CONTINUE                                                    SYM12930
         I=1                                                            SYM12940
380      W(I)=P                                                         SYM12950
         IND(I)=ITAG                                                    SYM12960
400      CONTINUE                                                       SYM12970
      GO TO 1000                                                        SYM12980
C ERROR: NO CONVERGENCE TO AN EIGENVALUE AFTER 30 ITERATIONS            SYM12990
999   IERR=L                                                            SYM13000
C                                                                       SYM13010
1000  RETURN                                                            SYM13020
      END                                                               SYM13030
