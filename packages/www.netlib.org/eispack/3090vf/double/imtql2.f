C-----------------------------------------------------------------------SYM10360
C EIGENSYSTEM OF A SYMMETRIC TRIDIAGONAL MATRIX BY THE QL ALGORITHM WITHSYM10370
C IMPLICIT SHIFT.                                                       SYM10380
C                                                                       SYM10390
      SUBROUTINE  IMTQL2 ( LD, N, D, E, Z, IER )                        SYM10400
C                                                                       SYM10410
      REAL*8      D(N), E(N), Z(LD,N)                                   SYM10420
      INTEGER     N, LD, IER                                            SYM10430
C                                                                       SYM10440
C LD     E  FIRST DIMENSION ASSIGNED TO THE TWO-DIMENSIONAL ARRAY       SYM10450
C           PARAMETERS IN THE CALLING PROGRAM.                          SYM10460
C                                                                       SYM10470
C N      E  ORDER OF THE TRIDIAGONAL MATRIX.                            SYM10480
C                                                                       SYM10490
C D      E  DIAGONAL OF THE TRIDIAGONAL MATRIX.                         SYM10500
C        R  EIGENVALUES IN THE ORDER OF INCREASING VALUE.               SYM10510
C                                                                       SYM10520
C E      E  CODIAGONAL OF THE TRIDIAGONAL MATRIX (E(1)=0).              SYM10530
C        R  CODIAGONAL RESIDUE.                                         SYM10540
C                                                                       SYM10550
C Z      E  FOR THE SOLUTION OF THE SYMMETRIC EIGENPROBLEM:             SYM10560
C              RIGHT MATRIX OF THE SIMILARITY TRANSFORMATION TO         SYM10570
C              TRIDIAGONAL FORM.                                        SYM10580
C           FOR THE SOLUTION OF THE TRIDIAGONAL EIGENPROBLEM:           SYM10590
C              IDENTITY MATRIX OF ORDER N.                              SYM10600
C        R  MATRIX OF EIGENVECTORS.                                     SYM10610
C                                                                       SYM10620
C IER    R     IER=0   NORMAL RETURN.                                   SYM10630
C              IER>0   THE UNORDERED EIGENPAIRS 1,...,IER-1 SHOULD      SYM10640
C                      BE CORRECT.                                      SYM10650
C                                                                       SYM10660
C     THIS SUBROUTINE IS BASED ON THE ALGOL PROCEDURE IMTQL2,           SYM10670
C     NUM. MATH. 12, 377-383(1968) BY MARTIN AND WILKINSON,             SYM10680
C     AS MODIFIED IN NUM. MATH. 15, 450(1970) BY DUBRULLE.              SYM10690
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 241-248(1971).   SYM10700
C                                                                       SYM10710
C     VERSION FOR THE IBM 3090VF DATED NOVEMBER 1987.                   SYM10720
C     ------------------------------------------------------------------SYM10730
      REAL*8      C, S, T, U, H, P, Q                                   SYM10740
      INTEGER     IT, ITMAX, I, J, K, M                                 SYM10750
      DATA        ITMAX/30/                                             SYM10760
C                                                                       SYM10770
      CALL XUFLOW(0)                                                    SYM10780
      IER=0                                                             SYM10790
      IF(N.EQ.1) GO TO 1000                                             SYM10800
         DO 80 I=2,N                                                    SYM10810
80       E(I-1)=E(I)                                                    SYM10820
      E(N)=0D0                                                          SYM10830
C                                                                       SYM10840
         DO 300 J=1,N                                                   SYM10850
         IT=0                                                           SYM10860
C SEARCH FOR A SMALL SUBDIAGONAL ELEMENT                                SYM10870
100         DO 120 M=J,N-1                                              SYM10880
            Q=DABS(D(M))+DABS(D(M+1))                                   SYM10890
            P=Q+DABS(E(M))                                              SYM10900
            IF(P.EQ.Q) GO TO 140                                        SYM10910
120         CONTINUE                                                    SYM10920
         M=N                                                            SYM10930
C AN EIGENVALUE IS FOUND                                                SYM10940
140      IF(M.EQ.J) GO TO 300                                           SYM10950
C ITERATION                                                             SYM10960
         U=D(J)                                                         SYM10970
         T=E(J)                                                         SYM10980
         IF(IT.EQ.ITMAX) GO TO 999                                      SYM10990
         IT=IT+1                                                        SYM11000
C SHIFT                                                                 SYM11010
         Q=(D(J+1)-U)/(T+T)                                             SYM11020
         IF(DABS(Q).LE.1D0) GO TO 160                                   SYM11030
            Q=D(M)-U+T/(Q*(1D0+DSQRT(1D0+(1D0/Q)**2)))                  SYM11040
            GO TO 180                                                   SYM11050
160      Q=D(M)-U+T/(Q+DSIGN(DSQRT(1D0+Q**2),Q))                        SYM11060
C QL SWEEP                                                              SYM11070
180      U=0D0                                                          SYM11080
         S=1D0                                                          SYM11090
         C=1D0                                                          SYM11100
            DO 260 I=M-1,J,-1                                           SYM11110
            P=S*E(I)                                                    SYM11120
            H=C*E(I)                                                    SYM11130
            IF(DABS(P).LT.DABS(Q)) GO TO 200                            SYM11140
               IF(P.NE.0D0) GO TO 190                                   SYM11150
                  E(I+1)=0D0                                            SYM11160
                  D(I+1)=D(I+1)-U                                       SYM11170
                  E(M)=0D0                                              SYM11180
                  GO TO 100                                             SYM11190
190            C=Q/P                                                    SYM11200
               T=DSQRT(1D0+C*C)                                         SYM11210
               E(I+1)=P*T                                               SYM11220
               S=1D0/T                                                  SYM11230
               C=C*S                                                    SYM11240
               GO TO 220                                                SYM11250
200         S=P/Q                                                       SYM11260
            T=DSQRT(1D0+S*S)                                            SYM11270
            E(I+1)=Q*T                                                  SYM11280
            C=1D0/T                                                     SYM11290
            S=S*C                                                       SYM11300
220         Q=D(I+1)-U                                                  SYM11310
            T=(D(I)-Q)*S+(C+C)*H                                        SYM11320
            U=S*T                                                       SYM11330
            D(I+1)=Q+U                                                  SYM11340
            Q=C*T-H                                                     SYM11350
C VECTORS                                                               SYM11360
               DO 240 K=1,N                                             SYM11370
               T       =Z(K,I+1)                                        SYM11380
               Z(K,I+1)=S*Z(K,I)+C*T                                    SYM11390
               Z(K,I  )=C*Z(K,I)-S*T                                    SYM11400
240            CONTINUE                                                 SYM11410
260         CONTINUE                                                    SYM11420
C COMPLETION OF QL SWEEP                                                SYM11430
         D(J)=D(J)-U                                                    SYM11440
         E(J)=Q                                                         SYM11450
         E(M)=0D0                                                       SYM11460
         GO TO 100                                                      SYM11470
300      CONTINUE                                                       SYM11480
C REORDERING OF THE EIGENPAIRS IN THE ORDER OF NONDECREASING ROOTS      SYM11490
         DO 360 J=1,N-1                                                 SYM11500
         K=J                                                            SYM11510
            DO 320 I=J+1,N                                              SYM11520
            IF(D(I).LT.D(K)) K=I                                        SYM11530
320         CONTINUE                                                    SYM11540
         IF (K.EQ.J) GO TO 360                                          SYM11550
            T   =D(J)                                                   SYM11560
            D(J)=D(K)                                                   SYM11570
            D(K)=T                                                      SYM11580
               DO 340 I=1,N                                             SYM11590
               T     =Z(I,J)                                            SYM11600
               Z(I,J)=Z(I,K)                                            SYM11610
340            Z(I,K)=T                                                 SYM11620
360      CONTINUE                                                       SYM11630
      GO TO 1000                                                        SYM11640
C ERROR                                                                 SYM11650
999   IER=J                                                             SYM11660
C                                                                       SYM11670
1000  RETURN                                                            SYM11680
      END                                                               SYM11690
