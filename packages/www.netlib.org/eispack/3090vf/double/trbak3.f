@PROCESS DIRECTIVE('"    (')                                            SYM21250
C-----------------------------------------------------------------------SYM21260
C     EIGENVECTORS OF A REAL SYMMETRIC MATRIX BY BACK TRANSFORMATION OF SYM21270
C     THOSE OF THE CORRESPONDING SYMMETRIC TRIDIAGONAL MATRIX PRODUCED  SYM21280
C     BY TRED3.                                                         SYM21290
C                                                                       SYM21300
      SUBROUTINE  TRBAK3( LD, N, NA, A, M, Z )                          SYM21310
C                                                                       SYM21320
      INTEGER     LD, N, NA, M                                          SYM21330
      REAL*8      A(NA), Z(LD,M)                                        SYM21340
C                                                                       SYM21350
C LD     E    FIRST DIMENSION ASSIGNED IN THE CALLING ROUTINE TO THE    SYM21360
C             TWO-DIMENSIONAL ARRAY PARAMETERS.                         SYM21370
C                                                                       SYM21380
C N      E    ORDER OF THE MATRIX.                                      SYM21390
C                                                                       SYM21400
C NA     E    DIMENSION ASSIGNED TO THE ARRAY PARAMETER A IN THE        SYM21410
C             CALLING ROUTINE.                                          SYM21420
C                                                                       SYM21430
C A      E    INFORMATION ABOUT THE ORTHOGONAL TRANSFORMATIONS USED     SYM21440
C             IN THE REDUCTION BY TRED3 IN THE FIRST N*(N+1)/2 CELLS.   SYM21450
C                                                                       SYM21460
C M      E    NUMBER OF EIGENVECTORS TO BE TRANSFORMED.                 SYM21470
C                                                                       SYM21480
C Z      E    EIGENVECTORS TO BE TRANSFORMED IN THE FIRST M COLUMNS     SYM21490
C        R    TRANSFORMED EIGENVECTORS                                  SYM21500
C                                                                       SYM21510
C     THIS SUBROUTINE IS BASED ON THE ALGOL PROCEDURE TRBAK3,           SYM21520
C     NUM. MATH. 11, 181-195(1968) BY MARTIN, REINSCH, AND WILKINSON.   SYM21530
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 212-226(1971).   SYM21540
C                                                                       SYM21550
C SUBPROGRAMS CALLED:  KACHEL                                           SYM21560
C                                                                       SYM21570
C     VERSION FOR THE IBM 3090VF DATED NOVEMBER 1987.                   SYM21580
C     ------------------------------------------------------------------SYM21590
      INTEGER     I, J, K, KK                                           SYM21600
      INTEGER     KACHEL                                                SYM21610
      REAL*8      T, S                                                  SYM21620
C                                                                       SYM21630
      CALL XUFLOW(0)                                                    SYM21640
      IF(M.LT.1) GO TO 1000                                             SYM21650
      IF(M.GT.2*KACHEL(LD)) GO TO 180                                   SYM21660
C PATH FOR SMALL ARRAYS                                                 SYM21670
         DO 160 K=3,N                                                   SYM21680
         KK=(K*K-K)/2                                                   SYM21690
         T=A(K+KK)                                                      SYM21700
         IF(T.EQ.0D0) GO TO 160                                         SYM21710
C"    ( PREFER VECTOR                                                   SYM21720
            DO 140 J=1,M                                                SYM21730
            S=0D0                                                       SYM21740
               DO 100 I=1,K-1                                           SYM21750
100            S=S+A(I+KK)*Z(I,J)                                       SYM21760
            S=T*S                                                       SYM21770
               DO 120 I=1,K-1                                           SYM21780
120            Z(I,J)=Z(I,J)+S*A(I+KK)                                  SYM21790
140         CONTINUE                                                    SYM21800
160      CONTINUE                                                       SYM21810
      GO TO 1000                                                        SYM21820
C PATH FOR LARGE ARRAYS                                                 SYM21830
180      DO 260 K=3,N                                                   SYM21840
         KK=(K*K-K)/2                                                   SYM21850
         T=A(K+KK)                                                      SYM21860
         IF(T.EQ.0D0) GO TO 260                                         SYM21870
            DO 240 J=1,M                                                SYM21880
            S=0D0                                                       SYM21890
               DO 200 I=1,K-1                                           SYM21900
200            S=S+A(I+KK)*Z(I,J)                                       SYM21910
            S=T*S                                                       SYM21920
               DO 220 I=1,K-1                                           SYM21930
220            Z(I,J)=Z(I,J)+S*A(I+KK)                                  SYM21940
240         CONTINUE                                                    SYM21950
260      CONTINUE                                                       SYM21960
1000  RETURN                                                            SYM21970
      END                                                               SYM21980
