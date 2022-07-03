@PROCESS DIRECTIVE('"    (')                                            SYM04860
C-----------------------------------------------------------------------SYM04870
C EIGENVECTORS OF A HERMITIAN MATRIX BY TRANSFORMATION OF THE           SYM04880
C EIGENVECTORS OF THE ASSOCIATED SYMMETRIC TRIDIAGONAL MATRIX PRODUCED  SYM04890
C BY SUBROUTINE HTRIDI.                                                 SYM04900
C                                                                       SYM04910
      SUBROUTINE  HTRIBK ( LD, N, AR,AI, TAU, M, ZR,ZI )                SYM04920
C                                                                       SYM04930
      INTEGER     LD, M, N                                              SYM04940
      REAL*8      AR(LD,N),AI(LD,N), TAU(2,N), ZR(LD,M),ZI(LD,M)        SYM04950
C                                                                       SYM04960
C LD     E  FIRST DIMENSION ASSIGNED TO THE TWO-DIMENSIONAL ARRAY       SYM04970
C           PARAMETERS IN THE CALLING PROGRAM.                          SYM04980
C                                                                       SYM04990
C N      E  ORDER OF THE MATRIX                                         SYM05000
C                                                                       SYM05010
C AR,AI  E  INFORMATION ABOUT THE TRANSFORMATIONS PERFORMED BY HTRIDI   SYM05020
C           IN THE SUBDIAGONAL CELLS.                                   SYM05030
C                                                                       SYM05040
C TAU    E  ADDITIONAL INFORMATION ABOUT THE TRANSFORMATIONS.           SYM05050
C                                                                       SYM05060
C M      E  NUMBER OF EIGENVECTORS TO BE TRANSFORMED.                   SYM05070
C                                                                       SYM05080
C ZR     E  EIGENVECTORS OF THE SYMMETRIC TRIDIAGONAL MATRIX (LEADING   SYM05090
C           M COLUMNS).                                                 SYM05100
C        R  REAL PARTS OF THE EIGENVECTORS OF THE HERMITIAN MATRIX.     SYM05110
C                                                                       SYM05120
C ZI     R  IMAGINARY PARTS OF THE EIGENVECTORS OF THE HERMITIAN MATRIX.SYM05130
C                                                                       SYM05140
C     THIS SUBROUTINE IS BASED ON A COMPLEX VARIANT OF THE ALGOL        SYM05150
C     PROCEDURE TRBAK1, NUM. MATH. 11, 181-195(1968) BY MARTIN, REINSCH,SYM05160
C     AND WILKINSON.                                                    SYM05170
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 212-226(1971).   SYM05180
C                                                                       SYM05190
C SUBPROGRAMS:  KACHEL                                                  SYM05200
C                                                                       SYM05210
C     VERSION FOR THE IBM 3090VF DATED NOVEMBER 1987.                   SYM05220
C     ------------------------------------------------------------------SYM05230
      INTEGER     I, J, K, KACHEL                                       SYM05240
      REAL*8      SR,SI, S                                              SYM05250
C                                                                       SYM05260
      CALL XUFLOW(0)                                                    SYM05270
C     IF(M.EQ.0) GO TO 1000                                             SYM05280
C EIGENVECTORS OF THE ASSOCIATED TRIDIAGONAL HERMITIAN MATRIX           SYM05290
         DO 120 I=1,N                                                   SYM05300
            DO 100 J=1,M                                                SYM05310
            ZI(I,J)=ZR(I,J)*TAU(2,I)                                    SYM05320
100         ZR(I,J)=ZR(I,J)*TAU(1,I)                                    SYM05330
120      CONTINUE                                                       SYM05340
      IF(M.GT.KACHEL(LD)) GO TO 220                                     SYM05350
C PATH FOR SMALL ARRAYS                                                 SYM05360
         DO 200 K=N-2,1,-1                                              SYM05370
         S=AR(K+1,K)                                                    SYM05380
         IF(S.EQ.0D0) GO TO 200                                         SYM05390
         S=1D0/S                                                        SYM05400
C"    ( PREFER VECTOR                                                   SYM05410
            DO 180 J=1,M                                                SYM05420
            SR=0D0                                                      SYM05430
            SI=0D0                                                      SYM05440
               DO 140 I=K+1,N                                           SYM05450
               SR=SR+AR(I,K)*ZR(I,J)+AI(I,K)*ZI(I,J)                    SYM05460
140            SI=SI+AR(I,K)*ZI(I,J)-AI(I,K)*ZR(I,J)                    SYM05470
            SR=SR*S                                                     SYM05480
            SI=SI*S                                                     SYM05490
               DO 160 I=K+1,N                                           SYM05500
               ZR(I,J)=ZR(I,J)+SR*AR(I,K)-SI*AI(I,K)                    SYM05510
160            ZI(I,J)=ZI(I,J)+SR*AI(I,K)+SI*AR(I,K)                    SYM05520
180         CONTINUE                                                    SYM05530
200      CONTINUE                                                       SYM05540
      GO TO 1000                                                        SYM05550
C PATH FOR LARGE ARRAYS                                                 SYM05560
220      DO 300 K=N-2,1,-1                                              SYM05570
         S=AR(K+1,K)                                                    SYM05580
         IF(S.EQ.0D0) GO TO 300                                         SYM05590
         S=1D0/S                                                        SYM05600
            DO 280 J=1,M                                                SYM05610
            SR=0D0                                                      SYM05620
            SI=0D0                                                      SYM05630
               DO 240 I=K+1,N                                           SYM05640
               SR=SR+AR(I,K)*ZR(I,J)+AI(I,K)*ZI(I,J)                    SYM05650
240            SI=SI+AR(I,K)*ZI(I,J)-AI(I,K)*ZR(I,J)                    SYM05660
            SR=SR*S                                                     SYM05670
            SI=SI*S                                                     SYM05680
               DO 260 I=K+1,N                                           SYM05690
               ZR(I,J)=ZR(I,J)+SR*AR(I,K)-SI*AI(I,K)                    SYM05700
260            ZI(I,J)=ZI(I,J)+SR*AI(I,K)+SI*AR(I,K)                    SYM05710
280         CONTINUE                                                    SYM05720
300      CONTINUE                                                       SYM05730
1000  RETURN                                                            SYM05740
      END                                                               SYM05750
