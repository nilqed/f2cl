C-----------------------------------------------------------------------SYM03980
C EIGENVECTORS OF A HERMITIAN MATRIX BY TRANSFORMATION OF THE           SYM03990
C EIGENVECTORS OF THE ASSOCIATED SYMMETRIC TRIDIAGONAL MATRIX PRODUCED  SYM04000
C BY SUBROUTINE HTRID3.                                                 SYM04010
C                                                                       SYM04020
      SUBROUTINE  HTRIB3 ( LD, N, A, TAU, M, ZR,ZI )                    SYM04030
C                                                                       SYM04040
      INTEGER     LD, N                                                 SYM04050
      REAL*8      A(LD,N), TAU(2,N), ZR(LD,M),ZI(LD,M)                  SYM04060
C                                                                       SYM04070
C                                                                       SYM04080
C LD     E  FIRST DIMENSION ASSIGNED TO THE TWO-DIMENSIONAL ARRAY       SYM04090
C           PARAMETERS IN THE CALLING PROGRAM.                          SYM04100
C                                                                       SYM04110
C N      E  ORDER OF THE MATRIX.                                        SYM04120
C                                                                       SYM04130
C A      E  INFORMATION ABOUT THE TRANSFORMATIONS PERFORMED BY HTRID3.  SYM04140
C                                                                       SYM04150
C TAU    E  ADDITIONAL INFORMATION ABOUT THE TRANSFORMATIONS.           SYM04160
C                                                                       SYM04170
C M      E  NUMBER OF EIGENVECTORS TO BE TRANSFORMED.                   SYM04180
C                                                                       SYM04190
C ZR     E  EIGENVECTORS OF THE SYMMETRIC TRIDIAGONAL MATRIX (LEADING   SYM04200
C           M COLUMNS).                                                 SYM04210
C        R  REAL PARTS OF THE EIGENVECTORS OF THE HERMITIAN MATRIX.     SYM04220
C                                                                       SYM04230
C ZI     R  IMAGINARY PARTS OF THE EIGENVECTORS OF THE HERMITIAN MATRIX.SYM04240
C                                                                       SYM04250
C     THIS SUBROUTINE IS BASED ON A COMPLEX VARIANT OF THE ALGOL        SYM04260
C     PROCEDURE TRBAK3, NUM. MATH. 11, 181-195(1968) BY MARTIN, REINSCH,SYM04270
C     AND WILKINSON.                                                    SYM04280
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 212-226(1971).   SYM04290
C                                                                       SYM04300
C     VERSION FOR THE IBM 3090VF DATED NOVEMBER 1987.                   SYM04310
C     ------------------------------------------------------------------SYM04320
      INTEGER     I, J, K, KACHEL                                       SYM04330
      REAL*8      SR,SI, S                                              SYM04340
C                                                                       SYM04350
      CALL XUFLOW(0)                                                    SYM04360
C     IF(M.EQ.0) GO TO 1000                                             SYM04370
C EIGENVECTORS OF THE ASSOCIATED TRIDIAGONAL HERMITIAN MATRIX           SYM04380
         DO 120 I=1,N                                                   SYM04390
            DO 100 J=1,M                                                SYM04400
            ZI(I,J)=ZR(I,J)*TAU(2,I)                                    SYM04410
100         ZR(I,J)=ZR(I,J)*TAU(1,I)                                    SYM04420
120      CONTINUE                                                       SYM04430
      IF(M.GT.KACHEL(LD)) GO TO 220                                     SYM04440
C PATH FOR SMALL ARRAYS                                                 SYM04450
         DO 200 K=N-2,1,-1                                              SYM04460
         S=A(K+1,K)                                                     SYM04470
         IF(S.EQ.0D0) GO TO 200                                         SYM04480
         S=1D0/S                                                        SYM04490
C"    ( PREFER VECTOR                                                   SYM04500
            DO 180 J=1,M                                                SYM04510
            SR=0D0                                                      SYM04520
            SI=0D0                                                      SYM04530
               DO 140 I=K+1,N                                           SYM04540
               SR=SR+A(I,K)*ZR(I,J)+A(K,I)*ZI(I,J)                      SYM04550
140            SI=SI+A(I,K)*ZI(I,J)-A(K,I)*ZR(I,J)                      SYM04560
            SR=SR*S                                                     SYM04570
            SI=SI*S                                                     SYM04580
               DO 160 I=K+1,N                                           SYM04590
               ZR(I,J)=ZR(I,J)+SR*A(I,K)-SI*A(K,I)                      SYM04600
160            ZI(I,J)=ZI(I,J)+SR*A(K,I)+SI*A(I,K)                      SYM04610
180         CONTINUE                                                    SYM04620
200      CONTINUE                                                       SYM04630
      GO TO 1000                                                        SYM04640
C PATH FOR LARGE ARRAYS                                                 SYM04650
220      DO 300 K=N-2,1,-1                                              SYM04660
         S=A(K+1,K)                                                     SYM04670
         IF(S.EQ.0D0) GO TO 300                                         SYM04680
         S=1D0/S                                                        SYM04690
C"    ( PREFER VECTOR                                                   SYM04700
            DO 280 J=1,M                                                SYM04710
            SR=0D0                                                      SYM04720
            SI=0D0                                                      SYM04730
               DO 240 I=K+1,N                                           SYM04740
               SR=SR+A(I,K)*ZR(I,J)+A(K,I)*ZI(I,J)                      SYM04750
240            SI=SI+A(I,K)*ZI(I,J)-A(K,I)*ZR(I,J)                      SYM04760
            SR=SR*S                                                     SYM04770
            SI=SI*S                                                     SYM04780
               DO 260 I=K+1,N                                           SYM04790
               ZR(I,J)=ZR(I,J)+SR*A(I,K)-SI*A(K,I)                      SYM04800
260            ZI(I,J)=ZI(I,J)+SR*A(K,I)+SI*A(I,K)                      SYM04810
280         CONTINUE                                                    SYM04820
300      CONTINUE                                                       SYM04830
1000  RETURN                                                            SYM04840
      END                                                               SYM04850
