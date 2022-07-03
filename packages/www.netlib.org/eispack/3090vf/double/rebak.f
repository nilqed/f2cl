C-----------------------------------------------------------------------GSY02080
C EIGENVECTORS OF A GENERALIZED SYMMETRIC EIGENPROBLEM BY TRANSFORMATIONGSY02090
C OF THOSE OF THE CORRESPONDING SYMMETRIC MATRIX PRODUCED BY REDUC.     GSY02100
C                                                                       GSY02110
      SUBROUTINE  REBAK ( LD, N, B, D, M, Z )                           GSY02120
C                                                                       GSY02130
      INTEGER     LD, M, N                                              GSY02140
      REAL*8      B(LD,N), D(N), Z(LD,M)                                GSY02150
C                                                                       GSY02160
C LD     E  FIRST DIMENSION ASSIGNED TO THE TWO-DIMENSIONAL ARRAY       GSY02170
C           PARAMETERS IN THE CALLING PROGRAM.                          GSY02180
C                                                                       GSY02190
C N      E  ORDER OF THE SYSTEM.                                        GSY02200
C                                                                       GSY02210
C B      E  INFORMATION ABOUT THE SIMILARITY TRANSFORMATION             GSY02220
C           (CHOLESKY DECOMPOSITION) USED BY REDUC, STORED IN THE       GSY02230
C           SUBDIAGONAL PART OF THE ARRAY.                              GSY02240
C                                                                       GSY02250
C D      E  ADDITIONAL INFORMATION ABOUT THE TRANSFORMATION.            GSY02260
C                                                                       GSY02270
C M      E  NUMBER OF EIGENVECTORS TO BE TRANSFORMED.                   GSY02280
C                                                                       GSY02290
C Z      E  EIGENVECTORS TO BE TRANSFORMED, IN THE FIRST M COLUMNS OF   GSY02300
C           THE ARRAY.                                                  GSY02310
C        R  TRANSFORMED EIGENVECTORS.                                   GSY02320
C                                                                       GSY02330
C     THIS SUBROUTINE IS BASED ON THE ALGOL PROCEDURE REBAKA,           GSY02340
C     NUM. MATH. 11, 99-110(1968) BY MARTIN AND WILKINSON.              GSY02350
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 303-314(1971).   GSY02360
C                                                                       GSY02370
C     VERSION FOR THE IBM 3090VF DATED NOVEMBER 1987.                   GSY02380
C     ------------------------------------------------------------------GSY02390
      INTEGER     I, J, K                                               GSY02400
      REAL*8      X                                                     GSY02410
C                                                                       GSY02420
      CALL XUFLOW(0)                                                    GSY02430
         DO 100 J=1,M                                                   GSY02440
            DO 80 I=N,1,-1                                              GSY02450
            X = Z(I,J)                                                  GSY02460
               DO 60 K=I+1,N                                            GSY02470
60             X=X-B(K,I)*Z(K,J)                                        GSY02480
80          Z(I,J)=X/D(I)                                               GSY02490
100      CONTINUE                                                       GSY02500
      RETURN                                                            GSY02510
      END                                                               GSY02520
