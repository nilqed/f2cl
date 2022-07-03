C-----------------------------------------------------------------------GSY02530
C EIGENVECTORS OF A GENERALIZED SYMMETRIC EIGENPROBLEM BY TRANSFORMATIONGSY02540
C OF THOSE OF THE CORRESPONDING SYMMETRIC MATRIX PRODUCED BY REDUC2.    GSY02550
C                                                                       GSY02560
      SUBROUTINE  REBAKB ( LD, N, B, D, M, Z )                          GSY02570
C                                                                       GSY02580
      INTEGER     M, N, LD                                              GSY02590
      REAL*8      B(LD,N), D(N), Z(LD,M)                                GSY02600
C                                                                       GSY02610
C LD     E  FIRST DIMENSION ASSIGNED TO THE TWO-DIMENSIONAL ARRAY       GSY02620
C           PARAMETERS IN THE CALLING PROGRAM.                          GSY02630
C                                                                       GSY02640
C N      E  ORDER OF THE SYSTEM.                                        GSY02650
C                                                                       GSY02660
C B      E  INFORMATION ABOUT THE SIMILARITY TRANSFORMATION             GSY02670
C           (CHOLESKY DECOMPOSITION) USED BY REDUC2, STORED IN THE      GSY02680
C           SUBDIAGONAL PART OF THE ARRAY.                              GSY02690
C                                                                       GSY02700
C D      E  ADDITIONAL INFORMATION ABOUT THE TRANSFORMATION.            GSY02710
C                                                                       GSY02720
C M      E  NUMBER OF EIGENVECTORS TO BE TRANSFORMED.                   GSY02730
C                                                                       GSY02740
C Z      E  EIGENVECTORS TO BE TRANSFORMED, IN THE FIRST M COLUMNS OF   GSY02750
C           THE ARRAY                                                   GSY02760
C        R  TRANSFORMED EIGENVECTORS.                                   GSY02770
C                                                                       GSY02780
C     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE REBAKB,   GSY02790
C     NUM. MATH. 11, 99-110(1968) BY MARTIN AND WILKINSON.              GSY02800
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 303-314(1971).   GSY02810
C                                                                       GSY02820
C     VERSION FOR THE IBM 3090VF DATED NOVEMBER 1987.                   GSY02830
C     ------------------------------------------------------------------GSY02840
      INTEGER     I, J, K                                               GSY02850
      REAL*8      X                                                     GSY02860
C                                                                       GSY02870
      CALL XUFLOW(0)                                                    GSY02880
         DO 100 J=1,M                                                   GSY02890
            DO 80 I=N,1,-1                                              GSY02900
            X=D(I)*Z(I,J)                                               GSY02910
               DO 60 K=1,I-1                                            GSY02920
60             X=X+B(I,K)*Z(K,J)                                        GSY02930
80          Z(I,J)=X                                                    GSY02940
100      CONTINUE                                                       GSY02950
      RETURN                                                            GSY02960
      END                                                               GSY02970
