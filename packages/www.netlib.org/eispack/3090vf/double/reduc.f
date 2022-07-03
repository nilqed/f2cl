C-----------------------------------------------------------------------GSY00020
C REDUCTION OF THE GENERALIZED SYMMETRIC EIGENPROBLEM A X = B X LAMBDA  GSY00030
C TO STANDARD FORM WHEN B IS POSITIVE-DEFINITE.  THE REDUCTION USES THE GSY00040
C CHOLESKY DECOMPOSITION OF B.                                          GSY00050
C                                                                       GSY00060
      SUBROUTINE  REDUC ( LD, N, A, B, D, IERR )                        GSY00070
C                                                                       GSY00080
      INTEGER     N, LD, IERR                                           GSY00090
      REAL*8      A(LD,N), B(LD,N), D(N)                                GSY00100
C                                                                       GSY00110
C LD     E  FIRST DIMENSION ASSIGNED TO THE TWO-DIMENSIONAL ARRAY       GSY00120
C           PARAMETERS IN THE CALLING PROGRAM.                          GSY00130
C                                                                       GSY00140
C N      E  ORDER OF THE MATRICES A AND B.                              GSY00150
C           IF THE CHOLESKY LOWER-TRIANGULAR FACTOR OF B IS PROVIDED TO GSY00160
C           THE SUBROUTINE (SEE THE DESCRIPTION OF ARRAYS B AND D       GSY00170
C           BELOW), N SHOULD BE SET TO MINUS THE ORDER OF A AND B.      GSY00180
C                                                                       GSY00190
C A      E  REAL SYMMETRIC MATRIX.  ONLY THE FULL UPPER-TRIANGULAR PART GSY00200
C           NEEDS TO BE SUPPLIED.                                       GSY00210
C        R  FULL LOWER-TRIANGULAR PART OF THE MATRIX OF THE EQUIVALENT  GSY00220
C           STANDARD PROBLEM IN THE CORRESPONDING PART OF THE ARRAY.    GSY00230
C           THE SUPERDIAGONAL PART OF A IS PRESERVED.                   GSY00240
C                                                                       GSY00250
C B      E  REAL SYMMETRIC POSITIVE-DEFINITE MATRIX. ONLY THE FULL UPPERGSY00260
C           TRIANGULAR PART NEEDS TO BE SUPPLIED.                       GSY00270
C           WHEN N IS NEGATIVE, THE SUBDIAGONAL PART OF THE CHOLESKY    GSY00280
C           LOWER-TRIANGULAR FACTOR OF B MUST BE IN THE SUBDIAGONAL     GSY00290
C           PART OF THE ARRAY.                                          GSY00300
C        R  SUBDIAGONAL PART OF THE LOWER-TRIANGULAR CHOLESKY FACTOR OF GSY00310
C           B IN THE CORRESPONDING CELLS OF THE ARRAY.                  GSY00320
C           THE FULL UPPER-TRIANGULAR PART OF THE ARRAY IS PRESERVED.   GSY00330
C                                                                       GSY00340
C D       E  DIAGONAL OF THE CHOLESKY FACTOR OF B WHEN N IS NEGATIVE.   GSY00350
C         R  DIAGONAL OF THE CHOLESKY FACTOR OF B.                      GSY00360
C                                                                       GSY00370
C IERR    R  ZERO    FOR NORMAL RETURN,                                 GSY00380
C            7*N+1   IF B IS NOT FOUND TO BE POSITIVE-DEFINITE.         GSY00390
C                                                                       GSY00400
C     THIS SUBROUTINE IS BASED ON THE ALGOL PROCEDURE REDUC1, NUM. MATH.GSY00410
C     11, 99-110(1968) BY MARTIN AND WILKINSON.                         GSY00420
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 303-314(1971).   GSY00430
C                                                                       GSY00440
C     VERSION FOR THE IBM 3090VF DATED NOVEMBER 1987.                   GSY00450
C     ------------------------------------------------------------------GSY00460
      INTEGER     I, J, K, M                                            GSY00470
      REAL*8      X, Y                                                  GSY00480
C                                                                       GSY00490
      CALL XUFLOW(0)                                                    GSY00500
      IERR=0                                                            GSY00510
      M=IABS(N)                                                         GSY00520
      IF (N.LT.0) GO TO 100                                             GSY00530
C COMPUTATION OF L IN THE ARRAYS B AND D                                GSY00540
      D(1)=B(1,1)                                                       GSY00550
         DO 20 J=2,N                                                    GSY00560
         B(J,1)=B(1,J)                                                  GSY00570
         D(J)=B(J,J)                                                    GSY00580
            DO 10 I=J+1,N                                               GSY00590
10          B(I,J)=0D0                                                  GSY00600
20       CONTINUE                                                       GSY00610
         DO 60 K=1,N-1                                                  GSY00620
         IF(B(K,K).LE.0D0) GO TO 999                                    GSY00630
         X=DSQRT(B(K,K))                                                GSY00640
         B(K,K)=D(K)                                                    GSY00650
         D(K)=X                                                         GSY00660
            DO 30 J=K+1,N                                               GSY00670
            B(J,K)=B(J,K)/X                                             GSY00680
30          B(J,K+1)=B(J,K+1)-B(K+1,K)*B(J,K)                           GSY00690
            DO 50 J=K+2,N                                               GSY00700
            B(J,K+1)=B(J,K+1)+B(K+1,J)                                  GSY00710
            Y=-B(J,K)                                                   GSY00720
               DO 40 I=J,N                                              GSY00730
40             B(I,J)=B(I,J)+Y*B(I,K)                                   GSY00740
50          CONTINUE                                                    GSY00750
60       CONTINUE                                                       GSY00760
      IF(B(N,N).LE.0D0) GO TO 999                                       GSY00770
      X=DSQRT(B(N,N))                                                   GSY00780
      B(N,N)=D(N)                                                       GSY00790
      D(N)=X                                                            GSY00800
C TRANSPOSE OF THE UPPER TRIANGLE OF INV(L)*A IN THE LOWER-TRIANGULAR   GSY00810
C PART OF ARRAY A                                                       GSY00820
100      DO 200 I=1,M                                                   GSY00830
         Y=D(I)                                                         GSY00840
            DO 180 J=I,M                                                GSY00850
            X=A(I,J)                                                    GSY00860
               DO 160 K=1,I-1                                           GSY00870
160            X=X-B(I,K)*A(J,K)                                        GSY00880
180         A(J,I)=X/Y                                                  GSY00890
200      CONTINUE                                                       GSY00900
C PRE-MULTIPLICATION BY INV(L)                                          GSY00910
         DO 300 J=1,M                                                   GSY00920
            DO 280 I=J,M                                                GSY00930
            X=A(I,J)                                                    GSY00940
               DO 220 K=J,I-1                                           GSY00950
220            X=X-A(K,J)*B(I,K)                                        GSY00960
               DO 260 K=1,J-1                                           GSY00970
260            X=X-A(J,K)*B(I,K)                                        GSY00980
280         A(I,J)=X/D(I)                                               GSY00990
300      CONTINUE                                                       GSY01000
      GO TO 1000                                                        GSY01010
C ERROR: B IS NOT POSITIVE DEFINITE                                     GSY01020
999   IERR=7*N+1                                                        GSY01030
1000  RETURN                                                            GSY01040
      END                                                               GSY01050
