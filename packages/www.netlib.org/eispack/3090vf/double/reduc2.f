C-----------------------------------------------------------------------GSY01060
C REDUCTION OF THE GENERALIZED SYMMETRIC EIGENPROBLEMS A B X=X LAMBDA   GSY01070
C OR  B A X=X LAMBDA  TO STANDARD FORM WHEN B IS POSITIVE-DEFINITE.     GSY01080
C THE REDUCTION USES THE CHOLESKY DECOMBOSITION OF B.                   GSY01090
C                                                                       GSY01100
      SUBROUTINE  REDUC2 ( LD, N, A, B, D, IERR )                       GSY01110
C                                                                       GSY01120
      INTEGER     LD, N, IERR                                           GSY01130
      REAL*8      A(LD,N), B(LD,N), D(N)                                GSY01140
C                                                                       GSY01150
C LD     E  FIRST DIMENSION ASSIGNED TO THE TWO-DIMENSIONAL ARRAY       GSY01160
C           PARAMETERS IN THE CALLING PROGRAM.                          GSY01170
C                                                                       GSY01180
C N      E  ORDER OF THE MATRICES A AND B.  IF THE CHOLESKY FACTOR L OF GSY01190
C           B IS ALREADY AVAILABLE, N SHOULD BE SET TO MINUS THE ORDER  GSY01200
C           OF A AND B.                                                 GSY01210
C                                                                       GSY01220
C A      E  REAL SYMMETRIC MATRIX.  ONLY THE FULL UPPER-TRIANGULAR PART GSY01230
C           NEEDS TO BE SUPPLIED.                                       GSY01240
C        R  FULL LOWER-TRIANGULAR PART OF THE MATRIX OF THE EQUIVALENT  GSY01250
C           STANDARD PROBLEM IN THE CORRESPONDING PART OF THE ARRAY.    GSY01260
C           THE SUPERDIAGONAL PART OF THE ARRAY IS PRESERVED.           GSY01270
C                                                                       GSY01280
C B      E  REAL SYMMETRIC POSITIVE-DEFINITE MATRIX. ONLY THE FULL UPPERGSY01290
C           TRIANGULAR PART NEEDS TO BE SUPPLIED.  IF N IS NEGATIVE,    GSY01300
C           THE SUBDIAGONAL PART OF THE CHOLESKY FACTOR OF B MUST BE IN GSY01310
C           THE CORRESPONDING PART OF THE ARRAY.                        GSY01320
C        R  SUBDIAGONAL PART OF THE CHOLESKY FACTOR IN THE CORRESPONDINGGSY01330
C           PART OF THE ARRAY.  THE FULL UPPER-TRIANGULAR PART OF THE   GSY01340
C           ARRAY IS PRESERVED.                                         GSY01350
C                                                                       GSY01360
C D      E  DIAGONAL OF THE CHOLESKY BACTOR OF B IF N IS NEGATIVE       GSY01370
C        R  DIAGONAL OF THE CHOLESKY BACTOR OF B.                       GSY01380
C                                                                       GSY01390
C        IERR IS SET TO                                                 GSY01400
C IERR   R  ZERO     FOR NORMAL RETURN,                                 GSY01410
C           7*N+1    IF B IS NOT FOUND TO BE POSITIVE DEFINITE.         GSY01420
C                                                                       GSY01430
C     THIS SUBROUTINE IS BASED ON THE ALGOL PROCEDURE REDUC2, NUM. MATH.GSY01440
C     11, 99-110(1968) BY MARTIN AND WILKINSON.                         GSY01450
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 303-314(1971).   GSY01460
C                                                                       GSY01470
C     VERSION FOR THE IBM 3090VF DATED NOVEMBER 1987.                   GSY01480
C     ------------------------------------------------------------------GSY01490
      INTEGER     I, J , K, M                                           GSY01500
      REAL*8      X, Y                                                  GSY01510
C                                                                       GSY01520
      CALL XUFLOW(0)                                                    GSY01530
      IERR=0                                                            GSY01540
      M=IABS(N)                                                         GSY01550
      IF(N.LT.0) GO TO 100                                              GSY01560
C COMPUTATION OF L IN THE ARRAYS B AND D                                GSY01570
      D(1)=B(1,1)                                                       GSY01580
         DO 20 J=2,N                                                    GSY01590
         B(J,1)=B(1,J)                                                  GSY01600
         D(J)=B(J,J)                                                    GSY01610
            DO 10 I=J+1,N                                               GSY01620
10          B(I,J)=0D0                                                  GSY01630
20       CONTINUE                                                       GSY01640
         DO 60 K=1,N-1                                                  GSY01650
         IF(B(K,K).LE.0D0) GO TO 999                                    GSY01660
         X=DSQRT(B(K,K))                                                GSY01670
         B(K,K)=D(K)                                                    GSY01680
         D(K)=X                                                         GSY01690
            DO 30 J=K+1,N                                               GSY01700
            B(J,K)=B(J,K)/X                                             GSY01710
30          B(J,K+1)=B(J,K+1)-B(K+1,K)*B(J,K)                           GSY01720
            DO 50 J=K+2,N                                               GSY01730
            B(J,K+1)=B(J,K+1)+B(K+1,J)                                  GSY01740
            Y=-B(J,K)                                                   GSY01750
               DO 40 I=J,N                                              GSY01760
40             B(I,J)=B(I,J)+Y*B(I,K)                                   GSY01770
50          CONTINUE                                                    GSY01780
60       CONTINUE                                                       GSY01790
      IF(B(N,N).LE.0D0) GO TO 999                                       GSY01800
      X=DSQRT(B(N,N))                                                   GSY01810
      B(N,N)=D(N)                                                       GSY01820
      D(N)=X                                                            GSY01830
C LOWER TRIANGLE OF A*L IN ARRAY OF A                                   GSY01840
100      DO 200 I=1,M                                                   GSY01850
            DO 180 J=1,I                                                GSY01860
            X=A(J,I)*D(J)                                               GSY01870
               DO 120 K=J+1,I                                           GSY01880
120            X=X+A(K,I)*B(K,J)                                        GSY01890
               DO 160 K=I+1,M                                           GSY01900
160            X=X+A(I,K)*B(K,J)                                        GSY01910
180         A(I,J)=X                                                    GSY01920
200      CONTINUE                                                       GSY01930
C PRE-MULTIPLICATION BY TRANSPOSE(L)                                    GSY01940
         DO 300 I=1,M                                                   GSY01950
         Y=D(I)                                                         GSY01960
            DO 280 J=1,I                                                GSY01970
            X=Y*A(I,J)                                                  GSY01980
               DO 260 K=I+1,M                                           GSY01990
260            X=X+A(K,J)*B(K,I)                                        GSY02000
280         A(I,J)=X                                                    GSY02010
300      CONTINUE                                                       GSY02020
      GO TO 1000                                                        GSY02030
C ERROR: B IS NOT POSITIVE DEFINITE                                     GSY02040
999   IERR=7*N+1                                                        GSY02050
1000  RETURN                                                            GSY02060
      END                                                               GSY02070
