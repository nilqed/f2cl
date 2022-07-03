@PROCESS DIRECTIVE('"    (')                                            QZ 00020
C-----------------------------------------------------------------------QZ 00030
C SIMULTANEOUS REDUCTIONS OF ONE MATRIX TO HESSENBERG FORM AND ANOTHER  QZ 00040
C TO UPPER-TRIANGULAR FORM BY ORTHOGONAL TRANSFORMATIONS, AS A FIRST    QZ 00050
C STEP TOWARDS THE SOLUTION OF THE GENERALIZED REAL MATRIX EIGENPROBLEM QZ 00060
C (SUBROUTINES QZIT, QZVAL, AND QZVEC PERFORM THE OTHER PHASES OF THE   QZ 00070
C COMPUTATION).                                                         QZ 00080
C                                                                       QZ 00090
      SUBROUTINE  QZHES ( LD, N, A, B, MATZ, Z )                        QZ 00100
C                                                                       QZ 00110
      INTEGER     LD, N                                                 QZ 00120
      REAL*8      A(LD,N), B(LD,N), Z(LD,N)                             QZ 00130
      LOGICAL     MATZ                                                  QZ 00140
C                                                                       QZ 00150
C LD     E  FIRST DIMENSION ASSIGNED TO THE TWO-DIMENSIONAL ARRAY       QZ 00160
C           PARAMETERS IN THE CALLING PROGRAM.                          QZ 00170
C                                                                       QZ 00180
C N      E  ORDER OF THE MATRICES.                                      QZ 00190
C                                                                       QZ 00200
C A      E  REAL MATRIX.                                                QZ 00210
C        R  UPPER-HESSENBERG MATRIX.                                    QZ 00220
C                                                                       QZ 00230
C B      E  REAL MATRIX.                                                QZ 00240
C        R  UPPER-TRIANGULAR MATRIX.                                    QZ 00250
C                                                                       QZ 00260
C MATZ   E  .TRUE.   IF THE RIGHT MATRIX OF THE REDUCTION IS REQUESTED  QZ 00270
C                    FOR LATER COMPUTATION OF THE EIGENVECTORS.         QZ 00280
C           .FALSE.  OTHERWISE.                                         QZ 00290
C                                                                       QZ 00300
C Z      R  RIGHT MATRIX OF THE REDUCTION IF MATZ=.TRUE.                QZ 00310
C           NO REFERENCE IS MADE TO Z IF  MATZ=.FALSE.                  QZ 00320
C                                                                       QZ 00330
C     "AN ALGORITHM FOR GENERALIZED MATRIX EIGENVALUE PROBLEMS"         QZ 00340
C      BY MOLER AND STEWART, SIAM J. NUMER. ANAL. 10, 241-256(1973).    QZ 00350
C                                                                       QZ 00360
C SUBPROGRAMS CALLED:  DSC16                                            QZ 00370
C                                                                       QZ 00380
C     VERSION FOR THE IBM 3090VF DATED NOVEMBER 1987.                   QZ 00390
C     ------------------------------------------------------------------QZ 00400
      INTEGER     I, J, K                                               QZ 00410
      REAL*8      P, Q, R, S                                            QZ 00420
      LOGICAL     NOZ                                                   QZ 00430
C                                                                       QZ 00440
      CALL XUFLOW(0)                                                    QZ 00450
      NOZ=.NOT.MATZ                                                     QZ 00460
C INITIALIZATION OF THE MATRIX OF Z TRANSFORMATIONS                     QZ 00470
      IF(NOZ) GO TO 140                                                 QZ 00480
         DO 120 I=1,N                                                   QZ 00490
            DO 100 J=1,N                                                QZ 00500
100         Z(I,J)=0D0                                                  QZ 00510
120      Z(I,I)=1D0                                                     QZ 00520
C REDUCTION OF B TO UPPER TRIANGULAR FORM (Q TRANSFORMATION)            QZ 00530
140      DO 360 K=1,N-1                                                 QZ 00540
         S=0D0                                                          QZ 00550
            DO 160 I=K+1,N                                              QZ 00560
            S=S+DABS(B(I,K))                                            QZ 00570
160         CONTINUE                                                    QZ 00580
         IF (S.EQ.0D0) GO TO 360                                        QZ 00590
         S=S+DABS(B(K,K))                                               QZ 00600
         CALL DSC16(S,Q,R)                                              QZ 00610
C     SCALED HOUSEHOLDER VECTOR                                         QZ 00620
         S=0D0                                                          QZ 00630
            DO 180 I=K,N                                                QZ 00640
            B(I,K)=R*B(I,K)                                             QZ 00650
180         S=S+B(I,K)**2                                               QZ 00660
         S=DSIGN(DSQRT(S),-B(K,K))                                      QZ 00670
         Q=Q*S                                                          QZ 00680
         P=B(K,K)-S                                                     QZ 00690
         B(K,K)=1D0                                                     QZ 00700
         R=P/S                                                          QZ 00710
         P=1D0/P                                                        QZ 00720
            DO 200 I=K+1,N                                              QZ 00730
200         B(I,K)=P*B(I,K)                                             QZ 00740
C     TRANSFORMATION OF B                                               QZ 00750
            DO 260 J=K+1,N                                              QZ 00760
            S=0D0                                                       QZ 00770
               DO 220 I=K,N                                             QZ 00780
220            S=S+B(I,K)*B(I,J)                                        QZ 00790
            S=R*S                                                       QZ 00800
               DO 240 I=K,N                                             QZ 00810
240            B(I,J)=B(I,J)+S*B(I,K)                                   QZ 00820
260         CONTINUE                                                    QZ 00830
C     TRANSFORMATION OF A                                               QZ 00840
            DO 320 J=1,N                                                QZ 00850
            S=0D0                                                       QZ 00860
               DO 280 I=K,N                                             QZ 00870
280            S=S+B(I,K)*A(I,J)                                        QZ 00880
            S=R*S                                                       QZ 00890
               DO 300 I=K,N                                             QZ 00900
300            A(I,J)=A(I,J)+S*B(I,K)                                   QZ 00910
320         CONTINUE                                                    QZ 00920
C     SUBDIAGONAL ELEMENTS OF B ARE ZERO                                QZ 00930
         B(K,K)=Q                                                       QZ 00940
            DO 340 I=K+1,N                                              QZ 00950
340         B(I,K)=0D0                                                  QZ 00960
360      CONTINUE                                                       QZ 00970
C REDUCTION OF A TO HESSENBERG FORM                                     QZ 00980
         DO 560 K=1,N-2                                                 QZ 00990
C     REDUCTION OF COLUMN K (Q TRANSFORMATION)                          QZ 01000
            DO 380 I=N,K+2,-1                                           QZ 01010
            Q=A(I,K)                                                    QZ 01020
            IF(Q.EQ.0D0) GO TO 380                                      QZ 01030
            P=A(I-1,K)                                                  QZ 01040
            S=DMAX1(DABS(P),DABS(Q))                                    QZ 01050
            P=P/S                                                       QZ 01060
            Q=Q/S                                                       QZ 01070
            R=DSIGN(DSQRT(P*P+Q*Q),-P)                                  QZ 01080
            A(I-1,K)=S*R                                                QZ 01090
            P=P-R                                                       QZ 01100
            Q=Q/P                                                       QZ 01110
            P=P/R                                                       QZ 01120
            A(I,K)=P                                                    QZ 01130
            B(I,K)=Q                                                    QZ 01140
380         CONTINUE                                                    QZ 01150
C     Q TRANSFORMATION OF A AND B                                       QZ 01160
            DO 440 J=K+1,N                                              QZ 01170
               DO 400 I=N,K+2,-1                                        QZ 01180
               P=(A(I-1,J)+B(I,K)*A(I,J))*A(I,K)                        QZ 01190
               A(I-1,J)=A(I-1,J)+P                                      QZ 01200
400            A(I,J)  =A(I,J)  +P*B(I,K)                               QZ 01210
               DO 420 I=MIN0(J+1,N),K+2,-1                              QZ 01220
               P=(B(I-1,J)+B(I,K)*B(I,J))*A(I,K)                        QZ 01230
               B(I-1,J)=B(I-1,J)+P                                      QZ 01240
420            B(I,J)  =B(I,J)  +P*B(I,K)                               QZ 01250
440         CONTINUE                                                    QZ 01260
C     ZEROS RESTORED IN COLUMN K OF A AND B                             QZ 01270
            DO 460 I=K+2,N                                              QZ 01280
            A(I,K)=0D0                                                  QZ 01290
460         B(I,K)=0D0                                                  QZ 01300
C REDUCTION OF B TO UPPER-TRIANGULAR FORM (Z TRANSFORMATION)            QZ 01310
            DO 540 J=N,K+2,-1                                           QZ 01320
            Q=B(J,J-1)                                                  QZ 01330
            IF(Q.EQ.0D0) GO TO 540                                      QZ 01340
            P=B(J,J)                                                    QZ 01350
            S=DMAX1(DABS(P),DABS(Q))                                    QZ 01360
            P=P/S                                                       QZ 01370
            Q=Q/S                                                       QZ 01380
            R=DSIGN(DSQRT(P*P+Q*Q),-P)                                  QZ 01390
            B(J,J)=S*R                                                  QZ 01400
            B(J,J-1)=0D0                                                QZ 01410
            P=P-R                                                       QZ 01420
            Q=Q/P                                                       QZ 01430
            P=P/R                                                       QZ 01440
C     Z TRANSFORMATION OF B                                             QZ 01450
C"    ( IGNORE RECRDEPS                                                 QZ 01460
               DO 480 I=1,J-1                                           QZ 01470
               S=(B(I,J)+B(I,J-1)*Q)*P                                  QZ 01480
               B(I,J)=B(I,J)+S                                          QZ 01490
480            B(I,J-1)=B(I,J-1)+S*Q                                    QZ 01500
C     Z TRANSFORMATION OF A                                             QZ 01510
C"    ( IGNORE RECRDEPS                                                 QZ 01520
               DO 500 I=1,N                                             QZ 01530
               S=(A(I,J)+A(I,J-1)*Q)*P                                  QZ 01540
               A(I,J)=A(I,J)+S                                          QZ 01550
500            A(I,J-1)=A(I,J-1)+S*Q                                    QZ 01560
C     UPDATE OF THE MATRIX OF Z TRANSFORMATIONS                         QZ 01570
            IF(NOZ) GO TO 540                                           QZ 01580
               DO 520 I=1,N                                             QZ 01590
               S=(Z(I,J)+Z(I,J-1)*Q)*P                                  QZ 01600
               Z(I,J)=Z(I,J)+S                                          QZ 01610
520            Z(I,J-1)=Z(I,J-1)+S*Q                                    QZ 01620
540         CONTINUE                                                    QZ 01630
560      CONTINUE                                                       QZ 01640
      RETURN                                                            QZ 01650
      END                                                               QZ 01660
