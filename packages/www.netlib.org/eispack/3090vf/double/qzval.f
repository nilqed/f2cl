C-----------------------------------------------------------------------QZ 05680
C EIGENVALUES OF THE GENERALIZED EIGENPROBLEM DEFINED BY A PAIR OF REAL QZ 05690
C MATRICES, ONE IN QUASI-TRIANGULAR FORM AND THE OTHER IN UPPER-        QZ 05700
C TRIANGULAR FORM.                                                      QZ 05710
C THE GENERALIZED EIGENVALUES ARE THE RATIOS OF CORRESPONDING ELEMENTS  QZ 05720
C OF TWO SETS RETURNED BY THE SUBROUTINE.  THIS IS THE THIRD PHASE OF   QZ 05730
C THE SOLUTION OF THE GENERALIZED EIGENPROBLEM BY THE QZ ALGORITHM      QZ 05740
C (AFTER CALLS TO SUBROUTINES QZHES AND QZIT, AND BEFORE A CALL TO QZVECQZ 05750
C IF NEEDED).                                                           QZ 05760
C                                                                       QZ 05770
      SUBROUTINE  QZVAL (LD,N,A,B,ALFR,ALFI,BETA,MATZ,Z)                QZ 05780
C                                                                       QZ 05790
      INTEGER     LD,N                                                  QZ 05800
      REAL*8      A(LD,N),B(LD,N),ALFR(N),ALFI(N),BETA(N),Z(LD,N)       QZ 05810
      LOGICAL     MATZ                                                  QZ 05820
C                                                                       QZ 05830
C LD     E  FIRST DIMENSION ASSIGNED TO THE TWO-DIMENSIONAL ARRAY       QZ 05840
C           PARAMETERS IN THE CALLING PROGRAM.                          QZ 05850
C                                                                       QZ 05860
C N      E  ORDER OF THE MATRICES.                                      QZ 05870
C                                                                       QZ 05880
C A      E  REAL UPPER QUASI-TRIANGULAR MATRIX.                         QZ 05890
C        R  QUASI-TRIANGULAR MATRIX WHOSE DIAGONAL BLOCKS OF ORDER 2    QZ 05900
C           CORRESPOND TO CONJUGATE PAIRS OF COMPLEX EIGENVALUES.       QZ 05910
C                                                                       QZ 05920
C B      E  REAL UPPER-TRIANGULAR MATRIX.  IN ADDITION, B(N,1) IS A     QZ 05930
C           NEGLIGIBILITY THRESHOLD (EPSB) RETURNED BY SUBROUTINE QZIT. QZ 05940
C        R  REAL UPPER-TRIANGULAR MATRIX (MODIFIED).                    QZ 05950
C           B(N,1) IS PRESERVED.                                        QZ 05960
C                                                                       QZ 05970
C ALFR,  R  REAL AND IMAGINARY PARTS OF THE DIAGONAL ELEMENTS OF THE    QZ 05980
C ALFI      UPPER-TRIANGULAR MATRIX OBTAINED FROM A BY UNITARY          QZ 05990
C           TRANSFORMATIONS.                                            QZ 06000
C                                                                       QZ 06010
C BETA   R  DIAGONAL ELEMENTS OF THE CORRESPONDING MATRIX B, REAL AND   QZ 06020
C           NONNEGATIVE.                                                QZ 06030
C           THE GENERALIZED EIGENVALUES ARE THE RATIOS                  QZ 06040
C                 (ALFR(K)+I*ALFI(K))/BETA(K),   K=1,...,N              QZ 06050
C                                                                       QZ 06060
C MATZ   E  .TRUE.   IF THE RIGHT MATRIX OF THE REDUCTION IS REQUESTED  QZ 06070
C                    FOR LATER COMPUTATION OF THE EIGENVECTORS.         QZ 06080
C           .FALSE.  OTHERWISE.                                         QZ 06090
C                                                                       QZ 06100
C Z      E  MATRIX OF THE RIGHT TRANSFORMATIONS PERFORMED BY SUBROUTINESQZ 06110
C           QZHES AND QZIT (OR THE IDENTITY MATRIX) IF MATZ IS .TRUE.   QZ 06120
C           IF(MATZ IS .FALSE.,NO REFERENCE IS MADE TO Z.               QZ 06130
C        R  MATRIX OF ALL THE RIGHT TRANSFORMATIONS IF MATZ IS .TRUE.   QZ 06140
C                                                                       QZ 06150
C     "AN ALGORITHM FOR GENERALIZED MATRIX EIGENVALUE PROBLEMS"         QZ 06160
C     BY MOLER AND STEWART,SIAM J. NUMER. ANAL. 10,241-256(1973).       QZ 06170
C                                                                       QZ 06180
C     VERSION FOR THE IBM 3090VF DATED NOVEMBER 1987.                   QZ 06190
C     ------------------------------------------------------------------QZ 06200
      INTEGER     I, J, M, ISW                                          QZ 06210
      REAL*8      C, D, E, R, S, T, AN, BN, CQ, CZ, DI, DR, EI, TI, TR  QZ 06220
      REAL*8      U1, U2, V1, V2, A1I, A11, A12, A2I, A21, A22          QZ 06230
      REAL*8      B11, B12, B22, SQI, SQR, SSI,SSR,SZI,SZR              QZ 06240
      REAL*8      A11I, A11R, A12I, A12R, A22I, A22R, EPSB              QZ 06250
C                                                                       QZ 06260
      CALL XUFLOW(0)                                                    QZ 06270
      EPSB=B(N,1)                                                       QZ 06280
      ISW=1                                                             QZ 06290
C EIGENVALUES OF THE QUASI TRIANGULAR MATRIX                            QZ 06300
         DO 510 M=N,1,-1                                                QZ 06310
         IF (ISW.EQ.2) GO TO 505                                        QZ 06320
         IF (M.EQ.1) GO TO 410                                          QZ 06330
         IF (A(M,M-1).NE.0D0) GO TO 420                                 QZ 06340
C ISOLATED REAL ROOT                                                    QZ 06350
410      ALFR(M)=A(M,M)                                                 QZ 06360
         IF (B(M,M).LT.0D0) ALFR(M)=-ALFR(M)                            QZ 06370
         BETA(M)=DABS(B(M,M))                                           QZ 06380
         ALFI(M)=0D0                                                    QZ 06390
         GO TO 510                                                      QZ 06400
C BLOCK OF ORDER 2                                                      QZ 06410
420      IF (DABS(B(M-1,M-1)).LE.EPSB) GO TO 455                        QZ 06420
         IF (DABS(B(M,M)).GT.EPSB) GO TO 430                            QZ 06430
         U1=A(M,M)                                                      QZ 06440
         U2=A(M,M-1)                                                    QZ 06450
         BN=0D0                                                         QZ 06460
         GO TO 435                                                      QZ 06470
430      AN=DMAX1(DABS(A(M-1,M-1)),DABS(A(M-1,M)),DABS(A(M,M-1)),       QZ 06480
     $            DABS(A(M,M)))                                         QZ 06490
         BN=DMAX1(DABS(B(M-1,M-1)),DABS(B(M-1,M)),DABS(B(M,M)))         QZ 06500
         A11=A(M-1,M-1)/AN                                              QZ 06510
         A12=A(M-1,M)/AN                                                QZ 06520
         A21=A(M,M-1)/AN                                                QZ 06530
         A22=A(M,M)/AN                                                  QZ 06540
         B11=B(M-1,M-1)/BN                                              QZ 06550
         B12=B(M-1,M)/BN                                                QZ 06560
         B22=B(M,M)/BN                                                  QZ 06570
         E=A11/B11                                                      QZ 06580
         EI=A22/B22                                                     QZ 06590
         S=A21/(B11*B22)                                                QZ 06600
         T=(A22-E*B22)/B22                                              QZ 06610
         IF (DABS(E).LE.DABS(EI)) GO TO 431                             QZ 06620
            E=EI                                                        QZ 06630
            T=(A11-E*B11)/B11                                           QZ 06640
431      C=0.5D0*(T-S*B12)                                              QZ 06650
         D=C*C+S*(A12-E*B12)                                            QZ 06660
         IF (D.LT.0D0) GO TO 480                                        QZ 06670
C    TWO REAL ROOTS.  A(M,M-1) AND B(M,M-1) ARE ANNIHILATED             QZ 06680
         E=E+(C+DSIGN(DSQRT(D),C))                                      QZ 06690
         A11=A11-E*B11                                                  QZ 06700
         A12=A12-E*B12                                                  QZ 06710
         A22=A22-E*B22                                                  QZ 06720
         IF (DABS(A11)+DABS(A12).LT.DABS(A21)+DABS(A22)) GO TO 432      QZ 06730
            U1=A12                                                      QZ 06740
            U2=A11                                                      QZ 06750
            GO TO 435                                                   QZ 06760
432      U1=A22                                                         QZ 06770
         U2=A21                                                         QZ 06780
C       REAL Z TRANSFORMATION                                           QZ 06790
435      S=DMAX1(DABS(U1),DABS(U2))                                     QZ 06800
         U1=U1/S                                                        QZ 06810
         U2=U2/S                                                        QZ 06820
         R=DSIGN(DSQRT(U1*U1+U2*U2),U1)                                 QZ 06830
         V1=-(U1+R)/R                                                   QZ 06840
         V2=-U2/R                                                       QZ 06850
         U2=V2/V1                                                       QZ 06860
            DO 440 I=1,M                                                QZ 06870
            T=A(I,M)+U2*A(I,M-1)                                        QZ 06880
            A(I,M)=A(I,M)+T*V1                                          QZ 06890
            A(I,M-1)=A(I,M-1)+T*V2                                      QZ 06900
            T=B(I,M)+U2*B(I,M-1)                                        QZ 06910
            B(I,M)=B(I,M)+T*V1                                          QZ 06920
            B(I,M-1)=B(I,M-1)+T*V2                                      QZ 06930
440         CONTINUE                                                    QZ 06940
         IF (.NOT.MATZ) GO TO 450                                       QZ 06950
            DO 445 I=1,N                                                QZ 06960
            T=Z(I,M)+U2*Z(I,M-1)                                        QZ 06970
            Z(I,M)=Z(I,M)+T*V1                                          QZ 06980
            Z(I,M-1)=Z(I,M-1)+T*V2                                      QZ 06990
445         CONTINUE                                                    QZ 07000
450      IF (BN.EQ.0D0) GO TO 475                                       QZ 07010
         IF (AN.LT.DABS(E)*BN) GO TO 455                                QZ 07020
            U1=B(M-1,M-1)                                               QZ 07030
            U2=B(M,M-1)                                                 QZ 07040
            GO TO 460                                                   QZ 07050
455      U1=A(M-1,M-1)                                                  QZ 07060
         U2=A(M,M-1)                                                    QZ 07070
C       REAL Q TRANSFORMATION                                           QZ 07080
460      S=DMAX1(DABS(U1),DABS(U2))                                     QZ 07090
         IF (S.EQ.0D0) GO TO 475                                        QZ 07100
         U1=U1/S                                                        QZ 07110
         U2=U2/S                                                        QZ 07120
         R=DSIGN(DSQRT(U1*U1+U2*U2),U1)                                 QZ 07130
         V1=-(U1+R)/R                                                   QZ 07140
         V2=-U2/R                                                       QZ 07150
         U2=V2/V1                                                       QZ 07160
            DO 470 J=M-1,N                                              QZ 07170
            T=A(M-1,J)+U2*A(M,J)                                        QZ 07180
            A(M-1,J)=A(M-1,J)+T*V1                                      QZ 07190
            A(M,J)=A(M,J)+T*V2                                          QZ 07200
            T=B(M-1,J)+U2*B(M,J)                                        QZ 07210
            B(M-1,J)=B(M-1,J)+T*V1                                      QZ 07220
            B(M,J)=B(M,J)+T*V2                                          QZ 07230
470         CONTINUE                                                    QZ 07240
475      A(M,M-1)=0D0                                                   QZ 07250
         B(M,M-1)=0D0                                                   QZ 07260
         ALFR(M-1)=A(M-1,M-1)                                           QZ 07270
         ALFR(M)=A(M,M)                                                 QZ 07280
         IF (B(M-1,M-1).LT.0D0) ALFR(M-1)=-ALFR(M-1)                    QZ 07290
         IF (B(M,M).LT.0D0) ALFR(M)=-ALFR(M)                            QZ 07300
         BETA(M-1)=DABS(B(M-1,M-1))                                     QZ 07310
         BETA(M)=DABS(B(M,M))                                           QZ 07320
         ALFI(M)=0D0                                                    QZ 07330
         ALFI(M-1)=0D0                                                  QZ 07340
         GO TO 505                                                      QZ 07350
C    TWO COMPLEX ROOTS                                                  QZ 07360
480      E=E+C                                                          QZ 07370
         EI=DSQRT(-D)                                                   QZ 07380
         A11R=A11-E*B11                                                 QZ 07390
         A11I=EI*B11                                                    QZ 07400
         A12R=A12-E*B12                                                 QZ 07410
         A12I=EI*B12                                                    QZ 07420
         A22R=A22-E*B22                                                 QZ 07430
         A22I=EI*B22                                                    QZ 07440
         IF (DABS(A11R)+DABS(A11I)+DABS(A12R)+DABS(A12I).LT.            QZ 07450
     $       DABS(A21)+DABS(A22R)+DABS(A22I)) GO TO 482                 QZ 07460
            U1=A12R                                                     QZ 07470
            A1I=A12I                                                    QZ 07480
            U2=-A11R                                                    QZ 07490
            A2I=-A11I                                                   QZ 07500
            GO TO 485                                                   QZ 07510
482      U1=A22R                                                        QZ 07520
         A1I=A22I                                                       QZ 07530
         U2=-A21                                                        QZ 07540
         A2I=0D0                                                        QZ 07550
C       COMPLEX Z TRANSFORMATION                                        QZ 07560
485      CZ=DSQRT(U1*U1+A1I*A1I)                                        QZ 07570
         IF (CZ.EQ.0D0) GO TO 487                                       QZ 07580
            SZR=(U1*U2+A1I*A2I)/CZ                                      QZ 07590
            SZI=(U1*A2I-A1I*U2)/CZ                                      QZ 07600
            R=DSQRT(CZ*CZ+SZR*SZR+SZI*SZI)                              QZ 07610
            CZ=CZ/R                                                     QZ 07620
            SZR=SZR/R                                                   QZ 07630
            SZI=SZI/R                                                   QZ 07640
            GO TO 490                                                   QZ 07650
487      SZR=1D0                                                        QZ 07660
         SZI=0D0                                                        QZ 07670
490      IF (AN.LT.(DABS(E)+EI)*BN) GO TO 492                           QZ 07680
         U1=CZ*B11+SZR*B12                                              QZ 07690
         A1I=SZI*B12                                                    QZ 07700
         U2=SZR*B22                                                     QZ 07710
         A2I=SZI*B22                                                    QZ 07720
         GO TO 495                                                      QZ 07730
492      U1=CZ*A11+SZR*A12                                              QZ 07740
         A1I=SZI*A12                                                    QZ 07750
         U2=CZ*A21+SZR*A22                                              QZ 07760
         A2I=SZI*A22                                                    QZ 07770
C       COMPLEX Q TRANSFORMATION                                        QZ 07780
495      CQ=DSQRT(U1*U1+A1I*A1I)                                        QZ 07790
         IF (CQ.EQ.0D0) GO TO 497                                       QZ 07800
         SQR=(U1*U2+A1I*A2I)/CQ                                         QZ 07810
         SQI=(U1*A2I-A1I*U2)/CQ                                         QZ 07820
         R=DSQRT(CQ*CQ+SQR*SQR+SQI*SQI)                                 QZ 07830
         CQ=CQ/R                                                        QZ 07840
         SQR=SQR/R                                                      QZ 07850
         SQI=SQI/R                                                      QZ 07860
         GO TO 500                                                      QZ 07870
497      SQR=1D0                                                        QZ 07880
         SQI=0D0                                                        QZ 07890
C      DIAGONAL ELEMENTS THAT WOULD RESULT FROM THE TRANSFORMATIONS     QZ 07900
500      SSR=SQR*SZR+SQI*SZI                                            QZ 07910
         SSI=SQR*SZI-SQI*SZR                                            QZ 07920
         I=1                                                            QZ 07930
         TR=CQ*CZ*A11+CQ*SZR*A12+SQR*CZ*A21+SSR*A22                     QZ 07940
         TI=CQ*SZI*A12-SQI*CZ*A21+SSI*A22                               QZ 07950
         DR=CQ*CZ*B11+CQ*SZR*B12+SSR*B22                                QZ 07960
         DI=CQ*SZI*B12+SSI*B22                                          QZ 07970
         GO TO 503                                                      QZ 07980
502      I=2                                                            QZ 07990
         TR=SSR*A11-SQR*CZ*A12-CQ*SZR*A21+CQ*CZ*A22                     QZ 08000
         TI=-SSI*A11-SQI*CZ*A12+CQ*SZI*A21                              QZ 08010
         DR=SSR*B11-SQR*CZ*B12+CQ*CZ*B22                                QZ 08020
         DI=-SSI*B11-SQI*CZ*B12                                         QZ 08030
503      T=TI*DR-TR*DI                                                  QZ 08040
         J=M-1                                                          QZ 08050
         IF (T.LT.0D0) J=M                                              QZ 08060
         R=DSQRT(DR*DR+DI*DI)                                           QZ 08070
         BETA(J)=BN*R                                                   QZ 08080
         ALFR(J)=AN*(TR*DR+TI*DI)/R                                     QZ 08090
         ALFI(J)=AN*T/R                                                 QZ 08100
         IF (I.EQ.1) GO TO 502                                          QZ 08110
505      ISW=3-ISW                                                      QZ 08120
510      CONTINUE                                                       QZ 08130
      B(N,1)=EPSB                                                       QZ 08140
C                                                                       QZ 08150
      RETURN                                                            QZ 08160
      END                                                               QZ 08170
