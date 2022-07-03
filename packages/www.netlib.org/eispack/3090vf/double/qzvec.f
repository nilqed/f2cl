@PROCESS DIRECTIVE('"    (')                                            QZ 08180
C-----------------------------------------------------------------------QZ 08190
C COMPUTATION OF THE EIGENVECTORS OF THE GENERALIZED EIGENPROBLEM FROM  QZ 08200
C THE RESULTS RETURNED BY SUBROUTINE QZVAL.  THIS IS THE LAST PHASE OF  QZ 08210
C THE SOLUTION OF THE COMPLETE GENERALIZED EIGENPROBLEM BY THE QZ       QZ 08220
C ALGORITHM (FOLLOWING CALLS TO SUBROUTINES QZHES, QZIT, AND QZVAL).    QZ 08230
C                                                                       QZ 08240
      SUBROUTINE  QZVEC ( LD, N, A, B, ALFR,ALFI, BETA, Z )             QZ 08250
C                                                                       QZ 08260
      INTEGER     LD, N                                                 QZ 08270
      REAL*8      A(LD,N), B(LD,N), ALFR(N),ALFI(N), BETA(N), Z(LD,N)   QZ 08280
C                                                                       QZ 08290
C LD     E  FIRST DIMENSION ASSIGNED TO THE TWO-DIMENSIONAL ARRAY       QZ 08300
C           PARAMETERS IN THE CALLING PROGRAM.                          QZ 08310
C                                                                       QZ 08320
C N      E  ORDER OF THE MATRICES.                                      QZ 08330
C                                                                       QZ 08340
C A      E  REAL UPPER QUASI-TRIANGULAR MATRIX RETURNED BY QZVAL.       QZ 08350
C                                                                       QZ 08360
C B      E  REAL UPPER-TRIANGULAR MATRIX RETURNED BY QZVAL. IN ADDITION,QZ 08370
C           B(N,1) IS A NEGLIGIBILITY THRESHOLD (EPSB) RETURNED BY      QZ 08380
C           QZIT AND QZVAL.                                             QZ 08390
C        R  THE MATRIX IS NOT PRESERVED.                                QZ 08400
C                                                                       QZ 08410
C ALFR,  E  REAL AND IMAGINARY PARTS OF THE DIAGONAL ELEMENTS OBTAINED  QZ 08420
C ALFI      FROM A IN A TRIANGULARIZATION OF A PRESERVING THE SHAPE OF  QZ 08430
C           B, AS RETURNED BY QZVAL.                                    QZ 08440
C                                                                       QZ 08450
C BETA   E  DIAGONAL ELEMENTS OBTAINED FROM B IN A TRIANGULARIZATION    QZ 08460
C           OF A PRESERVING THE SHAPE OF B, AS RETURNED BY QZVAL.       QZ 08470
C                 THE GENERALIZED EIGENVALUES ARE THE RATIOS            QZ 08480
C                           (ALFR+I*ALFI)/BETA                          QZ 08490
C                                                                       QZ 08500
C V      E  MATRIX OF TRANSFORMATIONS RETURNED BY QZVAL IF MATZ=.TRUE.. QZ 08510
C           IF THE EIGENVECTORS OF THE TRIANGULAR PROBLEM ARE DESIRED,  QZ 08520
C           Z MUST BE THE IDENTITY MATRIX.                              QZ 08530
C        R  REAL AND IMAGINARY PARTS OF THE EIGENVECTORS.               QZ 08540
C           IF ALFI(I) IS ZERO, THE I-TH EIGENVALUE IS REAL AND THE I-THQZ 08550
C              COLUMN OF Z IS THE CORRESPONDING EIGENVECTOR.            QZ 08560
C           IF ALFI(I) IS NOT 0, THE I-TH EIGENVALUE IS COMPLEX.        QZ 08570
C           -  IF ALFI(I) IS POSITIVE, THE EIGENVALUE IS THE FIRST OF   QZ 08580
C              A COMPLEX PAIR AND COLUMNS I AND I+1 OF Z ARE THE REAL   QZ 08590
C              AND IMAGINARY PARTS OF THE ASSOCIATED EIGENVECTOR.       QZ 08600
C           -  IF ALFI(I) IS NEGATIVE, THE EIGENVALUE IS THE SECOND OF  QZ 08610
C              A COMPLEX PAIR AND COLUMNS I-1 AND I OF Z ARE THE REAL   QZ 08620
C              AND IMAGINARY PARTS OF THE CONJUGATE OF THE EIGENVECTOR. QZ 08630
C           EACH EIGENVECTOR IS NORMALIZED SO THAT THE MODULUS          QZ 08640
C           OF ITS LARGEST COMPONENT IS UNITY.                          QZ 08650
C                                                                       QZ 08660
C SUBPROGRAMS CALLED:  DSC16                                            QZ 08670
C                                                                       QZ 08680
C     "AN ALGORITHM FOR GENERALIZED MATRIX EIGENVALUE PROBLEMS"         QZ 08690
C     BY MOLER AND STEWART, SIAM J. NUMER. ANAL. 10, 241-256(1973).     QZ 08700
C                                                                       QZ 08710
C     VERSION FOR THE IBM 3090VF DATED NOVEMBER 1987.                   QZ 08720
C     ------------------------------------------------------------------QZ 08730
      INTEGER     I, J, K, M                                            QZ 08740
      REAL*8      EPSB, C, D, E, F, G, H, P, Q, R, S, T, U, V, W, X, Y  QZ 08750
      REAL*8      ZR, ZI                                                QZ 08760
C                                                                       QZ 08770
      CALL XUFLOW(0)                                                    QZ 08780
      EPSB=B(N,1)                                                       QZ 08790
         DO 640 M=N,1,-1                                                QZ 08800
         IF(ALFI(M)) 320, 100, 640                                      QZ 08810
C REAL VECTOR                                                           QZ 08820
100      P=ALFR(M)                                                      QZ 08830
         R=BETA(M)                                                      QZ 08840
C     INITIAL RIGHT-HAND SIDE                                           QZ 08850
            DO 120 I=1,M-1                                              QZ 08860
120         B(I,M)=P*B(I,M)-R*A(I,M)                                    QZ 08870
         B(M,M)=1D0                                                     QZ 08880
C     BACKWARD SUBSTITUTION                                             QZ 08890
            DO 300 K=M-1,1,-1                                           QZ 08900
            IF(ALFI(K)) 180, 160, 140                                   QZ 08910
C        SECOND OF COUPLED COMPONENTS (DIAGONAL BLOCK OF ORDER 2 IN A)  QZ 08920
140         B(K,M)=X                                                    QZ 08930
            GO TO 220                                                   QZ 08940
C        ISOLATED COMPONENT                                             QZ 08950
160         D=R*A(K,K)-P*B(K,K)                                         QZ 08960
            IF(D.EQ.0D0) D=EPSB                                         QZ 08970
            B(K,M)=B(K,M)/D                                             QZ 08980
            GO TO 220                                                   QZ 08990
C        FIRST (LOWER) OF COUPLED COMPONENTS (CRAMER'S RULE)            QZ 09000
180         C=R*A(K-1,K-1)-P*B(K-1,K-1)                                 QZ 09010
            D=R*A(K-1,K)-P*B(K-1,K)                                     QZ 09020
            E=R*A(K,K-1)                                                QZ 09030
            F=R*A(K,K)-P*B(K,K)                                         QZ 09040
            IF(DABS(D).LT.DABS(F)) GO TO 200                            QZ 09050
            IF(D.EQ.0D0) D=EPSB                                         QZ 09060
               F=F/D                                                    QZ 09070
               X=(F*B(K-1,M)-B(K,M))/(C*F-E)                            QZ 09080
               B(K,M)=(B(K-1,M)-C*X)/D                                  QZ 09090
               GO TO 220                                                QZ 09100
200         D=D/F                                                       QZ 09110
            X=(B(K-1,M)-D*B(K,M))/(C-D*E)                               QZ 09120
            B(K,M)=(B(K,M)-E*X)/F                                       QZ 09130
C        SCALING (IF NEEDED) AND DEFLATION OF THE RIGHT-HAND SIDE       QZ 09140
220         C=DABS(B(K,M))                                              QZ 09150
            IF(C.EQ.0D0) GO TO 300                                      QZ 09160
            D=1D0/C                                                     QZ 09170
            E=C+D                                                       QZ 09180
            IF(E.GT.C) GO TO 260                                        QZ 09190
               DO 240 I=1,M                                             QZ 09200
240            B(I,M)=D*B(I,M)                                          QZ 09210
            IF(ALFI(K).LT.0D0) X=D*X                                    QZ 09220
C"    ( IGNORE RECRDEPS                                                 QZ 09230
260            DO 280 I=1,K-1                                           QZ 09240
280            B(I,M)=B(I,M)+B(K,M)*(P*B(I,K)-R*A(I,K))                 QZ 09250
300         CONTINUE                                                    QZ 09260
         GO TO 640                                                      QZ 09270
C COMPLEX VECTOR                                                        QZ 09280
320      P=ALFR(M-1)                                                    QZ 09290
         Q=ALFI(M-1)                                                    QZ 09300
         R=BETA(M-1)                                                    QZ 09310
C     LAST TWO COMPONENTS AND INITIAL RIGHT-HAND SIDE                   QZ 09320
         C=R*A(M,M-1)                                                   QZ 09330
         IF(C.EQ.0D0) C=EPSB                                            QZ 09340
         B(M-1,M-1)=-Q*B(M,M)/C                                         QZ 09350
         B(M-1,M)=(P*B(M,M)-R*A(M,M))/C                                 QZ 09360
         B(M,M-1)=0D0                                                   QZ 09370
         B(M,M)=1D0                                                     QZ 09380
C"    ( IGNORE RECRDEPS                                                 QZ 09390
            DO 340 I=1,M-2                                              QZ 09400
            C=P*B(I,M-1)-R*A(I,M-1)                                     QZ 09410
            D=Q*B(I,M-1)                                                QZ 09420
            B(I,M-1)=B(M-1,M-1)*C-B(M-1,M)*D-Q*B(I,M)                   QZ 09430
340         B(I,M)  =B(M-1,M-1)*D+B(M-1,M)*C+P*B(I,M)-R*A(I,M)          QZ 09440
C     BACKWARD SUBSTITUTION                                             QZ 09450
            DO 620 K=M-2,1,-1                                           QZ 09460
            IF(ALFI(K)) 420, 380, 360                                   QZ 09470
C        SECOND OF COUPLED COMPONENTS (DIAGONAL BLOCK OF ORDER 2 IN A)  QZ 09480
360         ZR=X                                                        QZ 09490
            ZI=Y                                                        QZ 09500
            GO TO 520                                                   QZ 09510
C        ISOLATED COMPONENT                                             QZ 09520
380         C=R*A(K,K)-P*B(K,K)                                         QZ 09530
            D=-Q*B(K,K)                                                 QZ 09540
            X=B(K,M-1)                                                  QZ 09550
            Y=B(K,M)                                                    QZ 09560
            IF(DABS(C).LT.DABS(D)) GO TO 400                            QZ 09570
               IF(C.EQ.0D0) C=EPSB                                      QZ 09580
               E=D/C                                                    QZ 09590
               F=C+D*E                                                  QZ 09600
               ZR=(X+Y*E)/F                                             QZ 09610
               ZI=(Y-X*E)/F                                             QZ 09620
               GO TO 520                                                QZ 09630
400         E=C/D                                                       QZ 09640
            F=C*E+D                                                     QZ 09650
            ZR=(X*E+Y)/F                                                QZ 09660
            ZI=(Y*E-X)/F                                                QZ 09670
            GO TO 520                                                   QZ 09680
C        FIRST (LOWER) OF COUPLED COMPONENTS (CRAMER'S RULE)            QZ 09690
420         C=R*A(K-1,K-1)-P*B(K-1,K-1)                                 QZ 09700
            D=-Q*B(K-1,K-1)                                             QZ 09710
            E=R*A(K-1,K)-P*B(K-1,K)                                     QZ 09720
            F=-Q*B(K-1,K)                                               QZ 09730
            W=R*A(K,K-1)                                                QZ 09740
            G=R*A(K,K)-P*B(K,K)                                         QZ 09750
            H=-Q*B(K,K)                                                 QZ 09760
            S=C*G-D*H-E*W                                               QZ 09770
            T=D*G+C*H-F*W                                               QZ 09780
            X=C*B(K,M-1)-D*B(K,M)-W*B(K-1,M-1)                          QZ 09790
            Y=D*B(K,M-1)+C*B(K,M)-W*B(K-1,M)                            QZ 09800
            IF(DABS(S).LT.DABS(T)) GO TO 440                            QZ 09810
               IF(S.EQ.0D0) S=EPSB                                      QZ 09820
               U=T/S                                                    QZ 09830
               V=S+T*U                                                  QZ 09840
               ZR=(X+Y*U)/V                                             QZ 09850
               ZI=(Y-X*U)/V                                             QZ 09860
               GO TO 460                                                QZ 09870
440         U=S/T                                                       QZ 09880
            V=T+U*S                                                     QZ 09890
            ZR=(X*U+Y)/V                                                QZ 09900
            ZI=(Y*U-X)/V                                                QZ 09910
C        SECOND (UPPER) OF COUPLED COMPONENTS FOR THE NEXT STEP         QZ 09920
460         IF(DABS(W).LT.0.707D0*(DABS(C)+DABS(D))) GO TO 480          QZ 09930
               IF(W.EQ.0D0) W=EPSB                                      QZ 09940
               X=(B(K,M-1)-G*ZR+H*ZI)/W                                 QZ 09950
               Y=(B(K,M)  -G*ZI-H*ZR)/W                                 QZ 09960
               GO TO 520                                                QZ 09970
480         G=B(K-1,M-1)-E*ZR+F*ZI                                      QZ 09980
            H=B(K-1,M)  -E*ZI-F*ZR                                      QZ 09990
            IF(DABS(C).LT.DABS(D)) GO TO 500                            QZ 10000
               IF(C.EQ.0D0) C=EPSB                                      QZ 10010
               U=D/C                                                    QZ 10020
               V=C+D*U                                                  QZ 10030
               X=(G+H*U)/V                                              QZ 10040
               Y=(H-G*U)/V                                              QZ 10050
               GO TO 520                                                QZ 10060
500         U=C/D                                                       QZ 10070
            V=D+C*U                                                     QZ 10080
            X=(G*U+H)/V                                                 QZ 10090
            Y=(H*U-G)/V                                                 QZ 10100
520         B(K,M-1)=ZR                                                 QZ 10110
            B(K,M)  =ZI                                                 QZ 10120
C        SCALING (IF NEEDED) AND DEFLATION OF THE RIGHT-HAND SIDE       QZ 10130
            C=0.707D0*(DABS(ZR)+DABS(ZI))                               QZ 10140
            IF(C.EQ.0D0) GO TO 620                                      QZ 10150
            D=1D0/C                                                     QZ 10160
            E=C+D                                                       QZ 10170
            IF(E.GT.C) GO TO 580                                        QZ 10180
               DO 560 I=1,M                                             QZ 10190
               B(I,M-1)=D*B(I,M-1)                                      QZ 10200
560            B(I,M)  =D*B(I,M)                                        QZ 10210
            IF(ALFI(K).GT.0D0) GO TO 580                                QZ 10220
               X=D*X                                                    QZ 10230
               Y=D*Y                                                    QZ 10240
580            DO 600 I=1,K-1                                           QZ 10250
               C=P*B(I,K)-R*A(I,K)                                      QZ 10260
               D=Q*B(I,K)                                               QZ 10270
               B(I,M-1)=B(I,M-1)+B(K,M-1)*C-B(K,M)*D                    QZ 10280
600            B(I,M)  =B(I,M)  +B(K,M-1)*D+B(K,M)*C                    QZ 10290
620         CONTINUE                                                    QZ 10300
640      CONTINUE                                                       QZ 10310
C TRANSFORMATION TO THE ORIGINAL BASIS                                  QZ 10320
         DO 700 J=N,1,-1                                                QZ 10330
            DO 680 I=1,N                                                QZ 10340
            S=0D0                                                       QZ 10350
               DO 660 K=1,J                                             QZ 10360
660            S=S+Z(I,K)*B(K,J)                                        QZ 10370
680         Z(I,J)=S                                                    QZ 10380
700      CONTINUE                                                       QZ 10390
C L-INFINITY NORMALIZATION OF THE EIGENVECTORS                          QZ 10400
         DO 860 J=1,N                                                   QZ 10410
         IF(ALFI(J)) 860, 720, 780                                      QZ 10420
C     REAL VECTOR                                                       QZ 10430
720      D=0D0                                                          QZ 10440
            DO 740 I=1,N                                                QZ 10450
            D=DMAX1(DABS(Z(I,J)),D)                                     QZ 10460
740         CONTINUE                                                    QZ 10470
            DO 760 I=1,N                                                QZ 10480
760         Z(I,J)=Z(I,J)/D                                             QZ 10490
         GO TO 860                                                      QZ 10500
C     COMPLEX VECTOR                                                    QZ 10510
780      D=0D0                                                          QZ 10520
            DO 800 I=1,N                                                QZ 10530
            D=DMAX1(DABS(Z(I,J)),DABS(Z(I,J+1)),D)                      QZ 10540
800         CONTINUE                                                    QZ 10550
         CALL DSC16(D,E,F)                                              QZ 10560
         D=0D0                                                          QZ 10570
            DO 820 I=1,N                                                QZ 10580
            Z(I,J)=F*Z(I,J)                                             QZ 10590
            Z(I,J+1)=F*Z(I,J+1)                                         QZ 10600
            D=DMAX1(Z(I,J)**2+Z(I,J+1)**2,D)                            QZ 10610
820         CONTINUE                                                    QZ 10620
         D=1D0/DSQRT(D)                                                 QZ 10630
            DO 840 I=1,N                                                QZ 10640
            Z(I,J)  =Z(I,J)*D                                           QZ 10650
840         Z(I,J+1)=Z(I,J+1)*D                                         QZ 10660
860      CONTINUE                                                       QZ 10670
      RETURN                                                            QZ 10680
      END                                                               QZ 10690
