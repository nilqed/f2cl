@PROCESS DIRECTIVE('"    (')                                            QZ 01670
C-----------------------------------------------------------------------QZ 01680
C REDUCTION OF AN UPPER-HESSENBERG MATRIX TO QUASI-TRIANGULAR FORM BY   QZ 01690
C ORTHOGONAL TRANSFORMATION WITH PRESERVATION OF THE UPPER-TRIANGULAR   QZ 01700
C FORM OF ANOTHER MATRIX.  THIS IS THE SECOND PHASE IN THE SOLUTION OF  QZ 01710
C THE GENERALIZED EIGENVALUE PROBLEM (AFTER A CALL TO QZHES, AND BEFORE QZ 01720
C CALLS TO QZVAL AND QZVEC).                                            QZ 01730
C                                                                       QZ 01740
      SUBROUTINE  QZIT ( LD, N, A, B, EPS1, MATZ, Z, IERR )             QZ 01750
C                                                                       QZ 01760
      INTEGER     LD, N, IERR                                           QZ 01770
      REAL*8      A(LD,N), B(LD,N), Z(LD,N), EPS1                       QZ 01780
      LOGICAL     MATZ                                                  QZ 01790
C                                                                       QZ 01800
C LD     E  FIRST DIMENSION ASSIGNED TO THE TWO-DIMENSIONAL ARRAY       QZ 01810
C           PARAMETERS IN THE CALLING PROGRAM.                          QZ 01820
C                                                                       QZ 01830
C N      E  ORDER OF THE MATRICES.                                      QZ 01840
C                                                                       QZ 01850
C A      E  REAL UPPER-HESSENBERG MATRIX.                               QZ 01860
C        R  QUASI-TRIANGULAR MATRIX.  NO TWO ADJACENT SUBDIAGONAL       QZ 01870
C           ELEMENTS ARE NONZERO.                                       QZ 01880
C                                                                       QZ 01890
C B      E  REAL UPPER-TRIANGULAR MATRIX.                               QZ 01900
C        R  MODIFIED REAL UPPER-TRIANGULAR MATRIX.  THE PRODUCT OF EPS1 QZ 01910
C           AND NORM(B) IS STORED IN THE CELL B(N,1) FOR LATER USE BY   QZ 01920
C           SUBROUTINES QZVAL AND QZVEC.                                QZ 01930
C                                                                       QZ 01940
C EPS1   E  RELATIVE TOLERANCE TO TEST THE NEGLIGIBILITY OF MATRIX      QZ 01950
C           ELEMENTS.  A NONPOSITIVE VALUE OF EPS1 IS INTERNALLY        QZ 01960
C           REPLACED BY MACHINE PRECISION.                              QZ 01970
C           AN ELEMENT OF A MATRIX IS CONSIDERED NEGLIGIBLE IF ITS      QZ 01980
C           MAGNITUDE IS LESS THAN THE PRODUCT OF EPS1 AND THE NORM     QZ 01990
C           OF THE MATRIX.                                              QZ 02000
C                                                                       QZ 02010
C MATZ   E  .TRUE.   IF THE RIGHT MATRIX OF THE REDUCTION IS REQUESTED  QZ 02020
C                    FOR LATER COMPUTATION OF THE EIGENVECTORS.         QZ 02030
C           .FALSE.  OTHERWISE.                                         QZ 02040
C                                                                       QZ 02050
C Z      E  RIGHT TRANSFORMATION MATRIX PRODUCED BY SUBROUTINE QZHES    QZ 02060
C           (OR THE IDENTITY MATRIX) IF MATZ IS .TRUE.                  QZ 02070
C           IF MATZ IS .FALSE., NO REFERENCE IS MADE TO Z.              QZ 02080
C        R  RIGHT MATRIX OF THE REDUCTION IF MATZ IS .TRUE.             QZ 02090
C                                                                       QZ 02100
C IERR   R  ZERO     FOR NORMAL RETURN,                                 QZ 02110
C           J        IF THE LIMIT OF 30*N ITERATIONS IS REACHED BEFORE  QZ 02120
C                    THE END OF THE REDUCTION                           QZ 02130
C                                                                       QZ 02140
C     "AN ALGORITHM FOR GENERALIZED MATRIX EIGENVALUE PROBLEMS"         QZ 02150
C     BY MOLER AND STEWART, SIAM J. NUMER. ANAL. 10, 241-256(1973),     QZ 02160
C     AS MODIFIED IN TECHNICAL NOTE NASA TN D-7305(1973) BY WARD.       QZ 02170
C                                                                       QZ 02180
C     VERSION FOR THE IBM 3090VF DATED NOVEMBER 1987.                   QZ 02190
C     ------------------------------------------------------------------QZ 02200
      INTEGER     I, J, K, L, M, LL, M1, M2, ISH, LIMIT, IT, LOR1, MORN QZ 02210
      REAL*8      R, S, T, SH, U1, U2, U3                               QZ 02220
      REAL*8      A11, A12, A21, A22, A33, A34, A43, A44                QZ 02230
      REAL*8      B11, B12, B22, B33, B34, B44                          QZ 02240
      REAL*8      EPSA, EPSB                                            QZ 02250
      LOGICAL     NOMATZ, MMLTM1                                        QZ 02260
      INTEGER     NB, KB, KK, MM, MP, MMM                               QZ 02270
C     INTERNAL WORKING ARRAY H(3,NB)                                    QZ 02280
      REAL*8      H(3,128)                                              QZ 02290
      REAL*8      EPS0                                                  QZ 02300
      DATA        EPS0/Z3410000000000000/, NB/128/                      QZ 02310
C                                                                       QZ 02320
      CALL XUFLOW(0)                                                    QZ 02330
      IERR=0                                                            QZ 02340
      NOMATZ=.NOT.MATZ                                                  QZ 02350
C NORMS OF A AND B AND ASSOCIATED NEGLIGIBILITY THRESHOLDS              QZ 02360
      EPSA=0D0                                                          QZ 02370
      S=DABS(A(1,1))                                                    QZ 02380
      EPSB=DABS(B(1,1))                                                 QZ 02390
         DO 40 J=2,N                                                    QZ 02400
         S=S+DABS(A(J,J-1))                                             QZ 02410
         EPSA=DMAX1(S,EPSA)                                             QZ 02420
         S=0D0                                                          QZ 02430
         T=0D0                                                          QZ 02440
            DO 20 I=1,J                                                 QZ 02450
            S=S+DABS(A(I,J))                                            QZ 02460
            T=T+DABS(B(I,J))                                            QZ 02470
20          CONTINUE                                                    QZ 02480
         EPSB=DMAX1(T,EPSB)                                             QZ 02490
40       CONTINUE                                                       QZ 02500
      EPSA=DMAX1(S,EPSA)                                                QZ 02510
      IF (EPSA.EQ.0D0) EPSA=1D0                                         QZ 02520
      IF (EPSB.EQ.0D0) EPSB=1D0                                         QZ 02530
      T=EPS1                                                            QZ 02540
      IF (T.LE.0D0) T=EPS0                                              QZ 02550
      EPSA=T*EPSA                                                       QZ 02560
      EPSB=T*EPSB                                                       QZ 02570
C INITIAL VALUES                                                        QZ 02580
      LOR1=1                                                            QZ 02590
      MORN=N                                                            QZ 02600
      M=N                                                               QZ 02610
      LIMIT=30*N                                                        QZ 02620
60    IF (M.LE.2) GO TO 1000                                            QZ 02630
      IF (NOMATZ) MORN=M                                                QZ 02640
      IT=0                                                              QZ 02650
      M1=M-1                                                            QZ 02660
      M2=M-2                                                            QZ 02670
80    ISH=2                                                             QZ 02680
C CHECK FOR CONVERGENCE OR REDUCIBILITY                                 QZ 02690
         DO 100 L=M,2,-1                                                QZ 02700
         IF (DABS(A(L,L-1)).LE.EPSA) GO TO 120                          QZ 02710
100      CONTINUE                                                       QZ 02720
      L=1                                                               QZ 02730
      GO TO 140                                                         QZ 02740
120   A(L,L-1)=0D0                                                      QZ 02750
      IF (L.LT.M1) GO TO 140                                            QZ 02760
C ISOLATED BLOCK OF DIMENSION 1 OR 2                                    QZ 02770
      M=L-1                                                             QZ 02780
      GO TO 60                                                          QZ 02790
C CHECK FOR SMALL TOP OF B                                              QZ 02800
140   LL=L                                                              QZ 02810
160   B11=B(L,L)                                                        QZ 02820
      IF (DABS(B11).GT.EPSB) GO TO 200                                  QZ 02830
      B(L,L)=0D0                                                        QZ 02840
      U1=A(L,L)                                                         QZ 02850
      U2=A(L+1,L)                                                       QZ 02860
      S=DMAX1(DABS(U1),DABS(U2))                                        QZ 02870
      U1=U1/S                                                           QZ 02880
      U2=U2/S                                                           QZ 02890
      R=-DSIGN(DSQRT(U1*U1+U2*U2),U1)                                   QZ 02900
      T=U1-R                                                            QZ 02910
      U1=T/R                                                            QZ 02920
      U2=U2/T                                                           QZ 02930
         DO 180 J=L,MORN                                                QZ 02940
         T=U1*(A(L,J)+U2*A(L+1,J))                                      QZ 02950
         A(L,J)=A(L,J)+T                                                QZ 02960
         A(L+1,J)=A(L+1,J)+T*U2                                         QZ 02970
         T=U1*(B(L,J)+U2*B(L+1,J))                                      QZ 02980
         B(L,J)=B(L,J)+T                                                QZ 02990
         B(L+1,J)=B(L+1,J)+T*U2                                         QZ 03000
180      CONTINUE                                                       QZ 03010
      IF (L.NE.1) A(L,L-1)=-A(L,L-1)                                    QZ 03020
      L=L+1                                                             QZ 03030
      GO TO 120                                                         QZ 03040
200   A11=A(L,L)/B11                                                    QZ 03050
      A21=A(L+1,L)/B11                                                  QZ 03060
      IF (ISH.EQ.1) GO TO 240                                           QZ 03070
      IF (LIMIT.EQ.0) GO TO 999                                         QZ 03080
      IF (IT.EQ.10) GO TO 280                                           QZ 03090
C TYPE OF SHIFT                                                         QZ 03100
      B22=B(L+1,L+1)                                                    QZ 03110
      IF (DABS(B22).LT.EPSB) B22=EPSB                                   QZ 03120
      B33=B(M1,M1)                                                      QZ 03130
      IF (DABS(B33).LT.EPSB) B33=EPSB                                   QZ 03140
      B44=B(M,M)                                                        QZ 03150
      IF (DABS(B44).LT.EPSB) B44=EPSB                                   QZ 03160
      A33=A(M1,M1)/B33                                                  QZ 03170
      A34=A(M1,M)/B44                                                   QZ 03180
      A43=A(M,M1)/B33                                                   QZ 03190
      A44=A(M,M)/B44                                                    QZ 03200
      B34=B(M1,M)/B44                                                   QZ 03210
      T=0.5D0*(A43*B34-A33-A44)                                         QZ 03220
      R=T*T+A34*A43-A33*A44                                             QZ 03230
      IF (R .LT. 0D0) GO TO 260                                         QZ 03240
C SINGLE SHIFT                                                          QZ 03250
      ISH=1                                                             QZ 03260
      R=DSQRT(R)                                                        QZ 03270
      SH=-T+R                                                           QZ 03280
      S=-T-R                                                            QZ 03290
      IF (DABS(S-A44).LT.DABS(SH-A44)) SH=S                             QZ 03300
C SEARCH FOR TWO ADJACENT SMALL SUB-DIAGONAL ELEMENTS OF A              QZ 03310
         DO 220 L=M2,LL+1,-1                                            QZ 03320
         T=A(L,L)                                                       QZ 03330
         IF (DABS(B(L,L)).GT.EPSB) T=T-SH*B(L,L)                        QZ 03340
         IF (DABS(A(L,L-1)).LE.DABS(T/A(L+1,L))*EPSA) GO TO 160         QZ 03350
220      CONTINUE                                                       QZ 03360
      L=LL                                                              QZ 03370
240   U1=A11-SH                                                         QZ 03380
      U2=A21                                                            QZ 03390
      IF (L.NE.LL) A(L,L-1)=-A(L,L-1)                                   QZ 03400
      GO TO 300                                                         QZ 03410
C DOUBLE SHIFT                                                          QZ 03420
260   A12=A(L,L+1)/B22                                                  QZ 03430
      A22=A(L+1,L+1)/B22                                                QZ 03440
      B12=B(L,L+1)/B22                                                  QZ 03450
      U1=((A33-A11)*(A44-A11)-A34*A43+A43*B34*A11)/A21+A12-A11*B12      QZ 03460
      U2=(A22-A11)-A21*B12-(A33-A11)-(A44-A11)+A43*B34                  QZ 03470
      U3=A(L+1+1,L+1)/B22                                               QZ 03480
      GO TO 300                                                         QZ 03490
C AD HOC SHIFT                                                          QZ 03500
280   U1=0D0                                                            QZ 03510
      U2=1D0                                                            QZ 03520
      U3=1.1605D0                                                       QZ 03530
300   IT=IT+1                                                           QZ 03540
      LIMIT=LIMIT-1                                                     QZ 03550
      IF (NOMATZ) LOR1=LL                                               QZ 03560
C QZ SWEEP                                                              QZ 03570
      KB=1-L+NB                                                         QZ 03580
C   THE LEFT TRANSFORMATIONS ARE PERFORMED ON MATRIX SEGMENTS OF NB ROWSQZ 03590
         DO 820 KK=L,M1,NB                                              QZ 03600
         KB=KB-NB                                                       QZ 03610
         MM=MIN0(KK-1+NB,M1)                                            QZ 03620
            DO 700 K=KK,MM                                              QZ 03630
            H(1,K+KB)=0D0                                               QZ 03640
            IF(ISH.EQ.1) GO TO 500                                      QZ 03650
            IF(K.EQ.M1)  GO TO 500                                      QZ 03660
C DOUBLE SHIFT                                                          QZ 03670
C    Q TRANSFORMATION (LEFT): ANNIHILATION OF A(K+1,K-1) AND A(K+2,K-1) QZ 03680
            IF(K.EQ.L) GO TO 320                                        QZ 03690
               U1=A(K,  K-1)                                            QZ 03700
               U2=A(K+1,K-1)                                            QZ 03710
               U3=A(K+2,K-1)                                            QZ 03720
               A(K+1,K-1)=0D0                                           QZ 03730
               A(K+2,K-1)=0D0                                           QZ 03740
320         IF(U2.EQ.0D0 .AND. U3.EQ.0D0) GO TO 400                     QZ 03750
            S=DMAX1(DABS(U1),DABS(U2),DABS(U3))                         QZ 03760
            U1=U1/S                                                     QZ 03770
            U2=U2/S                                                     QZ 03780
            U3=U3/S                                                     QZ 03790
            R=-DSIGN(DSQRT(U1*U1+U2*U2+U3*U3),U1)                       QZ 03800
            IF(K.NE.L) A(K,K-1)=S*R                                     QZ 03810
            T=U1-R                                                      QZ 03820
            U1=T/R                                                      QZ 03830
            U2=U2/T                                                     QZ 03840
            U3=U3/T                                                     QZ 03850
            H(1,K+KB)=U1                                                QZ 03860
            H(2,K+KB)=U2                                                QZ 03870
            H(3,K+KB)=U3                                                QZ 03880
               DO 340 J=K,K+2                                           QZ 03890
               T=U1*(A(K,J)+U2*A(K+1,J)+U3*A(K+2,J))                    QZ 03900
               A(K,J)  =A(K,J)+T                                        QZ 03910
               A(K+1,J)=A(K+1,J)+T*U2                                   QZ 03920
               A(K+2,J)=A(K+2,J)+T*U3                                   QZ 03930
               T=U1*(B(K,J)+U2*B(K+1,J)+U3*B(K+2,J))                    QZ 03940
               B(K,J)  =B(K,J)+T                                        QZ 03950
               B(K+1,J)=B(K+1,J)+T*U2                                   QZ 03960
340            B(K+2,J)=B(K+2,J)+T*U3                                   QZ 03970
400         IF (K+3.GT.M) GO TO 430                                     QZ 03980
C    Q TRANSFORMATIONS OF COLUMN K+3 IN A AND B                         QZ 03990
               DO 420 I=KK,K                                            QZ 04000
               IF(H(1,I+KB).EQ.0D0) GO TO 420                           QZ 04010
               T=H(1,I+KB)*                                             QZ 04020
     $            (A(I,K+3)+H(2,I+KB)*A(I+1,K+3)+H(3,I+KB)*A(I+2,K+3))  QZ 04030
               A(I,  K+3)=A(I  ,K+3)+T                                  QZ 04040
               A(I+1,K+3)=A(I+1,K+3)+T*H(2,I+KB)                        QZ 04050
               A(I+2,K+3)=A(I+2,K+3)+T*H(3,I+KB)                        QZ 04060
               T=H(1,I+KB)*                                             QZ 04070
     $            (B(I,K+3)+H(2,I+KB)*B(I+1,K+3)+H(3,I+KB)*B(I+2,K+3))  QZ 04080
               B(I,  K+3)=B(I  ,K+3)+T                                  QZ 04090
               B(I+1,K+3)=B(I+1,K+3)+T*H(2,I+KB)                        QZ 04100
               B(I+2,K+3)=B(I+2,K+3)+T*H(3,I+KB)                        QZ 04110
420            CONTINUE                                                 QZ 04120
430         IF(H(1,K+KB).EQ.0D0) GO TO 700                              QZ 04130
C    Z TRANSFORMATION (RIGHT): ANNIHILATION OF B(K+2,K) AND B(K+2,K+1)  QZ 04140
            U1=B(K+2,K+2)                                               QZ 04150
            U2=B(K+2,K+1)                                               QZ 04160
            U3=B(K+2,K)                                                 QZ 04170
            IF(U2.EQ.0D0 .AND. U3.EQ.0D0) GO TO 620                     QZ 04180
            S=DMAX1(DABS(U1),DABS(U2),DABS(U3))                         QZ 04190
            U1=U1/S                                                     QZ 04200
            U2=U2/S                                                     QZ 04210
            U3=U3/S                                                     QZ 04220
            R=-DSIGN(DSQRT(U1*U1+U2*U2+U3*U3),U1)                       QZ 04230
            B(K+2,K)  =0D0                                              QZ 04240
            B(K+2,K+1)=0D0                                              QZ 04250
            B(K+2,K+2)=S*R                                              QZ 04260
            T=U1-R                                                      QZ 04270
            U1=T/R                                                      QZ 04280
            U2=U2/T                                                     QZ 04290
            U3=U3/T                                                     QZ 04300
               DO 440 I=LOR1,K+1                                        QZ 04310
               T=U1*(U3*B(I,K)+U2*B(I,K+1)+B(I,K+2))                    QZ 04320
               B(I,K)  =B(I,K)  +T*U3                                   QZ 04330
               B(I,K+1)=B(I,K+1)+T*U2                                   QZ 04340
440            B(I,K+2)=B(I,K+2)+T                                      QZ 04350
               DO 460 I=LOR1,MIN0(K+3,M)                                QZ 04360
               T=U1*(U3*A(I,K)+U2*A(I,K+1)+A(I,K+2))                    QZ 04370
               A(I,K)  =A(I,K)  +T*U3                                   QZ 04380
               A(I,K+1)=A(I,K+1)+T*U2                                   QZ 04390
460            A(I,K+2)=A(I,K+2)+T                                      QZ 04400
            IF(NOMATZ) GO TO 620                                        QZ 04410
               DO 480 I=1,N                                             QZ 04420
               T=U1*(U3*Z(I,K)+U2*Z(I,K+1)+Z(I,K+2))                    QZ 04430
               Z(I,K)  =Z(I,K)  +T*U3                                   QZ 04440
               Z(I,K+1)=Z(I,K+1)+T*U2                                   QZ 04450
480            Z(I,K+2)=Z(I,K+2)+T                                      QZ 04460
            GO TO 620                                                   QZ 04470
C SINGLE-SHIFT ITERATION OR LAST DOUBLE-SHIFT TRANSFORMATION            QZ 04480
C    Q ANNIHILATION OF A(K+1,K-1)                                       QZ 04490
500         IF(K.EQ.L) GO TO 520                                        QZ 04500
               U1=A(K,K-1)                                              QZ 04510
               U2=A(K+1,K-1)                                            QZ 04520
               A(K+1,K-1)=0D0                                           QZ 04530
520         IF(U2.EQ.0D0) GO TO 580                                     QZ 04540
            S=DMAX1(DABS(U1),DABS(U2))                                  QZ 04550
            U1=U1/S                                                     QZ 04560
            U2=U2/S                                                     QZ 04570
            R=-DSIGN(DSQRT(U1*U1+U2*U2),U1)                             QZ 04580
            IF(K.NE.L) A(K,K-1)=S*R                                     QZ 04590
            T=U1-R                                                      QZ 04600
            U1=T/R                                                      QZ 04610
            U2=U2/T                                                     QZ 04620
            H(1,K+KB)=U1                                                QZ 04630
            H(2,K+KB)=U2                                                QZ 04640
               DO 540 J=K,K+1                                           QZ 04650
               T=U1*(A(K,J)+U2*A(K+1,J))                                QZ 04660
               A(K,J)  =A(K,J)  +T                                      QZ 04670
               A(K+1,J)=A(K+1,J)+T*U2                                   QZ 04680
               T=U1*(B(K,J)+U2*B(K+1,J))                                QZ 04690
               B(K,J)  =B(K,J)  +T                                      QZ 04700
540            B(K+1,J)=B(K+1,J)+T*U2                                   QZ 04710
C    Q TRANSFORMATIONS OF COLUMN K+2 IN A AND B                         QZ 04720
580         IF(K.EQ.M1) GO TO 620                                       QZ 04730
               DO 600 I=KK,K                                            QZ 04740
               IF(H(1,I+KB).EQ.0D0) GO TO 600                           QZ 04750
               T=H(1,I+KB)*(A(I,K+2)+H(2,I+KB)*A(I+1,K+2))              QZ 04760
               A(I,K+2)  =A(I,K+2)  +T                                  QZ 04770
               A(I+1,K+2)=A(I+1,K+2)+T*H(2,I+KB)                        QZ 04780
               T=H(1,I+KB)*(B(I,K+2)+H(2,I+KB)*B(I+1,K+2))              QZ 04790
               B(I,K+2)  =B(I,K+2)  +T                                  QZ 04800
               B(I+1,K+2)=B(I+1,K+2)+T*H(2,I+KB)                        QZ 04810
600            CONTINUE                                                 QZ 04820
            IF(H(1,K+KB).EQ.0D0) GO TO 700                              QZ 04830
C MORE Z TRANSFORMATION: ANNIHILATION OF B(K+1,K)                       QZ 04840
620         U2=B(K+1,K)                                                 QZ 04850
            IF(U2.EQ.0D0) GO TO 700                                     QZ 04860
            U1=B(K+1,K+1)                                               QZ 04870
            S=DMAX1(DABS(U1),DABS(U2))                                  QZ 04880
            U1=U1/S                                                     QZ 04890
            U2=U2/S                                                     QZ 04900
            R=-DSIGN(DSQRT(U1*U1+U2*U2),U1)                             QZ 04910
            B(K+1,K+1)=S*R                                              QZ 04920
            B(K+1,K)=0D0                                                QZ 04930
            T=U1-R                                                      QZ 04940
            U1=T/R                                                      QZ 04950
            U2=U2/T                                                     QZ 04960
               DO 640 I=LOR1,K                                          QZ 04970
               T=U1*(U2*B(I,K)+B(I,K+1))                                QZ 04980
               B(I,K)  =B(I,K)  +T*U2                                   QZ 04990
640            B(I,K+1)=B(I,K+1)+T                                      QZ 05000
            B(K+1,K)=0D0                                                QZ 05010
               DO 660 I=LOR1,MIN0(K+3,M)                                QZ 05020
               T=U1*(U2*A(I,K)+A(I,K+1))                                QZ 05030
               A(I,K)  =A(I,K)  +T*U2                                   QZ 05040
660            A(I,K+1)=A(I,K+1)+T                                      QZ 05050
            IF(NOMATZ) GO TO 700                                        QZ 05060
               DO 680 I=1,N                                             QZ 05070
               T=U1*(U2*Z(I,K)+Z(I,K+1))                                QZ 05080
               Z(I,K)  =Z(I,K)  +T*U2                                   QZ 05090
680            Z(I,K+1)=Z(I,K+1)+T                                      QZ 05100
700         CONTINUE                                                    QZ 05110
C COMPLETION OF THE Q (LEFT) TRANSFORMATIONS                            QZ 05120
         MP=M+1                                                         QZ 05130
         MMLTM1=MM.LT.M1                                                QZ 05140
         IF(ISH.EQ.1) GO TO 760                                         QZ 05150
         IF(MM+3.LE.M) MP=MM+4                                          QZ 05160
         MMM=M2                                                         QZ 05170
         IF(MMLTM1) MMM=MM                                              QZ 05180
C    DOUBLE SHIFT                                                       QZ 05190
            DO 740 J=MP,MORN                                            QZ 05200
               DO 720 I=KK,MMM                                          QZ 05210
               IF(H(1,I+KB).EQ.0D0) GO TO 720                           QZ 05220
               T=H(1,I+KB)*                                             QZ 05230
     $            (A(I,J)+H(2,I+KB)*A(I+1,J)+H(3,I+KB)*A(I+2,J))        QZ 05240
               A(I,J)  =A(I,J)  +T                                      QZ 05250
               A(I+1,J)=A(I+1,J)+T*H(2,I+KB)                            QZ 05260
               A(I+2,J)=A(I+2,J)+T*H(3,I+KB)                            QZ 05270
               T=H(1,I+KB)*                                             QZ 05280
     $            (B(I,J)+H(2,I+KB)*B(I+1,J)+H(3,I+KB)*B(I+2,J))        QZ 05290
               B(I,J)  =B(I,J)  +T                                      QZ 05300
               B(I+1,J)=B(I+1,J)+T*H(2,I+KB)                            QZ 05310
               B(I+2,J)=B(I+2,J)+T*H(3,I+KB)                            QZ 05320
720            CONTINUE                                                 QZ 05330
            IF(MMLTM1) GO TO 740                                        QZ 05340
            IF(H(1,M1+KB).EQ.0D0) GO TO 740                             QZ 05350
               T=H(1,M1+KB)*(A(M1,J)+H(2,M1+KB)*A(M,J))                 QZ 05360
               A(M1,J)=A(M1,J)+T                                        QZ 05370
               A(M,J) =A(M,J) +T*H(2,M1+KB)                             QZ 05380
               T=H(1,M1+KB)*(B(M1,J)+H(2,M1+KB)*B(M,J))                 QZ 05390
               B(M1,J)=B(M1,J)+T                                        QZ 05400
               B(M,J) =B(M,J) +T*H(2,M1+KB)                             QZ 05410
740         CONTINUE                                                    QZ 05420
         GO TO 820                                                      QZ 05430
C    SINGLE SHIFT                                                       QZ 05440
760      IF(MMLTM1) MP=MM+3                                             QZ 05450
            DO 800 J=MP,MORN                                            QZ 05460
               DO 780 I=KK,MM                                           QZ 05470
               IF(H(1,I+KB).EQ.0D0) GO TO 780                           QZ 05480
               T=H(1,I+KB)*(A(I,J)+H(2,I+KB)*A(I+1,J))                  QZ 05490
               A(I,J)  =A(I,J)  +T                                      QZ 05500
               A(I+1,J)=A(I+1,J)+T*H(2,I+KB)                            QZ 05510
               T=H(1,I+KB)*(B(I,J)+H(2,I+KB)*B(I+1,J))                  QZ 05520
               B(I,J)  =B(I,J)  +T                                      QZ 05530
               B(I+1,J)=B(I+1,J)+T*H(2,I+KB)                            QZ 05540
780            CONTINUE                                                 QZ 05550
800         CONTINUE                                                    QZ 05560
820      CONTINUE                                                       QZ 05570
C END OF QZ STEP                                                        QZ 05580
      GO TO 80                                                          QZ 05590
C                                                                       QZ 05600
C ERROR: INCOMPLETE TRIANGULARIZATION WITHIN 30*N ITERATIONS            QZ 05610
999   IERR=M                                                            QZ 05620
C                                                                       QZ 05630
C NEGLIGIBILITY THRESHOLD EPSB IS PRESERVED FOR USE IN QZVAL AND QZVEC  QZ 05640
1000  IF(N.GT.1) B(N,1)=EPSB                                            QZ 05650
      RETURN                                                            QZ 05660
      END                                                               QZ 05670
