C-----------------------------------------------------------------------BIS02050
C EIGENVALUES OF A SYMMETRIC TRIDIAGONAL MATRIX CONTAINED IN A GIVEN    BIS02060
C INTERVAL (BISECTION), AND ASSOCIATED EIGENVECTORS (INVERSE ITERATION).BIS02070
C                                                                       BIS02080
      SUBROUTINE  TSTURM(LD,N,EPS1,D,E,E2,BL,BU,MM,M,W,Z,               BIS02090
     $                   IERR,V1,V2,V3,V4,V5,V6)                        BIS02100
C                                                                       BIS02110
      INTEGER     LD, N, MM, M, IERR                                    BIS02120
      REAL*8      EPS1, BL, BU                                          BIS02130
      REAL*8      D(N), E(N), E2(N), W(MM), Z(LD,MM)                    BIS02140
      REAL*8      V1(N),V2(N),V3(N),V4(N),V5(N),V6(N)                   BIS02150
C                                                                       BIS02160
C LD     E  FIRST DIMENSION ASSIGNED TO THE TWO-DIMENSIONAL ARRAY       BIS02170
C           PARAMETERS IN THE CALLING PROGRAM.                          BIS02180
C                                                                       BIS02190
C N      E  ORDER OF THE MATRIX.                                        BIS02200
C                                                                       BIS02210
C EPS1   E  ABSOLUTE ERROR TOLERANCE FOR THE COMPUTED EIGENVALUES.      BIS02220
C           IT SHOULD BE COMMENSURATE WITH RELATIVE PERTURBATIONS IN THEBIS02230
C           MATRIX ELEMENTS OF THE ORDER OF MACHINE PRECISION.          BIS02240
C        R  IF EPS1 IS NOT POSITIVE UPON ENTRY, IT IS RESET FOR EACH    BIS02250
C           SUBMATRIX TO A DEFAULT VALUE, NAMELY, MINUS THE             BIS02260
C           PRODUCT OF MACHINE PRECISION AND THE L1-NORM OF THE         BIS02270
C           SUBMATRIX.                                                  BIS02280
C D      E  DIAGONAL ELEMENTS OF THE TRIDIAGONAL MATRIX.                BIS02290
C                                                                       BIS02300
C E      E  SUBDIAGONAL ELEMENTS OF THE TRIDIAGONAL MATRIX IN CELLS     BIS02310
C           2,...,N.                                                    BIS02320
C                                                                       BIS02330
C E2     E  SQUARES OF THE CORRESPONDING ELEMENTS OF E.                 BIS02340
C        R  ELEMENTS OF E2 CORRESPONDING TO ELEMENTS OF E REGARDED AS   BIS02350
C           NEGLIGIBLE ARE REPLACED BY ZERO, CAUSING THE MATRIX TO SPLITBIS02360
C           INTO A DIRECT SUM OF PRINCIPAL SUBMATRICES.                 BIS02370
C           E2(1) IS SET TO ZERO.                                       BIS02380
C                                                                       BIS02390
C BL,BU  E  BOUNDS OF THE INTERVAL TO BE SEARCHED FOR EIGENVALUES.      BIS02400
C           IF BL IS NOT LESS THAN BU, NO EIGENVALUE IS COMPUTED.       BIS02410
C                                                                       BIS02420
C MM     E  UPPER BOUND FOR THE NUMBER OF EIGENVALUES IN THE INTERVAL.  BIS02430
C           IF MORE THAN MM EIGENVALUES ARE FOUND TO LIE IN THE         BIS02440
C           INTERVAL, THE ROUTINE TERMINATES WITH AN ERROR CONDITION    BIS02450
C           AND NO EIGENVALUE IS COMPUTED.                              BIS02460
C                                                                       BIS02470
C M      R  NUMBER OF EIGENVALUES FOUND TO LIE IN (BL,BU).              BIS02480
C                                                                       BIS02490
C W      R  COMPUTED EIGENVALUES IN ASCENDING ORDER IF THE MATRIX       BIS02500
C           DOES NOT SPLIT.  IF THE MATRIX SPLITS, THE EIGENVALUES ARE  BIS02510
C           IN ASCENDING ORDER FOR EACH SUBMATRIX.  IF A VECTOR ERROR   BIS02520
C           EXIT OCCURS, W CONTAINS THOSE VALUES ALREADY COMPUTED.      BIS02530
C                                                                       BIS02540
C Z      R  ASSOCIATED SET OF ORTHONORMAL EIGENVECTORS.                 BIS02550
C                                                                       BIS02560
C IERR   R  ZERO       FOR NORMAL RETURN,                               BIS02570
C           3*N+1      IF M EXCEEDS MM, OR                              BIS02580
C           4*N+R      IF THE EIGENVECTOR CORRESPONDING TO THE R-TH     BIS02590
C                      EIGENVALUE FAILS TO CONVERGE IN 5 ITERATIONS.    BIS02600
C                                                                       BIS02610
C V1,...,V6 ANCILLARY STORAGE.                                          BIS02620
C                                                                       BIS02630
C     THIS SUBROUTINE IS BASED ON THE ALGOL PROCEDURE TRISTURM          BIS02640
C     BY PETERS AND WILKINSON,                                          BIS02650
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 418-439(1971).   BIS02660
C                                                                       BIS02670
C     VERSION FOR THE IBM 3090VF DATED NOVEMBER 1987.                   BIS02680
C     ------------------------------------------------------------------BIS02690
      INTEGER     I, J, K, IP, IQ, IR, IS, M1, M2, ITS, IGROUP, ISTURM  BIS02700
      REAL*8      U, V, UK, XU, X0, X1, EPS2, EPS3, EPS4, ANORM         BIS02710
      REAL*8      TST1,TST2, WBL, WBU, EPS, TWOEPS                      BIS02720
      DATA        EPS/Z3410000000000000/                                BIS02730
C                                                                       BIS02740
      CALL XUFLOW(0)                                                    BIS02750
      IERR=0                                                            BIS02760
      WBL=BL                                                            BIS02770
      WBU=BU                                                            BIS02780
      TWOEPS=EPS+EPS                                                    BIS02790
C SEARCH FOR SMALL SUB-DIAGONAL ENTRIES                                 BIS02800
      E2(1)=0D0                                                         BIS02810
         DO 40 I=2,N                                                    BIS02820
         TST1=DABS(D(I))+DABS(D(I-1))                                   BIS02830
         TST2=TST1+DABS(E(I))                                           BIS02840
         IF (TST2.GT.TST1) GO TO 40                                     BIS02850
         E2(I)=0.0D0                                                    BIS02860
40       CONTINUE                                                       BIS02870
C NUMBER OF EIGENVALUES IN THE INTERVAL                                 BIS02880
      IP=1                                                              BIS02890
      IQ=N                                                              BIS02900
      X1=WBU                                                            BIS02910
      ISTURM=1                                                          BIS02920
      GO TO 320                                                         BIS02930
60    M=IS                                                              BIS02940
      X1=WBL                                                            BIS02950
      ISTURM=2                                                          BIS02960
      GO TO 320                                                         BIS02970
80    M=M-IS                                                            BIS02980
      IF (M.GT.MM) GO TO 980                                            BIS02990
      IQ=0                                                              BIS03000
      IR=0                                                              BIS03010
C ISOLATION OF A SUBMATRIX AND REFINEMENT OF THE INTERVAL WITH          BIS03020
C GERSCHGORIN BOUNDS                                                    BIS03030
100   IF (IR.EQ.M) GO TO 1000                                           BIS03040
      IP=IQ+1                                                           BIS03050
      XU=D(IP)                                                          BIS03060
      X0=D(IP)                                                          BIS03070
      U=0.0D0                                                           BIS03080
         DO 120 IQ=IP,N                                                 BIS03090
         X1=U                                                           BIS03100
         U=0.0D0                                                        BIS03110
         V=0.0D0                                                        BIS03120
         IF (IQ.EQ.N) GO TO 110                                         BIS03130
         U=DABS(E(IQ+1))                                                BIS03140
         V=E2(IQ+1)                                                     BIS03150
110      XU=DMIN1(D(IQ)-(X1+U),XU)                                      BIS03160
         X0=DMAX1(D(IQ)+(X1+U),X0)                                      BIS03170
         IF (V.EQ.0.0D0) GO TO 140                                      BIS03180
120      CONTINUE                                                       BIS03190
140   X1=EPS*DMAX1(DABS(XU),DABS(X0))                                   BIS03200
      IF (EPS1.LE.0.0D0) EPS1=-X1                                       BIS03210
      IF (IP.NE.IQ) GO TO 180                                           BIS03220
C CHECK FOR ISOLATED ROOT WITHIN INTERVAL                               BIS03230
      IF (BL.GT.D(IP) .OR. D(IP).GE.BU) GO TO 940                       BIS03240
      IR=IR+1                                                           BIS03250
         DO 160 I=1,N                                                   BIS03260
160      Z(I,IR)=0.0D0                                                  BIS03270
      W(IR)=D(IP)                                                       BIS03280
      Z(IP,IR)=1.0D0                                                    BIS03290
      GO TO 940                                                         BIS03300
180   U=IQ-IP+1                                                         BIS03310
      X1=U*X1                                                           BIS03320
      WBL=DMAX1(BL,XU-X1)                                               BIS03330
      WBU=DMIN1(BU,X0+X1)                                               BIS03340
      X1=WBL                                                            BIS03350
      ISTURM=3                                                          BIS03360
      GO TO 320                                                         BIS03370
200   M1=IS+1                                                           BIS03380
      X1=WBU                                                            BIS03390
      ISTURM=4                                                          BIS03400
      GO TO 320                                                         BIS03410
220   M2=IS                                                             BIS03420
      IF (M1.GT.M2) GO TO 940                                           BIS03430
C INITIAL INTERVALS FOR THE K-TH EIGENVALUE                             BIS03440
      X0=WBU                                                            BIS03450
      ISTURM=5                                                          BIS03460
         DO 240 I=M1,M2                                                 BIS03470
         V5(I)=WBU                                                      BIS03480
         V4(I)=WBL                                                      BIS03490
240      CONTINUE                                                       BIS03500
C COMPUTATION OF THE K-TH EIGENVALUE                                    BIS03510
      K=M2                                                              BIS03520
250      XU=WBL                                                         BIS03530
            DO 260 I=K,M1,-1                                            BIS03540
            IF (XU.GE.V4(I)) GO TO 260                                  BIS03550
            XU=V4(I)                                                    BIS03560
            GO TO 280                                                   BIS03570
260      CONTINUE                                                       BIS03580
280      IF (X0.GT.V5(K)) X0=V5(K)                                      BIS03590
C BISECTION STEP                                                        BIS03600
300      X1=XU+0.5D0*(X0-XU)                                            BIS03610
C CONVERGENCE TEST FOR NONBINARY MACHINES BY L. HARDING (U. MICHIGAN)   BIS03620
         IF(X1.EQ.XU .OR. X1.GE.X0) GO TO 420                           BIS03630
         IF(X0-XU.LE.DABS(EPS1)) GO TO 420                              BIS03640
C PROCEDURE FOR STURM SEQUENCE                                          BIS03650
320      IS=IP-1                                                        BIS03660
         U=1.0D0                                                        BIS03670
            DO 340 I=IP,IQ                                              BIS03680
            IF (U.NE.0.0D0) GO TO 325                                   BIS03690
            V=DABS(E(I))/EPS                                            BIS03700
            IF (E2(I).EQ.0.0D0) V=0.0D0                                 BIS03710
            GO TO 330                                                   BIS03720
325         V=E2(I)/U                                                   BIS03730
330         U=D(I)-X1-V                                                 BIS03740
            IF (U.LT.0.0D0) IS=IS+1                                     BIS03750
340         CONTINUE                                                    BIS03760
         GO TO (60,80,200,220,360),ISTURM                               BIS03770
C INTERVAL REFINEMENT                                                   BIS03780
360      IF (IS.GE.K) GO TO 400                                         BIS03790
         XU=X1                                                          BIS03800
         IF (IS.GE.M1) GO TO 380                                        BIS03810
         V4(M1)=X1                                                      BIS03820
         GO TO 300                                                      BIS03830
380      V4(IS+1)=X1                                                    BIS03840
         IF (V5(IS).GT.X1) V5(IS)=X1                                    BIS03850
         GO TO 300                                                      BIS03860
400      X0=X1                                                          BIS03870
         GO TO 300                                                      BIS03880
C K-TH EIGENVALUE                                                       BIS03890
420   V5(K)=X1                                                          BIS03900
      K=K-1                                                             BIS03910
      IF (K.GE.M1) GO TO 250                                            BIS03920
C EIGENVECTORS BY INVERSE ITERATION                                     BIS03930
      ANORM=DABS(D(IP))                                                 BIS03940
         DO 500 I=IP+1,IQ                                               BIS03950
500      ANORM=DMAX1(ANORM,DABS(D(I))+DABS(E(I)))                       BIS03960
C EPS2 IS THE TEST VALUE FOR GROUPING,                                  BIS03970
C EPS3 REPLACES ZERO PIVOTS AND PERTURBS EQUAL ROOTS, AND               BIS03980
C EPS4 (RIGHT-HAND SIDE) IS TAKEN VERY SMALL TO AVOID OVERFLOW.         BIS03990
      EPS2=1.0D-3*ANORM                                                 BIS04000
      EPS3=EPS*ANORM                                                    BIS04010
      UK=IQ-IP+1                                                        BIS04020
      EPS4=UK*EPS3                                                      BIS04030
      UK=EPS4/DSQRT(UK)                                                 BIS04040
      IGROUP=0                                                          BIS04050
      IS=IP                                                             BIS04060
         DO 920 K=M1,M2                                                 BIS04070
         IR=IR+1                                                        BIS04080
         ITS=1                                                          BIS04090
         W(IR)=V5(K)                                                    BIS04100
         X1=V5(K)                                                       BIS04110
C SEPARATION OF CLUSTERED ROOTS                                         BIS04120
         IF (K.EQ.M1) GO TO 520                                         BIS04130
         IF (X1-X0.GE.EPS2) IGROUP=-1                                   BIS04140
         IGROUP=IGROUP+1                                                BIS04150
         IF (X1.LE.X0) X1=X0+EPS3                                       BIS04160
C MATRIX FACTORIZATION WITH INTERCHANGES, AND INITIAL RIGHT-HAND SIDE   BIS04170
520      V=0.0D0                                                        BIS04180
            DO 580 I=IP,IQ                                              BIS04190
            V6(I)=UK                                                    BIS04200
            IF (I.EQ.IP) GO TO 560                                      BIS04210
            IF (DABS(E(I)).LT.DABS(U)) GO TO 540                        BIS04220
            XU=U/E(I)                                                   BIS04230
            V4(I)=XU                                                    BIS04240
            V1(I-1)=E(I)                                                BIS04250
            V2(I-1)=D(I)-X1                                             BIS04260
            V3(I-1)=0.0D0                                               BIS04270
            IF (I.NE.IQ) V3(I-1)=E(I+1)                                 BIS04280
            U=V-XU*V2(I-1)                                              BIS04290
            V=-XU*V3(I-1)                                               BIS04300
            GO TO 580                                                   BIS04310
540         XU=E(I)/U                                                   BIS04320
            V4(I)=XU                                                    BIS04330
            V1(I-1)=U                                                   BIS04340
            V2(I-1)=V                                                   BIS04350
            V3(I-1)=0.0D0                                               BIS04360
560         U=D(I)-X1-XU*V                                              BIS04370
            IF (I.NE.IQ) V=E(I+1)                                       BIS04380
580      CONTINUE                                                       BIS04390
         IF (U.EQ.0.0D0) U=EPS3                                         BIS04400
         V1(IQ)=U                                                       BIS04410
         V2(IQ)=0.0D0                                                   BIS04420
         V3(IQ)=0.0D0                                                   BIS04430
C BACKWARD SUBSTITUTION                                                 BIS04440
600         DO 620 I=IQ,IP,-1                                           BIS04450
            V6(I)=(V6(I)-U*V2(I)-V*V3(I))/V1(I)                         BIS04460
            V=U                                                         BIS04470
            U=V6(I)                                                     BIS04480
620         CONTINUE                                                    BIS04490
C ORTHOGONALIZATION AGAINST PREVIOUS MEMBERS OF THE GROUP               BIS04500
            DO 680 J=IR-IGROUP,IR-1                                     BIS04510
            XU=0.0D0                                                    BIS04520
               DO 640 I=IP,IQ                                           BIS04530
640            XU=XU+V6(I)*Z(I,J)                                       BIS04540
               DO 660 I=IP,IQ                                           BIS04550
660            V6(I)=V6(I)-XU*Z(I,J)                                    BIS04560
680         CONTINUE                                                    BIS04570
         ANORM=0.0D0                                                    BIS04580
            DO 720 I=IP,IQ                                              BIS04590
720         ANORM=ANORM+DABS(V6(I))                                     BIS04600
         IF (ANORM.GE.1D0) GO TO 840                                    BIS04610
C  NEXT ITERATE                                                         BIS04620
         IF (ITS.EQ.5) GO TO 960                                        BIS04630
         IF (ANORM.NE.0.0D0) GO TO 740                                  BIS04640
         V6(IS)=EPS4                                                    BIS04650
         IS=IS+1                                                        BIS04660
         IF (IS.GT.IQ) IS=IP                                            BIS04670
         GO TO 780                                                      BIS04680
740      XU=EPS4/ANORM                                                  BIS04690
            DO 760 I=IP,IQ                                              BIS04700
760         V6(I)=V6(I)*XU                                              BIS04710
C FORWARD SUBSTITUTION                                                  BIS04720
780         DO 820 I=IP+1,IQ                                            BIS04730
            U=V6(I)                                                     BIS04740
            IF (V1(I-1).NE.E(I)) GO TO 800                              BIS04750
C ROW INTERCHANGE                                                       BIS04760
            U=V6(I-1)                                                   BIS04770
            V6(I-1)=V6(I)                                               BIS04780
800         V6(I)=U-V4(I)*V6(I-1)                                       BIS04790
820         CONTINUE                                                    BIS04800
         ITS=ITS+1                                                      BIS04810
         GO TO 600                                                      BIS04820
C L2 NORMALIZATION                                                      BIS04830
840      U=0.0D0                                                        BIS04840
            DO 850 I=IP,IQ                                              BIS04850
850         U=U+DABS(V6(I))                                             BIS04860
         XU=0D0                                                         BIS04870
            DO 860 I=IP,IQ                                              BIS04880
860         XU=XU+(V6(I)/U)**2                                          BIS04890
         XU=1.0D0/(U*DSQRT(XU))                                         BIS04900
            DO 880 I=1,N                                                BIS04910
880         Z(I,IR)=0.0D0                                               BIS04920
         X0=X1                                                          BIS04930
            DO 900 I=IP,IQ                                              BIS04940
900         Z(I,IR)=XU*V6(I)                                            BIS04950
         X0=X1                                                          BIS04960
920      CONTINUE                                                       BIS04970
940   IF (IQ.LT.N) GO TO 100                                            BIS04980
      GO TO 1000                                                        BIS04990
C ERROR: NO CONVERGENCE FOR THE EIGENVECTOR                             BIS05000
960   IERR=4*N+IR                                                       BIS05010
      GO TO 1000                                                        BIS05020
C ERROR: UNDERESTIMATION OF THE NUMBER OF EIGENVALUES IN THE INTERVAL   BIS05030
980   IERR=3*N+1                                                        BIS05040
1000  RETURN                                                            BIS05050
      END                                                               BIS05060
