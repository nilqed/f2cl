C-----------------------------------------------------------------------BIS00020
C EIGENVALUES OF A SYMMETRIC TRIDIAGONAL MATRIX CONTAINED IN A SPECIFIEDBIS00030
C INTERVAL (BISECTION).                                                 BIS00040
C                                                                       BIS00050
      SUBROUTINE  BISECT (N,EPS1,D,E,E2,BL,BU,MM,M,W,IND,IERR,WL,WU)    BIS00060
C                                                                       BIS00070
      INTEGER     IND(MM), M, N, MM, IERR                               BIS00080
      REAL*8      D(N),E(N),E2(N),W(MM),WL(N),WU(N),EPS1,BL,BU          BIS00090
C                                                                       BIS00100
C N      E  ORDER OF THE MATRIX.                                        BIS00110
C                                                                       BIS00120
C EPS1   E  ABSOLUTE ERROR TOLERANCE FOR THE COMPUTED EIGENVALUES.      BIS00130
C        E  IF EPS1 IS NOT POSITIVE UPON ENTRY, IT IS RESET FOR EACH    BIS00140
C           SUBMATRIX TO A DEFAULT VALUE, NAMELY, MINUS THE PRODUCT OF  BIS00150
C           THE RELATIVE MACHINE PRECISION AND THE L1-NORM OF THE       BIS00160
C           SUBMATRIX.                                                  BIS00170
C                                                                       BIS00180
C D      E  DIAGONAL ELEMENTS OF THE TRIDIAGONAL MATRIX.                BIS00190
C                                                                       BIS00200
C E      E  SUBDIAGONAL ELEMENTS OF THE TRIDIAGONAL MATRIX IN CELLS     BIS00210
C           2,...,N.                                                    BIS00220
C                                                                       BIS00230
C E2     E  SQUARES OF THE CORRESPONDING ELEMENTS OF E.                 BIS00240
C        R  ELEMENTS OF E2 CORRESPONDING TO ELEMENTS OF E REGARDED AS   BIS00250
C           NEGLIGIBLE ARE REPLACED BY ZERO, REFLECTING SPLITS OF THE   BIS00260
C           MATRIX INTO A DIRECT SUM OF PRINCIPAL SUBMATRICES.          BIS00270
C           E2(1) IS SET TO ZERO.                                       BIS00280
C                                                                       BIS00290
C BL,BU  E  BOUNDS OF THE INTERVAL TO BE SEARCHED FOR EIGENVALUES.      BIS00300
C           IF BL IS NOT LESS THAN BU, NO EIGENVALUE IS COMPUTED.       BIS00310
C                                                                       BIS00320
C MM     E  UPPER BOUND FOR THE NUMBER OF EIGENVALUES IN THE INTERVAL.  BIS00330
C           IF MORE THAN MM EIGENVALUES ARE FOUND TO LIE IN THE         BIS00340
C           INTERVAL, THE ROUTINE TERMINATES WITH AN ERROR CONDITION    BIS00350
C           AND NO EIGENVALUE IS COMPUTED.                              BIS00360
C                                                                       BIS00370
C M      R  NUMBER OF EIGENVALUES FOUND IN (BL,BU)                      BIS00380
C                                                                       BIS00390
C W      R  COMPUTED EIGENVALUES IN ASCENDING ORDER.                    BIS00400
C                                                                       BIS00410
C IND    R  INDICES OF THE SUBMATRICES ASSOCIATED WITH THE EIGENVALUES  BIS00420
C           IN W, IN TOP-TO-BOTTOM ORDER.                               BIS00430
C                                                                       BIS00440
C IERR   R  ZERO  FOR NORMAL RETURN.                                    BIS00450
C           3*N+1 IF M EXCEEDS MM.                                      BIS00460
C                                                                       BIS00470
C WL,WU  -  ANCILLARY STORAGE.                                          BIS00480
C                                                                       BIS00490
C NOTE THAT SUBROUTINES TQL1 OR IMTQL1 ARE GENERALLY FASTER THAN BISECT BIS00500
C IF MORE THAN N/4 EIGENVALUES ARE COMPUTED.                            BIS00510
C                                                                       BIS00520
C     THIS SUBROUTINE IS BASED ON THE BISECTION TECHNIQUE IN THE ALGOL  BIS00530
C     PROCEDURE TRISTURM BY PETERS AND WILKINSON.                       BIS00540
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 418-439(1971).   BIS00550
C                                                                       BIS00560
C     VERSION FOR THE IBM 3090VF DATED NOVEMBER 1987.                   BIS00570
C     ------------------------------------------------------------------BIS00580
      INTEGER     I, J, K, L, IP, IQ, IR, IS, M1, M2, ITAG, ISTURM      BIS00590
      REAL*8      U, V, WBL, WBU, XU, X0, X1, TST1, TST2, EPS           BIS00600
      DATA        EPS/Z3410000000000000/                                BIS00610
C                                                                       BIS00620
      CALL XUFLOW(0)                                                    BIS00630
      IERR=0                                                            BIS00640
      ITAG=0                                                            BIS00650
      WBL=BL                                                            BIS00660
      WBU=BU                                                            BIS00670
C SEARCH FOR SMALL SUBDIAGONAL ENTRIES                                  BIS00680
      E2(1)=0D0                                                         BIS00690
         DO 100 I=1,N                                                   BIS00700
         TST1=DABS(D(I))+DABS(D(I-1))                                   BIS00710
         TST2=TST1+DABS(E(I))                                           BIS00720
         IF(TST2.GT.TST1) GO TO 100                                     BIS00730
         E2(I)=0D0                                                      BIS00740
100      CONTINUE                                                       BIS00750
C NUMBER OF EIGENVALUES IN THE INTERVAL                                 BIS00760
      IP=1                                                              BIS00770
      IQ=N                                                              BIS00780
      X1=WBU                                                            BIS00790
      ISTURM=1                                                          BIS00800
      GO TO 400                                                         BIS00810
120   M=IS                                                              BIS00820
      X1=WBL                                                            BIS00830
      ISTURM=2                                                          BIS00840
      GO TO 400                                                         BIS00850
140   M=M-IS                                                            BIS00860
      IF(M.GT.MM) GO TO 999                                             BIS00870
      IQ=0                                                              BIS00880
      IR=0                                                              BIS00890
C ISOLATION OF A SUBMATRIX AND REFINEMENT OF THE INTERVAL WITH          BIS00900
C GERSCHGORIN BOUNDS                                                    BIS00910
160   IF(IR.EQ.M) GO TO 1000                                            BIS00920
      ITAG=ITAG+1                                                       BIS00930
      IP=IQ+1                                                           BIS00940
      XU=D(IP)                                                          BIS00950
      X0=D(IP)                                                          BIS00960
      U=0D0                                                             BIS00970
         DO 200 IQ=IP,N                                                 BIS00980
         X1=U                                                           BIS00990
         U=0D0                                                          BIS01000
         V=0D0                                                          BIS01010
         IF(IQ.EQ.N) GO TO 180                                          BIS01020
         U=DABS(E(IQ+1))                                                BIS01030
         V=E2(IQ+1)                                                     BIS01040
180      XU=DMIN1(D(IQ)-(X1+U),XU)                                      BIS01050
         X0=DMAX1(D(IQ)+(X1+U),X0)                                      BIS01060
         IF(V.EQ.0D0) GO TO 220                                         BIS01070
200      CONTINUE                                                       BIS01080
220   X1=EPS*DMAX1(DABS(XU),DABS(X0))                                   BIS01090
      IF(EPS1.LE.0D0) EPS1=-X1                                          BIS01100
      IF(IP.NE.IQ) GO TO 240                                            BIS01110
C CHECK FOR ISOLATED ROOT WITHIN THE INTERVAL                           BIS01120
      IF(BL.GT.D(IP) .OR. D(IP).GE.BU)GO TO 660                         BIS01130
      M1=IP                                                             BIS01140
      M2=IP                                                             BIS01150
      WU(IP)=D(IP)                                                      BIS01160
      GO TO 560                                                         BIS01170
240   X1=X1*(IQ-IP+1)                                                   BIS01180
      WBL=DMAX1(BL,XU-X1)                                               BIS01190
      WBU=DMIN1(BU,X0+X1)                                               BIS01200
      X1=WBL                                                            BIS01210
      ISTURM=3                                                          BIS01220
      GO TO 400                                                         BIS01230
260   M1=IS+1                                                           BIS01240
      X1=WBU                                                            BIS01250
      ISTURM=4                                                          BIS01260
      GO TO 400                                                         BIS01270
280   M2=IS                                                             BIS01280
      IF(M1.GT.M2) GO TO 660                                            BIS01290
C INITIAL INTERVALS FOR THE K-TH EIGENVALUE                             BIS01300
      X0=WBU                                                            BIS01310
      ISTURM=5                                                          BIS01320
         DO 300 I=M1,M2                                                 BIS01330
         WU(I)=WBU                                                      BIS01340
         WL(I)=WBL                                                      BIS01350
300      CONTINUE                                                       BIS01360
C COMPUTATION OF THE K-TH EIGENVALUE                                    BIS01370
      K=M2                                                              BIS01380
320      XU=WBL                                                         BIS01390
            DO 340 I=K,M1,-1                                            BIS01400
            IF (XU.GE.WL(I)) GO TO 340                                  BIS01410
               XU=WL(I)                                                 BIS01420
               GO TO 360                                                BIS01430
340         CONTINUE                                                    BIS01440
360      IF(X0.GT.WU(K)) X0=WU(K)                                       BIS01450
C BISECTION STEP                                                        BIS01460
380      X1=XU+0.5D0*(X0-XU)                                            BIS01470
C CONVERGENCE TEST FOR NONBINARY MACHINES BY L. HARDING (U. MICHIGAN)   BIS01480
         IF(X1.EQ.XU .OR. X1.GE.X0) GO TO 540                           BIS01490
         IF(X0-XU.LE.DABS(EPS1)) GO TO 540                              BIS01500
C PROCEDURE FOR STURM SEQUENCE                                          BIS01510
400      IS=IP-1                                                        BIS01520
         U=1D0                                                          BIS01530
            DO 460 I=IP,IQ                                              BIS01540
            IF(U.NE.0D0) GO TO 420                                      BIS01550
               V=DABS(E(I))/EPS                                         BIS01560
               IF(E2(I).EQ.0D0) V=0D0                                   BIS01570
               GO TO 440                                                BIS01580
420         V=E2(I)/U                                                   BIS01590
440         U=D(I)-X1-V                                                 BIS01600
            IF(U.LT.0D0) IS=IS+1                                        BIS01610
460         CONTINUE                                                    BIS01620
         GO TO (120,140,260,280,480),ISTURM                             BIS01630
C INTERVAL REFINEMENT                                                   BIS01640
480      IF(IS.GE.K) GO TO 520                                          BIS01650
            XU=X1                                                       BIS01660
            IF(IS.GE.M1) GO TO 500                                      BIS01670
               WL(M1)=X1                                                BIS01680
               GO TO 380                                                BIS01690
500         WL(IS+1)=X1                                                 BIS01700
            IF(WU(IS).GT.X1) WU(IS)=X1                                  BIS01710
               GO TO 380                                                BIS01720
520      X0=X1                                                          BIS01730
         GO TO 380                                                      BIS01740
C K_TH EIGENVALUE                                                       BIS01750
540   WU(K)=X1                                                          BIS01760
      K=K-1                                                             BIS01770
      IF(K.GE.M1) GO TO 320                                             BIS01780
C ORDERING OF THE EIGENVALUES WITH THEIR SUBMATRIX ASSOCIATIONS         BIS01790
560   IS=IR                                                             BIS01800
      IR=IR+M2-M1+1                                                     BIS01810
      J=1                                                               BIS01820
      K=M1                                                              BIS01830
         DO 640 L=1,IR                                                  BIS01840
         IF(J.GT.IS) GO TO 600                                          BIS01850
         IF(K.GT.M2) GO TO 660                                          BIS01860
         IF(WU(K).GE.W(L)) GO TO 620                                    BIS01870
            DO 580 I=L+IS-J,L,-1                                        BIS01880
            W(I+1)=W(I)                                                 BIS01890
            IND(I+1)=IND(I)                                             BIS01900
580         CONTINUE                                                    BIS01910
600      W(L)=WU(K)                                                     BIS01920
         IND(L)=ITAG                                                    BIS01930
         K=K+1                                                          BIS01940
         GO TO 640                                                      BIS01950
620      J=J+1                                                          BIS01960
640      CONTINUE                                                       BIS01970
660   IF(IQ.LT.N) GO TO 160                                             BIS01980
      GO TO 1000                                                        BIS01990
C ERROR: UNDERESTIMATED NUMBER OF EIGENVALUES IN INTERVAL               BIS02000
999   IERR=3*N+1                                                        BIS02010
C                                                                       BIS02020
1000  RETURN                                                            BIS02030
      END                                                               BIS02040
