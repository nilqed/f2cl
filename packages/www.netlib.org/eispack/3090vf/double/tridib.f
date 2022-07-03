C-----------------------------------------------------------------------SYM25600
C EIGENVALUES OF A SYMMETRIC TRIDIAGONAL MATRIX WHOSE SUBSCRIPTS LIE IN SYM25610
C A GIVEN CLOSED INTERVAL (BISECTION).                                  SYM25620
C                                                                       SYM25630
      SUBROUTINE  TRIDIB(N,EPS1,D,E,E2,BL,BU,M11,M,W,IND,IERR,WL,WU)    SYM25640
C                                                                       SYM25650
      INTEGER     M, N, M11, IERR, IND(N)                               SYM25660
      REAL*8      BL, BU, D(N), E(N), E2(N), W(M), WL(N), WU(N)         SYM25670
C                                                                       SYM25680
C N      E  ORDER OF THE MATRIX.                                        SYM25690
C                                                                       SYM25700
C EPS1   E  ABSOLUTE ERROR TOLERANCE FOR THE COMPUTED EIGENVALUES.      SYM25710
C           IT SHOULD BE COMMENSURATE WITH RELATIVE PERTURBATIONS IN THESYM25720
C           MATRIX ELEMENTS OF THE ORDER OF MACHINE PRECISION.          SYM25730
C        R  IF EPS1 IS NOT POSITIVE UPON ENTRY, IT IS RESET FOR EACH    SYM25740
C           SUBMATRIX TO A DEFAULT VALUE, NAMELY, MINUS THE             SYM25750
C           PRODUCT OF MACHINE PRECISION AND THE L1-NORM OF THE         SYM25760
C           SUBMATRIX.                                                  SYM25770
C                                                                       SYM25780
C D      E  DIAGONAL ELEMENTS OF THE TRIDIAGONAL MATRIX.                SYM25790
C                                                                       SYM25800
C E      E  SUBDIAGONAL ELEMENTS OF THE TRIDIAGONAL MATRIX IN CELLS     SYM25810
C           2,...,N.                                                    SYM25820
C                                                                       SYM25830
C E2     E  SQUARES OF THE CORRESPONDING ELEMENTS OF E.                 SYM25840
C        R  ELEMENTS OF E2 CORRESPONDING TO ELEMENTS OF E REGARDED AS   SYM25850
C           NEGLIGIBLE ARE REPLACED BY ZERO, CAUSING THE MATRIX TO SPLITSYM25860
C           INTO A DIRECT SUM OF PRINCIPAL SUBMATRICES.                 SYM25870
C                                                                       SYM25880
C BL,BU  R  BOUNDS OF AN INTERVAL CONTAINING EXACTLY THE DESIRED        SYM25890
C           EIGENVALUES.                                                SYM25900
C                                                                       SYM25910
C M11    E  LEFT BOUND OF THE SUBSCRIPT INTERVAL OF THE DESIRED         SYM25920
C           EIGENVALUES.                                                SYM25930
C                                                                       SYM25940
C M      E  NUMBER OF EIGENVALUES DESIRED.  THE RIGHT BOUND OF THE      SYM25950
C           SUBSCRIPT INTERVAL OF THE DESIRED EIGENVALUES IS            SYM25960
C           M22=M11+M-1.                                                SYM25970
C                                                                       SYM25980
C W      R  EIGENVALUES WITH SUBSCRIPTS  M11,...,M22  IN CELLS 1,...,M, SYM25990
C           IN THE ORDER OF INCREASING VALUE.                           SYM26000
C                                                                       SYM26010
C IND    R  INDICES OF THE SUBMATRICES ASSOCIATED WITH THE EIGENVALUES  SYM26020
C           IN W, IN TOP TO BOTTOM ORDER.                               SYM26030
C                                                                       SYM26040
C IERR   R  ZERO     FOR NORMAL RETURN,                                 SYM26050
C           3*N+1    IF MULTIPLE EIGENVALUES AT INDEX M11 MAKE          SYM26060
C                    UNIQUE SELECTION IMPOSSIBLE,                       SYM26070
C           3*N+2    IF MULTIPLE EIGENVALUES AT INDEX M22 MAKE          SYM26080
C                    UNIQUE SELECTION IMPOSSIBLE.                       SYM26090
C                                                                       SYM26100
C WL,WU  -  ANCILLARY STORAGE.                                          SYM26110
C                                                                       SYM26120
C     NOTE THAT SUBROUTINES TQL1, IMTQL1, OR TQLRAT ARE GENERALLY FASTERSYM26130
C     THAN TRIDIB IF MORE THAN N/4 EIGENVALUES ARE TO BE COMPUTED.      SYM26140
C                                                                       SYM26150
C     THIS SUBROUTINE IS BASED ON THE ALGOL PROCEDURE BISECT,           SYM26160
C     NUM. MATH. 9, 386-393(1967) BY BARTH, MARTIN, AND WILKINSON.      SYM26170
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 249-256(1971).   SYM26180
C                                                                       SYM26190
C     VERSION FOR THE IBM 3090VF DATED NOVEMBER 1987.                   SYM26200
C     ------------------------------------------------------------------SYM26210
      INTEGER     I, J, K, L, IP, IQ, IR, IS, M1, M2, M22, ITAG, ISTURM SYM26220
      REAL*8      U, V, WBL, WBU, XU, X0, X1, EPS1, TST1, TST2, EPS     SYM26230
      DATA        EPS/Z3410000000000000/                                SYM26240
C                                                                       SYM26250
      CALL XUFLOW(0)                                                    SYM26260
      IERR=0                                                            SYM26270
      ITAG=0                                                            SYM26280
      XU=D(1)                                                           SYM26290
      X0=D(1)                                                           SYM26300
      U=0D0                                                             SYM26310
C SEARCH FOR SMALL SUB-DIAGONAL ENTRIES AND DETERMINATION OF AN INTERVALSYM26320
C CONTAINING ALL THE EIGENVALUES                                        SYM26330
         DO 40 I=1,N                                                    SYM26340
         X1=U                                                           SYM26350
         U=0D0                                                          SYM26360
         IF (I.NE.N) U=DABS(E(I+1))                                     SYM26370
         XU=DMIN1(D(I)-(X1+U),XU)                                       SYM26380
         X0=DMAX1(D(I)+(X1+U),X0)                                       SYM26390
         IF (I.EQ.1) GO TO 20                                           SYM26400
         TST1=DABS(D(I))+DABS(D(I-1))                                   SYM26410
         TST2=TST1+DABS(E(I))                                           SYM26420
         IF (TST2.GT.TST1) GO TO 40                                     SYM26430
20       E2(I)=0D0                                                      SYM26440
40       CONTINUE                                                       SYM26450
      X1=N                                                              SYM26460
      X1=X1*EPS*DMAX1(DABS(XU),DABS(X0))                                SYM26470
      XU=XU-X1                                                          SYM26480
      BL=XU                                                             SYM26490
      X0=X0+X1                                                          SYM26500
      BU=X0                                                             SYM26510
C INTERVAL CONTAINING EXACTLY THE DESIRED EIGENVALUES                   SYM26520
      IP=1                                                              SYM26530
      IQ=N                                                              SYM26540
      M1=M11-1                                                          SYM26550
      IF (M1.EQ.0) GO TO 75                                             SYM26560
      ISTURM=1                                                          SYM26570
50    V=X1                                                              SYM26580
      X1=XU+(X0-XU)*0.5D0                                               SYM26590
      IF (X1.EQ.V) GO TO 980                                            SYM26600
      GO TO 320                                                         SYM26610
60    IF (IS-M1) 65,73,70                                               SYM26620
65    XU=X1                                                             SYM26630
      GO TO 50                                                          SYM26640
70    X0=X1                                                             SYM26650
      GO TO 50                                                          SYM26660
73    XU=X1                                                             SYM26670
      BL=X1                                                             SYM26680
75    M22=M1+M                                                          SYM26690
      IF (M22.EQ.N) GO TO 90                                            SYM26700
      X0=BU                                                             SYM26710
      ISTURM=2                                                          SYM26720
      GO TO 50                                                          SYM26730
80    IF (IS-M22) 65,85,70                                              SYM26740
85    BU=X1                                                             SYM26750
90    IQ=0                                                              SYM26760
      IR=0                                                              SYM26770
C ISOLATION OF A SUBMATRIX AND REFINEMENT OF THE INTERVAL WITH          SYM26780
C GERSCHGORIN BOUNDS                                                    SYM26790
100   IF (IR.EQ.M) GO TO 1001                                           SYM26800
      ITAG=ITAG+1                                                       SYM26810
      IP=IQ+1                                                           SYM26820
      XU=D(IP)                                                          SYM26830
      X0=D(IP)                                                          SYM26840
      U=0D0                                                             SYM26850
         DO 120 IQ=IP,N                                                 SYM26860
         X1=U                                                           SYM26870
         U=0D0                                                          SYM26880
         V=0D0                                                          SYM26890
         IF (IQ.EQ.N) GO TO 110                                         SYM26900
         U=DABS(E(IQ+1))                                                SYM26910
         V=E2(IQ+1)                                                     SYM26920
110      XU=DMIN1(D(IQ)-(X1+U),XU)                                      SYM26930
         X0=DMAX1(D(IQ)+(X1+U),X0)                                      SYM26940
         IF (V.EQ.0D0) GO TO 140                                        SYM26950
120      CONTINUE                                                       SYM26960
140   X1=EPS*DMAX1(DABS(XU),DABS(X0))                                   SYM26970
      IF (EPS1.LE.0D0) EPS1=-X1                                         SYM26980
      IF (IP.NE.IQ) GO TO 180                                           SYM26990
C CHECK FOR ISOLATED ROOT WITHIN INTERVAL                               SYM27000
      IF (BL.GT.D(IP) .OR. D(IP).GE.BU) GO TO 940                       SYM27010
      M1=IP                                                             SYM27020
      M2=IP                                                             SYM27030
      WU(IP)=D(IP)                                                      SYM27040
      GO TO 900                                                         SYM27050
180   X1=X1*(IQ-IP+1)                                                   SYM27060
      WBL=DMAX1(BL,XU-X1)                                               SYM27070
      WBU=DMIN1(BU,X0+X1)                                               SYM27080
      X1=WBL                                                            SYM27090
      ISTURM=3                                                          SYM27100
      GO TO 320                                                         SYM27110
200   M1=IS+1                                                           SYM27120
      X1=WBU                                                            SYM27130
      ISTURM=4                                                          SYM27140
      GO TO 320                                                         SYM27150
220   M2=IS                                                             SYM27160
      IF (M1.GT.M2) GO TO 940                                           SYM27170
C INITIAL INTERVAL FOR THE K-TH EIGENVALUE                              SYM27180
      X0=WBU                                                            SYM27190
      ISTURM=5                                                          SYM27200
         DO 240 I=M1,M2                                                 SYM27210
         WU(I)=WBU                                                      SYM27220
         WL(I)=WBL                                                      SYM27230
240      CONTINUE                                                       SYM27240
C COMPUTATION OF THE K-TH EIGENVALUE                                    SYM27250
      K=M2                                                              SYM27260
250      XU=WBL                                                         SYM27270
            DO 260 I=K,M1,-1                                            SYM27280
            IF (XU.GE.WL(I)) GO TO 260                                  SYM27290
            XU=WL(I)                                                    SYM27300
            GO TO 280                                                   SYM27310
260         CONTINUE                                                    SYM27320
280      IF (X0.GT.WU(K)) X0=WU(K)                                      SYM27330
C BISECTION STEP                                                        SYM27340
300      X1=(XU+X0)*0.5D0                                               SYM27350
         IF ((X0-XU).LE.DABS(EPS1)) GO TO 420                           SYM27360
         TST1=2D0*(DABS(XU)+DABS(X0))                                   SYM27370
         TST2=TST1+(X0-XU)                                              SYM27380
         IF (TST2.EQ.TST1) GO TO 420                                    SYM27390
C PROCEDURE FOR STURM SEQUENCE                                          SYM27400
320      IS=IP-1                                                        SYM27410
         U=1D0                                                          SYM27420
            DO 340 I=IP,IQ                                              SYM27430
            IF (U.NE.0D0) GO TO 325                                     SYM27440
            V=DABS(E(I))/EPS                                            SYM27450
            IF (E2(I).EQ.0D0) V=0D0                                     SYM27460
            GO TO 330                                                   SYM27470
325         V=E2(I)/U                                                   SYM27480
330         U=D(I)-X1-V                                                 SYM27490
            IF (U.LT.0D0) IS=IS+1                                       SYM27500
340         CONTINUE                                                    SYM27510
         GO TO (60,80,200,220,360),ISTURM                               SYM27520
C INTERVAL REFINEMENT                                                   SYM27530
360      IF (IS.GE.K) GO TO 400                                         SYM27540
         XU=X1                                                          SYM27550
         IF (IS.GE.M1) GO TO 380                                        SYM27560
         WL(M1)=X1                                                      SYM27570
         GO TO 300                                                      SYM27580
380      WL(IS+1)=X1                                                    SYM27590
         IF (WU(IS).GT.X1) WU(IS)=X1                                    SYM27600
         GO TO 300                                                      SYM27610
400      X0=X1                                                          SYM27620
         GO TO 300                                                      SYM27630
C K-TH EIGENVALUE                                                       SYM27640
420      WU(K)=X1                                                       SYM27650
      K=K-1                                                             SYM27660
      IF (K.GE.M1) GO TO 250                                            SYM27670
C ORDERING OF THE EIGENVALUES TAGGED WITH THEIR SUBMATRIX ASSOCIATIONS  SYM27680
900   IS=IR                                                             SYM27690
      IR=IR+M2-M1+1                                                     SYM27700
      J=1                                                               SYM27710
      K=M1                                                              SYM27720
         DO 920 L=1,IR                                                  SYM27730
         IF (J.GT.IS) GO TO 910                                         SYM27740
         IF (K.GT.M2) GO TO 940                                         SYM27750
         IF (WU(K).GE.W(L)) GO TO 915                                   SYM27760
            DO 905 I=L+IS-J,L,-1                                        SYM27770
            W(I+1)=W(I)                                                 SYM27780
            IND(I+1)=IND(I)                                             SYM27790
905         CONTINUE                                                    SYM27800
910      W(L)=WU(K)                                                     SYM27810
         IND(L)=ITAG                                                    SYM27820
         K=K+1                                                          SYM27830
         GO TO 920                                                      SYM27840
915      J=J+1                                                          SYM27850
920      CONTINUE                                                       SYM27860
940   IF (IQ.LT.N) GO TO 100                                            SYM27870
      GO TO 1001                                                        SYM27880
C ERROR: AN INTERVAL CONTAINING EXACTLY THE DESIRED EIGENVALUES CANNOT  SYM27890
C        BE FOUND                                                       SYM27900
980   IERR=3*N+ISTURM                                                   SYM27910
1001  RETURN                                                            SYM27920
      END                                                               SYM27930
