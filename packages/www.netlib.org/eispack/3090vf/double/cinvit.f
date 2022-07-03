C-----------------------------------------------------------------------COM00020
C EIGENVECTORS OF A COMPLEX UPPER-HESSENBERG MATRIX ASSOCIATED WITH A   COM00030
C GIVEN SET OF EIGENVALUES.                                             COM00040
                                                                        COM00050
      SUBROUTINE  CINVIT(LD,N,AR,AI,WR,WI,WANTED,MZ,M,ZR,ZI,            COM00060
     $                                                 IERR,B,DUMMY,U,V)COM00070
C                                                                       COM00080
      INTEGER     LD, N, MZ, M, IERR                                    COM00090
      REAL*8      AR(LD,N), AI(LD,N), WR(N), WI(N), ZR(LD,MZ), ZI(LD,MZ)COM00100
      REAL*8      B(1), DUMMY, U(N), V(N)                               COM00110
      LOGICAL     WANTED(N)                                             COM00120
C                                                                       COM00130
C LD     E  FIRST DIMENSION ASSIGNED TO THE TWO-DIMENSIONAL ARRAY       COM00140
C           PARAMETERS IN THE CALLING PROGRAM.                          COM00150
C                                                                       COM00160
C N      E  ORDER OF THE MATRIX.                                        COM00170
C                                                                       COM00180
C AR     E  REAL      PART OF THE HESSENBERG MATRIX.                    COM00190
C                                                                       COM00200
C AI     E  IMAGINARY PART OF THE HESSENBERG MATRIX.                    COM00210
C                                                                       COM00220
C WR     E  REAL PARTS OF THE EIGENVALUES AS RETURNED BY SUBROUTINE     COM00230
C           COMLR.                                                      COM00240
C        R  THE CONTENTS OF WR MAY HAVE BEEN ALTERED BECAUSE CLUSTERED  COM00250
C           EIGENVALUES ARE SLIGHTLY PERTURBED FOR THE GENERATION OF    COM00260
C           INDEPENDENT EIGENVECTORS.                                   COM00270
C                                                                       COM00280
C WI     E  IMAGINARY PARTS OF THE EIGENVALUES AS RETURNED BY SUBROUTINECOM00290
C           COMLR.                                                      COM00300
C                                                                       COM00310
C WANTED E  WANTED(J) MUST BE SET TO .TRUE. TO REQUEST THE EIGENVECTOR  COM00320
C           CORRESPONDING TO THE J-TH EIGENVALUE.                       COM00330
C                                                                       COM00340
C MZ     E  NUMBER OF COLUMNS ALLOCATED TO ARRAYS ZR AND ZI IN THE      COM00350
C           CALLING PROGRAM.                                            COM00360
C                                                                       COM00370
C M      R  NUMBER OF EIGENVECTORS COMPUTED.                            COM00380
C                                                                       COM00390
C ZR     R  ARRAY OF THE REAL PARTS OF THE EIGENVECTORS.                COM00400
C           THE EIGENVECTORS ARE NORMALIZED SO THAT THE COMPONENT OF    COM00410
C           LARGEST MAGNITUDE IS 1.                                     COM00420
C           ANY VECTOR WHICH FAILS THE ACCEPTANCE TEST IS SET TO ZERO.  COM00430
C                                                                       COM00440
C ZI     R  ARRAY OF THE IMAGINARY PARTS OF THE EIGENVECTORS.           COM00450
C                                                                       COM00460
C IERR   R  ZERO     : INDICATES A NORMAL RETURN.                       COM00470
C           -(2*N+1) : MORE THAN MZ COLUMNS OF Z ARE NEEDED TO STORE    COM00480
C                      THE REQUIRED EIGENVECTORS.                       COM00490
C           -K       : THE ITERATION CORRESPONDING TO THE K-TH EIGEN-   COM00500
C                      VALUE FAILED.                                    COM00510
C           -(N+K)   : BOTH OF THE ABOVE ERROR CONDITIONS OCCURRED.     COM00520
C                                                                       COM00530
C B      -  ANCILLARY STORAGE OF N*(N-1) CELLS.                         COM00540
C                                                                       COM00550
C DUMMY  -  UNUSED PARAMETER (TO PRESERVE THE EISPACK CALLING SEQUENCE) COM00560
C                                                                       COM00570
C U      -  ANCILLARY STORAGE                                           COM00580
C                                                                       COM00590
C V      -  ANCILLARY STORAGE                                           COM00600
C                                                                       COM00610
C     THIS SUBROUTINE IS BASED ON THE ALGOL PROCEDURE CXINVIT           COM00620
C     BY PETERS AND WILKINSON,                                          COM00630
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 418-439(1971).   COM00640
C                                                                       COM00650
C     VERSION FOR THE IBM 3090VF DATED NOVEMBER 1987.                   COM00660
C     ------------------------------------------------------------------COM00670
      INTEGER     I, II, J, K, L, KU, IT, KU2                           COM00680
      REAL*8      W, S, T, X, Y, EPS, EPS3, EPS4, GROTOL, ANORM, SQRKU  COM00690
      REAL*8      ROOTR, ROOTI                                          COM00700
      DATA        EPS/Z3410000000000000/                                COM00710
C                                                                       COM00720
      CALL XUFLOW(0)                                                    COM00730
      IERR=0                                                            COM00740
      KU=0                                                              COM00750
      L=0                                                               COM00760
         DO 780  K=1,N                                                  COM00770
         IF(.NOT.WANTED(K)) GO TO 780                                   COM00780
         J=L+1                                                          COM00790
         IF(J.GT.MZ) GO TO 999                                          COM00800
         L=J                                                            COM00810
         IF(KU.GE.K) GO TO 200                                          COM00820
C SEARCH FOR SPLITTING                                                  COM00830
            DO 120 KU=K,N-1                                             COM00840
            IF(AR(KU+1,KU).EQ.0D0 .AND. AI(KU+1,KU).EQ.0D0) GO TO 140   COM00850
120         CONTINUE                                                    COM00860
         KU=N                                                           COM00870
C L1 NORM OF THE LEADING HESSENBERG MATRIX OF ORDER KU                  COM00880
140      ANORM=0D0                                                      COM00890
         X=CDABS(DCMPLX(AR(1,1),AI(1,1)))                               COM00900
            DO 180 J=2,KU                                               COM00910
            ANORM=DMAX1(X+CDABS(DCMPLX(AR(J,J-1),AI(J,J-1))),ANORM)     COM00920
            X=0D0                                                       COM00930
               DO 160 I=1,J                                             COM00940
160            X=X+CDABS(DCMPLX(AR(I,J),AI(I,J)))                       COM00950
180         CONTINUE                                                    COM00960
         ANORM=DMAX1(X,ANORM)                                           COM00970
C EPS3 REPLACES ZERO PIVOTS AND PERTURBS CLUSTERED ROOTS                COM00980
         IF(ANORM.EQ.0D0) ANORM=1D0                                     COM00990
         EPS3=EPS*ANORM                                                 COM01000
C GROTOL IS THE TEST VALUE FOR GROWTH                                   COM01010
         SQRKU=DSQRT(DFLOAT(KU))                                        COM01020
         EPS4=EPS3/(1D0+SQRKU)                                          COM01030
         GROTOL=0.1D0/SQRKU                                             COM01040
C CURRENT ROOT                                                          COM01050
200      ROOTR=WR(K)                                                    COM01060
         ROOTI=WI(K)                                                    COM01070
         IF(K.EQ.1) GO TO 280                                           COM01080
         GO TO 240                                                      COM01090
C PERTURBATION OF THE ROOT IF IT IS IN A CLUSTER                        COM01100
220      ROOTR=ROOTR+EPS3                                               COM01110
240         DO 260 I=K-1,1,-1                                           COM01120
            IF(WANTED(I) .AND. DABS(WR(I)-ROOTR).LT.EPS3                COM01130
     $                   .AND. DABS(WI(I)-ROOTI).LT.EPS3) GO TO 220     COM01140
260         CONTINUE                                                    COM01150
         WR(K)=ROOTR                                                    COM01160
280      IT=1                                                           COM01170
C LR DECOMPOSITION OF THE SHIFTED MATRIX                                COM01180
C    INITIAL RUNNING ROW OF R AND RIGHT-HAND SIDE                       COM01190
         KU2=KU+KU                                                      COM01200
            DO 300 I=1,KU                                               COM01210
            U(I)=AR(1,I)                                                COM01220
            V(I)=AI(1,I)                                                COM01230
300         ZR(I,L)=EPS3                                                COM01240
         U(1)=U(1)-ROOTR                                                COM01250
         V(1)=V(1)-ROOTI                                                COM01260
C    R FACTOR OF THE DECOMPOSITION: DIAGONAL   IN U, V                  COM01270
C                                   UPPER PART IN B (PACKED)            COM01280
            DO 460 I=1,KU-1                                             COM01290
            II=KU2*(I-1)-I*I-I                                          COM01300
            S=AR(I+1,I)                                                 COM01310
            T=AI(I+1,I)                                                 COM01320
            X=DABS(U(I))+DABS(V(I))+DABS(S)+DABS(T)                     COM01330
            IF(U(I)*(U(I)/X)+V(I)*(V(I)/X).LT.S*(S/X)+T*(T/X)) GO TO 380COM01340
C        NO INTERCHANGE                                                 COM01350
            IF(U(I).EQ.0D0 .AND. V(I).EQ.0D0) U(I)=EPS3                 COM01360
            IF(DABS(U(I)).LT.DABS(V(I))) GO TO 320                      COM01370
               W=V(I)/U(I)                                              COM01380
               Y=-U(I)-W*V(I)                                           COM01390
               X=(S+W*T)/Y                                              COM01400
               Y=(T-W*S)/Y                                              COM01410
               GO TO 340                                                COM01420
320         W=U(I)/V(I)                                                 COM01430
            Y=-V(I)-W*U(I)                                              COM01440
            X=(W*S+T)/Y                                                 COM01450
            Y=(W*T-S)/Y                                                 COM01460
340            DO 360 J=I+1,KU                                          COM01470
               B(II+2*J-1)=U(J)                                         COM01480
               B(II+2*J  )=V(J)                                         COM01490
               T=U(J)                                                   COM01500
               U(J)=AR(I+1,J)+X*T-Y*V(J)                                COM01510
360            V(J)=AI(I+1,J)+Y*T+X*V(J)                                COM01520
            U(I+1)=U(I+1)-ROOTR                                         COM01530
            V(I+1)=V(I+1)-ROOTI                                         COM01540
            GO TO 460                                                   COM01550
C        INTERCHANGE OF ROWS I AND I+1                                  COM01560
380         IF(DABS(S).LT.DABS(T)) GO TO 400                            COM01570
               W=T/S                                                    COM01580
               Y=-S-W*T                                                 COM01590
               X=(U(I)+W*V(I))/Y                                        COM01600
               Y=(V(I)-W*U(I))/Y                                        COM01610
               GO TO 420                                                COM01620
400         W=S/T                                                       COM01630
            Y=-T-W*S                                                    COM01640
            X=(W*U(I)+V(I))/Y                                           COM01650
            Y=(W*V(I)-U(I))/Y                                           COM01660
420         U(I)=S                                                      COM01670
            V(I)=T                                                      COM01680
            S=AR(I+1,I+1)-ROOTR                                         COM01690
            T=AI(I+1,I+1)-ROOTI                                         COM01700
            B(II+2*I+1)=S                                               COM01710
            B(II+2*I+2)=T                                               COM01720
            U(I+1)=U(I+1)+X*S-Y*T                                       COM01730
            V(I+1)=V(I+1)+X*T+Y*S                                       COM01740
               DO 440 J=I+2,KU                                          COM01750
               S=AR(I+1,J)                                              COM01760
               T=AI(I+1,J)                                              COM01770
               B(II+2*J-1)=S                                            COM01780
               B(II+2*J  )=T                                            COM01790
               U(J)=U(J)+X*S-Y*T                                        COM01800
440            V(J)=V(J)+X*T+Y*S                                        COM01810
460         CONTINUE                                                    COM01820
         IF(U(KU).EQ.0D0 .AND. V(KU).EQ.0D0) U(KU)=EPS3                 COM01830
C BACKWARD SUBSTITUTION                                                 COM01840
480      X=U(KU)                                                        COM01850
         Y=V(KU)                                                        COM01860
         IF(DABS(X).LT.DABS(Y)) GO TO 500                               COM01870
            W=Y/X                                                       COM01880
            ZR(KU,L)=ZR(KU,L)/(X+W*Y)                                   COM01890
            ZI(KU,L)=-W*ZR(KU,L)                                        COM01900
            GO TO 520                                                   COM01910
500      W=X/Y                                                          COM01920
         ZI(KU,L)=-ZR(KU,L)/(Y+W*X)                                     COM01930
         ZR(KU,L)=-W*ZI(KU,L)                                           COM01940
520         DO 580 I=KU-1,1,-1                                          COM01950
            II=KU2*(I-1)-I*I-I                                          COM01960
            X=ZR(I,L)                                                   COM01970
            Y=0D0                                                       COM01980
               DO 540 J=I+1,KU                                          COM01990
               X=X-B(II+2*J-1)*ZR(J,L)+B(II+2*J)*ZI(J,L)                COM02000
540            Y=Y-B(II+2*J-1)*ZI(J,L)-B(II+2*J)*ZR(J,L)                COM02010
            IF(DABS(U(I)).LT.DABS(V(I))) GO TO 560                      COM02020
               W=V(I)/U(I)                                              COM02030
               T=U(I)+W*V(I)                                            COM02040
               ZR(I,L)=(X+W*Y)/T                                        COM02050
               ZI(I,L)=(Y-W*X)/T                                        COM02060
               GO TO 580                                                COM02070
560         W=U(I)/V(I)                                                 COM02080
            T=V(I)+W*U(I)                                               COM02090
            ZR(I,L)=(W*X+Y)/T                                           COM02100
            ZI(I,L)=(W*Y-X)/T                                           COM02110
580         CONTINUE                                                    COM02120
C ACCEPTANCE TEST                                                       COM02130
         ANORM=0D0                                                      COM02140
         X=0D0                                                          COM02150
            DO 600 I=1,KU                                               COM02160
            W=CDABS(DCMPLX(ZR(I,L),ZI(I,L)))                            COM02170
            ANORM=ANORM+W                                               COM02180
            IF(W.LE.X) GO TO 600                                        COM02190
               J=I                                                      COM02200
               X=W                                                      COM02210
600         CONTINUE                                                    COM02220
         IF(ANORM.LT.GROTOL) GO TO 700                                  COM02230
C ACCEPTED VECTOR                                                       COM02240
         X=ZR(J,L)                                                      COM02250
         Y=ZI(J,L)                                                      COM02260
         IF(DABS(X).LT.DABS(Y)) GO TO 620                               COM02270
            W=Y/X                                                       COM02280
            X=1D0/(X+W*Y)                                               COM02290
            Y=-W*X                                                      COM02300
            GO TO 640                                                   COM02310
620      W=X/Y                                                          COM02320
         Y=-1D0/(Y+W*X)                                                 COM02330
         X=-W*Y                                                         COM02340
640         DO 660 I=1,KU                                               COM02350
            T=ZR(I,L)                                                   COM02360
            ZR(I,L)=X*T-Y*ZI(I,L)                                       COM02370
660         ZI(I,L)=Y*T+X*ZI(I,L)                                       COM02380
            DO 680  I=KU+1,N                                            COM02390
            ZR(I,L)=0D0                                                 COM02400
680         ZI(I,L)=0D0                                                 COM02410
         GO TO 780                                                      COM02420
C      NEW STARTING VECTOR                                              COM02430
700      IF(IT.GE.KU) GO TO 740                                         COM02440
         IT=IT+1                                                        COM02450
         ZR(1,L)=EPS3                                                   COM02460
            DO 720  I=2,KU                                              COM02470
720         ZR(I,L)=EPS4                                                COM02480
         ZR(IT,L)=EPS4-SQRKU*EPS3                                       COM02490
         GO TO 480                                                      COM02500
C      REJECTED VECTOR                                                  COM02510
740      IERR=-K                                                        COM02520
            DO 760  I=1,N                                               COM02530
            ZR(I,L)=0D0                                                 COM02540
760         ZI(I,L)=0D0                                                 COM02550
780      CONTINUE                                                       COM02560
      GO TO 1000                                                        COM02570
C INSUFFICIENT STORAGE SPECIFIED FOR THE VECTORS                        COM02580
999   IF (IERR.NE.0) IERR=IERR-N                                        COM02590
      IF (IERR.EQ.0) IERR=-(2*N+1)                                      COM02600
C                                                                       COM02610
1000  M=L                                                               COM02620
      RETURN                                                            COM02630
      END                                                               COM02640
