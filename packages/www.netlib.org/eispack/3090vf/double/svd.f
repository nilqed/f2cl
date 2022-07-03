@PROCESS DIRECTIVE('"    (')                                            SIN00020
C-----------------------------------------------------------------------SIN00030
C SINGULAR-VALUE DECOMPOSITION OF A MATRIX  A = U D V'                  SIN00040
C                                                                       SIN00050
      SUBROUTINE  SVD ( LD, M, N, A, D, MATU, U, MATV, V, IERR, E )     SIN00060
C                                                                       SIN00070
      INTEGER     M, N, LD, IERR                                        SIN00080
      REAL*8      A(LD,N), D(N), U(LD,N), V(LD,N), E(N)                 SIN00090
      LOGICAL     MATU, MATV                                            SIN00100
C                                                                       SIN00110
C LD     E  FIRST DIMENSION ASSIGNED TO THE TWO-DIMENSIONAL ARRAY       SIN00120
C           PARAMETERS IN THE CALLING PROGRAM.  LD MUST NOT BE LESS     SIN00130
C           THAN MAX(M,N).                                              SIN00140
C                                                                       SIN00150
C M      E  NUMBER OF ROWS OF A (AND U).                                SIN00160
C                                                                       SIN00170
C N      E  NUMBER OF COLUMNS OF A (AND U) AND ORDER OF V.              SIN00180
C                                                                       SIN00190
C A      E  RECTANGULAR MATRIX TO BE DECOMPOSED.                        SIN00200
C        R  A MAY BE OVERWRITTEN BY U (OR V) IF THE CORRESPONDING ARRAYSSIN00210
C           ARE SPECIFIED TO COINCIDE IN STORAGE.                       SIN00220
C                                                                       SIN00230
C D      R  UNORDERED N SINGULAR VALUES OF A.                           SIN00240
C           WHEN AN ERROR EXIT OCCURS, THE SINGULAR VALUES OF INDICES   SIN00250
C           IERR+1,...,N  SHOULD BE CORRECT.                            SIN00260
C                                                                       SIN00270
C MATU   E  SET TO .TRUE. IF THE LEFT MATRIX OF THE DECOMPOSITION IS    SIN00280
C           TO BE COMPUTED, AND SET TO .FALSE. OTHERWISE.               SIN00290
C                                                                       SIN00300
C U      R  LEFT MATRIX OF THE DECOMPOSITION IF MATU IS SET TO .TRUE.   SIN00310
C           IF AN ERROR EXIT OCCURS, THE COLUMNS OF U OF INDICES        SIN00320
C           IERR+1,...,N SHOULD BE CORRECT.                             SIN00330
C           IF MATU IS SET TO .FALSE., THE ARRAY IS USED AS TEMPORARY   SIN00340
C           STORAGE.                                                    SIN00350
C           U MAY COINCIDE WITH A IN STORAGE.                           SIN00360
C                                                                       SIN00370
C MATV   E  SET TO .TRUE. IF THE RIGHT MATRIX OF THE DECOMPOSITION IS   SIN00380
C           TO BE COMPUTED, AND SET TO .FALSE. OTHERWISE.               SIN00390
C                                                                       SIN00400
C V      R  RIGHT MATRIX OF THE DECOMPOSITION IF MATV IS SET TO .TRUE.  SIN00410
C           IF AN ERROR EXIT OCCURS, THE COLUMNS OF V OF INDICES        SIN00420
C           IERR+1,...,N SHOULD BE CORRECT.                             SIN00430
C           IF MATV IS SET TO .FALSE., NO REFERENCE TO V IS MADE DURING SIN00440
C           PROGRAM EXECUTION.                                          SIN00450
C           V MAY COINCIDE WITH A IN STORAGE.                           SIN00460
C                                                                       SIN00470
C IERR   R  ZERO  FOR NORMAL RETURN,                                    SIN00480
C           K     IF THE K-TH SINGULAR VALUE CANNOT BE COMPUTED WITHIN  SIN00490
C                 30 ITERATIONS.                                        SIN00500
C                                                                       SIN00510
C E      -  ANCILLARY STORAGE.                                          SIN00520
C                                                                       SIN00530
C     THIS SUBROUTINE IS BASED ON THE ALGOL PROCEDURE SVD,              SIN00540
C     NUM. MATH. 14, 403-420(1970) BY GOLUB AND REINSCH.                SIN00550
C     HANDBOOK FOR AUTO. COMP., VOL II-LINEAR ALGEBRA, 134-151(1971).   SIN00560
C                                                                       SIN00570
C SUBPROGRAMS CALLED: DSC16   KACHEL                                    SIN00580
C                                                                       SIN00590
C     VERSION FOR THE IBM 3090VF DATED NOVEMBER 1987.                   SIN00600
C     ------------------------------------------------------------------SIN00610
      INTEGER     I, J, K, KK, L, MN, MAXIT, IT, KACHE                  SIN00620
      INTEGER     KACHEL                                                SIN00630
      REAL*8      C, S, X, Y, Z, F, G, H, BNORM, S16, R16               SIN00640
      LOGICAL     NOMATV, NOMATU                                        SIN00650
      DATA        MAXIT/30/                                             SIN00660
C                                                                       SIN00670
      CALL XUFLOW(0)                                                    SIN00680
      IERR=0                                                            SIN00690
      NOMATV=.NOT.MATV                                                  SIN00700
      NOMATU=.NOT.MATU                                                  SIN00710
         DO 110 I=1,M                                                   SIN00720
            DO 100 J=1,N                                                SIN00730
100         U(I,J)=A(I,J)                                               SIN00740
110      CONTINUE                                                       SIN00750
C                                                                       SIN00760
C REDUCTION TO UPPER-BIDIAGONAL FORM BY HOUSEHOLDER TRANSFORMATIONS     SIN00770
      E(1)=0D0                                                          SIN00780
      MN=MIN0(M,N)                                                      SIN00790
      KACHE=KACHEL(LD)                                                  SIN00800
      KK=N-KACHE                                                        SIN00810
         DO 340 K=1,MN                                                  SIN00820
C   L1 NORM OF THE SUBDIAGONAL PART OF COLUMN K                         SIN00830
         X=0D0                                                          SIN00840
            DO 120 I=K+1,M                                              SIN00850
120         X=X+DABS(U(I,K))                                            SIN00860
         IF(X.NE.0D0) GO TO 140                                         SIN00870
C   THE TRANSFORMATION IS BYPASSED IF THE NORM IS ZERO                  SIN00880
            D(K)=U(K,K)                                                 SIN00890
            U(K,K)=0D0                                                  SIN00900
               DO 130 J=K+1,N                                           SIN00910
130            E(J)=U(K,J)                                              SIN00920
            GO TO 240                                                   SIN00930
140      X=X+DABS(U(K,K))                                               SIN00940
C   SCALED HOUSEHOLDER VECTOR                                           SIN00950
         CALL DSC16 (X,S16,R16)                                         SIN00960
         S=0D0                                                          SIN00970
            DO 150 I=K,M                                                SIN00980
            U(I,K)=R16*U(I,K)                                           SIN00990
150         S=S+U(I,K)**2                                               SIN01000
         S=-DSIGN(DSQRT(S),U(K,K))                                      SIN01010
         X=1D0/S                                                        SIN01020
            DO 160 I=K,M                                                SIN01030
160         U(I,K)=X*U(I,K)                                             SIN01040
         U(K,K)=U(K,K)-1D0                                              SIN01050
C   DIAGONAL ELEMENT                                                    SIN01060
         D(K)=S16*S                                                     SIN01070
C   LEFT TRANSFORMATION                                                 SIN01080
         X=1D0/U(K,K)                                                   SIN01090
         IF(K.LT.KK) GO TO 200                                          SIN01100
C     PATH FOR SMALL ARRAYS                                             SIN01110
C"    ( IGNORE RECRDEPS                                                 SIN01120
C"    ( PREFER VECTOR                                                   SIN01130
            DO 190 J=K+1,N                                              SIN01140
            S=0D0                                                       SIN01150
               DO 170 I=K,M                                             SIN01160
170            S=S+U(I,J)*U(I,K)                                        SIN01170
            S=S*X                                                       SIN01180
               DO 180 I=K,M                                             SIN01190
180            U(I,J)=U(I,J)+S*U(I,K)                                   SIN01200
            E(J)=U(K,J)                                                 SIN01210
190         CONTINUE                                                    SIN01220
         GO TO 240                                                      SIN01230
C     PATH FOR LARGE ARRAYS                                             SIN01240
200         DO 230 J=K+1,N                                              SIN01250
            S=0D0                                                       SIN01260
               DO 210 I=K,M                                             SIN01270
210            S=S+U(I,J)*U(I,K)                                        SIN01280
            S=S*X                                                       SIN01290
               DO 220 I=K,M                                             SIN01300
220            U(I,J)=U(I,J)+S*U(I,K)                                   SIN01310
            E(J)=U(K,J)                                                 SIN01320
230         CONTINUE                                                    SIN01330
240      IF(K.EQ.N) GO TO 350                                           SIN01340
C   L1 NORM OF THE SUPERCODIAGONAL PART OF ROW K                        SIN01350
         X=0D0                                                          SIN01360
            DO 250 J=K+2,N                                              SIN01370
250         X=X+DABS(E(J))                                              SIN01380
         IF(X.NE.0D0) GO TO 260                                         SIN01390
C   THE TRANSFORMATION IS BYPASSED IF THE NORM IS ZERO                  SIN01400
            IF(NOMATV) GO TO 340                                        SIN01410
               DO 255 I=K+1,N                                           SIN01420
255            V(I,K+1)=0D0                                             SIN01430
            GO TO 340                                                   SIN01440
C   SCALED HOUSEHOLDER VECTOR                                           SIN01450
260      X=X+DABS(E(K+1))                                               SIN01460
         CALL DSC16(X,S16,R16)                                          SIN01470
         S=0D0                                                          SIN01480
            DO 270 J=K+1,N                                              SIN01490
            E(J)=R16*E(J)                                               SIN01500
270         S=S+E(J)**2                                                 SIN01510
         Y=-DSIGN(DSQRT(S),E(K+1))                                      SIN01520
         X=1D0/Y                                                        SIN01530
            DO 280 J=K+1,N                                              SIN01540
280         E(J)=X*E(J)                                                 SIN01550
         E(K+1)=E(K+1)-1D0                                              SIN01560
         X=1D0/E(K+1)                                                   SIN01570
C     THE HOUSEHOLDER VECTOR IS SAVED IF MATRIX V IS REQUESTED          SIN01580
         IF(NOMATV) GO TO 300                                           SIN01590
            DO 290 I=K+1,N                                              SIN01600
290         V(I,K+1)=E(I)                                               SIN01610
C   RIGHT TRANSFORMATION                                                SIN01620
300         DO 330 I=K+1,M                                              SIN01630
            S=0D0                                                       SIN01640
               DO 310 J=K+1,N                                           SIN01650
310            S=S+U(I,J)*E(J)                                          SIN01660
            S=S*X                                                       SIN01670
               DO 320 J=K+1,N                                           SIN01680
320            U(I,J)=U(I,J)+S*E(J)                                     SIN01690
330         CONTINUE                                                    SIN01700
C   CODIAGONAL ELEMENT                                                  SIN01710
         E(K+1)=S16*Y                                                   SIN01720
340      CONTINUE                                                       SIN01730
350   IF(NOMATU) GO TO 460                                              SIN01740
C                                                                       SIN01750
C LEFT MATRIX OF THE BIDIAGONAL DECOMPOSITION                           SIN01760
         DO 360 J=MN+1,N                                                SIN01770
            DO 360 I=1,M                                                SIN01780
            U(I,J)=0D0                                                  SIN01790
360      CONTINUE                                                       SIN01800
      KK=MN-KACHE                                                       SIN01810
         DO 450 K=MN,1,-1                                               SIN01820
            DO 370 I=1,K-1                                              SIN01830
370         U(I,K)=0D0                                                  SIN01840
         IF(U(K,K).EQ.0D0) GO TO 450                                    SIN01850
         X=1D0/U(K,K)                                                   SIN01860
         IF(K.LT.KK) GO TO 410                                          SIN01870
C   PATH FOR SMALL ARRAYS                                               SIN01880
C"    ( IGNORE RECRDEPS                                                 SIN01890
C"    ( PREFER VECTOR                                                   SIN01900
            DO 400 J=K+1,MN                                             SIN01910
            S=0D0                                                       SIN01920
               DO 380 I=K,M                                             SIN01930
380            S=S+U(I,K)*U(I,J)                                        SIN01940
            S=S*X                                                       SIN01950
               DO 390 I=K,M                                             SIN01960
390            U(I,J)=U(I,J)+S*U(I,K)                                   SIN01970
400         CONTINUE                                                    SIN01980
         GO TO 450                                                      SIN01990
C   PATH FOR LARGE ARRAYS                                               SIN02000
410         DO 440 J=K+1,MN                                             SIN02010
            S=0D0                                                       SIN02020
               DO 420 I=K,M                                             SIN02030
420            S=S+U(I,K)*U(I,J)                                        SIN02040
            S=S*X                                                       SIN02050
               DO 430 I=K,M                                             SIN02060
430            U(I,J)=U(I,J)+S*U(I,K)                                   SIN02070
440         CONTINUE                                                    SIN02080
450      U(K,K)=1D0+U(K,K)                                              SIN02090
460   L=MN                                                              SIN02100
      IF(M.LT.N) L=L+1                                                  SIN02110
      IF(NOMATV) GO TO 590                                              SIN02120
C RIGHT MATRIX OF THE BIDIAGONAL DECOMPOSITION                          SIN02130
         DO 480 J=L+1,N                                                 SIN02140
            DO 470 I=1,N                                                SIN02150
470         V(I,J)=0D0                                                  SIN02160
480      V(J,J)=1D0                                                     SIN02170
      KK=N-KACHE                                                        SIN02180
         DO 570  K=L,2,-1                                               SIN02190
            DO 490 I=1,K-1                                              SIN02200
490         V(I,K)=0D0                                                  SIN02210
         IF(V(K,K).EQ.0D0) GO TO 570                                    SIN02220
         X=1D0/V(K,K)                                                   SIN02230
         IF(K.LT.KK) GO TO 530                                          SIN02240
C   PATH FOR SMALL ARRAYS                                               SIN02250
C"    ( IGNORE RECRDEPS                                                 SIN02260
C"    ( PREFER VECTOR                                                   SIN02270
            DO 520 J=K+1,N                                              SIN02280
            S=0D0                                                       SIN02290
               DO 500 I=K,N                                             SIN02300
500            S=S+V(I,K)*V(I,J)                                        SIN02310
            S=S*X                                                       SIN02320
               DO 510 I=K,N                                             SIN02330
510            V(I,J)=V(I,J)+S*V(I,K)                                   SIN02340
520         CONTINUE                                                    SIN02350
         GO TO 570                                                      SIN02360
C   PATH FOR LARGE ARRAYS                                               SIN02370
530         DO 560  J=K+1,N                                             SIN02380
            S=0D0                                                       SIN02390
               DO 540  I=K,N                                            SIN02400
540            S=S+V(I,K)*V(I,J)                                        SIN02410
            S=S*X                                                       SIN02420
               DO 550  I=K,N                                            SIN02430
550            V(I,J)=V(I,J)+S*V(I,K)                                   SIN02440
560         CONTINUE                                                    SIN02450
570      V(K,K)=1D0+V(K,K)                                              SIN02460
      V(1,1)=1D0                                                        SIN02470
         DO 580  I=2,N                                                  SIN02480
580      V(I,1)=0D0                                                     SIN02490
C ANNIHILATION OF THE LAST SUPERDIAGONAL ELEMENT WHEN  M < N            SIN02500
590   IF(M.GE.N) GO TO 640                                              SIN02510
      C= 1D0                                                            SIN02520
      S=-1D0                                                            SIN02530
         DO 630  K=MN,1,-1                                              SIN02540
C  THE SPURIOUS ELEMENT X IS CHASED UPWARDS IN COLUMN MN+1              SIN02550
         X=-S*E(K+1)                                                    SIN02560
         E(K+1)=C*E(K+1)                                                SIN02570
         IF(X.EQ.0D0) GO TO 640                                         SIN02580
         IF(DABS(D(K)).LT.DABS(X)) GO TO 600                            SIN02590
            S=X/D(K)                                                    SIN02600
            Y=DSQRT(1D0+S*S)                                            SIN02610
            C=1D0/Y                                                     SIN02620
            S=S*C                                                       SIN02630
            D(K)=Y*D(K)                                                 SIN02640
            GO TO 610                                                   SIN02650
600      C=D(K)/X                                                       SIN02660
         Y=DSQRT(1D0+C*C)                                               SIN02670
         S=1D0/Y                                                        SIN02680
         C=C*S                                                          SIN02690
         D(K)=Y*X                                                       SIN02700
610      IF(NOMATV) GO TO 630                                           SIN02710
            DO 620  I=1,N                                               SIN02720
            X=V(I,K)                                                    SIN02730
            Y=V(I,L)                                                    SIN02740
            V(I,K )=C*X+S*Y                                             SIN02750
620         V(I,L)=C*Y-S*X                                              SIN02760
630      CONTINUE                                                       SIN02770
640      DO 650  I=MN+1,N                                               SIN02780
650      D(I)=0D0                                                       SIN02790
C                                                                       SIN02800
C SINGULAR-VALUE DECOMPOSITION OF THE UPPER-BIDIAGONAL MATRIX OF ORDER  SIN02810
C    MIN(M,N)                                                           SIN02820
C   L1 NORM OF THE BIDIAGONAL MATRIX                                    SIN02830
      BNORM=0D00                                                        SIN02840
         DO 660  I=1,MN                                                 SIN02850
         BNORM=DMAX1(DABS(D(I))+DABS(E(I)),BNORM)                       SIN02860
660      CONTINUE                                                       SIN02870
C   DIAGONALIZATION                                                     SIN02880
         DO 860  K=MN,1,-1                                              SIN02890
         IT=0                                                           SIN02900
C   TESTS FOR SPLITTING                                                 SIN02910
670         DO 680  L=K,1,-1                                            SIN02920
            X=BNORM+DABS(E(L))                                          SIN02930
            IF(X.EQ.BNORM) GO TO 740                                    SIN02940
            X=BNORM+DABS(D(L-1))                                        SIN02950
            IF (X.EQ.BNORM) GO TO 690                                   SIN02960
680         CONTINUE                                                    SIN02970
C   CANCELLATION OF E(L)                                                SIN02980
690      C=0D00                                                         SIN02990
         S=1D00                                                         SIN03000
            DO 730  I=L,K                                               SIN03010
            F=S*E(I)                                                    SIN03020
            E(I)=C*E(I)                                                 SIN03030
            X=BNORM+DABS(F)                                             SIN03040
            IF(X.EQ.BNORM) GO TO 740                                    SIN03050
            G=D(I)                                                      SIN03060
            IF(DABS(G).LT.DABS(F)) GO TO 700                            SIN03070
               S=F/G                                                    SIN03080
               H=DSQRT(1D0+S*S)                                         SIN03090
               C=1D0/H                                                  SIN03100
               S=-S*C                                                   SIN03110
               D(I)=G*H                                                 SIN03120
               GO TO 710                                                SIN03130
700         C=G/F                                                       SIN03140
            H=DSQRT(1D0+C*C)                                            SIN03150
            S=-1D0/H                                                    SIN03160
            C=-C*S                                                      SIN03170
            D(I)=F*H                                                    SIN03180
710         IF (NOMATU) GO TO 730                                       SIN03190
C   UPDATE OF MATRIX U                                                  SIN03200
               DO 720  J=1,M                                            SIN03210
               Y=U(J,L-1)                                               SIN03220
               Z=U(J,I)                                                 SIN03230
               U(J,L-1)=Y*C+Z*S                                         SIN03240
               U(J,I)=-Y*S+Z*C                                          SIN03250
720            CONTINUE                                                 SIN03260
730         CONTINUE                                                    SIN03270
C TEST FOR CONVERGENCE                                                  SIN03280
740      Z=D(K)                                                         SIN03290
         IF (L.EQ.K) GO TO 840                                          SIN03300
C SHIFT FROM BOTTOM PRINCIPAL MATRIX OF ORDER 2                         SIN03310
         IF (IT.EQ.MAXIT) GO TO 999                                     SIN03320
         IT=IT+1                                                        SIN03330
         X=D(L)                                                         SIN03340
         Y=D(K-1)                                                       SIN03350
         G=E(K-1)                                                       SIN03360
         H=E(K)                                                         SIN03370
         F=0.5D00*(((G+Z)/H)*((G-Z)/Y)+Y/H-H/Y)                         SIN03380
         IF(DABS(F).GE.1D0) GO TO 750                                   SIN03390
            F=X-(Z/X)*Z+(H/X)*(Y/(F+DSIGN(DSQRT(1D0+F*F),F))-H)         SIN03400
            GO TO 760                                                   SIN03410
750      F=X-(Z/X)*Z+(H/X)*(Y/(F*(1D0+DSQRT(1D0+(1D0/F)**2)))-H)        SIN03420
C QR SWEEP                                                              SIN03430
760      C=1D00                                                         SIN03440
         S=1D00                                                         SIN03450
            DO 830  I=L,K-1                                             SIN03460
            G=E(I+1)                                                    SIN03470
            Y=D(I+1)                                                    SIN03480
            H=S*G                                                       SIN03490
            G=C*G                                                       SIN03500
            IF(DABS(F).LT.DABS(H)) GO TO 770                            SIN03510
               S=H/F                                                    SIN03520
               Z=DSQRT(1D0+S*S)                                         SIN03530
               C=1D0/Z                                                  SIN03540
               S=S*C                                                    SIN03550
               E(I)=F*Z                                                 SIN03560
               GO TO 780                                                SIN03570
770         C=F/H                                                       SIN03580
            Z=DSQRT(1D0+C*C)                                            SIN03590
            S=1D0/Z                                                     SIN03600
            C=C*S                                                       SIN03610
            E(I)=H*Z                                                    SIN03620
780         F=X*C+G*S                                                   SIN03630
            G=-X*S+G*C                                                  SIN03640
            H=Y*S                                                       SIN03650
            Y=Y*C                                                       SIN03660
            IF (NOMATV) GO TO 800                                       SIN03670
C   UPDATE OF MATRIX V                                                  SIN03680
               DO 790  J=1,N                                            SIN03690
               X=V(J,I)                                                 SIN03700
               Z=V(J,I+1)                                               SIN03710
               V(J,I)=X*C+Z*S                                           SIN03720
               V(J,I+1)=-X*S+Z*C                                        SIN03730
790            CONTINUE                                                 SIN03740
800         Z=DMAX1(DABS(F),DABS(H))                                    SIN03750
            Z=Z*DSQRT((F/Z)**2+(H/Z)**2)                                SIN03760
            D(I)=Z                                                      SIN03770
            IF (Z.EQ.0D00) GO TO 810                                    SIN03780
            C=F/Z                                                       SIN03790
            S=H/Z                                                       SIN03800
810         F=C*G+S*Y                                                   SIN03810
            X=-S*G+C*Y                                                  SIN03820
            IF (NOMATU) GO TO 830                                       SIN03830
C   UPDATE OF MATRIX U                                                  SIN03840
               DO 820  J=1,M                                            SIN03850
               Y=U(J,I)                                                 SIN03860
               Z=U(J,I+1)                                               SIN03870
               U(J,I)=Y*C+Z*S                                           SIN03880
               U(J,I+1)=-Y*S+Z*C                                        SIN03890
820            CONTINUE                                                 SIN03900
830         CONTINUE                                                    SIN03910
         E(L)=0D00                                                      SIN03920
         E(K)=F                                                         SIN03930
         D(K)=X                                                         SIN03940
         GO TO 670                                                      SIN03950
C   K-TH SINGULAR VALUE                                                 SIN03960
840      IF (Z.GE.0D00) GO TO 860                                       SIN03970
         D(K)=-Z                                                        SIN03980
         IF (NOMATV) GO TO 860                                          SIN03990
            DO 850  J=1,N                                               SIN04000
850         V(J,K)=-V(J,K)                                              SIN04010
860      CONTINUE                                                       SIN04020
      GO TO 1000                                                        SIN04030
C                                                                       SIN04040
C NO CONVERGENCE TO A SINGULAR VALUE AFTER MAXIT ITERATIONS             SIN04050
999   IERR=K                                                            SIN04060
C                                                                       SIN04070
1000  RETURN                                                            SIN04080
      END                                                               SIN04090
