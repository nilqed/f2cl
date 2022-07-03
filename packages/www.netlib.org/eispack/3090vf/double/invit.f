C-----------------------------------------------------------------------REA09300
C EIGENVECTORS OF A REAL UPPER-HESSENBERG MATRIX ASSOCIATED WITH A GIVENREA09310
C SET OF EIGENVALUES.                                                   REA09320
C                                                                       REA09330
      SUBROUTINE  INVIT (LD,N,A,WR,WI,WANTED,MZ,M,Z,IERR,B,U,V)         REA09340
C                                                                       REA09350
      INTEGER     LD, N, MZ, M, IERR                                    REA09360
      REAL*8      A(LD,N), WR(N), WI(N), Z(LD,MZ), B(1), U(N), V(N)     REA09370
      LOGICAL     WANTED(N)                                             REA09380
C                                                                       REA09390
C LD     E  FIRST DIMENSION ASSIGNED TO THE TWO-DIMENSIONAL ARRAY       REA09400
C           PARAMETERS IN THE CALLING PROGRAM.                          REA09410
C                                                                       REA09420
C N      E  ORDER OF THE MATRIX.                                        REA09430
C                                                                       REA09440
C A      E  HESSENBERG MATRIX.                                          REA09450
C                                                                       REA09460
C WR     E  REAL PARTS OF THE EIGENVALUES AS RETURNED BY SUBROUTINE HQR.REA09470
C        R  THE CONTENTS OF WR MAY HAVE BEEN ALTERED BECAUSE CLUSTERED  REA09480
C           EIGENVALUES ARE SLIGHTLY PERTURBED FOR THE GENERATION OF    REA09490
C           INDEPENDENT EIGENVECTORS.                                   REA09500
C                                                                       REA09510
C WI     E  IMAGINARY PARTS OF THE EIGENVALUES AS RETURNED BY SUBROUTINEREA09520
C           HQR.                                                        REA09530
C                                                                       REA09540
C WANTED E  WANTED(J) MUST BE SET TO .TRUE. TO REQUEST THE EIGENVECTOR  REA09550
C           CORRESPONDING TO THE J-TH EIGENVALUE.                       REA09560
C        R  WANTED MAY HAVE BEEN ALTERED. IF THE ELEMENTS CORRESPONDING REA09570
C           TO A PAIR OF COMPLEX CONJUGATE EIGENVALUES WERE EACH        REA09580
C           INITIALLY SET TO .TRUE., THE PROGRAM RESETS THE SECOND OF   REA09590
C           THE TWO ELEMENTS TO .FALSE..                                REA09600
C                                                                       REA09610
C MZ     E  NUMBER OF COLUMNS ALLOCATED TO ARRAY Z IN THE CALLING       REA09620
C           PROGRAM.                                                    REA09630
C           NOTE THAT TWO COLUMNS ARE REQUIRED TO STORE THE EIGENVECTOR REA09640
C           CORRESPONDING TO A COMPLEX EIGENVALUE.                      REA09650
C                                                                       REA09660
C M      R  NUMBER OF COLUMNS OF ARRAY Z ACTUALLY USED TO STORE THE     REA09670
C           EIGENVECTORS.                                               REA09680
C                                                                       REA09690
C Z      R  ARRAY OF THE REAL AND IMAGINARY PARTS OF THE EIGENVECTORS.  REA09700
C           IF THE NEXT SELECTED EIGENVALUE IS REAL, THE NEXT COLUMN    REA09710
C           OF Z CONTAINS ITS EIGENVECTOR.  IF THE EIGENVALUE IS        REA09720
C           COMPLEX, THE NEXT TWO COLUMNS OF Z CONTAIN THE REAL AND     REA09730
C           IMAGINARY PARTS OF ITS EIGENVECTOR.  THE EIGENVECTORS ARE   REA09740
C           NORMALIZED SO THAT THE COMPONENT OF LARGEST MAGNITUDE IS 1. REA09750
C           ANY VECTOR WHICH FAILS THE ACCEPTANCE TEST IS SET TO ZERO.  REA09760
C                                                                       REA09770
C IERR   R  ZERO     : INDICATES A NORMAL RETURN.                       REA09780
C           -(2*N+1) : MORE THAN MZ COLUMNS OF Z ARE NEEDED TO STORE    REA09790
C                      THE REQUIRED EIGENVECTORS.                       REA09800
C           -K       : THE ITERATION CORRESPONDING TO THE K-TH EIGEN-   REA09810
C                      VALUE FAILED.                                    REA09820
C           -(N+K)   : BOTH OF THE ABOVE ERROR CONDITIONS OCCURRED.     REA09830
C                                                                       REA09840
C B      -  ANCILLARY STORAGE OF N*N CELLS.                             REA09850
C                                                                       REA09860
C U      -  ANCILLARY STORAGE                                           REA09870
C                                                                       REA09880
C V      -  ANCILLARY STORAGE                                           REA09890
C                                                                       REA09900
C     THIS SUBROUTINE IS BASED ON THE ALGOL PROCEDURE INVIT BY PETERS   REA09910
C     AND WILKINSON,                                                    REA09920
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 418-439(1971).   REA09930
C                                                                       REA09940
C     VERSION FOR THE IBM 3090VF DATED NOVEMBER 1987.                   REA09950
C     ------------------------------------------------------------------REA09960
      INTEGER     I, II, J, K, L, IP, KU, IT, KU2                       REA09970
      REAL*8      S, T, X, Y, EPS3, EPS4, ANORM                         REA09980
      REAL*8      ROOTR, SQRKU, GROTOL, ROOTI, EPS                      REA09990
      DATA        EPS/Z3410000000000000/                                REA10000
C                                                                       REA10010
      CALL XUFLOW(0)                                                    REA10020
      IERR=0                                                            REA10030
      KU=0                                                              REA10040
      L=0                                                               REA10050
      IP=0                                                              REA10060
C IP IS THE DESCRIPTOR OF THE CURRENT ROOT:                             REA10070
C 0 FOR A  REAL ROOT, AND 1 OR -1  FOR THE 1ST OR 2ND OF A COMPLEX PAIR REA10080
         DO 1080 K=1,N                                                  REA10090
         IF (WI(K).EQ.0D0) GO TO 100                                    REA10100
         IF(IP.EQ.0) IP=-1                                              REA10110
         IP=-IP                                                         REA10120
         IF (.NOT.WANTED(K)) GO TO 1080                                 REA10130
         J=L+2                                                          REA10140
         IF (IP.GT.0) WANTED(K+1)=.FALSE.                               REA10150
         GO TO 120                                                      REA10160
100      IP=0                                                           REA10170
         IF (.NOT.WANTED(K)) GO TO 1080                                 REA10180
         J=L+1                                                          REA10190
120      IF (J.GT.MZ) GO TO 9999                                        REA10200
         L=J                                                            REA10210
         IF (KU.GE.K) GO TO 220                                         REA10220
C SEARCH FOR SPLITTING                                                  REA10230
            DO 140 KU=K,N-1                                             REA10240
            IF (A(KU+1,KU).EQ.0D0) GO TO 160                            REA10250
140         CONTINUE                                                    REA10260
         KU=N                                                           REA10270
C L1 NORM OF THE LEADING HESSENBERG MATRIX OF ORDER KU                  REA10280
160      ANORM=0D0                                                      REA10290
         X=DABS(A(1,1))                                                 REA10300
            DO 200 J=2,KU                                               REA10310
            ANORM=DMAX1(X+DABS(A(J,J-1)),ANORM)                         REA10320
            X=0D0                                                       REA10330
               DO 180 I=1,J                                             REA10340
180            X=X+DABS(A(I,J))                                         REA10350
200         CONTINUE                                                    REA10360
         ANORM=DMAX1(X,ANORM)                                           REA10370
C EPS3 REPLACES ZERO PIVOTS AND PERTURBS CLUSTERED ROOTS                REA10380
         IF (ANORM.EQ.0D0) ANORM=1D0                                    REA10390
         EPS3=EPS*ANORM                                                 REA10400
C GROTOL IS THE TEST VALUE FOR GROWTH                                   REA10410
         SQRKU=DSQRT(DFLOAT(KU))                                        REA10420
         EPS4=EPS3/(1D0+SQRKU)                                          REA10430
         GROTOL=0.1D0/SQRKU                                             REA10440
C CURRENT ROOT                                                          REA10450
220      ROOTR=WR(K)                                                    REA10460
         ROOTI=WI(K)                                                    REA10470
         IF (K.EQ.1) GO TO 300                                          REA10480
         GO TO 260                                                      REA10490
C PERTURBATION OF THE ROOT IF IT IS IN A CLUSTER                        REA10500
240      ROOTR=ROOTR+EPS3                                               REA10510
260         DO 280 I=K-1,1,-1                                           REA10520
            IF (WANTED(I) .AND. DABS(WR(I)-ROOTR).LT.EPS3               REA10530
     *                    .AND. DABS(WI(I)-ROOTI).LT.EPS3) GO TO 240    REA10540
280         CONTINUE                                                    REA10550
         WR(K)=ROOTR                                                    REA10560
         WR(IP+K)=ROOTR                                                 REA10570
300      IT=1                                                           REA10580
         IF(ROOTI.NE.0D0) GO TO 620                                     REA10590
C REAL ROOT                                                             REA10600
C   LR DECOMPOSITION OF THE SHIFTED MATRIX                              REA10610
C      INITIAL RUNNING ROW OF R AND RIGHT-HAND SIDE                     REA10620
            DO 320 I=1,KU                                               REA10630
            U(I)=A(1,I)                                                 REA10640
320         Z(I,L)=EPS3                                                 REA10650
         U(1)=U(1)-ROOTR                                                REA10660
C      R FACTOR OF THE DECOMPOSITION: DIAGONAL     IN U                 REA10670
C                                     UPPER PART   IN B (PACKED)        REA10680
            DO 400 I=1,KU-1                                             REA10690
            II=KU*(I-1)-(I*I+I)/2                                       REA10700
            T=A(I+1,I)                                                  REA10710
            IF(DABS(U(I)).LT.DABS(T)) GO TO 360                         REA10720
C         NO INTERCHANGE                                                REA10730
               IF(U(I).EQ.0D0) U(I)=EPS3                                REA10740
               X=-T/U(I)                                                REA10750
                  DO 340 J=I+1,KU                                       REA10760
                  B(II+J)=U(J)                                          REA10770
340               U(J)=A(I+1,J)+X*U(J)                                  REA10780
               U(I+1)=U(I+1)-ROOTR                                      REA10790
               GO TO 400                                                REA10800
C         INTERCHANGE OF ROWS I AND I+1                                 REA10810
360         X=-U(I)/T                                                   REA10820
            U(I)=T                                                      REA10830
            T=A(I+1,I+1)-ROOTR                                          REA10840
            B(II+I+1)=T                                                 REA10850
            U(I+1)=U(I+1)+X*T                                           REA10860
               DO 380 J=I+2,KU                                          REA10870
               T=A(I+1,J)                                               REA10880
               B(II+J)=T                                                REA10890
380            U(J)=U(J)+X*T                                            REA10900
400         CONTINUE                                                    REA10910
         IF(U(KU).EQ.0D0) U(KU)=EPS3                                    REA10920
C      BACKWARD SUBSTITUTION                                            REA10930
420      Z(KU,L)=Z(KU,L)/U(KU)                                          REA10940
            DO 460 I=KU-1,1,-1                                          REA10950
            II=KU*(I-1)-(I*I+I)/2                                       REA10960
            X=Z(I,L)                                                    REA10970
               DO 440 J=I+1,KU                                          REA10980
440            X=X-B(II+J)*Z(J,L)                                       REA10990
            Z(I,L)=X/U(I)                                               REA11000
460         CONTINUE                                                    REA11010
C      ACCEPTANCE TEST                                                  REA11020
         ANORM=0D0                                                      REA11030
         X=0D0                                                          REA11040
            DO 480 I=1,KU                                               REA11050
            S=DABS(Z(I,L))                                              REA11060
            ANORM=ANORM+S                                               REA11070
            IF (X.GE.S) GO TO 480                                       REA11080
               J=I                                                      REA11090
               X=S                                                      REA11100
480         CONTINUE                                                    REA11110
         IF (ANORM.LT.GROTOL) GO TO 540                                 REA11120
C      ACCEPTED VECTOR                                                  REA11130
         X=1D0/Z(J,L)                                                   REA11140
            DO 500 I=1,KU                                               REA11150
500         Z(I,L)=X*Z(I,L)                                             REA11160
            DO 520 I=KU+1,N                                             REA11170
520         Z(I,L)=0D0                                                  REA11180
         GO TO 1080                                                     REA11190
C      NEW STARTING VECTOR                                              REA11200
540      IF(IT.GE.KU) GO TO 580                                         REA11210
         IT=IT+1                                                        REA11220
         Z(1,L)=EPS3                                                    REA11230
            DO 560 I=2,KU                                               REA11240
560         Z(I,L)=EPS4                                                 REA11250
         Z(IT,L)=EPS4-SQRKU*EPS3                                        REA11260
         GO TO 420                                                      REA11270
C      REJECTED VECTOR                                                  REA11280
580         IERR=-K                                                     REA11290
               DO 600 I=1,N                                             REA11300
600            Z(I,L)=0D0                                               REA11310
            GO TO 1080                                                  REA11320
C COMPLEX ROOT                                                          REA11330
C   LR DECOMPOSITION OF THE SHIFTED MATRIX                              REA11340
C      INITIAL RUNNING ROW OF R (COMPLEX) AND RIGHT-HAND SIDE           REA11350
620      KU2=KU+KU                                                      REA11360
            DO 640 I=1,KU                                               REA11370
            U(I)=A(1,I)                                                 REA11380
            V(I)=0D0                                                    REA11390
640         Z(I,L-1)=EPS3                                               REA11400
         U(1)=U(1)-ROOTR                                                REA11410
         V(1)=-ROOTI                                                    REA11420
C      R FACTOR OF THE DECOMPOSITION: DIAGONAL     IN U, V              REA11430
C                                     UPPER PART   IN B (PACKED)        REA11440
            DO 760 I=1,KU-1                                             REA11450
            II=KU2*(I-1)-I*I-I                                          REA11460
            T=A(I+1,I)                                                  REA11470
            S=DABS(U(I))+DABS(V(I))+DABS(T)                             REA11480
            IF(U(I)*(U(I)/S)+V(I)*(V(I)/S).LT.T*(T/S)) GO TO 720        REA11490
C         NO INTERCHANGE                                                REA11500
            IF(U(I).EQ.0D0 .AND. V(I).EQ.0D0) U(I)=EPS3                 REA11510
            IF(DABS(U(I)).LT.DABS(V(I))) GO TO 660                      REA11520
               S=V(I)/U(I)                                              REA11530
               X=-T/(U(I)+S*V(I))                                       REA11540
               Y=-S*X                                                   REA11550
               GO TO 680                                                REA11560
660         S=U(I)/V(I)                                                 REA11570
            Y=T/(V(I)+S*U(I))                                           REA11580
            X=-S*Y                                                      REA11590
680            DO 700 J=I+1,KU                                          REA11600
               B(II+2*J-1)=U(J)                                         REA11610
               B(II+2*J  )=V(J)                                         REA11620
               T=U(J)                                                   REA11630
               U(J)=A(I+1,J)+X*T-Y*V(J)                                 REA11640
700            V(J)=         Y*T+X*V(J)                                 REA11650
            U(I+1)=U(I+1)-ROOTR                                         REA11660
            V(I+1)=V(I+1)-ROOTI                                         REA11670
            GO TO 760                                                   REA11680
C         INTERCHANGE OF ROWS I AND I+1                                 REA11690
720         X=-U(I)/T                                                   REA11700
            Y=-V(I)/T                                                   REA11710
            U(I)=T                                                      REA11720
            V(I)=0D0                                                    REA11730
            T=A(I+1,I+1)-ROOTR                                          REA11740
            B(II+2*I+1)=T                                               REA11750
            B(II+2*I+2)=-ROOTI                                          REA11760
            U(I+1)=U(I+1)+X*T+Y*ROOTI                                   REA11770
            V(I+1)=V(I+1)-X*ROOTI+Y*T                                   REA11780
               DO 740 J=I+2,KU                                          REA11790
               T=A(I+1,J)                                               REA11800
               B(II+2*J-1)=T                                            REA11810
               B(II+2*J  )=0D0                                          REA11820
               U(J)=U(J)+X*T                                            REA11830
740            V(J)=V(J)+Y*T                                            REA11840
760         CONTINUE                                                    REA11850
         IF(U(KU).EQ.0D0 .AND. V(KU).EQ.0D0) U(KU)=EPS3                 REA11860
C      BACKWARD SUBSTITUTION                                            REA11870
780      X=U(KU)                                                        REA11880
         Y=V(KU)                                                        REA11890
         IF(DABS(X).LT.DABS(Y)) GO TO 800                               REA11900
            S=Y/X                                                       REA11910
            Z(KU,L-1)=Z(KU,L-1)/(X+S*Y)                                 REA11920
            Z(KU,L  )=-S*Z(KU,L-1)                                      REA11930
            GO TO 820                                                   REA11940
800      S=X/Y                                                          REA11950
         Z(KU,L  )=-Z(KU,L-1)/(Y+S*X)                                   REA11960
         Z(KU,L-1)=-S*Z(KU,L)                                           REA11970
820         DO 880 I=KU-1,1,-1                                          REA11980
            II=KU2*(I-1)-I*I-I                                          REA11990
            X=Z(I,L-1)                                                  REA12000
            Y=0D0                                                       REA12010
               DO 840 J=I+1,KU                                          REA12020
               X=X-B(II+2*J-1)*Z(J,L-1)+B(II+2*J)*Z(J,L)                REA12030
840            Y=Y-B(II+2*J-1)*Z(J,L  )-B(II+2*J)*Z(J,L-1)              REA12040
            IF(DABS(U(I)).LT.DABS(V(I))) GO TO 860                      REA12050
               S=V(I)/U(I)                                              REA12060
               T=U(I)+S*V(I)                                            REA12070
               Z(I,L-1)=(X+S*Y)/T                                       REA12080
               Z(I,L  )=(Y-S*X)/T                                       REA12090
               GO TO 880                                                REA12100
860         S=U(I)/V(I)                                                 REA12110
            T=V(I)+S*U(I)                                               REA12120
            Z(I,L-1)=(S*X+Y)/T                                          REA12130
            Z(I,L  )=(S*Y-X)/T                                          REA12140
880         CONTINUE                                                    REA12150
C      ACCEPTANCE TEST                                                  REA12160
         ANORM=0D0                                                      REA12170
         X=0D0                                                          REA12180
            DO 900 I=1,KU                                               REA12190
            S=CDABS(DCMPLX(Z(I,L-1),Z(I,L)))                            REA12200
            ANORM=ANORM+S                                               REA12210
            IF(S.LE.X) GO TO 900                                        REA12220
               J=I                                                      REA12230
               X=S                                                      REA12240
900         CONTINUE                                                    REA12250
         IF(ANORM.LT.GROTOL) GO TO 1000                                 REA12260
C      ACCEPTED VECTOR                                                  REA12270
         X=Z(J,L-1)                                                     REA12280
         Y=Z(J,L  )                                                     REA12290
         IF(DABS(X).LT.DABS(Y)) GO TO 920                               REA12300
            S=Y/X                                                       REA12310
            X=1D0/(X+S*Y)                                               REA12320
            Y=-S*X                                                      REA12330
            GO TO 940                                                   REA12340
920      S=X/Y                                                          REA12350
         Y=-1D0/(Y+S*X)                                                 REA12360
         X=-S*Y                                                         REA12370
940         DO 960 I=1,KU                                               REA12380
            T=Z(I,L-1)                                                  REA12390
            Z(I,L-1)=X*T-Y*Z(I,L)                                       REA12400
960         Z(I,L  )=Y*T+X*Z(I,L)                                       REA12410
            DO 980  I=KU+1,N                                            REA12420
            Z(I,L-1)=0D0                                                REA12430
980         Z(I,L  )=0D0                                                REA12440
         GO TO 1080                                                     REA12450
C      NEW STARTING VECTOR                                              REA12460
1000     IF(IT.GE.KU) GO TO 1040                                        REA12470
         IT=IT+1                                                        REA12480
         Z(1,L-1)=EPS3                                                  REA12490
            DO 1020 I=2,KU                                              REA12500
1020        Z(I,L-1)=EPS4                                               REA12510
         Z(IT,L-1)=EPS4-SQRKU*EPS3                                      REA12520
         GO TO 780                                                      REA12530
C      REJECTED VECTOR                                                  REA12540
1040     IERR=-K                                                        REA12550
            DO 1060 I=1,N                                               REA12560
            Z(I,L-1)=0D0                                                REA12570
1060        Z(I,L  )=0D0                                                REA12580
1080     CONTINUE                                                       REA12590
      GO TO 10000                                                       REA12600
C INSUFFICIENT STORAGE SPECIFIED FOR THE VECTORS                        REA12610
9999  IF (IERR.NE.0) IERR=IERR-N                                        REA12620
      IF (IERR.EQ.0) IERR=-(2*N+1)                                      REA12630
C                                                                       REA12640
10000 M=L                                                               REA12650
      RETURN                                                            REA12660
      END                                                               REA12670
