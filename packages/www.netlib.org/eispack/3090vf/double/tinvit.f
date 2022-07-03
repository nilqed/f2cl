C-----------------------------------------------------------------------BIS05070
C EIGENVECTORS OF A SYMMETRIC TRIDIAGONAL MATRIX ASSOCIATED WITH A GIVENBIS05080
C SET OF EIGENVALUES.                                                   BIS05090
C                                                                       BIS05100
      SUBROUTINE  TINVIT (LD,N,D,E,E2,M,W,IND,Z,IERR,V1,V2,V3,V4,V5)    BIS05110
C                                                                       BIS05120
      INTEGER     IND(N), M, N, LD, IERR                                BIS05130
      REAL*8      D(N), E(N), E2(N), W(M), Z(LD,M)                      BIS05140
      REAL*8      V1(N), V2(N), V3(N), V4(N), V5(N)                     BIS05150
C                                                                       BIS05160
C LD     E  FIRST DIMENSION ASSIGNED TO THE TWO-DIMENSIONAL ARRAY       BIS05170
C           PARAMETERS IN THE CALLING PROGRAM.                          BIS05180
C                                                                       BIS05190
C N      E  ORDER OF THE MATRIX                                         BIS05200
C                                                                       BIS05210
C D      E  DIAGONAL ELEMENTS OF THE TRIDIAGONAL MATRIX.                BIS05220
C                                                                       BIS05230
C E      E  SUBDIAGONAL ELEMENTS OF THE TRIDIAGONAL MATRIX IN CELLS     BIS05240
C           2,...,N.                                                    BIS05250
C                                                                       BIS05260
C E2     E  SQUARES OF THE CORRESPONDING ELEMENTS OF E WITH ZEROS       BIS05270
C           CORRESPONDING TO NEGLIGIBLE ELEMENTS OF E, AS SET BY        BIS05280
C           SUBROUTINES  BISECT,  TRIBID,  OR IMTQLV.                   BIS05290
C           E(I) IS CONSIDERED NEGLIGIBLE IF ITS ABSOLUTE VALUE IS NOT  BIS05300
C           LARGER THAN THE PRODUCT OF THE MACHINE PRECISION AND THE    BIS05310
C           SUM OF THE MAGNITUDES OF D(I) AND D(I-1).                   BIS05320
C           E2(1) MUST CONTAIN  0.0  IF THE EIGENVALUES ARE IN ASCENDINGBIS05330
C           ORDER, OR  2.0  IF THE EIGENVALUES ARE IN DESCENDING ORDER. BIS05340
C                                                                       BIS05350
C M      E  NUMBER OF SPECIFIED EIGENVALUES.                            BIS05360
C                                                                       BIS05370
C W      E  EIGENVALUES IN ASCENDING OR DESCENDING ORDER.               BIS05380
C                                                                       BIS05390
C IND    E  INDICES OF THE PRINCIPAL SUBMATRICES (NUMBERED FROM TOP TO  BIS05400
C           BOTTOM) ASSOCIATED WITH THE CORRESPONDING EIGENVALUES IN W. BIS05410
C                                                                       BIS05420
C Z      R  SET OF ORTHONORMAL EIGENVECTORS.                            BIS05430
C           ANY VECTOR FOR WHICH THE ITERATION FAILS TO CONVERGE IS     BIS05440
C           SET TO ZERO.                                                BIS05450
C                                                                       BIS05460
C IERR   R  ZERO     FOR NORMAL RETURN,                                 BIS05470
C           -R       IF THE EIGENVECTOR CORRESPONDING TO THE R-TH       BIS05480
C                    EIGENVALUE FAILS TO CONVERGE IN 5 ITERATIONS.      BIS05490
C                                                                       BIS05500
C V1,...,V5    ANCILLARY STORAGE.                                       BIS05510
C                                                                       BIS05520
C     THIS SUBROUTINE IS BASED ON THE INVERSE ITERATION TECHNIQUE IN    BIS05530
C     THE ALGOL PROCEDURE TRISTURM BY PETERS AND WILKINSON.             BIS05540
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 418-439(1971).   BIS05550
C                                                                       BIS05560
C     VERSION FOR THE IBM 3090VF DATED NOVEMBER 1987.                   BIS05570
C     ------------------------------------------------------------------BIS05580
      INTEGER     I, J, K, IP, IQ, IR, IS, ITS, ITAG, IGROUP            BIS05590
      REAL*8      U, V, UK, XU, X0, X1, EPS2, EPS3, EPS4                BIS05600
      REAL*8      ANORM, ORDER, EPS                                     BIS05610
      DATA        EPS/Z3410000000000000/                                BIS05620
C                                                                       BIS05630
      CALL XUFLOW(0)                                                    BIS05640
      IERR=0                                                            BIS05650
      IF (M .EQ. 0) GO TO 1000                                          BIS05660
      ITAG=0                                                            BIS05670
      ORDER=1D0-E2(1)                                                   BIS05680
      IQ=0                                                              BIS05690
C ISOLATION OF A SUBMATRIX                                              BIS05700
100   IP=IQ+1                                                           BIS05710
         DO 120 IQ=IP,N                                                 BIS05720
         IF (IQ .EQ. N) GO TO 140                                       BIS05730
         IF (E2(IQ+1) .EQ. 0D0) GO TO 140                               BIS05740
120      CONTINUE                                                       BIS05750
C INVERSE ITERATION                                                     BIS05760
140   ITAG=ITAG+1                                                       BIS05770
      IS=0                                                              BIS05780
         DO 920 IR=1,M                                                  BIS05790
         IF (IND(IR) .NE. ITAG) GO TO 920                               BIS05800
         ITS=1                                                          BIS05810
         X1=W(IR)                                                       BIS05820
         IF (IS .NE. 0) GO TO 510                                       BIS05830
C CHECK FOR ISOLATED ROOT                                               BIS05840
         XU=1D0                                                         BIS05850
         IF (IP .NE. IQ) GO TO 490                                      BIS05860
         V5(IP)=1D0                                                     BIS05870
         GO TO 870                                                      BIS05880
490      ANORM=DABS(D(IP))                                              BIS05890
            DO 500 I=IP+1,IQ                                            BIS05900
            ANORM=DMAX1(ANORM,DABS(D(I))+DABS(E(I)))                    BIS05910
500         CONTINUE                                                    BIS05920
C EPS2 IS THE TEST VALUE FOR GROUPING,                                  BIS05930
C EPS3 REPLACES ZERO PIVOTS AND PERTURBS EQUAL ROOTS, AND               BIS05940
C EPS4 (RIGHT-HAND SIDE) IS TAKEN VERY SMALL TO AVOID OVERFLOW.         BIS05950
         EPS2=1D-3*ANORM                                                BIS05960
         EPS3=EPS*ANORM                                                 BIS05970
         UK=IQ-IP+1                                                     BIS05980
         EPS4=UK*EPS3                                                   BIS05990
         UK=EPS4/DSQRT(UK)                                              BIS06000
         IS=IP                                                          BIS06010
505      IGROUP=0                                                       BIS06020
         GO TO 520                                                      BIS06030
C SEPARATION OF CLUSTERED ROOTS                                         BIS06040
510      IF (DABS(X1-X0) .GE. EPS2) GO TO 505                           BIS06050
         IGROUP=IGROUP+1                                                BIS06060
         IF (ORDER*(X1-X0) .LE. 0D0) X1=X0+ORDER*EPS3                   BIS06070
C MATRIX FACTORIZATION WITH INTERCHANGES, AND INITIAL RIGHT-HAND SIDE   BIS06080
520      V=0D0                                                          BIS06090
            DO 580 I=IP,IQ                                              BIS06100
            V5(I)=UK                                                    BIS06110
            IF (I .EQ. IP) GO TO 560                                    BIS06120
            IF (DABS(E(I)) .LT. DABS(U)) GO TO 540                      BIS06130
C WARNING: A DIVISION BY ZERO MAY OCCUR HERE IF E2 WAS NOT SPECIFIED    BIS06140
C CORRECTLY                                                             BIS06150
            XU=U/E(I)                                                   BIS06160
            V4(I)=XU                                                    BIS06170
            V1(I-1)=E(I)                                                BIS06180
            V2(I-1)=D(I)-X1                                             BIS06190
            V3(I-1)=0D0                                                 BIS06200
            IF (I .NE. IQ) V3(I-1)=E(I+1)                               BIS06210
            U=V-XU*V2(I-1)                                              BIS06220
            V=-XU*V3(I-1)                                               BIS06230
            GO TO 580                                                   BIS06240
540         XU=E(I)/U                                                   BIS06250
            V4(I)=XU                                                    BIS06260
            V1(I-1)=U                                                   BIS06270
            V2(I-1)=V                                                   BIS06280
            V3(I-1)=0D0                                                 BIS06290
560         U=D(I)-X1-XU*V                                              BIS06300
            IF (I .NE. IQ) V=E(I+1)                                     BIS06310
580         CONTINUE                                                    BIS06320
         IF (U .EQ. 0D0) U=EPS3                                         BIS06330
         V1(IQ)=U                                                       BIS06340
         V2(IQ)=0D0                                                     BIS06350
         V3(IQ)=0D0                                                     BIS06360
C BACKWARD SUBSTITUTION                                                 BIS06370
600         DO 620 I=IQ,IP,-1                                           BIS06380
            V5(I)=(V5(I)-U*V2(I)-V*V3(I))/V1(I)                         BIS06390
            V=U                                                         BIS06400
            U=V5(I)                                                     BIS06410
620         CONTINUE                                                    BIS06420
C ORTHOGONALIZATION AGAINST PREVIOUS MEMBERS OF THE GROUP               BIS06430
         IF (IGROUP .EQ. 0) GO TO 700                                   BIS06440
         J=IR                                                           BIS06450
            DO 680 K=1,IGROUP                                           BIS06460
630         J=J-1                                                       BIS06470
            IF (IND(J) .NE. ITAG) GO TO 630                             BIS06480
            XU=0D0                                                      BIS06490
               DO 640 I=IP,IQ                                           BIS06500
640            XU=XU+V5(I)*Z(I,J)                                       BIS06510
               DO 660 I=IP,IQ                                           BIS06520
660            V5(I)=V5(I)-XU*Z(I,J)                                    BIS06530
680         CONTINUE                                                    BIS06540
700      ANORM=0D0                                                      BIS06550
            DO 720 I=IP,IQ                                              BIS06560
720         ANORM=ANORM+DABS(V5(I))                                     BIS06570
         IF (ANORM .GE. 0.5D0) GO TO 840                                BIS06580
C NEXT ITERATE                                                          BIS06590
         IF (ITS .EQ. 5) GO TO 830                                      BIS06600
         IF (ANORM .NE. 0D0) GO TO 740                                  BIS06610
         V5(IS)=EPS4                                                    BIS06620
         IS=IS+1                                                        BIS06630
         IF (IS .GT. IQ) IS=IP                                          BIS06640
         GO TO 780                                                      BIS06650
740      XU=EPS4/ANORM                                                  BIS06660
            DO 760 I=IP,IQ                                              BIS06670
760         V5(I)=XU*V5(I)                                              BIS06680
C FORWARD SUBSTITUTION                                                  BIS06690
780         DO 820 I=IP+1,IQ                                            BIS06700
            U=V5(I)                                                     BIS06710
            IF (V1(I-1) .NE. E(I)) GO TO 800                            BIS06720
C ROW INTERCHANGE                                                       BIS06730
            U=V5(I-1)                                                   BIS06740
            V5(I-1)=V5(I)                                               BIS06750
800         V5(I)=U-V4(I)*V5(I-1)                                       BIS06760
820         CONTINUE                                                    BIS06770
         ITS=ITS+1                                                      BIS06780
         GO TO 600                                                      BIS06790
C ERROR: NO CONVERGENCE                                                 BIS06800
830      IERR=-IR                                                       BIS06810
         XU=0D0                                                         BIS06820
         GO TO 870                                                      BIS06830
C L2 NORMALIZATION                                                      BIS06840
840      U=0D0                                                          BIS06850
            DO 850 I=IP,IQ                                              BIS06860
850         U=U+DABS(V5(I))                                             BIS06870
         XU=0D0                                                         BIS06880
            DO 860 I=IP,IQ                                              BIS06890
860         XU=XU+(V5(I)/U)**2                                          BIS06900
         XU=1D0/(U*DSQRT(XU))                                           BIS06910
870         DO 880 I=1,N                                                BIS06920
880         Z(I,IR)=0D0                                                 BIS06930
         X0=X1                                                          BIS06940
            DO 900 I=IP,IQ                                              BIS06950
900         Z(I,IR)=XU*V5(I)                                            BIS06960
920      CONTINUE                                                       BIS06970
      IF (IQ .LT. N) GO TO 100                                          BIS06980
1000  RETURN                                                            BIS06990
      END                                                               BIS07000
