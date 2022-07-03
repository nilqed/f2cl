@PROCESS DIRECTIVE('"    (')                                            SYM07620
C-----------------------------------------------------------------------SYM07630
C REDUCTION OF A HERMITIAN MATRIX TO REAL SYMMETRIC TRIDIAGONAL FORM    SYM07640
C                                                                       SYM07650
      SUBROUTINE  HTRIDI ( LD, N, AR, AI, D, E, E2, TAU )               SYM07660
C                                                                       SYM07670
      INTEGER     LD, N                                                 SYM07680
      REAL*8      AR(LD,N), AI(LD,N), D(N), E(N), E2(N), TAU(2,N)       SYM07690
C                                                                       SYM07700
C LD     E  FIRST DIMENSION ASSIGNED TO THE TWO-DIMENSIONAL ARRAY       SYM07710
C           PARAMETERS IN THE CALLING PROGRAM.                          SYM07720
C                                                                       SYM07730
C N      E  ORDER OF THE MATRIX.                                        SYM07740
C                                                                       SYM07750
C AR,AI  E  REAL AND IMAGINARY PARTS OF THE HERMITIAN MATRIX (ONLY THE  SYM07760
C           LOWER-TRIANGULAR PART OF THE MATRIX IS USED).               SYM07770
C        R  INFORMATION ABOUT THE REDUCING TRANSFORMATIONS.  THE FULL   SYM07780
C           UPPER-TRIANGULAR PARTS OF THE ARRAYS ARE PRESERVED.         SYM07790
C                                                                       SYM07800
C D      R  DIAGONAL ELEMENTS OF THE TRIDIAGONAL MATRIX.                SYM07810
C                                                                       SYM07820
C E      R  CODIAGONAL ELEMENTS OF THE TRIDIAGONAL MATRIX (E(1)=0).     SYM07830
C                                                                       SYM07840
C E2     R  SQUARES OF THE ELEMENTS OF E.                               SYM07850
C           E2 MAY COINCIDE WITH E IF THE SQUARES ARE NOT NEEDED.       SYM07860
C                                                                       SYM07870
C TAU    R  ADDITIONAL INFORMATION ABOUT THE TRANSFORMATIONS.           SYM07880
C                                                                       SYM07890
C     THIS SUBROUTINE IS BASED ON A COMPLEX VARIANT OF THE ALGOL        SYM07900
C     PROCEDURE TRED1, NUM. MATH. 11, 181-195(1968) BY MARTIN, REINSCH, SYM07910
C     AND WILKINSON.                                                    SYM07920
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 212-226(1971).   SYM07930
C                                                                       SYM07940
C SUBPROGRAMS CALLED:  DSC16                                            SYM07950
C                                                                       SYM07960
C     VERSION FOR THE IBM 3090VF DATED NOVEMBER 1987.                   SYM07970
C     ------------------------------------------------------------------SYM07980
      INTEGER     I, J, K                                               SYM07990
      REAL*8      C, C2, S, T, S16, R16, X, Y, Z, TR, TI                SYM08000
C                                                                       SYM08010
      CALL XUFLOW(0)                                                    SYM08020
C PRESERVATION OF THE DIAGONAL ELEMENTS                                 SYM08030
         DO 100 I=1,N                                                   SYM08040
100      D(I)=AR(I,I)                                                   SYM08050
C INITIALIZATIONS                                                       SYM08060
      E2(1)=0D0                                                         SYM08070
      E(1) =0D0                                                         SYM08080
      TAU(1,1)=1D0                                                      SYM08090
      TAU(2,1)=0D0                                                      SYM08100
      IF(N.EQ.1) GO TO 1000                                             SYM08110
         DO 400 K=1,N-2                                                 SYM08120
C DIAGONAL ELEMENT                                                      SYM08130
         X=D(K)                                                         SYM08140
         D(K)=AR(K,K)                                                   SYM08150
         AR(K,K)=X                                                      SYM08160
C RIGHT MULTIPLIER TO MAKE THE CODIAGONAL ELEMENT REAL                  SYM08170
         X=AR(K+1,K)                                                    SYM08180
         Y=AI(K+1,K)                                                    SYM08190
         S=DMAX1(DABS(X),DABS(Y))                                       SYM08200
         IF(S.NE.0D0) GO TO 120                                         SYM08210
            X=-1D0                                                      SYM08220
            Y=0D0                                                       SYM08230
            Z=0D0                                                       SYM08240
            GO TO 140                                                   SYM08250
120      Z=S*DSQRT((X/S)**2+(Y/S)**2)                                   SYM08260
         X=-X/Z                                                         SYM08270
         Y=-Y/Z                                                         SYM08280
140      TR=X*TAU(1,K)-Y*TAU(2,K)                                       SYM08290
         TI=Y*TAU(1,K)+X*TAU(2,K)                                       SYM08300
C MODULUS OF THE CODIAGONAL ELEMENT AND ITS SQUARE                      SYM08310
         S=0D0                                                          SYM08320
            DO 160 I=K+2,N                                              SYM08330
160         S=S+DABS(AR(I,K))+DABS(AI(I,K))                             SYM08340
         IF(S.NE.0D0) GO TO 180                                         SYM08350
            AR(K+1,K)=0D0                                               SYM08360
            E2(K+1)=Z*Z                                                 SYM08370
            E(K+1) =Z                                                   SYM08380
            TAU(1,K+1)=-TR                                              SYM08390
            TAU(2,K+1)=-TI                                              SYM08400
            GO TO 400                                                   SYM08410
180      S=S+DABS(AR(K+1,K))+DABS(AI(K+1,K))                            SYM08420
         CALL DSC16(S,S16,R16)                                          SYM08430
         Z=R16*Z                                                        SYM08440
         C2=0D0                                                         SYM08450
            DO 200 I=K+1,N                                              SYM08460
            S=R16*AR(I,K)                                               SYM08470
            T=R16*AI(I,K)                                               SYM08480
            AR(I,K)=S                                                   SYM08490
            AI(I,K)=T                                                   SYM08500
200         C2=C2+S*S+T*T                                               SYM08510
         E2(K+1)=S16*C2*S16                                             SYM08520
         C=DSQRT(C2)                                                    SYM08530
         E(K+1)=S16*C                                                   SYM08540
C SCALED HOUSEHOLDER VECTOR                                             SYM08550
         X=X/C                                                          SYM08560
         Y=Y/C                                                          SYM08570
         AR(K+1,K)=-1D0-Z/C                                             SYM08580
         AI(K+1,K)=0D0                                                  SYM08590
            DO 220 I=K+2,N                                              SYM08600
            S=AR(I,K)                                                   SYM08610
            T=AI(I,K)                                                   SYM08620
            AR(I,K)=X*S+Y*T                                             SYM08630
220         AI(I,K)=X*T-Y*S                                             SYM08640
C HOUSEHOLDER VECTOR TRANSFORMED BY THE UNREDUCED SUBMATRIX             SYM08650
         TAU(1,N)=AR(N,N)*AR(N,K)                                       SYM08660
         TAU(2,N)=AR(N,N)*AI(N,K)                                       SYM08670
            DO 280 J=N-1,K+1,-1                                         SYM08680
            X=AR(J,J)*AR(J,K)                                           SYM08690
            Y=AR(J,J)*AI(J,K)                                           SYM08700
               DO 240 I=J+1,N                                           SYM08710
               X=X+AR(I,J)*AR(I,K)+AI(I,J)*AI(I,K)                      SYM08720
               Y=Y-AI(I,J)*AR(I,K)+AR(I,J)*AI(I,K)                      SYM08730
               TAU(1,I)=TAU(1,I)+AR(J,K)*AR(I,J)-AI(J,K)*AI(I,J)        SYM08740
240            TAU(2,I)=TAU(2,I)+AR(J,K)*AI(I,J)+AI(J,K)*AR(I,J)        SYM08750
            TAU(1,J)=X                                                  SYM08760
280         TAU(2,J)=Y                                                  SYM08770
C COMPOSITE VECTOR OF THE RANK-2 MODIFICATION                           SYM08780
         Z=1D0/AR(K+1,K)                                                SYM08790
         S=0D0                                                          SYM08800
            DO 300 I=K+1,N                                              SYM08810
300         S=S+AR(I,K)*TAU(1,I)+AI(I,K)*TAU(2,I)                       SYM08820
         S=0.5D0*S*Z                                                    SYM08830
C"    ( PREFER VECTOR                                                   SYM08840
            DO 320 I=K+1,N                                              SYM08850
            TAU(1,I)=Z*(TAU(1,I)+S*AR(I,K))                             SYM08860
320         TAU(2,I)=Z*(TAU(2,I)+S*AI(I,K))                             SYM08870
C TRANSFORMATION OF THE UNREDUCED SUBMATRIX                             SYM08880
            DO 360 J=K+1,N                                              SYM08890
C"    ( IGNORE RECRDEPS                                                 SYM08900
               DO 340 I=J,N                                             SYM08910
               AR(I,J)=AR(I,J)+AR(I,K)*TAU(1,J)+AI(I,K)*TAU(2,J)        SYM08920
     *                        +TAU(1,I)*AR(J,K)+TAU(2,I)*AI(J,K)        SYM08930
               AI(I,J)=AI(I,J)+AI(I,K)*TAU(1,J)-AR(I,K)*TAU(2,J)        SYM08940
     *                        +TAU(2,I)*AR(J,K)-TAU(1,I)*AI(J,K)        SYM08950
340            CONTINUE                                                 SYM08960
360         CONTINUE                                                    SYM08970
         TAU(1,K+1)=TR                                                  SYM08980
         TAU(2,K+1)=TI                                                  SYM08990
400      CONTINUE                                                       SYM09000
C LAST ELEMENTS OF THE TRIDIAGONAL MATRIX                               SYM09010
      X=D(N)                                                            SYM09020
      D(N)=AR(N,N)                                                      SYM09030
      AR(N,N)=X                                                         SYM09040
      X=D(N-1)                                                          SYM09050
      D(N-1)=AR(N-1,N-1)                                                SYM09060
      AR(N-1,N-1)=X                                                     SYM09070
      X=AR(N,N-1)                                                       SYM09080
      Y=AI(N,N-1)                                                       SYM09090
      Z=DMAX1(DABS(X),DABS(Y))                                          SYM09100
      IF(Z.NE.0D0) GO TO 420                                            SYM09110
         X=1D0                                                          SYM09120
         Y=0D0                                                          SYM09130
         C=0D0                                                          SYM09140
         GO TO 440                                                      SYM09150
420   C=Z*DSQRT((X/Z)**2+(Y/Z)**2)                                      SYM09160
      X=X/C                                                             SYM09170
      Y=Y/C                                                             SYM09180
440   TAU(1,N)=X*TAU(1,N-1)-Y*TAU(2,N-1)                                SYM09190
      TAU(2,N)=X*TAU(2,N-1)+Y*TAU(1,N-1)                                SYM09200
      E2(N)=C*C                                                         SYM09210
      E(N)=C                                                            SYM09220
1000  RETURN                                                            SYM09230
      END                                                               SYM09240
