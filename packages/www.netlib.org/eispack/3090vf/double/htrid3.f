@PROCESS DIRECTIVE('"    (')                                            SYM05760
C-----------------------------------------------------------------------SYM05770
C REDUCTION OF A HERMITIAN MATRIX STORED IN A SINGLE TWO-DIMENSIONAL    SYM05780
C ARRAY TO REAL SYMMETRIC TRIDIAGONAL FORM.                             SYM05790
C                                                                       SYM05800
      SUBROUTINE  HTRID3 ( LD, N, A, D, E, E2, TAU )                    SYM05810
C                                                                       SYM05820
      INTEGER     LD, N                                                 SYM05830
      REAL*8      A(LD,N), D(N), E(N), E2(N), TAU(2,N)                  SYM05840
C                                                                       SYM05850
C LD     E  FIRST DIMENSION ASSIGNED TO THE TWO-DIMENSIONAL ARRAY       SYM05860
C           PARAMETERS IN THE CALLING PROGRAM.                          SYM05870
C                                                                       SYM05880
C N      E  ORDER OF THE MATRIX                                         SYM05890
C                                                                       SYM05900
C A      E  LOWER TRIANGULAR PORTION OF THE HERMITIAN MATRIX.           SYM05910
C           THE REAL PARTS OF THE MATRIX ELEMENTS ARE STORED IN THE     SYM05920
C           CORRESPONDING DIAGONAL AND SUBDIAGONAL ARRAY CELLS, AND     SYM05930
C           THE IMAGINARY PARTS IN SYMMETRIC POSITIONS WITH RESPECT     SYM05940
C           TO THE MAIN DIAGONAL (SUPERDIAGONAL CELLS).                 SYM05950
C        R  INFORMATION ABOUT THE REDUCING TRANSFORMATIONS.             SYM05960
C                                                                       SYM05970
C D      R  DIAGONAL ELEMENTS OF THE TRIDIAGONAL MATRIX.                SYM05980
C                                                                       SYM05990
C E      R  CODIAGONAL ELEMENTS OF THE TRIDIAGONAL MATRIX (E(1)=0).     SYM06000
C                                                                       SYM06010
C E2     R  SQUARES OF THE ELEMENTS OF E.                               SYM06020
C           E2 MAY COINCIDE WITH E IF THE SQUARES ARE NOT NEEDED.       SYM06030
C                                                                       SYM06040
C TAU    R  ADDITIONAL INFORMATION ABOUT THE REDUCING TRANSFORMATIONS.  SYM06050
C                                                                       SYM06060
C     THIS SUBROUTINE IS BASED ON A COMPLEX VARIANT OF THE ALGOL        SYM06070
C     PROCEDURE TRED3, NUM. MATH. 11, 181-195(1968) BY MARTIN, REINSCH, SYM06080
C     AND WILKINSON.                                                    SYM06090
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 212-226(1971).   SYM06100
C                                                                       SYM06110
C     VERSION FOR THE IBM 3090VF DATED NOVEMBER 1987.                   SYM06120
C     ------------------------------------------------------------------SYM06130
      INTEGER     I, J, K                                               SYM06140
      REAL*8      C, C2, S, T, S16, R16, X, Y, Z, TR, TI                SYM06150
C                                                                       SYM06160
      CALL XUFLOW(0)                                                    SYM06170
C INITIALIZATIONS                                                       SYM06180
         DO 100 J=2,N                                                   SYM06190
100      E(J)=A(1,J)                                                    SYM06200
      E2(1)=0D0                                                         SYM06210
      E(1) =0D0                                                         SYM06220
      TAU(1,1)=1D0                                                      SYM06230
      TAU(2,1)=0D0                                                      SYM06240
      IF(N.EQ.1) GO TO 1000                                             SYM06250
         DO 400 K=1,N-2                                                 SYM06260
C DIAGONAL ELEMENT                                                      SYM06270
         D(K)=A(K,K)                                                    SYM06280
C IMAGINARY PART OF THE SUBDIAGONAL PORTION OF COLUMN K                 SYM06290
            DO 110 I=K+1,N                                              SYM06300
110         D(I)=E(I)                                                   SYM06310
C RIGHT MULTIPLIER TO MAKE THE CODIAGONAL ELEMENT REAL                  SYM06320
         X=A(K+1,K)                                                     SYM06330
         Y=D(K+1)                                                       SYM06340
         S=DMAX1(DABS(X),DABS(Y))                                       SYM06350
         IF(S.NE.0D0) GO TO 120                                         SYM06360
            X=-1D0                                                      SYM06370
            Y=0D0                                                       SYM06380
            Z=0D0                                                       SYM06390
            GO TO 140                                                   SYM06400
120      Z=S*DSQRT((X/S)**2+(Y/S)**2)                                   SYM06410
         X=-X/Z                                                         SYM06420
         Y=-Y/Z                                                         SYM06430
140      TR=X*TAU(1,K)-Y*TAU(2,K)                                       SYM06440
         TI=Y*TAU(1,K)+X*TAU(2,K)                                       SYM06450
C SCALED CODIAGONAL ELEMENT AND ITS SQUARE                              SYM06460
         S=0D0                                                          SYM06470
            DO 160 I=K+2,N                                              SYM06480
160         S=S+DABS(A(I,K))+DABS(D(I))                                 SYM06490
         IF(S.NE.0D0) GO TO 180                                         SYM06500
C     SUBDIAGONAL PART OF COLUMN K IS ZERO                              SYM06510
            A(K+1,K)=0D0                                                SYM06520
            E2(K+1)=Z*Z                                                 SYM06530
            E(K+1) =Z                                                   SYM06540
            TAU(1,K+1)=-TR                                              SYM06550
            TAU(2,K+1)=-TI                                              SYM06560
               DO 170 J=K+1,N                                           SYM06570
170            E(J)=A(K+1,J)                                            SYM06580
            GO TO 400                                                   SYM06590
C     SUBDIAGONAL PART OF COLUMN K IS NOT ZERO                          SYM06600
180      S=S+DABS(A(K+1,K))+DABS(D(K+1))                                SYM06610
         CALL DSC16(S,S16,R16)                                          SYM06620
         Z=R16*Z                                                        SYM06630
         C2=0D0                                                         SYM06640
            DO 200 I=K+1,N                                              SYM06650
            S=R16*A(I,K)                                                SYM06660
            T=R16*D(I)                                                  SYM06670
            A(I,K)=S                                                    SYM06680
            D(I)  =T                                                    SYM06690
200         C2=C2+S*S+T*T                                               SYM06700
         C=DSQRT(C2)                                                    SYM06710
C HOUSEHOLDER VECTOR                                                    SYM06720
         X=X/C                                                          SYM06730
         Y=Y/C                                                          SYM06740
         A(K+1,K)=-1D0-Z/C                                              SYM06750
         D(K+1)  =0D0                                                   SYM06760
            DO 210 I=K+2,N                                              SYM06770
            S=A(I,K)                                                    SYM06780
            T=D(I)                                                      SYM06790
            A(I,K)=X*S+Y*T                                              SYM06800
210         D(I)  =X*T-Y*S                                              SYM06810
C HOUSEHOLDER VECTOR TRANSFORMED BY THE UNREDUCED SUBMATRIX             SYM06820
C"    ( PREFER VECTOR                                                   SYM06830
            DO 230 I=K+1,N                                              SYM06840
            TAU(1,I)=0D0                                                SYM06850
230         TAU(2,I)=0D0                                                SYM06860
            DO 280 J=K+1,N                                              SYM06870
            X=0D0                                                       SYM06880
            Y=0D0                                                       SYM06890
C           (IMAGINARY PART OF THE K-TH HOUSEHOLDER VECTOR STORED)      SYM06900
            A(K,J)=D(J)                                                 SYM06910
               DO 240 I=K+1,J-1                                         SYM06920
               T=A(I,J)                                                 SYM06930
               X=X-D(I)*T                                               SYM06940
               Y=Y+A(I,K)*T                                             SYM06950
               TAU(1,I)=TAU(1,I)+D(J)*T                                 SYM06960
240            TAU(2,I)=TAU(2,I)-A(J,K)*T                               SYM06970
            X=X+A(J,J)*A(J,K)                                           SYM06980
            Y=Y+A(J,J)*D(J)                                             SYM06990
C"    ( PREFER VECTOR                                                   SYM07000
               DO 260 I=J+1,N                                           SYM07010
               T=A(I,J)                                                 SYM07020
               X=X+A(I,K)*T                                             SYM07030
               Y=Y+D(I)*T                                               SYM07040
               TAU(1,I)=TAU(1,I)+A(J,K)*T                               SYM07050
260            TAU(2,I)=TAU(2,I)+D(J)*T                                 SYM07060
            TAU(1,J)=TAU(1,J)+X                                         SYM07070
280         TAU(2,J)=TAU(2,J)+Y                                         SYM07080
C COMPOSITE VECTOR OF THE RANK-2 MODIFICATION                           SYM07090
         Z=1D0/A(K+1,K)                                                 SYM07100
         S=0D0                                                          SYM07110
            DO 300 I=K+1,N                                              SYM07120
300         S=S+A(I,K)*TAU(1,I)+D(I)*TAU(2,I)                           SYM07130
         S=0.5D0*S*Z                                                    SYM07140
C"    ( PREFER VECTOR                                                   SYM07150
            DO 320 I=K+1,N                                              SYM07160
            TAU(1,I)=Z*(TAU(1,I)+S*A(I,K))                              SYM07170
320         TAU(2,I)=Z*(TAU(2,I)+S*D(I))                                SYM07180
C TRANSFORMATION OF THE UNREDUCED SUBMATRIX                             SYM07190
            DO 380 J=K+1,N                                              SYM07200
C        IMAGINARY PARTS                                                SYM07210
C"    ( IGNORE RECRDEPS                                                 SYM07220
               DO 340 I=K+1,J-1                                         SYM07230
               A(I,J)=A(I,J)+TAU(1,I)*D(J)-TAU(2,I)*A(J,K)              SYM07240
     *                      -TAU(1,J)*D(I)+TAU(2,J)*A(I,K)              SYM07250
340            CONTINUE                                                 SYM07260
C           (NEEDED IMAGINARY PARTS FETCHED FOR NEXT STEP)              SYM07270
            E(J)=A(K+1,J)                                               SYM07280
C        REAL PARTS                                                     SYM07290
C"    ( IGNORE RECRDEPS                                                 SYM07300
               DO 360 I=J  ,N                                           SYM07310
360            A(I,J)=A(I,J)+A(I,K)*TAU(1,J)+D(I)*TAU(2,J)              SYM07320
     *                      +A(J,K)*TAU(1,I)+D(J)*TAU(2,I)              SYM07330
380         CONTINUE                                                    SYM07340
C CODIAGONAL ELEMENT AND ITS SQUARE                                     SYM07350
         E2(K+1)=S16*C2*S16                                             SYM07360
         E(K+1)=S16*C                                                   SYM07370
C ELEMENT OF THE TRANSFORMATION TO REAL TRIDIAGONAL FORM                SYM07380
         TAU(1,K+1)=TR                                                  SYM07390
         TAU(2,K+1)=TI                                                  SYM07400
400      CONTINUE                                                       SYM07410
C LAST ELEMENTS OF THE TRIDIAGONAL MATRIX                               SYM07420
      D(N)=A(N,N)                                                       SYM07430
      D(N-1)=A(N-1,N-1)                                                 SYM07440
      X=A(N,N-1)                                                        SYM07450
      Y=A(N-1,N)                                                        SYM07460
      Z=DMAX1(DABS(X),DABS(Y))                                          SYM07470
      IF(Z.NE.0D0) GO TO 420                                            SYM07480
         X=1D0                                                          SYM07490
         Y=0D0                                                          SYM07500
         C=0D0                                                          SYM07510
         GO TO 440                                                      SYM07520
420   C=Z*DSQRT((X/Z)**2+(Y/Z)**2)                                      SYM07530
      X=X/C                                                             SYM07540
      Y=Y/C                                                             SYM07550
440   TAU(1,N)=X*TAU(1,N-1)-Y*TAU(2,N-1)                                SYM07560
      TAU(2,N)=X*TAU(2,N-1)+Y*TAU(1,N-1)                                SYM07570
      E2(N)=C*C                                                         SYM07580
      E(N)=C                                                            SYM07590
1000  RETURN                                                            SYM07600
      END                                                               SYM07610
