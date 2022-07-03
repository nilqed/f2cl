@PROCESS DIRECTIVE('"    (')                                            SYM21990
C-----------------------------------------------------------------------SYM22000
C REDUCTION OF A REAL SYMMETRIC MATRIX TO SYMMETRIC TRIDIAGONAL FORM BY SYM22010
C ORTHOGONAL SIMILARITY TRANSFORMATION.                                 SYM22020
C                                                                       SYM22030
      SUBROUTINE  TRED1(LD,N,A,D,E,E2)                                  SYM22040
C                                                                       SYM22050
      INTEGER     LD, N                                                 SYM22060
      REAL*8      A(LD,N), D(N), E(N), E2(N)                            SYM22070
C                                                                       SYM22080
C LD     E  FIRST DIMENSION ASSIGNED TO THE TWO-DIMENSIONAL ARRAY       SYM22090
C           PARAMETERS IN THE CALLING PROGRAM.                          SYM22100
C                                                                       SYM22110
C N      E  ORDER OF THE MATRIX.                                        SYM22120
C                                                                       SYM22130
C A      E  GIVEN SYMMETRIC MATRIX (ONLY ITS LOWER HALF IS USED).       SYM22140
C        R  INFORMATION ABOUT THE REDUCING TRANSFORMATIONS.  THE FULL   SYM22150
C           UPPER-TRIANGULAR PART OF THE ARRAY IS PRESERVED.            SYM22160
C                                                                       SYM22170
C D      R  DIAGONAL ELEMENTS OF THE TRIDIAGONAL MATRIX.                SYM22180
C                                                                       SYM22190
C E      R  SUBDIAGONAL ELEMENTS OF THE TRIDIAGONAL MATRIX (E(1)=0).    SYM22200
C                                                                       SYM22210
C E2     R  SQUARES OF THE ELEMENTS OF E.                               SYM22220
C           E2 MAY COINCIDE WITH E IF THE SQUARES ARE NOT NEEDED.       SYM22230
C                                                                       SYM22240
C     THIS SUBROUTINE IS BASED ON THE ALGOL PROCEDURE TRED1,            SYM22250
C     NUM. MATH. 11, 181-195(1968) BY MARTIN, REINSCH, AND WILKINSON.   SYM22260
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 212-226(1971).   SYM22270
C                                                                       SYM22280
C SUBPROGRAMS CALLED:  DSC16                                            SYM22290
C                                                                       SYM22300
C     VERSION FOR THE IBM 3090VF DATED NOVEMBER 1987.                   SYM22310
C     ------------------------------------------------------------------SYM22320
      REAL*8      C, C2, S, T, S16, R16                                 SYM22330
      INTEGER     I, J, K                                               SYM22340
C                                                                       SYM22350
      CALL XUFLOW(0)                                                    SYM22360
C PRESERVATION OF THE DIAGONAL ELEMENTS                                 SYM22370
         DO 80 I=1,N                                                    SYM22380
80       D(I)=A(I,I)                                                    SYM22390
      E2(1)=0D0                                                         SYM22400
      E(1) =0D0                                                         SYM22410
         DO 300 K=1,N-2                                                 SYM22420
C SWAP OF THE ORIGINAL AND TRANSFORMED DIAGONAL ELEMENTS                SYM22430
         S=D(K)                                                         SYM22440
         D(K)=A(K,K)                                                    SYM22450
         A(K,K)=S                                                       SYM22460
C SCALING FOR THE COMPUTATION OF THE L2 NORM                            SYM22470
         S=0D0                                                          SYM22480
            DO 100 I=N,K+2,-1                                           SYM22490
100         S=S+DABS(A(I,K))                                            SYM22500
C THE TRANSFORMATION IS BYPASSED IF COLUMN K IS ALREADY IN REDUCED FORM SYM22510
         IF(S.NE.0D0) GO TO 120                                         SYM22520
            T=A(K+1,K)                                                  SYM22530
            A(K+1,K)=0D0                                                SYM22540
            E2(K+1)=T*T                                                 SYM22550
            E(K+1) =T                                                   SYM22560
            GO TO 300                                                   SYM22570
C SCALED HOUSEHOLDER VECTOR                                             SYM22580
120      S=S+DABS(A(K+1,K))                                             SYM22590
         C2=0D0                                                         SYM22600
         CALL DSC16 (S, S16, R16)                                       SYM22610
            DO 140 I=K+1,N                                              SYM22620
            E(I)=R16*A(I,K)                                             SYM22630
140         C2=C2+E(I)**2                                               SYM22640
         C=-DSIGN(DSQRT(C2),E(K+1))                                     SYM22650
         T=1D0/C                                                        SYM22660
            DO 150 I=K+2,N                                              SYM22670
150         A(I,K)=T*E(I)                                               SYM22680
         A(K+1,K)=T*E(K+1)-1D0                                          SYM22690
C NORMALIZING FACTOR OF THE REFLECTION                                  SYM22700
         T=1D0/A(K+1,K)                                                 SYM22710
C CODIAGONAL ELEMENT AND ITS SQUARE                                     SYM22720
         C =S16*C                                                       SYM22730
         C2=S16*S16*C2                                                  SYM22740
C TRANSFORMATION OF THE SCALED HOUSEHOLDER VECTOR BY THE UNREDUCED      SYM22750
C SUBMATRIX WITH RESULT IN E(K+1,...,N)                                 SYM22760
         E(N)=A(N,K)*A(N,N)                                             SYM22770
            DO 180 J=N-1,K+1,-1                                         SYM22780
            S=A(J,K)*A(J,J)                                             SYM22790
               DO 160 I=N,J+1,-1                                        SYM22800
               S=S+A(I,K)*A(I,J)                                        SYM22810
160            E(I)=E(I)+A(J,K)*A(I,J)                                  SYM22820
180         E(J)=S                                                      SYM22830
C COMPOSITE VECTOR OF THE SIMILARITY TRANSFORMATION IN E(K+1,...,N)     SYM22840
         S=0D0                                                          SYM22850
            DO 200 I=K+1,N                                              SYM22860
200         S=S+E(I)*A(I,K)                                             SYM22870
         S=0.5D0*S*T                                                    SYM22880
            DO 220 I=K+1,N                                              SYM22890
220         E(I)=T*(E(I)+S*A(I,K))                                      SYM22900
C TRANSFORMATION OF THE UNREDUCED SUBMATRIX                             SYM22910
            DO 260 J=K+1,N                                              SYM22920
C"    ( IGNORE RECRDEPS                                                 SYM22930
               DO 240 I=J,N                                             SYM22940
240            A(I,J)=A(I,J)+A(I,K)*E(J)+A(J,K)*E(I)                    SYM22950
260         CONTINUE                                                    SYM22960
         E2(K+1)=C2                                                     SYM22970
         E(K+1) =C                                                      SYM22980
300      CONTINUE                                                       SYM22990
C LAST ELEMENTS OF THE TRIDIAGONAL MATRIX                               SYM23000
         S=D(N)                                                         SYM23010
         D(N)=A(N,N)                                                    SYM23020
         A(N,N)=S                                                       SYM23030
         IF(N.EQ.1) GO TO 1000                                          SYM23040
            S=D(N-1)                                                    SYM23050
            D(N-1)=A(N-1,N-1)                                           SYM23060
            A(N-1,N-1)=S                                                SYM23070
            T=A(N,N-1)                                                  SYM23080
            E2(N)=T*T                                                   SYM23090
            E(N)=T                                                      SYM23100
1000  RETURN                                                            SYM23110
      END                                                               SYM23120
