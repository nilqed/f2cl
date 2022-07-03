C-----------------------------------------------------------------------SYM24520
C SIMILARITY REDUCTION TO TRIDIAGONAL FORM OF A REAL SYMMETRIC MATRIX   SYM24530
C REPRESENTED BY ITS LOWER HALF PACKED ROW-WISE IN A ONE-DIMENSIONAL    SYM24540
C ARRAY (OR ITS UPPER HALF PACKED COLUMN-WISE).                         SYM24550
C                                                                       SYM24560
      SUBROUTINE  TRED3 ( N, NA, A, D, E, E2 )                          SYM24570
C                                                                       SYM24580
      INTEGER     N, NA                                                 SYM24590
      REAL*8      A(NA), D(N), E(N), E2(N)                              SYM24600
C                                                                       SYM24610
C N      E  ORDER OF THE MATRIX.                                        SYM24620
C                                                                       SYM24630
C NA     E  DIMENSION ASSIGNED TO THE ONE-DIMENSIONAL ARRAY A IN        SYM24640
C           THE CALLING PROGRAM.                                        SYM24650
C                                                                       SYM24660
C A      E  LOWER-TRIANGULAR PART OF THE SYMMETRIC MATRIX PACKED ROW-   SYM24670
C           WISE IN THE FIRST N(N+1)/2 CELLS (OR UPPER-TRIANGULAR       SYM24680
C           PART PACKED COLUMN-WISE)                                    SYM24690
C        R  DATA DEFINING THE TRANSFORMATIONS OF THE REDUCTION.         SYM24700
C                                                                       SYM24710
C D      R  DIAGONAL ELEMENTS OF THE TRIDIAGONAL MATRIX.                SYM24720
C                                                                       SYM24730
C E      R  CODIAGONAL ELEMENTS OF THE TRIDIAGONAL MATRIX (E(1)=0).     SYM24740
C                                                                       SYM24750
C E2     R  SQUARES OF THE ELEMENTS OF E.                               SYM24760
C           E2 MAY COINCIDE WITH E IF THE SQUARES ARE NOT NEEDED.       SYM24770
C                                                                       SYM24780
C     THIS SUBROUTINE IS BASED ON THE ALGOL PROCEDURE TRED3,            SYM24790
C     NUM. MATH. 11, 181-195(1968) BY MARTIN, REINSCH, AND WILKINSON.   SYM24800
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 212-226(1971).   SYM24810
C                                                                       SYM24820
C SUBPROGRAMS CALLED:  DSC16                                            SYM24830
C                                                                       SYM24840
C     VERSION FOR THE IBM 3090VF DATED NOVEMBER 1987.                   SYM24850
C     ------------------------------------------------------------------SYM24860
      INTEGER     I, J, K, KK, JJ                                       SYM24870
      REAL*8      C, C2, S, T, R16, S16                                 SYM24880
C                                                                       SYM24890
      CALL XUFLOW(0)                                                    SYM24900
      E(1)=0D0                                                          SYM24910
      E2(1)=0D0                                                         SYM24920
         DO 300 K=N,3,-1                                                SYM24930
C KK IS THE ADDRESS OFFSET OF COLUMN K, SO THAT THE ELEMENT A(I,K) IS   SYM24940
C STORED IN THE ARRAY CELL I+KK                                         SYM24950
         KK=(K*K-K)/2                                                   SYM24960
C DIAGONAL ELEMENT                                                      SYM24970
         D(K)=A(K+KK)                                                   SYM24980
C SCALING OF THE HOUSEHOLDER VECTOR                                     SYM24990
         T=A(K-1+KK)                                                    SYM25000
         S=0D0                                                          SYM25010
            DO 100 I=1,K-2                                              SYM25020
100         S=S+DABS(A(I+KK))                                           SYM25030
C THE TRANSFORMATION IS BYPASSED IF COLUMN K IS ALREADY IN REDUCED FORM SYM25040
         IF(S.NE.0D0) GO TO 120                                         SYM25050
            A(K+KK)=0D0                                                 SYM25060
            E2(K)=T*T                                                   SYM25070
            E(K)=T                                                      SYM25080
            GO TO 300                                                   SYM25090
C SCALED HOUSEHOLDER VECTOR IN D(1,...,K-1)                             SYM25100
120      S=S+DABS(T)                                                    SYM25110
         CALL DSC16 (S, S16, R16)                                       SYM25120
         C2=0D0                                                         SYM25130
            DO 140 I=1,K-1                                              SYM25140
            D(I)=A(I+KK)*R16                                            SYM25150
            A(I+KK)=D(I)                                                SYM25160
140         C2=C2+D(I)**2                                               SYM25170
         C=-DSIGN(DSQRT(C2),D(K-1))                                     SYM25180
C NORMALIZING FACTOR OF THE REFLECTION                                  SYM25190
         T=1D0/(C*D(K-1)-C2)                                            SYM25200
         A(K+KK)=T                                                      SYM25210
         D(K-1)=D(K-1)-C                                                SYM25220
         A(K-1+KK)=D(K-1)                                               SYM25230
C CODIAGONAL ELEMENT AND ITS SQUARE                                     SYM25240
         E2(K)=S16*S16*C2                                               SYM25250
         E(K) =S16*C                                                    SYM25260
C TRANSFORMATION OF THE SCALED HOUSEHOLDER VECTOR BY THE UNREDUCED      SYM25270
C SUBMATRIX WITH RESULT IN E(1,...,K-1)                                 SYM25280
         E(1)=D(1)*A(1)                                                 SYM25290
            DO 200 J=2,K-1                                              SYM25300
            JJ=(J*J-J)/2                                                SYM25310
            S=0D0                                                       SYM25320
               DO 180 I=1,J-1                                           SYM25330
               S=S+D(I)*A(I+JJ)                                         SYM25340
180            E(I)=E(I)+D(J)*A(I+JJ)                                   SYM25350
200         E(J)=S+D(J)*A(J+JJ)                                         SYM25360
C COMPOSITE VECTOR OF THE SIMILARITY TRANSFORMATION IN E(1,...,K-1)     SYM25370
         S=0D0                                                          SYM25380
            DO 220 I=1,K-1                                              SYM25390
220         S=S+E(I)*D(I)                                               SYM25400
         S=0.5D0*S*T                                                    SYM25410
            DO 240 I=1,K-1                                              SYM25420
240         E(I)=T*(E(I)+S*D(I))                                        SYM25430
C TRANSFORMATION OF THE UNREDUCED SUBMATRIX                             SYM25440
            DO 280 J=1,K-1                                              SYM25450
            JJ=(J*J-J)/2                                                SYM25460
               DO 260 I=1,J                                             SYM25470
260            A(I+JJ)=A(I+JJ)+D(I)*E(J)+D(J)*E(I)                      SYM25480
280         CONTINUE                                                    SYM25490
300      CONTINUE                                                       SYM25500
C LAST ELEMENTS OF THE TRIDIAGONAL MATRIX                               SYM25510
         D(1)=A(1)                                                      SYM25520
         IF(N.EQ.1) GO TO 1000                                          SYM25530
            D(2)=A(3)                                                   SYM25540
            T=A(2)                                                      SYM25550
            E2(2)=T*T                                                   SYM25560
            E(2)=T                                                      SYM25570
1000  RETURN                                                            SYM25580
      END                                                               SYM25590
