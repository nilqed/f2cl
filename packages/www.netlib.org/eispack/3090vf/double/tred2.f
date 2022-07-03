@PROCESS DIRECTIVE('"    (')                                            SYM23130
C-----------------------------------------------------------------------SYM23140
C REDUCTION OF A REAL SYMMETRIC MATRIX TO SYMMETRIC TRIDIAGONAL FORM BY SYM23150
C ORTHOGONAL SIMILARITY TRANSFORMATION AND CONSTRUCTION OF THE RIGHT    SYM23160
C OPERATOR OF THE REDUCTION.                                            SYM23170
C                                                                       SYM23180
      SUBROUTINE  TRED2 ( LD, N, A, D, E, Z )                           SYM23190
C                                                                       SYM23200
      INTEGER     LD, N                                                 SYM23210
      REAL*8      A(LD,N), D(N), E(N), Z(LD,N)                          SYM23220
C                                                                       SYM23230
C LD     E  FIRST DIMENSION ASSIGNED TO THE TWO-DIMENSIONAL ARRAY       SYM23240
C           PARAMETERS IN THE CALLING PROGRAM.                          SYM23250
C                                                                       SYM23260
C N      E  ORDER OF THE MATRIX.                                        SYM23270
C                                                                       SYM23280
C A      E  GIVEN SYMMETRIC MATRIX (ONLY ITS LOWER HALF IS USED).       SYM23290
C        R  RIGHT OPERATOR OF THE REDUCTION IF A AND Z COINCIDE.        SYM23300
C                                                                       SYM23310
C D      R  DIAGONAL ELEMENTS OF THE TRIDIAGONAL MATRIX.                SYM23320
C                                                                       SYM23330
C E      R  SUBDIAGONAL ELEMENTS OF THE TRIDIAGONAL MATRIX (E(1)=0).    SYM23340
C                                                                       SYM23350
C Z      R  RIGHT OPERATOR OF THE REDUCTION.                            SYM23360
C              ARRAYS A AND Z MAY COINCIDE.                             SYM23370
C                                                                       SYM23380
C     THIS SUBROUTINE IS BASED ON THE ALGOL PROCEDURE TRED2,            SYM23390
C     NUM. MATH. 11, 181-195(1968) BY MARTIN, REINSCH, AND WILKINSON.   SYM23400
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 212-226(1971).   SYM23410
C                                                                       SYM23420
C SUBPROGRAMS CALLED:  DSC16   KACHEL                                   SYM23430
C                                                                       SYM23440
C     VERSION FOR THE IBM 3090VF DATED NOVEMBER 1987.                   SYM23450
C     ------------------------------------------------------------------SYM23460
      REAL*8      C, C2, S, T, S16, R16                                 SYM23470
      INTEGER     KACHEL                                                SYM23480
      INTEGER     I, J, K, KK                                           SYM23490
C                                                                       SYM23500
      CALL XUFLOW(0)                                                    SYM23510
      KK=N-KACHEL(LD)                                                   SYM23520
      CALL XUFLOW(0)                                                    SYM23530
         DO 80 J=1,N                                                    SYM23540
            DO 60 I=J,N                                                 SYM23550
60          Z(I,J)=A(I,J)                                               SYM23560
80       CONTINUE                                                       SYM23570
      E(1)=0D0                                                          SYM23580
         DO 300 K=1,N-2                                                 SYM23590
         D(K)=Z(K,K)                                                    SYM23600
C SCALING FOR THE COMPUTATION OF THE L2 NORM                            SYM23610
         S=0D0                                                          SYM23620
            DO 100 I=K+2,N                                              SYM23630
100         S=S+DABS(Z(I,K))                                            SYM23640
C THE TRANSFORMATION IS BYPASSED IF COLUMN K IS ALREADY IN REDUCED FORM SYM23650
         IF(S.GT.0D0) GO TO 120                                         SYM23660
            Z(K,K)=0D0                                                  SYM23670
            E(K+1)=Z(K+1,K)                                             SYM23680
            GO TO 300                                                   SYM23690
C SCALED HOUSEHOLDER VECTOR IN D(K+1,...,N) AND COPY IN Z(*,K)          SYM23700
120      S=S+DABS(Z(K+1,K))                                             SYM23710
         C2=0D0                                                         SYM23720
         CALL DSC16 (S, S16, R16)                                       SYM23730
            DO 140 I=K+1,N                                              SYM23740
            T=R16*Z(I,K)                                                SYM23750
            D(I)=T                                                      SYM23760
            Z(I,K)=T                                                    SYM23770
140         C2=C2+T*T                                                   SYM23780
         C=-DSIGN(DSQRT(C2),D(K+1))                                     SYM23790
C NORMALIZING FACTOR OF THE REFLECTION                                  SYM23800
         T=1D0/(C*D(K+1)-C2)                                            SYM23810
         Z(K,K)=T                                                       SYM23820
         D(K+1)=D(K+1)-C                                                SYM23830
         Z(K+1,K)=D(K+1)                                                SYM23840
C CODIAGONAL ELEMENT                                                    SYM23850
         C=S16*C                                                        SYM23860
C TRANSFORMATION OF THE SCALED HOUSEHOLDER VECTOR BY THE UNREDUCED      SYM23870
C SUBMATRIX WITH RESULT IN E(K+1,...,N)                                 SYM23880
         E(N)=D(N)*Z(N,N)                                               SYM23890
            DO 200 J=N-1,K+1,-1                                         SYM23900
            S=D(J)*Z(J,J)                                               SYM23910
               DO 180 I=J+1,N                                           SYM23920
               S=S+D(I)*Z(I,J)                                          SYM23930
180            E(I)=E(I)+D(J)*Z(I,J)                                    SYM23940
200         E(J)=S                                                      SYM23950
C COMPOSITE VECTOR OF THE SIMILARITY TRANSFORMATION IN E(K+1,...,N)     SYM23960
         S=0D0                                                          SYM23970
            DO 220 I=K+1,N                                              SYM23980
220         S=S+E(I)*D(I)                                               SYM23990
         S=0.5D0*S*T                                                    SYM24000
            DO 240 I=K+1,N                                              SYM24010
240         E(I)=T*(E(I)+S*D(I))                                        SYM24020
C TRANSFORMATION OF THE UNREDUCED SUBMATRIX                             SYM24030
            DO 280 J=K+1,N                                              SYM24040
               DO 260 I=J,N                                             SYM24050
260            Z(I,J)=Z(I,J)+D(I)*E(J)+D(J)*E(I)                        SYM24060
280         CONTINUE                                                    SYM24070
         E(K+1)=C                                                       SYM24080
300      CONTINUE                                                       SYM24090
C LAST ELEMENTS OF THE TRIDIAGONAL MATRIX                               SYM24100
      D(N)=Z(N,N)                                                       SYM24110
      IF(N.EQ.1) GO TO 320                                              SYM24120
         D(N-1)=Z(N-1,N-1)                                              SYM24130
         T=Z(N,N-1)                                                     SYM24140
         E(N)=T                                                         SYM24150
C RIGHT OPERATOR OF THE SIMILARITY TRANSFORMATION                       SYM24160
320      DO 360 J=MAX0(1,N-1),N                                         SYM24170
            DO 340 I=1,N                                                SYM24180
340         Z(I,J)=0D0                                                  SYM24190
360      Z(J,J)=1D0                                                     SYM24200
         DO 500 K=N-2,1,-1                                              SYM24210
         T=Z(K,K)                                                       SYM24220
         IF(T.EQ.0D0) GO TO 460                                         SYM24230
         IF(K.LT.KK)  GO TO 410                                         SYM24240
C    PATH FOR SMALL ARRAYS                                              SYM24250
C"    ( IGNORE RECRDEPS                                                 SYM24260
C"    ( PREFER VECTOR                                                   SYM24270
            DO 400 J=K+1,N                                              SYM24280
            S=0D0                                                       SYM24290
               DO 370 I=K+1,N                                           SYM24300
370            S=S+Z(I,K)*Z(I,J)                                        SYM24310
            S=S*T                                                       SYM24320
               DO 380 I=K+1,N                                           SYM24330
380            Z(I,J)=Z(I,J)+S*Z(I,K)                                   SYM24340
400         CONTINUE                                                    SYM24350
         GO TO 460                                                      SYM24360
C    PATH FOR LARGE ARRAYS                                              SYM24370
410         DO 440 J=K+1,N                                              SYM24380
            S=0D0                                                       SYM24390
               DO 420 I=K+1,N                                           SYM24400
420            S=S+Z(I,K)*Z(I,J)                                        SYM24410
            S=S*T                                                       SYM24420
               DO 430 I=K+1,N                                           SYM24430
430            Z(I,J)=Z(I,J)+S*Z(I,K)                                   SYM24440
440         CONTINUE                                                    SYM24450
460         DO 480 I=1,N                                                SYM24460
480         Z(I,K)=0D0                                                  SYM24470
         Z(K,K)=1D0                                                     SYM24480
500      CONTINUE                                                       SYM24490
      RETURN                                                            SYM24500
      END                                                               SYM24510
