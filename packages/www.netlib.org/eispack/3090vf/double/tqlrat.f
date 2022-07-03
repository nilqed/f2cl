C-----------------------------------------------------------------------SYM19450
C EIGENVALUES OF A SYMMETRIC TRIDIAGONAL MATRIX BY THE RATIONAL QL      SYM19460
C ALGORITHM.                                                            SYM19470
C                                                                       SYM19480
      SUBROUTINE  TQLRAT ( N, D, E2, IERR )                             SYM19490
C                                                                       SYM19500
      INTEGER     N, IERR                                               SYM19510
      REAL*8      D(N), E2(N)                                           SYM19520
C                                                                       SYM19530
C N      E  ORDER OF THE TRIDIAGONAL MATRIX.                            SYM19540
C                                                                       SYM19550
C D      E  DIAGONAL OF THE TRIDIAGONAL MATRIX.                         SYM19560
C        R  EIGENVALUES IN THE ORDER OF INCREASING VALUE.               SYM19570
C                                                                       SYM19580
C E2     E  SQUARES OF THE CODIAGONAL ELEMENTS OF THE TRIDIAGONAL MATRIXSYM19590
C           IN CELLS 2,...,N.                                           SYM19600
C        R  CODIAGONAL RESIDUE.                                         SYM19610
C                                                                       SYM19620
C IERR   R  IERR=0   NORMAL RETURN                                      SYM19630
C           IERR>0   30 ITERATIONS COULD NOT PRODUCE THE IERR-TH        SYM19640
C                    EIGENVALUE.  THE UNORDERED EIGENVALUES 1,...,IERR-1SYM19650
C                    SHOULD BE CORRECT.                                 SYM19660
C                                                                       SYM19670
C     THIS SUBROUTINE IS BASED ON THE ALGOL PROCEDURE TQLRAT,           SYM19680
C     ALGORITHM 464, COMM. ACM 16, 689(1973) BY REINSCH.                SYM19690
C                                                                       SYM19700
C     VERSION FOR THE IBM 3090VF DATED NOVEMBER 1987.                   SYM19710
C     ------------------------------------------------------------------SYM19720
      INTEGER     I, J, L, M                                            SYM19730
      REAL*8      B, C, F, G, H, P, R, S, T, EPS                        SYM19740
      DATA        EPS/Z3410000000000000/                                SYM19750
C                                                                       SYM19760
      CALL XUFLOW(0)                                                    SYM19770
      IERR=0                                                            SYM19780
      IF (N.EQ.1) GO TO 1000                                            SYM19790
         DO 100 I=2,N                                                   SYM19800
100      E2(I-1)=E2(I)                                                  SYM19810
      F=0D0                                                             SYM19820
      T=0D0                                                             SYM19830
      E2(N)=0D0                                                         SYM19840
C                                                                       SYM19850
         DO 340 L=1,N                                                   SYM19860
         J=0                                                            SYM19870
         H=DABS(D(L))+DSQRT(E2(L))                                      SYM19880
         IF (T.GT.H) GO TO 120                                          SYM19890
         T=H                                                            SYM19900
C SEARCH FOR A SMALL SUBDIAGONAL ELEMENT                                SYM19910
         B=EPS*T                                                        SYM19920
         C=B*B                                                          SYM19930
120         DO 140 M=L,N                                                SYM19940
            IF (E2(M).LE.C) GO TO 160                                   SYM19950
140         CONTINUE                                                    SYM19960
C AN EIGENVALUE IS FOUND                                                SYM19970
160      IF (M.EQ.L) GO TO 280                                          SYM19980
180      IF (J.EQ.30) GO TO 999                                         SYM19990
C ITERATION                                                             SYM20000
         J=J+1                                                          SYM20010
C SHIFT                                                                 SYM20020
         S=DSQRT(E2(L))                                                 SYM20030
         G=D(L)                                                         SYM20040
         P=(D(L+1)-G)/(S+S)                                             SYM20050
         IF(DABS(P).GT.1D0) GO TO 200                                   SYM20060
            R=DSIGN(DSQRT(1D0+P*P),P)                                   SYM20070
            GO TO 220                                                   SYM20080
200      R=P*DSQRT(1D0+(1D0/P)**2)                                      SYM20090
220      D(L)=S/(P+R)                                                   SYM20100
         H=G-D(L)                                                       SYM20110
            DO 240 I=L+1,N                                              SYM20120
240         D(I)=D(I)-H                                                 SYM20130
         F=F+H                                                          SYM20140
C RATIONAL QL SWEEP                                                     SYM20150
         G=D(M)                                                         SYM20160
         IF (G.EQ.0D0) G=B                                              SYM20170
         H=G                                                            SYM20180
         S=0D0                                                          SYM20190
            DO 260 I=M-1,L,-1                                           SYM20200
            P=G*H                                                       SYM20210
            R=P+E2(I)                                                   SYM20220
            E2(I+1)=S*R                                                 SYM20230
            S=E2(I)/R                                                   SYM20240
            D(I+1)=H+S*(H+D(I))                                         SYM20250
            G=D(I)-E2(I)/G                                              SYM20260
            IF (G.EQ.0D0) G=B                                           SYM20270
            H=G*P/R                                                     SYM20280
260         CONTINUE                                                    SYM20290
         E2(L)=S*G                                                      SYM20300
         D(L)=H                                                         SYM20310
         IF (H.EQ.0D0) GO TO 280                                        SYM20320
         IF (DABS(E2(L)).LE.DABS(C/H)) GO TO 280                        SYM20330
C RECOVERY FROM FLOATING-POINT UNDERFLOW OF A SUBDIAGONAL ELEMENT       SYM20340
         E2(L)=H*E2(L)                                                  SYM20350
         IF (E2(L) .NE. 0D0) GO TO 180                                  SYM20360
280      P=D(L)+F                                                       SYM20370
C ORDERING OF THE EIGENVALUES                                           SYM20380
            DO 300 I=L,2,-1                                             SYM20390
            IF (P .GE. D(I-1)) GO TO 320                                SYM20400
            D(I)=D(I-1)                                                 SYM20410
300         CONTINUE                                                    SYM20420
         I=1                                                            SYM20430
320      D(I)=P                                                         SYM20440
340      CONTINUE                                                       SYM20450
      GO TO 1000                                                        SYM20460
C     ERROR                                                             SYM20470
999   IERR=L                                                            SYM20480
C                                                                       SYM20490
1000  RETURN                                                            SYM20500
      END                                                               SYM20510
