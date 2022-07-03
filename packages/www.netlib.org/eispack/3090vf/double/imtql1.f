C-----------------------------------------------------------------------SYM09250
C EIGENVALUES OF A SYMMETRIC TRIDIAGONAL MATRIX BY THE QL ALGORITHM WITHSYM09260
C IMPLICIT SHIFT.                                                       SYM09270
C                                                                       SYM09280
      SUBROUTINE  IMTQL1 ( N, D, E, IER )                               SYM09290
C                                                                       SYM09300
      REAL*8      D(N), E(N)                                            SYM09310
      INTEGER     N, IER                                                SYM09320
C                                                                       SYM09330
C N      E  ORDER OF THE TRIDIAGONAL MATRIX.                            SYM09340
C                                                                       SYM09350
C D      E  DIAGONAL OF THE TRIDIAGONAL MATRIX.                         SYM09360
C        R  EIGENVALUES IN THE ORDER OF INCREASING VALUE.               SYM09370
C                                                                       SYM09380
C E      E  CODIAGONAL OF THE TRIDIAGONAL MATRIX  (E(1)=0).             SYM09390
C        R  CODIAGONAL RESIDUE.                                         SYM09400
C                                                                       SYM09410
C IER    R     IER=0   NORMAL RETURN.                                   SYM09420
C              IER>0   THE UNORDERED EIGENVALUES 1,...,IER-1 SHOULD     SYM09430
C                      BE CORRECT.                                      SYM09440
C                                                                       SYM09450
C     THIS SUBROUTINE IS BASED ON THE ALGOL PROCEDURE IMTQL1,           SYM09460
C     NUM. MATH. 12, 377-383(1968) BY MARTIN AND WILKINSON,             SYM09470
C     AS MODIFIED IN NUM. MATH. 15, 450(1970) BY DUBRULLE.              SYM09480
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 241-248(1971).   SYM09490
C                                                                       SYM09500
C     VERSION FOR THE IBM 3090VF DATED NOVEMBER 1987.                   SYM09510
C     ------------------------------------------------------------------SYM09520
      REAL*8      C, S, T, U, H, P, Q                                   SYM09530
      INTEGER     IT, ITMAX, I, J, M                                    SYM09540
      DATA        ITMAX/30/                                             SYM09550
C                                                                       SYM09560
      CALL XUFLOW(0)                                                    SYM09570
      IER=0                                                             SYM09580
      IF(N.EQ.1) GO TO 1000                                             SYM09590
         DO 80 I=2,N                                                    SYM09600
80       E(I-1)=E(I)                                                    SYM09610
      E(N)=0D0                                                          SYM09620
C                                                                       SYM09630
         DO 340 J=1,N                                                   SYM09640
         IT=0                                                           SYM09650
C SEARCH FOR A SMALL SUBDIAGONAL ELEMENT                                SYM09660
100         DO 120 M=J,N-1                                              SYM09670
            Q=DABS(D(M))+DABS(D(M+1))                                   SYM09680
            P=Q+DABS(E(M))                                              SYM09690
            IF(P.EQ.Q) GO TO 140                                        SYM09700
120         CONTINUE                                                    SYM09710
         M=N                                                            SYM09720
C AN EIGENVALUE IS FOUND                                                SYM09730
140      IF(M.EQ.J) GO TO 280                                           SYM09740
C ITERATION                                                             SYM09750
         U=D(J)                                                         SYM09760
         T=E(J)                                                         SYM09770
         IF(IT.EQ.ITMAX) GO TO 999                                      SYM09780
         IT=IT+1                                                        SYM09790
C SHIFT                                                                 SYM09800
         Q=(D(J+1)-U)/(T+T)                                             SYM09810
         IF(DABS(Q).LE.1D0) GO TO 160                                   SYM09820
            Q=D(M)-U+T/(Q*(1D0+DSQRT(1D0+(1D0/Q)**2)))                  SYM09830
            GO TO 180                                                   SYM09840
160      Q=D(M)-U+T/(Q+DSIGN(DSQRT(1D0+Q**2),Q))                        SYM09850
C QL SWEEP                                                              SYM09860
180      U=0D0                                                          SYM09870
         S=1D0                                                          SYM09880
         C=1D0                                                          SYM09890
            DO 240 I=M-1,J,-1                                           SYM09900
            P=S*E(I)                                                    SYM09910
            H=C*E(I)                                                    SYM09920
            IF(DABS(P).LT.DABS(Q)) GO TO 200                            SYM09930
               IF(P.NE.0D0) GO TO 190                                   SYM09940
                  E(I+1)=0D0                                            SYM09950
                  D(I+1)=D(I+1)-U                                       SYM09960
                  E(M)=0D0                                              SYM09970
                  GO TO 100                                             SYM09980
190            C=Q/P                                                    SYM09990
               T=DSQRT(1D0+C*C)                                         SYM10000
               E(I+1)=P*T                                               SYM10010
               S=1D0/T                                                  SYM10020
               C=C*S                                                    SYM10030
               GO TO 220                                                SYM10040
200         S=P/Q                                                       SYM10050
            T=DSQRT(1D0+S*S)                                            SYM10060
            E(I+1)=Q*T                                                  SYM10070
            C=1D0/T                                                     SYM10080
            S=S*C                                                       SYM10090
220         Q=D(I+1)-U                                                  SYM10100
            T=(D(I)-Q)*S+(C+C)*H                                        SYM10110
            U=S*T                                                       SYM10120
            D(I+1)=Q+U                                                  SYM10130
            Q=C*T-H                                                     SYM10140
240         CONTINUE                                                    SYM10150
C COMPLETION OF QL SWEEP                                                SYM10160
         D(J)=D(J)-U                                                    SYM10170
         E(J)=Q                                                         SYM10180
         E(M)=0D0                                                       SYM10190
         GO TO 100                                                      SYM10200
C ORDERING OF THE EIGENVALUES                                           SYM10210
280      P=D(M)                                                         SYM10220
            DO 300 I=M,2,-1                                             SYM10230
            IF(P.GE.D(I-1)) GO TO 320                                   SYM10240
            D(I)=D(I-1)                                                 SYM10250
300         CONTINUE                                                    SYM10260
         I=1                                                            SYM10270
320      D(I)=P                                                         SYM10280
340      CONTINUE                                                       SYM10290
      GO TO 1000                                                        SYM10300
C ERROR                                                                 SYM10310
999   IER=J                                                             SYM10320
C                                                                       SYM10330
1000  RETURN                                                            SYM10340
      END                                                               SYM10350
