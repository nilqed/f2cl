C-----------------------------------------------------------------------SYM16890
C EIGENVALUES OF A SYMMETRIC TRIDIAGONAL MATRIX BY THE QL ALGORITHM WITHSYM16900
C EXPLICIT SHIFT.                                                       SYM16910
C                                                                       SYM16920
      SUBROUTINE  TQL1 ( N, D, E, IERR )                                SYM16930
C                                                                       SYM16940
      INTEGER     N, IERR                                               SYM16950
      REAL*8      D(N), E(N)                                            SYM16960
C                                                                       SYM16970
C N      E  ORDER OF THE TRIDIAGONAL MATRIX.                            SYM16980
C                                                                       SYM16990
C D      E  DIAGONAL OF THE TRIDIAGONAL MATRIX.                         SYM17000
C        R  EIGENVALUES IN THE ORDER OF INCREASING VALUE.               SYM17010
C                                                                       SYM17020
C E      E  CODIAGONAL OF THE TRIDIAGONAL MATRIX  (E(1)=0).             SYM17030
C        R  CODIAGONAL RESIDUE.                                         SYM17040
C                                                                       SYM17050
C IERR   R  IERR=0   NORMAL RETURN.                                     SYM17060
C           IERR>0   30 ITERATIONS COULD NOT PRODUCE THE IERR-TH        SYM17070
C                    EIGENVALUE. THE UNORDERED EIGENVALUES 1,...,IER-1  SYM17080
C                    SHOULD BE CORRECT.                                 SYM17090
C                                                                       SYM17100
C     THIS SUBROUTINE IS BASED ON THE ALGOL PROCEDURE TQL1,             SYM17110
C     NUM. MATH. 11, 293-306(1968) BY BOWDLER, MARTIN, REINSCH, AND     SYM17120
C     WILKINSON.                                                        SYM17130
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 227-240(1971).   SYM17140
C                                                                       SYM17150
C     VERSION FOR THE IBM 3090VF DATED NOVEMBER 1987.                   SYM17160
C     ------------------------------------------------------------------SYM17170
      INTEGER     I, J, L, M                                            SYM17180
      REAL*8      C, C2, C3, DL1, EL1, F, G, H, P, R, S, S2, TST1, TST2 SYM17190
C                                                                       SYM17200
      CALL XUFLOW(0)                                                    SYM17210
      IERR=0                                                            SYM17220
      IF (N .EQ. 1) GO TO 1000                                          SYM17230
         DO 100 I=2,N                                                   SYM17240
100      E(I-1)=E(I)                                                    SYM17250
      F=0D0                                                             SYM17260
      TST1=0D0                                                          SYM17270
      E(N)=0D0                                                          SYM17280
C                                                                       SYM17290
         DO 290 L=1,N                                                   SYM17300
         J=0                                                            SYM17310
         H=DABS(D(L))+DABS(E(L))                                        SYM17320
         IF (TST1 .LT. H) TST1=H                                        SYM17330
C SEARCH FOR A SMALL SUBDIAGONAL ELEMENT                                SYM17340
            DO 110 M=L,N                                                SYM17350
            TST2=TST1+DABS(E(M))                                        SYM17360
            IF (TST2 .EQ. TST1) GO TO 120                               SYM17370
110         CONTINUE                                                    SYM17380
C AN EIGENVALUE IS FOUND                                                SYM17390
120      IF (M .EQ. L) GO TO 210                                        SYM17400
C ITERATION                                                             SYM17410
130      IF (J .EQ. 30) GO TO 999                                       SYM17420
         J=J+1                                                          SYM17430
C SHIFT                                                                 SYM17440
         G=D(L)                                                         SYM17450
         P=(D(L+1)-G)/(2D0*E(L))                                        SYM17460
         IF(DABS(P).GT.1D0) GO TO 134                                   SYM17470
            P=P+DSIGN(DSQRT(1D0+P*P),P)                                 SYM17480
            GO TO 137                                                   SYM17490
134      P=P*(1D0+DSQRT(1D0+(1D0/P)**2))                                SYM17500
137      D(L)=E(L)/P                                                    SYM17510
         D(L+1)=E(L)*P                                                  SYM17520
         H=G-D(L)                                                       SYM17530
            DO 140 I=L+2,N                                              SYM17540
140         D(I)=D(I)-H                                                 SYM17550
         F=F+H                                                          SYM17560
C QL SWEEP                                                              SYM17570
         P=D(M)                                                         SYM17580
         DL1=D(L+1)                                                     SYM17590
         EL1=E(L+1)                                                     SYM17600
         C=1D0                                                          SYM17610
         C2=1D0                                                         SYM17620
         S=0D0                                                          SYM17630
            DO 200 I=M-1,L,-1                                           SYM17640
            C3=C2                                                       SYM17650
            C2=C                                                        SYM17660
            S2=S                                                        SYM17670
            G=C*E(I)                                                    SYM17680
            H=C*P                                                       SYM17690
            IF(DABS(P).LT.DABS(E(I))) GO TO 160                         SYM17700
               S=E(I)/P                                                 SYM17710
               R=DSQRT(1D0+S*S)                                         SYM17720
               E(I+1)=S2*P*R                                            SYM17730
               C=1D0/R                                                  SYM17740
               S=S*C                                                    SYM17750
               GO TO 180                                                SYM17760
160         C=P/E(I)                                                    SYM17770
            R=DSQRT(1D0+C*C)                                            SYM17780
            E(I+1)=S2*E(I)*R                                            SYM17790
            S=1D0/R                                                     SYM17800
            C=C*S                                                       SYM17810
180         P=C*D(I)-S*G                                                SYM17820
            D(I+1)=H+S*(C*G+S*D(I))                                     SYM17830
200         CONTINUE                                                    SYM17840
         P=-S*S2*C3*EL1*E(L)/DL1                                        SYM17850
         E(L)=S*P                                                       SYM17860
         D(L)=C*P                                                       SYM17870
         TST2=TST1+DABS(E(L))                                           SYM17880
         IF (TST2.GT.TST1) GO TO 130                                    SYM17890
210      P=D(L)+F                                                       SYM17900
C ORDERING OF THE EIGENVALUES                                           SYM17910
            DO 230 I=L,2,-1                                             SYM17920
            IF (P.GE.D(I-1)) GO TO 270                                  SYM17930
            D(I)=D(I-1)                                                 SYM17940
230         CONTINUE                                                    SYM17950
         I=1                                                            SYM17960
270      D(I)=P                                                         SYM17970
290      CONTINUE                                                       SYM17980
      GO TO 1000                                                        SYM17990
C ERROR                                                                 SYM18000
999   IERR=L                                                            SYM18010
C                                                                       SYM18020
1000  RETURN                                                            SYM18030
      END                                                               SYM18040
