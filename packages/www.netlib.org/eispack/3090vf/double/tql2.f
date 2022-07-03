C-----------------------------------------------------------------------SYM18050
C EIGENVALUES AND EIGENVECTORS OF A SYMMETRIC TRIDIAGONAL MATRIX BY THE SYM18060
C QL ALGORITHM WITH EXPLICIT SHIFT.                                     SYM18070
C                                                                       SYM18080
      SUBROUTINE  TQL2 ( LD, N, D, E, Z, IERR )                         SYM18090
C                                                                       SYM18100
      INTEGER     LD, N, IERR                                           SYM18110
      REAL*8      Z(LD,N), D(N), E(N)                                   SYM18120
C                                                                       SYM18130
C LD     E  FIRST DIMENSION ASSIGNED TO THE TWO-DIMENSIONAL ARRAY       SYM18140
C           PARAMETERS IN THE CALLING PROGRAM.                          SYM18150
C                                                                       SYM18160
C N      E  ORDER OF THE TRIDIAGONAL MATRIX.                            SYM18170
C                                                                       SYM18180
C D      E  DIAGONAL OF THE TRIDIAGONAL MATRIX.                         SYM18190
C        R  EIGENVALUES IN THE ORDER OF INCREASING VALUE.               SYM18200
C                                                                       SYM18210
C E      E  CODIAGONAL OF THE TRIDIAGONAL MATRIX  (E(1)=0).             SYM18220
C        R  CODIAGONAL RESIDUE.                                         SYM18230
C                                                                       SYM18240
C Z      E  FOR THE SOLUTION OF THE SYMMETRIC EIGENPROBLEM:             SYM18250
C              RIGHT MATRIX OF THE SIMILARITY TRANSFORMATION TO         SYM18260
C              TRIDIAGONAL FORM.                                        SYM18270
C           FOR THE SOLUTION OF THE TRIDIAGONAL EIGENPROBLEM:           SYM18280
C              IDENTITY MATRIX OF ORDER N.                              SYM18290
C        R  MATRIX OF EIGENVECTORS.                                     SYM18300
C                                                                       SYM18310
C IERR   R  IERR=0   NORMAL RETURN.                                     SYM18320
C           IERR>0   30 ITERATIONS COULD NOT PRODUCE THE IERR-TH        SYM18330
C                    EIGENVALUE. THE UNORDERED EIGENPAIRS 1,...,IER-1   SYM18340
C                    SHOULD BE CORRECT.                                 SYM18350
C                                                                       SYM18360
C     THIS SUBROUTINE IS BASED ON THE ALGOL PROCEDURE TQL2,             SYM18370
C     NUM. MATH. 11, 293-306(1968) BY BOWDLER, MARTIN, REINSCH, AND     SYM18380
C     WILKINSON.                                                        SYM18390
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 227-240(1971).   SYM18400
C                                                                       SYM18410
C     VERSION FOR THE IBM 3090VF DATED NOVEMBER 1987.                   SYM18420
C     ------------------------------------------------------------------SYM18430
      INTEGER     I, J, L, M                                            SYM18440
      REAL*8      C, C2, C3, DL1, EL1, F, G, H, P, R, S, S2, TST1, TST2 SYM18450
C                                                                       SYM18460
      CALL XUFLOW(0)                                                    SYM18470
      IERR=0                                                            SYM18480
      IF (N .EQ. 1) GO TO 1000                                          SYM18490
         DO 100 I=2,N                                                   SYM18500
100      E(I-1)=E(I)                                                    SYM18510
      F=0D0                                                             SYM18520
      TST1=0D0                                                          SYM18530
      E(N)=0D0                                                          SYM18540
C                                                                       SYM18550
         DO 340 L=1,N                                                   SYM18560
         J=0                                                            SYM18570
         H=DABS(D(L))+DABS(E(L))                                        SYM18580
         IF (TST1 .LT. H) TST1=H                                        SYM18590
C SEARCH FOR A SMALL SUBDIAGONAL ELEMENT                                SYM18600
            DO 120 M=L,N                                                SYM18610
            TST2=TST1+DABS(E(M))                                        SYM18620
            IF (TST2 .EQ. TST1) GO TO 140                               SYM18630
120         CONTINUE                                                    SYM18640
C AN EIGENVALUE IS FOUND                                                SYM18650
140      IF (M .EQ. L) GO TO 320                                        SYM18660
C ITERATION                                                             SYM18670
160      IF (J .EQ. 30) GO TO 999                                       SYM18680
         J=J+1                                                          SYM18690
C SHIFT                                                                 SYM18700
         G=D(L)                                                         SYM18710
         P=(D(L+1)-G)/(2D0*E(L))                                        SYM18720
         IF(DABS(P).GT.1D0) GO TO 180                                   SYM18730
            P=P+DSIGN(DSQRT(1D0+P*P),P)                                 SYM18740
            GO TO 200                                                   SYM18750
180      P=P*(1D0+DSQRT(1D0+(1D0/P)**2))                                SYM18760
200      D(L)=E(L)/P                                                    SYM18770
         D(L+1)=E(L)*P                                                  SYM18780
         H=G-D(L)                                                       SYM18790
            DO 220 I=L+2,N                                              SYM18800
220         D(I)=D(I)-H                                                 SYM18810
         F=F+H                                                          SYM18820
C QL SWEEP                                                              SYM18830
         P=D(M)                                                         SYM18840
         DL1=D(L+1)                                                     SYM18850
         EL1=E(L+1)                                                     SYM18860
         C=1D0                                                          SYM18870
         C2=1D0                                                         SYM18880
         S=0D0                                                          SYM18890
            DO 300 I=M-1,L,-1                                           SYM18900
            C3=C2                                                       SYM18910
            C2=C                                                        SYM18920
            S2=S                                                        SYM18930
            G=C*E(I)                                                    SYM18940
            H=C*P                                                       SYM18950
            IF(DABS(P).LT.DABS(E(I))) GO TO 240                         SYM18960
               S=E(I)/P                                                 SYM18970
               R=DSQRT(1D0+S*S)                                         SYM18980
               E(I+1)=S2*P*R                                            SYM18990
               C=1D0/R                                                  SYM19000
               S=S*C                                                    SYM19010
               GO TO 260                                                SYM19020
240         C=P/E(I)                                                    SYM19030
            R=DSQRT(1D0+C*C)                                            SYM19040
            E(I+1)=S2*E(I)*R                                            SYM19050
            S=1D0/R                                                     SYM19060
            C=C*S                                                       SYM19070
260         P=C*D(I)-S*G                                                SYM19080
            D(I+1)=H+S*(C*G+S*D(I))                                     SYM19090
C    EIGENVECTORS                                                       SYM19100
               DO 280 K=1,N                                             SYM19110
               G=Z(K,I)                                                 SYM19120
               H=Z(K,I+1)                                               SYM19130
               Z(K,I+1)=S*G+C*H                                         SYM19140
280            Z(K,I  )=C*G-S*H                                         SYM19150
300         CONTINUE                                                    SYM19160
         P=-S*S2*C3*EL1*E(L)/DL1                                        SYM19170
         E(L)=S*P                                                       SYM19180
         D(L)=C*P                                                       SYM19190
         TST2=TST1+DABS(E(L))                                           SYM19200
         IF (TST2.GT.TST1) GO TO 160                                    SYM19210
320      D(L)=D(L)+F                                                    SYM19220
340      CONTINUE                                                       SYM19230
C ORDERING OF THE EIGENPAIRS                                            SYM19240
         DO 400 I=1,N-1                                                 SYM19250
         J=I                                                            SYM19260
            DO 360 L=I+1,N                                              SYM19270
            IF(D(L).LT.D(J)) J=L                                        SYM19280
360         CONTINUE                                                    SYM19290
         IF(J.EQ.I) GO TO 400                                           SYM19300
         P=D(I)                                                         SYM19310
         D(I)=D(J)                                                      SYM19320
         D(J)=P                                                         SYM19330
            DO 380 L=1,N                                                SYM19340
            P=Z(L,I)                                                    SYM19350
            Z(L,I)=Z(L,J)                                               SYM19360
380         Z(L,J)=P                                                    SYM19370
400      CONTINUE                                                       SYM19380
      GO TO 1000                                                        SYM19390
C ERROR                                                                 SYM19400
999   IERR=L                                                            SYM19410
C                                                                       SYM19420
1000  RETURN                                                            SYM19430
      END                                                               SYM19440
