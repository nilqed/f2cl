C-----------------------------------------------------------------------SYM13040
C SMALLEST OR LARGEST EIGENVALUES OF A SYMMETRIC TRIDIAGONAL MATRIX BY  SYM13050
C THE RATIONAL QR METHOD WITH NEWTON SHIFTS.                            SYM13060
C                                                                       SYM13070
      SUBROUTINE  RATQR (N,EPS1,D,E,E2,M,W,IND,BD,TYPE,IDEF,IERR)       SYM13080
C                                                                       SYM13090
      INTEGER     N, M, IND(N), IDEF, IERR                              SYM13100
      REAL*8      EPS1, D(N), E(N), E2(N), W(N), BD(N)                  SYM13110
      LOGICAL     TYPE                                                  SYM13120
C                                                                       SYM13130
C N      E  ORDER OF THE MATRIX.                                        SYM13140
C                                                                       SYM13150
C EPS1   E  ABSOLUTE ERROR TOLERANCE FOR THE COMPUTED EIGENVALUES.      SYM13160
C        R  EPS1 IS NOT ALTERED IF ITS ENTRY VALUE NEVER FALLS UNDER    SYM13170
C           A DEFAULT VALUE EQUAL TO THE PRODUCT OF THE MACHINE         SYM13180
C           PRECISION AND THE ABSOLUTE VALUE OF ANY EIGENVALUE ITERATE. SYM13190
C           OTHERWISE, EPS1 IS RETURN AS THE DEFAULT VALUE FOR THE      SYM13200
C           LAST COMPUTED EIGENVALUE.                                   SYM13210
C           THE ABSOLUTE ERROR IN THE K-TH EIGENVALUE IS USUALLY SMALLERSYM13220
C           THAN K TIMES EPS1.                                          SYM13230
C                                                                       SYM13240
C D      E  DIAGONAL ELEMENTS OF THE TRIDIAGONAL MATRIX.                SYM13250
C                                                                       SYM13260
C E      E  CODIAGONAL ELEMENTS OF THE TRIDIAGONAL MATRIX IN            SYM13270
C           CELLS 2,...,N.                                              SYM13280
C                                                                       SYM13290
C E2     E  SQUARES OF THE CODIAGONAL ELEMENTS OF THE TRIDIAGONAL MATRIXSYM13300
C           IN CELLS 2,...,N.                                           SYM13310
C        R  THE ELEMENTS OF E2 CORRESPONDING TO ELEMENTS OF E REGARDED  SYM13320
C           AS NEGLIGIBLE ARE REPLACED BY ZEROS. THESE DEFINE SPLITTINGSSYM13330
C           OF THE MATRIX INTO PRINCIPAL SUBMATRICES.                   SYM13340
C           E2(1) IS SET TO 0.0 (OR 2.0) IF THE SMALLEST (OR LARGEST)   SYM13350
C           EIGENVALUES WERE COMPUTED.                                  SYM13360
C           E2 IS OTHERWISE UNALTERED (UNLESS OVERWRITTEN BY BD)        SYM13370
C                                                                       SYM13380
C M      E  NUMBER OF EIGENVALUES TO BE FOUND.                          SYM13390
C                                                                       SYM13400
C W      R  THE M ALGEBRAICALLY SMALLEST EIGENVALUES IN ASCENDING ORDER,SYM13410
C           OR THE M LARGEST EIGENVALUES IN DESCENDING ORDER.           SYM13420
C           IF AN ERROR EXIT OCCURS BECAUSE OF AN IMPROPER SPECIFICATIONSYM13430
C           OF IDEF, NO EIGENVALUE IS COMPUTED.                         SYM13440
C           IF THE NEWTON ITERATES FOR A PARTICULAR EIGENVALUE ARE NOT  SYM13450
C           MONOTONIC, THE BEST ESTIMATE OBTAINED IS RETURNED AS AN     SYM13460
C           EIGENVALUE AND IERR IS SET TO REFLECT THIS CONDITION.       SYM13470
C           W MAY COINCIDE WITH D.                                      SYM13480
C                                                                       SYM13490
C IND    R  INDICES OF THE PRINCIPAL SUBMATRICES CORRESPONDING TO THE   SYM13500
C           EIGENVALUES IN W, IN TOP-TO-BOTTOM ORDER.                   SYM13510
C                                                                       SYM13520
C BD     R  REFINED BOUNDS FOR THE ERRORS OF THE EIGENVALUES IN W.      SYM13530
C           THESE BOUNDS ARE USUALLY WITHIN THE TOLERANCE DEFINED BY    SYM13540
C           BY EPS1.                                                    SYM13550
C           BD MAY COINCIDE WITH E2.                                    SYM13560
C                                                                       SYM13570
C TYPE   E  .TRUE.   IF THE SMALLEST EIGENVALUES ARE TO BE COMPUTED,    SYM13580
C           .FALSE.  IF THE LARGEST  EIGENVALUES ARE TO BE COMPUTED.    SYM13590
C                                                                       SYM13600
C IDEF   E   1 IF THE MATRIX IS KNOWN TO BE POSITIVE-DEFINITE,          SYM13610
C           -1                              NEGATIVE-DEFINITE, AND      SYM13620
C            0 OTHERWISE.                                               SYM13630
C                                                                       SYM13640
C IERR   R  ZERO     FOR NORMAL RETURN,                                 SYM13650
C           6*N+1    IF  IDEF  IS SET TO 1 AND  TYPE  TO .TRUE.         SYM13660
C                    WHEN THE MATRIX IS NOT POSITIVE DEFINITE, OR       SYM13670
C                    IF  IDEF  IS SET TO -1 AND  TYPE  TO .FALSE.       SYM13680
C                    WHEN THE MATRIX IS NOT NEGATIVE DEFINITE,          SYM13690
C           5*N+K    IF SUCCESSIVE ITERATES TO THE K-TH EIGENVALUE      SYM13700
C                    ARE NOT MONOTONICALLY INCREASING, K CORRESPONDING  SYM13710
C                    TO THE LAST SUCH OCCURRENCE.                       SYM13720
C                                                                       SYM13730
C     NOTE THAT SUBROUTINE TRIDIB IS GENERALLY FASTER AND MORE ACCURATE SYM13740
C     THAN RATQR IF THE EIGENVALUES ARE CLUSTERED.                      SYM13750
C                                                                       SYM13760
C     THIS SUBROUTINE IS BASED ON THE ALGOL PROCEDURE RATQR,            SYM13770
C     NUM. MATH. 11, 264-272(1968) BY REINSCH AND BAUER.                SYM13780
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 257-265(1971).   SYM13790
C                                                                       SYM13800
C     VERSION FOR THE IBM 3090VF DATED NOVEMBER 1987.                   SYM13810
C     ------------------------------------------------------------------SYM13820
      INTEGER     I, J, K, L, JDEF                                      SYM13830
      REAL*8      F, P, Q, R, S, EP, QP, ERR, TOT, DELTA, EPS           SYM13840
      DATA        EPS/Z3410000000000000/                                SYM13850
C                                                                       SYM13860
      CALL XUFLOW(0)                                                    SYM13870
      IERR=0                                                            SYM13880
      JDEF=IDEF                                                         SYM13890
         DO 20 I=1,N                                                    SYM13900
20       W(I)=D(I)                                                      SYM13910
      IF (TYPE) GO TO 40                                                SYM13920
      J=1                                                               SYM13930
      GO TO 400                                                         SYM13940
40    ERR=0D0                                                           SYM13950
      S=0D0                                                             SYM13960
C SEARCH FOR SMALL SUBDIAGONAL ENTRIES                                  SYM13970
C INITIAL SHIFT (LOWER GERSCHGORIN BOUND)                               SYM13980
      TOT=W(1)                                                          SYM13990
      Q=0D0                                                             SYM14000
      J=0                                                               SYM14010
         DO 100 I=1,N                                                   SYM14020
         P=Q                                                            SYM14030
         IF (I .EQ. 1) GO TO 60                                         SYM14040
         IF (P.GT.EPS*(DABS(D(I))+DABS(D(I-1)))) GO TO 80               SYM14050
60       E2(I)=0D0                                                      SYM14060
80       BD(I)=E2(I)                                                    SYM14070
C COUNT OF UNDERFLOW OCCURRENCES FOR THE ELEMENTS OF E2                 SYM14080
         IF (E2(I) .EQ. 0D0) J=J+1                                      SYM14090
         IND(I)=J                                                       SYM14100
         Q=0D0                                                          SYM14110
         IF (I .NE. N) Q=DABS(E(I+1))                                   SYM14120
         TOT=DMIN1(W(I)-P-Q,TOT)                                        SYM14130
100      CONTINUE                                                       SYM14140
      IF (JDEF .EQ. 1 .AND. TOT.LT.0D0) GO TO 140                       SYM14150
         DO 110 I=1,N                                                   SYM14160
110      W(I)=W(I)-TOT                                                  SYM14170
      GO TO 160                                                         SYM14180
140   TOT=0D0                                                           SYM14190
160      DO 360 K=1,M                                                   SYM14200
C QR TRANSFORMATION                                                     SYM14210
  180    TOT=TOT+S                                                      SYM14220
         DELTA=W(N)-S                                                   SYM14230
         I=N                                                            SYM14240
         F=EPS*DABS(TOT)                                                SYM14250
         IF (EPS1.LT.F) EPS1=F                                          SYM14260
         IF (DELTA.GT.EPS1) GO TO 190                                   SYM14270
         IF (DELTA.LT.(-EPS1)) GO TO 999                                SYM14280
         GO TO 300                                                      SYM14290
C SMALL SQUARES OF SUBDIAGONAL ARE REPLACED BY ZERO                     SYM14300
190      IF (K .EQ. N) GO TO 210                                        SYM14310
            DO 200 J=K+1,N                                              SYM14320
            IF (BD(J).LE.(EPS*(W(J)+W(J-1)))**2) BD(J)=0D0              SYM14330
200         CONTINUE                                                    SYM14340
210      F=BD(N)/DELTA                                                  SYM14350
         QP=DELTA+F                                                     SYM14360
         P=1D0                                                          SYM14370
         IF (K .EQ. N) GO TO 260                                        SYM14380
            DO 240 I=N-1,K,-1                                           SYM14390
            Q=W(I)-S-F                                                  SYM14400
            R=Q/QP                                                      SYM14410
            P=P*R+1D0                                                   SYM14420
            EP=F*R                                                      SYM14430
            W(I+1)=QP+EP                                                SYM14440
            DELTA=Q-EP                                                  SYM14450
            IF (DELTA.GT.EPS1) GO TO 220                                SYM14460
            IF (DELTA.LT.(-EPS1)) GO TO 999                             SYM14470
            GO TO 300                                                   SYM14480
220         F=BD(I)/Q                                                   SYM14490
            QP=DELTA+F                                                  SYM14500
            BD(I+1)=QP*EP                                               SYM14510
240         CONTINUE                                                    SYM14520
260      W(K)=QP                                                        SYM14530
         S=QP/P                                                         SYM14540
         IF (TOT+S.GT.TOT) GO TO 180                                    SYM14550
C IRREGULAR END OF ITERATION                                            SYM14560
C DEFLATION OF THE MINIMUM DIAGONAL ELEMENT                             SYM14570
         IERR=5*N+K                                                     SYM14580
         S=0D0                                                          SYM14590
         DELTA=QP                                                       SYM14600
            DO 280 J=K,N                                                SYM14610
            IF (W(J).GT.DELTA) GO TO 280                                SYM14620
            I=J                                                         SYM14630
            DELTA=W(J)                                                  SYM14640
280         CONTINUE                                                    SYM14650
C CONVERGENCE                                                           SYM14660
300      IF (I.LT.N) BD(I+1)=BD(I)*F/QP                                 SYM14670
         L=IND(I)                                                       SYM14680
         IF (I.EQ.K) GO TO 340                                          SYM14690
            DO 320 J=I-1,K,-1                                           SYM14700
            W(J+1)=W(J)-S                                               SYM14710
            BD(J+1)=BD(J)                                               SYM14720
            IND(J+1)=IND(J)                                             SYM14730
320         CONTINUE                                                    SYM14740
340      W(K)=TOT                                                       SYM14750
         ERR=ERR+DABS(DELTA)                                            SYM14760
         BD(K)=ERR                                                      SYM14770
         IND(K)=L                                                       SYM14780
360      CONTINUE                                                       SYM14790
      IF (TYPE) GO TO 1000                                              SYM14800
      F=BD(1)                                                           SYM14810
      E2(1)=2D0                                                         SYM14820
      BD(1)=F                                                           SYM14830
      J=2                                                               SYM14840
C SIGN CHANGE IN W FOR LARGEST ROOTS                                    SYM14850
400      DO 500 I=1,N                                                   SYM14860
500      W(I)=-W(I)                                                     SYM14870
      JDEF=-JDEF                                                        SYM14880
      GO TO (40,1000),J                                                 SYM14890
C ERROR FOR IMPROPER VALUE OF IDEF                                      SYM14900
999   IERR=6*N+1                                                        SYM14910
1000  RETURN                                                            SYM14920
      END                                                               SYM14930
