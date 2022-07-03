@PROCESS DIRECTIVE('"    (')                                            COM13810
C-----------------------------------------------------------------------COM13820
C EIGENSYSTEM OF A COMPLEX UPPER-HESSENBERG MATRIX (QR METHOD), OR      COM13830
C EIGENSYSTEM OF A COMPLEX GENERAL MATRIX REDUCED TO HESSENBERG FORM    COM13840
C     BY SUBROUTINE CORTH.                                              COM13850
C                                                                       COM13860
      SUBROUTINE  COMQR2(LD,N,LOW,IGH,ORTR,ORTI,HR,HI,WR,WI,VR,VI,IERR) COM13870
C                                                                       COM13880
      INTEGER     LD, N, LOW, IGH, IERR                                 COM13890
      REAL*8      HR(LD,N), HI(LD,N), WR(N), WI(N), VR(LD,N), VI(LD,N)  COM13900
      REAL*8      ORTR(IGH), ORTI(IGH)                                  COM13910
C                                                                       COM13920
C LD     E  FIRST DIMENSION ASSIGNED TO THE TWO-DIMENSIONAL ARRAY       COM13930
C           PARAMETERS IN THE CALLING PROGRAM.                          COM13940
C                                                                       COM13950
C N      E  ORDER OF THE MATRIX.                                        COM13960
C                                                                       COM13970
C LOW    E  PARAMETER DETERMINED BY THE BALANCING SUBROUTINE CBAL.      COM13980
C           IF  CBAL WAS NOT USED, SET  LOW = 1.                        COM13990
C                                                                       COM14000
C IGH    E  PARAMETER DETERMINED BY THE BALANCING SUBROUTINE CBAL.      COM14010
C           IF  CBAL WAS NOT USED, SET  IGH = ORDER OF THE MATRIX.      COM14020
C                                                                       COM14030
C ORTR   E  INFORMATION ABOUT THE UNITARY TRANSFORMATIONS PERFORMED BY  COM14040
C           CORTH (IF USED), IN CELLS LOW,...,IGH.                      COM14050
C           IF THE EIGENVECTORS OF THE HESSENBERG MATRIX ARE DESIRED,   COM14060
C           SET THESE ELEMENTS OF ORTR AND ORTI TO ZERO.                COM14070
C                                                                       COM14080
C ORTI   -  ANCILLARY STORAGE                                           COM14090
C                                                                       COM14100
C HR,HI  E  REAL AND IMAGINARY PARTS OF THE COMPLEX HESSENBERG MATRIX.  COM14110
C           THE SUB-HESSENBERG PARTS OF HR AND HI CONTAIN FURTHER       COM14120
C           INFORMATION ABOUT THE TRANSFORMATIONS PERFORMED BY CORTH IF COM14130
C           CORTH PRODUCED THE HESSENBERG MATRIX.                       COM14140
C        R  THE UPPER-HESSENBERG PARTS OF THESE ARRAYS ARE NOT PRESERVEDCOM14150
C                                                                       COM14160
C WR,WI  R  REAL AND IMAGINARY PARTS OF THE EIGENVALUES.                COM14170
C           IF AN ERROR RETURN OCCURS, THE EIGENVALUES OF INDICES       COM14180
C           IERR+1,...,N  SHOULD BE CORRECT.                            COM14190
C                                                                       COM14200
C VR,VI  R  REAL AND IMAGINARY PARTS OF THE MATRIX OF EIGENVECTORS.     COM14210
C           THE EIGENVECTORS ARE NOT NORMALIZED.                        COM14220
C           IF AN ERROR RETURN OCCURS, NO EIGENVECTOR IS COMPUTED.      COM14230
C                                                                       COM14240
C IERR   R  ZERO:    FOR NORMAL RETURN,                                 COM14250
C           NONZERO: NOT ALL THE EIGENVALUES COULD BE OBTAINED IN       COM14260
C                    30*(IGH-LOW+1) ITERATIONS. THE EIGENVALUES OF      COM14270
C                    INDICES IERR+1,...,N SHOULD BE CORRECT.            COM14280
C                                                                       COM14290
C     THIS SUBROUTINE IS BASED ON A UNITARY VARIANT OF THE ALGOL        COM14300
C     PROCEDURE  COMLR2, NUM. MATH. 16, 181-204(1970) BY PETERS         COM14310
C     AND WILKINSON.                                                    COM14320
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 372-395(1971).   COM14330
C     THE VARIANT SUBSTITUTES THE QR ALGORITHM OF FRANCIS (COMP. JOUR.  COM14340
C     4, 332-345(1962)) FOR THE LR ALGORITHM.                           COM14350
C                                                                       COM14360
C SUBPROGRAMS CALLED: KACHEL                                            COM14370
C                                                                       COM14380
C     VERSION FOR THE IBM 3090VF DATED NOVEMBER 1987.                   COM14390
C     ------------------------------------------------------------------COM14400
      INTEGER     I, J, K, KK, M, LIMIT, IT                             COM14410
      REAL*8      X, Y, S, CR, CI, TR, TI, SHR, SHI, U, V, EPS          COM14420
      COMPLEX*16  Z                                                     COM14430
      DATA        EPS/Z3410000000000000/                                COM14440
C                                                                       COM14450
      CALL XUFLOW(0)                                                    COM14460
      IERR = 0                                                          COM14470
C INITIALIZATION OF THE EIGENVECTOR MATRIX                              COM14480
         DO 110 J=1,LOW-1                                               COM14490
            DO 100 I=1,N                                                COM14500
            VR(I,J)=0D0                                                 COM14510
100         VI(I,J)=0D0                                                 COM14520
110      VR(J,J)=1D0                                                    COM14530
      KK=IGH-KACHEL(LD+LD)                                              COM14540
         DO 130 J=MAX0(LOW,IGH-1),N                                     COM14550
            DO 120 I=1,N                                                COM14560
            VR(I,J)=0D0                                                 COM14570
120         VI(I,J)=0D0                                                 COM14580
130      VR(J,J)=1D0                                                    COM14590
C RIGHT MATRIX OF THE SIMILARITY REDUCTION TO HESSENBERG FORM BY CORTH  COM14600
         DO 340 K=IGH-2,LOW,-1                                          COM14610
         S=ORTR(K+1)                                                    COM14620
         IF(S.EQ.0D0) GO TO 320                                         COM14630
         X=1D0/S                                                        COM14640
         VR(K+1,K+1)=S                                                  COM14650
            DO 140 I=K+2,IGH                                            COM14660
            VR(I,K+1)=HR(I,K)                                           COM14670
140         VI(I,K+1)=HI(I,K)                                           COM14680
         IF(K.LT.KK) GO TO 220                                          COM14690
C    PATH FOR SMALL ARRAYS                                              COM14700
C"    ( IGNORE RECRDEPS                                                 COM14710
C"    ( PREFER VECTOR                                                   COM14720
            DO 200 J=K+2,IGH                                            COM14730
            CR=0D0                                                      COM14740
            CI=0D0                                                      COM14750
               DO 160 I=K+1,IGH                                         COM14760
               CR=CR+VR(I,K+1)*VR(I,J)+VI(I,K+1)*VI(I,J)                COM14770
160            CI=CI+VR(I,K+1)*VI(I,J)-VI(I,K+1)*VR(I,J)                COM14780
            CR=X*CR                                                     COM14790
            CI=X*CI                                                     COM14800
               DO 180 I=K+1,IGH                                         COM14810
               VR(I,J)=VR(I,J)+CR*VR(I,K+1)-CI*VI(I,K+1)                COM14820
180            VI(I,J)=VI(I,J)+CR*VI(I,K+1)+CI*VR(I,K+1)                COM14830
200         CONTINUE                                                    COM14840
         GO TO 320                                                      COM14850
C    PATH FOR LARGE ARRAYS                                              COM14860
220         DO 300 J=K+2,IGH                                            COM14870
            CR=0D0                                                      COM14880
            CI=0D0                                                      COM14890
               DO 240 I=K+1,IGH                                         COM14900
               CR=CR+VR(I,K+1)*VR(I,J)+VI(I,K+1)*VI(I,J)                COM14910
240            CI=CI+VR(I,K+1)*VI(I,J)-VI(I,K+1)*VR(I,J)                COM14920
            CR=X*CR                                                     COM14930
            CI=X*CI                                                     COM14940
               DO 260 I=K+1,IGH                                         COM14950
               VR(I,J)=VR(I,J)+CR*VR(I,K+1)-CI*VI(I,K+1)                COM14960
260            VI(I,J)=VI(I,J)+CR*VI(I,K+1)+CI*VR(I,K+1)                COM14970
300         CONTINUE                                                    COM14980
320      VR(K+1,K+1)=1D0+S                                              COM14990
         VI(K+1,K+1)=0D0                                                COM15000
            DO 330 I=1,N                                                COM15010
            VR(I,K)=0D0                                                 COM15020
330         VI(I,K)=0D0                                                 COM15030
         VR(K,K)=1D0                                                    COM15040
340      CONTINUE                                                       COM15050
C SIMILARITY TRANSFORMATIONS TO MAKE THE SUBDIAGONAL ELEMENTS REAL      COM15060
      CR=1D0                                                            COM15070
      CI=0D0                                                            COM15080
C   COLUMNS LOW,...,IGH                                                 COM15090
         DO 400 J=LOW+1,IGH                                             COM15100
         X=HR(J,J-1)                                                    COM15110
         Y=HI(J,J-1)                                                    COM15120
         TR=CR*X-CI*Y                                                   COM15130
         TI=CR*Y+CI*X                                                   COM15140
         WI(J)=0D0                                                      COM15150
         HR(J,J-1)=TR                                                   COM15160
         IF(TI.NE.0D0) GO TO 350                                        COM15170
            CR=1D0                                                      COM15180
            CI=0D0                                                      COM15190
            GO TO 370                                                   COM15200
350      X=DMAX1(DABS(TR),DABS(TI))                                     COM15210
         CR=TR/X                                                        COM15220
         CI=TI/X                                                        COM15230
         Y=DSQRT(CR*CR+CI*CI)                                           COM15240
         HR(J,J-1)=X*Y                                                  COM15250
         HI(J,J-1)=0D0                                                  COM15260
         CR=CR/Y                                                        COM15270
         CI=CI/Y                                                        COM15280
C     UPDATE OF THE RIGHT MATRIX OF THE REDUCTION TO HESSENBERG FORM    COM15290
            DO 355 I=LOW,IGH                                            COM15300
            X=VR(I,J)                                                   COM15310
            Y=VI(I,J)                                                   COM15320
            VR(I,J)=CR*X-CI*Y                                           COM15330
355         VI(I,J)=CR*Y+CI*X                                           COM15340
C     RIGHT TRANSFORMATION                                              COM15350
            DO 360 I=1,J-1                                              COM15360
            X=HR(I,J)                                                   COM15370
            Y=HI(I,J)                                                   COM15380
            HR(I,J)=CR*X-CI*Y                                           COM15390
360         HI(I,J)=CR*Y+CI*X                                           COM15400
C     LEFT TRANSFORMATION                                               COM15410
370      WR(J)=CR                                                       COM15420
         WI(J)=CI                                                       COM15430
            DO 380 I=LOW+1,J-1                                          COM15440
            IF(WI(I).EQ.0D0) GO TO 380                                  COM15450
            X=HR(I,J)                                                   COM15460
            Y=HI(I,J)                                                   COM15470
            HR(I,J)=WR(I)*X+WI(I)*Y                                     COM15480
            HI(I,J)=WR(I)*Y-WI(I)*X                                     COM15490
380         CONTINUE                                                    COM15500
400      CONTINUE                                                       COM15510
C   LEFT TRANSFORMATION OF COLUMNS IGH+1,...,N                          COM15520
         DO 440 J=IGH+1,N                                               COM15530
            DO 420 I=LOW+1,IGH                                          COM15540
            IF(WI(I).EQ.0D0) GO TO 420                                  COM15550
            X=HR(I,J)                                                   COM15560
            Y=HI(I,J)                                                   COM15570
            HR(I,J)=WR(I)*X+WI(I)*Y                                     COM15580
            HI(I,J)=WR(I)*Y-WI(I)*X                                     COM15590
420         CONTINUE                                                    COM15600
440      CONTINUE                                                       COM15610
C ROOTS ISOLATED BY CBAL                                                COM15620
         DO 460 I=1,LOW-1                                               COM15630
         WR(I)=HR(I,I)                                                  COM15640
460      WI(I)=HI(I,I)                                                  COM15650
         DO 480 I=IGH+1,N                                               COM15660
         WR(I)=HR(I,I)                                                  COM15670
480      WI(I)=HI(I,I)                                                  COM15680
C                                                                       COM15690
C COMPUTATION OF THE EIGENVALUES                                        COM15700
      M=IGH                                                             COM15710
      SHR=0D0                                                           COM15720
      SHI=0D0                                                           COM15730
      LIMIT=30*(IGH-LOW+1)                                              COM15740
500   IF(M.LT.LOW) GO TO 860                                            COM15750
      M1=M-1                                                            COM15760
C MORE ROOTS                                                            COM15770
      IT=0                                                              COM15780
C SEARCH FOR A SMALL SUBDIAGONAL ELEMENT                                COM15790
520      DO 540 L=M,LOW+1,-1                                            COM15800
         TR=DABS(HR(L-1,L-1))+DABS(HI(L-1,L-1))                         COM15810
     $     +DABS(HR(L,L))    +DABS(HI(L,L))                             COM15820
         TI=TR+DABS(HR(L,L-1))                                          COM15830
         IF(TR.EQ.TI) GO TO 560                                         COM15840
540      CONTINUE                                                       COM15850
      L=LOW                                                             COM15860
560   IF(L.EQ.M) GO TO 840                                              COM15870
      IF(LIMIT.EQ.0) GO TO 9999                                         COM15880
      IF(IT.NE.10 .AND. IT.NE.20) GO TO 580                             COM15890
C EXCEPTIONAL SHIFT                                                     COM15900
         CR=DABS(HR(M,M1))+DABS(HR(M1,M-2))                             COM15910
         CI=0D0                                                         COM15920
         GO TO 600                                                      COM15930
C REGULAR SHIFT FROM THE BOTTOM PRINCIPAL SUBMATRIX OF ORDER 2          COM15940
580   CR=0.5D0*(HR(M1,M1)-HR(M,M))                                      COM15950
      CI=0.5D0*(HI(M1,M1)-HI(M,M))                                      COM15960
      TR=HR(M1,M)*HR(M,M1)                                              COM15970
      TI=HI(M1,M)*HR(M,M1)                                              COM15980
      Z=CDSQRT(DCMPLX(TR+CR*CR-CI*CI,TI+(CR+CR)*CI))                    COM15990
      IF(CR*DREAL(Z)+CI*DIMAG(Z).LT.0D0) Z=-Z                           COM16000
      CR=DREAL(Z)+CR                                                    COM16010
      CI=DIMAG(Z)+CI                                                    COM16020
      IF(DABS(CR).LE.DABS(CI)) GO TO 590                                COM16030
         S=CI/CR                                                        COM16040
         X=CR+S*CI                                                      COM16050
         CR=HR(M,M)-(TR+S*TI)/X                                         COM16060
         CI=HI(M,M)-(TI-S*TR)/X                                         COM16070
         GO TO 600                                                      COM16080
590   S=CR/CI                                                           COM16090
      X=S*CR+CI                                                         COM16100
      CR=HR(M,M)-(S*TR+TI)/X                                            COM16110
      CI=HI(M,M)-(S*TI-TR)/X                                            COM16120
600      DO 620 I=LOW,M                                                 COM16130
         HR(I,I)=HR(I,I)-CR                                             COM16140
620      HI(I,I)=HI(I,I)-CI                                             COM16150
C ACCUMULATED SHIFTS                                                    COM16160
      SHR=SHR+CR                                                        COM16170
      SHI=SHI+CI                                                        COM16180
C ITERATION COUNTS                                                      COM16190
      LIMIT=LIMIT-1                                                     COM16200
      IT=IT+1                                                           COM16210
C QR SWEEP                                                              COM16220
         DO 660 J=L,M1                                                  COM16230
C     PREVIOUS LEFT TRANSFORMATIONS APPLIED TO COLUMN J                 COM16240
            DO 640 I=L,J-1                                              COM16250
            TR=HR(I,J)                                                  COM16260
            TI=HI(I,J)                                                  COM16270
            X =HR(I+1,J)                                                COM16280
            Y =HI(I+1,J)                                                COM16290
            HR(I,J)  =WR(I)*TR+WI(I)*TI+ORTI(I)*X                       COM16300
            HI(I,J)  =WR(I)*TI-WI(I)*TR+ORTI(I)*Y                       COM16310
            HR(I+1,J)=WR(I)*X-WI(I)*Y-ORTI(I)*TR                        COM16320
640         HI(I+1,J)=WR(I)*Y+WI(I)*X-ORTI(I)*TI                        COM16330
C     ANNIHILATION OF THE SUBDIAGONAL ELEMENT IN COLUMN J               COM16340
         CR=HR(J,J)                                                     COM16350
         CI=HI(J,J)                                                     COM16360
         S =HR(J+1,J)                                                   COM16370
         X=DMAX1(DABS(CR),DABS(CI),DABS(S))                             COM16380
         CR=CR/X                                                        COM16390
         CI=CI/X                                                        COM16400
         S =S /X                                                        COM16410
         Y=DSQRT(CR*CR+CI*CI+S*S)                                       COM16420
         ORTI(J)=S/Y                                                    COM16430
         HR(J,J)=X*Y                                                    COM16440
         HI(J,J)=0D0                                                    COM16450
         WR(J)=CR/Y                                                     COM16460
         WI(J)=CI/Y                                                     COM16470
         IF(J.EQ.L) GO TO 660                                           COM16480
C     PREVIOUS RIGHT TRANSFORMATION APPLIED TO COLUMNS J-1 AND J        COM16490
         CR=WR(J-1)                                                     COM16500
         CI=WI(J-1)                                                     COM16510
         S=ORTI(J-1)                                                    COM16520
            DO 650 I=1,J-1                                              COM16530
            TR=HR(I,J-1)                                                COM16540
            TI=HI(I,J-1)                                                COM16550
            X=HR(I,J)                                                   COM16560
            Y=HI(I,J)                                                   COM16570
            HR(I,J-1)=CR*TR-CI*TI+S*X                                   COM16580
            HI(I,J-1)=CR*TI+CI*TR+S*Y                                   COM16590
            HR(I,J  )=CR*X+CI*Y-S*TR                                    COM16600
650         HI(I,J  )=CR*Y-CI*X-S*TI                                    COM16610
         TR=HR(J,J)                                                     COM16620
         HR(J,J-1)=S*TR                                                 COM16630
         HR(J,J)=CR*TR                                                  COM16640
         HI(J,J)=-CI*TR                                                 COM16650
660      CONTINUE                                                       COM16660
C     COLUMN M                                                          COM16670
         DO 680 I=L,M1                                                  COM16680
         TR=HR(I,M)                                                     COM16690
         TI=HI(I,M)                                                     COM16700
         X =HR(I+1,M)                                                   COM16710
         Y =HI(I+1,M)                                                   COM16720
         HR(I,M)  =WR(I)*TR+WI(I)*TI+ORTI(I)*X                          COM16730
         HI(I,M)  =WR(I)*TI-WI(I)*TR+ORTI(I)*Y                          COM16740
         HR(I+1,M)=WR(I)*X-WI(I)*Y-ORTI(I)*TR                           COM16750
680      HI(I+1,M)=WR(I)*Y+WI(I)*X-ORTI(I)*TI                           COM16760
C     THE M-TH DIAGONAL ELEMENT IS MADE REAL                            COM16770
      CI=HI(M,M)                                                        COM16780
      IF(CI.EQ.0D0) GO TO 690                                           COM16790
      CR=HR(M,M)                                                        COM16800
      X=DMAX1(DABS(CR),DABS(CI))                                        COM16810
      CR=CR/X                                                           COM16820
      CI=CI/X                                                           COM16830
      Y=DSQRT(CR*CR+CI*CI)                                              COM16840
      HR(M,M)=X*Y                                                       COM16850
      CR=CR/Y                                                           COM16860
      CI=CI/Y                                                           COM16870
      WR(M)=CR                                                          COM16880
690   WI(M)=CI                                                          COM16890
C LAST RIGHT TRANSFORMATION (COLUMNS M-1 AND M)                         COM16900
      CR=WR(M1)                                                         COM16910
      CI=WI(M1)                                                         COM16920
      S=ORTI(M1)                                                        COM16930
         DO 700 I=1,M1                                                  COM16940
         TR=HR(I,M1)                                                    COM16950
         TI=HI(I,M1)                                                    COM16960
         X=HR(I,M)                                                      COM16970
         Y=HI(I,M)                                                      COM16980
         HR(I,M1)=CR*TR-CI*TI+S*X                                       COM16990
         HI(I,M1)=CR*TI+CI*TR+S*Y                                       COM17000
         HR(I,M  )=CR*X+CI*Y-S*TR                                       COM17010
700      HI(I,M  )=CR*Y-CI*X-S*TI                                       COM17020
      TR=HR(M,M)                                                        COM17030
      HR(M,M1)=S*TR                                                     COM17040
      HR(M,M)=CR*TR                                                     COM17050
      HI(M,M)=-CI*TR                                                    COM17060
      CI=WI(M)                                                          COM17070
      IF(CI.EQ.0D0) GO TO 740                                           COM17080
      CR=WR(M)                                                          COM17090
         DO 720 I=1,M                                                   COM17100
         TR=HR(I,M)                                                     COM17110
         TI=HI(I,M)                                                     COM17120
         HR(I,M)=CR*TR-CI*TI                                            COM17130
720      HI(I,M)=CR*TI+CI*TR                                            COM17140
C     COMPLETION OF THE LEFT TRANSFORMATIONS                            COM17150
740      DO 780 J=M+1,N                                                 COM17160
            DO 760 I=L,M1                                               COM17170
            TR=HR(I,J)                                                  COM17180
            TI=HI(I,J)                                                  COM17190
            X =HR(I+1,J)                                                COM17200
            Y =HI(I+1,J)                                                COM17210
            HR(I,J)  =WR(I)*TR+WI(I)*TI+ORTI(I)*X                       COM17220
            HI(I,J)  =WR(I)*TI-WI(I)*TR+ORTI(I)*Y                       COM17230
            HR(I+1,J)=WR(I)*X-WI(I)*Y-ORTI(I)*TR                        COM17240
760         HI(I+1,J)=WR(I)*Y+WI(I)*X-ORTI(I)*TI                        COM17250
         IF(CI.EQ.0D0) GO TO 780                                        COM17260
         X=HR(M,J)                                                      COM17270
         Y=HI(M,J)                                                      COM17280
         HR(M,J)=CR*X+CI*Y                                              COM17290
         HI(M,J)=CR*Y-CI*X                                              COM17300
780      CONTINUE                                                       COM17310
C   UPDATE OF THE RIGHT MATRIX OF THE REDUCTION TO TRIANGULAR FORM      COM17320
790      DO 820 J=L,M1                                                  COM17330
         CR=WR(J)                                                       COM17340
         CI=WI(J)                                                       COM17350
         S=ORTI(J)                                                      COM17360
            DO 800 I=LOW,IGH                                            COM17370
            TR=VR(I,J)                                                  COM17380
            TI=VI(I,J)                                                  COM17390
            X=VR(I,J+1)                                                 COM17400
            Y=VI(I,J+1)                                                 COM17410
            VR(I,J  )=CR*TR-CI*TI+S*X                                   COM17420
            VI(I,J  )=CR*TI+CI*TR+S*Y                                   COM17430
            VR(I,J+1)=CR*X+CI*Y-S*TR                                    COM17440
800         VI(I,J+1)=CR*Y-CI*X-S*TI                                    COM17450
820      CONTINUE                                                       COM17460
      CI=WI(M)                                                          COM17470
      IF(CI.EQ.0D0) GO TO 520                                           COM17480
      CR=WR(M)                                                          COM17490
         DO 830 I=LOW,IGH                                               COM17500
         TR=VR(I,M)                                                     COM17510
         TI=VI(I,M)                                                     COM17520
         VR(I,M)=CR*TR-CI*TI                                            COM17530
830      VI(I,M)=CR*TI+CI*TR                                            COM17540
      GO TO 520                                                         COM17550
C ROOT                                                                  COM17560
840   WR(M)=HR(M,M)+SHR                                                 COM17570
      HR(M,M)=WR(M)                                                     COM17580
      WI(M)=HI(M,M)+SHI                                                 COM17590
      HI(M,M)=WI(M)                                                     COM17600
      M=M1                                                              COM17610
      GO TO 500                                                         COM17620
C                                                                       COM17630
C EIGENVECTORS OF THE UPPER-TRIANGULAR MATRIX PRODUCED BY QR            COM17640
860   S=0D0                                                             COM17650
         DO 900 J=1,N                                                   COM17660
            DO 880 I=1,J                                                COM17670
            S=DMAX1(DABS(HR(I,J))+DABS(HI(I,J)),S)                      COM17680
880         CONTINUE                                                    COM17690
900      CONTINUE                                                       COM17700
      IF(S.EQ.0D0) GO TO 10000                                          COM17710
      S=S*EPS                                                           COM17720
         DO 1040 K=N,2,-1                                               COM17730
         HR(K,K)=1D0                                                    COM17740
         HI(K,K)=0D0                                                    COM17750
         X=WR(K)                                                        COM17760
         Y=WI(K)                                                        COM17770
            DO 1020 J=K-1,1,-1                                          COM17780
            IF(J.EQ.K-1) GO TO 940                                      COM17790
            CR=HR(J+1,K)                                                COM17800
            CI=HI(J+1,K)                                                COM17810
               DO 920 I=1,J                                             COM17820
               TR=HR(I,J+1)                                             COM17830
               TI=HI(I,J+1)                                             COM17840
               HR(I,K)=HR(I,K)+CR*TR-CI*TI                              COM17850
920            HI(I,K)=HI(I,K)+CR*TI+CI*TR                              COM17860
940         CR=X-WR(J)                                                  COM17870
            CI=Y-WI(J)                                                  COM17880
            IF(DABS(CR)+DABS(CI).NE.0D0) GO TO 960                      COM17890
               HR(J,K)=HR(J,K)/S                                        COM17900
               HI(J,K)=HI(J,K)/S                                        COM17910
               GO TO 980                                                COM17920
960         TR=HR(J,K)                                                  COM17930
            TI=HI(J,K)                                                  COM17940
            IF(DABS(CR).LE.DABS(CI)) GO TO 970                          COM17950
               U=CI/CR                                                  COM17960
               V=CR+U*CI                                                COM17970
               HR(J,K)=(TR+U*TI)/V                                      COM17980
               HI(J,K)=(TI-U*TR)/V                                      COM17990
               GO TO 980                                                COM18000
970         U=CR/CI                                                     COM18010
            V=CI+U*CR                                                   COM18020
            HR(J,K)=(U*TR+TI)/V                                         COM18030
            HI(J,K)=(U*TI-TR)/V                                         COM18040
C PROTECTION AGAINST OVERFLOW OF FLOATING-POINT REPRESENTATION          COM18050
980         U=DABS(HR(J,K))+DABS(HI(J,K))                               COM18060
            IF(U.EQ.0D0) GO TO 1020                                     COM18070
            V=U+1D0/U                                                   COM18080
            IF(V.GT.U) GO TO 1020                                       COM18090
               DO 1000 I=1,K                                            COM18100
               HR(I,K)=HR(I,K)/U                                        COM18110
1000           HI(I,K)=HI(I,K)/U                                        COM18120
1020        CONTINUE                                                    COM18130
1040     CONTINUE                                                       COM18140
      HR(1,1)=1D0                                                       COM18150
      HI(1,1)=0D0                                                       COM18160
C                                                                       COM18170
C EIGENVECTORS OF THE ORIGINAL MATRIX                                   COM18180
C   ROOTS ISOLATED BY CBAL:  EIGENVECTORS OF THE TRIANGULAR MATRIX      COM18190
      IF(LOW.EQ.1) GO TO 1100                                           COM18200
         DO 1080 J=1,N                                                  COM18210
            DO 1060 I=1,MIN0(J,LOW-1)                                   COM18220
            VR(I,J)=HR(I,J)                                             COM18230
1060        VI(I,J)=HI(I,J)                                             COM18240
1080     CONTINUE                                                       COM18250
1100     DO 1140 J=IGH+1,N                                              COM18260
            DO 1120 I=IGH+1,J                                           COM18270
            VR(I,J)=HR(I,J)                                             COM18280
1120        VI(I,J)=HI(I,J)                                             COM18290
1140     CONTINUE                                                       COM18300
C   OTHER ROOTS:  TRANSFORMATION BY THE RIGHT MATRIX OF THE REDUCTION   COM18310
C                 TO TRIANGULAR FORM                                    COM18320
         DO 1200 J=N,LOW+1,-1                                           COM18330
         M=MIN0(J,IGH)                                                  COM18340
            DO 1180 I=LOW,IGH                                           COM18350
            TR=0D0                                                      COM18360
            TI=0D0                                                      COM18370
               DO 1160 K=LOW,M                                          COM18380
               TR=TR+VR(I,K)*HR(K,J)-VI(I,K)*HI(K,J)                    COM18390
1160           TI=TI+VR(I,K)*HI(K,J)+VI(I,K)*HR(K,J)                    COM18400
            VR(I,J)=TR                                                  COM18410
1180        VI(I,J)=TI                                                  COM18420
1200     CONTINUE                                                       COM18430
      GO TO 10000                                                       COM18440
C                                                                       COM18450
9999  IERR=M                                                            COM18460
10000 RETURN                                                            COM18470
      END                                                               COM18480
