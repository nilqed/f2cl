@PROCESS DIRECTIVE('"    (')                                            COM11190
C-----------------------------------------------------------------------COM11200
C EIGENVALUES OF A COMPLEX UPPER-HESSENBERG MATRIX (QR METHOD)          COM11210
C                                                                       COM11220
      SUBROUTINE  COMQR (LD,N,LOW,IGH,HR,HI,WR,WI,IERR)                 COM11230
C                                                                       COM11240
      INTEGER     LD, N, LOW, IGH, IERR                                 COM11250
      REAL*8      HR(LD,N),HI(LD,N), WR(N),WI(N)                        COM11260
C                                                                       COM11270
C LD     E  FIRST DIMENSION ASSIGNED TO THE TWO-DIMENSIONAL ARRAY       COM11280
C           PARAMETERS IN THE CALLING PROGRAM.                          COM11290
C                                                                       COM11300
C N      E  ORDER OF THE MATRIX.                                        COM11310
C                                                                       COM11320
C LOW    E  PARAMETER DETERMINED BY THE BALANCING SUBROUTINE CBAL.      COM11330
C           IF  CBAL WAS NOT USED, SET  LOW = 1.                        COM11340
C                                                                       COM11350
C IGH    E  PARAMETER DETERMINED BY THE BALANCING SUBROUTINE CBAL.      COM11360
C           IF  CBAL WAS NOT USED, SET  IGH = ORDER OF THE MATRIX.      COM11370
C                                                                       COM11380
C HR,HI  E  REAL AND IMAGINARY PARTS OF THE COMPLEX HESSENBERG MATRIX.  COM11390
C           THE SUB-HESSENBERG PARTS OF HR AND HI CONTAIN FURTHER       COM11400
C           INFORMATION ABOUT THE TRANSFORMATIONS PERFORMED BY CORTH IF COM11410
C           CORTH PRODUCED THE HESSENBERG MATRIX.                       COM11420
C        R  THE UPPER-HESSENBERG PARTS OF THESE ARRAYS ARE NOT PRESERVEDCOM11430
C                                                                       COM11440
C WR,WI  R  REAL AND IMAGINARY PARTS OF THE EIGENVALUES.                COM11450
C           IF AN ERROR RETURN OCCURS, THE EIGENVALUES OF INDICES       COM11460
C           IERR+1,...,N  SHOULD BE CORRECT.                            COM11470
C                                                                       COM11480
C IERR   R  ZERO:    FOR NORMAL RETURN,                                 COM11490
C           NONZERO: NOT ALL THE EIGENVALUES COULD BE OBTAINED IN       COM11500
C                    30*(IGH-LOW+1) ITERATIONS. THE EIGENVALUES OF      COM11510
C                    INDICES IERR+1,...,N SHOULD BE CORRECT.            COM11520
C                                                                       COM11530
C     THIS SUBROUTINE IS BASED ON A UNITARY VARIANT OF THE ALGOL        COM11540
C     PROCEDURE  COMLR, NUM. MATH. 12, 369-376(1968) BY MARTIN          COM11550
C     AND WILKINSON.                                                    COM11560
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 396-403(1971).   COM11570
C     THE VARIANT SUBSTITUTES THE QR ALGORITHM OF FRANCIS (COMP. JOUR.  COM11580
C     4, 332-345(1962)) FOR THE LR ALGORITHM.                           COM11590
C                                                                       COM11600
C     VERSION FOR THE IBM 3090VF DATED NOVEMBER 1987.                   COM11610
C     ------------------------------------------------------------------COM11620
      INTEGER     I, J, M, M1, LIMIT, IT                                COM11630
      REAL*8      X, Y, S, CR, CI, TR, TI, SHR, SHI                     COM11640
      COMPLEX*16  Z                                                     COM11650
C                                                                       COM11660
      CALL XUFLOW(0)                                                    COM11670
      IERR = 0                                                          COM11680
C SIMILARITY TRANSFORMATIONS TO MAKE THE SUBDIAGONAL ELEMENTS REAL      COM11690
      CR=1D0                                                            COM11700
      CI=0D0                                                            COM11710
C   COLUMNS LOW,...,IGH                                                 COM11720
         DO 400 J=LOW+1,IGH                                             COM11730
         X=HR(J,J-1)                                                    COM11740
         Y=HI(J,J-1)                                                    COM11750
         TR=CR*X-CI*Y                                                   COM11760
         TI=CR*Y+CI*X                                                   COM11770
         WI(J)=0D0                                                      COM11780
         HR(J,J-1)=TR                                                   COM11790
         IF(TI.NE.0D0) GO TO 350                                        COM11800
            CR=1D0                                                      COM11810
            CI=0D0                                                      COM11820
            GO TO 370                                                   COM11830
350      X=DMAX1(DABS(TR),DABS(TI))                                     COM11840
         CR=TR/X                                                        COM11850
         CI=TI/X                                                        COM11860
         Y=DSQRT(CR*CR+CI*CI)                                           COM11870
         HR(J,J-1)=X*Y                                                  COM11880
         CR=CR/Y                                                        COM11890
         CI=CI/Y                                                        COM11900
C     RIGHT TRANSFORMATION                                              COM11910
            DO 360 I=1,J-1                                              COM11920
            X=HR(I,J)                                                   COM11930
            Y=HI(I,J)                                                   COM11940
            HR(I,J)=CR*X-CI*Y                                           COM11950
360         HI(I,J)=CR*Y+CI*X                                           COM11960
C     LEFT TRANSFORMATION                                               COM11970
370      WR(J)=CR                                                       COM11980
         WI(J)=CI                                                       COM11990
            DO 380 I=LOW+1,J-1                                          COM12000
            IF(WI(I).EQ.0D0) GO TO 380                                  COM12010
            X=HR(I,J)                                                   COM12020
            Y=HI(I,J)                                                   COM12030
            HR(I,J)=WR(I)*X+WI(I)*Y                                     COM12040
            HI(I,J)=WR(I)*Y-WI(I)*X                                     COM12050
380         CONTINUE                                                    COM12060
C THE SUBDIAGONAL CELLS OF HI ARE USED TO HOLD THE ELEMENTS OF COLUMN 1 COM12070
         HI(J,J-1)=HI(J,1)                                              COM12080
400      CONTINUE                                                       COM12090
C ROOTS ISOLATED BY CBAL                                                COM12100
         DO 460 I=1,LOW-1                                               COM12110
         WR(I)=HR(I,I)                                                  COM12120
460      WI(I)=HI(I,I)                                                  COM12130
         DO 480 I=IGH+1,N                                               COM12140
         WR(I)=HR(I,I)                                                  COM12150
480      WI(I)=HI(I,I)                                                  COM12160
C                                                                       COM12170
C COMPUTATION OF THE EIGENVALUES                                        COM12180
      M=IGH                                                             COM12190
      SHR=0D0                                                           COM12200
      SHI=0D0                                                           COM12210
      LIMIT=30*(IGH-LOW+1)                                              COM12220
500   IF(M.LT.LOW) GO TO 1000                                           COM12230
      M1=M-1                                                            COM12240
C MORE ROOTS                                                            COM12250
      IT=0                                                              COM12260
C SEARCH FOR A SMALL SUBDIAGONAL ELEMENT                                COM12270
520      DO 540 L=M,LOW+1,-1                                            COM12280
         TR=DABS(HR(L-1,L-1))+DABS(HI(L-1,L-1))                         COM12290
     *     +DABS(HR(L,L))    +DABS(HI(L,L))                             COM12300
         TI=TR+DABS(HR(L,L-1))                                          COM12310
         IF(TR.EQ.TI) GO TO 560                                         COM12320
540      CONTINUE                                                       COM12330
      L=LOW                                                             COM12340
560   IF(L.EQ.M) GO TO 840                                              COM12350
      IF(LIMIT.EQ.0) GO TO 9999                                         COM12360
      IF(IT.NE.10 .AND. IT.NE.20) GO TO 580                             COM12370
C EXCEPTIONAL SHIFT                                                     COM12380
         CR=DABS(HR(M,M1))+DABS(HR(M1,M-2))                             COM12390
         CI=0D0                                                         COM12400
         GO TO 600                                                      COM12410
C REGULAR SHIFT FROM THE BOTTOM PRINCIPAL SUBMATRIX OF ORDER 2          COM12420
580   CR=0.5D0*(HR(M1,M1)-HR(M,M))                                      COM12430
      CI=0.5D0*(HI(M1,M1)-HI(M,M))                                      COM12440
      TR=HR(M1,M)*HR(M,M1)                                              COM12450
      TI=HI(M1,M)*HR(M,M1)                                              COM12460
      Z=CDSQRT(DCMPLX(TR+CR*CR-CI*CI,TI+(CR+CR)*CI))                    COM12470
      IF(CR*DREAL(Z)+CI*DIMAG(Z).LT.0D0) Z=-Z                           COM12480
      CR=DREAL(Z)+CR                                                    COM12490
      CI=DIMAG(Z)+CI                                                    COM12500
      IF(DABS(CR).LE.DABS(CI)) GO TO 590                                COM12510
         S=CI/CR                                                        COM12520
         X=CR+S*CI                                                      COM12530
         CR=HR(M,M)-(TR+S*TI)/X                                         COM12540
         CI=HI(M,M)-(TI-S*TR)/X                                         COM12550
         GO TO 600                                                      COM12560
590   S=CR/CI                                                           COM12570
      X=S*CR+CI                                                         COM12580
      CR=HR(M,M)-(S*TR+TI)/X                                            COM12590
      CI=HI(M,M)-(S*TI-TR)/X                                            COM12600
600      DO 620 I=LOW,M                                                 COM12610
         HR(I,I)=HR(I,I)-CR                                             COM12620
620      HI(I,I)=HI(I,I)-CI                                             COM12630
C ACCUMULATED SHIFTS                                                    COM12640
      SHR=SHR+CR                                                        COM12650
      SHI=SHI+CI                                                        COM12660
C ITERATION COUNTS                                                      COM12670
      LIMIT=LIMIT-1                                                     COM12680
      IT=IT+1                                                           COM12690
C QR SWEEP                                                              COM12700
         DO 660 J=L,M1                                                  COM12710
C     PREVIOUS LEFT TRANSFORMATIONS APPLIED TO COLUMN J                 COM12720
            DO 640 I=L,J-1                                              COM12730
            TR=HR(I,J)                                                  COM12740
            TI=HI(I,J)                                                  COM12750
            X =HR(I+1,J)                                                COM12760
            Y =HI(I+1,J)                                                COM12770
            S=HI(I+1,1)                                                 COM12780
            HR(I,J)  =WR(I)*TR+WI(I)*TI+S*X                             COM12790
            HI(I,J)  =WR(I)*TI-WI(I)*TR+S*Y                             COM12800
            HR(I+1,J)=WR(I)*X-WI(I)*Y-S*TR                              COM12810
640         HI(I+1,J)=WR(I)*Y+WI(I)*X-S*TI                              COM12820
C     ANNIHILATION OF THE SUBDIAGONAL ELEMENT IN COLUMN J               COM12830
         CR=HR(J,J)                                                     COM12840
         CI=HI(J,J)                                                     COM12850
         S =HR(J+1,J)                                                   COM12860
         X=DMAX1(DABS(CR),DABS(CI),DABS(S))                             COM12870
         CR=CR/X                                                        COM12880
         CI=CI/X                                                        COM12890
         S =S /X                                                        COM12900
         Y=DSQRT(CR*CR+CI*CI+S*S)                                       COM12910
         HI(J+1,1)=S/Y                                                  COM12920
         HR(J,J)=X*Y                                                    COM12930
         HI(J,J)=0D0                                                    COM12940
         WR(J)=CR/Y                                                     COM12950
         WI(J)=CI/Y                                                     COM12960
         IF(J.EQ.L) GO TO 660                                           COM12970
C     PREVIOUS RIGHT TRANSFORMATION APPLIED TO COLUMNS J-1 AND J        COM12980
         CR=WR(J-1)                                                     COM12990
         CI=WI(J-1)                                                     COM13000
         S=HI(J,1)                                                      COM13010
            DO 650 I=L,J-1                                              COM13020
            TR=HR(I,J-1)                                                COM13030
            TI=HI(I,J-1)                                                COM13040
            X=HR(I,J)                                                   COM13050
            Y=HI(I,J)                                                   COM13060
            HR(I,J-1)=CR*TR-CI*TI+S*X                                   COM13070
            HI(I,J-1)=CR*TI+CI*TR+S*Y                                   COM13080
            HR(I,J  )=CR*X+CI*Y-S*TR                                    COM13090
650         HI(I,J  )=CR*Y-CI*X-S*TI                                    COM13100
         TR=HR(J,J)                                                     COM13110
         HR(J,J-1)=S*TR                                                 COM13120
         HR(J,J)=CR*TR                                                  COM13130
         HI(J,J)=-CI*TR                                                 COM13140
660      CONTINUE                                                       COM13150
C     COLUMN M                                                          COM13160
         DO 680 I=L,M1                                                  COM13170
         TR=HR(I,M)                                                     COM13180
         TI=HI(I,M)                                                     COM13190
         X =HR(I+1,M)                                                   COM13200
         Y =HI(I+1,M)                                                   COM13210
         S=HI(I+1,1)                                                    COM13220
         HR(I,M)  =WR(I)*TR+WI(I)*TI+S*X                                COM13230
         HI(I,M)  =WR(I)*TI-WI(I)*TR+S*Y                                COM13240
         HR(I+1,M)=WR(I)*X-WI(I)*Y-S*TR                                 COM13250
680      HI(I+1,M)=WR(I)*Y+WI(I)*X-S*TI                                 COM13260
C     THE M-TH DIAGONAL ELEMENT IS MADE REAL                            COM13270
      CI=HI(M,M)                                                        COM13280
      IF(CI.EQ.0D0) GO TO 690                                           COM13290
      CR=HR(M,M)                                                        COM13300
      X=DMAX1(DABS(CR),DABS(CI))                                        COM13310
      CR=CR/X                                                           COM13320
      CI=CI/X                                                           COM13330
      Y=DSQRT(CR*CR+CI*CI)                                              COM13340
      HR(M,M)=X*Y                                                       COM13350
      CR=CR/Y                                                           COM13360
      CI=CI/Y                                                           COM13370
      WR(M)=CR                                                          COM13380
690   WI(M)=CI                                                          COM13390
C LAST RIGHT TRANSFORMATION (COLUMNS M-1 AND M)                         COM13400
      CR=WR(M1)                                                         COM13410
      CI=WI(M1)                                                         COM13420
      S=HI(M,1)                                                         COM13430
         DO 700 I=L,M1                                                  COM13440
         TR=HR(I,M1)                                                    COM13450
         TI=HI(I,M1)                                                    COM13460
         X=HR(I,M)                                                      COM13470
         Y=HI(I,M)                                                      COM13480
         HR(I,M1)=CR*TR-CI*TI+S*X                                       COM13490
         HI(I,M1)=CR*TI+CI*TR+S*Y                                       COM13500
         HR(I,M  )=CR*X+CI*Y-S*TR                                       COM13510
700      HI(I,M  )=CR*Y-CI*X-S*TI                                       COM13520
      TR=HR(M,M)                                                        COM13530
      HR(M,M1)=S*TR                                                     COM13540
      HR(M,M)=CR*TR                                                     COM13550
      HI(M,M)=-CI*TR                                                    COM13560
      CI=WI(M)                                                          COM13570
      IF(CI.EQ.0D0) GO TO 520                                           COM13580
      CR=WR(M)                                                          COM13590
         DO 720 I=L,M                                                   COM13600
         TR=HR(I,M)                                                     COM13610
         TI=HI(I,M)                                                     COM13620
         HR(I,M)=CR*TR-CI*TI                                            COM13630
720      HI(I,M)=CR*TI+CI*TR                                            COM13640
      GO TO 520                                                         COM13650
C ROOT                                                                  COM13660
840   WR(M)=HR(M,M)+SHR                                                 COM13670
      HR(M,M)=WR(M)                                                     COM13680
      WI(M)=HI(M,M)+SHI                                                 COM13690
      HI(M,M)=WI(M)                                                     COM13700
      M=M1                                                              COM13710
      GO TO 500                                                         COM13720
C                                                                       COM13730
9999  IERR=M                                                            COM13740
C RESTORATION OF THE FIRST COLUMN OF HI                                 COM13750
1000     DO 1020 J=LOW+1,IGH                                            COM13760
         HI(J,1)=HI(J,J-1)                                              COM13770
1020     HI(J,J-1)=0D0                                                  COM13780
      RETURN                                                            COM13790
      END                                                               COM13800
