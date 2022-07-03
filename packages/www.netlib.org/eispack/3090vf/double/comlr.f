C-----------------------------------------------------------------------COM04520
C EIGENVALUES OF A COMPLEX UPPER-HESSENBERG MATRIX (LR METHOD)          COM04530
C                                                                       COM04540
      SUBROUTINE  COMLR (LD, N, LOW, IGH, HR,HI, WR,WI, IERR)           COM04550
C                                                                       COM04560
      INTEGER     LD, N, LOW, IGH, IERR                                 COM04570
      REAL*8      HR(LD,N),HI(LD,N), WR(N),WI(N)                        COM04580
C                                                                       COM04590
C LD     E  FIRST DIMENSION ASSIGNED TO THE TWO-DIMENSIONAL ARRAY       COM04600
C           PARAMETERS IN THE CALLING PROGRAM.                          COM04610
C                                                                       COM04620
C N      E  ORDER OF THE MATRIX.                                        COM04630
C                                                                       COM04640
C LOW    E  PARAMETER DETERMINED BY THE BALANCING SUBROUTINE CBAL.      COM04650
C           IF CBAL WAS NOT USED, SET  LOW=1.                           COM04660
C                                                                       COM04670
C IGH    E  PARAMETER DETERMINED BY THE BALANCING SUBROUTINE CBAL.      COM04680
C           IF CBAL WAS NOT USED, SET  IGH=ORDER OF THE MATRIX.         COM04690
C                                                                       COM04700
C HR,HI  E  REAL AND IMAGINARY PARTS OF THE COMPLEX HESSENBERG MATRIX.  COM04710
C           THE SUB-HESSENBERG PARTS OF THESE ARRAYS CONTAIN FURTHER    COM04720
C           INFORMATION ABOUT THE TRANSFORMATIONS PERFORMED BY COMHES,  COM04730
C           IF COMHES PRODUCED THE HESSENBERG MATRIX.  THESE ARRAY      COM04740
C           ELEMENTS MUST BE SET TO ZERO IF THE EIGENVECTORS OF THE     COM04750
C           HESSENBERG MATRIX ARE DESIRED.                              COM04760
C        R  THE UPPER-HESSENBERG PARTS OF THESE ARRAYS ARE NOT PRESERVEDCOM04770
C                                                                       COM04780
C WR,WI  R  REAL AND IMAGINARY PARTS OF THE EIGENVALUES.                COM04790
C           IF AN ERROR RETURN OCCURS, THE EIGENVALUES OF INDICES       COM04800
C           IERR+1,...,N SHOULD BE CORRECT.                             COM04810
C                                                                       COM04820
C IERR   R  ZERO:       NORMAL RETURN,                                  COM04830
C           NONZERO:    NOT ALL THE EIGENVALUES COULD BE OBTAINED IN    COM04840
C                       30*(IGH-LOW+1) ITERATIONS.  THE EIGENVALUES OF  COM04850
C                       INDICES IERR+1,...,N SHOULD BE CORRECT.         COM04860
C                                                                       COM04870
C     THIS SUBROUTINE IS BASED ON THE ALGOL PROCEDURE COMLR,            COM04880
C     NUM. MATH. 12, 369-376(1968) BY MARTIN AND WILKINSON.             COM04890
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 396-403(1971).   COM04900
C                                                                       COM04910
C     VERSION FOR THE IBM 3090VF DATED NOVEMBER 1987.                   COM04920
C     ------------------------------------------------------------------COM04930
      INTEGER     I, J, KK, MM, K, L, M, M1, LIMIT, IT, NB1, NB, IB     COM04940
      REAL*8      SI,SR, TR,TI, SHI,SHR, XI,XR, YI,YR, ZR,ZI            COM04950
      COMPLEX*16  Z                                                     COM04960
      LOGICAL     NOX(128), PNOX                                        COM04970
      DATA        NB/128/                                               COM04980
C                                                                       COM04990
      CALL XUFLOW(0)                                                    COM05000
      IERR=0                                                            COM05010
      NB1=NB-1                                                          COM05020
C ROOTS ISOLATED BY CBAL                                                COM05030
         DO 200 I=1,N                                                   COM05040
         IF (I.GE.LOW .AND. I.LE.IGH) GO TO 200                         COM05050
         WR(I)=HR(I,I)                                                  COM05060
         WI(I)=HI(I,I)                                                  COM05070
200      CONTINUE                                                       COM05080
C                                                                       COM05090
C COMPUTATION OF THE EIGENVALUES                                        COM05100
      M=IGH                                                             COM05110
      SHR=0D0                                                           COM05120
      SHI=0D0                                                           COM05130
      LIMIT=30*(IGH-LOW+1)                                              COM05140
220   IF(M.LT.LOW) GO TO 1000                                           COM05150
      M1=M-1                                                            COM05160
C MORE ROOTS                                                            COM05170
      IT=0                                                              COM05180
C SEARCH FOR A SMALL SUBDIAGONAL ELEMENT                                COM05190
240      DO 260 L=M,LOW+1,-1                                            COM05200
         TR=DABS(HR(L-1,L-1))+DABS(HI(L-1,L-1))                         COM05210
     $           +DABS(HR(L,L))+DABS(HI(L,L))                           COM05220
         TI=TR+DABS(HR(L,L-1))+DABS(HI(L,L-1))                          COM05230
         IF (TR.EQ.TI) GO TO 280                                        COM05240
260   CONTINUE                                                          COM05250
      L=LOW                                                             COM05260
280   IF (L.EQ.M) GO TO 900                                             COM05270
      IF (LIMIT.EQ.0) GO TO 999                                         COM05280
      IF (IT.EQ.10 .OR. IT.EQ.20) GO TO 320                             COM05290
C REGULAR SHIFT FROM THE BOTTOM PRINCIPAL SUBMATRIX OF ORDER 2          COM05300
      SR=HR(M,M)                                                        COM05310
      SI=HI(M,M)                                                        COM05320
      XR=HR(M1,M)*HR(M,M1)-HI(M1,M)*HI(M,M1)                            COM05330
      XI=HR(M1,M)*HI(M,M1)+HI(M1,M)*HR(M,M1)                            COM05340
      IF (XR.EQ.0D0 .AND. XI.EQ.0D0) GO TO 340                          COM05350
      YR=0.5D0*(HR(M1,M1)-SR)                                           COM05360
      YI=0.5D0*(HI(M1,M1)-SI)                                           COM05370
      Z=CDSQRT(DCMPLX(XR+YR*YR-YI*YI,XI+YI*(YR+YR)))                    COM05380
      IF(YR*DREAL(Z)+YI*DIMAG(Z).LT.0D0) Z=-Z                           COM05390
      ZR=YR+DREAL(Z)                                                    COM05400
      ZI=YI+DIMAG(Z)                                                    COM05410
      IF(DABS(ZR).LE.DABS(ZI)) GO TO 300                                COM05420
         YR=ZI/ZR                                                       COM05430
         YI=ZR+YR*ZI                                                    COM05440
         SR=SR-(XR+YR*XI)/YI                                            COM05450
         SI=SI-(XI-YR*XR)/YI                                            COM05460
         GO TO 340                                                      COM05470
300   YR=ZR/ZI                                                          COM05480
      YI=ZI+YR*ZR                                                       COM05490
      SR=SR-(XI+YR*XR)/YI                                               COM05500
      SI=SI-(YR*XI-XR)/YI                                               COM05510
      GO TO 340                                                         COM05520
C EXCEPTIONAL SHIFT                                                     COM05530
320   SR=DABS(HR(M,M1))+DABS(HR(M1,M-2))                                COM05540
      SI=DABS(HI(M,M1))+DABS(HI(M1,M-2))                                COM05550
C ACCUMULATED SHIFTS                                                    COM05560
340   SHR=SHR+SR                                                        COM05570
      SHI=SHI+SI                                                        COM05580
         DO 360 I=LOW,M                                                 COM05590
         HR(I,I)=HR(I,I)-SR                                             COM05600
         HI(I,I)=HI(I,I)-SI                                             COM05610
360      CONTINUE                                                       COM05620
C ITERATION COUNTS                                                      COM05630
      IT=IT+1                                                           COM05640
      LIMIT=LIMIT-1                                                     COM05650
C SEARCH FOR TWO CONSECUTIVE SMALL SUBDIAGONAL ELEMENTS                 COM05660
      XR=DABS(HR(M1,M1))+DABS(HI(M1,M1))                                COM05670
      YR=DABS(HR(M,M1))+DABS(HI(M,M1))                                  COM05680
      ZR=DABS(HR(M,M))+DABS(HI(M,M))                                    COM05690
         DO 380 K=M1,L+1,-1                                             COM05700
         YI=YR                                                          COM05710
         YR=DABS(HR(K,K-1))+DABS(HI(K,K-1))                             COM05720
         XI=ZR                                                          COM05730
         ZR=XR                                                          COM05740
         XR=DABS(HR(K-1,K-1))+DABS(HI(K-1,K-1))                         COM05750
         TR=ZR/YI*(ZR+XR+XI)                                            COM05760
         TI=TR+YR                                                       COM05770
         IF (TR.EQ.TI) GO TO 400                                        COM05780
380      CONTINUE                                                       COM05790
      K=L                                                               COM05800
C LR SWEEP                                                              COM05810
400   IB=1-K+NB                                                         COM05820
C      (THE LEFT TRANSFORMATIONS ARE PERFORMED ON BLOCKS OF NB ROWS)    COM05830
         DO 720 KK=K,M,NB                                               COM05840
         IB=IB-NB                                                       COM05850
         MM=MIN0(KK+NB1,M1)                                             COM05860
            DO 600 J=KK,MM                                              COM05870
C    PREVIOUS LEFT TRANSFORMATIONS APPLIED TO COLUMN J                  COM05880
               DO 440 I=KK,J-1                                          COM05890
               IF(NOX(I+IB)) GO TO 420                                  COM05900
C       ROW EXCHANGE                                                    COM05910
                  XR=HR(I+1,J)                                          COM05920
                  XI=HI(I+1,J)                                          COM05930
                  HR(I+1,J)=HR(I,J)-WR(I)*XR+WI(I)*XI                   COM05940
                  HI(I+1,J)=HI(I,J)-WR(I)*XI-WI(I)*XR                   COM05950
                  HR(I,J)=XR                                            COM05960
                  HI(I,J)=XI                                            COM05970
                  GO TO 440                                             COM05980
C       NO EXCHANGE                                                     COM05990
420            HR(I+1,J)=HR(I+1,J)-WR(I)*HR(I,J)+WI(I)*HI(I,J)          COM06000
               HI(I+1,J)=HI(I+1,J)-WR(I)*HI(I,J)-WI(I)*HR(I,J)          COM06010
440            CONTINUE                                                 COM06020
C    ANNIHILATION OF THE SUBDIAGONAL ELEMENT IN COLUMN J                COM06030
            IF(DABS(HR(J,J))  +DABS(HI(J,J)) .GE.                       COM06040
     $         DABS(HR(J+1,J))+DABS(HI(J+1,J))) GO TO 460               COM06050
C       ROW EXCHANGE                                                    COM06060
               NOX(J+IB)=.FALSE.                                        COM06070
               XR=HR(J+1,J)                                             COM06080
               XI=HI(J+1,J)                                             COM06090
               YR=HR(J,J)                                               COM06100
               YI=HI(J,J)                                               COM06110
               HR(J,J)=XR                                               COM06120
               HI(J,J)=XI                                               COM06130
               HR(J+1,J)=YR                                             COM06140
               HI(J+1,J)=YI                                             COM06150
               GO TO 480                                                COM06160
C       NO EXCHANGE                                                     COM06170
460         NOX(J+IB)=.TRUE.                                            COM06180
            XR=HR(J,J)                                                  COM06190
            XI=HI(J,J)                                                  COM06200
            YR=HR(J+1,J)                                                COM06210
            YI=HI(J+1,J)                                                COM06220
C       GAUSSIAN MULTIPLIER (W=Y/X)                                     COM06230
480         IF(DABS(XR).LT.DABS(XI)) GO TO 500                          COM06240
               SR=XI/XR                                                 COM06250
               TR=XR+SR*XI                                              COM06260
               WR(J)=(YR+SR*YI)/TR                                      COM06270
               WI(J)=(YI-SR*YR)/TR                                      COM06280
               GO TO 520                                                COM06290
500         SR=XR/XI                                                    COM06300
            TR=XI+SR*XR                                                 COM06310
            WR(J)=(YI+SR*YR)/TR                                         COM06320
            WI(J)=(SR*YI-YR)/TR                                         COM06330
520         IF(J.EQ.K) GO TO 600                                        COM06340
C       RIGHT TRANSFORMATION OF COLUMNS J-1 AND J                       COM06350
            ZR=WR(J-1)                                                  COM06360
            ZI=WI(J-1)                                                  COM06370
            IF(PNOX)GO TO 560                                           COM06380
               DO 540 I=L,J-1                                           COM06390
               XR=HR(I,J-1)                                             COM06400
               XI=HI(I,J-1)                                             COM06410
               HR(I,J-1)=HR(I,J)+ZR*XR-ZI*XI                            COM06420
               HI(I,J-1)=HI(I,J)+ZR*XI+ZI*XR                            COM06430
               HR(I,J)=XR                                               COM06440
               HI(I,J)=XI                                               COM06450
540            CONTINUE                                                 COM06460
            HR(J,J-1)=HR(J,J)                                           COM06470
            HI(J,J-1)=HI(J,J)                                           COM06480
            HR(J,J)=0D0                                                 COM06490
            HI(J,J)=0D0                                                 COM06500
            GO TO 600                                                   COM06510
560            DO 580 I=L,J-1                                           COM06520
               HR(I,J-1)=HR(I,J-1)+ZR*HR(I,J)-ZI*HI(I,J)                COM06530
580            HI(I,J-1)=HI(I,J-1)+ZR*HI(I,J)+ZI*HR(I,J)                COM06540
            HR(J,J-1)=ZR*HR(J,J)-ZI*HI(J,J)                             COM06550
            HI(J,J-1)=ZR*HI(J,J)+ZI*HR(J,J)                             COM06560
600         PNOX=NOX(J+IB)                                              COM06570
C LEFT TRANSFORMATION OF COLUMNS MM+1,...,M                             COM06580
            DO 700 J=MM+1,M                                             COM06590
               DO 680 I=KK,MM                                           COM06600
               IF(NOX(I+IB)) GO TO 660                                  COM06610
                  XR=HR(I+1,J)                                          COM06620
                  XI=HI(I+1,J)                                          COM06630
                  YR=HR(I,J)                                            COM06640
                  YI=HI(I,J)                                            COM06650
                  HR(I,J)=XR                                            COM06660
                  HI(I,J)=XI                                            COM06670
                  HR(I+1,J)=YR-WR(I)*XR+WI(I)*XI                        COM06680
                  HI(I+1,J)=YI-WR(I)*XI-WI(I)*XR                        COM06690
                  GO TO 680                                             COM06700
660            HR(I+1,J)=HR(I+1,J)-WR(I)*HR(I,J)+WI(I)*HI(I,J)          COM06710
               HI(I+1,J)=HI(I+1,J)-WR(I)*HI(I,J)-WI(I)*HR(I,J)          COM06720
680            CONTINUE                                                 COM06730
700         CONTINUE                                                    COM06740
720      CONTINUE                                                       COM06750
C RIGHT TRANSFORMATION OF COLUMNS M-1 AND M                             COM06760
      ZR=WR(M1)                                                         COM06770
      ZI=WI(M1)                                                         COM06780
      IF(PNOX) GO TO 800                                                COM06790
         DO 780 I=L,M1                                                  COM06800
         XR=HR(I,M1)                                                    COM06810
         XI=HI(I,M1)                                                    COM06820
         HR(I,M1)=HR(I,M)+ZR*XR-ZI*XI                                   COM06830
         HI(I,M1)=HI(I,M)+ZR*XI+ZI*XR                                   COM06840
         HR(I,M)=XR                                                     COM06850
         HI(I,M)=XI                                                     COM06860
780      CONTINUE                                                       COM06870
      HR(M,M1)=HR(M,M)                                                  COM06880
      HI(M,M1)=HI(M,M)                                                  COM06890
      HR(M,M)=0D0                                                       COM06900
      HI(M,M)=0D0                                                       COM06910
      GO TO 240                                                         COM06920
800      DO 820 I=L,M1                                                  COM06930
         HR(I,M1)=HR(I,M1)+ZR*HR(I,M)-ZI*HI(I,M)                        COM06940
820      HI(I,M1)=HI(I,M1)+ZR*HI(I,M)+ZI*HR(I,M)                        COM06950
      HR(M,M1)=ZR*HR(M,M)-ZI*HI(M,M)                                    COM06960
      HI(M,M1)=ZR*HI(M,M)+ZI*HR(M,M)                                    COM06970
      GO TO 240                                                         COM06980
C ROOT                                                                  COM06990
900   WR(M)=HR(M,M)+SHR                                                 COM07000
      HR(M,M)=WR(M)                                                     COM07010
      WI(M)=HI(M,M)+SHI                                                 COM07020
      HI(M,M)=WI(M)                                                     COM07030
      M=M1                                                              COM07040
      GO TO 220                                                         COM07050
C                                                                       COM07060
C ERROR: NOT ALL THE EIGENVALUES COULD BE OBTAINED WITHIN 30*(IGH-LOW+1)COM07070
C        ITERATIONS                                                     COM07080
999   IERR=M                                                            COM07090
1000  RETURN                                                            COM07100
      END                                                               COM07110
