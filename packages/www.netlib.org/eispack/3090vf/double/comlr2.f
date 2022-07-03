C-----------------------------------------------------------------------COM07120
C EIGENSYSTEM OF A COMPLEX UPPER-HESSENBERG MATRIX (LR METHOD), OR      COM07130
C EIGENSYSTEM OF A COMPLEX GENERAL MATRIX REDUCED TO HESSENBERG FORM    COM07140
C     BY SUBROUTINE COMHES.                                             COM07150
C                                                                       COM07160
      SUBROUTINE  COMLR2 (LD,N,LOW,IGH,INT,HR,HI,WR,WI,VR,VI,IERR)      COM07170
C                                                                       COM07180
      INTEGER     LD, N, LOW, IGH, IERR, INT(IGH)                       COM07190
      REAL*8      HR(LD,N),HI(LD,N), WR(N),WI(N), VR(LD,N),VI(LD,N)     COM07200
C                                                                       COM07210
C LD     E  FIRST DIMENSION ASSIGNED TO THE TWO-DIMENSIONAL ARRAY       COM07220
C           PARAMETERS IN THE CALLING PROGRAM.                          COM07230
C                                                                       COM07240
C N      E  ORDER OF THE MATRIX.                                        COM07250
C                                                                       COM07260
C LOW    E  PARAMETER DETERMINED BY THE BALANCING SUBROUTINE CBAL.      COM07270
C           IF CBAL WAS NOT USED, SET  LOW=1.                           COM07280
C                                                                       COM07290
C IGH    E  PARAMETER DETERMINED BY THE BALANCING SUBROUTINE CBAL.      COM07300
C           IF CBAL WAS NOT USED, SET  IGH=ORDER OF THE MATRIX.         COM07310
C                                                                       COM07320
C INT    E  RECORD OF ROW AND COLUMN INTERCHANGES PERFORMED IN          COM07330
C           SUBROUTINE COMHES, IF USED, IN CELLS LOW,...,IGH.           COM07340
C           IF THE EIGENVECTORS OF THE HESSENBERG MATRIX ARE DESIRED,   COM07350
C           SET INT(J)=J FOR THESE ELEMENTS.                            COM07360
C                                                                       COM07370
C HR,HI  E  REAL AND IMAGINARY PARTS OF THE COMPLEX HESSENBERG MATRIX.  COM07380
C           THE SUB-HESSENBERG PARTS OF THESE ARRAYS CONTAIN FURTHER    COM07390
C           INFORMATION ABOUT THE TRANSFORMATIONS PERFORMED BY COMHES,  COM07400
C           IF COMHES PRODUCED THE HESSENBERG MATRIX.  THESE ARRAY      COM07410
C           ELEMENTS MUST BE SET TO ZERO IF THE EIGENVECTORS OF THE     COM07420
C           HESSENBERG MATRIX ARE DESIRED.                              COM07430
C        R  THE UPPER-HESSENBERG PARTS OF THESE ARRAYS ARE NOT PRESERVEDCOM07440
C           THE NORM OF THE TRIANGULARIZED MATRIX IS STORED IN HR(1,1). COM07450
C                                                                       COM07460
C WR,WI  R  REAL AND IMAGINARY PARTS OF THE EIGENVALUES.                COM07470
C           IF AN ERROR RETURN OCCURS, THE EIGENVALUES OF INDICES       COM07480
C           IERR+1,...,N SHOULD BE CORRECT.                             COM07490
C                                                                       COM07500
C VR,VI  R  REAL AND IMAGINARY PARTS OF THE MATRIX OF EIGENVECTORS.     COM07510
C           THE EIGENVECTORS ARE NOT NORMALIZED.                        COM07520
C           IF AN ERROR RETURN OCCURS, NO EIGENVECTOR IS COMPUTED.      COM07530
C                                                                       COM07540
C IERR   R  ZERO:       NORMAL RETURN,                                  COM07550
C           NONZERO:    NOT ALL THE EIGENVALUES COULD BE OBTAINED IN    COM07560
C                       30*(IGH-LOW+1) ITERATIONS.  THE EIGENVALUES OF  COM07570
C                       INDICES IERR+1,...,N SHOULD BE CORRECT.         COM07580
C                                                                       COM07590
C     THIS SUBROUTINE IS BASED ON THE ALGOL PROCEDURE COMLR2,           COM07600
C     NUM. MATH. 16, 181-204(1970) BY PETERS AND WILKINSON.             COM07610
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 372-395(1971).   COM07620
C                                                                       COM07630
C     VERSION FOR THE IBM 3090VF DATED NOVEMBER 1987.                   COM07640
C     ------------------------------------------------------------------COM07650
      INTEGER     I, J, K, L, M, M1, LIMIT, IT, INT1                    COM07660
      REAL*8      SI,SR, TR,TI, SHI,SHR, XI,XR, YI,YR, ZR,ZI            COM07670
      REAL*8      TNORM, TEST, EPS                                      COM07680
      COMPLEX*16  Z                                                     COM07690
      DATA        EPS/2D-16/                                            COM07700
C                                                                       COM07710
      CALL XUFLOW(0)                                                    COM07720
      IERR=0                                                            COM07730
C INITIALIZATION OF THE EIGENVECTOR MATRIX TO IDENTITY                  COM07740
         DO 120 I=1,N                                                   COM07750
            DO 100 J=1,N                                                COM07760
            VR(I,J)=0D0                                                 COM07770
100         VI(I,J)=0D0                                                 COM07780
120      VR(I,I)=1D0                                                    COM07790
C RIGHT MATRIX OF THE SIMILARITY REDUCTION TO HESSENBERG FORM BY COMHES COM07800
         DO 180 I=IGH-1,LOW+1,-1                                        COM07810
            DO 140 K=I+1,IGH                                            COM07820
            VR(K,I)=HR(K,I-1)                                           COM07830
140         VI(K,I)=HI(K,I-1)                                           COM07840
         J=INT(I)                                                       COM07850
         IF (I.EQ.J) GO TO 180                                          COM07860
            DO 160 K=I,IGH                                              COM07870
            VR(I,K)=VR(J,K)                                             COM07880
            VI(I,K)=VI(J,K)                                             COM07890
            VR(J,K)=0D0                                                 COM07900
160         VI(J,K)=0D0                                                 COM07910
         VR(J,I)=1D0                                                    COM07920
180      CONTINUE                                                       COM07930
C ROOTS ISOLATED BY CBAL                                                COM07940
         DO 200 I=1,N                                                   COM07950
         IF (I.GE.LOW .AND. I.LE.IGH) GO TO 200                         COM07960
         WR(I)=HR(I,I)                                                  COM07970
         WI(I)=HI(I,I)                                                  COM07980
200      CONTINUE                                                       COM07990
C                                                                       COM08000
C COMPUTATION OF THE EIGENVALUES                                        COM08010
      INT1=INT(LOW)                                                     COM08020
      INT(LOW)=LOW                                                      COM08030
      M=IGH                                                             COM08040
      SHR=0D0                                                           COM08050
      SHI=0D0                                                           COM08060
      LIMIT=30*(IGH-LOW+1)                                              COM08070
220   IF(M.LT.LOW) GO TO 900                                            COM08080
      M1=M-1                                                            COM08090
C MORE ROOTS                                                            COM08100
      IT=0                                                              COM08110
C SEARCH FOR A SMALL SUBDIAGONAL ELEMENT                                COM08120
240      DO 260 L=M,LOW+1,-1                                            COM08130
         TR=DABS(HR(L-1,L-1))+DABS(HI(L-1,L-1))                         COM08140
     $           +DABS(HR(L,L))+DABS(HI(L,L))                           COM08150
         TI=TR+DABS(HR(L,L-1))+DABS(HI(L,L-1))                          COM08160
         IF (TR.EQ.TI) GO TO 280                                        COM08170
260   CONTINUE                                                          COM08180
      L=LOW                                                             COM08190
280   IF (L.EQ.M) GO TO 880                                             COM08200
      IF (LIMIT.EQ.0) GO TO 9999                                        COM08210
      IF (IT.EQ.10 .OR. IT.EQ.20) GO TO 320                             COM08220
C REGULAR SHIFT FROM THE BOTTOM PRINCIPAL SUBMATRIX OF ORDER 2          COM08230
      SR=HR(M,M)                                                        COM08240
      SI=HI(M,M)                                                        COM08250
      XR=HR(M1,M)*HR(M,M1)-HI(M1,M)*HI(M,M1)                            COM08260
      XI=HR(M1,M)*HI(M,M1)+HI(M1,M)*HR(M,M1)                            COM08270
      IF (XR.EQ.0D0 .AND. XI.EQ.0D0) GO TO 340                          COM08280
      YR=0.5D0*(HR(M1,M1)-SR)                                           COM08290
      YI=0.5D0*(HI(M1,M1)-SI)                                           COM08300
      Z=CDSQRT(DCMPLX(XR+YR*YR-YI*YI,XI+YI*(YR+YR)))                    COM08310
      IF(YR*DREAL(Z)+YI*DIMAG(Z).LT.0D0) Z=-Z                           COM08320
      ZR=YR+DREAL(Z)                                                    COM08330
      ZI=YI+DIMAG(Z)                                                    COM08340
      IF(DABS(ZR).LE.DABS(ZI)) GO TO 300                                COM08350
         YR=ZI/ZR                                                       COM08360
         YI=ZR+YR*ZI                                                    COM08370
         SR=SR-(XR+YR*XI)/YI                                            COM08380
         SI=SI-(XI-YR*XR)/YI                                            COM08390
         GO TO 340                                                      COM08400
300   YR=ZR/ZI                                                          COM08410
      YI=ZI+YR*ZR                                                       COM08420
      SR=SR-(XI+YR*XR)/YI                                               COM08430
      SI=SI-(YR*XI-XR)/YI                                               COM08440
      GO TO 340                                                         COM08450
C EXCEPTIONAL SHIFT                                                     COM08460
320   SR=DABS(HR(M,M1))+DABS(HR(M1,M-2))                                COM08470
      SI=DABS(HI(M,M1))+DABS(HI(M1,M-2))                                COM08480
C ACCUMULATED SHIFTS                                                    COM08490
340   SHR=SHR+SR                                                        COM08500
      SHI=SHI+SI                                                        COM08510
         DO 360 I=LOW,M                                                 COM08520
         HR(I,I)=HR(I,I)-SR                                             COM08530
         HI(I,I)=HI(I,I)-SI                                             COM08540
360      CONTINUE                                                       COM08550
C ITERATION COUNTS                                                      COM08560
      IT=IT+1                                                           COM08570
      LIMIT=LIMIT-1                                                     COM08580
C SEARCH FOR TWO CONSECUTIVE SMALL SUBDIAGONAL ELEMENTS                 COM08590
      XR=DABS(HR(M1,M1))+DABS(HI(M1,M1))                                COM08600
      YR=DABS(HR(M,M1))+DABS(HI(M,M1))                                  COM08610
      ZR=DABS(HR(M,M))+DABS(HI(M,M))                                    COM08620
         DO 380 K=M1,L+1,-1                                             COM08630
         YI=YR                                                          COM08640
         YR=DABS(HR(K,K-1))+DABS(HI(K,K-1))                             COM08650
         XI=ZR                                                          COM08660
         ZR=XR                                                          COM08670
         XR=DABS(HR(K-1,K-1))+DABS(HI(K-1,K-1))                         COM08680
         TR=ZR/YI*(ZR+XR+XI)                                            COM08690
         TI=TR+YR                                                       COM08700
         IF (TR.EQ.TI) GO TO 400                                        COM08710
380      CONTINUE                                                       COM08720
      K=L                                                               COM08730
C LR SWEEP                                                              COM08740
400      DO 600 J=K,M1                                                  COM08750
C    PREVIOUS LEFT TRANSFORMATIONS APPLIED TO COLUMN J                  COM08760
            DO 440 I=K,J-1                                              COM08770
            IF(INT(I).GT.0) GO TO 420                                   COM08780
C       ROW EXCHANGE                                                    COM08790
               XR=HR(I+1,J)                                             COM08800
               XI=HI(I+1,J)                                             COM08810
               HR(I+1,J)=HR(I,J)-WR(I)*XR+WI(I)*XI                      COM08820
               HI(I+1,J)=HI(I,J)-WR(I)*XI-WI(I)*XR                      COM08830
               HR(I,J)=XR                                               COM08840
               HI(I,J)=XI                                               COM08850
               GO TO 440                                                COM08860
C       NO EXCHANGE                                                     COM08870
420         HR(I+1,J)=HR(I+1,J)-WR(I)*HR(I,J)+WI(I)*HI(I,J)             COM08880
            HI(I+1,J)=HI(I+1,J)-WR(I)*HI(I,J)-WI(I)*HR(I,J)             COM08890
440         CONTINUE                                                    COM08900
C    ANNIHILATION OF THE SUBDIAGONAL ELEMENT IN COLUMN J                COM08910
         IF(DABS(HR(J,J))  +DABS(HI(J,J)) .GE.                          COM08920
     $      DABS(HR(J+1,J))+DABS(HI(J+1,J))) GO TO 460                  COM08930
C       ROW EXCHANGE                                                    COM08940
            INT(J)=-IABS(INT(J))                                        COM08950
            XR=HR(J+1,J)                                                COM08960
            XI=HI(J+1,J)                                                COM08970
            YR=HR(J,J)                                                  COM08980
            YI=HI(J,J)                                                  COM08990
            HR(J,J)=XR                                                  COM09000
            HI(J,J)=XI                                                  COM09010
            HR(J+1,J)=YR                                                COM09020
            HI(J+1,J)=YI                                                COM09030
            GO TO 480                                                   COM09040
C       NO EXCHANGE                                                     COM09050
460      INT(J)=IABS(INT(J))                                            COM09060
         XR=HR(J,J)                                                     COM09070
         XI=HI(J,J)                                                     COM09080
         YR=HR(J+1,J)                                                   COM09090
         YI=HI(J+1,J)                                                   COM09100
C       GAUSSIAN MULTIPLIER (W=Y/X)                                     COM09110
480      IF(DABS(XR).LT.DABS(XI)) GO TO 500                             COM09120
            SR=XI/XR                                                    COM09130
            TR=XR+SR*XI                                                 COM09140
            WR(J)=(YR+SR*YI)/TR                                         COM09150
            WI(J)=(YI-SR*YR)/TR                                         COM09160
            GO TO 520                                                   COM09170
500      SR=XR/XI                                                       COM09180
         TR=XI+SR*XR                                                    COM09190
         WR(J)=(YI+SR*YR)/TR                                            COM09200
         WI(J)=(SR*YI-YR)/TR                                            COM09210
520      IF(J.EQ.K) GO TO 600                                           COM09220
C       RIGHT TRANSFORMATION OF COLUMNS J-1 AND J                       COM09230
         ZR=WR(J-1)                                                     COM09240
         ZI=WI(J-1)                                                     COM09250
         IF(INT(J-1).GT.0)GO TO 560                                     COM09260
            DO 540 I=1,J-1                                              COM09270
            XR=HR(I,J-1)                                                COM09280
            XI=HI(I,J-1)                                                COM09290
            HR(I,J-1)=HR(I,J)+ZR*XR-ZI*XI                               COM09300
            HI(I,J-1)=HI(I,J)+ZR*XI+ZI*XR                               COM09310
            HR(I,J)=XR                                                  COM09320
            HI(I,J)=XI                                                  COM09330
540         CONTINUE                                                    COM09340
         HR(J,J-1)=HR(J,J)                                              COM09350
         HI(J,J-1)=HI(J,J)                                              COM09360
         HR(J,J)=0D0                                                    COM09370
         HI(J,J)=0D0                                                    COM09380
         GO TO 600                                                      COM09390
560         DO 580 I=1,J-1                                              COM09400
            HR(I,J-1)=HR(I,J-1)+ZR*HR(I,J)-ZI*HI(I,J)                   COM09410
580         HI(I,J-1)=HI(I,J-1)+ZR*HI(I,J)+ZI*HR(I,J)                   COM09420
         HR(J,J-1)=ZR*HR(J,J)-ZI*HI(J,J)                                COM09430
         HI(J,J-1)=ZR*HI(J,J)+ZI*HR(J,J)                                COM09440
600      CONTINUE                                                       COM09450
C    LEFT TRANSFORMATION OF COLUMN M                                    COM09460
         DO 640 I=K,M1                                                  COM09470
         IF(INT(I).GT.0) GO TO 620                                      COM09480
C       ROW EXCHANGE                                                    COM09490
            XR=HR(I+1,M)                                                COM09500
            XI=HI(I+1,M)                                                COM09510
            HR(I+1,M)=HR(I,M)-WR(I)*XR+WI(I)*XI                         COM09520
            HI(I+1,M)=HI(I,M)-WR(I)*XI-WI(I)*XR                         COM09530
            HR(I,M)=XR                                                  COM09540
            HI(I,M)=XI                                                  COM09550
            GO TO 640                                                   COM09560
C       NO EXCHANGE                                                     COM09570
620      HR(I+1,M)=HR(I+1,M)-WR(I)*HR(I,M)+WI(I)*HI(I,M)                COM09580
         HI(I+1,M)=HI(I+1,M)-WR(I)*HI(I,M)-WI(I)*HR(I,M)                COM09590
640      CONTINUE                                                       COM09600
C    RIGHT TRANSFORMATION OF COLUMNS M-1 AND M                          COM09610
      ZR=WR(M1)                                                         COM09620
      ZI=WI(M1)                                                         COM09630
      IF(INT(M1).GT.0)GO TO 680                                         COM09640
         DO 660 I=1,M1                                                  COM09650
         XR=HR(I,M1)                                                    COM09660
         XI=HI(I,M1)                                                    COM09670
         HR(I,M1)=HR(I,M)+ZR*XR-ZI*XI                                   COM09680
         HI(I,M1)=HI(I,M)+ZR*XI+ZI*XR                                   COM09690
         HR(I,M)=XR                                                     COM09700
         HI(I,M)=XI                                                     COM09710
660      CONTINUE                                                       COM09720
      HR(M,M1)=HR(M,M)                                                  COM09730
      HI(M,M1)=HI(M,M)                                                  COM09740
      HR(M,M)=0D0                                                       COM09750
      HI(M,M)=0D0                                                       COM09760
      GO TO 720                                                         COM09770
680      DO 700 I=1,M1                                                  COM09780
         HR(I,M1)=HR(I,M1)+ZR*HR(I,M)-ZI*HI(I,M)                        COM09790
700      HI(I,M1)=HI(I,M1)+ZR*HI(I,M)+ZI*HR(I,M)                        COM09800
      HR(M,M1)=ZR*HR(M,M)-ZI*HI(M,M)                                    COM09810
      HI(M,M1)=ZR*HI(M,M)+ZI*HR(M,M)                                    COM09820
C    COMPLETION OF THE LEFT TRANSFORMATIONS (COLUMNS M+1,...,N)         COM09830
720      DO 780 J=M+1,N                                                 COM09840
            DO 760 I=K,M1                                               COM09850
            IF(INT(I).GT.0) GO TO 740                                   COM09860
               XR=HR(I+1,J)                                             COM09870
               YR=HR(I,J)                                               COM09880
               XI=HI(I+1,J)                                             COM09890
               YI=HI(I,J)                                               COM09900
               HR(I,J)=XR                                               COM09910
               HI(I,J)=XI                                               COM09920
               HR(I+1,J)=YR-WR(I)*XR+WI(I)*XI                           COM09930
               HI(I+1,J)=YI-WR(I)*XI-WI(I)*XR                           COM09940
               GO TO 760                                                COM09950
740         HR(I+1,J)=HR(I+1,J)-WR(I)*HR(I,J)+WI(I)*HI(I,J)             COM09960
            HI(I+1,J)=HI(I+1,J)-WR(I)*HI(I,J)-WI(I)*HR(I,J)             COM09970
760         CONTINUE                                                    COM09980
780      CONTINUE                                                       COM09990
C    UPDATE OF THE RIGHT MATRIX OF THE REDUCTION TO TRIANGULAR FORM     COM10000
         DO 860 J=K,M1                                                  COM10010
         ZR=WR(J)                                                       COM10020
         ZI=WI(J)                                                       COM10030
         IF(INT(J).GT.0)GO TO 820                                       COM10040
            DO 800 I=LOW,IGH                                            COM10050
            XR=VR(I,J)                                                  COM10060
            XI=VI(I,J)                                                  COM10070
            VR(I,J)=VR(I,J+1)+ZR*XR-ZI*XI                               COM10080
            VI(I,J)=VI(I,J+1)+ZR*XI+ZI*XR                               COM10090
            VR(I,J+1)=XR                                                COM10100
            VI(I,J+1)=XI                                                COM10110
800         CONTINUE                                                    COM10120
         GO TO 860                                                      COM10130
820         DO 840 I=LOW,IGH                                            COM10140
            VR(I,J)=VR(I,J)+ZR*VR(I,J+1)-ZI*VI(I,J+1)                   COM10150
840         VI(I,J)=VI(I,J)+ZR*VI(I,J+1)+ZI*VR(I,J+1)                   COM10160
860      CONTINUE                                                       COM10170
      GO TO 240                                                         COM10180
C ROOT                                                                  COM10190
880   WR(M)=HR(M,M)+SHR                                                 COM10200
      HR(M,M)=WR(M)                                                     COM10210
      WI(M)=HI(M,M)+SHI                                                 COM10220
      HI(M,M)=WI(M)                                                     COM10230
      M=M1                                                              COM10240
      GO TO 220                                                         COM10250
C                                                                       COM10260
C EIGENVECTORS OF THE UPPER-TRIANGULAR MATRIX PRODUCED BY LR            COM10270
900   TNORM=0D0                                                         COM10280
         DO 940 J=1,N                                                   COM10290
            DO 920 I=1,J                                                COM10300
            TNORM=DMAX1(DABS(HR(I,J))+DABS(HI(I,J)),TNORM)              COM10310
920         CONTINUE                                                    COM10320
940      CONTINUE                                                       COM10330
      IF(TNORM.EQ.0D0) GO TO 10000                                      COM10340
      TEST=TNORM*EPS                                                    COM10350
         DO 1100 K=N,2,-1                                               COM10360
         HR(K,K)=1D0                                                    COM10370
         HI(K,K)=0D0                                                    COM10380
         XR=WR(K)                                                       COM10390
         XI=WI(K)                                                       COM10400
            DO 1080 J=K-1,1,-1                                          COM10410
            IF(J.EQ.K-1) GO TO 980                                      COM10420
            ZR=HR(J+1,K)                                                COM10430
            ZI=HI(J+1,K)                                                COM10440
               DO 960 I=1,J                                             COM10450
               TR=HR(I,J+1)                                             COM10460
               TI=HI(I,J+1)                                             COM10470
               HR(I,K)=HR(I,K)+ZR*TR-ZI*TI                              COM10480
960            HI(I,K)=HI(I,K)+ZR*TI+ZI*TR                              COM10490
980         ZR=XR-WR(J)                                                 COM10500
            ZI=XI-WI(J)                                                 COM10510
            IF(DABS(ZR)+DABS(ZI).NE.0D0) GO TO 1000                     COM10520
               HR(J,K)=HR(J,K)/TEST                                     COM10530
               HI(J,K)=HI(J,K)/TEST                                     COM10540
               GO TO 1040                                               COM10550
1000        TR=HR(J,K)                                                  COM10560
            TI=HI(J,K)                                                  COM10570
            IF(DABS(ZR).LE.DABS(ZI)) GO TO 1020                         COM10580
               YR=ZI/ZR                                                 COM10590
               YI=ZR+YR*ZI                                              COM10600
               HR(J,K)=(TR+YR*TI)/YI                                    COM10610
               HI(J,K)=(TI-YR*TR)/YI                                    COM10620
               GO TO 1040                                               COM10630
1020        YR=ZR/ZI                                                    COM10640
            YI=ZI+YR*ZR                                                 COM10650
            HR(J,K)=(YR*TR+TI)/YI                                       COM10660
            HI(J,K)=(YR*TI-TR)/YI                                       COM10670
C PROTECTION AGAINST OVERFLOW OF FLOATING-POINT REPRESENTATION          COM10680
1040        YR=DABS(HR(J,K))+DABS(HI(J,K))                              COM10690
            IF(YR.EQ.0D0) GO TO 1080                                    COM10700
            YI=YR+1D0/YR                                                COM10710
            IF(YI.GT.YR) GO TO 1080                                     COM10720
               DO 1060 I=1,K                                            COM10730
               HR(I,K)=HR(I,K)/YR                                       COM10740
1060           HI(I,K)=HI(I,K)/YR                                       COM10750
1080        CONTINUE                                                    COM10760
1100     CONTINUE                                                       COM10770
      HR(1,1)=1D0                                                       COM10780
      HI(1,1)=0D0                                                       COM10790
C                                                                       COM10800
C EIGENVECTORS OF THE ORIGINAL MATRIX                                   COM10810
C   ROOTS ISOLATED BY CBAL:  EIGENVECTORS OF THE TRIANGULAR MATRIX      COM10820
      IF(LOW.EQ.1) GO TO 1160                                           COM10830
         DO 1140 J=1,N                                                  COM10840
            DO 1120 I=1,MIN0(J,LOW-1)                                   COM10850
            VR(I,J)=HR(I,J)                                             COM10860
1120        VI(I,J)=HI(I,J)                                             COM10870
1140     CONTINUE                                                       COM10880
1160     DO 1200 J=IGH+1,N                                              COM10890
            DO 1180 I=IGH+1,J                                           COM10900
            VR(I,J)=HR(I,J)                                             COM10910
1180        VI(I,J)=HI(I,J)                                             COM10920
1200     CONTINUE                                                       COM10930
C   OTHER ROOTS:  TRANSFORMATION BY THE RIGHT MATRIX OF THE REDUCTION   COM10940
C                 TO TRIANGULAR FORM                                    COM10950
         DO 1260 J=N,LOW+1,-1                                           COM10960
         M=MIN0(J,IGH)                                                  COM10970
            DO 1240 I=LOW,IGH                                           COM10980
            TR=0D0                                                      COM10990
            TI=0D0                                                      COM11000
               DO 1220 K=LOW,M                                          COM11010
               TR=TR+VR(I,K)*HR(K,J)-VI(I,K)*HI(K,J)                    COM11020
1220           TI=TI+VR(I,K)*HI(K,J)+VI(I,K)*HR(K,J)                    COM11030
            VR(I,J)=TR                                                  COM11040
1240        VI(I,J)=TI                                                  COM11050
1260     CONTINUE                                                       COM11060
      HR(1,1)=TNORM                                                     COM11070
      GO TO 10000                                                       COM11080
C                                                                       COM11090
C ERROR: NOT ALL THE EIGENVALUES COULD BE OBTAINED WITHIN 30*(IGH-LOW+1)COM11100
C        ITERATIONS                                                     COM11110
9999  IERR=M                                                            COM11120
C RESTORATION OF THE SIGNS IN INT                                       COM11130
10000    DO 10020 I=LOW+1,IGH-1                                         COM11140
10020    INT(I)=IABS(INT(I))                                            COM11150
      INT(LOW)=INT1                                                     COM11160
      RETURN                                                            COM11170
      END                                                               COM11180
