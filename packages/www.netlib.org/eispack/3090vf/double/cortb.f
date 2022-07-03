@PROCESS DIRECTIVE('"    (')                                            COM18490
C-----------------------------------------------------------------------COM18500
C EIGENVECTORS OF A COMPLEX MATRIX BY TRANSFORMATION OF THOSE OF THE    COM18510
C CORRESPONDING UPPER-HESSENBERG MATRIX PRODUCED BY CORTH.              COM18520
C                                                                       COM18530
      SUBROUTINE  CORTB (LD, LOW, IGH, AR, AI, ORTR, ORTI, M, ZR, ZI)   COM18540
C                                                                       COM18550
      INTEGER     LD, LOW, IGH, M                                       COM18560
      REAL*8      AR(LD,IGH), AI(LD,IGH), ORTR(IGH), ORTI(IGH)          COM18570
      REAL*8      ZR(LD,M), ZI(LD,M)                                    COM18580
C                                                                       COM18590
C LD     E  FIRST DIMENSION ASSIGNED TO THE TWO-DIMENSIONAL ARRAY       COM18600
C           PARAMETERS IN THE CALLING PROGRAM.                          COM18610
C                                                                       COM18620
C LOW    E  PARAMETER DETERMINED BY THE BALANCING SUBROUTINE CBAL.      COM18630
C           IF CBAL WAS NOT USED, SET LOW = 1.                          COM18640
C                                                                       COM18650
C IGH    E  PARAMETER DETERMINED BY THE BALANCING SUBROUTINE CBAL.      COM18660
C           IF CBAL WAS NOT USED, SET IGH = ORDER OF THE MATRIX.        COM18670
C                                                                       COM18680
C AR,AI  E  INFORMATION ABOUT THE UNITARY TRANSFORMATIONS USED IN THE   COM18690
C           REDUCTION BY CORTH IN THE SUB-HESSENBERG PARTS OF THE       COM18700
C           ARRAYS.                                                     COM18710
C                                                                       COM18720
C ORTR   E  ADDITIONAL INFORMATION ABOUT THE UNITARY TRANSFORMATIONS    COM18730
C           USED BY CORTH.  ONLY ORTR(LOW),...,ORTR(IGH) ARE USED.      COM18740
C                                                                       COM18750
C ORTI   -  UNUSED PARAMETER (TO PRESERVE THE EISPACK CALLING SEQUENCE).COM18760
C                                                                       COM18770
C M      E  NUMBER OF EIGENVECTORS TO BE TRANSFORMED.                   COM18780
C                                                                       COM18790
C ZR,ZI  E  REAL AND IMAGINARY PARTS OF THE MATRIX OF EIGENVECTORS TO BECOM18800
C           TRANSFORMED.                                                COM18810
C        R  TRANSFORMED REAL AND IMAGINARY PARTS OF THE MATRIX OF       COM18820
C           EIGENVECTORS.                                               COM18830
C                                                                       COM18840
C     THIS SUBROUTINE IS BASED ON THE ALGOL PROCEDURE ORTBAK,           COM18850
C     NUM. MATH. 12, 349-368(1968) BY MARTIN AND WILKINSON.             COM18860
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 339-358(1971).   COM18870
C                                                                       COM18880
C SUBROUTINES OR FUNCTIONS CALLED:  KACHEL                              COM18890
C                                                                       COM18900
C     VERSION FOR THE IBM 3090VF DATED NOVEMBER 1987.                   COM18910
C     ------------------------------------------------------------------COM18920
      INTEGER     I, J, K, KACHEL                                       COM18930
      REAL*8      TR, TI, D, T                                          COM18940
C                                                                       COM18950
      CALL XUFLOW(0)                                                    COM18960
      IF (M.EQ.0) GO TO 1000                                            COM18970
      IF (M.GT.KACHEL(LD)) GO TO 180                                    COM18980
C PATH FOR SMALL ARRAYS                                                 COM18990
         DO 160 K=IGH-2,LOW,-1                                          COM19000
         T=ORTR(K+1)                                                    COM19010
         IF(T.EQ.0D0) GO TO 160                                         COM19020
         D=1D0/T                                                        COM19030
C"    ( PREFER VECTOR                                                   COM19040
            DO 140 J=1,M                                                COM19050
            TR=T*ZR(K+1,J)                                              COM19060
            TI=T*ZI(K+1,J)                                              COM19070
               DO 100 I=K+2,IGH                                         COM19080
               TR=TR+AR(I,K)*ZR(I,J)+AI(I,K)*ZI(I,J)                    COM19090
100            TI=TI+AR(I,K)*ZI(I,J)-AI(I,K)*ZR(I,J)                    COM19100
            ZR(K+1,J)=ZR(K+1,J)+TR                                      COM19110
            ZI(K+1,J)=ZI(K+1,J)+TI                                      COM19120
            TR=D*TR                                                     COM19130
            TI=D*TI                                                     COM19140
               DO 120 I=K+2,IGH                                         COM19150
               ZR(I,J)=ZR(I,J)+TR*AR(I,K)-TI*AI(I,K)                    COM19160
120            ZI(I,J)=ZI(I,J)+TR*AI(I,K)+TI*AR(I,K)                    COM19170
140         CONTINUE                                                    COM19180
160      CONTINUE                                                       COM19190
      GO TO 1000                                                        COM19200
C PATH FOR LARGE ARRAYS                                                 COM19210
180      DO 260 K=IGH-2,LOW,-1                                          COM19220
         T=ORTR(K+1)                                                    COM19230
         IF(T.EQ.0D0) GO TO 260                                         COM19240
         D=1D0/T                                                        COM19250
            DO 240 J=1,M                                                COM19260
            TR=T*ZR(K+1,J)                                              COM19270
            TI=T*ZI(K+1,J)                                              COM19280
               DO 200 I=K+2,IGH                                         COM19290
               TR=TR+AR(I,K)*ZR(I,J)+AI(I,K)*ZI(I,J)                    COM19300
200            TI=TI+AR(I,K)*ZI(I,J)-AI(I,K)*ZR(I,J)                    COM19310
            ZR(K+1,J)=ZR(K+1,J)+TR                                      COM19320
            ZI(K+1,J)=ZI(K+1,J)+TI                                      COM19330
            TR=D*TR                                                     COM19340
            TI=D*TI                                                     COM19350
               DO 220 I=K+2,IGH                                         COM19360
               ZR(I,J)=ZR(I,J)+TR*AR(I,K)-TI*AI(I,K)                    COM19370
220            ZI(I,J)=ZI(I,J)+TR*AI(I,K)+TI*AR(I,K)                    COM19380
240         CONTINUE                                                    COM19390
260      CONTINUE                                                       COM19400
C                                                                       COM19410
1000  RETURN                                                            COM19420
      END                                                               COM19430
