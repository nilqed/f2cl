@PROCESS DIRECTIVE('"    (')                                            COM19440
C-----------------------------------------------------------------------COM19450
C REDUCTION OF A COMPLEX MATRIX TO UPPER-HESSENBERG FORM BY UNITARY     COM19460
C SIMILARITY TRANSFORMATIONS (HOUSEHOLDER).                             COM19470
C                                                                       COM19480
      SUBROUTINE  CORTH ( LD, N, LOW, IGH, AR, AI, ORTR, ORTI )         COM19490
C                                                                       COM19500
      INTEGER     LD, LOW, IGH, N                                       COM19510
      REAL*8      AR(LD,N), AI(LD,N), ORTR(IGH), ORTI(IGH)              COM19520
C                                                                       COM19530
C LD     E  FIRST DIMENSION ASSIGNED TO THE TWO-DIMENSIONAL ARRAY       COM19540
C           PARAMETERS IN THE CALLING PROGRAM.                          COM19550
C                                                                       COM19560
C N      E  ORDER OF THE MATRIX.                                        COM19570
C                                                                       COM19580
C LOW    E  PARAMETER DETERMINED BY THE BALANCING SUBROUTINE CBAL.      COM19590
C           IF  CBAL  WHAS NOT USED,  SET  LOW = 1.                     COM19600
C                                                                       COM19610
C IGH    E  PARAMETER DETERMINED BY THE BALANCING SUBROUTINE CBAL.      COM19620
C           IF  CBAL  WHAS NOT USED,  SET  IGH = ORDER OF THE MATRIX.   COM19630
C                                                                       COM19640
C AR,AI  E  REAL AND IMAGINARY PARTS OF THE COMPLEX MATRIX.             COM19650
C        R  HESSENBERG MATRIX AND INFORMATION ON THE REDUCTION OPERATORSCOM19660
C           IN THE SUB-HESSENBERG PARTS OF THE ARRAYS.                  COM19670
C                                                                       COM19680
C ORTR   R  ADDITIONAL INFORMATION ON THE REDUCTION OPERATORS IN CELLS  COM19690
C           LOW,...,IGH.                                                COM19700
C                                                                       COM19710
C ORTI   -  ANCILLARY STORAGE                                           COM19720
C                                                                       COM19730
C     THIS SUBROUTINE IS BASED ON THE ALGOL PROCEDURE ORTHES,           COM19740
C     NUM. MATH. 12, 349-368(1968) BY MARTIN AND WILKINSON.             COM19750
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 339-358(1971).   COM19760
C                                                                       COM19770
C SUBPROGRAMS CALLED:  DSC16   KACHEL                                   COM19780
C                                                                       COM19790
C     VERSION FOR THE IBM 3090VF DATED NOVEMBER 1987.                   COM19800
C     ------------------------------------------------------------------COM19810
      INTEGER     I, J, K, KK, KACHEL                                   COM19820
      REAL*8      C, D, R, S, T, S16, R16, TR, TI                       COM19830
C                                                                       COM19840
      CALL XUFLOW(0)                                                    COM19850
      KK=N-KACHEL(LD)/2                                                 COM19860
         DO 440 K=LOW,IGH-2                                             COM19870
C SCALED HOUSEHOLDER VECTOR                                             COM19880
         S=0D0                                                          COM19890
            DO 100 I=K+2,IGH                                            COM19900
100         S=S+DABS(AR(I,K))+DABS(AI(I,K))                             COM19910
         ORTR(K+1)=0D0                                                  COM19920
         IF(S.EQ.0D0) GO TO 440                                         COM19930
         S=S+DABS(AR(K+1,K))+DABS(AI(K+1,K))                            COM19940
         CALL DSC16 (S, S16, R16)                                       COM19950
         T=0D0                                                          COM19960
            DO 120 I=K+1,IGH                                            COM19970
            TR=R16*AR(I,K)                                              COM19980
            TI=R16*AI(I,K)                                              COM19990
            ORTR(I)=TR                                                  COM20000
            ORTI(I)=TI                                                  COM20010
120         T=T+TR*TR+TI*TI                                             COM20020
         T=DSQRT(T)                                                     COM20030
         TR=ORTR(K+1)                                                   COM20040
         TI=ORTI(K+1)                                                   COM20050
         IF(TR.NE.0D0 .OR. TI.NE.0D0) GO TO 140                         COM20060
            C=1D0                                                       COM20070
            S=0D0                                                       COM20080
            D=-1D0                                                      COM20090
            GO TO 160                                                   COM20100
140      D=DMAX1(DABS(TR),DABS(TI))                                     COM20110
         C=TR/D                                                         COM20120
         S=TI/D                                                         COM20130
         R=DSQRT(C*C+S*S)                                               COM20140
         C=C/R                                                          COM20150
         S=S/R                                                          COM20160
         D=-(1D0+(D*R)/T)                                               COM20170
160      ORTR(K+1)=D                                                    COM20180
         ORTI(K+1)=0D0                                                  COM20190
C SUBDIAGONAL ELEMENT OF THE HESSENBERG MATRIX                          COM20200
         R=-S16*T                                                       COM20210
         AR(K+1,K)=R*C                                                  COM20220
         AI(K+1,K)=R*S                                                  COM20230
C SCALING                                                               COM20240
         C=-C/T                                                         COM20250
         S= S/T                                                         COM20260
            DO 180 I=K+2,IGH                                            COM20270
            TR=ORTR(I)                                                  COM20280
            TI=ORTI(I)                                                  COM20290
            ORTR(I)=C*TR-S*TI                                           COM20300
            ORTI(I)=S*TR+C*TI                                           COM20310
            AR(I,K)=ORTR(I)                                             COM20320
180         AI(I,K)=ORTI(I)                                             COM20330
C NORMALIZING FACTOR OF THE REFLECTOR                                   COM20340
         D=1D0/D                                                        COM20350
C LEFT TRANSFORMATION                                                   COM20360
         IF(K.LT.KK) GO TO 280                                          COM20370
C    PATH FOR SMALL ARRAYS                                              COM20380
C"    ( IGNORE RECRDEPS                                                 COM20390
C"    ( PREFER VECTOR                                                   COM20400
            DO 260 J=K+1,N                                              COM20410
            TR=0D0                                                      COM20420
            TI=0D0                                                      COM20430
               DO 220 I=K+1,IGH                                         COM20440
               TR=TR+ORTR(I)*AR(I,J)+ORTI(I)*AI(I,J)                    COM20450
220            TI=TI+ORTR(I)*AI(I,J)-ORTI(I)*AR(I,J)                    COM20460
            TR=D*TR                                                     COM20470
            TI=D*TI                                                     COM20480
               DO 240 I=K+1,IGH                                         COM20490
               AR(I,J)=AR(I,J)+TR*ORTR(I)-TI*ORTI(I)                    COM20500
240            AI(I,J)=AI(I,J)+TI*ORTR(I)+TR*ORTI(I)                    COM20510
260         CONTINUE                                                    COM20520
         GO TO 360                                                      COM20530
C    PATH FOR LARGE ARRAYS                                              COM20540
280         DO 340 J=K+1,N                                              COM20550
            TR=0D0                                                      COM20560
            TI=0D0                                                      COM20570
               DO 300 I=K+1,IGH                                         COM20580
               TR=TR+ORTR(I)*AR(I,J)+ORTI(I)*AI(I,J)                    COM20590
300            TI=TI+ORTR(I)*AI(I,J)-ORTI(I)*AR(I,J)                    COM20600
            TR=D*TR                                                     COM20610
            TI=D*TI                                                     COM20620
               DO 320 I=K+1,IGH                                         COM20630
               AR(I,J)=AR(I,J)+TR*ORTR(I)-TI*ORTI(I)                    COM20640
320            AI(I,J)=AI(I,J)+TI*ORTR(I)+TR*ORTI(I)                    COM20650
340         CONTINUE                                                    COM20660
C RIGHT TRANSFORMATION                                                  COM20670
C"    ( IGNORE RECRDEPS                                                 COM20680
360         DO 420 I=1,IGH                                              COM20690
            TR=0D0                                                      COM20700
            TI=0D0                                                      COM20710
               DO 380 J=K+1,IGH                                         COM20720
               TR=TR+ORTR(J)*AR(I,J)-ORTI(J)*AI(I,J)                    COM20730
380            TI=TI+ORTR(J)*AI(I,J)+ORTI(J)*AR(I,J)                    COM20740
            TR=D*TR                                                     COM20750
            TI=D*TI                                                     COM20760
               DO 400 J=K+1,IGH                                         COM20770
               AR(I,J)=AR(I,J)+TR*ORTR(J)+TI*ORTI(J)                    COM20780
400            AI(I,J)=AI(I,J)+TI*ORTR(J)-TR*ORTI(J)                    COM20790
420         CONTINUE                                                    COM20800
440      CONTINUE                                                       COM20810
      RETURN                                                            COM20820
      END                                                               COM20830
