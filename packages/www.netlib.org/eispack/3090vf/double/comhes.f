@PROCESS DIRECTIVE('"    (')                                            COM03280
C-----------------------------------------------------------------------COM03290
C REDUCTION OF A COMPLEX MATRIX TO UPPER-HESSENBERG FORM BY STABILIZED  COM03300
C ELEMENTARY SIMILARITY TRANSFORMATIONS.                                COM03310
C                                                                       COM03320
      SUBROUTINE  COMHES ( LD, N, LOW, IGH, AR, AI, INT )               COM03330
C                                                                       COM03340
      INTEGER     LD, N, LOW, IGH, INT(IGH)                             COM03350
      REAL*8      AR(LD,N), AI(LD,N)                                    COM03360
C                                                                       COM03370
C LD     E  FIRST DIMENSION ASSIGNED TO THE TWO-DIMENSIONAL ARRAY       COM03380
C           PARAMETERS IN THE CALLING PROGRAM.                          COM03390
C                                                                       COM03400
C N      E  ORDER OF THE MATRIX.                                        COM03410
C                                                                       COM03420
C LOW    E  PARAMETER DETERMINED BY THE BALANCING SUBROUTINE CBAL.      COM03430
C           IF CBAL WAS NOT USED, SET LOW = 1.                          COM03440
C                                                                       COM03450
C IGH    E  PARAMETER DETERMINED BY THE BALANCING SUBROUTINE CBAL.      COM03460
C           IF CBAL WAS NOT USED, SET IGH = N.                          COM03470
C                                                                       COM03480
C AR, AI E  REAL AND IMAGINARY PARTS OF THE MATRIX TO BE REDUCED.       COM03490
C        R  REAL AND IMAGINARY PARTS OF THE HESSENBERG MATRIX, AND      COM03500
C           ELIMINATION MULTIPLIERS IN THE SUB-HESSENBERG PARTS OF THE  COM03510
C           ARRAYS.                                                     COM03520
C                                                                       COM03530
C INT    R  RECORD OF ROW AND COLUMN INTERCHANGES IN CELLS LOW,...,IGH. COM03540
C                                                                       COM03550
C     THIS SUBROUTINE IS BASED ON THE ALGOL PROCEDURE COMHES,           COM03560
C     NUM. MATH. 12, 349-368(1968) BY MARTIN AND WILKINSON.             COM03570
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 339-358(1971).   COM03580
C                                                                       COM03590
C     VERSION FOR THE IBM 3090VF DATED NOVEMBER 1987.                   COM03600
C     ------------------------------------------------------------------COM03610
      INTEGER     I, J, K, IP                                           COM03620
      REAL*8      S, T, TR, TI, WR, WI                                  COM03630
      LOGICAL     NOEXCH                                                COM03640
C                                                                       COM03650
      CALL XUFLOW(0)                                                    COM03660
         DO 400 K=LOW,IGH-2                                             COM03670
C PIVOT ROW                                                             COM03680
         IP=K+1                                                         COM03690
         S=DABS(AR(K+1,K))+DABS(AI(K+1,K))                              COM03700
            DO 100 I=K+2,IGH                                            COM03710
            T=DABS(AR(I,K))+DABS(AI(I,K))                               COM03720
            IF(T.LE.S) GO TO 100                                        COM03730
               S=T                                                      COM03740
               IP=I                                                     COM03750
100         CONTINUE                                                    COM03760
         INT(K+1)=IP                                                    COM03770
         IF(S.EQ.0D0) GO TO 400                                         COM03780
         TR=AR(IP,K)                                                    COM03790
         TI=AI(IP,K)                                                    COM03800
         NOEXCH=IP.EQ.K+1                                               COM03810
         IF(NOEXCH) GO TO 140                                           COM03820
C ROW EXCHANGE IN COLUMN K                                              COM03830
            AR(IP,K)=AR(K+1,K)                                          COM03840
            AR(K+1,K)=TR                                                COM03850
            AI(IP,K)=AI(K+1,K)                                          COM03860
            AI(K+1,K)=TI                                                COM03870
C COLUMN EXCHANGE                                                       COM03880
            DO 120 I=1,IGH                                              COM03890
            WR=AR(I,K+1)                                                COM03900
            AR(I,K+1)=AR(I,IP)                                          COM03910
            AR(I,IP)=WR                                                 COM03920
            WI=AI(I,K+1)                                                COM03930
            AI(I,K+1)=AI(I,IP)                                          COM03940
120         AI(I,IP)=WI                                                 COM03950
C ROW EXCHANGE IN COLUMN K+1                                            COM03960
         WR=AR(IP,K+1)                                                  COM03970
         AR(IP,K+1)=AR(K+1,K+1)                                         COM03980
         AR(K+1,K+1)=WR                                                 COM03990
         WI=AI(IP,K+1)                                                  COM04000
         AI(IP,K+1)=AI(K+1,K+1)                                         COM04010
         AI(K+1,K+1)=WI                                                 COM04020
C GAUSSIAN MULTIPLIERS                                                  COM04030
         TR=AR(K+1,K)                                                   COM04040
         TI=AI(K+1,K)                                                   COM04050
140      IF(DABS(TR).LE.DABS(TI)) GO TO 180                             COM04060
            S=TI/TR                                                     COM04070
            T=TR+S*TI                                                   COM04080
               DO 160 I=K+2,IGH                                         COM04090
               WR=AR(I,K)                                               COM04100
               WI=AI(I,K)                                               COM04110
               AR(I,K)=(WR+S*WI)/T                                      COM04120
160            AI(I,K)=(WI-S*WR)/T                                      COM04130
            GO TO 220                                                   COM04140
180      S=TR/TI                                                        COM04150
         T=TI+S*TR                                                      COM04160
            DO 200 I=K+2,IGH                                            COM04170
            WR=AR(I,K)                                                  COM04180
            WI=AI(I,K)                                                  COM04190
            AR(I,K)=(S*WR+WI)/T                                         COM04200
200         AI(I,K)=(S*WI-WR)/T                                         COM04210
C TRANSFORMATION OF COLUMNS K+1,...,N                                   COM04220
220         DO 340 J=K+2,N                                              COM04230
C ROW EXCHANGE                                                          COM04240
            TR=AR(IP,J)                                                 COM04250
            TI=AI(IP,J)                                                 COM04260
            IF(NOEXCH) GO TO 260                                        COM04270
               AR(IP,J)=AR(K+1,J)                                       COM04280
               AR(K+1,J)=TR                                             COM04290
               AI(IP,J)=AI(K+1,J)                                       COM04300
               AI(K+1,J)=TI                                             COM04310
C CONTRIBUTION OF COLUMN J TO THE RIGHT TRANSFORMATION (COLUMN K+1)     COM04320
260         IF(J.GT.IGH) GO TO 300                                      COM04330
C"    ( IGNORE RECRDEPS                                                 COM04340
               DO 280 I=1,IGH                                           COM04350
               AR(I,K+1)=AR(I,K+1)+AR(I,J)*AR(J,K)-AI(I,J)*AI(J,K)      COM04360
280            AI(I,K+1)=AI(I,K+1)+AI(I,J)*AR(J,K)+AR(I,J)*AI(J,K)      COM04370
C TRANSFORMATION OF COLUMN J                                            COM04380
300            DO 320 I=K+2,IGH                                         COM04390
               AR(I,J)=AR(I,J)-TR*AR(I,K)+TI*AI(I,K)                    COM04400
320            AI(I,J)=AI(I,J)-TI*AR(I,K)-TR*AI(I,K)                    COM04410
340         CONTINUE                                                    COM04420
C TRANSFORMATION OF COLUMN K+1                                          COM04430
         TR=AR(K+1,K+1)                                                 COM04440
         TI=AI(K+1,K+1)                                                 COM04450
            DO 360 I=K+2,IGH                                            COM04460
            AR(I,K+1)=AR(I,K+1)-TR*AR(I,K)+TI*AI(I,K)                   COM04470
360         AI(I,K+1)=AI(I,K+1)-TI*AR(I,K)-TR*AI(I,K)                   COM04480
400      CONTINUE                                                       COM04490
      RETURN                                                            COM04500
      END                                                               COM04510
