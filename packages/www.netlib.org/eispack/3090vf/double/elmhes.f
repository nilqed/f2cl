@PROCESS DIRECTIVE('"    (')                                            REA00630
C-----------------------------------------------------------------------REA00640
C REDUCTION OF A GENERAL MATRIX TO UPPER-HESSENBERG FORM BY STABILIZED  REA00650
C ELEMENTARY SIMILARITY TRANSFORMATIONS                                 REA00660
C                                                                       REA00670
      SUBROUTINE  ELMHES ( LD, N, LOW, IGH, A, INT )                    REA00680
C                                                                       REA00690
      INTEGER        LD, N, LOW, IGH, INT(IGH)                          REA00700
      REAL*8         A(LD,N)                                            REA00710
C                                                                       REA00720
C LD     E  FIRST DIMENSION ASSIGNED TO THE TWO-DIMENSIONAL ARRAY       REA00730
C           PARAMETERS IN THE CALLING PROGRAM.                          REA00740
C                                                                       REA00750
C N      E  ORDER OF THE MATRIX.                                        REA00760
C                                                                       REA00770
C LOW    E  SUBSCRIPT DETERMINED BY SUBROUTINE BALANC.  IF BALANC WAS   REA00780
C           NOT USED, SET LOW = 1.                                      REA00790
C                                                                       REA00800
C IGH    E  SUBSCRIPT DETERMINED BY SUBROUTINE BALANC.  IF BALANC WAS   REA00810
C           NOT USED, SET IGH = ORDER OF THE MATRIX.                    REA00820
C                                                                       REA00830
C A      E  MATRIX TO BE REDUCED.                                       REA00840
C        R  UPPER-HESSENBERG MATRIX AND ELIMINATION MULTIPLIERS IN THE  REA00850
C           SUB-HESSENBERG PART OF THE ARRAY.                           REA00860
C                                                                       REA00870
C INT    R  INFORMATION ABOUT ROW AND COLUMN INTERCHANGES IN CELLS      REA00880
C           LOW,...,IGH.                                                REA00890
C                                                                       REA00900
C     THIS SUBROUTINE IS BASED ON THE ALGOL PROCEDURE ELMHES,           REA00910
C     NUM. MATH. 12, 349-368(1968) BY MARTIN AND WILKINSON.             REA00920
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 339-358(1971).   REA00930
C                                                                       REA00940
C     VERSION FOR THE IBM 3090VF DATED NOVEMBER 1987.                   REA00950
C     ------------------------------------------------------------------REA00960
      INTEGER     I, J, K, IP                                           REA00970
      REAL*8      T                                                     REA00980
      LOGICAL     NOEXCH                                                REA00990
C                                                                       REA01000
      CALL XUFLOW(0)                                                    REA01010
         DO 400 K=LOW,IGH-2                                             REA01020
C PIVOT ROW                                                             REA01030
         IP=K+1                                                         REA01040
            DO 100 I=K+2,IGH                                            REA01050
            IF(DABS(A(I,K)).GT.DABS(A(IP,K))) IP=I                      REA01060
100         CONTINUE                                                    REA01070
         IF(A(IP,K).EQ.0D0) GO TO 400                                   REA01080
         NOEXCH=IP.EQ.K+1                                               REA01090
         IF(NOEXCH) GO TO 140                                           REA01100
C ROW EXCHANGE IN COLUMN K                                              REA01110
         T=A(IP,K)                                                      REA01120
         A(IP,K)=A(K+1,K)                                               REA01130
         A(K+1,K)=T                                                     REA01140
C COLUMN EXCHANGE                                                       REA01150
            DO 120 I=1,IGH                                              REA01160
            T=A(I,K+1)                                                  REA01170
            A(I,K+1)=A(I,IP)                                            REA01180
120         A(I,IP)=T                                                   REA01190
C ROW EXCHANGE IN COLUMN K+1                                            REA01200
         T=A(IP,K+1)                                                    REA01210
         A(IP,K+1)=A(K+1,K+1)                                           REA01220
         A(K+1,K+1)=T                                                   REA01230
C GAUSSIAN MULTIPLIERS                                                  REA01240
140      T=A(K+1,K)                                                     REA01250
            DO 160 I=K+2,IGH                                            REA01260
160         A(I,K)=A(I,K)/T                                             REA01270
C TRANSFORMATION OF COLUMNS K+1,...,IGH                                 REA01280
            DO 240 J=K+2,IGH                                            REA01290
C    ROW EXCHANGE                                                       REA01300
            T=A(IP,J)                                                   REA01310
            IF(NOEXCH) GO TO 180                                        REA01320
            A(IP,J)=A(K+1,J)                                            REA01330
            A(K+1,J)=T                                                  REA01340
C    CONTRIBUTION OF COLUMN J TO THE RIGHT TRANSFORMATION (COLUMN K+1)  REA01350
C"    ( IGNORE RECRDEPS                                                 REA01360
180            DO 200 I=1,IGH                                           REA01370
200            A(I,K+1)=A(I,K+1)+A(I,J)*A(J,K)                          REA01380
C    TRANSFORMATION OF COLUMN J                                         REA01390
            T=-T                                                        REA01400
               DO 220 I=K+2,IGH                                         REA01410
220            A(I,J)=A(I,J)+T*A(I,K)                                   REA01420
240         CONTINUE                                                    REA01430
C    TRANSFORMATION OF COLUMN K+1                                       REA01440
         T=-A(K+1,K+1)                                                  REA01450
            DO 260 I=K+2,IGH                                            REA01460
260         A(I,K+1)=A(I,K+1)+T*A(I,K)                                  REA01470
C    TRANSFORMATION OF COLUMNS IGH+1,...,N                              REA01480
            DO 320 J=IGH+1,N                                            REA01490
            T=A(IP,J)                                                   REA01500
            IF(NOEXCH) GO TO 280                                        REA01510
               A(IP,J)=A(K+1,J)                                         REA01520
               A(K+1,J)=T                                               REA01530
280         T=-T                                                        REA01540
               DO 300 I=K+2,IGH                                         REA01550
300            A(I,J)=A(I,J)+T*A(I,K)                                   REA01560
320         CONTINUE                                                    REA01570
400      INT(K+1)=IP                                                    REA01580
      RETURN                                                            REA01590
      END                                                               REA01600
