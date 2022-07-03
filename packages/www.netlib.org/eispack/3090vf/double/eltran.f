C-----------------------------------------------------------------------REA01610
C RIGHT MATRIX OF THE SIMILARITY TRANSFORMATION TO HESSENBERG FORM      REA01620
C PERFORMED BY ELMHES.                                                  REA01630
C                                                                       REA01640
      SUBROUTINE  ELTRAN ( LD, N, LOW, IGH, A, INT, Z )                 REA01650
C                                                                       REA01660
      INTEGER     N, LD, IGH, LOW, INT(IGH)                             REA01670
      REAL*8      A(LD,IGH), Z(LD,N)                                    REA01680
C                                                                       REA01690
C LD     E  FIRST DIMENSION ASSIGNED TO THE TWO-DIMENSIONAL ARRAY       REA01700
C           PARAMETERS IN THE CALLING PROGRAM.                          REA01710
C                                                                       REA01720
C N      E  ORDER OF THE MATRIX.                                        REA01730
C                                                                       REA01740
C LOW    E  SUBSCRIPT RETURNED BY SUBROUTINE BALANC.  IF BALANC WAS NOT REA01750
C           USED, SET  LOW = 1.                                         REA01760
C                                                                       REA01770
C IGH    E  SUBSCRIPT RETURNED BY SUBROUTINE BALANC.  IF BALANC WAS NOT REA01780
C           USED, SET  IGH = ORDER OF THE MATRIX REDUCED BY ELMHES.     REA01790
C                                                                       REA01800
C A      E  GAUSSIAN MULTIPLIERS PRODUCED BY ELMHES IN THE CELLS BELOW  REA01810
C           THE SUBDIAGONAL.                                            REA01820
C                                                                       REA01830
C INT    E  RECORD OF THE ROW AND COLUMN INTERCHANGES PERFORMED BY      REA01840
C           ELMHES, IN CELLS LOW,...,IGH.                               REA01850
C                                                                       REA01860
C Z      E  RIGHT MATRIX OF THE SIMILARITY TRANSFORMATION PERFORMED BY  REA01870
C           ELMHES.                                                     REA01880
C                                                                       REA01890
C     THIS SUBROUTINE IS BASED ON THE ALGOL PROCEDURE ELMTRANS,         REA01900
C     NUM. MATH. 16, 181-204(1970) BY PETERS AND WILKINSON.             REA01910
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 372-395(1971).   REA01920
C                                                                       REA01930
C SUBPROGRAMS CALLED:  KACHEL                                           REA01940
C                                                                       REA01950
C     VERSION FOR THE IBM 3090VF DATED NOVEMBER 1987.                   REA01960
C     ------------------------------------------------------------------REA01970
      INTEGER     I, J, K                                               REA01980
      INTEGER     KACHEL                                                REA01990
      REAL*8      T                                                     REA02000
C                                                                       REA02010
      CALL XUFLOW(0)                                                    REA02020
         DO 120 J=1,N                                                   REA02030
            DO 100 I=1,N                                                REA02040
100         Z(I,J)=0D0                                                  REA02050
120      Z(J,J)=1D0                                                     REA02060
      IF(IGH-LOW.GT.2*KACHEL(LD)) GO TO 200                             REA02070
C PATH FOR SMALL ARRAYS                                                 REA02080
         DO 180 K=IGH-1,LOW+1,-1                                        REA02090
            DO 140 I=K+1,IGH                                            REA02100
140         Z(I,K)=A(I,K-1)                                             REA02110
         I=INT(K)                                                       REA02120
         IF(I.EQ.K) GO TO 180                                           REA02130
            DO 160 J=K,IGH                                              REA02140
            Z(K,J)=Z(I,J)                                               REA02150
160         Z(I,J)=0D0                                                  REA02160
         Z(I,K)=1D0                                                     REA02170
180      CONTINUE                                                       REA02180
      GO TO 1000                                                        REA02190
C PATH FOR LARGE ARRAYS                                                 REA02200
200      DO 300 K=IGH-1,LOW+1,-1                                        REA02210
            DO 260 I=K+1,IGH                                            REA02220
260         Z(I,K)=A(I,K-1)                                             REA02230
            DO 280 I=K,LOW+1,-1                                         REA02240
            J=INT(I)                                                    REA02250
            T=Z(I,K)                                                    REA02260
            Z(I,K)=Z(J,K)                                               REA02270
            Z(J,K)=T                                                    REA02280
280         CONTINUE                                                    REA02290
300      CONTINUE                                                       REA02300
         DO 320 I=IGH-1,LOW+1,-1                                        REA02310
         T=Z(I,IGH)                                                     REA02320
         Z(I,IGH)=Z(INT(I),IGH)                                         REA02330
         Z(INT(I),IGH)=T                                                REA02340
320      CONTINUE                                                       REA02350
1000  RETURN                                                            REA02360
      END                                                               REA02370
