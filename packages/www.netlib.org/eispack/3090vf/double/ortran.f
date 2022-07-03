@PROCESS DIRECTIVE('"    (')                                            REA14470
C-----------------------------------------------------------------------REA14480
C RIGHT MATRIX OF THE SIMILARITY TRANSFORMATION TO HESSENBERG FORM      REA14490
C PERFORMED BY SUBROUTINE ORTHES.                                       REA14500
C                                                                       REA14510
      SUBROUTINE  ORTRAN ( LD, N, LOW, IGH, A, ORT, Z )                 REA14520
C                                                                       REA14530
      INTEGER     LD, N, IGH, LOW                                       REA14540
      REAL*8      A(LD,IGH), ORT(IGH), Z(LD,N)                          REA14550
C                                                                       REA14560
C LD     E  FIRST DIMENSION ASSIGNED TO THE TWO-DIMENSIONAL ARRAY       REA14570
C           PARAMETERS IN THE CALLING PROGRAM.                          REA14580
C                                                                       REA14590
C N      E  ORDER OF THE MATRIX TRANSFORMED BY ORTHES.                  REA14600
C                                                                       REA14610
C LOW    E  SUBSCRIPT OBTAINED FROM SUBROUTINE BALANC.  IF BALANC WAS   REA14620
C           NOT USED, SET LOW = 1.                                      REA14630
C                                                                       REA14640
C IGH    E  SUBSCRIPT OBTAINED FROM SUBROUTINE BALANC.  IF BALANC WAS   REA14650
C           NOT USED, SET IGH = ORDER OF THE MATRIX REDUCED BY ORTHES.  REA14660
C                                                                       REA14670
C A      E  INFORMATION ABOUT THE OPERATIONS PERFORMED BY ORTHES, IN THEREA14680
C           SUBDIAGONAL PART OF THE ARRAY.                              REA14690
C        R  RIGHT MATRIX OF THE SIMILARITY TRANSFORMATION IF Z AND A    REA14700
C           ARE SPECIFIED TO COINCIDE IN STORAGE.                       REA14710
C                                                                       REA14720
C ORT    E  ADDITIONAL INFORMATION ABOUT THE OPERATIONS PERFORMED BY    REA14730
C           ORTHES, IN CELLS  ORT(LOW),...,ORT(IGH).                    REA14740
C           THE INFORMATION CONTAINED IN ORT IS PRESERVED.              REA14750
C                                                                       REA14760
C Z      R  RIGHT MATRIX OF THE SIMILARITY TRANSFORMATION PERFORMED BY  REA14770
C           ORTHES.                                                     REA14780
C           Z AND A MAY COINCIDE IN STORAGE.                            REA14790
C                                                                       REA14800
C     THIS SUBROUTINE IS BASED ON THE ALGOL PROCEDURE ORTRANS,          REA14810
C     NUM. MATH. 16, 181-204(1970) BY PETERS AND WILKINSON.             REA14820
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 372-395(1971).   REA14830
C                                                                       REA14840
C     VERSION FOR THE IBM 3090VF DATED NOVEMBER 1987.                   REA14850
C     ------------------------------------------------------------------REA14860
      INTEGER     I, J, K, KK                                           REA14870
      REAL*8      S, T                                                  REA14880
C                                                                       REA14890
      CALL XUFLOW(0)                                                    REA14900
C INITIAL SETTING FOR THE IDENTITY MATRIX                               REA14910
         DO 120 J=1,LOW-1                                               REA14920
            DO 100 I=1,N                                                REA14930
100         Z(I,J)=0D0                                                  REA14940
120      Z(J,J)=1D0                                                     REA14950
      KK=IGH-KACHEL(LD)                                                 REA14960
         DO 160 J=MAX0(LOW,IGH-1),N                                     REA14970
            DO 140 I=1,N                                                REA14980
140         Z(I,J)=0D0                                                  REA14990
160      Z(J,J)=1D0                                                     REA15000
         DO 520 K=IGH-1,LOW+1,-1                                        REA15010
         Z(K,K)=ORT(K)                                                  REA15020
         IF(ORT(K).EQ.0D0) GO TO 480                                    REA15030
            DO 200 I=K+1,IGH                                            REA15040
200         Z(I,K)=A(I,K-1)                                             REA15050
         T=1D0/Z(K,K)                                                   REA15060
         IF(K.LT.KK) GO TO 400                                          REA15070
C PATH FOR SMALL ARRAYS                                                 REA15080
C"    ( IGNORE RECRDEPS                                                 REA15090
C"    ( PREFER VECTOR                                                   REA15100
            DO 260 J=K+1,IGH                                            REA15110
            S=0D0                                                       REA15120
               DO 220 I=K,IGH                                           REA15130
220            S=S+Z(I,K)*Z(I,J)                                        REA15140
            S=S*T                                                       REA15150
               DO 240 I=K,IGH                                           REA15160
240            Z(I,J)=Z(I,J)+S*Z(I,K)                                   REA15170
260         CONTINUE                                                    REA15180
         GO TO 480                                                      REA15190
C PATH FOR LARGE ARRAYS                                                 REA15200
400         DO 460 J=K+1,IGH                                            REA15210
            S=0D0                                                       REA15220
               DO 420 I=K,IGH                                           REA15230
420            S=S+Z(I,K)*Z(I,J)                                        REA15240
            S=S*T                                                       REA15250
               DO 440 I=K,IGH                                           REA15260
440            Z(I,J)=Z(I,J)+S*Z(I,K)                                   REA15270
460         CONTINUE                                                    REA15280
480      Z(K,K)=Z(K,K)+1D0                                              REA15290
            DO 500 I=1,N                                                REA15300
500         Z(I,K-1)=0D0                                                REA15310
         Z(K-1,K-1)=1D0                                                 REA15320
520      CONTINUE                                                       REA15330
1000  RETURN                                                            REA15340
      END                                                               REA15350
