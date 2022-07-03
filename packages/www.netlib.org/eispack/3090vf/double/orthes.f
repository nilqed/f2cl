@PROCESS DIRECTIVE('"    (')                                            REA13470
C-----------------------------------------------------------------------REA13480
C REDUCTION OF A GENERAL MATRIX TO UPPER-HESSENBERG FORM BY             REA13490
C ORTHOGONAL SIMILARITY TRANSFORMATIONS.                                REA13500
C                                                                       REA13510
      SUBROUTINE  ORTHES ( LD, N, LOW, IGH, A, ORT )                    REA13520
C                                                                       REA13530
      INTEGER        LD, N, LOW, IGH                                    REA13540
      REAL*8         A(LD,N), ORT(IGH)                                  REA13550
C                                                                       REA13560
C LD     E  FIRST DIMENSION ASSIGNED TO THE TWO-DIMENSIONAL ARRAY       REA13570
C           PARAMETERS IN THE CALLING PROGRAM.                          REA13580
C                                                                       REA13590
C N      E  ORDER OF THE MATRIX.                                        REA13600
C                                                                       REA13610
C LOW    E  SUBSCRIPT OBTAINED FROM SUBROUTINE BALANC.  IF BALANC WAS   REA13620
C           NOT USED, SET LOW = 1.                                      REA13630
C                                                                       REA13640
C IGH    E  SUBSCRIPT OBTAINED FROM SUBROUTINE BALANC.  IF BALANC WAS   REA13650
C           NOT USED, SET IGH = ORDER OF THE MATRIX.                    REA13660
C                                                                       REA13670
C A      E  MATRIX TO BE REDUCED.                                       REA13680
C        R  REDUCED MATRIX AND INFORMATION ON THE REDUCING OPERATORS IN REA13690
C           THE SUB-HESSENBERG PART OF THE ARRAY.                       REA13700
C                                                                       REA13710
C ORT    R  ADDITIONAL INFORMATION ABOUT THE TRANSFORMATIONS IN         REA13720
C           ORT(LOW),...,ORT(IGH)  (NONTRIVIAL LEADING ELEMENTS OF THE  REA13730
C           HOUSEHOLDER VECTORS).                                       REA13740
C                                                                       REA13750
C     THIS SUBROUTINE IS BASED ON THE ALGOL PROCEDURE ORTHES,           REA13760
C     NUM. MATH. 12, 349-368(1968) BY MARTIN AND WILKINSON.             REA13770
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 339-358(1971).   REA13780
C                                                                       REA13790
C     VERSION FOR THE IBM 3090VF DATED NOVEMBER 1987.                   REA13800
C     ------------------------------------------------------------------REA13810
      INTEGER        I, J, K, KK                                        REA13820
      REAL*8         T, D, S, S16, R16                                  REA13830
C                                                                       REA13840
      CALL XUFLOW(0)                                                    REA13850
      KK=N-KACHEL(LD)                                                   REA13860
         DO 360 K=LOW,IGH-2                                             REA13870
         S=0D0                                                          REA13880
            DO 100 I=K+2,IGH                                            REA13890
100         S=S+DABS(A(I,K))                                            REA13900
         ORT(K+1)=0D0                                                   REA13910
         IF (S.EQ.0D0) GO TO 360                                        REA13920
         S=S+DABS(A(K+1,K))                                             REA13930
         CALL DSC16 (S, S16, R16)                                       REA13940
         S=0D0                                                          REA13950
C SCALED HOUSEHOLDER VECTOR                                             REA13960
            DO 120 I=K+1,IGH                                            REA13970
            T=R16*A(I,K)                                                REA13980
            A(I,K)=T                                                    REA13990
120         S=S+T*T                                                     REA14000
         T=-DSIGN(DSQRT(S),A(K+1,K))                                    REA14010
         S=1D0/T                                                        REA14020
            DO 130 I=K+2,IGH                                            REA14030
130         A(I,K)=S*A(I,K)                                             REA14040
         A(K+1,K)=S*A(K+1,K)-1D0                                        REA14050
C NORMALIZING FACTOR OF THE REFLECTOR                                   REA14060
         D=1D0/A(K+1,K)                                                 REA14070
C LEFT TRANSFORMATION                                                   REA14080
         IF(K.LT.KK) GO TO 200                                          REA14090
C    PATH FOR SMALL ARRAYS                                              REA14100
C"    ( IGNORE RECRDEPS                                                 REA14110
C"    ( PREFER VECTOR                                                   REA14120
            DO 180 J=K+1,N                                              REA14130
            S=0D0                                                       REA14140
               DO 140 I=K+1,IGH                                         REA14150
140            S=S+A(I,K)*A(I,J)                                        REA14160
            S=S*D                                                       REA14170
               DO 160 I=K+1,IGH                                         REA14180
160            A(I,J)=A(I,J)+S*A(I,K)                                   REA14190
180         CONTINUE                                                    REA14200
         GO TO 280                                                      REA14210
C    PATH FOR LARGE ARRAYS                                              REA14220
200         DO 260 J=K+1,N                                              REA14230
            S=0D0                                                       REA14240
               DO 220 I=K+1,IGH                                         REA14250
220            S=S+A(I,K)*A(I,J)                                        REA14260
            S=S*D                                                       REA14270
               DO 240 I=K+1,IGH                                         REA14280
240            A(I,J)=A(I,J)+S*A(I,K)                                   REA14290
260         CONTINUE                                                    REA14300
C RIGHT TRANSFORMATION                                                  REA14310
C"    ( IGNORE RECRDEPS                                                 REA14320
280         DO 340 I=1,IGH                                              REA14330
            S=0D0                                                       REA14340
               DO 300 J=K+1,IGH                                         REA14350
300            S=S+A(J,K)*A(I,J)                                        REA14360
            S=S*D                                                       REA14370
               DO 320 J=K+1,IGH                                         REA14380
320            A(I,J)=A(I,J)+S*A(J,K)                                   REA14390
340         CONTINUE                                                    REA14400
C SUBDIAGONAL ELEMENT                                                   REA14410
         ORT(K+1)=A(K+1,K)                                              REA14420
         A(K+1,K)=S16*T                                                 REA14430
360      CONTINUE                                                       REA14440
      RETURN                                                            REA14450
      END                                                               REA14460
