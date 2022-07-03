@PROCESS DIRECTIVE('"    (')                                            REA12680
C-----------------------------------------------------------------------REA12690
C     EIGENVECTORS OF A REAL GENERAL MATRIX BY BACK TRANSFORMATION OF   REA12700
C     THOSE OF THE CORRESPONDING UPPER-HESSENBERG MATRIX PRODUCED BY    REA12710
C     ORTHES.                                                           REA12720
C                                                                       REA12730
      SUBROUTINE  ORTBAK ( LD, LOW, IGH, A, ORT, M, Z )                 REA12740
C                                                                       REA12750
      INTEGER        LD, LOW, IGH, M                                    REA12760
      REAL*8         A(LD,IGH), ORT(IGH), Z(LD,M)                       REA12770
C                                                                       REA12780
C LD     E  FIRST DIMENSION ASSIGNED TO THE TWO-DIMENSIONAL ARRAY       REA12790
C           PARAMETERS IN THE CALLING PROGRAM.                          REA12800
C                                                                       REA12810
C LOW    E  SUBSCRIPT DETERMINED BY SUBROUTINE BALANC.  IF BALANC WAS   REA12820
C           NOT USED, SET LOW = 1.                                      REA12830
C                                                                       REA12840
C IGH    E  SUBSCRIPT DETERMINED BY SUBROUTINE BALANC.  IF BALANC WAS   REA12850
C           NOT USED, SET IGH = ORDER OF THE MATRIX.                    REA12860
C                                                                       REA12870
C A      E  INFORMATION ABOUT THE ORTHOGONAL TRANSFORMATIONS USED IN THEREA12880
C           REDUCTION BY ORTHES IN THE SUB-HESSENBERG PART OF THE ARRAY.REA12890
C                                                                       REA12900
C ORT    E  ADDITIONAL INFORMATION ABOUT THE TRANSFORMATIONS USED IN THEREA12910
C           REDUCTION BY  ORTHES.  ONLY ORT(LOW),...,ORT(IGH) ARE USED. REA12920
C           THE INFORMATION CONTAINED IN ORT IS PRESERVED.              REA12930
C                                                                       REA12940
C M      E  NUMBER OF COLUMNS OF Z TO BE TRANSFORMED.                   REA12950
C                                                                       REA12960
C Z      E  REAL AND IMAGINARY PARTS OF THE EIGENVECTORS TO BE          REA12970
C           TRANSFORMED, IN THE FIRST M COLUMNS.                        REA12980
C        R  TRANSFORMED REAL AND IMAGINARY PARTS OF THE EIGENVECTORS.   REA12990
C                                                                       REA13000
C     THIS SUBROUTINE IS BASED ON THE ALGOL PROCEDURE ORTBAK,           REA13010
C     NUM. MATH. 12, 349-368(1968) BY MARTIN AND WILKINSON.             REA13020
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 339-358(1971).   REA13030
C                                                                       REA13040
C     VERSION FOR THE IBM 3090VF DATED NOVEMBER 1987.                   REA13050
C     ------------------------------------------------------------------REA13060
      INTEGER     I, J, K                                               REA13070
      REAL*8      S, T, D                                               REA13080
C                                                                       REA13090
      CALL XUFLOW(0)                                                    REA13100
      IF (M.EQ.0) GO TO 1000                                            REA13110
      IF(M.GT.2*KACHEL(LD)) GO TO 180                                   REA13120
C PATH FOR SMALL ARRAYS                                                 REA13130
         DO 160 K=IGH-2,LOW,-1                                          REA13140
         T=ORT(K+1)                                                     REA13150
         IF (T.EQ.0D0) GO TO 160                                        REA13160
         D=1D0/T                                                        REA13170
C"    ( PREFER VECTOR                                                   REA13180
            DO 140 J=1,M                                                REA13190
            S=T*Z(K+1,J)                                                REA13200
               DO 100 I=K+2,IGH                                         REA13210
100            S=S+A(I,K)*Z(I,J)                                        REA13220
            Z(K+1,J)=Z(K+1,J)+S                                         REA13230
            S=D*S                                                       REA13240
               DO 120 I=K+2,IGH                                         REA13250
120            Z(I,J)=Z(I,J)+S*A(I,K)                                   REA13260
140         CONTINUE                                                    REA13270
160      CONTINUE                                                       REA13280
      GO TO 1000                                                        REA13290
C PATH FOR LARGE ARRAYS                                                 REA13300
180      DO 260 K=IGH-2,LOW,-1                                          REA13310
         T=ORT(K+1)                                                     REA13320
         IF(T.EQ.0D0) GO TO 260                                         REA13330
         D=1D0/T                                                        REA13340
            DO 240 J=1,M                                                REA13350
            S=T*Z(K+1,J)                                                REA13360
               DO 200 I=K+2,IGH                                         REA13370
200            S=S+A(I,K)*Z(I,J)                                        REA13380
            Z(K+1,J)=Z(K+1,J)+S                                         REA13390
            S=D*S                                                       REA13400
               DO 220 I=K+2,IGH                                         REA13410
220            Z(I,J)=Z(I,J)+S*A(I,K)                                   REA13420
240         CONTINUE                                                    REA13430
260      CONTINUE                                                       REA13440
1000  RETURN                                                            REA13450
      END                                                               REA13460
