@PROCESS DIRECTIVE('"    (')                                            REA00020
C-----------------------------------------------------------------------REA00030
C     EIGENVECTORS OF A REAL GENERAL MATRIX BY BACK TRANSFORMATION OF   REA00040
C     THOSE OF THE CORRESPONDING UPPER-HESSENBERG MATRIX PRODUCED BY    REA00050
C     ELMHES.                                                           REA00060
C                                                                       REA00070
      SUBROUTINE  ELMBAK ( LD, LOW, IGH, A, INT, M, Z )                 REA00080
C                                                                       REA00090
      INTEGER     LD, LOW, IGH, M, INT(IGH)                             REA00100
      REAL*8      A(LD,IGH), Z(LD,M)                                    REA00110
C                                                                       REA00120
C LD     E  FIRST DIMENSION ASSIGNED TO THE TWO-DIMENSIONAL ARRAY       REA00130
C           PARAMETERS IN THE CALLING PROGRAM.                          REA00140
C                                                                       REA00150
C LOW    E  SUBSCRIPT RETURNED BY SUBROUTINE BALANC.  IF BALANC WAS NOT REA00160
C           USED, SET  LOW = 1.                                         REA00170
C                                                                       REA00180
C IGH    E  SUBSCRIPT RETURNED BY SUBROUTINE BALANC.  IF BALANC WAS NOT REA00190
C           USED, SET  IGH = ORDER OF THE MATRIX.                       REA00200
C                                                                       REA00210
C A      E  ELIMINATION MULTIPLIERS RETURNED BY ELMHES IN THE SUB-      REA00220
C           HESSENBERG PART OF THE ARRAY.                               REA00230
C                                                                       REA00240
C INT    E  INFORMATION RETURNED BY ELMHES ABOUT ROW AND COLUMN         REA00250
C           INTERCHANGES IN CELLS LOW,...,IGH.                          REA00260
C                                                                       REA00270
C M      E  NUMBER OF COLUMNS OF Z TO BE TRANSFORMED.                   REA00280
C                                                                       REA00290
C Z      E  REAL AND IMAGINARY PARTS OF THE EIGENVECTORS TO BE          REA00300
C           TRANSFORMED, IN THE FIRST M COLUMNS.                        REA00310
C        R  TRANSFORMED REAL AND IMAGINARY PARTS OF THE EIGENVECTORS.   REA00320
C                                                                       REA00330
C     THIS SUBROUTINE IS BASED ON THE ALGOL PROCEDURE ELMBAK,           REA00340
C     NUM. MATH. 12, 349-368 (1968) BY MARTIN AND WILKINSON.            REA00350
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 339-358 (1971).  REA00360
C                                                                       REA00370
C     VERSION FOR THE IBM 3090VF DATED NOVEMBER 1987.                   REA00380
C     ------------------------------------------------------------------REA00390
      INTEGER     I, J, K                                               REA00400
      REAL*8      T                                                     REA00410
C                                                                       REA00420
      CALL XUFLOW(0)                                                    REA00430
      IF(M.LE.0) GO TO 1000                                             REA00440
         DO 160 K=IGH-1,LOW+1,-1                                        REA00450
         IF(A(K,K-1).EQ.0D0) GO TO 160                                  REA00460
C"    ( IGNORE RECRDEPS                                                 REA00470
            DO 120 I=K+1,IGH                                            REA00480
            T=A(I,K-1)                                                  REA00490
               DO 100 J=1,M                                             REA00500
100            Z(I,J)=Z(I,J)+Z(K,J)*T                                   REA00510
120         CONTINUE                                                    REA00520
         I=INT(K)                                                       REA00530
         IF(I.EQ.K) GO TO 160                                           REA00540
            DO 140 J=1,M                                                REA00550
            T=Z(K,J)                                                    REA00560
            Z(K,J)=Z(I,J)                                               REA00570
140         Z(I,J)=T                                                    REA00580
160      CONTINUE                                                       REA00590
C                                                                       REA00600
1000  RETURN                                                            REA00610
      END                                                               REA00620
