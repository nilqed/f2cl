C-----------------------------------------------------------------------SYM00020
C EIGENVECTORS OF A NONSYMMETRIC TRIDIAGONAL MATRIX BY TRANSFORMATION OFSYM00030
C THOSE OF THE CORRESPONDING SYMMETRIC TRIDIAGONAL MATRIX PRODUCED BY   SYM00040
C SUBROUTINE FIGI.                                                      SYM00050
C                                                                       SYM00060
      SUBROUTINE  BAKVEC ( LD, N, T, E, M, Z, IERR )                    SYM00070
C                                                                       SYM00080
      INTEGER     M, N, LD, IERR                                        SYM00090
      REAL*8      T(LD,3), E(N), Z(LD,M)                                SYM00100
C                                                                       SYM00110
C LD     E  FIRST DIMENSION ASSIGNED TO THE TWO-DIMENSIONAL ARRAY       SYM00120
C           PARAMETERS IN THE CALLING PROGRAM.                          SYM00130
C                                                                       SYM00140
C N      E  ORDER OF THE MATRIX.                                        SYM00150
C                                                                       SYM00160
C T      E  NONSYMMETRIC TRIDIAGONAL MATRIX.  ITS SUBDIAGONAL IS        SYM00170
C           STORED IN CELLS 2,...,N OF THE FIRST COLUMN,                SYM00180
C           ITS DIAGONAL IN THE N CELLS OF THE SECOND COLUMN, AND       SYM00190
C           ITS SUPERDIAGONAL IN CELLS 1,...,N-1 OF THE THIRD           SYM00200
C           COLUMN.  T(1,1) AND T(N,3) ARE ARBITRARY.                   SYM00210
C                                                                       SYM00220
C E      E  SUBDIAGONAL ELEMENTS OF THE SYMMETRIC MATRIX IN CELLS       SYM00230
C           2,...,N.                                                    SYM00240
C        R  THIS INFORMATION IS NOT PRESERVED.                          SYM00250
C                                                                       SYM00260
C M      E  NUMBER OF EIGENVECTORS TO BE TRANSFORMED.                   SYM00270
C                                                                       SYM00280
C Z      E  EIGENVECTORS TO BE TRANSFORMED (LEADING M COLUMNS).         SYM00290
C        R  TRANSFORMED EIGENVECTORS.                                   SYM00300
C                                                                       SYM00310
C IERR   R  ZERO     FOR NORMAL RETURN,                                 SYM00320
C           2*N+I    IF E(I) IS ZERO WITH T(I,1) OR T(I-1,3) NON-ZERO.  SYM00330
C                    IN THIS CASE, THE SYMMETRIC MATRIX IS NOT SIMILAR  SYM00340
C                    TO THE ORIGINAL MATRIX, AND THE EIGENVECTORS       SYM00350
C                    CANNOT BE OBTAINED FROM THIS SUBROUTINE.           SYM00360
C                                                                       SYM00370
C     VERSION FOR THE IBM 3090VF DATED NOVEMBER 1987.                   SYM00380
C     ------------------------------------------------------------------SYM00390
      INTEGER     I, J                                                  SYM00400
C                                                                       SYM00410
      CALL XUFLOW(0)                                                    SYM00420
      IERR=0                                                            SYM00430
      IF(M.EQ.0) GO TO 1000                                             SYM00440
      E(1)=1D0                                                          SYM00450
      IF(N.EQ.1) GO TO 1000                                             SYM00460
         DO 120 I=2,N                                                   SYM00470
         IF(E(I).NE.0D0) GO TO 100                                      SYM00480
            IF(T(I,1).NE.0D0 .OR. T(I-1,3).NE.0D0) GO TO 999            SYM00490
               E(I)=1D0                                                 SYM00500
               GO TO 120                                                SYM00510
100      E(I)=E(I-1)*E(I)/T(I-1,3)                                      SYM00520
120      CONTINUE                                                       SYM00530
         DO 160 J=1,M                                                   SYM00540
            DO 140 I=2,N                                                SYM00550
140         Z(I,J)=Z(I,J)*E(I)                                          SYM00560
160      CONTINUE                                                       SYM00570
      GO TO 1000                                                        SYM00580
C                                                                       SYM00590
999   IERR=2*N+I                                                        SYM00600
C                                                                       SYM00610
1000  RETURN                                                            SYM00620
      END                                                               SYM00630
