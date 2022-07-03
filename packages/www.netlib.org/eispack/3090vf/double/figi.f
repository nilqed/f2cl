C-----------------------------------------------------------------------SYM02670
C TRANSFORMATION OF A NONSYMMETRIC TRIDIAGONAL MATRIX WHOSE OFF-DIAGONALSYM02680
C ELEMENTS IN SYMMETRIC POSITIONS HAVE THE SAME SIGNS TO SYMMETRIC      SYM02690
C TRIDIAGONAL FORM WITH PRESERVATION OF THE EIGENVALUES.                SYM02700
C (IF ZERO OFF-DIAGONAL ELEMENTS OCCUR ONLY AS SYMMETRIC PAIRS, THE     SYM02710
C TRANSFORMATION IS A SIMILARITY TRANSFORMATION)                        SYM02720
C                                                                       SYM02730
      SUBROUTINE  FIGI ( LD, N, T, D, E, E2, IERR )                     SYM02740
C                                                                       SYM02750
      INTEGER     LD, N, IERR                                           SYM02760
      REAL*8      T(LD,3), D(N), E(N), E2(N)                            SYM02770
C                                                                       SYM02780
C LD     E  FIRST DIMENSION ASSIGNED TO THE TWO-DIMENSIONAL ARRAY       SYM02790
C           PARAMETERS IN THE CALLING PROGRAM.                          SYM02800
C                                                                       SYM02810
C N      E  ORDER OF THE MATRIX.                                        SYM02820
C                                                                       SYM02830
C T      E  ARRAY OF THE GIVEN MATRIX.  THE SUBDIAGONAL MUST BE STORED  SYM02840
C           IN CELLS  2,...,N  OF THE FIRST COLUMN, THE DIAGONAL IN THE SYM02850
C           FIRST N CELLS OF THE SECOND COLUMN, AND THE SUPERDIAGONAL   SYM02860
C           IN THE FIRST N-1 CELLS OF THE THIRD COLUMN.                 SYM02870
C                                                                       SYM02880
C D      R  DIAGONAL ELEMENTS OF THE SYMMETRIC MATRIX.                  SYM02890
C                                                                       SYM02900
C E      R  SUBDIAGONAL ELEMENTS OF THE SYMMETRIC MATRIX IN CELLS       SYM02910
C           2,...,N.                                                    SYM02920
C                                                                       SYM02930
C E2     R  SQUARES OF THE CORRESPONDING ELEMENTS OF E.                 SYM02940
C           ARRAYS E2 AND E MAY COINCIDE IF THE SQUARES ARE NOT NEEDED. SYM02950
C                                                                       SYM02960
C IERR   R  ZERO       FOR NORMAL RETURN,                               SYM02970
C           N+I        IF T(I,1)*T(I-1,3) IS NEGATIVE,                  SYM02980
C           -(3*N+I)   IF T(I,1)*T(I-1,3) IS ZERO WITH ONE FACTOR       SYM02990
C                      NON-ZERO.  IN THIS CASE, THE EIGENVECTORS OF     SYM03000
C                      THE SYMMETRIC MATRIX ARE NOT SIMPLY RELATED      SYM03010
C                      TO THOSE OF  T  AND SHOULD NOT BE SOUGHT.        SYM03020
C                                                                       SYM03030
C     VERSION FOR THE IBM 3090VF DATED NOVEMBER 1987.                   SYM03040
C     ------------------------------------------------------------------SYM03050
      INTEGER I                                                         SYM03060
C                                                                       SYM03070
      CALL XUFLOW(0)                                                    SYM03080
      IERR=0                                                            SYM03090
C COMPUTATION FOR THE NORMAL CASE                                       SYM03100
      D(1)=T(1,2)                                                       SYM03110
         DO 100 I=2,N                                                   SYM03120
         D(I)=T(I,2)                                                    SYM03130
         E2(I)=T(I,1)*T(I-1,3)                                          SYM03140
100      E(I)=DSQRT(DABS(E2(I)))                                        SYM03150
C CHECK FOR VALIDITY                                                    SYM03160
         DO 120 I=2,N                                                   SYM03170
         IF (E2(I).GT.0D0) GO TO 120                                    SYM03180
            IF (E2(I).LT.0D0) GO TO 999                                 SYM03190
               IF (T(I,1).EQ.0D0 .AND. T(I-1,3).EQ.0D0) GO TO 120       SYM03200
               IERR=-3*N-I                                              SYM03210
120      CONTINUE                                                       SYM03220
      GO TO 1000                                                        SYM03230
C ERROR: SOME ELEMENTS IN SYMMETRIC POSITIONS DO NOT HAVE THE SAME SIGN SYM03240
999   IERR=N+I                                                          SYM03250
C                                                                       SYM03260
1000  RETURN                                                            SYM03270
      END                                                               SYM03280
