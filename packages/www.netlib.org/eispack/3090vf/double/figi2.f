C-----------------------------------------------------------------------SYM03290
C SIMILARITY TRANSFORMATION OF A NONSYMMETRIC TRIDIAGONAL MATRIX WHOSE  SYM03300
C PAIRS OF OFF-DIAGONAL ELEMENTS IN SYMMETRIC POSITIONS HAVE THE SAME   SYM03310
C SIGNS (OR ARE ZERO) TO SYMMETRIC TRIDIAGONAL FORM.  THE RIGHT MATRIX  SYM03320
C OF THE SIMILARITY TRANSFORMATION IS CONSTRUCTED.                      SYM03330
C                                                                       SYM03340
      SUBROUTINE  FIGI2 ( LD, N, T, D, E, Z, IERR )                     SYM03350
C                                                                       SYM03360
      INTEGER     LD, N, IERR                                           SYM03370
      REAL*8      T(LD,3), D(N), E(N), Z(LD,N)                          SYM03380
C                                                                       SYM03390
C LD     E  FIRST DIMENSION ASSIGNED TO THE TWO-DIMENSIONAL ARRAY       SYM03400
C           PARAMETERS IN THE CALLING PROGRAM.                          SYM03410
C                                                                       SYM03420
C N      E  ORDER OF THE MATRIX.                                        SYM03430
C                                                                       SYM03440
C T      E  ARRAY OF THE GIVEN MATRIX.  THE SUBDIAGONAL MUST BE STORED  SYM03450
C           IN CELLS  2,...,N  OF THE FIRST COLUMN, THE DIAGONAL IN THE SYM03460
C           FIRST N CELLS OF THE SECOND COLUMN, AND THE SUPERDIAGONAL   SYM03470
C           IN THE FIRST N-1 CELLS OF THE THIRD COLUMN.                 SYM03480
C                                                                       SYM03490
C D      R  DIAGONAL ELEMENTS OF THE SYMMETRIC MATRIX.                  SYM03500
C                                                                       SYM03510
C E      R  CODIAGONAL ELEMENTS OF THE SYMMETRIC MATRIX IN CELLS        SYM03520
C           2,...,N.                                                    SYM03530
C                                                                       SYM03540
C Z      R  RIGHT MATRIX OF THE SIMILARITY TRANSFORMATION               SYM03550
C                                                                       SYM03560
C IERR   R  ZERO       FOR NORMAL RETURN,                               SYM03570
C           N+I        IF T(I,1)*T(I-1,3) IS NEGATIVE,                  SYM03580
C           2*N+I      IF T(I,1)*T(I-1,3) IS ZERO WITH ONE FACTOR       SYM03590
C                      NON-ZERO.                                        SYM03600
C                                                                       SYM03610
C     VERSION FOR THE IBM 3090VF DATED NOVEMBER 1987.                   SYM03620
C     ------------------------------------------------------------------SYM03630
      INTEGER     I, J                                                  SYM03640
C                                                                       SYM03650
      CALL XUFLOW(0)                                                    SYM03660
      IERR=0                                                            SYM03670
C SQUARES OF THE CODIANORMAL ELEMENTS                                   SYM03680
         DO 100 J=2,N                                                   SYM03690
         Z(J,1)=0D0                                                     SYM03700
100      E(J)=T(J,1)*T(J-1,3)                                           SYM03710
C CHECK FOR VALIDITY                                                    SYM03720
         DO 120 J=2,N                                                   SYM03730
         IF (E(J).GT.0D0) GO TO 120                                     SYM03740
            IF (E(J).LT.0D0) GO TO 999                                  SYM03750
               IF (T(J,1).NE.0D0 .OR. T(J-1,3).NE.0D0) GO TO 997        SYM03760
120      CONTINUE                                                       SYM03770
C NO ERROR: DIAGONAL AND CODIAGONAL ELEMENTS, AND TRANSFORMATION MATRIX SYM03780
      Z(1,1)=1D0                                                        SYM03790
      D(1)=T(1,2)                                                       SYM03800
         DO 160 J=2,N                                                   SYM03810
         D(J)=T(J,2)                                                    SYM03820
         E(J)=DSQRT(E(J))                                               SYM03830
            DO 140 I=1,N                                                SYM03840
140         Z(I,J)=0D0                                                  SYM03850
         Z(J,J)=1D0                                                     SYM03860
         IF(T(J-1,3).NE.0D0) Z(J,J)=Z(J-1,J-1)*E(J)/T(J-1,3)            SYM03870
160      CONTINUE                                                       SYM03880
      GO TO 1000                                                        SYM03890
C ERROR: ONLY ONE ELEMENT IN A SYMMETRIC PAIR IS ZERO                   SYM03900
997   IERR=N+N+I                                                        SYM03910
      GO TO 1000                                                        SYM03920
C ERROR: SOME ELEMENTS IN SYMMETRIC POSITIONS DO NOT HAVE THE SAME SIGN SYM03930
999   IERR=N+I                                                          SYM03940
C                                                                       SYM03950
1000  RETURN                                                            SYM03960
      END                                                               SYM03970
