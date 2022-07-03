@PROCESS DIRECTIVE('"    (')                                            SYM20520
C-----------------------------------------------------------------------SYM20530
C EIGENVECTORS OF A REAL SYMMETRIC MATRIX BY TRANSFORMATION OF THOSE    SYM20540
C OF THE CORRESPONDING SYMMETRIC TRIDIAGONAL MATRIX PRODUCED BY TRED1.  SYM20550
C                                                                       SYM20560
      SUBROUTINE  TRBAK1 ( LD, N, A, DUMMY, M, Z )                      SYM20570
C                                                                       SYM20580
      INTEGER     LD, N, M                                              SYM20590
      REAL*8      A(LD,N), DUMMY, Z(LD,M)                               SYM20600
C                                                                       SYM20610
C LD     E  FIRST DIMENSION ASSIGNED TO THE TWO-DIMENSIONAL ARRAY       SYM20620
C           PARAMETERS IN THE CALLING PROGRAM.                          SYM20630
C                                                                       SYM20640
C N      E  ORDER OF THE MATRIX.                                        SYM20650
C                                                                       SYM20660
C A      E  INFORMATION ABOUT THE ORTHOGONAL TRANSFORMATIONS USED IN THESYM20670
C           REDUCTION BY TRED1, IN THE SUBDIAGONAL CELLS.               SYM20680
C                                                                       SYM20690
C DUMMY  E  UNUSED PARAMETER (TO PRESERVE THE EISPACK CALLING SEQUENCE).SYM20700
C                                                                       SYM20710
C M      E  NUMBER OF EIGENVECTORS TO BE TRANSFORMED.                   SYM20720
C                                                                       SYM20730
C Z      E  EIGENVECTORS OF THE TRIDIAGONAL MATRIX IN THE FIRST M       SYM20740
C           COLUMNS.                                                    SYM20750
C        R  TRANSFORMED EIGENVECTORS.                                   SYM20760
C                                                                       SYM20770
C     THIS SUBROUTINE IS BASED ON THE ALGOL PROCEDURE TRBAK1,           SYM20780
C     NUM. MATH. 11, 181-195(1968) BY MARTIN, REINSCH, AND WILKINSON.   SYM20790
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 212-226(1971).   SYM20800
C                                                                       SYM20810
C SUBRPROGRAMS CALLED:  KACHEL                                          SYM20820
C                                                                       SYM20830
C     VERSION FOR THE IBM 3090VF DATED NOVEMBER 1987.                   SYM20840
C     ------------------------------------------------------------------SYM20850
      INTEGER     I, J, K                                               SYM20860
      INTEGER     KACHEL                                                SYM20870
      REAL*8      S, T                                                  SYM20880
C                                                                       SYM20890
      CALL XUFLOW(0)                                                    SYM20900
      IF(M.LT.1) GO TO 1000                                             SYM20910
      IF(M.GT.2*KACHEL(LD)) GO TO 180                                   SYM20920
C PATH FOR SMALL ARRAYS                                                 SYM20930
         DO 160 K=N-2,1,-1                                              SYM20940
         T=A(K+1,K)                                                     SYM20950
         IF (T.EQ.0D0) GO TO 160                                        SYM20960
         T=1D0/T                                                        SYM20970
C"    ( PREFER VECTOR                                                   SYM20980
            DO 140 J=1,M                                                SYM20990
            S=0D0                                                       SYM21000
               DO 100 I=K+1,N                                           SYM21010
100            S=S+A(I,K)*Z(I,J)                                        SYM21020
            S=T*S                                                       SYM21030
               DO 120 I=K+1,N                                           SYM21040
120            Z(I,J)=Z(I,J)+S*A(I,K)                                   SYM21050
140         CONTINUE                                                    SYM21060
160      CONTINUE                                                       SYM21070
      GO TO 1000                                                        SYM21080
C PATH FOR LARGE ARRAYS                                                 SYM21090
180      DO 260 K=N-2,1,-1                                              SYM21100
         T=A(K+1,K)                                                     SYM21110
         IF (T.EQ.0D0) GO TO 260                                        SYM21120
         T=1D0/T                                                        SYM21130
            DO 240 J=1,M                                                SYM21140
            S=0D0                                                       SYM21150
               DO 200 I=K+1,N                                           SYM21160
200            S=S+A(I,K)*Z(I,J)                                        SYM21170
            S=T*S                                                       SYM21180
               DO 220 I=K+1,N                                           SYM21190
220            Z(I,J)=Z(I,J)+S*A(I,K)                                   SYM21200
240         CONTINUE                                                    SYM21210
260      CONTINUE                                                       SYM21220
1000  RETURN                                                            SYM21230
      END                                                               SYM21240
