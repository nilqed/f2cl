@PROCESS DIRECTIVE('"    (')                                            COM02650
C-----------------------------------------------------------------------COM02660
C EIGENVECTORS OF A COMPLEX MATRIX BY TRANSFORMATION OF THOSE OF THE    COM02670
C CORRESPONDING UPPER-HESSENBERG MATRIX PRODUCED BY COMHES.             COM02680
C                                                                       COM02690
      SUBROUTINE COMBAK ( LD, LOW, IGH, AR, AI, INT, M, ZR, ZI )        COM02700
C                                                                       COM02710
      INTEGER    LD, LOW, IGH, M, INT(IGH)                              COM02720
      REAL*8     AR(LD,IGH), AI(LD,IGH), ZR(LD,M), ZI(LD,M)             COM02730
C                                                                       COM02740
C LD     E  FIRST DIMENSION ASSIGNED TO THE TWO-DIMENSIONAL ARRAY       COM02750
C           PARAMETERS IN THE CALLING PROGRAM.                          COM02760
C                                                                       COM02770
C LOW    E  PARAMETER DETERMINED BY THE BALANCING SUBROUTINE CBAL.      COM02780
C           IF  CBAL WAS NOT USED, SET LOW = 1.                         COM02790
C                                                                       COM02800
C IGH    E  PARAMETER DETERMINED BY THE BALANCING SUBROUTINE CBAL.      COM02810
C           IF  CBAL WAS NOT USED, SET IGH = ORDER OF THE MATRIX.       COM02820
C                                                                       COM02830
C AR,AI  E  ELIMINATION MULTIPLIERS PRODUCED BY COMHES IN THE SUB-      COM02840
C           HESSENBERG PARTS OF THE ARRAYS.                             COM02850
C                                                                       COM02860
C INT    E  RECORD OF ROW AND COLUMN EXCHANGES PRODUCED BY COMHES IN    COM02870
C           CELLS LOW,...,IGH.                                          COM02880
C                                                                       COM02890
C M      E  NUMBER OF EIGENVECTORS TO BE TRANSFORMED.                   COM02900
C                                                                       COM02910
C ZR,ZI  E  REAL AND IMAGINARY PARTS OF THE MATRIX OF EIGENVECTORS TO   COM02920
C           BE TRANSFORMED (FIRST M COLUMNS).                           COM02930
C        R  REAL AND IMAGINARY PARTS OF THE MATRIX OF TRANSFORMED       COM02940
C           EIGENVECTORS.                                               COM02950
C                                                                       COM02960
C     THIS SUBROUTINE IS BASED ON THE ALGOL PROCEDURE COMBAK,           COM02970
C     NUM. MATH. 12, 349-368(1968) BY MARTIN AND WILKINSON.             COM02980
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 339-358(1971).   COM02990
C                                                                       COM03000
C     VERSION FOR THE IBM S/370-3090VF DATED NOVEMBER 1987.             COM03010
C     ------------------------------------------------------------------COM03020
      INTEGER     I, J, K                                               COM03030
      REAL*8      T                                                     COM03040
C                                                                       COM03050
      CALL XUFLOW(0)                                                    COM03060
      IF(M.LE.0) GO TO 1000                                             COM03070
         DO 260 K=IGH-1,LOW+1,-1                                        COM03080
         IF(AR(K,K-1).EQ.0D0 .AND. AI(K,K-1).EQ.0D0) GO TO 260          COM03090
C"    ( IGNORE RECRDEPS                                                 COM03100
            DO 220 I=K+1,IGH                                            COM03110
               DO 200 J=1,M                                             COM03120
               ZR(I,J)=ZR(I,J)+ZR(K,J)*AR(I,K-1)-ZI(K,J)*AI(I,K-1)      COM03130
200            ZI(I,J)=ZI(I,J)+ZR(K,J)*AI(I,K-1)+ZI(K,J)*AR(I,K-1)      COM03140
220         CONTINUE                                                    COM03150
         I=INT(K)                                                       COM03160
         IF(I.EQ.K) GO TO 260                                           COM03170
            DO 240 J=1,M                                                COM03180
            T=ZR(K,J)                                                   COM03190
            ZR(K,J)=ZR(I,J)                                             COM03200
            ZR(I,J)=T                                                   COM03210
            T=ZI(K,J)                                                   COM03220
            ZI(K,J)=ZI(I,J)                                             COM03230
240         ZI(I,J)=T                                                   COM03240
260      CONTINUE                                                       COM03250
1000  RETURN                                                            COM03260
      END                                                               COM03270
