@PROCESS DIRECTIVE('"    (')                                            REA02380
C-----------------------------------------------------------------------REA02390
C     EIGENVALUES OF A REAL UPPER-HESSENBERG MATRIX BY THE QR METHOD.   REA02400
C                                                                       REA02410
      SUBROUTINE  HQR ( LD, N, LOW, IGH, H, WR, WI, IER )               REA02420
C                                                                       REA02430
      INTEGER     LD, N, LOW, IGH, IER                                  REA02440
      REAL*8      H(LD,N), WR(N), WI(N)                                 REA02450
C                                                                       REA02460
C LD     E  FIRST DIMENSION ASSIGNED TO THE TWO-DIMENSIONAL ARRAY       REA02470
C           PARAMETERS IN THE CALLING PROGRAM.                          REA02480
C                                                                       REA02490
C N      E  ORDER OF THE MATRIX.                                        REA02500
C                                                                       REA02510
C LOW    E  SUBSCRIPT OBTAINED FROM SUBROUTINE BALANC.  IF BALANC WAS   REA02520
C           NOT USED, SET LOW = 1.                                      REA02530
C                                                                       REA02540
C IGH    E  SUBSCRIPT OBTAINED FROM SUBROUTINE BALANC.  IF BALANC WAS   REA02550
C           NOT USED, SET IGH = N.                                      REA02560
C                                                                       REA02570
C H      E  UPPER-HESSENBERG MATRIX RETURNED BY ELMHES OR ORTHES.       REA02580
C        R  THE CONTENTS OF THE ARRAY ARE LOST.  IF THIS INFORMATION    REA02590
C           WERE NEEDED IN LATER COMPUTATIONS (E.G., EIGENVECTORS), IT  REA02600
C           MUST BE SAVED BEFORE A CALL TO HQR.                         REA02610
C                                                                       REA02620
C WR     R  REAL      PARTS OF THE EIGENVALUES.                         REA02630
C                                                                       REA02640
C WI     R  IMAGINARY PARTS OF THE EIGENVALUES.                         REA02650
C                                                                       REA02660
C IER    R  ZERO   : NORMAL RETURN.                                     REA02670
C           NONZERO: NOT ALL THE EIGENVALUES COULD BE OBTAINED WITHIN   REA02680
C                    30*(IGH-LOW+1) ITERATIONS. THE EIGENVALUES OF      REA02690
C                    INDICES IER+1,...,N SHOULD BE CORRECT.             REA02700
C                                                                       REA02710
C     THIS SUBROUTINE IS BASED ON THE ALGOL PROCEDURE HQR,              REA02720
C     NUM. MATH. 14, 219-231(1970) BY MARTIN, PETERS, AND WILKINSON.    REA02730
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 359-371(1971).   REA02740
C                                                                       REA02750
C     VERSION FOR THE IBM 3090VF DATED NOVEMBER 1987.                   REA02760
C     ------------------------------------------------------------------REA02770
      INTEGER     I, J, K, L, M, M1, M2, MAXIT, IT                      REA02780
      REAL*8      P, Q, R, S, XSHIFT, T, X, Y, Z, ANORM, T1, T2         REA02790
      LOGICAL     NOTLAS, JNEK                                          REA02800
C                                                                       REA02810
C                                                                       REA02820
      CALL XUFLOW(0)                                                    REA02830
      IER=0                                                             REA02840
C ROOTS ISOLATED BY BALANC                                              REA02850
         DO 10   I=1,LOW-1                                              REA02860
         WR(I)=H(I,I)                                                   REA02870
10       WI(I)=0D0                                                      REA02880
         DO 20   I=IGH+1,N                                              REA02890
         WR(I)=H(I,I)                                                   REA02900
20       WI(I)=0D0                                                      REA02910
C MATRIX NORM                                                           REA02920
      ANORM=DABS(H(LOW,LOW))                                            REA02930
         DO 40  J=LOW+1,IGH                                             REA02940
         ANORM=ANORM+DABS(H(J,J-1))                                     REA02950
            DO 30  I=LOW,J                                              REA02960
30          ANORM=ANORM+DABS(H(I,J))                                    REA02970
40       CONTINUE                                                       REA02980
C                                                                       REA02990
      M=IGH                                                             REA03000
      XSHIFT=0D0                                                        REA03010
      MAXIT=30*(IGH-LOW+1)                                              REA03020
C NEXT EIGENVALUES                                                      REA03030
50    IF(M.LT.LOW) GO TO 1000                                           REA03040
      IT=0                                                              REA03050
      M1=M-1                                                            REA03060
      M2=M1-1                                                           REA03070
C SEARCH FOR A SMALL SUBDIAGONAL SINGLETON                              REA03080
60       DO 70   L=M,LOW+1,-1                                           REA03090
         S=DABS(H(L-1,L-1))+DABS(H(L,L))                                REA03100
         IF(S.EQ.0D0) S=ANORM                                           REA03110
         T2=S+DABS(H(L,L-1))                                            REA03120
         IF(T2.EQ.S) GO TO 80                                           REA03130
70       CONTINUE                                                       REA03140
      L=LOW                                                             REA03150
C SHIFT                                                                 REA03160
80    X=H(M,M)                                                          REA03170
      IF(L.EQ.M) GO TO 230                                              REA03180
      Y=H(M1,M1)                                                        REA03190
      T=H(M,M1)*H(M1,M)                                                 REA03200
      IF (L.EQ.M1) GO TO 240                                            REA03210
      IF (IT.EQ.MAXIT) GO TO 999                                        REA03220
      IF (IT.NE.10 .AND. IT.NE.20) GO TO 100                            REA03230
C EXCEPTIONAL EXPLICIT SHIFT                                            REA03240
      XSHIFT=XSHIFT+X                                                   REA03250
         DO 90  I=LOW,M                                                 REA03260
90       H(I,I)=H(I,I)-X                                                REA03270
      S=DABS(H(M,M1))+DABS(H(M1,M2))                                    REA03280
      X=0.75D0*S                                                        REA03290
      Y=X                                                               REA03300
      T=-0.4375D0*S*S                                                   REA03310
100   IT=IT+1                                                           REA03320
      MAXIT=MAXIT-1                                                     REA03330
C SEARCH FOR TWO ADJACENT SMALL SUBDIAGONAL ELEMENTS                    REA03340
         DO 110 K=M2,L,-1                                               REA03350
         Z=H(K,K)                                                       REA03360
         R=X-Z                                                          REA03370
         S=Y-Z                                                          REA03380
         P=(R*S-T)/H(K+1,K)+H(K,K+1)                                    REA03390
         Q=H(K+1,K+1)-Z-R-S                                             REA03400
         R=H(K+2,K+1)                                                   REA03410
         S=DABS(P)+DABS(Q)+DABS(R)                                      REA03420
         P=P/S                                                          REA03430
         Q=Q/S                                                          REA03440
         R=R/S                                                          REA03450
         H(K+2,K)=0D0                                                   REA03460
         IF(K.EQ.L) GO TO 120                                           REA03470
         T1=DABS(P)*(DABS(H(K-1,K-1))+DABS(Z)+DABS(H(K+1,K+1)))         REA03480
         T2=T1+DABS(H(K,K-1))*(DABS(Q)+DABS(R))                         REA03490
         IF(T2.EQ.T1) GO TO 120                                         REA03500
         H(K+2,K-1)=0D0                                                 REA03510
110      CONTINUE                                                       REA03520
C DOUBLE QR STEP.  FOR A UNIT-STRIDE ALGORITHM, THE TRIPLETS DEFINING   REA03530
C    THE HOUSEHOLDER TRANSFORMATIONS ARE SAVED IN THE FREE CELLS OF     REA03540
C    WR, WI, AND H(*,1).                                                REA03550
120   JNEK=.FALSE.                                                      REA03560
         DO 210 J=K,M1                                                  REA03570
         NOTLAS=J.NE.M1                                                 REA03580
C     SCALED HOUSEHOLDER VECTOR (FIRST NONTRIVIAL ELEMENT IS UNITY)     REA03590
         IF(JNEK) GO TO 130                                             REA03600
            JNEK=.TRUE.                                                 REA03610
            S=-DSIGN(DSQRT(P*P+Q*Q+R*R),P)                              REA03620
            IF(L.NE.K) H(J,J-1)=-H(J,J-1)                               REA03630
            GO TO 140                                                   REA03640
130      P=H(J,J-1)                                                     REA03650
         Q=H(J+1,J-1)                                                   REA03660
         H(J+1,1)=R                                                     REA03670
         R=0D0                                                          REA03680
         IF(NOTLAS) R=H(J+2,J-1)                                        REA03690
         T=DMAX1(DABS(P),DABS(Q),DABS(R))                               REA03700
         WR(J)=0D0                                                      REA03710
         WI(J)=0D0                                                      REA03720
         IF(T.EQ.0D0) GO TO 180                                         REA03730
         P=P/T                                                          REA03740
         Q=Q/T                                                          REA03750
         R=R/T                                                          REA03760
         S=-DSIGN(DSQRT(P*P+Q*Q+R*R),P)                                 REA03770
         H(J,J-1)=S*T                                                   REA03780
140      P=P-S                                                          REA03790
         Q=Q/P                                                          REA03800
         R=R/P                                                          REA03810
         P=P/S                                                          REA03820
         WR(J)=P                                                        REA03830
         WI(J)=Q                                                        REA03840
C     RIGHT TRANSFORMATION OF COLUMNS J, J+1, J+2                       REA03850
         IF(NOTLAS) GO TO 160                                           REA03860
C"    ( PREFER VECTOR                                                   REA03870
            DO 150 I=L,MIN0(J+3,M)                                      REA03880
            T=P*(H(I,J)+Q*H(I,J+1))                                     REA03890
            H(I,J)  =H(I,J)+T                                           REA03900
150         H(I,J+1)=H(I,J+1)+T*Q                                       REA03910
         GO TO 180                                                      REA03920
C"    ( PREFER VECTOR                                                   REA03930
160         DO 170 I=L,MIN0(J+3,M)                                      REA03940
            T=P*(H(I,J)+Q*H(I,J+1)+R*H(I,J+2))                          REA03950
            H(I,J)  =H(I,J)+T                                           REA03960
            H(I,J+1)=H(I,J+1)+T*Q                                       REA03970
170         H(I,J+2)=H(I,J+2)+T*R                                       REA03980
C     LEFT TRANSFORMATION OF COLUMN J                                   REA03990
180         DO 190 I=K,J-1                                              REA04000
            T=WR(I)*(H(I,J)+WI(I)*H(I+1,J)+H(I+2,1)*H(I+2,J))           REA04010
            H(I,J)  =H(I,J)+T                                           REA04020
            H(I+1,J)=H(I+1,J)+T*WI(I)                                   REA04030
            H(I+2,J)=H(I+2,J)+T*H(I+2,1)                                REA04040
190         CONTINUE                                                    REA04050
         IF(NOTLAS) GO TO 200                                           REA04060
            T=P*(H(J,J)+Q*H(J+1,J))                                     REA04070
            H(J,J)  =H(J,J)+T                                           REA04080
            H(J+1,J)=H(J+1,J)+T*Q                                       REA04090
            GO TO 210                                                   REA04100
200      T=P*(H(J,J)+Q*H(J+1,J)+R*H(J+2,J))                             REA04110
         H(J,J)  =H(J,J)+T                                              REA04120
         H(J+1,J)=H(J+1,J)+T*Q                                          REA04130
         H(J+2,J)=H(J+2,J)+T*R                                          REA04140
210      CONTINUE                                                       REA04150
C COMPLETION OF THE LEFT TRANSFORMATION                                 REA04160
         DO 220 I=K,M2                                                  REA04170
         T=WR(I)*(H(I,M)+WI(I)*H(I+1,M)+H(I+2,1)*H(I+2,M))              REA04180
         H(I,M)  =H(I,M)+T                                              REA04190
         H(I+1,M)=H(I+1,M)+T*WI(I)                                      REA04200
         H(I+2,M)=H(I+2,M)+T*H(I+2,1)                                   REA04210
220      CONTINUE                                                       REA04220
         T=P*(H(M1,M)+Q*H(M,M))                                         REA04230
         H(M1,M)=H(M1,M)+T                                              REA04240
         H(M,M)=H(M,M)+T*Q                                              REA04250
         GO TO 60                                                       REA04260
C ONE ROOT FOUND                                                        REA04270
230   WR(M)=X+XSHIFT                                                    REA04280
      WI(M)=0D0                                                         REA04290
      M=M1                                                              REA04300
      GO TO 50                                                          REA04310
C TWO ROOTS FOUND                                                       REA04320
240   P=0.5D0*(Y-X)                                                     REA04330
      Q=P*P+T                                                           REA04340
      Z=DSQRT(DABS(Q))                                                  REA04350
      X=X+XSHIFT                                                        REA04360
      IF(Q.LT.0D0) GO TO 250                                            REA04370
C REAL PAIR                                                             REA04380
      Z=P+DSIGN(Z,P)                                                    REA04390
      WR(M1)=X+Z                                                        REA04400
      WR(M)=WR(M1)                                                      REA04410
      IF(Z.NE.0D0) WR(M)=X-T/Z                                          REA04420
      WI(M1)=0D0                                                        REA04430
      WI(M)=0D0                                                         REA04440
      M=M2                                                              REA04450
      GO TO 50                                                          REA04460
C COMPLEX CONJUGATE PAIR                                                REA04470
250   WR(M1)=X+P                                                        REA04480
      WR(M)=WR(M1)                                                      REA04490
      WI(M1)=Z                                                          REA04500
      WI(M)=-Z                                                          REA04510
      M=M2                                                              REA04520
      GO TO 50                                                          REA04530
C ERROR: NOT ALL THE EIGENVALUES COULD BE OBTAINED IN 30*(IGH-LOW+1)    REA04540
C        ITERATIONS                                                     REA04550
999   IER=M                                                             REA04560
C                                                                       REA04570
1000  RETURN                                                            REA04580
      END                                                               REA04590
