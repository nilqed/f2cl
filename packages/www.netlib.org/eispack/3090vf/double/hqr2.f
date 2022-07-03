@PROCESS DIRECTIVE('"    (')                                            REA04600
C-----------------------------------------------------------------------REA04610
C EIGENSYSTEM OF A REAL UPPER-HESSENBERG MATRIX BY THE QR METHOD.       REA04620
C                                                                       REA04630
      SUBROUTINE  HQR2  ( LD, N, LOW, IGH, H, WR, WI, VEC, IER )        REA04640
C                                                                       REA04650
      INTEGER     LD, N, LOW, IGH, IER                                  REA04660
      REAL*8      H(LD,N), WR(N), WI(N), VEC(LD,N)                      REA04670
C                                                                       REA04680
C LD     E  FIRST DIMENSION ASSIGNED TO THE TWO-DIMENSIONAL ARRAY       REA04690
C           PARAMETERS IN THE CALLING PROGRAM.                          REA04700
C                                                                       REA04710
C N      E  ORDER OF THE MATRIX.                                        REA04720
C                                                                       REA04730
C LOW    E  SUBSCRIPT OBTAINED FROM SUBROUTINE BALANC.  IF BALANC WAS   REA04740
C           NOT USED, SET LOW = 1.                                      REA04750
C                                                                       REA04760
C IGH    E  SUBSCRIPT OBTAINED FROM SUBROUTINE BALANC.  IF BALANC WAS   REA04770
C           NOT USED, SET IGH = N.                                      REA04780
C                                                                       REA04790
C H      E  UPPER-HESSENBERG MATRIX RETURNED BY ELMHES OR ORTHES.       REA04800
C        R  THE CONTENTS OF THE ARRAY ARE LOST.  IF THIS INFORMATION    REA04810
C           WERE NEEDED IN LATER COMPUTATIONS (E.G., EIGENVECTORS), IT  REA04820
C           MUST BE SAVED BEFORE A CALL TO HQR.                         REA04830
C                                                                       REA04840
C WR     R  REAL      PARTS OF THE EIGENVALUES.                         REA04850
C                                                                       REA04860
C WI     R  IMAGINARY PARTS OF THE EIGENVALUES.                         REA04870
C                                                                       REA04880
C VEC    E  IDENTITY MATRIX IF THE MATRIX OF THE ORIGINAL PROBLEM IS    REA04890
C           UPPER-HESSENBERG.                                           REA04900
C           RIGHT MATRIX OF THE SIMILARITY TRANSFORMATION TO UPPER-     REA04910
C           HESSENBERG FORM OTHERWISE.                                  REA04920
C        R  MATRIX OF UNNORMALIZED EIGENVECTORS.                        REA04930
C                                                                       REA04940
C IER    R  ZERO   : NORMAL RETURN.                                     REA04950
C           NONZERO: NOT ALL THE EIGENVALUES COULD BE OBTAINED WITHIN   REA04960
C                    30*(IGH-LOW+1) ITERATIONS. THE EIGENVALUES OF      REA04970
C                    INDICES IER+1,...,N SHOULD BE CORRECT.             REA04980
C                    THE EIGENVECTORS ARE NOT COMPUTED.                 REA04990
C                                                                       REA05000
C     THIS SUBROUTINE IS BASED ON THE ALGOL PROCEDURE HQR2,             REA05010
C     NUM. MATH. 16, 181-204(1970) BY PETERS AND WILKINSON.             REA05020
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 372-395(1971).   REA05030
C                                                                       REA05040
C     VERSION FOR THE IBM 3090VF DATED NOVEMBER 1987.                   REA05050
C     ------------------------------------------------------------------REA05060
      INTEGER     I, J, K, L, M1, M2, MAXIT, IT                         REA05070
      REAL*8      P, Q, R, S, XSHIFT, T, X, Y, Z, ANORM, T1, T2, EPS    REA05080
      REAL*8      A, B, C, D, E, F                                      REA05090
      LOGICAL     NOTLAS, JNEK                                          REA05100
      DATA        EPS/Z3410000000000000/                                REA05110
C                                                                       REA05120
      CALL XUFLOW(0)                                                    REA05130
      IER=0                                                             REA05140
C ROOTS ISOLATED BY BALANC                                              REA05150
         DO 10   I=1,LOW-1                                              REA05160
         WR(I)=H(I,I)                                                   REA05170
10       WI(I)=0D0                                                      REA05180
         DO 20   I=IGH+1,N                                              REA05190
         WR(I)=H(I,I)                                                   REA05200
20       WI(I)=0D0                                                      REA05210
C MATRIX NORM                                                           REA05220
      ANORM=DABS(H(LOW,LOW))                                            REA05230
         DO 40  J=LOW+1,IGH                                             REA05240
         ANORM=ANORM+DABS(H(J,J-1))                                     REA05250
            DO 30  I=LOW,J                                              REA05260
30          ANORM=ANORM+DABS(H(I,J))                                    REA05270
40       CONTINUE                                                       REA05280
C INITIAL VALUES                                                        REA05290
      M=IGH                                                             REA05300
      XSHIFT=0D0                                                        REA05310
      MAXIT=30*(IGH-LOW+1)                                              REA05320
50    IF(M.LT.LOW) GO TO 320                                            REA05330
C MORE EIGENVALUES                                                      REA05340
      M1=M-1                                                            REA05350
      M2=M-2                                                            REA05360
      IT=0                                                              REA05370
C SEARCH FOR A SMALL SUBDIAGONAL SINGLETON                              REA05380
60       DO 70   L=M,LOW+1,-1                                           REA05390
         S=DABS(H(L-1,L-1))+DABS(H(L,L))                                REA05400
         IF(S.EQ.0D0) S=ANORM                                           REA05410
         T2=S+DABS(H(L,L-1))                                            REA05420
         IF(T2.EQ.S) GO TO 80                                           REA05430
70       CONTINUE                                                       REA05440
      L=LOW                                                             REA05450
C SHIFT                                                                 REA05460
80    X=H(M,M)                                                          REA05470
      IF(L.EQ.M) GO TO 260                                              REA05480
      Y=H(M1,M1)                                                        REA05490
      T=H(M,M1)*H(M1,M)                                                 REA05500
      IF (L.EQ.M1) GO TO 270                                            REA05510
      IF (IT.EQ.MAXIT) GO TO 999                                        REA05520
      IF (IT.NE.10 .AND. IT.NE.20) GO TO 100                            REA05530
C EXCEPTIONAL EXPLICIT SHIFT                                            REA05540
      XSHIFT=XSHIFT+X                                                   REA05550
         DO 90  I=LOW,M                                                 REA05560
90       H(I,I)=H(I,I)-X                                                REA05570
      S=DABS(H(M,M1))+DABS(H(M1,M2))                                    REA05580
      X=0.75D0*S                                                        REA05590
      Y=X                                                               REA05600
      T=-0.4375D0*S*S                                                   REA05610
100   IT=IT+1                                                           REA05620
      MAXIT=MAXIT-1                                                     REA05630
C SEARCH FOR TWO ADJACENT SMALL SUBDIAGONAL ELEMENTS                    REA05640
         DO 110 K=M2,L,-1                                               REA05650
         Z=H(K,K)                                                       REA05660
         R=X-Z                                                          REA05670
         S=Y-Z                                                          REA05680
         P=(R*S-T)/H(K+1,K)+H(K,K+1)                                    REA05690
         Q=H(K+1,K+1)-Z-R-S                                             REA05700
         R=H(K+2,K+1)                                                   REA05710
         S=DABS(P)+DABS(Q)+DABS(R)                                      REA05720
         P=P/S                                                          REA05730
         Q=Q/S                                                          REA05740
         R=R/S                                                          REA05750
         H(K+2,K)=0D0                                                   REA05760
         IF(K.EQ.L) GO TO 120                                           REA05770
         T1=DABS(P)*(DABS(H(K-1,K-1))+DABS(Z)+DABS(H(K+1,K+1)))         REA05780
         T2=T1+DABS(H(K,K-1))*(DABS(Q)+DABS(R))                         REA05790
         IF(T2.EQ.T1) GO TO 120                                         REA05800
         H(K+2,K-1)=0D0                                                 REA05810
110      CONTINUE                                                       REA05820
C DOUBLE QR STEP.  FOR A UNIT-STRIDE ALGORITHM, THE TRIPLETS DEFINING   REA05830
C    THE HOUSEHOLDER TRANSFORMATIONS ARE SAVED IN THE FREE CELLS OF     REA05840
C    WR, WI, AND H(*,1).                                                REA05850
120   JNEK=.FALSE.                                                      REA05860
         DO 230 J=K,M1                                                  REA05870
         NOTLAS=J.NE.M1                                                 REA05880
C     SCALED HOUSEHOLDER VECTOR (FIRST NONTRIVIAL ELEMENT IS UNITY)     REA05890
         IF(JNEK) GO TO 130                                             REA05900
            JNEK=.TRUE.                                                 REA05910
            S=-DSIGN(DSQRT(P*P+Q*Q+R*R),P)                              REA05920
            IF(L.NE.K) H(J,J-1)=-H(J,J-1)                               REA05930
            GO TO 140                                                   REA05940
130      P=H(J,J-1)                                                     REA05950
         Q=H(J+1,J-1)                                                   REA05960
         H(J+1,1)=R                                                     REA05970
         R=0D0                                                          REA05980
         IF(NOTLAS) R=H(J+2,J-1)                                        REA05990
         T=DMAX1(DABS(P),DABS(Q),DABS(R))                               REA06000
         WR(J)=0D0                                                      REA06010
         WI(J)=0D0                                                      REA06020
         IF(T.EQ.0D0) GO TO 200                                         REA06030
         P=P/T                                                          REA06040
         Q=Q/T                                                          REA06050
         R=R/T                                                          REA06060
         S=-DSIGN(DSQRT(P*P+Q*Q+R*R),P)                                 REA06070
         H(J,J-1)=S*T                                                   REA06080
140      P=P-S                                                          REA06090
         Q=Q/P                                                          REA06100
         R=R/P                                                          REA06110
         P=P/S                                                          REA06120
         WR(J)=P                                                        REA06130
         WI(J)=Q                                                        REA06140
C     RIGHT TRANSFORMATION OF COLUMNS J, J+1, J+2                       REA06150
         IF(NOTLAS) GO TO 170                                           REA06160
C"    ( PREFER VECTOR                                                   REA06170
            DO 150 I=1,MIN0(J+3,M)                                      REA06180
            T=P*(H(I,J)+Q*H(I,J+1))                                     REA06190
            H(I,J)  =H(I,J)+T                                           REA06200
150         H(I,J+1)=H(I,J+1)+T*Q                                       REA06210
C"    ( PREFER VECTOR                                                   REA06220
            DO 160 I=LOW,IGH                                            REA06230
            T=P*(VEC(I,J)+Q*VEC(I,J+1))                                 REA06240
            VEC(I,J)  =VEC(I,J)+T                                       REA06250
160         VEC(I,J+1)=VEC(I,J+1)+T*Q                                   REA06260
         GO TO 200                                                      REA06270
C"    ( PREFER VECTOR                                                   REA06280
170         DO 180 I=1,MIN0(J+3,M)                                      REA06290
            T=P*(H(I,J)+Q*H(I,J+1)+R*H(I,J+2))                          REA06300
            H(I,J)  =H(I,J)+T                                           REA06310
            H(I,J+1)=H(I,J+1)+T*Q                                       REA06320
180         H(I,J+2)=H(I,J+2)+T*R                                       REA06330
C"    ( PREFER VECTOR                                                   REA06340
            DO 190 I=LOW,IGH                                            REA06350
            T=P*(VEC(I,J)+Q*VEC(I,J+1)+R*VEC(I,J+2))                    REA06360
            VEC(I,J)  =VEC(I,J)+T                                       REA06370
            VEC(I,J+1)=VEC(I,J+1)+T*Q                                   REA06380
190         VEC(I,J+2)=VEC(I,J+2)+T*R                                   REA06390
C     LEFT TRANSFORMATION OF COLUMN J                                   REA06400
200         DO 210 I=K,J-1                                              REA06410
            T=WR(I)*(H(I,J)+WI(I)*H(I+1,J)+H(I+2,1)*H(I+2,J))           REA06420
            H(I,J)  =H(I,J)+T                                           REA06430
            H(I+1,J)=H(I+1,J)+T*WI(I)                                   REA06440
            H(I+2,J)=H(I+2,J)+T*H(I+2,1)                                REA06450
210         CONTINUE                                                    REA06460
         IF(NOTLAS) GO TO 220                                           REA06470
            T=P*(H(J,J)+Q*H(J+1,J))                                     REA06480
            H(J,J)  =H(J,J)+T                                           REA06490
            H(J+1,J)=H(J+1,J)+T*Q                                       REA06500
            GO TO 230                                                   REA06510
220      T=P*(H(J,J)+Q*H(J+1,J)+R*H(J+2,J))                             REA06520
         H(J,J)  =H(J,J)+T                                              REA06530
         H(J+1,J)=H(J+1,J)+T*Q                                          REA06540
         H(J+2,J)=H(J+2,J)+T*R                                          REA06550
230      CONTINUE                                                       REA06560
C COMPLETION OF THE LEFT TRANSFORMATION                                 REA06570
         DO 250 J=M,N                                                   REA06580
            DO 240 I=K,M2                                               REA06590
            T=WR(I)*(H(I,J)+WI(I)*H(I+1,J)+H(I+2,1)*H(I+2,J))           REA06600
            H(I,J)  =H(I,J)+T                                           REA06610
            H(I+1,J)=H(I+1,J)+T*WI(I)                                   REA06620
            H(I+2,J)=H(I+2,J)+T*H(I+2,1)                                REA06630
240         CONTINUE                                                    REA06640
            T=P*(H(M1,J)+Q*H(M,J))                                      REA06650
            H(M1,J)=H(M1,J)+T                                           REA06660
            H(M,J)=H(M,J)+T*Q                                           REA06670
250      CONTINUE                                                       REA06680
      GO TO 60                                                          REA06690
C ONE ROOT FOUND                                                        REA06700
260   H(M,M)=X+XSHIFT                                                   REA06710
      WR(M)=H(M,M)                                                      REA06720
      WI(M)=0D0                                                         REA06730
      M=M1                                                              REA06740
      GO TO 50                                                          REA06750
C TWO ROOTS FOUND                                                       REA06760
270   P=0.5D0*(Y-X)                                                     REA06770
      Q=P*P+T                                                           REA06780
      Z=DSQRT(DABS(Q))                                                  REA06790
      X=X+XSHIFT                                                        REA06800
      H(M,M)=X                                                          REA06810
      H(M1,M1)=Y+XSHIFT                                                 REA06820
      IF(Q.LT.0D0) GO TO 310                                            REA06830
C REAL PAIR                                                             REA06840
      Z=P+DSIGN(Z,P)                                                    REA06850
      WR(M1)=X+Z                                                        REA06860
      WR(M)=WR(M1)                                                      REA06870
      IF(Z.NE.0D0) WR(M)=X-T/Z                                          REA06880
      WI(M1)=0D0                                                        REA06890
      WI(M)=0D0                                                         REA06900
      X=H(M,M1)                                                         REA06910
      S=DABS(X)+DABS(Z)                                                 REA06920
      P=X/S                                                             REA06930
      Q=Z/S                                                             REA06940
      R=DSQRT(P*P+Q*Q)                                                  REA06950
      P=P/R                                                             REA06960
      Q=Q/R                                                             REA06970
C   DIAGONALIZATION OF THE BLOCK OF ORDER 2                             REA06980
C      LEFT TRANSFORMATION                                              REA06990
C"    ( PREFER VECTOR                                                   REA07000
         DO 280 I=M1,N                                                  REA07010
         Z=H(M1,I)                                                      REA07020
         H(M1,I)=P*H(M,I)+Q*Z                                           REA07030
280      H(M ,I)=Q*H(M,I)-P*Z                                           REA07040
C      RIGHT TRANSFORMATION                                             REA07050
         DO 290 I=1,M                                                   REA07060
         Z=H(I,M1)                                                      REA07070
         H(I,M1)=P*H(I,M)+Q*Z                                           REA07080
290      H(I,M )=Q*H(I,M)-P*Z                                           REA07090
C      RIGHT MATRIX OF THE SIMILARITY TRANSFORMATION TO TRIANGULAR FORM REA07100
         DO 300 I=LOW,IGH                                               REA07110
         Z=VEC(I,M1)                                                    REA07120
         VEC(I,M1)=P*VEC(I,M)+Q*Z                                       REA07130
300      VEC(I,M )=Q*VEC(I,M)-P*Z                                       REA07140
      M=M2                                                              REA07150
      GO TO 50                                                          REA07160
C COMPLEX CONJUGATE PAIR                                                REA07170
310   WR(M)=X+P                                                         REA07180
      WR(M1)=WR(M)                                                      REA07190
      WI(M1)=Z                                                          REA07200
      WI(M)=-Z                                                          REA07210
      M=M2                                                              REA07220
      GO TO 50                                                          REA07230
C EIGENVECTORS OF THE (BLOCK-) TRIANGULAR MATRIX PRODUCED BY QR         REA07240
320   IF(ANORM.EQ.0D0) GO TO 1000                                       REA07250
      Z=EPS*ANORM                                                       REA07260
         DO 650  M=N,1,-1                                               REA07270
         P=WR(M)                                                        REA07280
         Q=WI(M)                                                        REA07290
         IF(Q.NE.0D0) GO TO 420                                         REA07300
C    REAL VECTOR                                                        REA07310
         H(M,M)=1D0                                                     REA07320
            DO 410 K=M-1,1,-1                                           REA07330
            IF(WI(K)) 340,330,360                                       REA07340
C       ISOLATED COMPONENT                                              REA07350
330         R=P-H(K,K)                                                  REA07360
            IF(R.EQ.0D0) R=Z                                            REA07370
            H(K,M)=H(K,M)/R                                             REA07380
            GO TO 370                                                   REA07390
C       LOWER COMPONENT ASSOCIATED WITH A BLOCK OF ORDER 2              REA07400
340         R=P-H(K,K)                                                  REA07410
            S=P-H(K-1,K-1)                                              REA07420
            IF(DABS(S).LT.DABS(H(K,K-1))) GO TO 350                     REA07430
               T=H(K,K-1)/S                                             REA07440
               H(K,M)=(H(K,M)+T*H(K-1,M))/(R-T*H(K-1,K))                REA07450
               X=(H(K-1,M)+H(K-1,K)*H(K,M))/S                           REA07460
               GO TO 370                                                REA07470
350         T=S/H(K,K-1)                                                REA07480
            X=H(K,M)                                                    REA07490
            H(K,M)=(T*H(K,M)+H(K-1,M))/(T*R-H(K-1,K))                   REA07500
            X=(R*H(K,M)-X)/H(K,K-1)                                     REA07510
            GO TO 370                                                   REA07520
C       UPPER COMPONENT ASSOCIATED WITH A BLOCK OF ORDER 2              REA07530
360         H(K,M)=X                                                    REA07540
C       PROTECTION AGAINST OVERFLOW OF FLOATING-POINT REPRESENTATION    REA07550
370         T1=DABS(H(K,M))                                             REA07560
            IF(T1.EQ.0D0) GO TO 410                                     REA07570
            S=1D0/T1                                                    REA07580
            T2=T1+S                                                     REA07590
            IF(T2.GT.T1) GO TO 390                                      REA07600
               DO 380 I=1,M                                             REA07610
380            H(I,M)=S*H(I,M)                                          REA07620
            IF(WI(K).LT.0D0) X=S*X                                      REA07630
C       UPDATE OF THE RIGHT-HAND SIDE                                   REA07640
C"    ( IGNORE RECRDEPS                                                 REA07650
390            DO 400 I=1,K-1                                           REA07660
400            H(I,M)=H(I,M)+H(K,M)*H(I,K)                              REA07670
410         CONTINUE                                                    REA07680
         GO TO 650                                                      REA07690
C    COMPLEX VECTOR ASSOCIATED WITH ROOT P-IQ, Q<0                      REA07700
420      IF(Q.GT.0D0) GO TO 650                                         REA07710
C       LAST COMPONENT IS IMAGINARY.  COMPONENT NEXT TO LAST            REA07720
         IF(DABS(H(M,M-1)).LT.DABS(H(M-1,M))) GO TO 430                 REA07730
            H(M-1,M-1)=Q/H(M,M-1)                                       REA07740
            H(M-1,M)=(P-H(M,M))/H(M,M-1)                                REA07750
            GO TO 450                                                   REA07760
430      A=H(M-1,M)                                                     REA07770
         D=H(M-1,M-1)-P                                                 REA07780
         IF(DABS(Q).LT.DABS(D)) GO TO 440                               REA07790
            E=-D/Q                                                      REA07800
            A=A/(D*E-Q)                                                 REA07810
            H(M-1,M-1)=A                                                REA07820
            H(M-1,M  )=-A*E                                             REA07830
            GO TO 450                                                   REA07840
440      E=-Q/D                                                         REA07850
         A=A/(D-Q*E)                                                    REA07860
         H(M-1,M-1)=A*E                                                 REA07870
         H(M-1,M)  =-A                                                  REA07880
450      H(M,M-1)=0D0                                                   REA07890
         H(M,M)=1D0                                                     REA07900
C       RIGHT-HAND SIDE                                                 REA07910
C"    ( IGNORE RECRDEPS                                                 REA07920
            DO 460 I=1,M-2                                              REA07930
            H(I,M  )=H(I,M)+H(M-1,M)*H(I,M-1)                           REA07940
460         H(I,M-1)=H(M-1,M-1)*H(I,M-1)                                REA07950
C       BACKWARD SUBSTITUTION                                           REA07960
            DO 640  K=M-2,1,-1                                          REA07970
            IF(WI(K))490,470,590                                        REA07980
C          ISOLATED COMPONENT                                           REA07990
470         A=H(K,M-1)                                                  REA08000
            B=H(K,M)                                                    REA08010
            C=P-H(K,K)                                                  REA08020
            IF(DABS(C).LT.DABS(Q)) GO TO 480                            REA08030
               E=-Q/C                                                   REA08040
               F=C-Q*E                                                  REA08050
               H(K,M-1)=(A+B*E)/F                                       REA08060
               H(K,M  )=(B-A*E)/F                                       REA08070
               GO TO 600                                                REA08080
480         E=-C/Q                                                      REA08090
            F=C*E-Q                                                     REA08100
            H(K,M-1)=(A*E+B)/F                                          REA08110
            H(K,M  )=(B*E-A)/F                                          REA08120
            GO TO 600                                                   REA08130
C          LOWER COMPONENT ASSOCIATED WITH A BLOCK OF ORDER 2           REA08140
490         R=P-H(K,K)                                                  REA08150
            S=P-H(K-1,K-1)                                              REA08160
            IF(DABS(S)+DABS(Q).LT.DABS(H(K,K-1))) GO TO 550             REA08170
               IF(DABS(S).LT.DABS(Q)) GO TO 500                         REA08180
                  E=Q/S                                                 REA08190
                  A=H(K,K-1)/(S+Q*E)                                    REA08200
                  B=A*E                                                 REA08210
                  GO TO 510                                             REA08220
500            E=S/Q                                                    REA08230
               B=H(K,K-1)/(Q+S*E)                                       REA08240
               A=B*E                                                    REA08250
510            X=H(K,M-1)+A*H(K-1,M-1)-B*H(K-1,M)                       REA08260
               Y=H(K,M  )+B*H(K-1,M-1)+A*H(K-1,M)                       REA08270
               T1= R-A*H(K-1,K)                                         REA08280
               T2=-Q-B*H(K-1,K)                                         REA08290
               IF(T1.EQ.0D0 .AND. T2.EQ.0D0) T1=Z                       REA08300
               IF(DABS(T1).LT.DABS(T2)) GO TO 520                       REA08310
                  E=T2/T1                                               REA08320
                  F=T1+T2*E                                             REA08330
                  H(K,M-1)=(X+Y*E)/F                                    REA08340
                  H(K,M  )=(Y-X*E)/F                                    REA08350
                  GO TO 530                                             REA08360
520            E=T1/T2                                                  REA08370
               F=T1*E+T2                                                REA08380
               H(K,M-1)=(X*E+Y)/F                                       REA08390
               H(K,M  )=(Y*E-X)/F                                       REA08400
530            A=H(K-1,M-1)+H(K-1,K)*H(K,M-1)                           REA08410
               B=H(K-1,M  )+H(K-1,K)*H(K,M  )                           REA08420
               IF(DABS(S).LT.DABS(Q)) GO TO 540                         REA08430
                  E=Q/S                                                 REA08440
                  F=S+Q*E                                               REA08450
                  X=(A-B*E)/F                                           REA08460
                  Y=(B+A*E)/F                                           REA08470
                  GO TO 600                                             REA08480
540            E=S/Q                                                    REA08490
               F=S*E+Q                                                  REA08500
               X=(A*E-B)/F                                              REA08510
               Y=(B*E+A)/F                                              REA08520
               GO TO 600                                                REA08530
550         T1= S/H(K,K-1)                                              REA08540
            T2=-Q/H(K,K-1)                                              REA08550
            X=T1*H(K,M-1)-T2*H(K,M)+H(K-1,M-1)                          REA08560
            Y=T2*H(K,M-1)+T1*H(K,M)+H(K-1,M  )                          REA08570
            C=T1*R+T2*Q-H(K-1,K)                                        REA08580
            D=T2*R-T1*Q                                                 REA08590
            IF(C.NE.0D0 .OR. D.NE.0D0) GO TO 560                        REA08600
               C=X/Z                                                    REA08610
               D=Y/Z                                                    REA08620
               GO TO 580                                                REA08630
560         IF(DABS(C).LT.DABS(D)) GO TO 570                            REA08640
               E=D/C                                                    REA08650
               F=C+D*E                                                  REA08660
               C=(X+Y*E)/F                                              REA08670
               D=(Y-X*E)/F                                              REA08680
               GO TO 580                                                REA08690
570         E=C/D                                                       REA08700
            F=C*E+D                                                     REA08710
            C=(X*E+Y)/F                                                 REA08720
            D=(Y*E-X)/F                                                 REA08730
580         X=H(K,M-1)                                                  REA08740
            Y=H(K,M  )                                                  REA08750
            H(K,M-1)=C                                                  REA08760
            H(K,M  )=D                                                  REA08770
            X=(R*H(K,M-1)+Q*H(K,M)-X)/H(K,K-1)                          REA08780
            Y=(R*H(K,M)-Q*H(K,M-1)-Y)/H(K,K-1)                          REA08790
            GO TO 600                                                   REA08800
C       UPPER COMPONENT ASSOCIATED WITH A BLOCK OF ORDER 2              REA08810
590         H(K,M-1)=X                                                  REA08820
            H(K,M  )=Y                                                  REA08830
C       PROTECTION AGAINST OVERFLOW OF FLOATING-POINT REPRESENTATION    REA08840
600         T1=DMAX1(DABS(H(K,M-1)),DABS(H(K,M)))                       REA08850
            IF(T1.EQ.0D0) GO TO 640                                     REA08860
            S=1D0/T1                                                    REA08870
            T2=T1+S                                                     REA08880
            IF(T2.GT.T1) GO TO 620                                      REA08890
               DO 610 I=1,M                                             REA08900
               H(I,M-1)=S*H(I,M-1)                                      REA08910
610            H(I,M)  =S*H(I,M)                                        REA08920
            IF(WI(K).GE.0D0) GO TO 620                                  REA08930
               X=S*X                                                    REA08940
               Y=S*Y                                                    REA08950
C       UPDATE OF THE RIGHT-HAND SIDE                                   REA08960
C"    ( IGNORE RECRDEPS                                                 REA08970
620            DO 630  I=1,K-1                                          REA08980
               H(I,M-1)=H(I,M-1)+H(K,M-1)*H(I,K)                        REA08990
630            H(I,M)  =H(I,M)  +H(K,M  )*H(I,K)                        REA09000
640         CONTINUE                                                    REA09010
650      CONTINUE                                                       REA09020
C EIGENVECTORS OF THE HESSENBERG MATRIX                                 REA09030
C    VECTORS OF THE ROOTS ISOLATED BY BALANC                            REA09040
         DO 670  J=1,N                                                  REA09050
            DO 660  I=1,MIN0(LOW-1,J)                                   REA09060
660         VEC(I,J)=H(I,J)                                             REA09070
670      CONTINUE                                                       REA09080
         DO 690  J=IGH+1,N                                              REA09090
            DO 680  I=1,J                                               REA09100
680         VEC(I,J)=H(I,J)                                             REA09110
690      CONTINUE                                                       REA09120
C    OTHER VECTORS: TRANSFORMATION BY THE RIGHT MATRIX OF THE REDUCTION REA09130
C                   TO TRIANGULAR FORM                                  REA09140
         DO 720  J=N,LOW,-1                                             REA09150
         M=MIN0(J,IGH)                                                  REA09160
            DO 710  I=LOW,IGH                                           REA09170
            S=0D0                                                       REA09180
               DO 700  K=LOW,M                                          REA09190
700            S=S+VEC(I,K)*H(K,J)                                      REA09200
710         VEC(I,J)=S                                                  REA09210
720      CONTINUE                                                       REA09220
      GO TO 1000                                                        REA09230
C ERROR: NOT ALL THE EIGENVALUES COULD BE OBTAINED IN 30*(IGH-LOW+1)    REA09240
C        ITERATIONS                                                     REA09250
999   IER=M                                                             REA09260
C                                                                       REA09270
1000  RETURN                                                            REA09280
      END                                                               REA09290
