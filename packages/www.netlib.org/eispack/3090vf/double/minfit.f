@PROCESS DIRECTIVE('"    (')                                            SIN04100
C-----------------------------------------------------------------------SIN04110
C FOR A GIVEN SYSTEM OF LINEAR EQUATIONS  A X = B,  THIS SUBROUTINE     SIN04120
C COMPUTES V, D, AND  U'B,  WHERE  A = U D V'  IS THE SINGULAR-VALUE    SIN04130
C DECOMPOSITION OF A.                                                   SIN04140
C                                                                       SIN04150
      SUBROUTINE  MINFIT( LD, M, N, A, D, NB, B, IERR, E )              SIN04160
C                                                                       SIN04170
      INTEGER     M, N, NB, LD, IERR                                    SIN04180
      REAL*8      A(LD,N), D(N), B(LD,NB), E(N)                         SIN04190
C                                                                       SIN04200
C LD     E  FIRST DIMENSION ASSIGNED TO THE TWO-DIMENSIONAL ARRAY       SIN04210
C           PARAMETERS IN THE CALLING PROGRAM.  LD MUST NOT BE LESS     SIN04220
C           THAN MAX(M,N).                                              SIN04230
C                                                                       SIN04240
C M      E  NUMBER OF ROWS OF A AND B.                                  SIN04250
C                                                                       SIN04260
C N      E  NUMBER OF COLUMNS OF A AND ORDER OF V.                      SIN04270
C                                                                       SIN04280
C A      E  COEFFICIENT MATRIX OF THE SYSTEM.                           SIN04290
C        R  N BY N MATRIX V.  WHEN AN ERROR RETURN OCCURS, THE COLUMNS  SIN04300
C           OF V OF INDICES IERR+1,...,N SHOULD BE CORRECT.             SIN04310
C                                                                       SIN04320
C D      R  THE N SINGULAR VALUES OF A, UNORDERED.  WHEN AN ERROR RETURNSIN04330
C           OCCURS, THE SINGULAR VALUES OF INDICES IERR+1,...,N SHOULD  SIN04340
C           BE CORRECT.                                                 SIN04350
C                                                                       SIN04360
C NB     E  NUMBER OF COLUMNS OF B.  NB CAN BE ZERO.                    SIN04370
C                                                                       SIN04380
C B      E  RIGHT-HAND SIDE OF THE LINEAR SYSTEM IF NB IS NOT ZERO.     SIN04390
C           OTHERWISE, NO REFERENCE IS MADE TO B.                       SIN04400
C                                                                       SIN04410
C        R  MATRIX U'B.  WHEN AN ERROR RETURN OCCURS, THE ROWS OF U'B   SIN04420
C           OF INDICES IERR+1,...,N SHOULD BE CORRECT.                  SIN04430
C                                                                       SIN04440
C IERR   R  ZERO  FOR NORMAL RETURN,                                    SIN04450
C           K     IF THE K-TH SINGULAR VALUE CANNOT BE OBTAINED WITHIN  SIN04460
C                    30 ITERATIONS.                                     SIN04470
C                                                                       SIN04480
C E      -  ANCILLARY STORAGE.                                          SIN04490
C                                                                       SIN04500
C     THIS SUBROUTINE IS BASED ON THE ALGOL PROCEDURE MINFIT,           SIN04510
C     NUM. MATH. 14, 403-420(1970) BY GOLUB AND REINSCH.                SIN04520
C     HANDBOOK FOR AUTO. COMP., VOL II-LINEAR ALGEBRA, 134-151(1971).   SIN04530
C                                                                       SIN04540
C SUBPROGRAMS CALLED:  DSC16   KACHEL                                   SIN04550
C                                                                       SIN04560
C     VERSION FOR THE IBM 3090VF DATED NOVEMBER 1987.                   SIN04570
C     ------------------------------------------------------------------SIN04580
      INTEGER     I, J, K, KK, L, MN, MAXIT, IT, KACHE                  SIN04590
      INTEGER     KACHEL                                                SIN04600
      REAL*8      C, S, X, Y, Z, F, G, H, BNORM, S16, R16               SIN04610
      DATA        MAXIT/30/                                             SIN04620
C                                                                       SIN04630
      CALL XUFLOW(0)                                                    SIN04640
      IERR=0                                                            SIN04650
C                                                                       SIN04660
C REDUCTION TO UPPER-BIDIAGONAL FORM BY HOUSEHOLDER TRANSFORMATIONS     SIN04670
      E(1)=0D0                                                          SIN04680
      MN=MIN0(M,N)                                                      SIN04690
      KACHE=KACHEL(LD)                                                  SIN04700
      KK=N-KACHE                                                        SIN04710
         DO 340 K=1,MN                                                  SIN04720
C   L1 NORM OF THE SUBDIAGONAL PART OF COLUMN K                         SIN04730
         X=0D0                                                          SIN04740
            DO 120 I=K+1,M                                              SIN04750
120         X=X+DABS(A(I,K))                                            SIN04760
         IF(X.NE.0D0) GO TO 140                                         SIN04770
C   THE TRANSFORMATION IS BYPASSED IF THE NORM IS ZERO                  SIN04780
            D(K)=A(K,K)                                                 SIN04790
               DO 130 J=K+1,N                                           SIN04800
130            E(J)=A(K,J)                                              SIN04810
            GO TO 240                                                   SIN04820
140      X=X+DABS(A(K,K))                                               SIN04830
C   SCALED HOUSEHOLDER VECTOR                                           SIN04840
         CALL DSC16 (X,S16,R16)                                         SIN04850
         S=0D0                                                          SIN04860
            DO 150 I=K,M                                                SIN04870
            A(I,K)=R16*A(I,K)                                           SIN04880
150         S=S+A(I,K)**2                                               SIN04890
         S=-DSIGN(DSQRT(S),A(K,K))                                      SIN04900
         X=1D0/S                                                        SIN04910
            DO 160 I=K,M                                                SIN04920
160         A(I,K)=X*A(I,K)                                             SIN04930
         A(K,K)=A(K,K)-1D0                                              SIN04940
C   DIAGONAL ELEMENT                                                    SIN04950
         D(K)=S16*S                                                     SIN04960
C   LEFT TRANSFORMATION                                                 SIN04970
         X=1D0/A(K,K)                                                   SIN04980
         IF(K.LT.KK) GO TO 200                                          SIN04990
C     PATH FOR SMALL ARRAYS                                             SIN05000
C"    ( IGNORE RECRDEPS                                                 SIN05010
C"    ( PREFER VECTOR                                                   SIN05020
            DO 190 J=K+1,N                                              SIN05030
            S=0D0                                                       SIN05040
               DO 170 I=K,M                                             SIN05050
170            S=S+A(I,J)*A(I,K)                                        SIN05060
            S=S*X                                                       SIN05070
               DO 180 I=K,M                                             SIN05080
180            A(I,J)=A(I,J)+S*A(I,K)                                   SIN05090
            E(J)=A(K,J)                                                 SIN05100
190         CONTINUE                                                    SIN05110
         GO TO 232                                                      SIN05120
C     PATH FOR LARGE ARRAYS                                             SIN05130
200         DO 230 J=K+1,N                                              SIN05140
            S=0D0                                                       SIN05150
               DO 210 I=K,M                                             SIN05160
210            S=S+A(I,J)*A(I,K)                                        SIN05170
            S=S*X                                                       SIN05180
               DO 220 I=K,M                                             SIN05190
220            A(I,J)=A(I,J)+S*A(I,K)                                   SIN05200
            E(J)=A(K,J)                                                 SIN05210
230         CONTINUE                                                    SIN05220
C   TRANSFORMATION OF THE RIGHT-HAND SIDE                               SIN05230
232         DO 238 J=1,NB                                               SIN05240
            S=0D0                                                       SIN05250
               DO 234 I=K,M                                             SIN05260
234            S=S+A(I,K)*B(I,J)                                        SIN05270
            S=S*X                                                       SIN05280
               DO 236 I=K,M                                             SIN05290
236            B(I,J)=B(I,J)+A(I,K)*S                                   SIN05300
238         CONTINUE                                                    SIN05310
240      IF(K.EQ.N) GO TO 350                                           SIN05320
C   L1 NORM OF THE SUPERCODIAGONAL PART OF ROW K                        SIN05330
         X=0D0                                                          SIN05340
            DO 250 J=K+2,N                                              SIN05350
250         X=X+DABS(E(J))                                              SIN05360
         IF(X.NE.0D0) GO TO 260                                         SIN05370
C   THE TRANSFORMATION IS BYPASSED IF THE NORM IS ZERO                  SIN05380
               DO 255 I=K+1,N                                           SIN05390
255            A(I,K)=0D0                                               SIN05400
            GO TO 340                                                   SIN05410
C   SCALED HOUSEHOLDER VECTOR                                           SIN05420
260      X=X+DABS(E(K+1))                                               SIN05430
         CALL DSC16(X,S16,R16)                                          SIN05440
         S=0D0                                                          SIN05450
            DO 270 J=K+1,N                                              SIN05460
            E(J)=R16*E(J)                                               SIN05470
270         S=S+E(J)**2                                                 SIN05480
         Y=-DSIGN(DSQRT(S),E(K+1))                                      SIN05490
         E(K+1)=E(K+1)-Y                                                SIN05500
         X=1D0/Y                                                        SIN05510
            DO 280 J=K+1,N                                              SIN05520
            E(J)=X*E(J)                                                 SIN05530
280         A(J,K)=E(J)                                                 SIN05540
         X=1D0/E(K+1)                                                   SIN05550
C   RIGHT TRANSFORMATION                                                SIN05560
            DO 330 I=K+1,M                                              SIN05570
            S=0D0                                                       SIN05580
               DO 310 J=K+1,N                                           SIN05590
310            S=S+A(I,J)*E(J)                                          SIN05600
            S=S*X                                                       SIN05610
               DO 320 J=K+1,N                                           SIN05620
320            A(I,J)=A(I,J)+E(J)*S                                     SIN05630
330         CONTINUE                                                    SIN05640
C   CODIAGONAL ELEMENT                                                  SIN05650
         E(K+1)=S16*Y                                                   SIN05660
340      CONTINUE                                                       SIN05670
350   L=MN                                                              SIN05680
      IF(M.LT.N) L=L+1                                                  SIN05690
C RIGHT MATRIX OF THE BIDIAGONAL DECOMPOSITION                          SIN05700
         DO 380 J=L+1,N                                                 SIN05710
            DO 370 I=1,N                                                SIN05720
370         A(I,J)=0D0                                                  SIN05730
380      A(J,J)=1D0                                                     SIN05740
      KK=N-KACHE                                                        SIN05750
         DO 570  K=L,2,-1                                               SIN05760
            DO 390 I=1,K-1                                              SIN05770
390         A(I,K)=0D0                                                  SIN05780
            DO 400 I=K,N                                                SIN05790
400         A(I,K)=A(I,K-1)                                             SIN05800
         IF(A(K,K).EQ.0D0) GO TO 570                                    SIN05810
         X=1D0/A(K,K)                                                   SIN05820
         IF(K.LT.KK) GO TO 530                                          SIN05830
C   PATH FOR SMALL ARRAYS                                               SIN05840
C"    ( IGNORE RECRDEPS                                                 SIN05850
C"    ( PREFER VECTOR                                                   SIN05860
            DO 520 J=K+1,N                                              SIN05870
            S=0D0                                                       SIN05880
               DO 500 I=K,N                                             SIN05890
500            S=S+A(I,K)*A(I,J)                                        SIN05900
            S=S*X                                                       SIN05910
               DO 510 I=K,N                                             SIN05920
510            A(I,J)=A(I,J)+S*A(I,K)                                   SIN05930
520         CONTINUE                                                    SIN05940
         GO TO 570                                                      SIN05950
C   PATH FOR LARGE ARRAYS                                               SIN05960
530         DO 560  J=K+1,N                                             SIN05970
            S=0D0                                                       SIN05980
               DO 540  I=K,N                                            SIN05990
540            S=S+A(I,K)*A(I,J)                                        SIN06000
            S=S*X                                                       SIN06010
               DO 550  I=K,N                                            SIN06020
550            A(I,J)=A(I,J)+S*A(I,K)                                   SIN06030
560         CONTINUE                                                    SIN06040
570      A(K,K)=1D0+A(K,K)                                              SIN06050
      A(1,1)=1D0                                                        SIN06060
         DO 580  I=2,N                                                  SIN06070
580      A(I,1)=0D0                                                     SIN06080
C ANNIHILATION OF THE LAST SUPERDIAGONAL ELEMENT WHEN  M < N            SIN06090
      IF(M.GE.N) GO TO 640                                              SIN06100
      C= 1D0                                                            SIN06110
      S=-1D0                                                            SIN06120
         DO 630  K=MN,1,-1                                              SIN06130
         X=-S*E(K+1)                                                    SIN06140
         E(K+1)=C*E(K+1)                                                SIN06150
         IF(X.EQ.0D0) GO TO 640                                         SIN06160
         IF(DABS(D(K)).LT.DABS(X)) GO TO 600                            SIN06170
            S=X/D(K)                                                    SIN06180
            Y=DSQRT(1D0+S*S)                                            SIN06190
            C=1D0/Y                                                     SIN06200
            S=S*C                                                       SIN06210
            D(K)=Y*D(K)                                                 SIN06220
            GO TO 610                                                   SIN06230
600      C=D(K)/X                                                       SIN06240
         Y=DSQRT(1D0+C*C)                                               SIN06250
         S=1D0/Y                                                        SIN06260
         C=C*S                                                          SIN06270
         D(K)=Y*X                                                       SIN06280
610         DO 620  I=1,N                                               SIN06290
            X=A(I,K)                                                    SIN06300
            Y=A(I,L)                                                    SIN06310
            A(I,K )=C*X+S*Y                                             SIN06320
620         A(I,L)=C*Y-S*X                                              SIN06330
630      CONTINUE                                                       SIN06340
640      DO 650  I=MN+1,N                                               SIN06350
            DO 655 J=1,NB                                               SIN06360
655         B(I,J)=0D0                                                  SIN06370
650      D(I)=0D0                                                       SIN06380
C                                                                       SIN06390
C SINGULAR-VALUE DECOMPOSITION OF THE UPPER-BIDIAGONAL MATRIX OF ORDER  SIN06400
C    MIN(M,N)                                                           SIN06410
C   L1 NORM OF THE BIDIAGONAL MATRIX                                    SIN06420
      BNORM=0D00                                                        SIN06430
         DO 660  I=1,MN                                                 SIN06440
         BNORM=DMAX1(DABS(D(I))+DABS(E(I)),BNORM)                       SIN06450
660      CONTINUE                                                       SIN06460
C   DIAGONALIZATION                                                     SIN06470
         DO 860  K=MN,1,-1                                              SIN06480
         IT=0                                                           SIN06490
C   TESTS FOR SPLITTING                                                 SIN06500
670         DO 680  L=K,1,-1                                            SIN06510
            X=BNORM+DABS(E(L))                                          SIN06520
            IF(X.EQ.BNORM) GO TO 740                                    SIN06530
            X=BNORM+DABS(D(L-1))                                        SIN06540
            IF (X.EQ.BNORM) GO TO 690                                   SIN06550
680         CONTINUE                                                    SIN06560
C   CANCELLATION OF E(L)                                                SIN06570
690      C=0D00                                                         SIN06580
         S=1D00                                                         SIN06590
            DO 730  I=L,K                                               SIN06600
            F=S*E(I)                                                    SIN06610
            E(I)=C*E(I)                                                 SIN06620
            X=BNORM+DABS(F)                                             SIN06630
            IF(X.EQ.BNORM) GO TO 740                                    SIN06640
            G=D(I)                                                      SIN06650
            IF(DABS(G).LT.DABS(F)) GO TO 700                            SIN06660
               S=F/G                                                    SIN06670
               H=DSQRT(1D0+S*S)                                         SIN06680
               C=1D0/H                                                  SIN06690
               S=-S*C                                                   SIN06700
               D(I)=G*H                                                 SIN06710
               GO TO 710                                                SIN06720
700         C=G/F                                                       SIN06730
            H=DSQRT(1D0+C*C)                                            SIN06740
            S=-1D0/H                                                    SIN06750
            C=-C*S                                                      SIN06760
            D(I)=F*H                                                    SIN06770
C   UPDATE OF THE RIGHT-HAND SIDE                                       SIN06780
710            DO 720  J=1,NB                                           SIN06790
               Y=B(L-1,J)                                               SIN06800
               Z=B(I,J)                                                 SIN06810
               B(L-1,J)=Y*C+Z*S                                         SIN06820
               B(I,J)=-Y*S+Z*C                                          SIN06830
720            CONTINUE                                                 SIN06840
730         CONTINUE                                                    SIN06850
C TEST FOR CONVERGENCE                                                  SIN06860
740      Z=D(K)                                                         SIN06870
         IF (L.EQ.K) GO TO 840                                          SIN06880
C SHIFT FROM BOTTOM PRINCIPAL MATRIX OF ORDER 2                         SIN06890
         IF (IT.EQ.MAXIT) GO TO 999                                     SIN06900
         IT=IT+1                                                        SIN06910
         X=D(L)                                                         SIN06920
         Y=D(K-1)                                                       SIN06930
         G=E(K-1)                                                       SIN06940
         H=E(K)                                                         SIN06950
         F=0.5D00*(((G+Z)/H)*((G-Z)/Y)+Y/H-H/Y)                         SIN06960
         IF(DABS(F).GE.1D0) GO TO 750                                   SIN06970
            F=X-(Z/X)*Z+(H/X)*(Y/(F+DSIGN(DSQRT(1D0+F*F),F))-H)         SIN06980
            GO TO 760                                                   SIN06990
750      F=X-(Z/X)*Z+(H/X)*(Y/(F*(1D0+DSQRT(1D0+(1D0/F)**2)))-H)        SIN07000
C QR SWEEP                                                              SIN07010
760      C=1D00                                                         SIN07020
         S=1D00                                                         SIN07030
            DO 830  I=L,K-1                                             SIN07040
            G=E(I+1)                                                    SIN07050
            Y=D(I+1)                                                    SIN07060
            H=S*G                                                       SIN07070
            G=C*G                                                       SIN07080
            IF(DABS(F).LT.DABS(H)) GO TO 770                            SIN07090
               S=H/F                                                    SIN07100
               Z=DSQRT(1D0+S*S)                                         SIN07110
               C=1D0/Z                                                  SIN07120
               S=S*C                                                    SIN07130
               E(I)=F*Z                                                 SIN07140
               GO TO 780                                                SIN07150
770         C=F/H                                                       SIN07160
            Z=DSQRT(1D0+C*C)                                            SIN07170
            S=1D0/Z                                                     SIN07180
            C=C*S                                                       SIN07190
            E(I)=H*Z                                                    SIN07200
780         F=X*C+G*S                                                   SIN07210
            G=-X*S+G*C                                                  SIN07220
            H=Y*S                                                       SIN07230
            Y=Y*C                                                       SIN07240
C   UPDATE OF MATRIX V                                                  SIN07250
               DO 790  J=1,N                                            SIN07260
               X=A(J,I)                                                 SIN07270
               Z=A(J,I+1)                                               SIN07280
               A(J,I)=X*C+Z*S                                           SIN07290
               A(J,I+1)=-X*S+Z*C                                        SIN07300
790            CONTINUE                                                 SIN07310
            Z=DMAX1(DABS(F),DABS(H))                                    SIN07320
            Z=Z*DSQRT((F/Z)**2+(H/Z)**2)                                SIN07330
            D(I)=Z                                                      SIN07340
            IF (Z.EQ.0D00) GO TO 810                                    SIN07350
            C=F/Z                                                       SIN07360
            S=H/Z                                                       SIN07370
810         F=C*G+S*Y                                                   SIN07380
            X=-S*G+C*Y                                                  SIN07390
C   UPDATE OF THE RIGHT-HAND SIDE                                       SIN07400
               DO 820  J=1,NB                                           SIN07410
               Y=B(I,J)                                                 SIN07420
               Z=B(I+1,J)                                               SIN07430
               B(I,J)=Y*C+Z*S                                           SIN07440
               B(I+1,J)=-Y*S+Z*C                                        SIN07450
820            CONTINUE                                                 SIN07460
830         CONTINUE                                                    SIN07470
         E(L)=0D00                                                      SIN07480
         E(K)=F                                                         SIN07490
         D(K)=X                                                         SIN07500
         GO TO 670                                                      SIN07510
C   K-TH SINGULAR VALUE                                                 SIN07520
840      IF (Z.GE.0D00) GO TO 860                                       SIN07530
         D(K)=-Z                                                        SIN07540
            DO 850  J=1,N                                               SIN07550
850         A(J,K)=-A(J,K)                                              SIN07560
860      CONTINUE                                                       SIN07570
      GO TO 1000                                                        SIN07580
C                                                                       SIN07590
C NO CONVERGENCE TO A SINGULAR VALUE AFTER MAXIT ITERATIONS             SIN07600
999   IERR=K                                                            SIN07610
C                                                                       SIN07620
1000  RETURN                                                            SIN07630
      END                                                               SIN07640
