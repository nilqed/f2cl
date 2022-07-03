C-----------------------------------------------------------------------COM20840
C INTEGER POWER OF 16 "NEAR" A POSITIVE SCALAR AND RECIPROCAL           COM20850
C                                                                       COM20860
      SUBROUTINE DSC16 ( S, S16, R16 )                                  COM20870
C                                                                       COM20880
      REAL*8   S, S16, R16                                              COM20890
C                                                                       COM20900
C S      E  POSITIVE SCALAR                                             COM20910
C                                                                       COM20920
C S16    R  INTEGER POWER OF 16 "NEAR" S                                COM20930
C                                                                       COM20940
C R16    R  RECIPROCAL OF S16                                           COM20950
C                                                                       COM20960
C AUTHOR: A.A. DUBRULLE   IBM PASC (MAY 1987)                           COM20970
C-----------------------------------------------------------------------COM20980
      REAL        H                                                     COM20990
      INTEGER     K, R                                                  COM21000
      DATA        R/51380224/                                           COM21010
C     R IS A REPRESENTATION OF THE RECIPROCAL OF THE LARGEST FLOATING-  COM21020
C     POINT POWER OF 16.                                                COM21030
      EQUIVALENCE (H,K)                                                 COM21040
C                                                                       COM21050
      H=S                                                               COM21060
      K=IAND(MAX0(K,R),2130706432)+1048576                              COM21070
      S16=H                                                             COM21080
      K=(1091567616-K)+1091567616                                       COM21090
      R16=H                                                             COM21100
      RETURN                                                            COM21110
      END                                                               COM21120
