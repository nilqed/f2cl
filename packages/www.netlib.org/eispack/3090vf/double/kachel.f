C-----------------------------------------------------------------------COM21130
C ESTIMATED NUMBER OF COLUMNS OF A LONG-PRECISION MATRIX FITTING IN THE COM21140
C CACHE.                                                                COM21150
C                                                                       COM21160
      INTEGER FUNCTION KACHEL (LD)                                      COM21170
C                                                                       COM21180
      INTEGER     LD                                                    COM21190
C                                                                       COM21200
C LD     E  FIRST DIMENSION OF THE MATRIX ARRAY                         COM21210
C                                                                       COM21220
C     EISPACK VERSION FOR THE IBM 3090VF DATED NOVEMBER 1987.           COM21230
C-----------------------------------------------------------------------COM21240
      KACHEL=5184/MAX0(LD,1)                                            COM21250
      RETURN                                                            COM21260
      END                                                               COM21270
