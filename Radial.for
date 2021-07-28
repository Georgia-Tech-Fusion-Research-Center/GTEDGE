      SUBROUTINE RADIAL
      INCLUDE 'Soldiv.fi'
C           CALCULATES RADIAL LOSS TERMS FOR SOLDIV PLASMA
C           RADIAL LOSS RATES USING BOHM DIFFUSION

      IF(CHIRSOL.EQ.0.0) GOTO 50
C     PARTICLE
      TAUPART = (DELN**2)/CHIRSOL
      DNRAD = (DELN/TAUPART)*(XLPERP*XNSOL +
     2        (XLPAR - XLPERP - DELLT)*XNDIV/EPDIV + DELLT*XND/EPDIV)
C     ENERGY
      TAUENERGY = (DELEA**2)/CHIRSOL
      DQPERP = (5.*DELEA/TAUPART)*XK*(XNSOL*TSOL*XLPERP +
     2            (XLPAR - XLPERP - DELLT)*XNDIV*TDIV/EPDIV +
     3                 DELLT*XND*TD/EPDIV)
      TRADLOSS = (XNSOL*TSOL*XLPERP +
     2            (XLPAR - XLPERP - DELLT)*XNDIV*TDIV/EPDIV +
     3                 DELLT*XND*TD/EPDIV)/(XNSOL*XLPERP +
     2            (XLPAR - XLPERP - DELLT)*XNDIV/EPDIV      +
     3                 DELLT*XND/EPDIV)
      DQPERP = 3.*XK*TRADLOSS*DNRAD
      GOTO 100
 50   DQPERP = FOUT*((YIONSOL+YIONSOLXPT)*TSEP+YIONDIV*TDIV)*XK
      DNRAD = 0.
 100  RETURN
      END
