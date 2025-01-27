      SUBROUTINE CORAD
      DOUBLE PRECISION TEDP,XLZ1DP,DLDT1DP,XLZ2DP,DLDT2DP
      INCLUDE 'Soldiv.fi'

C           CALCULATES CORE RADIATION & OHMIC AND FUSION HEATING
      PFUSIONOLD = PFUSION
      PRADOLD = PRAD
      FZ1 = CFZINT
      FZ2 = CFZINJ
      NZIMP1 = IZINTRIN
      NZIMP2 = IZINJECT
      ZEFFC=(1.+(IZINJECT**2)*FZ2+16.*FBE+4.*FHE +(IZINTRIN**2)*FZ1)/
     2      (1.+ IZINJECT*FZ2 + 4.*FBE + 2.*FHE + IZINTRIN*FZ1)
      ZEFFCM=(1.+16.*FBE+4.*FHE)/
     2           (1.+IZINJECT*FZ2+4.*FBE+2.*FHE+IZINTRIN*FZ1)
      RATNENI2 =  (1.+ IZINJECT*FZ2 + 4.*FBE + 2.*FHE + IZINTRIN*FZ1)**2
C     SE = SQRT(0.5*(1.+ELONG**2))
C     DELC = (1.0-DELTB/(AMINOR*SE))/56.0
      DELC = 1./56.

      RHO(1) = 0.5*DELC

      TT = 1.E-3*TE(1)
      SVF = SIGMAV(TT)
      IF(PFUSION.LE.0.0) SVF = 0.0
      PFUS(1) = (7.04E-19)*(XNC(1)**2)*SVF/RATNENI2
      PFUSION = PFUS(1)*0.5*(RHO(1)**2)

      ETA = (2.8E-8)*ZEFFC/(TE(1)**1.5)
      CURDEN(1)=(1.E6*PLASMACUR/(3.14*(AMINOR**2)*ELONG))
      POHM(1) = ETA*(CURDEN(1)**2)
      POHMH = POHM(1)*0.5*(RHO(1)**2)
      C3 = (TE(1)**1.5)*0.5*(RHO(1)**2)
      C1 = 4.8E-37*RADMULT
      RADBREM =C1*(ZEFFCM)*(XNC(1)**2)*SQRT(1E-3*TE(1))*RATNENI2
      TEDP = TE(1)
c*************temp avoid fo affect on radiation*******
      goto 4
      if(nzimp1.eq.4.or.nzimp1.eq.6.or.nzimp1.eq.74) then
        xF0 = FNO(1)
        if(xf0.lt.1.e-5) xf0 = 1.e-5

        CALL CXRCEFITS(NzIMP1,TEDP,xF0,XLZ1DP,DLDT1DP,ZAV)
        goto 5
      endif
 4    CALL CEFITS(NZIMP1,TEDP,XLZ1DP,0,DLDT1DP)
c*************temp avoid fo affect on radiation*******
      goto 6
 5    if(nzimp2.eq.4.or.nzimp2.eq.6.or.nzimp2.eq.74) then
        xF0 = FNO(1)
        if(xf0.lt.1.e-5) xf0 = 1.e-5
        CALL CXRCEFITS(NzIMP2,TEDP,xF0,XLZ2DP,DLDT2DP,ZAV)
        goto 7
      endif
 6    CALL CEFITS(NZIMP2,TEDP,XLZ2DP,0,DLDT2DP)

C           CONVERT ERG-cm3/s TO J-m3/s
 7    C2 = 1.E-13
      RADIMP = (XNC(1)**2)*(FZ1*XLZ1DP + FZ2*XLZ2DP)*QZMULTCORE*C2
      PRAD =(RADIMP+RADBREM)*0.5*(RHO(1)**2)
      PBREM =(RADBREM)*0.5*(RHO(1)**2)
      PIMP =(RADIMP)*0.5*(RHO(1)**2)
      DO 60 I = 2,55
        RHO(I) = RHO(I-1) + DELC

        TT =  1.E-3*TE(I)
        SVF = SIGMAV(TT)
        IF(PFUSIONin.LE.0.0) SVF = 0.0
        PFUS(I) = (7.04E-19)*(XNC(I)**2)*SVF/RATNENI2
        PFUSION = PFUSION + PFUS(I)*0.5*((RHO(I)**2) - (RHO(I-1)**2))

        C3 = C3 + (TE(I)**1.5)*0.5*((RHO(I)**2) - (RHO(I-1)**2))
        ETA = (2.8E-8)*ZEFFC/(TE(I)**1.5)
        CURDEN(I)=(1.E6*PLASMACUR/(3.14*(AMINOR**2)*ELONG))
        POHM(I) = ETA*(CURDEN(I)**2)
        POHMH = POHMH + POHM(I)*0.5*((RHO(I)**2) - (RHO(I-1)**2))
        RADBREM =C1*(ZEFFCM)*(XNC(I)**2)*SQRT(1E-3*TE(I))*RATNENI2
        TEDP = TE(I)
        if(nzimp1.eq.4.or.nzimp1.eq.6.or.nzimp1.eq.74) then
          xF0 = FNO(I)
          if(xf0.lt.1.e-5) xf0 = 1.e-5
c     stick here tedp,xf0 for test ****************************


          CALL CXRCEFITS(NzIMP1,TEDP,xF0,XLZ1DP,DLDT1DP,ZAV)
          goto 10
        endif
        CALL CEFITS(NZIMP1,TEDP,XLZ1DP,0,DLDT1DP)

 10     if(nzimp2.eq.4.or.nzimp2.eq.6.or.nzimp2.eq.74) then
          xF0 = FNO(I)
          if(xf0.lt.1.e-5) xf0 = 1.e-5

          CALL CXRCEFITS(NzIMP2,TEDP,xF0,XLZ2DP,DLDT2DP,ZAV)
          goto 20
        endif
        CALL CEFITS(NZIMP2,TEDP,XLZ2DP,0,DLDT2DP)

 20     if(i.lt.55) goto 55
        ri = i

 55     continue
        RADIMP = (XNC(I)**2)*(FZ1*XLZ1DP + FZ2*XLZ2DP)*QZMULTCORE*C2
        PRAD=PRAD+(RADIMP+RADBREM)*0.5*((RHO(I)**2)-(RHO(I-1)**2))
        PBREM = PBREM + (RADBREM)*0.5*((RHO(I)**2) - (RHO(I-1)**2))
        PIMP = PIMP + (RADIMP)*0.5*((RHO(I)**2) - (RHO(I-1)**2))
 60   CONTINUE
      ETA = (2.8E-8)*ZEFFC/(TE(56)**1.5)
      CURDEN(56)=(1.E6*PLASMACUR/(3.14*(AMINOR**2)*ELONG))
      I  = 56
      TT = 1.E-3*TE(56)
      SVF = SIGMAV(TT)
      IF(PFUSION.LE.0.0) SVF = 0.0
      PFUS(I) = (7.04E-19)*(XNC(I)**2)*SVF/RATNENI2
      PFUSION = PFUSION + PFUS(I)*0.5*(1.0 - (RHO(55)**2))

      POHM(56) = ETA*(CURDEN(56)**2)
      C3 = C3 + (TE(I)**1.5)*0.5*(1.0 - (RHO(55)**2))
      POHMH = POHMH + POHM(56)*0.5*(1.0 - (RHO(55)**2))
      RADBREM =C1*(ZEFFCM)*(XNC(56)**2)*SQRT(1E-3*TE(56))*RATNENI2
      TEDP = TE(56)

      if(nzimp1.eq.4.or.nzimp1.eq.6.or.nzimp1.eq.74) then
        xF0 = FNO(56)
        if(xf0.lt.1.e-5) xf0 = 1.e-5
        CALL CXRCEFITS(NzIMP1,TEDP,xF0,XLZ1DP,DLDT1DP,ZAV)
      endif
      goto 70
      CALL CEFITS(NZIMP1,TEDP,XLZ1DP,0,DLDT1DP)


 70   if(nzimp2.eq.4.or.nzimp2.eq.6.or.nzimp2.eq.74) then
        xF0 = FNO(56)
        if(xf0.lt.1.e-5) xf0 = 1.e-5
        CALL CXRCEFITS(NzIMP2,TEDP,xF0,XLZ2DP,DLDT2DP,ZAV)
        goto 80
      endif
      CALL CEFITS(NZIMP2,TEDP,XLZ2DP,0,DLDT2DP)

 80   RADIMP = (XNC(56)**2)*(FZ1*XLZ1DP+ FZ2*XLZ2DP)*QZMULTCORE *C2
      PRAD =PRAD + (RADIMP+RADBREM)*0.5*(1.0 - (RHO(55)**2))
      PBREM =PBREM + (RADBREM)*0.5*(1.0 - (RHO(55)**2))
      PIMP =PIMP + (RADIMP)*0.5*(1.0 - (RHO(55)**2))
C           SYCHROTRON RADIATION--GLOBAL  FROM UCKAN ET AL ITER10 CDA IAEA
      T10 = TAV/1.E4
      YN20 = XNAV/1.E20
      GSYN = 0.16*(T10**1.5)*SQRT(1. + 5.7*AMINOR/(RMAJOR*SQRT(T10)))
      XLAM12 = 77.7*SQRT(YN20*AMINOR/B)
      PHISYN = GSYN*SQRT(1.-RSYN)/XLAM12
      CSYN = 6.2E-2
      PSYN = 1.e6*CSYN*YN20*T10*(B**2)*PHISYN*RADMULT
C           CONVERT TO TOTAL VOLUMETRIC POWER
      PSYN = 39.44*ELONG*RMAJOR*(AMINOR**2)*PSYN
      PRAD = 39.44*ELONG*RMAJOR*(AMINOR**2)*PRAD + PSYN
      PBREM = 39.44*ELONG*RMAJOR*(AMINOR**2)*PBREM
      PIMP = 39.44*ELONG*RMAJOR*(AMINOR**2)*PIMP
      POHMH = 39.44*ELONG*RMAJOR*(AMINOR**2)*POHMH
      PFUSION = 39.44*ELONG*RMAJOR*(AMINOR**2)*PFUSION
      IF(ITERN.GT.10) GOTO 100
      PFUSION = PFUSIONin
c     pfusion = 0.5*(pfusion+pfusionold)
c     IF(ITERN.LT.2) PFUSION = PFUSIONIN
C           PARTICLE FLUX CONSISTENT WITH SOURCE TO PLASMA CORE
 100  FLUXNEUTIN=CURSEP*(1.-ALPHASEP)
      FLUXIONIN = YIONSOL*FIN*2.*6.28*RMAJOR/AP
      FLUXSEPIN = FLUXNEUTIN + FLUXIONIN
      FLUXNEUTINXPT = CURSEPXPT*(1.-ALPHAX)
      IF(DELXPT.GT.0.0) FLUXIONINXPT = YIONXPT*FINXPT/(0.5*DELXPT)
      FLUXSEPINXPT = FLUXNEUTINXPT + FLUXIONINXPT
c     INWARD POWER FLUX INTO CORE FROM SOL
      PINSEP = 1.5*XK*(TSOL*(FLUXNEUTIN + 2.*FLUXIONIN)*AP    +
     2 TXPT*(FLUXNEUTINXPT + 2.*FLUXIONINXPT)*DELXPT*6.2832*RMAJOR)

C      ********************************************
      XRAD = (PRAD+PRADOLD)/(2.*PRAD)
      IF(ITERN.LT.10) GOTO 145
      PRAD = XRAD*PRAD
      PBREM = XRAD*PBREM
      PSYN = XRAD*PSYN
      PIMP = XRAD*PIMP
      IF(PFUSION.GT.0.0) XFUS = (PFUSION+PFUSIONOLD)/(2.*PFUSION)
      PFUSION = XFUS*PFUSION
C      ********************************************
C           CALCULATES CORE HEAT LOSS FRACTIONS
 145  IF(IOPTOHM.EQ.1) POHMH = POHMIN
      HEATIN =  ((PFUSION/5.+PBEAM)*1.E6+POHMH +
     2            3.*XK*SPELLET*TPEL + PINSEP)
 150  IF(IPFLUX.EQ.2) PRAD = FRACRAD*HEATIN - PIONCORE
      FRACRAD = (PRAD+PIONCORE)/HEATIN
      FRACSOL = 1.- FRACRAD
      FRACBREM=PBREM/HEATIN

      FRACSYN=PSYN/HEATIN
      FRACIMP = PIMP/HEATIN
      IF(FRACRAD.LE.0.95) GOTO 200
      PRAD = (0.95/FRACRAD)*PRAD
      PSYN = (0.95/FRACRAD)*PSYN
      PBREM = (0.95/FRACRAD)*PBREM
      PIMP = (0.95/FRACRAD)*PIMP
      PIONCORE = (0.95/FRACRAD)*PIONCORE
      GOTO 150
 200  RETURN
      END



