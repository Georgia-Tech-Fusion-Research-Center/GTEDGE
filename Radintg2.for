      SUBROUTINE RADINTG2
C           PERFORMS RADIAL INTEGRALS NEEDED TO EVALUATE N-T INSTAB DENSITY LIMIT
C           INTEGRATES dLZ/dT WEIGHTED BY J0 OVER RADIUS
      DOUBLE PRECISION TEDP,XLZ1DP,DLDT1DP,XLZ2DP,DLDT2DP
      INCLUDE 'Soldiv.fi'
C           PLASMA CURRENT IN AMPS
      PLASMACUR = 1.E6*PLASMACUR

      EIONPED = 17.5
      IF(XNPED.LE.1.E21)
     2      EIONPED = 17.5 + (5.+37.5/TPED)*LOG10(1.E21/XNPED)
      IF(XNPED.GT.1.E21)
     2    EIONPED = (30.6 - 16.4*EXP(-5.E19/XNPED))*
     3              EXP(5.45/(TPED*EXP((XNPED/1.37E20)**0.26)))
      DENOMC = -0.062
C           DENOMC = J1(5.5)/5.5 THE DENOMINATOR OF < > OF EQ.(15)
C     SE = SQRT(0.5*(1.+ELONG**2))
      DELC = 1./56.
C      DELC = (1.0-DELTB/(AMINOR*SE))/56.0
C     NOTE THAT DELC IS ACTUALLY DELC/AMINOR DIMENSIONLESS
      RHO(1) = 0.5*DELC
c     TE(1) = TPED + (T0-TPED)*((1.0 - (RHO(1)**2))**ALPHAT)
c     XNC(1) = XNPED + XNPED*(XNCTRPED-1.)*((1.0 - (RHO(1)**2))**ALPHAN)
      VOL = 2.*(3.14159**2)*RMAJOR*(AMINOR**2)
      HBEAMAV = PBEAM*1.0E6/VOL
      CURDENAV = PLASMACUR/(3.14*(AMINOR**2))
C           C1 CONVERTS ERGS-cm^3 TO JOULES-m^3
      C1 = 1.0E-13
      ETA = (8.85E-4)*ZEFF/(TE(1)**1.5)
      CURDEN(1) = (PLASMACUR/(6.28*(AMINOR**2)))*(TE(1)**1.5)
      POHM(1) = 1.5*ETA*(CURDEN(1)**2)/TE(1)
      HOHM = POHM(1)*RHO(1)*DELC
      C3 = (TE(1)**1.5)*RHO(1)*DELC
      PB(1) = HBEAMAV*EXP(-0.5*((RHO(1)-RHOGAUSS)/SIGMA)**2)
      TEDP = TE(1)
      CALL CEFITS(NZIMP1,TEDP,XLZ1DP,1,DLDT1DP)
      if(nzimp1.eq.4.or.nzimp1.eq.6.or.nzimp1.eq.74) then
        xf0 = 1.e-5
        CALL CXRCEFITS(NzIMP1,TEDP,xF0,XLZ1DP,DLDT1DP,ZAV)
      endif
      CALL CEFITS(NZIMP2,TEDP,XLZ2DP,1,DLDT2DP)
      if(nzimp2.eq.4.or.nzimp2.eq.6.or.nzimp2.eq.74) then
        xf0 = 1.e-5
        CALL CXRCEFITS(NzIMP2,TEDP,xF0,XLZ2DP,DLDT2DP,ZAV)
      endif
      DLZDT(1) = FZ1*DLDT1DP + FZ2*DLDT2DP
c           1.6e-12 ergs/ev
      C8 = 1.6E-6
      DLZDT(1) = DLZDT(1) + FNCOLD(1)*1.5*SVATPED*C8
      XLZC(1) = FZ1*XLZ1DP + FZ2*XLZ2DP
      XLZC(1) = XLZC(1) + FNCOLD(1)*1.5*SVATPED*TE(1)*C8
     2             + FNO(1)*EIONPED*SVIONPED*C8

      RAD = DLZDT(1)*RHO(1)*DELC
      RADN = XNC(1)*XLZC(1)*RHO(1)*DELC
      RADN2 = (XNC(1)**2)*DLZDT(1)*RHO(1)*DELC
      if(pfusion.gt.0.0) then
        TDP = 1.E-3*TEDP
        call DSIGMAV(tdp,sigv,dsigvdt)
        c12 = 1.4E-3
        C13 = 1.E+3*C12
        radF = c12*dsigvdt*rho(1)*delc
        radnF =  c13*xnc(1)*sigmav(tDP)*rho(1)*delc
        radn2F =  c12*(xnc(1)**2)*dsigvdt*rho(1)*delc
      endif

      DHDT(1) = -1.5*PB(1)/(TE(1))
      HEAT = DHDT(1)*RHO(1)*DELC
      BEAMPOW = PB(1)*RHO(1)*DELC
      XNJ0 = XNC(1)*RHO(1)*DELC
      TJ0 = TE(1)*RHO(1)*DELC
      DENOM1 = 0.5*RHO(1)*DELC

      DO 25 I = 2,56
        RHO(I) = RHO(I-1) + DELC
C     TE(I) = TPED + (T0-TPED)*((1.0 - (RHO(I)**2))**ALPHAT)
C     XNC(I) = XNPED + XNPED*(XNCTRPED-1.)*((1.0 - (RHO(I)**2))**ALPHAN)
        ETA = (8.85E-4)*ZEFF/(TE(I)**1.5)
        CURDEN(I) = (PLASMACUR/(6.28*(AMINOR**2)))*(TE(I)**1.5)
        POHM(I) = 1.5*ETA*(CURDEN(I)**2)/TE(I)
        HOHM = HOHM + POHM(I)*RHO(I)*DELC
        C3 = C3 + (TE(I)**1.5)*RHO(I)*DELC
        PB(I) = HBEAMAV*EXP(-0.5*((RHO(I)-RHOGAUSS)/SIGMA)**2)
        TEDP = TE(I)
        CALL CEFITS(NZIMP1,TEDP,XLZ1DP,1,DLDT1DP)
        if(nzimp1.eq.4.or.nzimp1.eq.6.or.nzimp1.eq.74) then
          xF0 = FNO(I)
          if(xf0.lt.1.e-5) xf0 = 1.e-5
          CALL CXRCEFITS(NzIMP1,TEDP,xF0,XLZ1DP,DLDT1DP,ZAV)
        endif
 30     CALL CEFITS(NZIMP2,TEDP,XLZ2DP,1,DLDT2DP)
        if(nzimp2.eq.4.or.nzimp2.eq.6.or.nzimp2.eq.74) then
          xF0 = FNO(I)
          if(xf0.lt.1.e-5) xf0 = 1.e-5
          CALL CXRCEFITS(NzIMP2,TEDP,xF0,XLZ2DP,DLDT2DP,ZAV)
        endif
        DLZDT(I) = FZ1*DLDT1DP + FZ2*DLDT2DP
        DLZDT(I) = DLZDT(I) + FNCOLD(I)*1.5*SVATPED*C8
        XLZC(I) = FZ1*XLZ1DP + FZ2*XLZ2DP
        XLZC(I) = XLZC(I) + FNCOLD(I)*1.5*SVATPED*TE(I)*C8 +
     2                    FNO(I)*EIONPED*SVIONPED*C8
        X = RHO(I)
        CALL BESJ0(X,BES)

        RAD = RAD + DLZDT(I)*BES*RHO(I)*DELC
        RADN = RADN + XNC(I)*XLZC(I)*BES*RHO(I)*DELC
        RADN2 = RADN2 + (XNC(I)**2)*DLZDT(I)*BES*RHO(I)*DELC
        if(pfusion.gt.0.0) then
          TDP = 1.E-3*TEDP
          call DSIGMAV(tdp,sigv,dsigvdt)
          radF = radF + c12*dsigvdt*bes*rho(i)*delc
          radnF = radnF + c13*xnc(i)*sigmav(tDP)*BES*rho(i)*delc
          radn2F = radn2F + c12*(xnc(i)**2)*dsigvdt*BES*rho(i)*delc
        endif

        DHDT(I) = -1.5*PB(I)/(TE(I))
        HEAT = HEAT + DHDT(I)*BES*RHO(I)*DELC
        BEAMPOW = BEAMPOW + PB(I)*RHO(I)*DELC
        XNJ0 = XNJ0 + XNC(I)*BES*RHO(I)*DELC
        TJ0    = TJ0 + TE(I)*BES*RHO(I)*DELC
        DENOM1 = DENOM1 + BES*RHO(I)*DELC

 25   CONTINUE
      RHO(57) = RHO(56) + 0.5*(DELC+DELTB/AMINOR)
C     NOTE THAT DELC IS ACTUALLY DELC/AMINOR
      TEDP = TE(57)
      ETA = (8.85E-4)*ZEFF/(TE(57)**1.5)
      CURDEN(57) = (PLASMACUR/(6.28*(AMINOR**2)))*(TE(57)**1.5)
      POHM(57) = 1.5*ETA*(CURDEN(57)**2)/TE(57)
      HOHM = HOHM + POHM(57)*RHO(1)*DELC

      C3 = C3 + (TE(57)**1.5)*RHO(57)*DELC
      PB(57) = HBEAMAV*EXP(-0.5*((RHO(57)-RHOGAUSS)/SIGMA)**2)
      CALL CEFITS(NZIMP1,TEDP,XLZ1DP,1,DLDT1DP)
      if(nzimp1.eq.4.or.nzimp1.eq.6.or.nzimp1.eq.74) then
        xF0 = FNO(57)
        if(xf0.lt.1.e-5) xf0 = 1.e-5
        CALL CXRCEFITS(NzIMP1,TEDP,xF0,XLZ1DP,DLDT1DP,ZAV)
      endif
      CALL CEFITS(NZIMP2,TEDP,XLZ2DP,1,DLDT2DP)
      if(nzimp2.eq.4.or.nzimp2.eq.6.or.nzimp2.eq.74) then
        xF0 = FNO(57)
        if(xf0.lt.1.e-5) xf0 = 1.e-5
        CALL CXRCEFITS(NzIMP2,TEDP,xF0,XLZ2DP,DLDT2DP,ZAV)
      endif

      DLZDT(57) = FZ1*DLDT1DP + FZ2*DLDT2DP
      DLZDT(57) = DLZDT(57) + FNCOLD(57)*1.5*SVATBAR*C8
      XLZC(57) = FZ1*XLZ1DP + FZ2*XLZ2DP
      XLZC(57)=XLZC(57)+FNCOLD(57)*1.5*SVATBAR*TE(57)*C8 +
     2                  FNO(57)*EIONPED*SVIONBAR*C8
      X = RHO(57)
      CALL BESJ0(X,BES)

C     GOTO 35
      RAD = RAD + DLZDT(57)*BES*RHO(57)*DELTB
      RADN = RADN + XNC(57)*XLZC(57)*BES*RHO(57)*DELTB
      RADN2 = RADN2+(XNC(57)**2)*DLZDT(57)*BES*RHO(57)*DELTB
      if(pfusion.gt.0.0) then
        TDP = 1.E-3*TEDP
        call DSIGMAV(tdp,sigv,dsigvdt)
        radF = radF + c12*dsigvdt*bes*rho(57)*deltb
        radnF = radnF + c13*xnc(57)*sigmav(tDP)*BES*rho(57)*delc
        radn2F = radn2F + c12*(xnc(57)**2)*dsigvdt*BES*rho(57)*delc
      endif

 35   DENOM1 = DENOM1 + BES*RHO(57)*DELTB

      RAD = C1*RAD/DENOMC
      RADN = C1*RADN/DENOMC
      RADN2 = C1*RADN2/DENOMC
      RADF = C1*RADF/DENOMC
      RADNF = C1*RADNF/DENOMC
      RADN2F = C1*RADN2F/DENOMC

      PB(57) = HBEAMAV*EXP(-0.5*((1.0-RHOGAUSS)/SIGMA)**2)
      DHDT(57) = -1.5*PB(57)/(TE(57))
      HEAT = HEAT + DHDT(57)*BES*RHO(57)*DELTB
      BEAMPOW = BEAMPOW + PB(57)*RHO(57)*DELTB
      BEAMPOW = BEAMPOW*((6.2832*AMINOR)**2)*RMAJOR
      XNJ0 = XNJ0 + XNC(57)*BES*RHO(57)*DELTB
      TJ0 = TJ0 + TE(57)*BES*RHO(57)*DELTB
      RAT = 1.0
      IF(PBEAM.GT.0.0) RAT = PBEAM*1.0E6/BEAMPOW
      HEAT = HEAT*RAT/DENOMC
      HOHM = HOHM/(DENOMC*(C3**2))
      XNJ0 = XNJ0/DENOMC
      TJ0 = TJ0/DENOMC

C           DIVIDE BY BOLTZMAN CONSTANT TO CONVERT TO UNITS m^3/s
      RAD = RAD/1.6E-19
      RADN = RADN/1.6E-19
      RADN2 = RADN2/1.6E-19
      RADF = RADF/1.6E-19
      RADNF = RADNF/1.6E-19
      RADN2F = RADN2F/1.6E-19
      RAD = RAD - RADF
      RADN = RADN - RADNF
      RADN2 = RADN2 - RADN2F
      HEAT = HEAT/1.6E-19
C           COMBINE OHMIC & BEAM HEATING INTO HEATING TERM
      HEAT = HEAT + HOHM
      DO 50 I = 1,57
        POHM(I) = POHM(I)/(C3**2)
        PB(I) = PB(I)*RAT
        DHDT(I) = DHDT(I)*RAT
        DLZDT(I) = C1*DLZDT(I)
        XLZC(I) = C1*XLZC(I)
 50   CONTINUE
C           PLASMACUR IN MA
      PLASMACUR = 1.E-6*PLASMACUR
C           IONIZATION SOURCE
      CALL SVION(1,DSDN,DSDT,FLUXIN)
      SN = DSDN*RHO(1)*DELC
      SORN = SN*XNC(1)
      ST = DSDT*RHO(1)*DELC
      DO 80 I=2,56
        CALL SVION(I,DSDN,DSDT,FLUXIN)
        X = RHO(I)
        CALL BESJ0(X,BES)
        SN = SN + DSDN*BES*RHO(I)*DELC
        SORN = SORN + DSDN*XNC(I)*RHO(I)*DELC
        ST = ST + DSDT*BES*RHO(I)*DELC
 80   CONTINUE
      CALL SVION(57,DSDN,DSDT,FLUXIN)
      X = RHO(57)
      CALL BESJ0(X,BES)
      SN = SN + DSDN*BES*RHO(57)*DELC
      SORN = SORN + DSDN*XNC(I)*RHO(57)*DELC
      ST = ST + DSDT*BES*RHO(57)*DELC
      SN = SN/DENOMC
      ST = ST/DENOMC
      SORN = 6.2832*SORN
      RETURN
      END

C           TE DISTRIBUTED AS QUADRATIC TO POWER ALPHAT PLUS PEDESTAL
C           PBEAM DISTRIBUTED AS GAUSSIAN ABOUT RHOGAUSS
C           RAD IN UNITS m^3/s
C           HEAT IN UNITS m^3/s
C           DLZDT & DHDT IN UNITS OF W-m^3/eV


