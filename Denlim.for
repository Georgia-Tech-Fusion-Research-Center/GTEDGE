      SUBROUTINE DENLIM
      INCLUDE 'Soldiv.fi'
C           EVALUATES DENSITY LIMIT FOR MARFE ONSET
C (THIS PART OF CODE WORKS WITH CGS UNITS--CONVERSIONS)

C           TRANSPORT BARRIER ATOMIC DATA & RADIATION FUNCTION
      DOUBLE PRECISION TDBL, XLZDBL, DLZDBL
      real  Lzcarb,xkri
      if(jjoptped.eq.10) goto 5
      if(jjoptped.eq.9)  goto 5
      fion = 1.0
 5    continue
      DHEATDT = 0.
      HEATB = 0.
C     *********TEMPORARY BEGIN***************
C     GRADNBAR(4)=10.0
C     GRADTBAR(4)= GRADNBAR(4)
C     GRADTEBAR(4)= GRADNBAR(4)
C     CHIRTB = 0.1
C     CFZINTTB = 0.1
C     CFZINJTB = 0.0
C     FOBAR = 1.E-3
C     XNOBAR = FOBAR*XNBAR
C     XNOBARXPT = FOBAR*XNBAR
C     XNOCOLDBAR = 0.1*XNOBAR
C     XNOCOLDBARXPT = 0.1*XNOBARXPT
C     *********TEMPORARY END*****************
      JOPTTB = 0
      DO 1000 I = 1,14
        IF(I.LE.10) GOTO 15
C     EVALUATION BASED ON CALCULATED N, T & GRADS
        IF(I.EQ.13) GOTO 10
        XNM = XNBAR
        TM= TBAR
        XNOM = XNOBAR
        XNOMXPT = XNOBARXPT
        XNOCOLDM = XNOCOLDBAR
        XNOCOLDMXPT = XNOCOLDBARXPT
        SVELM = SVELBAR
        SVCXM = SVCXBAR
        SVIONM = SVIONBAR
        EIONM = EIONBAR
        GOTO 25

 10     XNM = XNPED
        TM= TPED
        XNOM = XNOPED
        XNOMXPT = XNOPEDXPT
        XNOCOLDM = XNOPEDCOLD
        XNOCOLDMXPT = XNOPEDCOLDXPT
        SVELM = SVELPED
        SVCXM = SVCXPED
        SVIONM = SVIONPED
        EIONM = EIONPED
        GOTO 25

C     EVALUATION BASED ON EXPERIMENTAL N,T AND & GRADS
 15     IF(I.LT.4) GOTO 20
        IF(I.GE.6.AND.I.LE.8) GOTO 20
C     TRANSPORT BARRIER
        XNM = XNTBEX
        TM = TTBEX
        XNOM = XNOBAR
        XNOMXPT = XNOBARXPT
        XNOCOLDM = XNOCOLDBAR
        XNOCOLDMXPT = XNOCOLDBARXPT
        GOTO 22
C     PEDESTAL
 20     XNM = XNPEDEX
        TM = 0.5*(TPEDEXE + TPEDEXI)
        XNOM = XNOPED
        XNOMXPT = XNOPEDXPT
        XNOCOLDM = XNOPEDCOLD
        XNOCOLDMXPT = XNOPEDCOLDXPT


 22     TZZ = TM
        IF(TM.LT.1.E-1) TZZ = 1.05E-1
        IF(TM.GT.1E3) TZZ = .95E3
        XNZZ = XNM
        IF(XNM.GT.1E22) XNZZ = 0.95E22
        IF(XNM.LT.1E16) XNZZ = 1.1E16
        TNZZ = TM
        IF(TNZZ.GE.100) TNZZ    = 95.
        CALL INTERP(TZZ,TZZ,TNZZ,XNZZ)
        SVELM = SEL(1)
        SVELMN= SELN(1)
        SVCXMBAR = SCX(1)
        SVATM = SEL(1) + SCX(1)
        SVIONM = SION(1)
        EIONM = 17.5
        IF(XNBAR.LE.1.E21)
     2      EIONM = 17.5 + (5.+37.5/TM)*LOG10(1.E21/XNM)
        IF(XNBAR.GT.1.E21)
     2      EIONM = (30.6 - 16.4*EXP(-5.E19/XNM))*
     3      EXP(5.45/(TM*EXP((XNM/1.37E20)**0.26)))

C           ATOMIC DATA
 25     SIGVEL = 1.E6*SVELM
        SIGVCX = 1.E6*SVCXM
        SIGVION = 1.E6*SVIONM
        x = EIONM
        XNUAT = 1.E-6*(SIGVEL + SIGVCX)*XNOCOLDM
        DNUATDT = 0.0
        XNUION = 1.E-6*SIGVION*XNOM
        DIONDT = 0.0
C           RADIATION
        QZMULT = 1.0
        IF((CFZINT+CFZINJ).GT.0.0) GOTO 50
        CFZINT = 1.E-4
        CFZINJ = 1.E-4
 50     CONTINUE
        TDBL = TM
        IZ1 = IZINJECT
        IZ2 = IZINTRIN
        if(iz1.eq.4.or.iz1.eq.6.or.iz1.eq.74) then
          fon = 0.5*(fno(57)+fnoxpt(57))
          if(fon.lt.1.e-5) fon = 1.e-5
          CALL CXRCEFITS(iz1,Tdbl,fon,xlzdbl,dlzdbl,ZAV)
          goto 55
        endif

        CALL cefits (IZ1, TDBL, XLZDBL, 1, DLZDBL)
 55     XLZ1 = XLZDBL
        XLZ = XLZDBL*CFZINJ
        DLZZDT1 = DLZDBL
        DLZZDT = DLZDBL*CFZINJ
        if(iz1.eq.4.or.iz1.eq.6.or.iz1.eq.74) then
          fon = 0.5*(fno(57)+fnoxpt(57))
          if(fon.lt.1.e-5) fon = 1.e-5
          CALL CXRCEFITS(iz1,Tdbl,fon,xlzdbl,dlzdbl,ZAV)
          goto 65
        endif

        CALL CEFITS (IZ2, TDBL, XLZDBL, 1, DLZDBL)
 65     XLZ2 = XLZDBL
        XLZ = XLZ + XLZDBL*CFZINT
        DLZZDT2 = DLZDBL
        DLZZDT = DLZZDT + DLZDBL*CFZINT

C           CONVERT FROM ERG-CM3/EV-S TO CM3/S
        XLZ = XLZ/1.6E-12
        DLZZDT = DLZZDT/1.6E-12
        XLZ1 = XLZ1/1.6E-12
        XLZ2 = XLZ2/1.6E-12
        DLZZDT1 = DLZZDT1/1.6E-12
        DLZZDT2 = DLZZDT2/1.6E-12
C           FRICTION FACTOR
        F0 = 0.5*(fno(57)+fnoxpt(57))
        TAURES = 1.E-3
        if(iz1.eq.4.or.iz1.eq.6.or.iz1.eq.74) then
          fon = 0.5*(fno(57)+fnoxpt(57))
          if(fon.lt.1.e-5) fon = 1.e-5
          CALL CXRCEFITS(izinject,Tdbl,fon,xlzdbl,dlzdbl,ZAV)
          goto 75
        endif
        CALL NCEFITS(IZINJECT,TM,TAURES,XLZF,ZAV)

 75     Z0 = CFZINJ*(ZAV**2) + CFZINT*(ZAVTRIN**2)
        ZEFFC1 = (Z0+ 1.)/(CFZINJ*ZAV +
     2      CFZINT*ZAVTRIN + 1.)
        C2E = 1.5*(1. - 0.6934/((1.3167)**ZEFFC1))/zeffc1
        ZIMP = (CFZINJ*ZAV+CFZINT*IZINTRIN)/
     2      (CFZINT+CFZINJ)
        X  = 1./(Z0 + SQRT(0.5*(1. + 1./ZIMP)))
        C2I= 1.56*(1.+1.414*Z0)*(1.+0.52*Z0)*X/((1.+2.65*Z0)*(1.+.285*Z0))
        ALPHA = Z0
        C2 = C2E*(1. + ALPHA)

c     ************************************************

C***********************************************************************************
C           MARFE DENSITY LIMIT & GROWTH RATE OF RADIAL MODES

C     OPTION 1    USE EXPERIMENTAL INPUT GRADS,CALCULATE LOCAL CHI TO REMOVE HEAT
C     GRADT = GRADTI
 100    IF(I.GT.5) GOTO 200
        XLT = 1.E-2*GRADTBAR(I)
        XLN = 1.E-2*GRADNBAR(I)
        XLTI = 1.E-2*GRADTBAR(I)
        XLTE = 1.E-2*GRADTEBAR(I)
        XLTM = 1.E-2*(TTBEXI*GRADTBAR(I-5)+TTBEXE*GRADTEBAR(I-5))/
     2      (TTBEXI + TTBEXE)

        IF(GRADTBAR(I).LE.0.0) GOTO 105

        CHIR = 1.E4*fcond*FLUXHEAT/
     2      (XNM*XK*TM*GRADTBAR(I))

 105    GOTO 500

C     OPTION 2 USE EXP INPUT GRADS, CALCULATE LOCAL CHI TO REMOVE HEAT
C     GRADT = (TE*GRADTE + TI*GRADTI)/(TE+TI)
 200    IF(I.GT.10) GOTO 300
        XLT = 1.E-2*(TTBEXI*GRADTBAR(I-5)+TTBEXE*GRADTEBAR(I-5))/
     2      (TTBEXI + TTBEXE)
        XLTM = XLT
        XLN = 1.E-2*GRADNBAR(I-5)
        XLTI = 1.E-2*GRADTBAR(I-5)
        XLTE = 1.E-2*GRADTEBAR(I-5)
        IF(GRADTBAR(I-5).LE.0.0) GOTO 205
        IF(GRADTEBAR(I-5).LE.0.0) GOTO 205
        CHIR = 1.E4*fcond*FLUXHEAT/
     2      (XNM*XK*TM*1.E2*XLT)
 205    CHIREXP(I-5) = 1.E-4*CHIR
        GOTO 500

C     OPTION 3    USE CALCULATED TRANSPORT BARRIER GRADS & INPUT CHI
 300    IF(I.NE.11) GOTO 400
        XLT = 1.E-2*XLTBAR
        XLN = 1.E-2*XLNBAR
        XLTI = XLT/fion
        XLTE = XLT
        XLTM = 0.5*(1.+1./FION)*XLT
        CHIR = 1.E4*CHIREDGE

        GOTO 500

C     OPTION 4 USE CALCULATED TRANSPORT BARRIER GRADS & CALCULATED CHI TO REMOVE HEAT
C           I = 12 USED TRANS BARRIER N & T, I=13 USES PEDESTAL N & T
 400    IF(I.EQ.14) GOTO 450
        IF(XLTBAR.EQ.0.0) XLTBAR = XLNBAR
        XLT = 1.E-2*XLTBAR
        XLN = 1.E-2*XLNBAR
        XLTI = XLT/fion
        XLTE = XLT
        XLTM = 0.5*(1.+1./FION)*XLT
        CHIR = 1.E4*FCOND*FLUXHEAT/(XNM*XK*TM*XLTBAR)
        IF(JJOPTPED.EQ.9) CHIR = 1.E4*CHIREDGE
        GOTO 500
C     OPTION 5 PREDICTIVE: INPUT CHI, CALCULATED GRAD SCALE LENGTHS
 450    XLN = 1.E-2/XLNBAR
        XLTI = 1.E-2/XLTIBAR
        XLTE = 1.E-2/XLTEBAR
        XLT = XLTI
        XLTM = 0.5*(XLTI+XLTE)
        CHIR = (CHITBI*XI + CHITBE*XE)

        CHIRTBI = CHITBI*XI
        CHIRTBE = CHITBE*XE
        CHIRTB = (CHIRTBI + CHIRTBE)

 500    ZMULT = 1.
        ZNEUT = 1.

C     ********MIDPLANE EVALUATION************************
        SIGVAT = SIGVEL + SIGVCX
        FOMP = XNOM/XNM
C     CALL CARBONMULT(TM,FOMP,QZMULTEDGE,DQZMULT)
C     XLZ = CFZINJ*XLZ1 + CFZINT*XLZ2*QZMULTEDGE
C     DLZZDT = CFZINT*DLZZDT1 + CFZINJ*DLZZDT2*DQZMULT
c     CALL CXRCEFITS(IZ2,TM,FOMP,XLZ2,DLZZDT2,ZAV2)
c     XLZ2 = XLZ2/1.6E-12
c     DLZZDT2 = DLZZDT2/1.6E-12
        XLZ = CFZINJTB*XLZ1 + CFZINTTB*XLZ2
        DLZZDT = CFZINJTB*DLZZDT1 + CFZINTTB*DLZZDT2

        FOMPCOLD = XNOCOLDM/XNM
C           MARFE DENSITY LIMIT
        DIMP = ((XNU+1.-C2)*XLZ/TM -DLZZDT)*qzmultcore
        DNEUT= FOMPCOLD*(1.5*SIGVAT*(XNU - 1. - DNUATDT)) +
     2      FOMP*(SIGVION*(EIONM/TM)*(XNU - DIONDT))
        DIMP = ZMULT*DIMP
        DNEUT = ZNEUT*DNEUT
        DENOM = DIMP + DNEUT
        XNUM    = CHIR*(XNU*(XLTM**2)-(1.-C2)*XLTM*XLN)
        DENLIMMP(I) = 1.e6*XNUM/DENOM
        Y = DENLIMMP(I)

c           *************************************************************
C           GROWTH RATE OF RADIAL MODE IN TRANSPORT BARRIER
        XNCGS = 1.E-6*XNM
        XNUAT = FOMPCOLD*XNCGS*SIGVAT
        XNUION = FOMP*XNCGS*SIGVION
        XKR = 3.1416/(1.E2*DELTB)

        FOXPT = XNOMXPT/XNM
        CC1 = CHIRTB*(XKR**2) + CHIR*XNU*(XLT**2)
     4      + (XNU*HEATB/TM-DHEATDT)/XNCGS
     2      - XNCGS*(XNU*XLZ/TM -DLZZDT) - 1.5*XNUAT*(XNU - 1. - DNUATDT)-
     3      XNUION*(EIONM/TM)*(XNU - DIONDT)
        GROW1MP(I) = -1.*CC1/3.
        CC2 = CHIRTB*((2.*XKR)**2)+CHIR*XNU*(XLT**2)
     4      + (XNU*HEATB/TM-DHEATDT)/XNCGS
     2      - XNCGS*(XNU*XLZ/TM -DLZZDT) - 1.5*XNUAT*(XNU - 1. - DNUATDT)-
     3      XNUION*(EIONM/TM)*(XNU - DIONDT)
        B1 = CHIR*(XLN + 2.*XNU*XLT)*(XKR**2)
        B2 = 4.*B1
        A21 =  8./(3.*3.1416)
        A12 =  -4./(3.*3.1416)
        GROW2MP(I) = -1.*(CC1 + CC2)/6.
        GROW3MP(I) = GROW2MP(I)
        RXX = 1. - 4.*(CC1*CC2-B1*A12*B2*A21)/((CC1+CC2)**2)
        IF(RXX.LT.0.0) GOTO 550
        GROW2MP(I) = GROW2MP(I)*(1. + SQRT(RXX))
        GROW3MP(I) = GROW3MP(I)*(1. - SQRT(RXX))
 550    CONTINUE
C     *******X-POINT EVALUATION*************************

C     CALL CARBONMULT(TM,FOXPT,QZMULTEDGE,DQZMULT)
C     XLZ = CFZINJ*XLZ1 + CFZINT*XLZ2*QZMULTEDGE
C     DLZZDT = CFZINT*DLZZDT1 + CFZINJ*DLZZDT2*DQZMULT
c     CALL CXRCEFITS(IZ2,TM,FOXPT,XLZ2,DLZZDT2,ZAV2)
c     XLZ2 = XLZ2/1.6E-12
c     DLZZDT2 = DLZZDT2/1.6E-12
        XLZ = CFZINJTB*XLZ1 + CFZINTTB*XLZ2
        DLZZDT = CFZINJTB*DLZZDT1 + CFZINTTB*DLZZDT2
        FOXPTCOLD = XNOCOLDMXPT/XNM
        DIMP = ((XNU+1.-C2)*XLZ/TM -DLZZDT)*qzmultcore
        DNEUT =  FOXPTCOLD*(1.5*SIGVAT*(XNU - 1. - DNUATDT)) +
     2      FOXPT*(SIGVION*(EIONM/TM)*(XNU - DIONDT))
        DIMP = ZMULT*DIMP
        DNEUT = ZNEUT*DNEUT
        DENOM = DIMP + DNEUT
        XNUM    = CHIR*(XNU*(XLTM**2)-(1.-C2)*XLTM*XLN)
        DENLIMXPT(I) = 1.e6*XNUM/DENOM
        X = DENLIMXPT(I)

C********************************************************************

C           GROWTH RATE OF RADIAL MODE
        XKR = 3.1416/(1.E2*DELTB)
        XNUAT = FOXPTCOLD*XNCGS*SIGVAT
        XNUION = FOXPT*XNCGS*SIGVION
        CC1 = CHIRTB*(XKR**2) + CHIR*XNU*(XLT**2)
     4      + (XNU*HEATB/TM-DHEATDT)/XNCGS
     2      - XNCGS*(XNU*XLZ/TM -DLZZDT) - 1.5*XNUAT*(XNU - 1. - DNUATDT)-
     3      XNUION*(EIONM/TM)*(XNU - DIONDT)
        GROW1XP(I) = -1.*CC1/3.
        CC2 = CHIRTB*(2.*XKR)**2 + CHIR*XNU*(XLT**2)
     4      +(XNU*HEATB/TM-DHEATDT)/XNCGS
     2      - XNCGS*(XNU*XLZ/TM -DLZZDT) - 1.5*XNUAT*(XNU - 1. - DNUATDT)-
     3      XNUION*(EIONM/TM)*(XNU - DIONDT)
        B1 = CHIRTB*(XLN + 2.*XNU*XLT)*(XKR)
        B2 = 2.*B1
        A21 =  8./(3.*3.1416)
        A12 =  -4./(3.*3.1416)
        GROW2XP(I) = -1.*(CC1 + CC2)/6.
        GROW3XP(I) = GROW2XP(I)
        RXX = 1. - 4.*(CC1*CC2-B1*A12*B2*A21)/((CC1+CC2)**2)
        IF(RXX.LT.0.0) GOTO 650
        GROW2XP(I) = GROW2XP(I)*(1. + SQRT(RXX))
        GROW3XP(I) = GROW3XP(I)*(1. - SQRT(RXX))
 650    CONTINUE
C           GROWTH RATE OF RADIAL MODE***AVERAGE NEUTRAL DENSITY
        FOAV = 0.5*(XNOMXPT+XNOM)/XNM
        CALL CXRCEFITS(IZ2,Tdbl,FOAV,XLZdbl,DLZdbl,ZAV2)
        XLZ2 = XLZdbl/1.6E-12
        DLZZDT2 = DLZdbl/1.6E-12
        XLZ = CFZINJTB*XLZ1 + CFZINTTB*XLZ2
        DLZZDT = CFZINJTB*DLZZDT1 + CFZINTTB*DLZZDT2

        XKR = 3.1416/(1.E2*DELTB)
        XNUAT = 0.5*(FOXPTCOLD+FOMPCOLD)*XNCGS*SIGVAT
        XNUION = 0.5*(FOXPT+FOMP)*XNCGS*SIGVION
C     THERMAL INSTABILITY W/STRONG EQUILIBRATION
        CC1 = CHIRTB*(XKR**2) + CHIR*XNU*(XLT**2)
     4      + (XNU*HEATB/TM-DHEATDT)/XNCGS
     2      - XNCGS*(XNU*XLZ/TM -DLZZDT) - 1.5*XNUAT*(XNU - 1. - DNUATDT)-
     3      XNUION*(EIONM/TM)*(XNU - DIONDT)
        GROW1AV(I) = -1.*CC1/3.
        CC2 = CHIRTB*(2.*XKR)**2 + CHIR*XNU*(XLT**2)
     4      +(XNU*HEATB/TM-DHEATDT)/XNCGS
     2      - XNCGS*(XNU*XLZ/TM -DLZZDT) - 1.5*XNUAT*(XNU - 1. - DNUATDT)-
     3      XNUION*(EIONM/TM)*(XNU - DIONDT)
        B1 = CHIRTB*(XLN + 2.*XNU*XLT)*(XKR)
        B2 = 2.*B1
        A21 =  8./(3.*3.1416)
        A12 =  -4./(3.*3.1416)
        GROW2AV(I) = -1.*(CC1 + CC2)/6.
        GROW3AV(I) = GROW2AV(I)
        RXX = 1. - 4.*(CC1*CC2-B1*A12*B2*A21)/((CC1+CC2)**2)
        IF(RXX.LT.0.0) GOTO 750
        GROW2AV(I) = GROW2AV(I)*(1. + SQRT(RXX))
        GROW3AV(I) = GROW3AV(I)*(1. - SQRT(RXX))
 750    CONTINUE

C     N-V-T INSTABILITY (CGS UNITS)
        BMAG = 1.E4*B
        ERAD = 1.E-4*ERAD/3.
        VRAD = 2.E2*FLUXPART/(XNPED+XNSEP)
        XM = 3.343E-27
        Q = 1.6E-19
        OMI = Q*B/XM
        CS = 1.E2*SQRT(XK*(TSOL+TPED)/XMASS)
        RHOT = CS/OMI
        XKPERP = 3.1416/(1.E2*BETAG*XLPERP)
        CHIPERPI = 0.5*CHIR
        CHIPERPE = 0.5*CHIR
        CHIRI = 0.5*CHIR
        IF(I.EQ.14) CHIRI = CHITBI*XI
        CHIRE = 0.5*CHIR
        IF(I.EQ.14) CHIRE = CHITBE*XE
        TIZ = (TPEDEXI + TSEPEXI)/2.
        TEZ = (TPEDEXE + TSEPEXE)/2.

        VPERPI = 1E2*(-1.*ERAD - TEZ*(XLN + XLT)*1E2)/(BMAG)
        VPERPE = 1E2*(-1.*ERAD + TIZ*(XLN + XLT)*1E2)/(BMAG)
        DELNP= -0.5*XNUAT*(XLT+XLN)/OMI + 2.*ERAD/(TPED+TSEP)
        DELTP = 0.


C           STRONG TEMPERATURE EQUILIBRATION **EQ 49
        FTB = 1.0
        IF(I.EQ.14) FTB = 0.0
        OMRESTRONG(I) = -1.*(FTB*CHIRTB*(XKR**2) + CHIR*XNU*(XLT**2) +
     2      FTB*CHIRTB*(XKPERP**2) + FTB*CHIR*XNU*(DELTP**2) +
     3      5.*VRAD*XNU*XLT -
     4      XNCGS*(XNU*(XLZ/TM) - DLZZDT) -
     5      5.*XNUION*(XNU-1) - XNUION*XNU*EIONM/TM -
     6      1.5*XNUAT*(XNU -1.) +
     7      5.*XNU*DELTP*(VPERPI + VPERPE)/2.)/3.

C           WEAK TEMPERATURE EQUILIBRATION ** EQS 50 & 51

        OMREWEAKI(I)= -2.*(FTB*CHIRTBI*(XKR**2) + CHIRI*XNU*(XLTI**2) +
     2      FTB*CHIRTBI*(XKPERP**2) + FTB*CHIPERPI*XNU*(DELTP**2) +
     2      CC*VRAD*XNU*XLTI -
     3      CC*XNUION*(XNU - 1.) -
     4      3.*XNUAT*(XNU - 1.)/2. + CC*XNU*DELTP*VPERPI
     5      + CC*RHOT*CS*(DELNP*XLTI - XLN*DELTP))/3.

        IF(I.EQ.14) ALPHI =     -1.*(CC*VRAD*XNU*XLTI -
     3      CC*XNUION*(XNU - 1.) -
     4      3.*XNUAT*(XNU - 1.)/2. + CC*XNU*DELTP*VPERPI
     5      + CC*RHOT*CS*(DELNP*XLTI - XLN*DELTP))


        OMREWEAKE(I)= -2.*(FTB*CHIRTBE*(XKR**2) + CHIRE*XNU*(XLTE**2) +
     2      FTB*CHIRTBE*(XKPERP**2) + FTB*CHIPERPE*XNU*(DELTP**2) +
     2      CC*VRAD*XNU*XLTE -
     3      CC*XNUION*(XNU-1.) -
     4      XNUION*EIONM*XNU/TEZ + CC*XNU*DELTP*VPERPE -
     5      XNCGS*(XNU*XLZ/TEZ - DLZZDT) +
     6      CC*RHOT*CS*(DELNP*XLTE-DELTP*XLN))/3.

c     IF(I.EQ.14)
        ALPHE =  -1.*(CC*VRAD*XNU*XLTE -
     3     CC*XNUION*(XNU-1.) -
     4     XNUION*EIONM*XNU/TEZ + CC*XNU*DELTP*VPERPE -
     5     XNCGS*(XNU*XLZ/TEZ - DLZZDT) +
     6     CC*RHOT*CS*(DELNP*XLTE-DELTP*XLN))
        IF(I.NE.9) GOTO 900
C           RADIAL WAVELENGTH THRESHOLDS
C     ELECTRON TEMP MODES WITH Kll<XKRE ARE UNSTABLE
        XKRE2 =   -1.0*(2.*CHIRE*XNU*(XLTE**2) +
     2      CHIRTBE*(XKPERP**2) + CHIPERPE*XNU*(DELTP**2) +
     2      5.*VRAD*XNU*XLTE/2. -
     3      5.*XNUION*(XNU-1.)/2. -
     4      XNUION*EIONM*XNU/TEZ + 5.*XNU*DELTP*VPERPE/2. -
     5      XNCGS*(XNU*XLZ/TEZ - DLZZDT) +
     6      5.*RHOT*CS*(DELNP*XLTE-DELTP*XLN)/2.)/
     7      (2.*CHIRTBE)
C     ION TEMP MODES WITH Kll<XKRI ARE UNSTABLE
        XKRI2 =        -1.0*(2.0*CHIRI*XNU*(XLTI**2) +
     2      CHIRTBI*(XKPERP**2) + CHIPERPI*XNU*(DELTP**2) +
     2      5.*VRAD*XNU*XLTI/2. -
     3      5.*XNUION*(XNU - 1.)/2. -
     4      3.*XNUAT*(XNU - 1.)/2. + 5.*XNU*DELTP*VPERPI/2.
     5      + 5.*RHOT*CS*(DELNP*XLTI - XLN*DELTP)/2.)/
     6      (2.0*CHIRTBI)
C     XK2<0 MEANS THAT THE MODE IS STABLE FOR ALL XK
        IF(XKRE2.LT.0.0) GOTO 800
        XKRE = SQRT(XKRE2)
        GOTO 810
 800    XKRE = 0.0
 810    IF(XKRI2.LT.0.0) GOTO 820
        XKRI = SQRT(XKRI2)
        GOTO 830
 820    XKRI = 0.0
 830    CONTINUE
C  DT IS THE THRESHOLD WAVELENGTH;MODES WITH LAMBDA>DT ARE UNSTABLE
C  ANALYSIS GOOD ONLY FOR LAMDA<DELTB (I.E. OVER CONST LT-1 RANGE)
C     EXPRESS IN MKS
        IF(XKRE.GT.0.0) DTE = 3.1416E-2/XKRE
        IF(XKRI.GT.0.0) DTI = 3.1416E-2/XKRI

C           RESET FIELDS TO MKS
 900    BMAG = 1E-4*BMAG
        ERAD = 3E4*ERAD
        VRAD = 1.E-2*VRAD




 1000 CONTINUE


      RETURN
      END
