      SUBROUTINE NEUTFRAC
      INCLUDE 'Soldiv.fi'
C         CALCULATES NEUTRAL FRACTION & COLD NEUTRAL FRACTION IN SOL, TB & PEDESTAL
      PARAMETER (I10 = 59)
      DIMENSION ALPH(I10),XJM(I10),REFLECT(I10),TRANS(I10)

      xnpedz = xnped
      xnbarz = xnbar
      if(ioptsoln.eq.1) then
        xnbarz = 0.5*(xnpedex+xnsepex)
        xnpedz = xnpedex
      endif
      xnc(56) = xnpedz
      xnc(57) = xnbarz
      xnc(58) = xnsol

      RATNENI2D=(1.+ IZINJECT*FZINJECT+4.*FBE+2.*FHE+
     2              IZINTRIN*FZINTRIN)**2
      SE = SQRT(0.5*(1.+ELONG**2))
      DELRAD = AMINOR/56.
C    DELRAD = (AMINOR-DELTB)*SE/56.
C         TOTAL NEUTRALS
      ASCAT = SVATSOL/SVTOTSOL
C    AVG DENSITY IN SOL =NSOL( 1. - EXP(-1.))= 0.632
      ZSOL = DELN*.632*XNSOL*SVTOTSOL/VOSOL
      CALL EXPINT(ZSOL,E1,E2,E3,E4)
      PO = (0.5 - E3)/ZSOL
      PESCAPE = PO/(1. - ASCAT*(1. - PO))
      TOSOL = 3.*E4
      REFLECT(58) = 0.5*ASCAT*PESCAPE*(1. - TOSOL)
      TRANS(58) = TOSOL + REFLECT(58)
      ASCAT = SVATBAR/SVTOTBAR
      ZBAR = DELTB*XNBARz*SVTOTBAR/VOBAR
      CALL EXPINT(ZBAR,E1,E2,E3,E4)
      PO = (0.5 - E3)/ZBAR
      PESCAPE = PO/(1. - ASCAT*(1. - PO))
      TOBAR = 3.*E4
      REFLECT(57) = 0.5*ASCAT*PESCAPE*(1. - TOBAR)
      TRANS(57) = TOBAR + REFLECT(57)
      DO 200 N = NNEUT,56
        ASCAT = SVATPED/SVTOTPED
        ZPED = DELRAD*0.5*(XNC(N)+XNC(N+1))*SVTOTPED/VOPED
        CALL EXPINT(ZPED,E1,E2,E3,E4)
        PO = (0.5 - E3)/ZPED
        PESCAPE = PO/(1. - ASCAT*(1. - PO))
        TOPED = 3.*E4
        REFLECT(N) = 0.5*ASCAT*PESCAPE*(1. - TOPED)
        TRANS(N) = TOPED + REFLECT(N)
 200  CONTINUE

C         RECURSIVE "ALBEDO" RELATION
      ALPH(NNEUT) = ALPHACORE
      DO 300 N = NNEUT+1,59
        ALPH(N) =(ALPH(N-1)*(TRANS(N-1)**2-REFLECT(N-1)**2)+REFLECT(N-1))/
     2            (1. - ALPH(N-1)*REFLECT(N-1))
 300  CONTINUE

C         ALBEDOS
      ALPHASOL = ALPH(59)
      ALPHASEP = ALPH(58)

C         RECURSIVE INWARD CURRENT RELATION
      XJM(59) = GAMOUTSPL
      DO 400 N = 1,59-NNEUT
        J = 59-N
        XJM(J) = XJM(J+1)*(REFLECT(J)*ALPH(J+1)/TRANS(J) +
     2           TRANS(J) - (REFLECT(J)**2)/TRANS(J) )
 400  CONTINUE

C         INWARD NEUTRAL CURRENT CROSSING SEPARATRIX
      CURSEP = XJM(58)

C         TOTAL NEUTRAL DENSITY & CONCENTRATION
      DO 500 N = NNEUT,58
        DD = DELRAD
        SVT = SVIONPED
        IF(N.NE.58) GOTO 450
        DD = DELN
        SVT = SVIONSOL
        GOTO 455
 450    IF(N.NE.57) GOTO 455
        DD = DELTB
        SVT = SVIONBAR
 455    XNO(N) = (XJM(N+1)*(1.-ALPH(N+1)) - XJM(N)*(1.-ALPH(N)))/
     2                 (XNC(N)*SVT*DD)
        FNO(N) = XNO(N)/XNC(N)
c        write(*,*) 'N =', N
c         write(*,*) 'XNO(N) =', XNO(N)
 500  CONTINUE
      XNOSOL = XNO(58)
      XNOBAR = XNO(57)
      XNOPED = XNO(56)
C         IONIZATION RATES
      ZL = BETAG*XLPERP - 0.5*DELXPT
      YIONSOL =  (XJM(59)*(1.-ALPH(59)) - XJM(58)*(1.-ALPH(58)))*ZL
      YIONTB  =  (XJM(58)*(1.-ALPH(58)) - XJM(57)*(1.-ALPH(57)))*ZL
      YIONPED =  (XJM(57)*(1.-ALPH(57)) - XJM(56)*(1.-ALPH(56)))*ZL

C         COLD, UNCOLLIDED NEUTRALS

      DELRAD = AMINOR/55.
      VCOLD = SQRT(XK*TSPL/XMASS)
      Z4 = 0.632*XNSOL*SVTOTSOL*DELN/VCOLD
      CALL EXPINT(Z4,E14,E24,E34,E44)
      FNCOLD(58)=(GAMOUTSPL/xnsol)*(1.-3.*E44)/
     2           (xnsol*SVTOTSOL*DELN)
      Z3 = Z4
      Z4 = Z3 + XNBARz*SVTOTBAR*DELTB/VCOLD
      CALL EXPINT(Z3,E13,E23,E33,E43)
      CALL EXPINT(Z4,E14,E24,E34,E44)
      FNCOLD(57)=(GAMOUTSPL/xnbarz)*3.*(E43-E44)/
     2              (xnbarz*SVTOTBAR*DELTB)
      DO 550 NN = 1,5
        N = 57-NN
        Z3 = Z4
        ZPED = DELRAD*0.5*(XNC(N-1)+XNC(N))*SVTOTPED/VCOLD
        Z4 = Z3 + ZPED
        CALL EXPINT(Z3,E13,E23,E33,E43)
        CALL EXPINT(Z4,E14,E24,E34,E44)
        FNCOLD(N)=(GAMOUTSPL/XNC(N))*3.*(E43-E44)/
     2        (XNC(N)*SVTOTPED*DELRAD)
 550  CONTINUE
C         AVERAGE PEDESTAL, TB & SOL COLD NEUTRAL DENSITY
      XNOPEDCOLD = FNCOLD(56)*XNC(56)
      XNOCOLDBAR = FNCOLD(57)*xnbarz
      XNOCOLDSOL = FNCOLD(58)*xnsol

C         FIRST-COLLISION RATES FOR PLASMA CALCULATION
      FCRATSOL = (XNC(58)*FNCOLD(58))*(DELN)*SVATSOL*BETAG
     2           *XLPERP*xnsol
      FCRATBAR = (XNC(57)*FNCOLD(57))*(DELTB)*SVATBAR*BETAG
     2           *XLPERP*xnbarz
      FCRATPED = (XNC(56)*FNCOLD(56))*DELRAD*SVATPED*BETAG*
     2           XLPERP*XNC(56)

      RETURN
      END








