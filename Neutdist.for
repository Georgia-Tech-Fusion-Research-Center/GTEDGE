      SUBROUTINE NEUTDIST
      INCLUDE 'Soldiv.fi'
C         CALCULATES NEUTRAL & COLD NEUTRAL DISTRIBUTION IN EDGE
      PARAMETER (I10 = 26)
      DIMENSION ALPH(I10),REFLECT(I10),TRANS(I10), XJM(I10),
     1  SVTOT(I10)
c    xfrac = 0.
      IOPTEXP = 2
c         ioptexp=n, t0= (n-1)E^n
      RATNENI2D=(1.+ IZINJECT*FZINJECT+4.*FBE+2.*FHE+
     2  IZINTRIN*FZINTRIN)**2
      SE = SQRT(0.5*(1.+ELONG**2))
      gamouteff = xfrac*gamoutpf + (1.-xfrac)*gamoutspl
C         COLD, UNCOLLIDED NEUTRALS

      DO 600 N = 1,25
C         ATOMIC DATA    FOR COLD NEUTRALS TN = TSPL
        CNEUT = 1.
        IF(IOPTELN.EQ.0) CNEUT = 0.
        TED = TEL(N)
        TID = TI(N)
        IF(TED.LT.1.E-1) TED = 1.05E-1
        IF(TED.GT.1E3) TED = .95E3
        IF(TID.LT.1.E-1) TID = 1.05E-1
        IF(TID.GT.1E3) TID = .95E3
        YND = YNI(N,1)
        IF(YND.GT.1E22) YND = 0.95E22
        IF(YND.LT.1E16) YND = 1.1E16
        TND = TSPL
        IF(TND.GE.1000) TND = 995.
        CALL INTERP(TED,TID,TND,YND)
        SVEL = SEL(1)
        SVELN= SELN(1)

        SVCX = SCX(1)
        SVATA(N)= SEL(1) + CNEUT*SELN(1)*YNO(N)/YNI(N,1) + SCX(1)
        SVIONA(N) = SION(1)
        x = sion(1)
        SVREC = RECOM(1)
        SVTOT(N) = SVIONA(N) + SVATA(N)
600   CONTINUE
      SVIOINSOL = SVIONA(25)
      SVATSOL = SVATA(25)
      SVTOTSOL = SVTOT(25)
      VCOLD = SQRT(XK*TSPL/XMASS)
      Z4 = 0.0
      DO 650 NN = 1,25
        N = 26-NN
        DENS(N) = 0.5*(YNI(N+1,1)+YNI(N,1))
        DELMA = DELNA
        IF(N.EQ.25) THEN
          DENS(N) = 0.632*XNSOL
          SVTOT(N) = SVTOTSOL
          DELMA = DELN
        ENDIF
c    if(n.eq.24) delma = 0.5*delna
c    if(n.eq.1)  delma = 0.5*delna
        ZP = DELMA*DENS(N)*SVTOT(N)/VCOLD
        if(n.eq.1.or.n.eq.24) zp = 0.5*zp
        Z3 = Z4
        Z4 = Z3 + ZP
        IF(Z3.EQ.0.0) Z3 = 0.0001
        CALL EXPINT(Z3,E13,E23,E33,E43)
        CALL EXPINT(Z4,E14,E24,E34,E44)
        IF(IOPTEXP.EQ.4)DELEXP = E43-E44
        IF(IOPTEXP.EQ.3)DELEXP = E33-E34
        IF(IOPTEXP.EQ.2)DELEXP = E23-E24

        YNOCOLD(N) = enh*(GAMOUTeff)*(ioptexp-1)*DELEXP/
     1  (DENS(N)*SVTOT(N)*DELMA)
        FNCOLD(N)  = YNOCOLD(N)/YNI(N,1)
        const = ioptexp-1
        if(ioptexp.eq.4) atten = e44
        if(ioptexp.eq.3) atten = e34
        if(ioptexp.eq.2) atten = e24
        coldno(n) = gamouteff*const*atten/vcold
650   CONTINUE
C    NOTE:  FNCOLD(N) & FNO(N)     REFER TO THE REGION N BETWEEN MP N & N+1


C         TOTAL NEUTRALS
c         SOL ATTENTUATION AT TN = TSPL
      ASCAT = SVATSOL/SVTOTSOL
      ZSOL = DELN*.632*XNSOL*SVTOTSOL/VOSOL
      CALL EXPINT(ZSOL,E1,E2,E3,E4)
      PO = (0.5 - E3)/ZSOL
      PESCAPE = PO/(1. - ASCAT*(1. - PO))
      IF(IOPTEXP.EQ.4) TOSOL = 3.*E4
      IF(IOPTEXP.EQ.3) TOSOL = 2.*E3
      IF(IOPTEXP.EQ.2) TOSOL = 1.*E2
      REFLECT(25) = 0.5*ASCAT*PESCAPE*(1. - TOSOL)
      TRANS(25) = TOSOL + REFLECT(25)
      SVIONA(25) = SVIONSOL

      write(9015,'(A1,A10,A7,3A10)') 'n','PESCAPE','TO','ASCAT',
     1'Reflect(N)','TRANS(N)'

      DO 200 N = 1,24
C         ATOMIC DATA    FOR TN = LOCAL TI
        CNEUT = 1.
        IF(IOPTELN.EQ.0) CNEUT = 0.
        TED = TEL(N)
        TID = TI(N)
        IF(TED.LT.1.E-1) TED = 1.05E-1
        IF(TED.GT.1E3) TED = .95E3
        IF(TID.LT.1.E-1) TID = 1.05E-1
        IF(TID.GT.1E3) TID = .95E3
        YND = YNI(N,1)
        IF(YND.GT.1E22) YND = 0.95E22
        IF(YND.LT.1E16) YND = 1.1E16
        TND = TID
        IF(TND.GE.1000) TND = 995.
        CALL INTERP(TED,TID,TND,YND)
        SVEL = SEL(1)
        SVELN= SELN(1)
        SVCX = SCX(1)
        SVATA(N)= SEL(1) + CNEUT*SELN(1)*YNO(N)/YNI(N,1) + SCX(1)
        SVIONA(N) = SION(1)
        SVREC = RECOM(1)
        SVTOT(N) = SVIONA(N) + SVATA(N)
        ASCAT = SVATA(N)/SVTOT(N)
        VO = SQRT(XK*TND/XMASS)
C         TRANSMISSION & REFLECTION COEFFICIENTS
c    if(n.eq.24) delma = 0.5*delna
c    if(n.eq.1)  delma = 0.5*delna

        ZP = DELMA*0.5*(YNI(N,1)+YNI(N+1,1))*SVTOT(N)/VO

        CALL EXPINT(ZP,E1,E2,E3,E4)
        PO = (0.5 - E3)/ZP
        PESCAPE = PO/(1. - ASCAT*(1. - PO))
        IF(IOPTEXP.EQ.4) TO = 3.*E4
        IF(IOPTEXP.EQ.3) TO = 2.*E3
        IF(IOPTEXP.EQ.2) TO = 1.*E2
        REFLECT(N) = 0.5*ASCAT*PESCAPE*(1. - TO)
        TRANS(N) = TO + REFLECT(N)
9555    format(I2,5E10.3)
        write(9015,9555) N,PESCAPE,TO,ASCAT,Reflect(N),TRANS(N)

200   CONTINUE

C         RECURSIVE "ALBEDO" RELATION
      ALPH(1) = ALPHACORE
      DO 300 N = 2,26
        ALPH(N) =(ALPH(N-1)*(TRANS(N-1)**2-REFLECT(N-1)**2)+REFLECT(N-1))/
     2  (1. - ALPH(N-1)*REFLECT(N-1))
300   CONTINUE

C         ALBEDOS
      ALPHASOL = ALPH(26)
      ALPHASEP = ALPH(25)

C         RECURSIVE INWARD CURRENT RELATION
      XJM(26) = enh*GAMOUTeff
      DO 400 N = 1,25
        J = 26-N
        XJM(J) = XJM(J+1)*(REFLECT(J)*ALPH(J+1)/TRANS(J) +
     2           TRANS(J) - (REFLECT(J)**2)/TRANS(J) )
        FLUXINNEUT(N) = XJM(N)
400   CONTINUE

C         TOTAL NEUTRAL DENSITY & CONCENTRATION
c         (neutral density n is average for region between mp's n and n+1)
      DO 500 N = 1,25
        SVT = SVIONA(N)
        DELMA = DELNA
        IF(N.EQ.25) THEN
          SVT = SVIONSOL
          DELMA = DELN
        ENDIF
c    if(n.eq.1.or.n.eq.24) delma = 0.5*delna
455     YNO(N) = (XJM(N+1)*(1.-ALPH(N+1)) - XJM(N)*(1.-ALPH(N)))/
     2  (DENS(N)*SVT*DELMA)

c     write(*,*) 'N =', N
c     write(*,*) 'YNO(N) =', YNO(N)
c       write(*,*) 'XJM(N) =', XJM(N)
c       write(*,*) 'ALPH(N) =', ALPH(N)
c       write(*,*) 'REFLECT(N) =', REFLECT(N)
c       write(*,*) 'TRANS(N) =', TRANS(N)
c     write(*,*) 'DENS(N) =', DENS(N)

        FNO(N) = YNO(N)/DENS(N)

500   CONTINUE


      RETURN
      END








