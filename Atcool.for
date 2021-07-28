      SUBROUTINE ATCOOL(TDZ,XNDZ,XNOZ,XNOCOLDZ,TNCOLD,SATZ,SRADZ,XNUPZ,
     1    xnumomz,ZAV)
      INCLUDE 'Soldiv.fi'
      DOUBLE PRECISION TEV, CELZ, DLZZDT

      RATNENI2D=(1.+ IZINJECT*FZINJECT+4.*FBE+2.*FHE+
     2    IZINTRIN*FZINTRIN)**2

C     ATOMIC PHYSICS
c********different Te and Ti based on exp ratite= Ti/Te for Sol**********
c     ratite = 4.0
c        value for 98889 @ 3960
c     tev = 2.*tdz/(1+ratite)
c     tid = 2.*tdz*(1. - 1./(1. + ratite))
c*************************************************************************
      CNEUT = 1.
      IF(IOPTELN.EQ.0) CNEUT = 0.
      TDD = TDZ
      IF(TdD.LT.1.E-1) TDD = 1.05E-1
      IF(TdD.GT.1E3) TDD = .95E3
      XNDD = XNDZ
      IF(XNDd.GT.1E22) XNDD = 0.95E22
      IF(XNDd.LT.1E16) XNDD = 1.1E16
      TND = TdD
      IF(TND.GE.1000) TND = 995.
c
      CALL INTERP(Tdd,TdD,TND,XNDD)
      SVELZ = SEL(1)
      SVELZN= SELN(1)
      SVCXZ = SCX(1)
      XNODZ = XNOZ
      SVATZ = SEL(1) + CNEUT*SELN(1)*XNODZ/XNDD + SCX(1)
      SVIONZ = SION(1)
      SVRECZ = RECOM(1)
      IF(XNDD.LE.1.E21)
     2    EIONZ = 17.5 + (5.+37.5/TDD)*LOG10(1.E21/(XNDD))
      iF(XNDD.GT.1.E21)
     2    EIONZD = (30.6 - 16.4*EXP(-5.E19/XNDD))*
     3    EXP(5.45/(TDD*EXP((XNDD/1.37E20)**0.26)))
      ERECION = 13.6
      SATZ = XK*EIONZ*XNDD*XNODZ*SVIONZ -
     1    XK*FREC*ERECION*(XNDD**2)*SVRECZ
     2    + 1.5*XNDD*XNOCOLDZ*SVATZ*XK*(TDD-TNCOLD)
      XNUPZ = (XNOZ*SVIONZ-XNDZ*SVRECZ)
      xnumomz = xndz*svrecz + xnocoldz*svatz
      xmas1 = 3.34e-27
      veencold = sqrt(2.*xk*tncold/xmas1)
      veen = sqrt(2.*xk*tdz/xmas1)
      ylmfp=1.0/((XNOZ*SVIONZ+XNDZ*SVRECZ)/veen+xnocoldz*svatz/veencold)
C     RADIATION
      TEV = TDD
      FON = XNODZ/XNDD
      IZ1 = IZINJECT
      IZ2 = IZINTRIN
      if(fon.lt.1.e-5) fon = 1.e-5
c     injected impurity
      if(iz1.ne.4.and.iz1.ne.6.and.iz1.ne.74) goto 125
      CALL CXRCEFITS(iz1,TEV,fon,celz,dlzzdt,ZAV)
      DQRAD1 = 1.E-13*CELZ*FZINJECT*qzmultdiv*(XNDZ**2)
      goto 150
 125  CALL cefits (IZ1, TEV, celz, 1, dlzZdt)
      DQRAD1 = 1.E-13*CELZ*FZINJECT*qzmultdiv*(XNDZ**2)
c     intrinsic impurity
 150  if(iz2.ne.4.and.iz2.ne.6.and.iz2.ne.74) goto 175
      CALL CXRCEFITS(iz2,TEV,fon,celz,dlzzdt,ZAV)
      DQRAD2 = 1.E-13*CELZ*FZINTRIN*QZMULTDIV*(XNDZ**2)
      goto 190
 175  CALL CEFITS (IZ2, TEV, celz, 1, dlZzdt)
      DQRAD2 = 1.E-13*CELZ*FZINTRIN*QZMULTDIV*(XNDZ**2)
 190  continue
c     bremsstrahlung
      dqradbrem = 4.8e-37*zeff*(XNDZ**2)*sqrt(TDD)
 200  continue
      SRADZ = dqradbrem + dqrad1 + DQRAD2
      RETURN
      END
