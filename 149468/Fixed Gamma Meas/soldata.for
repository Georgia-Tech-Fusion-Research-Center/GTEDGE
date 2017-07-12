
      SUBROUTINE SOLDATA(nbdep1,nbdep2,nbdep3,erextot,fpsi0)

c*****************************************************************
c     Reads data in from text files and defines edge profiles from 
c     the DIII-D database
c*****************************************************************

	INCLUDE 'SOLDIV.FI'

	real ibal,imod,ioptsep,vplas,ioptgeom,dchirat,changeouter,tw,
	1     gammid,xlmid,czd(25),cmid,delsol,rwall,ioptdel,ioptpump,
	2     hxpt,riol,bpiol,btiol,psireal,vpold,vtord
	real nbdep1(51),nbdep2(51),nbdep3(51), erextot(51), fpsi0(51)

	
	NAMELIST/GEOM/IOPTSN,ZX,RX,RSEP1,ZSEP1,RSEP2,ZSEP2,RDOME,PHIDOME,
	2		XRING,ZRING,DELLT1,DELLT2,YPUMP1,YPUMP2,
     3		RPUMP1,RPUMP2,R1,R2,PHITRAN,RINROUT,XLWPL1,XLWPL2,XLUP1,
     4		XLWPF1,XLWPF2,XLWSPL,elong,AMINOR,RMAJOR,TRIANG,DELXreal
     5		,DELXPT,zdome,xdome

	
	NAMELIST/CORE/IPFLUX,IBAL,IMOD,ISHEATH,FLUXPART,FLUXHEAT,FRACRAD,
     2    	TSEP,TD,TSOL,TBAR,TPED,TPLAS,TAV,XND,XNOD,XNPLAS,XNPED,
     3        XNBAR,XNSEP,XNSOL,XNDIV,XNOSOL,XNODIV,IOPTSEP,DELRATNT,
     4		DELRATNV,IOPTDEL,PFUSION,FLUXNEUTIN,FLUXIONIN,IOPTPROF,
	5		IOPTOHM,POHMIN
	

	NAMELIST/PARAM/B,IOPTQ95,Q95,GAMSHEATH,IOPTRAD,TAURES,IZINJECT,
	1        FZINJECT,AZINJECT,AZTRACE,IZTRACE,REDKAPPA,SOLNMULT,
     2 SOLTMULT,IZINTRIN,FZINTRIN,FHE,FBE,VPLAS,DELSOL,IOPTSOL,XNATOM,
     3    XLZRAD,FS,ALB,IOPTGEOM,CHANGE,CHANGEOUTER,DELNMAX,DELNMIN,
     4        DCHIRAT,ENHANCE,QZMULTDIV,CHIRSOL,FUDGE,REDKAPPA2,
	5  XDSEP,ASYMPART,ASYMHEAT,ioptdiv,deld,divdepth,xenhance,ZHGHT
	NAMELIST/GASNEUT/REFLECT,TW,GAMDIV,GAMMID,XLMID,CZD,CMID,EIOND,
     2                 EIONSOL,RWALL,EEX
	NAMELIST/XSECT1/IOPTLYMAN,ELAST,ELASTN
	NAMELIST/XSECT2/CX,EION
	NAMELIST/XSECT3/REC,TINT,ZNINT,FREC
	NAMELIST/GASNEUT2/BETAG,FPF,FPD,THETA,FMOL,FUELPF,FUELPLOUT,
     2	FUELPLIN,FUELMP,SPELLET,RWPF,RWPL,HXPT,
     3	RWSOL,XMSOL,HNDIV,HVSOL,HVDIV,HTSOL,HTDIV,DELTB,DELPED,
     4	RWDP,IOPTPUMP,DELPUMP,RPUMP,PUMPSPEED,HNSOL,wtxpt,
	5	EPDIV,FRACXPT,IMAT,IMOL,FEX,TSOR,AMU,wallmult,iopttheta,
	6	REABSORBD,REABSORBDIV,REABSORBSOL,REABSORBCOR,IOPTELN,IOPTSOLN
 	NAMELIST/PEDBAR/NBAR,NPED,GRADTPEDIS,XNU,ZEFF,Z0,CFZINJ,
     2	CFZINT,cfzinjtb,cfzinttb,ALPHAT1,ALPHAT2,IOPTPEDWIDTH,CPED,
     3	A1,A2,CHIREDGE,PLASMACUR,PBEAM,ENBI,PHIIN,RADMULT,TAVMULT,
	2    IOPTPART,HRAT,RHOGAUSS,SIGMA,ZEFFC,DPERP,TAUE,TAURATIO,HCONF,
	3	c89,CHEI,ALPHAN,RCOEF,XNCTRPED,TCTRPED,NNEUT,IOPTPED,FION,
	4	RXLTIXLTE,ERAD,VPHI,VTHET,VRAD,XNUBAR,XLQ,XLE,XLVTHET,ISHAPE,
	5	C89N,CC,XLVPHI,XNAV,AMASS,XNRHOPED,RSYN,IPROF,JJOPTPED,
	6 IOPTGRAD,FCOND,CBALLOON,HN,QZMULTCORE,PEDFAC,CHIRTBI,CHIRTBE,
	7	cPINCH,VPINCH,DTBI,IOPTLN,CIE,CNE,HEATFRACE,IOPTPINCH,CPED2,
     8  CPED1,JOPTPED2,fluxheatianom,fluxheateanom,IGRADPED,FI,IOPTCONF,
	9	IDRAKE,SHEARQ,CBALL,FISEP,CHITBLI,CHITBLE,SHEAR0,CSHEAR,ASS,
	1	IOPTTAUE,IOPTTRAN,MODPED,FSHAPE,CMHD,ccrit,cmfp

	NAMELIST/EXPDATA/XNPEDEX,XNSEPEX,TPEDEXI,TSEPEXI,GRADNBAR,GRADTBAR
     2,GRADNPED,GRADTPED,GRADTEBAR,CHIRC,CHIBAR,CHIRMARF,APED,TPEDAV, 
	3DREDGE,TPEDEXE,TSEPEXE,WIDTHNX,WIDTHTEX,WIDTHTIX, 
	4  dln_dt,dlnw_dt,wmhd,pradcorex,ssi95,chii0,chie0,xkrtherm,
	5  dlnnped_dt,dlnpped_dt
	NAMELIST/EDGE/IOPTEDGE,PHITE,IGRAD,XLT11,XLV1,FRACZ,ZION,ZIMP,
     1		AION,AIMP,BPHI,YLTIbarx,YLNBARX,YLVBARX,eb,abeam,rtan,
     2		alphain,VPHIAPEDX,DELNA,ephia,enh,sindeno,yltebarx,
     3		joptedped,SHEARM,dedr,sheare,ioptvphi,ioptpinchi,
     4	ioptvdif,ioptxlvm,sheareconst,fheate,ioptran,
	5	chixpi,chixpe,chitop,chetop,teintop,tiintop,xnintop,xltitop,
     5		xltetop,pedrhoti,pedrhote, rhotein,rhotiin,ioptdrag,ylti,
	6		ylte,ioptzerodrag,ioptapproach,iopterad,ioptvthet,
	7		ioptranscoef,erex,  ioptexpdata,xnesepreal,
	8		pedrhon,rhonin,OMEGTSEP1,OMEGTSEP2,OMEGTSEP1C,OMEGTSEP2C,
	9		OMEGTSEP1S,OMEGTSEP2S,anomdrag
	1		,delbsep,ylntop,VTHEXP,TORV,ioptpol


	NAMELIST/EXPROFILE1/EXNE
	NAMELIST/EXPROFILE2/XLNE,exlv
	NAMELIST/EXPROFILE3/XTE
	NAMELIST/EXPROFILE4/EXLTE
	NAMELIST/EXPROFILE5/XTI
	NAMELIST/EXPROFILE6/EXLTI

	NAMELIST/TIMEDEP/dlnn_dt,dlnwe_dt,dlnwi_dt
	namelist/balance/hconf,hrat,alphan,alphat2,cballoon
		
	OPEN(UNIT=10,FILE='DATA10',STATUS='OLD')
      READ (10,GEOM)
      CLOSE (UNIT=10,STATUS='KEEP') 
	OPEN(UNIT=11,FILE='DATA11',STATUS='OLD')
      READ (11,CORE)
 	CLOSE (UNIT=11,STATUS='KEEP')

     
	OPEN(UNIT=12,FILE='DATA12',STATUS='OLD')
      READ (12,PARAM)
      CLOSE (UNIT=12,STATUS='KEEP')
	OPEN(UNIT=13,FILE='DATA13',STATUS='OLD')
      READ (13,GASNEUT)
      CLOSE (UNIT=13,STATUS='KEEP')
C	OPEN(UNIT=14,FILE='DATA14',STATUS='OLD')
C      READ(14,XSECT1)
C      READ(14,XSECT2)
C	READ(14,XSECT3)
C	CLOSE(UNIT=14,STATUS='KEEP')
      open(unit=21,file='balance.txt',status='old')
	read(21,balance)
	close(21,status='keep')
	
	open(unit=14,file='radius_psi.txt',status='old')
 	do 25 j = 1,25
	    do 26 k = 1,9
             read(14,*) riol(j,k)
26        continue
25    continue
	close(14,status='keep')

	IOPTLYMAN = 0
	ELAST(1,1) = 2.7E-14
	ELAST(1,2) = 2.7E-14
	ELAST(1,3) = 4.6E-14
	ELAST(1,4) = 9.2E-14
	ELAST(1,5) = 9.2E-14
	ELAST(1,6) = 9.2E-14 
	ELAST(2,1) = 2.7E-14
	ELAST(2,2) = 2.7E-14
	ELAST(2,3) = 4.6E-14
	ELAST(2,4) = 9.2E-14
	ELAST(2,5) = 9.2E-14
	ELAST(2,6) = 9.2E-14 
	ELAST(3,1) = 4.6E-14
	ELAST(3,2) = 4.6E-14
	ELAST(3,3) = 6.8E-14
	ELAST(3,4) = 9.0E-14
	ELAST(3,5) = 9.0E-14
	ELAST(3,6) = 9.0E-14
	ELAST(4,1) = 9.2E-14
	ELAST(4,2) = 9.2E-14
	ELAST(4,3) = 9.0E-14
	ELAST(4,4) = 16.0E-14
	ELAST(4,5) = 16.0E-14
	ELAST(4,6) = 16.0E-14
	ELAST(5,1) = 9.2E-14
	ELAST(5,2) = 9.2E-14
	ELAST(5,3) = 9.0E-14
	ELAST(5,4) = 16.0E-14
	ELAST(5,5) = 16.0E-14
	ELAST(5,6) = 16.0E-14
	ELAST(6,1) = 9.2E-14
	ELAST(6,2) = 9.2E-14
	ELAST(6,3) = 9.0E-14
	ELAST(6,4) = 16.0E-14
	ELAST(6,5) = 16.0E-14
	ELAST(6,6) = 16.0E-14 
	ELASTN(1) = 2.7E-15
	ELASTN(2) = 6.8E-15
	ELASTN(3) = 16.0E-15
	ELASTN(4) = 16.0E-15
	ELASTN(5) = 16.0E-15
	ELASTN(6) = 16.0E-15 

	CX(1,1)    = 8.0E-15
	CX(1,2)    = 8.0E-15
	CX(1,3)    = 1.2E-14
	CX(1,4)    = 2.8E-14
	CX(1,5)    = 5.6E-14
	CX(1,6)    = 1.2E-13
	CX(2,1)    = 8.0E-15
	CX(2,2)    = 8.0E-15
	CX(2,3)    = 1.2E-14
	CX(2,4)    = 2.8E-14
	CX(2,5)    = 5.6E-14
	CX(2,6)    = 1.2E-13 
	CX(3,1)    = 1.2E-14
	CX(3,2)    = 1.2E-14
	CX(3,3)    = 1.5E-14
	CX(3,4)    = 2.9E-14
	CX(3,5)    = 5.8E-14
	CX(3,6)    = 1.2E-13
	CX(4,1)    = 2.8E-14
	CX(4,2)    = 2.8E-14
	CX(4,3)    = 2.9E-14
	CX(4,4)    = 3.7E-14
	CX(4,5)    = 7.4E-14
	CX(4,6)    = 1.5E-13
	CX(5,1)    = 2.8E-14
	CX(5,2)    = 2.8E-14
	CX(5,3)    = 2.9E-14
	CX(5,4)    = 3.7E-14
	CX(5,5)    = 7.4E-14
	CX(5,6)    = 1.5E-13
	CX(6,1)    = 2.8E-14
	CX(6,2)    = 2.8E-14
	CX(6,3)    = 2.9E-14
	CX(6,4)    = 3.7E-14
	CX(6,5)    = 7.4E-14
	CX(6,6)    = 1.5E-13 
	eion(1,1)  = 3.0E-21
	eion(1,2)  = 7.6E-21
	eion(1,3)  = 5.3E-15
	eion(1,4)  = 3.1E-14
	eion(1,5)  = 1.5E-14
	EION(1,6) =	 7.5E-15
	eion(2,1)  = 3.0E-21
	eion(2,2)  = 7.6E-21
	eion(2,3)  = 8.0E-15
	eion(2,4)  = 3.7E-14
	eion(2,5)  = 1.9E-14
	EION(2,6) =	 9 5E-15
	eion(3,1)  = 3.0E-21
	eion(3,2)  = 7.6E-21
	eion(3,3)  = 1.2E-14
	eion(3,4)  = 4.0E-14
	eion(3,5)  = 2.0E-14
	EION(3,6) =	 1.0E-14
	eion(4,1)  = 3.0E-21
	eion(4,2)  = 7.6E-21
	eion(4,3)  = 2.2E-14
	eion(4,4)  = 6.0E-14
	eion(4,5)  = 3.0E-14
	EION(4,6) =	 1.5E-14
	eion(5,1)  = 3.0E-21
	eion(5,2)  = 7.6E-21
	eion(5,3)  = 2.4E-14
	eion(5,4)  = 8.0E-14
	eion(5,5)  = 4.0E-14
	EION(5,6) =	 2.0E-14
	
	REC(1,1)   = 3.0E-16
	REC(1,2)   = 7.0E-17
	REC(1,3)   = 8.0E-20
	REC(1,4)   = 7.0E-21
	REC(1,5)   = 4.0E-22
	REC(2,1)   = 2.5E-15
	REC(2,2)   = 1.1E-18
	REC(2,3)   = 8.0E-20
	REC(2,4)   = 7.0E-21
	REC(2,5)   = 4.0E-22
	REC(3,1)   = 2.5E-14
	REC(3,2)   = 2.5E-18
	REC(3,3)   = 8.0E-20
	REC(3,4)   = 7.0E-21
	REC(3,5)   = 4.0E-22
	REC(4,1)   = 2.0E-13
	REC(4,2)   = 7.0E-18
	REC(4,3)   = 8.0E-20
	REC(4,4)   = 7.0E-21
	REC(4,5)   = 4.0E-22
	REC(5,1)   = 1.0E-12
	REC(5,2)   = 2.0E-17
	REC(5,3)   = 8.0E-20
	REC(5,4)   = 7.0E-21
	REC(5,5)   = 4.0E-22
	TINT(1) = 0.1
	TINT(2) = 1.0
	TINT(3) = 10.0
	TINT(4) = 100.0
	TINT(5) = 1000.0
	TINT(6) = 10000.0
	ZNINT(1) = 1.0E16
	ZNINT(2) = 1.0E18
	ZNINT(3) = 1.0E20
	ZNINT(4) = 1.0E21
	ZNINT(5) = 1.0E22
	FREC = 1.
	
C	ALL DATA ARE SIGMA-V FROM E.W. THOMAS EVALUATION 9/94.
C	SCHULTZ ELASTIC, JANEV C-X,IONIZATION ****not****LYMAN SUPPRESSED
C	RECOMBINATION FROM POST ALB=JANEV W/COLLISION-RADIATIVE, POST ET AL 10/9/98
C	FIRST INDEX Tn= 1, 10, 100 eV; 2ND INDEX Ti=.1,1,10,100,1000 eV: EL & CX
C    	1ST INDEX LOG10Ne=16,18,20,21,22 MKS; 2ND INDEX Te=.1,1,10,100,1000 eV: ION & RECOM

     
      OPEN(UNIT=15,FILE='DATA15',STATUS='OLD')
      READ(15,GASNEUT2)
	CLOSE(UNIT=15,STATUS='KEEP')
	OPEN(UNIT=16,FILE='DATA16',STATUS='OLD')
      READ(16,PEDBAR)
	CLOSE(UNIT=16,STATUS='KEEP')
	OPEN(UNIT=17,FILE='DATA17',STATUS='OLD')
      READ (17,EXPDATA)
      CLOSE (UNIT=17,STATUS='KEEP')
	OPEN(UNIT=18,FILE='DATA18',STATUS='OLD')
      READ (18,EDGE) 	
      CLOSE (UNIT=18,STATUS='KEEP')	

	ixx = ioptq95
	thetain = theta
	q95in = q95
	CHIRCORE(1) = CHIRC
	SPELLETREAL = SPELLET
	FUELMPREAL = FUELMP  
10	PFUSIONIN = PFUSION 
	BFIELD = B
  	XMASS = XNATOM*1.67E-27
	XK = 1.6E-19 
	FESEP = 1. - FISEP

C		SHEATH BOUNDARY CONDITION
	FSHEATH = 1.0
	IF(ISHEATH.EQ.1) FSHEATH = 1.0/BETAG
C		ATOMS ONLY RECYCLING
	IF(IMOL.NE.0) GOTO 100 
C	RWDP = 1.0	 
	FMOL = 0.0
C		ATOMS + GROUND STATE MOLECULES RECYCLING
100	IF(IMOL.EQ.1) FEX = 0.0
C		CALCULATE RATIO EXP TI & TE GRAD SCALE LENGTHS IN TB
	IF(JJOPTPED.EQ.9) FION = GRADTBAR(4)/GRADTEBAR(4)
	
C		INVERT EXPERIMENTAL GRAD SCALE LENGTHS
	DO 125 I=1,5
	IF(GRADNBAR(I).GT.0.0) GRADNBAR(I) = 1./GRADNBAR(I) 
	IF(GRADTBAR(I).GT.0.0) GRADTBAR(I) = 1./GRADTBAR(I)
	IF(GRADTEBAR(I).GT.0.0) GRADTEBAR(I) = 1./GRADTEBAR(I)
125	CONTINUE
	
C	CONSTRUCT EXPERIMENTAL TRANS BARRIER PARAMETERS
	XNTBEX = 0.5*(XNPEDEX + XNSEPEX)
	TTBEX  = 0.25*(TPEDEXE+TPEDEXI + TSEPEXE+TSEPEXI)
	TTBEXE = 0.5*(TPEDEXE + TSEPEXE)
	TTBEXI = 0.5*(TPEDEXI + TSEPEXI)  

C	SET CHIR IN TB IN CGS FOR GROWTH RATE CALCULATION
	
C	SET CHIR IN TB FOR PREDICTIVE MODE
							 
	CHITBI = 1.E4*CHIREDGE/(1.+CHEI)
	CHITBE = 1.E4*CHIREDGE/(1.+(1./CHEI))
	CHIRTBI = 1.E4*CHIRTBI
	CHIRTBE = 1.E4*CHIRTBE
 
C	CONFINEMENT ADJUSTMENT FACTOR
C		TRIANGULARITY DEPENDENCE OF H89, OSBORNE ET AL, PPCF 42,A175,2000
C	IF(JJOPTPED.EQ.10) HCONF = HCONF + 0.4*TRIANG	
	IF(IOPTTAUE.EQ.89) H89 = HCONF	
	IF(IOPTTAUE.EQ.98) H98 = HCONF	
	
c*****************************************************************
c                             OPTIONS
c     ioptFIOL - NBI ion orbit loss. requires more profiles
c     solovev - more realistic flux surface model for IOL
c     ioptxtran - xtransport (non-operational 9/1/15)
c     ioptionorb - IOL
c     ioptsol - IOL extended into scrape off layer (non-operational 9/1/15)
c     ioptedge - do edgecalc analysis
c     RLOSSIOL - fraction of IOL particles that remain lost
c     NBIspin - fraction of NBI momentum deposited 
c               co-current=1, ctr-current=1.2 are good estimates
c     NBIeloss - fraction of energy lost from NBI IOL
c               co-current=0, ctr-current=0.5 are good estimates
c*****************************************************************     
	if(jjoptped.eq.11) then
	ioptsoln = 1
	ioptpedn = 1
	endif 
	IF(JJOPTPED.EQ.9) IOPTSOLN = 1	
      ioptDdata = 1
	ioptFIOL = 0    	
	solovev = 0
 	ioptxtran = 0
 	ioptionorb = 1
	ioptsol = 0 
	ioptedge =1
	
	RLOSSIOL = 0.5
	NBIspin = 1.0
	NBIeloss = 0.0
c******************************************************
c    Begin shot specific input data
c******************************************************
c	 shot:149468 @ 1905, type: L-mode,  Date: 9/09/2015

c     Electron density [#/m^3]- drvr_pedxax.pro (after profile fitting) 
      exne(1) = 1.07e+19
      exne(2) = 1.06e+19
      exne(3) = 1.04e+19
      exne(4) = 1.03e+19
      exne(5) = 1.01e+19
      exne(6) = 9.91e+18
      exne(7) = 9.77e+18
      exne(8) = 9.58e+18
      exne(9) = 9.39e+18
      exne(10) = 9.22e+18
      exne(11) = 9.03e+18
      exne(12) = 8.83e+18
      exne(13) = 8.67e+18
      exne(14) = 8.47e+18
      exne(15) = 8.27e+18
      exne(16) = 8.11e+18
      exne(17) = 7.90e+18
      exne(18) = 7.74e+18
      exne(19) = 7.53e+18
      exne(20) = 7.32e+18
      exne(21) = 7.14e+18
      exne(22) = 6.93e+18
      exne(23) = 6.72e+18
      exne(24) = 6.55e+18
      exne(25) = 6.34e+18
c     Electron d 0.75E19ensity gradient scale length [m]-
c     drvr_pedxax.pro (after profile fitting)
c     xlne = [(1/n)dn/dr]^-1   
      xlne(1) = 0.96
      xlne(2) = 0.94
      xlne(3) = 0.92
      xlne(4) = 0.91
      xlne(5) = 0.89
      xlne(6) = 0.87
      xlne(7) = 0.86
      xlne(8) = 0.84
      xlne(9) = 0.82
      xlne(10) = 0.80
      xlne(11) = 0.78
      xlne(12) = 0.76
      xlne(13) = 0.75
      xlne(14) = 0.73
      xlne(15) = 0.71
      xlne(16) = 0.69
      xlne(17) = 0.67
      xlne(18) = 0.65
      xlne(19) = 0.63
      xlne(20) = 0.61
      xlne(21) = 0.60
      xlne(22) = 0.58
      xlne(23) = 0.56
      xlne(24) = 0.54
      xlne(25) = 0.52
c     Toroidal V .015elocity gradient scale length [m]- 
c     drvr_pedxax.pro (after profile fitting)
c     exlv = [(1/v)dv/dr]^-1	
      exlv(1) = 0.64
      exlv(2) = 0.64
      exlv(3) = 0.64
      exlv(4) = 0.64
      exlv(5) = 0.63
      exlv(6) = 0.62
      exlv(7) = 0.62
      exlv(8) = 0.58
      exlv(9) = 0.53
      exlv(10) = 0.49
      exlv(11) = 0.44
      exlv(12) = 0.38
      exlv(13) = 0.33
      exlv(14) = 0.28
      exlv(15) = 0.24
      exlv(16) = 0.21
      exlv(17) = 0.18
      exlv(18) = 0.15
      exlv(19) = 0.14
      exlv(20) = 0.13
      exlv(21) = 0.11
      exlv(22) = 0.105
      exlv(23) = 0.099
      exlv(24) = 0.094
	exlv(25) =  0.05
c     Electron temperature gradient scale length [m] - 
c     drvr_pedxax.pro (after profile fitting)
c     exlte = [(1/Te)dTe/dr]^-1
      exlte(1) = 0.29
      exlte(2) = 0.27
      exlte(3) = 0.26
      exlte(4) = 0.25
      exlte(5) = 0.23
      exlte(6) = 0.22
      exlte(7) = 0.21
      exlte(8) = 0.20
      exlte(9) = 0.18
      exlte(10) = 0.17
      exlte(11) = 0.16
      exlte(12) = 0.15
      exlte(13) = 0.14
      exlte(14) = 0.13
      exlte(15) = 0.12
      exlte(16) = 0.11
      exlte(17) = 0.10
      exlte(18) = 0.09
      exlte(19) = 0.08
      exlte(20) = 0.07
      exlte(21) = 0.06
      exlte(22) = 0.05
      exlte(23) = 0.04
      exlte(24) = 0.03
      exlte(25) = 0.02
c     Ion temperature [eV]- drvr_pedxax.pro (after profile fitting)	 
      xti(1) = 721.
      xti(2) = 710.
      xti(3) = 700.
      xti(4) = 691.
      xti(5) = 680.
      xti(6) = 670.
      xti(7) = 661.
      xti(8) = 650.
      xti(9) = 640.
      xti(10) = 632.
      xti(11) = 621.
      xti(12) = 611.
      xti(13) = 603.
      xti(14) = 592.
      xti(15) = 582.
      xti(16) = 574.
      xti(17) = 564.
      xti(18) = 555.
      xti(19) = 545.
      xti(20) = 535.
      xti(21) = 527.
      xti(22) = 517.
      xti(23) = 507.
      xti(24) = 499.
      xti(25) = 489.
c     Ion temperature gradient scale length [m]- 
c     drvr_pedxax.pro (after profile fitting)
c     exlti = [(1/Ti)dTi/dr]^-1
      exlti(1) = 0.66
      exlti(2) = 0.65
      exlti(3) = 0.64
      exlti(4) = 0.63
      exlti(5) = 0.62
      exlti(6) = 0.61
      exlti(7) = 0.60
      exlti(8) = 0.59
      exlti(9) = 0.58
      exlti(10) = 0.57
      exlti(11) = 0.56
      exlti(12) = 0.55
      exlti(13) = 0.54
      exlti(14) = 0.53
      exlti(15) = 0.52
      exlti(16) = 0.51
      exlti(17) = 0.50
      exlti(18) = 0.49
      exlti(19) = 0.48
      exlti(20) = 0.47
      exlti(21) = 0.46
      exlti(22) = 0.45
      exlti(23) = 0.44
      exlti(24) = 0.43
      exlti(25) = 0.42
c     Electron te .015mperature [eV]- drvr_pedxax.pro (after profile fitting)				
      xte(1) = 343.06
      xte(2) = 329.30
      xte(3) = 315.55
      xte(4) = 304.09
      xte(5) = 290.33
      xte(6) = 276.58
      xte(7) = 265.12
      xte(8) = 252.18
      xte(9) = 239.64
      xte(10) = 229.19
      xte(11) = 216.66
      xte(12) = 204.12
      xte(13) = 193.68
      xte(14) = 181.14
      xte(15) = 169.04
      xte(16) = 159.13
      xte(17) = 147.25
      xte(18) = 137.34
      xte(19) = 126.10
      xte(20) = 114.87
      xte(21) = 105.50
      xte(22) = 94.26
      xte(23) = 83.03
      xte(24) = 73.66
      xte(25) = 62.42
c     safety factor [1]- EFITtools --> profiles_1_mse
c     need to manually input for GTEDGE rho vector
	qedge(1) = 3.21
	qedge(2) = 3.26
	qedge(3) = 3.34
	qedge(4) = 3.39
	qedge(5) = 3.47
	qedge(6) = 3.55
	qedge(7) = 3.61
	qedge(8) = 3.69
	qedge(9) = 3.78
	qedge(10) = 3.87
	qedge(11) = 3.96
	qedge(12) = 4.06
	qedge(13) = 4.15
	qedge(14) = 4.28
	qedge(15) = 4.39
	qedge(16) = 4.50
	qedge(17) = 4.65
	qedge(18) = 4.76
	qedge(19) = 4.94
	qedge(20) = 5.12
	qedge(21) = 5.36
	qedge(22) = 5.62
	qedge(23) = 5.93
	qedge(24) = 6.11
	qedge(25) = 6.28
c     Radial Electric Field [V/m]- drvr_pedxax.pro (after profile fitting)
      erex(1) = 1.72e+04
      erex(2) = 1.69e+04
      erex(3) = 1.66e+04
      erex(4) = 1.64e+04
      erex(5) = 1.61e+04
      erex(6) = 1.58e+04
      erex(7) = 1.56e+04
      erex(8) = 1.53e+04
      erex(9) = 1.50e+04
      erex(10) = 1.47e+04
      erex(11) = 1.44e+04
      erex(12) = 1.41e+04
      erex(13) = 1.38e+04
      erex(14) = 1.34e+04
      erex(15) = 1.29e+04
      erex(16) = 1.24e+04
      erex(17) = 1.19e+04
      erex(18) = 1.14e+04
      erex(19) = 1.06e+04
      erex(20) = 9.78e+03
      erex(21) = 9.09e+03
      erex(22) = 8.20e+03
      erex(23) = 7.23e+03
      erex(24) = 6.43e+03
      erex(25) = 5.46e+03
c     Carbon Poloidal Velocity [m/s]- drvr_pedxax.pro (after profile fitting)
c	rh sign convention + is down at outer midplane
      vthexp(1) = -1.69e+03
      vthexp(2) = -1.71e+03
      vthexp(3) = -1.73e+03
      vthexp(4) = -1.75e+03
      vthexp(5) = -1.77e+03
      vthexp(6) = -1.78e+03
      vthexp(7) = -1.80e+03
      vthexp(8) = -1.82e+03
      vthexp(9) = -1.84e+03
      vthexp(10) = -1.86e+03
      vthexp(11) = -1.88e+03
      vthexp(12) = -1.90e+03
      vthexp(13) = -1.91e+03
      vthexp(14) = -1.93e+03
      vthexp(15) = -1.95e+03
      vthexp(16) = -1.97e+03
      vthexp(17) = -1.99e+03
      vthexp(18) = -2.01e+03
      vthexp(19) = -2.03e+03
      vthexp(20) = -2.05e+03
      vthexp(21) = -2.06e+03
      vthexp(22) = -2.08e+03
      vthexp(23) = -2.10e+03
      vthexp(24) = -2.11e+03
      vthexp(25) = -2.13e+03
c     Carbon Toroidal Velocity [m/s]- drvr_pedxax.pro (after profile fitting)
      torv(1) =  5.71e+04
      torv(2) =  5.66e+04
      torv(3) =  5.60e+04
      torv(4) =  5.56e+04
      torv(5) =  5.51e+04
      torv(6) =  5.45e+04
      torv(7) =  5.41e+04
      torv(8) =  5.36e+04
      torv(9) =  5.31e+04
      torv(10) =  5.26e+04
      torv(11) =  5.21e+04
      torv(12) =  5.14e+04
      torv(13) =  5.08e+04
      torv(14) =  5.01e+04
      torv(15) =  4.91e+04
      torv(16) =  4.81e+04
      torv(17) =  4.70e+04
      torv(18) =  4.60e+04
      torv(19) =  4.42e+04
      torv(20) =  4.24e+04
      torv(21) =  4.09e+04
      torv(22) =  3.89e+04
      torv(23) =  3.66e+04
      torv(24) =  3.48e+04
      torv(25) =  3.26e+04
c     Density time derivative correction [#/m^3-s]- drvr_pedtimed.pro (after profile fitting)
	dlnn_dt(1) = 6.178e-1
	dlnn_dt(2) = 6.243e-1
	dlnn_dt(3) = 6.313e-1
	dlnn_dt(4) = 6.352e-1
	dlnn_dt(5) = 6.341e-1
	dlnn_dt(6) = 6.309e-1
	dlnn_dt(7) = 6.275e-1
	dlnn_dt(8) = 6.228e-1
	dlnn_dt(9) = 6.173e-1
	dlnn_dt(10) = 6.108e-1
	dlnn_dt(11) = 6.169e-1
	dlnn_dt(12) = 6.210e-1
	dlnn_dt(13) = 5.994e-1
	dlnn_dt(14) = 5.449e-1
	dlnn_dt(15) = 4.538e-1
	dlnn_dt(16) = 3.103e-1
	dlnn_dt(17) = 1.115e-1
	dlnn_dt(18) = -8.607e-2
	dlnn_dt(19) = -4.165e-1
	dlnn_dt(20) = -7.858e-1
	dlnn_dt(21) = -9.755e-1
	dlnn_dt(22) = -1.042e+0
	dlnn_dt(23) = -1.059e+0
	dlnn_dt(24) = -1.069e+0
	dlnn_dt(25) = -1.079e+0
c     Electron temperature time derivative correction [eV/s]- drvr_pedtimed.pro (after profile fitting)
	dlnwe_dt(1) = 6.876e-1
	dlnwe_dt(2) = 6.794e-1
	dlnwe_dt(3) = 6.670e-1
	dlnwe_dt(4) = 6.481e-1
	dlnwe_dt(5) = 6.208e-1
	dlnwe_dt(6) = 5.872e-1
	dlnwe_dt(7) = 5.544e-1
	dlnwe_dt(8) = 5.080e-1
	dlnwe_dt(9) = 4.522e-1
	dlnwe_dt(10) = 3.857e-1
	dlnwe_dt(11) = 3.188e-1
	dlnwe_dt(12) = 2.515e-1
	dlnwe_dt(13) = 1.343e-1
	dlnwe_dt(14) = -2.072e-2
	dlnwe_dt(15) = -2.004e-1
	dlnwe_dt(16) = -3.920e-1
	dlnwe_dt(17) = -5.642e-1
	dlnwe_dt(18) =  -6.693e-1
	dlnwe_dt(19) = -8.212e-1
	dlnwe_dt(20) = -1.038e+0
	dlnwe_dt(21) = -1.215e+0
	dlnwe_dt(22) = -1.488e+0
	dlnwe_dt(23) = -1.866e+0
	dlnwe_dt(24) = -1.463e+0
	dlnwe_dt(25) = -3.041e+0
c     Ion temperature time derivative correction [eV/s]- drvr_pedtimed.pro (after profile fitting)
	dlnwi_dt(1) = 3.169e-1
	dlnwi_dt(2) = 3.247e-1
	dlnwi_dt(3) = 3.352e-1
	dlnwi_dt(4) = 3.444e-1
	dlnwi_dt(5) = 3.508e-1
	dlnwi_dt(6) = 3.577e-1
	dlnwi_dt(7) = 3.649e-1
	dlnwi_dt(8) = 3.757e-1
	dlnwi_dt(9) = 3.892e-1
	dlnwi_dt(10) = 4.058e-1
	dlnwi_dt(11) = 4.401e-1
	dlnwi_dt(12) = 4.712e-1
	dlnwi_dt(13) = 4.861e-1
	dlnwi_dt(14) = 4.733e-1
	dlnwi_dt(15) = 4.308e-1
	dlnwi_dt(16) = 3.438e-1
	dlnwi_dt(17) = 2.114e-1
	dlnwi_dt(18) = 7.712e-2
	dlnwi_dt(19) = -1.813e-1
	dlnwi_dt(20) = -4.780e-1
	dlnwi_dt(21) = -5.708e-1
	dlnwi_dt(22) = -5.229e-1
	dlnwi_dt(23) = -4.343e-1
	dlnwi_dt(24) = -3.002e-1
	dlnwi_dt(25) = -1.475e-1

	if(ioptDdata.eq.1) then
c     Deuterium Toroidal Velocity [m/s]- drvr_pedxax.pro OR from personal contact (after profile fitting)
      vtord(1) = 7.41e+04
      vtord(2) = 7.43e+04
      vtord(3) = 7.45e+04
      vtord(4) = 7.47e+04
      vtord(5) = 7.52e+04
      vtord(6) = 7.57e+04
      vtord(7) = 7.62e+04
      vtord(8) = 7.69e+04
      vtord(9) = 7.77e+04
      vtord(10) = 7.84e+04
      vtord(11) = 7.92e+04
      vtord(12) = 8.03e+04
      vtord(13) = 8.11e+04
      vtord(14) = 8.22e+04
      vtord(15) = 8.34e+04
      vtord(16) = 8.44e+04
      vtord(17) = 8.56e+04
      vtord(18) = 8.67e+04
      vtord(19) = 8.80e+04
      vtord(20) = 8.94e+04
      vtord(21) = 9.05e+04
      vtord(22) = 9.19e+04
      vtord(23) = 9.34e+04
      vtord(24) = 9.46e+04
      vtord(25) = 9.60e+04
c     Deuterium Poloidal Velocity [m/s]- drvr_pedxax.pro OR from personal contact (after profile fitting)
      vpold(1) = -5.23e+02
      vpold(2) = -6.08e+02
      vpold(3) = -6.93e+02
      vpold(4) = -7.73e+02
      vpold(5) = -9.07e+02
      vpold(6) = -1.04e+03
      vpold(7) = -1.15e+03
      vpold(8) = -1.32e+03
      vpold(9) = -1.50e+03
      vpold(10) = -1.65e+03
      vpold(11) = -1.84e+03
      vpold(12) = -2.09e+03
      vpold(13) = -2.29e+03
      vpold(14) = -2.53e+03
      vpold(15) = -2.83e+03
      vpold(16) = -3.10e+03
      vpold(17) = -3.43e+03
      vpold(18) = -3.70e+03
      vpold(19) = -4.10e+03
      vpold(20) = -4.50e+03
      vpold(21) = -4.84e+03
      vpold(22) = -5.30e+03
      vpold(23) = -5.84e+03
      vpold(24) = -6.28e+03
      vpold(25) = -6.81e+03
	endif
	
	if (ioptFIOL.eq.0) goto	500
c*************************************************
c     Input for neutral beam fast IOL calculation
c     Requires full profiles for rho [0,1]
c     Requires output data from NBeams 
c**************************************************
c	 shot:123302 @ 2600,  type:H-mode,  date:8/27/2015

c     Radial electric field [V/m] for rho [0,1], deltarho = 0.02
c     drvr_pedxax.pro (after profile fitting)
      erextot(1) = 10.9806E3
      erextot(2) =  7.9585E3
      erextot(3) = 10.3835E3
      erextot(4) = 13.4411E3
      erextot(5) = 16.7147E3
      erextot(6) = 20.8302E3
      erextot(7) = 25.8111E3
      erextot(8) = 29.4480E3
      erextot(9) = 32.8280E3
      erextot(10) = 36.1473E3
      erextot(11) = 38.9778E3
      erextot(12) = 41.5539E3
      erextot(13) = 43.7963E3
      erextot(14) = 45.7465E3
      erextot(15) = 47.4008E3
      erextot(16) = 48.7825E3
      erextot(17) = 49.9089E3
      erextot(18) = 50.7976E3
      erextot(19) = 51.4685E3
      erextot(20) = 51.9619E3
      erextot(21) = 52.2887E3
      erextot(22) = 52.4713E3
      erextot(23) = 52.5497E3
      erextot(24) = 52.5407E3
      erextot(25) = 52.4609E3
      erextot(26) = 52.3471E3
      erextot(27) = 52.2221E3
      erextot(28) = 52.0926E3
      erextot(29) = 51.9752E3
      erextot(30) = 51.8837E3
      erextot(31) = 51.8079E3
      erextot(32) = 51.7716E3
      erextot(33) = 51.7336E3
      erextot(34) = 51.6559E3
      erextot(35) = 51.5057E3
      erextot(36) = 51.1843E3
      erextot(37) = 50.7504E3
      erextot(38) = 50.1952E3
      erextot(39) = 49.5770E3
      erextot(40) = 48.5530E3
      erextot(41) = 46.9796E3
      erextot(42) = 44.5705E3
      erextot(43) = 40.4896E3
      erextot(44) = 34.3786E3
      erextot(45) = 25.6980E3
      erextot(46) = 15.4734E3
      erextot(47) =  4.8638E3
      erextot(48) = -4.3835E3
      erextot(49) = -8.5847E3
      erextot(50) =  1.7194E3
      erextot(51) = 15.6930E3

c     Neutral beam deposition profile (hofr1) from NBeams (Dr. John Mandrekas)
c     units [#/s] - need to convert to GTEDGE rho coordinates using MATLAB script
c     nbeams2gtedge.m	from T.M.Wilks
      NBdep1(1) =    5.3869
      NBdep1(2) =   11.1940
      NBdep1(3) =    7.4459
      NBdep1(4) =    6.6674
      NBdep1(5) =    6.3071
      NBdep1(6) =    6.0618
      NBdep1(7) =    5.8731
      NBdep1(8) =    5.6857
      NBdep1(9) =    5.4993
      NBdep1(10) =    5.2954
      NBdep1(11) =    5.0649
      NBdep1(12) =    4.8049
      NBdep1(13) =    4.4937
      NBdep1(14) =    4.0969
      NBdep1(15) =    3.6472
      NBdep1(16) =    3.3563
      NBdep1(17) =    3.1111
      NBdep1(18) =    2.8892
      NBdep1(19) =    2.6873
      NBdep1(20) =    2.4922
      NBdep1(21) =    2.3050
      NBdep1(22) =    2.1243
      NBdep1(23) =    1.9518
      NBdep1(24) =    1.7818
      NBdep1(25) =    1.6218
      NBdep1(26) =    1.4669
      NBdep1(27) =    1.3189
      NBdep1(28) =    1.1796
      NBdep1(29) =    1.0485
      NBdep1(30) =    0.9267
      NBdep1(31) =    0.8146
      NBdep1(32) =    0.7123
      NBdep1(33) =    0.6197
      NBdep1(34) =    0.5363
      NBdep1(35) =    0.4625
      NBdep1(36) =    0.3979
      NBdep1(37) =    0.3417
      NBdep1(38) =    0.2934
      NBdep1(39) =    0.2526
      NBdep1(40) =    0.2184
      NBdep1(41) =    0.1902
      NBdep1(42) =    0.1670
      NBdep1(43) =    0.1486
      NBdep1(44) =    0.1341
      NBdep1(45) =    0.1230
      NBdep1(46) =    0.1144
      NBdep1(47) =    0.1080
      NBdep1(48) =    0.1032
      NBdep1(49) =    0.0992
      NBdep1(50) =    0.0964
      NBdep1(51) =    0.0000

      NBdep2(1) =   4.2856
      NBdep2(2) =   8.9138
      NBdep2(3) =   5.9510
      NBdep2(4) =   5.3598
      NBdep2(5) =   5.1118
      NBdep2(6) =   4.9613
      NBdep2(7) =   4.8641
      NBdep2(8) =   4.7709
      NBdep2(9) =   4.6820
      NBdep2(10) =   4.5792
      NBdep2(11) =   4.4526
      NBdep2(12) =   4.2982
      NBdep2(13) =   4.0933
      NBdep2(14) =   3.8071
      NBdep2(15) =   3.4627
      NBdep2(16) =   3.2449
      NBdep2(17) =   3.0579
      NBdep2(18) =   2.8829
      NBdep2(19) =   2.7201
      NBdep2(20) =   2.5555
      NBdep2(21) =   2.3923
      NBdep2(22) =   2.2294
      NBdep2(23) =   2.0702
      NBdep2(24) =   1.9077
      NBdep2(25) =   1.7522
      NBdep2(26) =   1.5979
      NBdep2(27) =   1.4474
      NBdep2(28) =   1.3034
      NBdep2(29) =   1.1658
      NBdep2(30) =   1.0363
      NBdep2(31) =   0.9157
      NBdep2(32) =   0.8044
      NBdep2(33) =   0.7028
      NBdep2(34) =   0.6106
      NBdep2(35) =   0.5283
      NBdep2(36) =   0.4557
      NBdep2(37) =   0.3924
      NBdep2(38) =   0.3377
      NBdep2(39) =   0.2912
      NBdep2(40) =   0.2522
      NBdep2(41) =   0.2200
      NBdep2(42) =   0.1937
      NBdep2(43) =   0.1726
      NBdep2(44) =   0.1562
      NBdep2(45) =   0.1435
      NBdep2(46) =   0.1338
      NBdep2(47) =   0.1267
      NBdep2(48) =   0.1214
      NBdep2(49) =   0.1175
      NBdep2(50) =   0.1149
      NBdep2(51) =   0.0000

      NBdep3(1) =   3.8662
      NBdep3(2) =   8.0454
      NBdep3(3) =   5.3811
      NBdep3(4) =   4.8607
      NBdep3(5) =   4.6546
      NBdep3(6) =   4.5394
      NBdep3(7) =   4.4764
      NBdep3(8) =   4.4184
      NBdep3(9) =   4.3662
      NBdep3(10) =   4.3019
      NBdep3(11) =   4.2151
      NBdep3(12) =   4.1013
      NBdep3(13) =   3.9379
      NBdep3(14) =   3.6950
      NBdep3(15) =   3.3921
      NBdep3(16) =   3.2033
      NBdep3(17) =   3.0395
      NBdep3(18) =   2.8832
      NBdep3(19) =   2.7359
      NBdep3(20) =   2.5834
      NBdep3(21) =   2.4296
      NBdep3(22) =   2.2736
      NBdep3(23) =   2.1193
      NBdep3(24) =   1.9593
      NBdep3(25) =   1.8052
      NBdep3(26) =   1.6506
      NBdep3(27) =   1.4986
      NBdep3(28) =   1.3523
      NBdep3(29) =   1.2117
      NBdep3(30) =   1.0786
      NBdep3(31) =   0.9542
      NBdep3(32) =   0.8390
      NBdep3(33) =   0.7337
      NBdep3(34) =   0.6377
      NBdep3(35) =   0.5520
      NBdep3(36) =   0.4763
      NBdep3(37) =   0.4102
      NBdep3(38) =   0.3530
      NBdep3(39) =   0.3045
      NBdep3(40) =   0.2638
      NBdep3(41) =   0.2301
      NBdep3(42) =   0.2027
      NBdep3(43) =   0.1808
      NBdep3(44) =   0.1638
      NBdep3(45) =   0.1507
      NBdep3(46) =   0.1408
      NBdep3(47) =   0.1336
      NBdep3(48) =   0.1285
      NBdep3(49) =   0.1250
      NBdep3(50) =   0.1230
      NBdep3(51) =   0.0000

c      cosine of angle between injected NB particle and Bphi (NBeams output)
      fpsi0(1) =  -0.4459
      fpsi0(2) =  -0.4429
      fpsi0(3) =  -0.4399
      fpsi0(4) =  -0.4370
      fpsi0(5) =  -0.4341
      fpsi0(6) =  -0.4311
      fpsi0(7) =  -0.4285
      fpsi0(8) =  -0.4256
      fpsi0(9) =  -0.4229
      fpsi0(10) =  -0.4201
      fpsi0(11) =  -0.4175
      fpsi0(12) =  -0.4149
      fpsi0(13) =  -0.4123
      fpsi0(14) =  -0.4096
      fpsi0(15) =  -0.4071
      fpsi0(16) =  -0.4047
      fpsi0(17) =  -0.4021
      fpsi0(18) =  -0.3997
      fpsi0(19) =  -0.3973
      fpsi0(20) =  -0.3949
      fpsi0(21) =  -0.3925
      fpsi0(22) =  -0.3901
      fpsi0(23) =  -0.3878
      fpsi0(24) =  -0.3856
      fpsi0(25) =  -0.3833
      fpsi0(26) =  -0.3811
      fpsi0(27) =  -0.3788
      fpsi0(28) =  -0.3767
      fpsi0(29) =  -0.3746
      fpsi0(30) =  -0.3723
      fpsi0(31) =  -0.3702
      fpsi0(32) =  -0.3683
      fpsi0(33) =  -0.3662
      fpsi0(34) =  -0.3641
      fpsi0(35) =  -0.3621
      fpsi0(36) =  -0.3602
      fpsi0(37) =  -0.3582
      fpsi0(38) =  -0.3562
      fpsi0(39) =  -0.3543
      fpsi0(40) =  -0.3523
      fpsi0(41) =  -0.3505
      fpsi0(42) =  -0.3486
      fpsi0(43) =  -0.3468
      fpsi0(44) =  -0.3450
      fpsi0(45) =  -0.3432
      fpsi0(46) =  -0.3414
      fpsi0(47) =  -0.3396
      fpsi0(48) =  -0.3379
      fpsi0(49) =  -0.3361
      fpsi0(50) =  -0.3345
      fpsi0(51) =  -0.3327
c*************************************************
c     End neutral beam data
c*************************************************
500   continue

c***************************************************************************
c                 EFITtools Data
c    rmajor - major radius [m] - Plasma Equilibrium
c    aminor - minor radius [m] - Plasma Equilibrium
c    elong - elongation [1] - Plasma Equilibrium
c    triang - lower triangularity [1]	- Plasma Equilibrium
c    plasmacur - plasma current [MA] - Plasma Equilibrium
c    B - magnitude of toroidal magnetic field [T] - Plasma Equilibrium
c    bphi - vector toroidal magnetic field [T] - Plasma Equilibrium
c    q95 - safety factor at 95% flux surface [1] - Plasma Equilibrium
c    pbeam - total beam power [MW] - ReviewPlus 'pinj'
c    Rx - radius of x-point [m] - Plasma Equilibrium
c    zx - height of x-point [m] - Plasma Equilibrium
c    Rsep1 - radius of inside strike point [m] - Plasma Equilibrium
c    Rsep2 - radius of outside strike point [m] - Plasma Equilibrium
c    zsep1 - height of inside strike point [m] - Plasma Equilibrium
c    zsep2 - height of outside strike point [m] - Plasma Equilibrium
c    ssi95 - magnetic shear at 95% flux surface [1] - pointnames_vs_time
c    pohmin - ohmic heating power [MW] - pointnames_vs_time
c    fzintrin - impurity fraction [1] - estimate
c    cfzint - impurity fraction in core [1] - estimate
c    cfzinttb - impurity fraction at transport barrier [1] - estimate
c    fuelpf - fueling rate in private flux region [/s] - estimate
c    fuelplout - fueling rate out of divertor plenum [/s]	- estimate
c    fuelplin - fueling rate into divertor plenum [/s] - estimate
c    tauratio - ratio of particle to energy confinement time [1] - estimate
c    delxp - width of x-region [rad] - estimate
c    delxreal - width from inner to outer plenum in SOL [rad] - estimate
c    fheate - ratio of heat flux to particle flux in SOL [1] - estimate
c***************************************************************************
c	149468 @ 4905ms	 L-mode
	rmajor = 1.663
	aminor = .569
	elong = 1.732
	triang = 0.303
	plasmacur = 1.2
   	B = 2.01
	bphi = -2.01
	q95 = 4.086
	pbeam = 5.5
	Rx = 1.263
	zx = 1.127
	Rsep1 = 1.167
	rsep2 = 1.381
	zsep1 = 1.178
	zsep2 = 1.348
	ssi95 = 5.2
	pohmin = 538.
  	fzintrin = 0.03
	cfzint =   0.03
	cfzinttb = 0.03
 	fuelpf = 0.0e21
 	fuelplout = 0.0e19
	fuelplin = 0.0
	fuelmp = 0.0
	tauratio =0.5
 	delxpt = 0.1
 	delxreal = 0.05
	fheate = 0.4

c*******************************************************
c                CALCULATED PARAMTERS
c    use GTEDGE input calcs by T.M.Wilks
c*******************************************************	   	
      xnpedex = 0.98e19
	xnsepex = 0.1e19
	tpedexi = 513.
	tsepexi = 381.
	tpedexe = 258.
	tsepexe = 40.
	widthnx = .11
	widthtex = .1
	widthtix = .1
	gradnbar(3) = .067
	gradnbar(4) = .067
	gradTbar (3) = .34
 	gradTbar (4) = .34
	gradTebar(3) = .068
	gradTebar(4) = .068
	aped = 0.55
	xnctrped = 3.16
	tctrped	 = 9.6
c*************************************************
c              END INPUT DATA
c*************************************************
		
c	---------------temporary ---------------------
	do 1044 n = 1,25
	dlnwi_dt(n)=0.
	dlnwe_dt(n)=0.
	dlnn_dt(n) = 0.
1044	continue		     
 
c	normalize Rich's gsl to FSA geometry
225	se = sqrt(0.5*(1.+(elong**2)))
	do 250 n=1,26
	xlne(n) = se*xlne(n)
	exlte(n) = se*exlte(n)
	exlti(n) = se*exlti(n)	    	
c	qedge(n) = q95
250	continue
c	save experimental impurity poloidal velocity
	rhor(25) = 1.0
      	 do 255, NN=1,24
		  n= 25-NN
		 rhor(n) = rhor(n+1) - delna/(aminor*SQRT(0.5*(1.+ELONG**2)))
255		 continue 
 
	do 275 n = 1, 25
	vpol_imp(n) = vthexp(n)
275	continue
	do 1150 n = 1,24
	shearho(n) =  (qedge(n+1)-qedge(n))/(rhor(n+1)-rhor(n))
	dEdredge(n) = (erex(n+1)-erex(n))/(rhor(n+1)-rhor(n))
1150	CONTINUE
	shearho(25) = shearho(24) 
	dEdredge(25) = dEdredge(24)
c	smooth derivatives
	do 1155 n =2,24
	shearhonew(n) = (shearho(n-1)+shearho(n)+shearho(n+1))/3.
	dEdrnew(n)= (dEdredge(n-1)+dEdredge(n)+dEdredge(n+1))/3.
1155	continue
 	shearhonew(1)=shearho(1)
	shearhonew(25) = shearho(25)
	dEdrnew(1) = dEdredge(1)
	dEdrnew(25) = dEdredge(25)
	do 1160 n=1,25
	shearho(n) = shearhonew(n)
	dEdredge(n) = Dedrnew(n)
1160	continue 


300   return
	end   
	
	
