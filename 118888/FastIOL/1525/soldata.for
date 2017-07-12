      
	  SUBROUTINE SOLDATA(nbdep1,nbdep2,nbdep3,erextot,fpsi0,fb)

c*****************************************************************
c     Reads data in from text files and defines edge profiles from 
c     the DIII-D database
c*****************************************************************

	INCLUDE 'SOLDIV.FI'

	real ibal,imod,ioptsep,vplas,ioptgeom,dchirat,changeouter,tw,
	1     gammid,xlmid,czd(25),cmid,delsol,rwall,ioptdel,ioptpump,
	2     hxpt,riol,bpiol,btiol,psireal,vpold,vtord
	real nbdep1(51),nbdep2(51),nbdep3(51), erextot(51), fpsi0(51),
	1    fb(3)

	
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
      ioptDdata = 0
	ioptFIOL = 1    	
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
c	 shot:118888 @ 1525, type: L-mode,  Date: 06/16/2004

c     Electron density [#/m^3]- drvr_pedxax.pro (after profile fitting) 
	  EXNE(1) = 2.02E19 
	  EXNE(2) = 1.99E19
	  EXNE(3) = 1.96E19
	  EXNE(4) = 1.93E19
	  EXNE(5) = 1.89E19
	  EXNE(6) = 1.86E19
	  EXNE(7) = 1.83E19
	  EXNE(8) = 1.79E19
	  EXNE(9) = 1.75E19
	  EXNE(10) = 1.71E19
	  EXNE(11) = 1.67E19
	  EXNE(12) = 1.63E19
	  EXNE(13) = 1.59E19
	  EXNE(14) = 1.54E19
	  EXNE(15) = 1.49E19
	  EXNE(16) = 1.45E19
	  EXNE(17) = 1.39E19
	  EXNE(18) = 1.34E19 
	  EXNE(19) = 1.29E19
	  EXNE(20) = 1.23E19
	  EXNE(21) = 1.16E19
	  EXNE(22) = 1.08E19
	  EXNE(23) = 9.84E18
	  EXNE(24) = 8.62E18
	  EXNE(25) = 7.17E18
c     Electron density gradient scale length [m]-
c     drvr_pedxax.pro (after profile fitting)
c     xlne = [(1/n)dn/dr]^-1  
	  XLNE(1) = 0.182
	  XLNE(2) = 0.171 
	  XLNE(3) = 0.161
	  XLNE(4) = 0.152
	  XLNE(5) = 0.142
	  XLNE(6) = 0.133
	  XLNE(7) = 0.124
	  XLNE(8) = 0.116
	  XLNE(9) = 0.109
	  XLNE(10) = 0.101
	  XLNE(11) = 0.942e-1
	  XLNE(12) = 0.870e-1
	  XLNE(13) = 0.809e-1
	  XLNE(14) = 0.741e-1
	  XLNE(15) = 0.678e-1
	  XLNE(16) = 0.625e-1
	  XLNE(17) = 0.573e-1
	  XLNE(18) = 0.523e-1
	  XLNE(19) = 0.460e-2
	  XLNE(20) = 0.383e-2
	  XLNE(21) = 0.300e-2
	  XLNE(22) = 0.217e-2
	  XLNE(23) = 0.146e-2
	  XLNE(24) = 0.960e-2
	  XLNE(25) = 0.682e-2
c     Toroidal Velocity gradient scale length [m]- 
c     drvr_pedxax.pro (after profile fitting)
c     exlv = [(1/v)dv/dr]^-1	
	  exlv(1) = 0.830e-1
	  exlv(2) = .845e-1
	  exlv(3) = .876e-1
	  exlv(4) = .924e-1
	  exlv(5) = .100
	  exlv(6) = .112
	  exlv(7) = .129
	  exlv(8) = .152
	  exlv(9) = .181
	  exlv(10) = .218
	  exlv(11) = .264
	  exlv(12) = .317
	  exlv(13) = .377
	  exlv(14) = .423
	  exlv(15) = .442
	  exlv(16) = .426
	  exlv(17) = .378
	  exlv(18) = .317
	  exlv(19) = .251
	  exlv(20) = .192
	  exlv(21) = .144
	  exlv(22) = .109
	  exlv(23) = .814e-1
	  exlv(24) = .621e-1
	  exlv(25) = .509e-1
c     Electron temperature gradient scale length [m] - 
c     drvr_pedxax.pro (after profile fitting)
c     exlte = [(1/Te)dTe/dr]^-1
	  EXLTE(1) = .667e-1
	  EXLTE(2) = .644e-1
	  EXLTE(3) = .620e-1
	  EXLTE(4) = .595e-1
	  EXLTE(5) = .569e-1
	  EXLTE(6) = .543e-1
	  EXLTE(7) = .514e-1
	  EXLTE(8) = .486e-1
	  EXLTE(9) = .456e-1
	  EXLTE(10) = .426e-1
	  EXLTE(11) = .394e-1
	  EXLTE(12) = .360e-1
	  EXLTE(13) = .328e-1
	  EXLTE(14) = .294e-1
	  EXLTE(15) = .262e-1
	  EXLTE(16) = .235e-1
	  EXLTE(17) = .211e-1
	  EXLTE(18) = .190e-1
	  EXLTE(19) = .170e-1
	  EXLTE(20) = .150e-1
	  EXLTE(21) = .133e-1
	  EXLTE(22) = .120e-1
	  EXLTE(23) = .109e-1
	  EXLTE(24) = .104e-1
	  EXLTE(25) = .102e-1
c     Ion temperature [eV]- drvr_pedxax.pro (after profile fitting)	 
	  XTI(1) = 294.
	  XTI(2) = 287.
	  XTI(3) = 280.
	  XTI(4) = 274.
	  XTI(5) = 267.
	  XTI(6) = 261.
	  XTI(7) = 255.
	  XTI(8) = 249.
	  XTI(9) = 244.
	  XTI(10) = 238.
	  XTI(11) = 233.
	  XTI(12) = 228.
	  XTI(13) = 223.
	  XTI(14) = 218.
	  XTI(15) = 213.
	  XTI(16) = 209.
	  XTI(17) = 204.
	  XTI(18) = 200.
	  XTI(19) = 196.
	  XTI(20) = 192.
	  XTI(21) = 189.
	  XTI(22) = 185.
	  XTI(23) = 182.
	  XTI(24) = 178.
	  XTI(25) = 175.
c     Ion temperature gradient scale length [m]- 
c     drvr_pedxax.pro (after profile fitting)
c     exlti = [(1/Ti)dTi/dr]^-1
	  EXLTI(1) = .110
	  EXLTI(2) = .109
	  EXLTI(3) = .108
	  EXLTI(4) = .108
	  EXLTI(5) = .107
	  EXLTI(6) = .106
	  EXLTI(7) = .105
	  EXLTI(8) = .105
	  EXLTI(9) = .104
	  EXLTI(10) = .104
	  EXLTI(11) = .103
	  EXLTI(12) = .102
	  EXLTI(13) = .102
	  EXLTI(14) = .101
	  EXLTI(15) = .101
	  EXLTI(16) = .101
	  EXLTI(17) = .102
	  EXLTI(18) = .103
	  EXLTI(19) = .103
	  EXLTI(20) = .100
	  EXLTI(21) = .966e-1
	  EXLTI(22) = .929e-1
	  EXLTI(23) = .886e-1
	  EXLTI(24) = .857e-1
	  EXLTI(25) = .885e-1
c     Electron temperature [eV]- drvr_pedxax.pro (after profile fitting)					
	  XTE(1) = 240.
	  XTE(2) = 231.
	  XTE(3) = 221.
	  XTE(4) = 212.
	  XTE(5) = 203.
	  XTE(6) = 194.
	  XTE(7) = 185.
	  XTE(8) = 177.
	  XTE(9) = 168.
	  XTE(10) = 159.
	  XTE(11) = 150.
	  XTE(12) = 141.
	  XTE(13) = 132.
	  XTE(14) = 123.
	  XTE(15) = 114.
	  XTE(16) = 104.
	  XTE(17) = 94.9
	  XTE(18) = 85.4
	  XTE(19) = 76.1
	  XTE(20) = 67.1
	  XTE(21) = 58.6
	  XTE(22) = 50.9
	  XTE(23) = 43.9
	  XTE(24) = 37.9
	  XTE(25) = 32.9 
c     safety factor [1]- EFITtools --> profiles_1_mse
c     need to manually input for GTEDGE rho vector
	  qedge(1) = 2.76
	  qedge(2) = 2.86
	  qedge(3) = 2.91
	  qedge(4) = 2.95
	  qedge(5) = 3.00
	  qedge(6) = 3.05
	  qedge(7) = 3.10
	  qedge(8) = 3.15
	  qedge(9) = 3.20
	  qedge(10) = 3.30
	  qedge(11) = 3.35
	  qedge(12) = 3.41
	  qedge(13) = 3.51
	  qedge(14) = 3.57
	  qedge(15) = 3.67
	  qedge(16) = 3.75
	  qedge(17) = 3.82
	  qedge(18) = 3.95
	  qedge(19) = 4.00
	  qedge(20) = 4.17
	  qedge(21) = 4.20
	  qedge(22) = 4.54
	  qedge(23) = 4.76
	  qedge(24) = 4.94
	  qedge(25) = 5.11
c     Radial Electric Field [V/m]- drvr_pedxax.pro (after profile fitting)
	  erex(1) = 5.85e3
	  erex(2) = 5.92e3
	  erex(3) = 5.98e3
	  erex(4) = 6.03e3
	  erex(5) = 6.07e3
	  erex(6) = 6.09e3
	  erex(7) = 6.09e3
	  erex(8) = 6.09e3
	  erex(9) = 6.07e3
	  erex(10) = 6.06e3
	  erex(11) = 6.03e3
	  erex(12) = 5.98e3
	  erex(13) = 5.91e3
	  erex(14) = 5.86e3
	  erex(15) = 5.81e3
	  erex(16) = 5.76e3
	  erex(17) = 5.72e3
	  erex(18) = 5.67e3
	  erex(19) = 5.60e3
	  erex(20) = 5.49e3
	  erex(21) = 5.30e3
	  erex(22) = 5.00e3
	  erex(23) = 4.37e3
	  erex(24) = 3.40e3
	  erex(25) = 2.36e3
c     Carbon Poloidal Velocity [m/s]- drvr_pedxax.pro (after profile fitting)
c	rh sign convention + is down at outer midplane
	  vthexp(1) = -8.64
	  vthexp(2) = 0.191e3
	  vthexp(3) = 0.376e3
	  vthexp(4) = 0.545e3
	  vthexp(5) = 0.694e3
	  vthexp(6) = 0.821e3
	  vthexp(7) = 0.923e3
	  vthexp(8) = 1.00e3
	  vthexp(9) = 1.06e3
	  vthexp(10) = 1.11e3
	  vthexp(11) = 1.14e3
	  vthexp(12) = 1.16e3
	  vthexp(13) = 1.17e3
	  vthexp(14) = 1.18e3
	  vthexp(15) = 1.19e3
	  vthexp(16) = 1.20e3
	  vthexp(17) = 1.21e3
	  vthexp(18) = 1.24e3
	  vthexp(19) = 1.27e3
	  vthexp(20) = 1.32e3
	  vthexp(21) = 1.39e3
	  vthexp(22) = 1.48e3
	  vthexp(23) = 1.59e3
	  vthexp(24) = 1.73e3
	  vthexp(25) = 1.90e3
c     Carbon Toroidal Velocity [m/s]- drvr_pedxax.pro (after profile fitting)
	  torv(1) = 16.7e3
	  torv(2) = 16.1e3
	  torv(3) = 15.7e3
	  torv(4) = 15.2e3
	  torv(5) = 14.8e3
	  torv(6) = 14.5e3
	  torv(7) = 14.2e3
	  torv(8) = 13.9e3 
	  torv(9) = 13.7e3
	  torv(10) = 13.6e3
	  torv(11) = 13.4e3
	  torv(12) = 13.3e3
	  torv(13) = 13.3e3
	  torv(14) = 13.2e3
	  torv(15) = 13.1e3 
	  torv(16) = 13.0e3 
	  torv(17) = 13.0e3 
	  torv(18) = 12.9e3
	  torv(19) = 12.8e3
	  torv(20) = 12.7e3
	  torv(21) = 12.5e3
	  torv(22) = 12.4e3
	  torv(23) = 12.1e3
	  torv(24) = 11.9e3
	  torv(25) = 11.6e3
c     Density time derivative correction [#/m^3-s]- drvr_pedtimed.pro (after profile fitting)	  
	  dlnn_dt(1) = 43.98
	  dlnn_dt(2) = 45.72
	  dlnn_dt(3) = 47.56
	  dlnn_dt(4) = 49.52
	  dlnn_dt(5) = 51.59
	  dlnn_dt(6) = 53.79
	  dlnn_dt(7) = 56.12
	  dlnn_dt(8) = 58.60
	  dlnn_dt(9) = 61.24
	  dlnn_dt(10) = 64.13
	  dlnn_dt(11) = 67.38
	  dlnn_dt(12) = 70.76
	  dlnn_dt(13) = 74.20
	  dlnn_dt(14) = 77.68
	  dlnn_dt(15) = 81.22
	  dlnn_dt(16) = 84.18
	  dlnn_dt(17) = 85.88
	  dlnn_dt(18) = 84.94
	  dlnn_dt(19) = 79.30
	  dlnn_dt(20) = 66.59
	  dlnn_dt(21) = 45.79
	  dlnn_dt(22) = 19.24
	  dlnn_dt(23) = -7.519
	  dlnn_dt(24) = -29.51
	  dlnn_dt(25) = -45.19
c     Electron temperature time derivative correction [eV/s]- drvr_pedtimed.pro (after profile fitting)
	  dlnwe_dt(1) = 75.02 
	  dlnwe_dt(2) = 76.82 
	  dlnwe_dt(3) = 78.66 
	  dlnwe_dt(4) = 80.55 
	  dlnwe_dt(5) = 82.48 
	  dlnwe_dt(6) = 84.47 
	  dlnwe_dt(7) = 86.54 
	  dlnwe_dt(8) = 88.73 
	  dlnwe_dt(9) = 91.07 
	  dlnwe_dt(10) = 93.61 
	  dlnwe_dt(11) = 96.44 
	  dlnwe_dt(12) = 99.65 
	  dlnwe_dt(13) = 103.3 
	  dlnwe_dt(14) = 107.6
	  dlnwe_dt(15) = 112.4
	  dlnwe_dt(16) = 117.4
	  dlnwe_dt(17) = 120.5
	  dlnwe_dt(18) = 115.8
	  dlnwe_dt(19) = 91.87
	  dlnwe_dt(20) = 46.86
	  dlnwe_dt(21) = 6.702
	  dlnwe_dt(22) = -12.77
	  dlnwe_dt(23) = -18.87
	  dlnwe_dt(24) = -20.01
	  dlnwe_dt(25) = -20.32
c     Ion temperature time derivative correction [eV/s]- drvr_pedtimed.pro (after profile fitting)
	  dlnwi_dt(1) = 58.56
	  dlnwi_dt(2) = 59.14
	  dlnwi_dt(3) = 59.61
	  dlnwi_dt(4) = 59.95
	  dlnwi_dt(5) = 60.17
	  dlnwi_dt(6) = 60.24
	  dlnwi_dt(7) = 60.17
	  dlnwi_dt(8) = 59.94
	  dlnwi_dt(9) = 59.53
	  dlnwi_dt(10) = 58.96
	  dlnwi_dt(11) = 58.20
	  dlnwi_dt(12) = 57.24
	  dlnwi_dt(13) = 56.06
	  dlnwi_dt(14) = 54.67
	  dlnwi_dt(15) = 53.05
	  dlnwi_dt(16) = 51.20
	  dlnwi_dt(17) = 49.09 
	  dlnwi_dt(18) = 46.72 
	  dlnwi_dt(19) = 44.09
	  dlnwi_dt(20) = 41.17
	  dlnwi_dt(21) = 37.98
	  dlnwi_dt(22) = 34.48
	  dlnwi_dt(23) = 30.87
	  dlnwi_dt(24) = 28.31
	  dlnwi_dt(25) = 28.34
	  
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
c	 shot:118888 @ 1525,  type:L-mode,  date:06/16/2004

c     Radial electric field [V/m] for rho [0,1], deltarho = 0.02
c     drvr_pedxax.pro (after profile fitting)
      erextot(1) = 1.3811026e3
      erextot(2) = 1.6878563e3
      erextot(3) = 2.2008715e3
      erextot(4) = 2.8072406e3
      erextot(5) = 3.5068314e3
      erextot(6) = 4.3862773e3
      erextot(7) = 5.3568017e3
      erextot(8) = 6.0836103e3
      erextot(9) = 6.8420674e3
      erextot(10) = 7.5404307e3
      erextot(11) = 8.1828820e3
      erextot(12) = 8.7685723e3
      erextot(13) = 9.2960532e3
      erextot(14) = 9.7647542e3
      erextot(15) = 10.174298e3
      erextot(16) = 10.523786e3
      erextot(17) = 10.798316e3
      erextot(18) = 11.012339e3
      erextot(19) = 11.166547e3
      erextot(20) = 11.262497e3
      erextot(21) = 11.292489e3
      erextot(22) = 11.226017e3
      erextot(23) = 11.074803e3
      erextot(24) = 10.868811e3
      erextot(25) = 10.594099e3
      erextot(26) = 10.267649e3
      erextot(27) = 9.8895923e3
      erextot(28) = 9.4541429e3
      erextot(29) = 8.9625007e3
      erextot(30) = 8.4285904e3
      erextot(31) = 7.8865273e3
      erextot(32) = 7.3474348e3
      erextot(33) = 6.8077146e3
      erextot(34) = 6.4016696e3
      erextot(35) = 5.9766130e3
      erextot(36) = 5.5240574e3
      erextot(37) = 5.2332914e3
      erextot(38) = 5.0261125e3
      erextot(39) = 4.9292031e3
      erextot(40) = 4.9532804e3
      erextot(41) = 5.0944467e3
      erextot(42) = 5.3357932e3
      erextot(43) = 5.6293424e3
      erextot(44) = 5.8982224e3
      erextot(45) = 6.0682016e3
      erextot(46) = 6.0867769e3
      erextot(47) = 5.9971314e3
      erextot(48) = 5.8116020e3
      erextot(49) = 5.6451405e3
      erextot(50) = 5.1032485e3
      erextot(51) = 2.3576561e3

c     Neutral beam deposition profile (hofr1) from NBeams (Dr. John Mandrekas)
c     units [#/s] - need to convert to GTEDGE rho coordinates using MATLAB script
c     nbeams2gtedge.m	from T.M.Wilks
      NBdep1(1) = .73576E+01
      NBdep1(2) = .14850E+02
      NBdep1(3) = .97788E+01
      NBdep1(4) = .86274E+01
      NBdep1(5) = .79881E+01
      NBdep1(6) = .74904E+01
      NBdep1(7) = .70435E+01
      NBdep1(8) = .66113E+01
      NBdep1(9) = .61841E+01
      NBdep1(10) = .57625E+01
      NBdep1(11) = .43249E+01
      NBdep1(12) = .36337E+01
      NBdep1(13) = .31556E+01
      NBdep1(14) = .27897E+01
      NBdep1(15) = .24954E+01
      NBdep1(16) = .22518E+01
      NBdep1(17) = .20463E+01
      NBdep1(18) = .18704E+01
      NBdep1(19) = .17185E+01
      NBdep1(20) = .15857E+01
      NBdep1(21) = .14689E+01
      NBdep1(22) = .13656E+01
      NBdep1(23) = .12739E+01
      NBdep1(24) = .11919E+01
      NBdep1(25) = .11185E+01
      NBdep1(26) = .10526E+01
      NBdep1(27) = .99328E+00
      NBdep1(28) = .93952E+00
      NBdep1(29) = .89079E+00
      NBdep1(30) = .84634E+00
      NBdep1(31) = .80551E+00
      NBdep1(32) = .76803E+00
      NBdep1(33) = .73347E+00
      NBdep1(34) = .70125E+00
      NBdep1(35) = .67098E+00
      NBdep1(36) = .64251E+00
      NBdep1(37) = .61542E+00
      NBdep1(38) = .58932E+00
      NBdep1(39) = .56400E+00
      NBdep1(40) = .53920E+00
      NBdep1(41) = .51469E+00
      NBdep1(42) = .49021E+00
      NBdep1(43) = .46551E+00
      NBdep1(44) = .44063E+00
      NBdep1(45) = .41600E+00
      NBdep1(46) = .39340E+00
      NBdep1(47) = .38002E+00
      NBdep1(48) = .32943E+00
      NBdep1(49) = .28729E+00
      NBdep1(50) = .22422E+00
      NBdep1(51) = .00000E+00

      NBdep2(1) = .59225E+01
      NBdep2(2) = .11966E+02
      NBdep2(3) = .78972E+01
      NBdep2(4) = .69895E+01
      NBdep2(5) = .64983E+01
      NBdep2(6) = .61249E+01
      NBdep2(7) = .57955E+01
      NBdep2(8) = .54803E+01
      NBdep2(9) = .51700E+01
      NBdep2(10) = .48631E+01
      NBdep2(11) = .37010E+01
      NBdep2(12) = .31508E+01
      NBdep2(13) = .27725E+01
      NBdep2(14) = .24837E+01
      NBdep2(15) = .22518E+01
      NBdep2(16) = .20602E+01
      NBdep2(17) = .18983E+01
      NBdep2(18) = .17593E+01
      NBdep2(19) = .16390E+01
      NBdep2(20) = .15332E+01
      NBdep2(21) = .14396E+01
      NBdep2(22) = .13565E+01
      NBdep2(23) = .12821E+01
      NBdep2(24) = .12150E+01
      NBdep2(25) = .11546E+01
      NBdep2(26) = .11000E+01
      NBdep2(27) = .10506E+01
      NBdep2(28) = .10052E+01
      NBdep2(29) = .96378E+00
      NBdep2(30) = .92568E+00
      NBdep2(31) = .89019E+00
      NBdep2(32) = .85729E+00
      NBdep2(33) = .82661E+00
      NBdep2(34) = .79756E+00
      NBdep2(35) = .76962E+00
      NBdep2(36) = .74299E+00
      NBdep2(37) = .71695E+00
      NBdep2(38) = .69104E+00
      NBdep2(39) = .66505E+00
      NBdep2(40) = .63869E+00
      NBdep2(41) = .61161E+00
      NBdep2(42) = .58347E+00
      NBdep2(43) = .55394E+00
      NBdep2(44) = .52281E+00
      NBdep2(45) = .49007E+00
      NBdep2(46) = .45585E+00
      NBdep2(47) = .42225E+00
      NBdep2(48) = .37552E+00
      NBdep2(49) = .32857E+00
      NBdep2(50) = .26805E+00
      NBdep2(51) = .00000E+00

      NBdep3(1) = .46081E+01
      NBdep3(2) = .93187E+01
      NBdep3(3) = .61631E+01
      NBdep3(4) = .54725E+01
      NBdep3(5) = .51103E+01
      NBdep3(6) = .48434E+01
      NBdep3(7) = .46134E+01
      NBdep3(8) = .43964E+01
      NBdep3(9) = .41841E+01
      NBdep3(10) = .39743E+01
      NBdep3(11) = .30740E+01
      NBdep3(12) = .26560E+01
      NBdep3(13) = .23713E+01
      NBdep3(14) = .21552E+01
      NBdep3(15) = .19824E+01
      NBdep3(16) = .18402E+01
      NBdep3(17) = .17201E+01
      NBdep3(18) = .16168E+01
      NBdep3(19) = .15276E+01
      NBdep3(20) = .14487E+01
      NBdep3(21) = .13786E+01
      NBdep3(22) = .13163E+01
      NBdep3(23) = .12602E+01
      NBdep3(24) = .12092E+01
      NBdep3(25) = .11630E+01
      NBdep3(26) = .11213E+01
      NBdep3(27) = .10833E+01
      NBdep3(28) = .10481E+01
      NBdep3(29) = .10158E+01
      NBdep3(30) = .98590E+00
      NBdep3(31) = .95771E+00
      NBdep3(32) = .93141E+00
      NBdep3(33) = .90669E+00
      NBdep3(34) = .88291E+00
      NBdep3(35) = .85947E+00
      NBdep3(36) = .83689E+00
      NBdep3(37) = .81416E+00
      NBdep3(38) = .79078E+00
      NBdep3(39) = .76653E+00
      NBdep3(40) = .74109E+00
      NBdep3(41) = .71403E+00
      NBdep3(42) = .68493E+00
      NBdep3(43) = .65342E+00
      NBdep3(44) = .61915E+00
      NBdep3(45) = .58200E+00
      NBdep3(46) = .54147E+00
      NBdep3(47) = .49822E+00
      NBdep3(48) = .44734E+00
      NBdep3(49) = .39283E+00
      NBdep3(50) = .32465E+00
      NBdep3(51) = .00000E+00

c      cosine of angle between injected NB particle and Bphi
      fpsi0(1) =  -6.39E-01
      fpsi0(2) =  -6.39E-01
      fpsi0(3) =  -6.39E-01
      fpsi0(4) =  -6.38E-01
      fpsi0(5) =  -6.38E-01
      fpsi0(6) =  -6.37E-01
      fpsi0(7) =  -6.37E-01
      fpsi0(8) =  -6.36E-01
      fpsi0(9) =  -6.35E-01
      fpsi0(10) = -6.34E-01
      fpsi0(11) = -6.31E-01
      fpsi0(12) = -6.29E-01
      fpsi0(13) = -6.26E-01
      fpsi0(14) = -6.24E-01
      fpsi0(15) = -6.22E-01
      fpsi0(16) = -6.19E-01
      fpsi0(17) = -6.17E-01
      fpsi0(18) = -6.14E-01
      fpsi0(19) = -6.12E-01
      fpsi0(20) = -6.10E-01
      fpsi0(21) = -6.07E-01
      fpsi0(22) = -6.05E-01
      fpsi0(23) = -6.02E-01
      fpsi0(24) = -6.00E-01
      fpsi0(25) = -5.98E-01
      fpsi0(26) = -5.95E-01
      fpsi0(27) = -5.93E-01
      fpsi0(28) = -5.91E-01
      fpsi0(29) = -5.89E-01
      fpsi0(30) = -5.87E-01
      fpsi0(31) = -5.85E-01
      fpsi0(32) = -5.83E-01
      fpsi0(33) = -5.81E-01
      fpsi0(34) = -5.80E-01
      fpsi0(35) = -5.78E-01
      fpsi0(36) = -5.77E-01
      fpsi0(37) = -5.76E-01
      fpsi0(38) = -5.75E-01
      fpsi0(39) = -5.74E-01
      fpsi0(40) = -5.74E-01
      fpsi0(41) = -5.74E-01
      fpsi0(42) = -5.74E-01
      fpsi0(43) = -5.76E-01
      fpsi0(44) = -5.79E-01
      fpsi0(45) = -5.83E-01
      fpsi0(46) = -5.91E-01
      fpsi0(47) = -6.10E-01
      fpsi0(48) = -6.02E-01
      fpsi0(49) = -6.01E-01
      fpsi0(50) = -5.83E-01
      fpsi0(51) = -0.00E+00
	  fb(1) = 0.76
	  fb(2) = 0.13
	  fb(3) = 0.11
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
c		118888 @ 1525ms	 L-mode
		rmajor = 1.719
		aminor = 0.598
		elong = 1.8206
		triang = 0.367
		plasmacur = 1.38
   		B = 2.00
		bphi = -2.00
		q95 = 3.572
		pbeam = 4.6
  		betag = 0.25
		Rx = 1.48
		zx = -1.2381
		Rsep1 = 1.21
		rsep2 = 1.57
		zsep1 = -1.366
		zsep2 = -1.366
		ssi95 = 4.07
		pohmin = 1.14
		fzintrin = 0.038
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
		xnpedex = 2.20e19	 
		xnsepex = 7.20e18
		tpedexi = 370.
		tsepexi = 180.
		tpedexe = 340.
		tsepexe = 33.0
		widthnx = .2
		widthtex = .2
		widthtix = .2
		gradnbar(3) = .197
		gradnbar(4) = .197
		gradTbar(3) = .2895
 		gradTbar(4) = .2895
		gradTebar(3) = .1215
		gradTebar(4) = .1215
		aped = 0.588
		xnctrped = 2.18
		tctrped = 4.0986
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
	
	