      
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
c	 shot:118888 @ 1570, type: H-mode,  Date: 06/16/2004

c     Electron density [#/m^3]- drvr_pedxax.pro (after profile fitting) 
	  EXNE(1) = 3.14E19 
	  EXNE(2) = 3.14E19
	  EXNE(3) = 3.14E19
	  EXNE(4) = 3.14E19
	  EXNE(5) = 3.13E19
	  EXNE(6) = 3.13E19
	  EXNE(7) = 3.12E19
	  EXNE(8) = 3.11E19
	  EXNE(9) = 3.11E19
	  EXNE(10) = 3.10E19
	  EXNE(11) = 3.09E19
	  EXNE(12) = 3.09E19
	  EXNE(13) = 3.07E19
	  EXNE(14) = 3.05E19
	  EXNE(15) = 3.03E19
	  EXNE(16) = 2.98E19
	  EXNE(17) = 2.91E19
	  EXNE(18) = 2.78E19 
	  EXNE(19) = 2.58E19
	  EXNE(20) = 2.26E19
	  EXNE(21) = 1.83E19
	  EXNE(22) = 1.35E19
	  EXNE(23) = 8.94E18
	  EXNE(24) = 5.44E18
	  EXNE(25) = 3.11E18
c     Electron density gradient scale length [m]-
c     drvr_pedxax.pro (after profile fitting)
c     xlne = [(1/n)dn/dr]^-1  
	  XLNE(1) = 2.36
	  XLNE(2) = 2.18 
	  XLNE(3) = 2.03
	  XLNE(4) = 1.86
	  XLNE(5) = 1.72
	  XLNE(6) = 1.58
	  XLNE(7) = 1.44
	  XLNE(8) = 1.29
	  XLNE(9) = 1.15
	  XLNE(10) = 0.991
	  XLNE(11) = 0.823
	  XLNE(12) = 0.637
	  XLNE(13) = 0.458
	  XLNE(14) = 0.302
	  XLNE(15) = 0.185
	  XLNE(16) = 0.108
	  XLNE(17) = 0.615e-1
	  XLNE(18) = 0.344e-1
	  XLNE(19) = 0.194e-1
	  XLNE(20) = 0.111e-1
	  XLNE(21) = 0.685e-2
	  XLNE(22) = 0.456e-2
	  XLNE(23) = 0.339e-2
	  XLNE(24) = 0.271e-2
	  XLNE(25) = 0.234e-2
c     Toroidal Velocity gradient scale length [m]- 
c     drvr_pedxax.pro (after profile fitting)
c     exlv = [(1/v)dv/dr]^-1	
	  exlv(1) = 0.110
	  exlv(2) = 0.101
	  exlv(3) = 0.930e-1
	  exlv(4) = 0.849e-1
	  exlv(5) = 0.776e-1
	  exlv(6) = 0.707e-1
	  exlv(7) = 0.644e-1
	  exlv(8) = 0.581e-1
	  exlv(9) = 0.525e-1
	  exlv(10) = 0.469e-1
	  exlv(11) = 0.413e-1
	  exlv(12) = 0.354e-1
	  exlv(13) = 0.299e-1
	  exlv(14) = 0.246e-1
	  exlv(15) = 0.199e-1
	  exlv(16) = 0.161e-1
	  exlv(17) = 0.147e-1
	  exlv(18) = 0.167e-1
	  exlv(19) = 0.335e-1
	  exlv(20) = -0.456e-1
	  exlv(21) = -0.182e-1
	  exlv(22) = -0.168e-1
	  exlv(23) = -0.257e-1
	  exlv(24) = 0.166
	  exlv(25) = 0.113e-1
c     Electron temperature gradient scale length [m] - 
c     drvr_pedxax.pro (after profile fitting)
c     exlte = [(1/Te)dTe/dr]^-1
	  EXLTE(1) = 0.904e-1
	  EXLTE(2) = 0.863e-1
	  EXLTE(3) = 0.826e-1
	  EXLTE(4) = 0.784e-1
	  EXLTE(5) = 0.746e-1
	  EXLTE(6) = 0.708e-1
	  EXLTE(7) = 0.672e-1
	  EXLTE(8) = 0.632e-1
	  EXLTE(9) = 0.596e-1
	  EXLTE(10) = 0.559e-1
	  EXLTE(11) = 0.527e-1
	  EXLTE(12) = 0.490e-1
	  EXLTE(13) = 0.454e-1
	  EXLTE(14) = 0.413e-1
	  EXLTE(15) = 0.365e-1
	  EXLTE(16) = 0.299e-1
	  EXLTE(17) = 0.209e-1
	  EXLTE(18) = 0.115e-1
	  EXLTE(19) = 0.579e-2
	  EXLTE(20) = 0.363e-2
	  EXLTE(21) = 0.364e-2
	  EXLTE(22) = 0.531e-2
	  EXLTE(23) = 0.814e-2
	  EXLTE(24) = 0.960e-2
	  EXLTE(25) = 0.909e-2
c     Ion temperature [eV]- drvr_pedxax.pro (after profile fitting)	 
	  XTI(1) = 512.
	  XTI(2) = 502.
	  XTI(3) = 491.
	  XTI(4) = 481.
	  XTI(5) = 471.
	  XTI(6) = 460.
	  XTI(7) = 449.
	  XTI(8) = 438.
	  XTI(9) = 427.
	  XTI(10) = 416.
	  XTI(11) = 404.
	  XTI(12) = 392.
	  XTI(13) = 380.
	  XTI(14) = 368.
	  XTI(15) = 356.
	  XTI(16) = 344.
	  XTI(17) = 331.
	  XTI(18) = 318.
	  XTI(19) = 306.
	  XTI(20) = 293.
	  XTI(21) = 279.
	  XTI(22) = 266.
	  XTI(23) = 253.
	  XTI(24) = 243.
	  XTI(25) = 238.
c     Ion temperature gradient scale length [m]- 
c     drvr_pedxax.pro (after profile fitting)
c     exlti = [(1/Ti)dTi/dr]^-1
	  EXLTI(1) = 0.130
	  EXLTI(2) = 0.124
	  EXLTI(3) = 0.118
	  EXLTI(4) = 0.112
	  EXLTI(5) = 0.106
	  EXLTI(6) = 0.101
	  EXLTI(7) = 0.961e-1
	  EXLTI(8) = 0.906e-1
	  EXLTI(9) = 0.858e-1
	  EXLTI(10) = 0.810e-1
	  EXLTI(11) = 0.769e-1
	  EXLTI(12) = 0.722e-1
	  EXLTI(13) = 0.679e-1
	  EXLTI(14) = 0.635e-1
	  EXLTI(15) = 0.593e-1
	  EXLTI(16) = 0.558e-1
	  EXLTI(17) = 0.528e-1
	  EXLTI(18) = 0.495e-1
	  EXLTI(19) = 0.457e-1
	  EXLTI(20) = 0.412e-1
	  EXLTI(21) = 0.369e-1
	  EXLTI(22) = 0.325e-1
	  EXLTI(23) = 0.321e-1
	  EXLTI(24) = 0.454e-1
	  EXLTI(25) = -81.393
c     Electron temperature [eV]- drvr_pedxax.pro (after profile fitting)					
	  XTE(1) = 468.
	  XTE(2) = 455.
	  XTE(3) = 442.
	  XTE(4) = 428.
	  XTE(5) = 415.
	  XTE(6) = 402.
	  XTE(7) = 388.
	  XTE(8) = 375.
	  XTE(9) = 361.
	  XTE(10) = 347.
	  XTE(11) = 333.
	  XTE(12) = 319.
	  XTE(13) = 305.
	  XTE(14) = 291.
	  XTE(15) = 275.
	  XTE(16) = 259.
	  XTE(17) = 239.
	  XTE(18) = 210.
	  XTE(19) = 164.
	  XTE(20) = 107.
	  XTE(21) = 63.7
	  XTE(22) = 42.8
	  XTE(23) = 33.6
	  XTE(24) = 28.5
	  XTE(25) = 24.6
c     safety factor [1]- EFITtools --> profiles_1_mse
c     need to manually input for GTEDGE rho vector
	  qedge(1) = 2.83
	  qedge(2) = 2.87
	  qedge(3) = 2.93
	  qedge(4) = 2.98
	  qedge(5) = 3.03
	  qedge(6) = 3.09
	  qedge(7) = 3.14
	  qedge(8) = 3.20
	  qedge(9) = 3.27
	  qedge(10) = 3.33
	  qedge(11) = 3.40
	  qedge(12) = 3.47
	  qedge(13) = 3.53
	  qedge(14) = 3.62
	  qedge(15) = 3.69
	  qedge(16) = 3.79
	  qedge(17) = 3.88
	  qedge(18) = 3.96
	  qedge(19) = 4.07
	  qedge(20) = 4.19
	  qedge(21) = 4.35
	  qedge(22) = 4.55
	  qedge(23) = 4.76
	  qedge(24) = 4.93
	  qedge(25) = 5.08
c     Radial Electric Field [V/m]- drvr_pedxax.pro (after profile fitting)
	  erex(1) = 14.0e3
	  erex(2) = 13.9e3
	  erex(3) = 13.7e3
	  erex(4) = 13.4e3
	  erex(5) = 13.1e3
	  erex(6) = 12.7e3
	  erex(7) = 12.2e3
	  erex(8) = 11.6e3
	  erex(9) = 10.9e3
	  erex(10) = 9.97e3
	  erex(11) = 8.94e3
	  erex(12) = 7.80e3
	  erex(13) = 6.60e3
	  erex(14) = 5.35e3
	  erex(15) = 4.06e3
	  erex(16) = 2.77e3
	  erex(17) = 1.44e3
	  erex(18) = 0.982e-1
	  erex(19) = -2.85e3
	  erex(20) = -9.45e3
	  erex(21) = -17.2e3
	  erex(22) = -22.3e3
	  erex(23) = -23.5e3
	  erex(24) = -18.3e3
	  erex(25) = -5.74e3
c     Carbon Poloidal Velocity [m/s]- drvr_pedxax.pro (after profile fitting)
c	rh sign convention + is down at outer midplane
	  vthexp(1) = -0.360e3
	  vthexp(2) = -0.174e3
	  vthexp(3) = 0.341e1
	  vthexp(4) = 0.163e3
	  vthexp(5) = 0.292e3
	  vthexp(6) = 0.381e3
	  vthexp(7) = 0.418e3
	  vthexp(8) = 0.392e3
	  vthexp(9) = 0.293e3
	  vthexp(10) = 0.113e3
	  vthexp(11) = -0.126e3
	  vthexp(12) = -0.393e3
	  vthexp(13) = -0.656e3
	  vthexp(14) = -0.884e3
	  vthexp(15) = -1.05e3
	  vthexp(16) = -1.11e3
	  vthexp(17) = -1.04e3
	  vthexp(18) = -0.876e3
	  vthexp(19) = -1.65e3
	  vthexp(20) = -4.66e3
	  vthexp(21) = -8.18e3
	  vthexp(22) = -9.89e3
	  vthexp(23) = -8.60e3
	  vthexp(24) = -3.38e3
	  vthexp(25) = 6.74e3
c     Carbon Toroidal Velocity [m/s]- drvr_pedxax.pro (after profile fitting)
	  torv(1) = 38.4e3
	  torv(2) = 37.5e3
	  torv(3) = 36.6e3
	  torv(4) = 35.6e3
	  torv(5) = 34.53e3
	  torv(6) = 33.4e3
	  torv(7) = 32.3e3
	  torv(8) = 31.1e3 
	  torv(9) = 29.8e3
	  torv(10) = 28.5e3
	  torv(11) = 27.1e3
	  torv(12) = 25.6e3
	  torv(13) = 24.0e3
	  torv(14) = 22.2e3
	  torv(15) = 20.2e3
	  torv(16) = 18.0e3
	  torv(17) = 15.8e3
	  torv(18) = 13.8e3
	  torv(19) = 12.6e3
	  torv(20) = 12.5e3
	  torv(21) = 13.4e3
	  torv(22) = 14.9e3
	  torv(23) = 16.1e3
	  torv(24) = 16.6e3
	  torv(25) = 15.6e3
c     Density time derivative correction [#/m^3-s]- drvr_pedtimed.pro (after profile fitting)	  
	  dlnn_dt(1) = 2.02
	  dlnn_dt(2) = 2.07
	  dlnn_dt(3) = 2.12
	  dlnn_dt(4) = 2.17
	  dlnn_dt(5) = 2.22
	  dlnn_dt(6) = 2.28
	  dlnn_dt(7) = 2.33
	  dlnn_dt(8) = 2.39
	  dlnn_dt(9) = 2.45
	  dlnn_dt(10) = 2.51
	  dlnn_dt(11) = 2.58
	  dlnn_dt(12) = 2.64
	  dlnn_dt(13) = 2.71
	  dlnn_dt(14) = 2.77
	  dlnn_dt(15) = 2.83
	  dlnn_dt(16) = 2.88
	  dlnn_dt(17) = 2.91
	  dlnn_dt(18) = 2.89
	  dlnn_dt(19) = 2.80
	  dlnn_dt(20) = 2.56
	  dlnn_dt(21) = 2.07
	  dlnn_dt(22) = 1.14
	  dlnn_dt(23) = -0.492
	  dlnn_dt(24) = -3.14
	  dlnn_dt(25) = -7.01
c     Electron temperature time derivative correction [eV/s]- drvr_pedtimed.pro (after profile fitting)
	  dlnwe_dt(1) = 2.72 
	  dlnwe_dt(2) = 2.76 
	  dlnwe_dt(3) = 2.79 
	  dlnwe_dt(4) = 2.82 
	  dlnwe_dt(5) = 2.85 
	  dlnwe_dt(6) = 2.89 
	  dlnwe_dt(7) = 2.92 
	  dlnwe_dt(8) = 2.95 
	  dlnwe_dt(9) = 2.99 
	  dlnwe_dt(10) = 3.02 
	  dlnwe_dt(11) = 3.07 
	  dlnwe_dt(12) = 3.11 
	  dlnwe_dt(13) = 3.16 
	  dlnwe_dt(14) = 3.21
	  dlnwe_dt(15) = 3.27
	  dlnwe_dt(16) = 3.33
	  dlnwe_dt(17) = 3.36
	  dlnwe_dt(18) = 3.31
	  dlnwe_dt(19) = 3.00
	  dlnwe_dt(20) = 2.10
	  dlnwe_dt(21) = 0.500
	  dlnwe_dt(22) = -0.966
	  dlnwe_dt(23) = -1.62
	  dlnwe_dt(24) = -1.76
	  dlnwe_dt(25) = -1.79
c     Ion temperature time derivative correction [eV/s]- drvr_pedtimed.pro (after profile fitting)
	  dlnwi_dt(1) = 2.389
	  dlnwi_dt(2) = 2.402
	  dlnwi_dt(3) = 2.413
	  dlnwi_dt(4) = 2.420
	  dlnwi_dt(5) = 2.425
	  dlnwi_dt(6) = 2.427
	  dlnwi_dt(7) = 2.425
	  dlnwi_dt(8) = 2.420
	  dlnwi_dt(9) = 2.411
	  dlnwi_dt(10) = 2.398
	  dlnwi_dt(11) = 2.381
	  dlnwi_dt(12) = 2.359
	  dlnwi_dt(13) = 2.331
	  dlnwi_dt(14) = 2.298
	  dlnwi_dt(15) = 2.259
	  dlnwi_dt(16) = 2.212
	  dlnwi_dt(17) = 2.158 
	  dlnwi_dt(18) = 2.094 
	  dlnwi_dt(19) = 2.021
	  dlnwi_dt(20) = 1.937
	  dlnwi_dt(21) = 1.839
	  dlnwi_dt(22) = 1.726
	  dlnwi_dt(23) = 1.601
	  dlnwi_dt(24) = 1.508
	  dlnwi_dt(25) = 1.509
	  
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
c	 shot:118888 @ 1570,  type:H-mode,  date:06/16/2004

c     Radial electric field [V/m] for rho [0,1], deltarho = 0.02
c     drvr_pedxax.pro (after profile fitting)
      erextot(1) = 3.7971258e3
      erextot(2) = 4.8967462e3
      erextot(3) = 6.5801345e3
      erextot(4) = 8.4462512e3
      erextot(5) = 10.383589e3
      erextot(6) = 12.636019e3
      erextot(7) = 15.084097e3
      erextot(8) = 16.978704e3
      erextot(9) = 18.794785e3
      erextot(10) = 20.460152e3
      erextot(11) = 21.941612e3
      erextot(12) = 23.192651e3
      erextot(13) = 24.267442e3
      erextot(14) = 25.182657e3
      erextot(15) = 25.862066e3
      erextot(16) = 26.386630e3
      erextot(17) = 26.707086e3
      erextot(18) = 26.867523e3
      erextot(19) = 26.842897e3
      erextot(20) = 26.655946e3
      erextot(21) = 26.335756e3
      erextot(22) = 25.906210e3
      erextot(23) = 25.420053e3
      erextot(24) = 24.821583e3
      erextot(25) = 24.112723e3
      erextot(26) = 23.464634e3
      erextot(27) = 22.816822e3
      erextot(28) = 22.213417e3
      erextot(29) = 21.658071e3
      erextot(30) = 21.159538e3
      erextot(31) = 20.706720e3
      erextot(32) = 20.307010e3
      erextot(33) = 19.940796e3
      erextot(34) = 19.605929e3
      erextot(35) = 19.303461e3
      erextot(36) = 19.009269e3
      erextot(37) = 18.711668e3
      erextot(38) = 18.365164e3
      erextot(39) = 17.885588e3
      erextot(40) = 17.183370e3
      erextot(41) = 16.291165e3
      erextot(42) = 15.219480e3
      erextot(43) = 14.418048e3
      erextot(44) = 13.914118e3
      erextot(45) = 13.117152e3
      erextot(46) = 11.385809e3
      erextot(47) = 8.1925454e3
      erextot(48) = 4.0609205e3
      erextot(49) = -0.56582106e3
      erextot(50) = -21.035674e3
      erextot(51) = -5.7431691e3

c     Neutral beam deposition profile (hofr1) from NBeams (Dr. John Mandrekas)
c     units [#/s] - need to convert to GTEDGE rho coordinates using MATLAB script
c     nbeams2gtedge.m	from T.M.Wilks
      NBdep1(1) = .55926E+01
      NBdep1(2) = .11348E+02
      NBdep1(3) = .74921E+01
      NBdep1(4) = .66328E+01
      NBdep1(5) = .61694E+01
      NBdep1(6) = .58147E+01
      NBdep1(7) = .54942E+01
      NBdep1(8) = .51849E+01
      NBdep1(9) = .48772E+01
      NBdep1(10) = .45711E+01
      NBdep1(11) = .42676E+01
      NBdep1(12) = .33403E+01
      NBdep1(13) = .28469E+01
      NBdep1(14) = .25043E+01
      NBdep1(15) = .22405E+01
      NBdep1(16) = .20275E+01
      NBdep1(17) = .18511E+01
      NBdep1(18) = .17027E+01
      NBdep1(19) = .15752E+01
      NBdep1(20) = .14643E+01
      NBdep1(21) = .13668E+01
      NBdep1(22) = .12807E+01
      NBdep1(23) = .12044E+01
      NBdep1(24) = .11367E+01
      NBdep1(25) = .10764E+01
      NBdep1(26) = .10226E+01
      NBdep1(27) = .97420E+00
      NBdep1(28) = .93075E+00
      NBdep1(29) = .89107E+00
      NBdep1(30) = .85520E+00
      NBdep1(31) = .82273E+00
      NBdep1(32) = .79325E+00
      NBdep1(33) = .76632E+00
      NBdep1(34) = .74158E+00
      NBdep1(35) = .71932E+00
      NBdep1(36) = .69983E+00
      NBdep1(37) = .68156E+00
      NBdep1(38) = .66453E+00
      NBdep1(39) = .64891E+00
      NBdep1(40) = .63443E+00
      NBdep1(41) = .62131E+00
      NBdep1(42) = .60862E+00
      NBdep1(43) = .59660E+00
      NBdep1(44) = .58608E+00
      NBdep1(45) = .57696E+00
      NBdep1(46) = .56748E+00
      NBdep1(47) = .56006E+00
      NBdep1(48) = .55314E+00
      NBdep1(49) = .50521E+00
      NBdep1(50) = .27855E+00
      NBdep1(51) = .00000E+00

      NBdep2(1) = .43240E+01
      NBdep2(2) = .87680E+01
      NBdep2(3) = .57898E+01
      NBdep2(4) = .51311E+01
      NBdep2(5) = .47819E+01
      NBdep2(6) = .45202E+01
      NBdep2(7) = .42884E+01
      NBdep2(8) = .40691E+01
      NBdep2(9) = .38536E+01
      NBdep2(10) = .36401E+01
      NBdep2(11) = .34287E+01
      NBdep2(12) = .27212E+01
      NBdep2(13) = .23507E+01
      NBdep2(14) = .20962E+01
      NBdep2(15) = .19014E+01
      NBdep2(16) = .17444E+01
      NBdep2(17) = .16146E+01
      NBdep2(18) = .15053E+01
      NBdep2(19) = .14117E+01
      NBdep2(20) = .13305E+01
      NBdep2(21) = .12593E+01
      NBdep2(22) = .11967E+01
      NBdep2(23) = .11412E+01
      NBdep2(24) = .10921E+01
      NBdep2(25) = .10484E+01
      NBdep2(26) = .10098E+01
      NBdep2(27) = .97505E+00
      NBdep2(28) = .94427E+00
      NBdep2(29) = .91599E+00
      NBdep2(30) = .89062E+00
      NBdep2(31) = .86782E+00
      NBdep2(32) = .84732E+00
      NBdep2(33) = .82880E+00
      NBdep2(34) = .81194E+00
      NBdep2(35) = .79729E+00
      NBdep2(36) = .78579E+00
      NBdep2(37) = .77471E+00
      NBdep2(38) = .76426E+00
      NBdep2(39) = .75466E+00
      NBdep2(40) = .74585E+00
      NBdep2(41) = .73841E+00
      NBdep2(42) = .73083E+00
      NBdep2(43) = .72351E+00
      NBdep2(44) = .71717E+00
      NBdep2(45) = .71266E+00
      NBdep2(46) = .70625E+00
      NBdep2(47) = .70012E+00
      NBdep2(48) = .68688E+00
      NBdep2(49) = .62169E+00
      NBdep2(50) = .35021E+00
      NBdep2(51) = .00000E+00

      NBdep3(1) = .32570E+01
      NBdep3(2) = .66035E+01
      NBdep3(3) = .43640E+01
      NBdep3(4) = .38742E+01
      NBdep3(5) = .36201E+01
      NBdep3(6) = .34341E+01
      NBdep3(7) = .32730E+01
      NBdep3(8) = .31235E+01
      NBdep3(9) = .29784E+01
      NBdep3(10) = .28355E+01
      NBdep3(11) = .26944E+01
      NBdep3(12) = .21710E+01
      NBdep3(13) = .19021E+01
      NBdep3(14) = .17201E+01
      NBdep3(15) = .15823E+01
      NBdep3(16) = .14721E+01
      NBdep3(17) = .13815E+01
      NBdep3(18) = .13059E+01
      NBdep3(19) = .12415E+01
      NBdep3(20) = .11862E+01
      NBdep3(21) = .11382E+01
      NBdep3(22) = .10964E+01
      NBdep3(23) = .10596E+01
      NBdep3(24) = .10275E+01
      NBdep3(25) = .99913E+00
      NBdep3(26) = .97496E+00
      NBdep3(27) = .95329E+00
      NBdep3(28) = .93482E+00
      NBdep3(29) = .91787E+00
      NBdep3(30) = .90312E+00
      NBdep3(31) = .89032E+00
      NBdep3(32) = .87930E+00
      NBdep3(33) = .86980E+00
      NBdep3(34) = .86158E+00
      NBdep3(35) = .85542E+00
      NBdep3(36) = .85285E+00
      NBdep3(37) = .85007E+00
      NBdep3(38) = .84750E+00
      NBdep3(39) = .84541E+00
      NBdep3(40) = .84387E+00
      NBdep3(41) = .84386E+00
      NBdep3(42) = .84335E+00
      NBdep3(43) = .84293E+00
      NBdep3(44) = .84327E+00
      NBdep3(45) = .84639E+00
      NBdep3(46) = .84676E+00
      NBdep3(47) = .84697E+00
      NBdep3(48) = .83695E+00
      NBdep3(49) = .76248E+00
      NBdep3(50) = .43256E+00
      NBdep3(51) = .00000E+00

c      cosine of angle between injected NB particle and Bphi
      fpsi0(1) =  -6.29E-01
      fpsi0(2) =  -6.29E-01
      fpsi0(3) =  -6.29E-01
      fpsi0(4) =  -6.28E-01
      fpsi0(5) =  -6.28E-01
      fpsi0(6) =  -6.27E-01
      fpsi0(7) =  -6.27E-01
      fpsi0(8) =  -6.26E-01
      fpsi0(9) =  -6.25E-01
      fpsi0(10) = -6.24E-01
      fpsi0(11) = -6.23E-01
      fpsi0(12) = -6.21E-01
      fpsi0(13) = -6.18E-01
      fpsi0(14) = -6.16E-01
      fpsi0(15) = -6.14E-01
      fpsi0(16) = -6.11E-01
      fpsi0(17) = -6.09E-01
      fpsi0(18) = -6.06E-01
      fpsi0(19) = -6.04E-01
      fpsi0(20) = -6.01E-01
      fpsi0(21) = -5.99E-01
      fpsi0(22) = -5.96E-01
      fpsi0(23) = -5.94E-01
      fpsi0(24) = -5.91E-01
      fpsi0(25) = -5.88E-01
      fpsi0(26) = -5.86E-01
      fpsi0(27) = -5.83E-01
      fpsi0(28) = -5.80E-01
      fpsi0(29) = -5.78E-01
      fpsi0(30) = -5.75E-01
      fpsi0(31) = -5.73E-01
      fpsi0(32) = -5.70E-01
      fpsi0(33) = -5.68E-01
      fpsi0(34) = -5.65E-01
      fpsi0(35) = -5.63E-01
      fpsi0(36) = -5.60E-01
      fpsi0(37) = -5.58E-01
      fpsi0(38) = -5.56E-01
      fpsi0(39) = -5.54E-01
      fpsi0(40) = -5.52E-01
      fpsi0(41) = -5.50E-01
      fpsi0(42) = -5.48E-01
      fpsi0(43) = -5.46E-01
      fpsi0(44) = -5.45E-01
      fpsi0(45) = -5.44E-01
      fpsi0(46) = -5.44E-01
      fpsi0(47) = -5.46E-01
      fpsi0(48) = -5.52E-01
      fpsi0(49) = -5.59E-01
      fpsi0(50) = -5.51E-01
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
c		118888 @ 1570ms	 H-mode
		rmajor = 1.724
		aminor = 0.597
		elong = 1.827
		triang = 0.369
		plasmacur = 1.40
   		B = 2.004
		bphi = -2.004
		q95 = 3.575
		pbeam = 4.565
		Rx = 1.479
		zx = -1.242
		Rsep1 = 1.213
		rsep2 = 1.566
		zsep1 = -1.366
		zsep2 = -1.366
		ssi95 = 3.999
		pohmin = 0.219
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
		xnpedex = 3.11e19
		xnsepex = 3.00e18
		tpedexi = 335.
		tsepexi = 238.
		tpedexe = 245.
		tsepexe = 25.0
		widthnx = 0.1
		widthtex = 0.05
		widthtix = 0.05
		gradnbar(3) = 6.07e-2
		gradnbar(4) = 6.07e-2
		gradTbar(3) = 0.148
 		gradTbar(4) = 0.148
		gradTebar(3) = 0.0307
		gradTebar(4) = 0.0307
		aped = 0.685
		xnctrped = 1.45
		tctrped = 7.347
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
	
	