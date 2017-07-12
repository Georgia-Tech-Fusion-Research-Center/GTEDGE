      
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
c	 shot:118890 @ 1560, type: H-mode,  Date: 06/16/2004

c     Electron density [#/m^3]- drvr_pedxax.pro (after profile fitting) 
	  EXNE(1) = 0.29254289E20
	  EXNE(2) = 0.29097717E20
	  EXNE(3) = 0.28935035E20
	  EXNE(4) = 0.28766113E20
	  EXNE(5) = 0.28587409E20
	  EXNE(6) = 0.28398421E20
	  EXNE(7) = 0.28203292E20
	  EXNE(8) = 0.27998720E20
	  EXNE(9) = 0.27782690E20
	  EXNE(10) = 0.27558158E20
	  EXNE(11) = 0.27323839E20
	  EXNE(12) = 0.27076803E20
	  EXNE(13) = 0.26821317E20
	  EXNE(14) = 0.26553200E20
	  EXNE(15) = 0.26273440E20
	  EXNE(16) = 0.25982441E20
	  EXNE(17) = 0.25678006E20
	  EXNE(18) = 0.25363119E20  
	  EXNE(19) = 0.25026563E20
	  EXNE(20) = 0.24637372E20
	  EXNE(21) = 0.24073294E20
	  EXNE(22) = 0.22773904E20
	  EXNE(23) = 0.18983370E20
	  EXNE(24) = 0.12281754E20
	  EXNE(25) = 0.079020123E20
c     Electron density gradient scale length [m]-
c     drvr_pedxax.pro (after profile fitting)
c     xlne = [(1/n)dn/dr]^-1  
	  XLNE(1) = 0.45909279
	  XLNE(2) = 0.43450305
	  XLNE(3) = 0.40846295
	  XLNE(4) = 0.38201839
	  XLNE(5) = 0.35840252
	  XLNE(6) = 0.33616418
	  XLNE(7) = 0.31429647
	  XLNE(8) = 0.29432496
	  XLNE(9) = 0.27606593
	  XLNE(10) = 0.25977371
	  XLNE(11) = 0.24237967
	  XLNE(12) = 0.22645150
	  XLNE(13) = 0.21070418
	  XLNE(14) = 0.19636215
	  XLNE(15) = 0.18290068
	  XLNE(16) = 0.17197034
	  XLNE(17) = 0.16170442
	  XLNE(18) = 0.14997744
	  XLNE(19) = 0.13258639
	  XLNE(20) = 0.10119592
	  XLNE(21) = 0.053240405
	  XLNE(22) = 0.016938983
	  XLNE(23) = 0.0049730935
	  XLNE(24) = 0.0026831418
	  XLNE(25) = 0.0043322110
c     Toroidal Velocity gradient scale length [m]- 
c     drvr_pedxax.pro (after profile fitting)
c     exlv = [(1/v)dv/dr]^-1	
	  exlv(1) = 0.059262052
	  exlv(2) = 0.053047754
	  exlv(3) = 0.046977818
	  exlv(4) = 0.041181579
	  exlv(5) = 0.035990201
	  exlv(6) = 0.031370617
	  exlv(7) = 0.027716743
	  exlv(8) = 0.024948100
	  exlv(9) = 0.022890951
	  exlv(10) = 0.021489725
	  exlv(11) = 0.020510389
	  exlv(12) = 0.020315289
	  exlv(13) = 0.021102965
	  exlv(14) = 0.023890164
	  exlv(15) = 0.031532414
	  exlv(16) = 0.061409518
	  exlv(17) = -0.30362505
	  exlv(18) = -0.039782200
	  exlv(19) = -0.021085054
	  exlv(20) = -0.016087485
	  exlv(21) = -0.022407753
	  exlv(22) = 0.23366134
	  exlv(23) = 0.0098625300
	  exlv(24) = 0.0033047020
	  exlv(25) = 0.0015062534
c     Electron temperature gradient scale length [m] - 
c     drvr_pedxax.pro (after profile fitting)
c     exlte = [(1/Te)dTe/dr]^-1
	  EXLTE(1) = 0.099171152
	  EXLTE(2) = 0.095952703
	  EXLTE(3) = 0.092126576
	  EXLTE(4) = 0.087875262
	  EXLTE(5) = 0.083922275
	  EXLTE(6) = 0.079963097
	  EXLTE(7) = 0.075750356
	  EXLTE(8) = 0.071614797
	  EXLTE(9) = 0.067508640
	  EXLTE(10) = 0.063414112
	  EXLTE(11) = 0.058488472
	  EXLTE(12) = 0.053289249
	  EXLTE(13) = 0.047367150
	  EXLTE(14) = 0.041014108
	  EXLTE(15) = 0.034253431
	  EXLTE(16) = 0.027692731
	  EXLTE(17) = 0.021500796
	  EXLTE(18) = 0.016039216
	  EXLTE(19) = 0.011607015
	  EXLTE(20) = 0.0083559954
	  EXLTE(21) = 0.0062856214
	  EXLTE(22) = 0.0050331524
	  EXLTE(23) = 0.0044337143
	  EXLTE(24) = 0.0043026379
	  EXLTE(25) = 0.0048030377
c     Ion temperature [eV]- drvr_pedxax.pro (after profile fitting)	 
	  XTI(1) = 454.08464
	  XTI(2) = 444.16542
	  XTI(3) = 434.20688
	  XTI(4) = 424.21511
	  XTI(5) = 414.19875
	  XTI(6) = 404.16576
	  XTI(7) = 394.12419
	  XTI(8) = 384.08228
	  XTI(9) = 374.04799
	  XTI(10) = 364.02932
	  XTI(11) = 354.03450
	  XTI(12) = 344.07153
	  XTI(13) = 334.14850
	  XTI(14) = 324.27229
	  XTI(15) = 314.45218
	  XTI(16) = 304.69628
	  XTI(17) = 295.01272
	  XTI(18) = 285.40927
	  XTI(19) = 275.89263
	  XTI(20) = 266.47314
	  XTI(21) = 257.15795
	  XTI(22) = 247.95353
	  XTI(23) = 238.87127
	  XTI(24) = 229.91543
	  XTI(25) = 221.09747
c     Ion temperature gradient scale length [m]- 
c     drvr_pedxax.pro (after profile fitting)
c     exlti = [(1/Ti)dTi/dr]^-1
	  EXLTI(1) = 0.10881861
	  EXLTI(2) = 0.10581374
	  EXLTI(3) = 0.10219552
	  EXLTI(4) = 0.098222159
	  EXLTI(5) = 0.094699033
	  EXLTI(6) = 0.091280811
	  EXLTI(7) = 0.087740235
	  EXLTI(8) = 0.084454209
	  EXLTI(9) = 0.081438489
	  EXLTI(10) = 0.078793585
	  EXLTI(11) = 0.075577952
	  EXLTI(12) = 0.072637245
	  EXLTI(13) = 0.069499366
	  EXLTI(14) = 0.066625029
	  EXLTI(15) = 0.063854687
	  EXLTI(16) = 0.061806563
	  EXLTI(17) = 0.059933338
	  EXLTI(18) = 0.057715181
	  EXLTI(19) = 0.054637317
	  EXLTI(20) = 0.050698932
	  EXLTI(21) = 0.047000274
	  EXLTI(22) = 0.043160990
	  EXLTI(23) = 0.039727211
	  EXLTI(24) = 0.036136024
	  EXLTI(25) = 0.033496756
c     Electron temperature [eV]- drvr_pedxax.pro (after profile fitting)					
	  XTE(1) = 460.25155
	  XTE(2) = 449.20273
	  XTE(3) = 438.07748
	  XTE(4) = 426.86860
	  XTE(5) = 415.56780
	  XTE(6) = 404.16442
	  XTE(7) = 392.64270
	  XTE(8) = 380.98023
	  XTE(9) = 369.14314
	  XTE(10) = 357.08389
	  XTE(11) = 344.72044
	  XTE(12) = 331.91386
	  XTE(13) = 318.48652
	  XTE(14) = 304.10068
	  XTE(15) = 288.29169
	  XTE(16) = 270.37346
	  XTE(17) = 249.46181
	  XTE(18) = 224.60660
	  XTE(19) = 195.24749
	  XTE(20) = 161.99437
	  XTE(21) = 127.25030
	  XTE(22) = 94.820143
	  XTE(23) = 68.169387
	  XTE(24) = 48.777848
	  XTE(25) = 36.034448
c     safety factor [1]- EFITtools --> profiles_1_mse
c     need to manually input for GTEDGE rho vector
	  qedge(1) = 2.91
	  qedge(2) = 2.96
	  qedge(3) = 3.02
	  qedge(4) = 3.07
	  qedge(5) = 3.13
	  qedge(6) = 3.19
	  qedge(7) = 3.25
	  qedge(8) = 3.31
	  qedge(9) = 3.37
	  qedge(10) = 3.44
	  qedge(11) = 3.50
	  qedge(12) = 3.58
	  qedge(13) = 3.65
	  qedge(14) = 3.73
	  qedge(15) = 3.81
	  qedge(16) = 3.90
	  qedge(17) = 3.98
	  qedge(18) = 4.07
	  qedge(19) = 4.17
	  qedge(20) = 4.28
	  qedge(21) = 4.47
	  qedge(22) = 4.67
	  qedge(23) = 4.89
	  qedge(24) = 5.06
	  qedge(25) = 5.20
c     Radial Electric Field [V/m]- drvr_pedxax.pro (after profile fitting)
	  erex(1) = 9.5323587e3
	  erex(2) = 8.9789916e3
	  erex(3) = 8.4071136e3
	  erex(4) = 7.7893975e3
	  erex(5) = 7.1359701e3
	  erex(6) = 6.4246399e3
	  erex(7) = 5.6440620e3
	  erex(8) = 4.8147759e3
	  erex(9) = 3.9471632e3
	  erex(10) = 3.0335177e3
	  erex(11) = 2.0964614e3
	  erex(12) = 1.1022046e3
	  erex(13) = 0.044737843e3
	  erex(14) = -1.1129803e3
	  erex(15) = -2.3777335e3
	  erex(16) = -3.7454767e3
	  erex(17) = -5.3062190e3
	  erex(18) = -7.0484285e3
	  erex(19) = -8.5430528e3
	  erex(20) = -9.2601708e3
	  erex(21) = -9.4136796e3
	  erex(22) = -9.9503822e3
	  erex(23) = -13.468540e3
	  erex(24) = -15.993918e3
	  erex(25) = -6.4276820e3
c     Carbon Poloidal Velocity [m/s]- drvr_pedxax.pro (after profile fitting)
c	rh sign convention + is down at outer midplane
	  vthexp(1) = -0.60079844e3
	  vthexp(2) = -0.59723125e3
	  vthexp(3) = -0.58975995e3
	  vthexp(4) = -0.58089508e3
	  vthexp(5) = -0.57884445e3
	  vthexp(6) = -0.58912695e3
	  vthexp(7) = -0.61658970e3
	  vthexp(8) = -0.66693065e3
	  vthexp(9) = -0.74582826e3
	  vthexp(10) = -0.85883622e3
	  vthexp(11) = -1.0118945e3
	  vthexp(12) = -1.2210742e3
	  vthexp(13) = -1.5120199e3
	  vthexp(14) = -1.9107717e3
	  vthexp(15) = -2.4433573e3
	  vthexp(16) = -3.1358156e3
	  vthexp(17) = -4.0141916e3
	  vthexp(18) = -5.0532715e3
	  vthexp(19) = -5.9745002e3
	  vthexp(20) = -6.4254819e3
	  vthexp(21) = -6.0319424e3
	  vthexp(22) = -4.7226704e3
	  vthexp(23) = -2.7644196e3
	  vthexp(24) = -0.46022263e3
	  vthexp(25) = 1.8868163e3
c     Carbon Toroidal Velocity [m/s]- drvr_pedxax.pro (after profile fitting)
	  torv(1) = 28.958669e3
	  torv(2) = 27.759409e3
	  torv(3) = 26.478594e3
	  torv(4) = 25.110983e3
	  torv(5) = 23.651598e3
	  torv(6) = 22.098134e3
	  torv(7) = 20.470332e3
	  torv(8) = 18.809245e3
	  torv(9) = 17.155599e3
	  torv(10) = 15.550322e3
	  torv(11) = 14.034252e3
	  torv(12) = 12.648286e3
	  torv(13) = 11.433173e3
	  torv(14) = 10.430015e3
	  torv(15) = 9.6793519e3 
	  torv(16) = 9.2166374e3 
	  torv(17) = 9.0948022e3 
	  torv(18) = 9.3537484e3
	  torv(19) = 10.024708e3
	  torv(20) = 11.124040e3
	  torv(21) = 12.262250e3
	  torv(22) = 12.752720e3
	  torv(23) = 11.855680e3
	  torv(24) = 8.9839255e3
	  torv(25) = 4.8185350e3
c     Density time derivative correction [#/m^3-s]- drvr_pedtimed.pro (after profile fitting)	  
	  dlnn_dt(1) = 1.5981354
	  dlnn_dt(2) = 1.6460006
	  dlnn_dt(3) = 1.6965111
	  dlnn_dt(4) = 1.7497969
	  dlnn_dt(5) = 1.8055239
	  dlnn_dt(6) = 1.8638915
	  dlnn_dt(7) = 1.9256847
	  dlnn_dt(8) = 1.9907509
	  dlnn_dt(9) = 2.0591359
	  dlnn_dt(10) = 2.1314683
	  dlnn_dt(11) = 2.2081392
	  dlnn_dt(12) = 2.2894464
	  dlnn_dt(13) = 2.3767173
	  dlnn_dt(14) = 2.4709315
	  dlnn_dt(15) = 2.5744393
	  dlnn_dt(16) = 2.6911571
	  dlnn_dt(17) = 2.8271749
	  dlnn_dt(18) = 2.9919567
	  dlnn_dt(19) = 3.1963992
	  dlnn_dt(20) = 3.4472675
	  dlnn_dt(21) = 3.7298560
	  dlnn_dt(22) = 3.9803872
	  dlnn_dt(23) = 4.0513883
	  dlnn_dt(24) = 3.7177136
	  dlnn_dt(25) = 3.1593874
c     Electron temperature time derivative correction [eV/s]- drvr_pedtimed.pro (after profile fitting)
	  dlnwe_dt(1) = 3.0615253
	  dlnwe_dt(2) = 3.0962021
	  dlnwe_dt(3) = 3.1324351
	  dlnwe_dt(4) = 3.1704030
	  dlnwe_dt(5) = 3.2102962
	  dlnwe_dt(6) = 3.2523286
	  dlnwe_dt(7) = 3.2967288
	  dlnwe_dt(8) = 3.3437240
	  dlnwe_dt(9) = 3.3935468
	  dlnwe_dt(10) = 3.4464314
	  dlnwe_dt(11) = 3.5025213
	  dlnwe_dt(12) = 3.5618117
	  dlnwe_dt(13) = 3.6243117
	  dlnwe_dt(14) = 3.6894901
	  dlnwe_dt(15) = 3.7566078
	  dlnwe_dt(16) = 3.8243124
	  dlnwe_dt(17) = 3.8906550
	  dlnwe_dt(18) = 3.9527371
	  dlnwe_dt(19) = 4.0056620
	  dlnwe_dt(20) = 4.0382657
	  dlnwe_dt(21) = 4.0218730
	  dlnwe_dt(22) = 3.8981204
	  dlnwe_dt(23) = 3.5913005
	  dlnwe_dt(24) = 3.0622108
	  dlnwe_dt(25) = 2.3726220
c     Ion temperature time derivative correction [eV/s]- drvr_pedtimed.pro (after profile fitting)
	  dlnwi_dt(1) = 2.0597241
	  dlnwi_dt(2) = 2.0282769
	  dlnwi_dt(3) = 1.9957782
	  dlnwi_dt(4) = 1.9624718
	  dlnwi_dt(5) = 1.9285316
	  dlnwi_dt(6) = 1.8944782
	  dlnwi_dt(7) = 1.8602425
	  dlnwi_dt(8) = 1.8265612
	  dlnwi_dt(9) = 1.7937471
	  dlnwi_dt(10) = 1.7620324
	  dlnwi_dt(11) = 1.7321600
	  dlnwi_dt(12) = 1.7046622
	  dlnwi_dt(13) = 1.6801437
	  dlnwi_dt(14) = 1.6592762
	  dlnwi_dt(15) = 1.6428523
	  dlnwi_dt(16) = 1.6317755
	  dlnwi_dt(17) = 1.6270921
	  dlnwi_dt(18) = 1.6299915
	  dlnwi_dt(19) = 1.6416695
	  dlnwi_dt(20) = 1.6636587
	  dlnwi_dt(21) = 1.6975042
	  dlnwi_dt(22) = 1.7451551
	  dlnwi_dt(23) = 1.8087044
	  dlnwi_dt(24) = 1.8905246
	  dlnwi_dt(25) = 1.9933254
	  
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
c	 shot:118890 @ 1560,  type:H-mode,  date:06/16/2004

c     Radial electric field [V/m] for rho [0,1], deltarho = 0.02
c     drvr_pedxax.pro (after profile fitting)
      erextot(1) = 3.3318877E3
      erextot(2) = 4.8454295E3
      erextot(3) = 6.6970070E3
      erextot(4) = 8.5921713E3
      erextot(5) = 10.460901E3
      erextot(6) = 12.637535E3
      erextot(7) = 14.858479E3
      erextot(8) = 16.580506E3
      erextot(9) = 18.168194E3
      erextot(10) = 19.554879E3
      erextot(11) = 20.740691E3
      erextot(12) = 21.679210E3
      erextot(13) = 22.440469E3
      erextot(14) = 23.003153E3
      erextot(15) = 23.356925E3
      erextot(16) = 23.536876E3
      erextot(17) = 23.534828E3
      erextot(18) = 23.360140E3
      erextot(19) = 23.043908E3
      erextot(20) = 22.606371E3
      erextot(21) = 22.069962E3
      erextot(22) = 21.525092E3
      erextot(23) = 20.903742E3
      erextot(24) = 20.171274E3
      erextot(25) = 19.525191E3
      erextot(26) = 18.916638E3
      erextot(27) = 18.391724E3
      erextot(28) = 17.958661E3
      erextot(29) = 17.612723E3
      erextot(30) = 17.339765E3
      erextot(31) = 17.127708E3
      erextot(32) = 16.965183E3
      erextot(33) = 16.837037E3
      erextot(34) = 16.729964E3
      erextot(35) = 16.627984E3
      erextot(36) = 16.475148E3
      erextot(37) = 16.216162E3
      erextot(38) = 15.869833E3
      erextot(39) = 15.403444E3
      erextot(40) = 14.738483E3
      erextot(41) = 13.828283E3
      erextot(42) = 12.563751E3
      erextot(43) = 10.917338E3
      erextot(44) = 9.1680012E3
      erextot(45) = 7.1359701E3
      erextot(46) = 4.5297997E3
      erextot(47) = 1.4402083E3
      erextot(48) = -2.3777335E3
      erextot(49) = -7.6025934E3
      erextot(50) = -9.5852450E3
      erextot(51) = -6.4276820E3

c     Neutral beam deposition profile (hofr1) from NBeams (Dr. John Mandrekas)
c     units [#/s] - need to convert to GTEDGE rho coordinates using MATLAB script
c     nbeams2gtedge.m	from T.M.Wilks
      NBdep1(1) = .59445E+01
      NBdep1(2) = .12054E+02
      NBdep1(3) = .79491E+01
      NBdep1(4) = .70279E+01
      NBdep1(5) = .65247E+01
      NBdep1(6) = .61368E+01
      NBdep1(7) = .57885E+01
      NBdep1(8) = .54550E+01
      NBdep1(9) = .51270E+01
      NBdep1(10) = .48007E+01
      NBdep1(11) = .44797E+01
      NBdep1(12) = .34035E+01
      NBdep1(13) = .29149E+01
      NBdep1(14) = .25669E+01
      NBdep1(15) = .22971E+01
      NBdep1(16) = .20791E+01
      NBdep1(17) = .18977E+01
      NBdep1(18) = .17432E+01
      NBdep1(19) = .16103E+01
      NBdep1(20) = .14952E+01
      NBdep1(21) = .13944E+01
      NBdep1(22) = .13060E+01
      NBdep1(23) = .12279E+01
      NBdep1(24) = .11586E+01
      NBdep1(25) = .10967E+01
      NBdep1(26) = .10418E+01
      NBdep1(27) = .99186E+00
      NBdep1(28) = .94651E+00
      NBdep1(29) = .90538E+00
      NBdep1(30) = .86811E+00
      NBdep1(31) = .83394E+00
      NBdep1(32) = .80257E+00
      NBdep1(33) = .77382E+00
      NBdep1(34) = .74717E+00
      NBdep1(35) = .72215E+00
      NBdep1(36) = .69865E+00
      NBdep1(37) = .67646E+00
      NBdep1(38) = .65517E+00
      NBdep1(39) = .63454E+00
      NBdep1(40) = .61463E+00
      NBdep1(41) = .59497E+00
      NBdep1(42) = .57562E+00
      NBdep1(43) = .55617E+00
      NBdep1(44) = .53667E+00
      NBdep1(45) = .51696E+00
      NBdep1(46) = .49723E+00
      NBdep1(47) = .47733E+00
      NBdep1(48) = .45879E+00
      NBdep1(49) = .45206E+00
      NBdep1(50) = .41873E+00
      NBdep1(51) = .00000E+00

      NBdep2(1) = .45826E+01
      NBdep2(2) = .92948E+01
      NBdep2(3) = .61377E+01
      NBdep2(4) = .54392E+01
      NBdep2(5) = .50674E+01
      NBdep2(6) = .47885E+01
      NBdep2(7) = .45426E+01
      NBdep2(8) = .43096E+01
      NBdep2(9) = .40810E+01
      NBdep2(10) = .38542E+01
      NBdep2(11) = .36298E+01
      NBdep2(12) = .27961E+01
      NBdep2(13) = .24258E+01
      NBdep2(14) = .21642E+01
      NBdep2(15) = .19621E+01
      NBdep2(16) = .17992E+01
      NBdep2(17) = .16640E+01
      NBdep2(18) = .15497E+01
      NBdep2(19) = .14516E+01
      NBdep2(20) = .13671E+01
      NBdep2(21) = .12927E+01
      NBdep2(22) = .12276E+01
      NBdep2(23) = .11699E+01
      NBdep2(24) = .11188E+01
      NBdep2(25) = .10732E+01
      NBdep2(26) = .10336E+01
      NBdep2(27) = .99709E+00
      NBdep2(28) = .96417E+00
      NBdep2(29) = .93440E+00
      NBdep2(30) = .90776E+00
      NBdep2(31) = .88334E+00
      NBdep2(32) = .86104E+00
      NBdep2(33) = .84080E+00
      NBdep2(34) = .82211E+00
      NBdep2(35) = .80448E+00
      NBdep2(36) = .78805E+00
      NBdep2(37) = .77245E+00
      NBdep2(38) = .75722E+00
      NBdep2(39) = .74207E+00
      NBdep2(40) = .72708E+00
      NBdep2(41) = .71169E+00
      NBdep2(42) = .69585E+00
      NBdep2(43) = .67912E+00
      NBdep2(44) = .66145E+00
      NBdep2(45) = .64263E+00
      NBdep2(46) = .62280E+00
      NBdep2(47) = .60094E+00
      NBdep2(48) = .57781E+00
      NBdep2(49) = .55783E+00
      NBdep2(50) = .52263E+00
      NBdep2(51) = .00000E+00

      NBdep3(1) = .34541E+01
      NBdep3(2) = .70086E+01
      NBdep3(3) = .46345E+01
      NBdep3(4) = .41169E+01
      NBdep3(5) = .38488E+01
      NBdep3(6) = .36537E+01
      NBdep3(7) = .34854E+01
      NBdep3(8) = .33283E+01
      NBdep3(9) = .31751E+01
      NBdep3(10) = .30234E+01
      NBdep3(11) = .28730E+01
      NBdep3(12) = .22485E+01
      NBdep3(13) = .19780E+01
      NBdep3(14) = .17892E+01
      NBdep3(15) = .16446E+01
      NBdep3(16) = .15288E+01
      NBdep3(17) = .14334E+01
      NBdep3(18) = .13535E+01
      NBdep3(19) = .12855E+01
      NBdep3(20) = .12276E+01
      NBdep3(21) = .11767E+01
      NBdep3(22) = .11324E+01
      NBdep3(23) = .10934E+01
      NBdep3(24) = .10591E+01
      NBdep3(25) = .10290E+01
      NBdep3(26) = .10039E+01
      NBdep3(27) = .98038E+00
      NBdep3(28) = .95966E+00
      NBdep3(29) = .94121E+00
      NBdep3(30) = .92532E+00
      NBdep3(31) = .91097E+00
      NBdep3(32) = .89822E+00
      NBdep3(33) = .88710E+00
      NBdep3(34) = .87712E+00
      NBdep3(35) = .86779E+00
      NBdep3(36) = .85945E+00
      NBdep3(37) = .85159E+00
      NBdep3(38) = .84371E+00
      NBdep3(39) = .83549E+00
      NBdep3(40) = .82703E+00
      NBdep3(41) = .81771E+00
      NBdep3(42) = .80737E+00
      NBdep3(43) = .79553E+00
      NBdep3(44) = .78208E+00
      NBdep3(45) = .76680E+00
      NBdep3(46) = .74990E+00
      NBdep3(47) = .72962E+00
      NBdep3(48) = .70663E+00
      NBdep3(49) = .68428E+00
      NBdep3(50) = .64543E+00
      NBdep3(51) = .00000E+00

c      cosine of angle between injected NB particle and Bphi
      fpsi0(1) =  -6.25E-01
      fpsi0(2) =  -6.25E-01
      fpsi0(3) =  -6.25E-01
      fpsi0(4) =  -6.25E-01
      fpsi0(5) =  -6.24E-01
      fpsi0(6) =  -6.24E-01
      fpsi0(7) =  -6.23E-01
      fpsi0(8) =  -6.23E-01
      fpsi0(9) =  -6.22E-01
      fpsi0(10) = -6.21E-01
      fpsi0(11) = -6.20E-01
      fpsi0(12) = -6.17E-01
      fpsi0(13) = -6.15E-01
      fpsi0(14) = -6.12E-01
      fpsi0(15) = -6.10E-01
      fpsi0(16) = -6.08E-01
      fpsi0(17) = -6.05E-01
      fpsi0(18) = -6.03E-01
      fpsi0(19) = -6.00E-01
      fpsi0(20) = -5.98E-01
      fpsi0(21) = -5.95E-01
      fpsi0(22) = -5.93E-01
      fpsi0(23) = -5.90E-01
      fpsi0(24) = -5.88E-01
      fpsi0(25) = -5.85E-01
      fpsi0(26) = -5.83E-01
      fpsi0(27) = -5.80E-01
      fpsi0(28) = -5.78E-01
      fpsi0(29) = -5.75E-01
      fpsi0(30) = -5.73E-01
      fpsi0(31) = -5.70E-01
      fpsi0(32) = -5.68E-01
      fpsi0(33) = -5.66E-01
      fpsi0(34) = -5.63E-01
      fpsi0(35) = -5.61E-01
      fpsi0(36) = -5.59E-01
      fpsi0(37) = -5.56E-01
      fpsi0(38) = -5.54E-01
      fpsi0(39) = -5.52E-01
      fpsi0(40) = -5.50E-01
      fpsi0(41) = -5.48E-01
      fpsi0(42) = -5.47E-01
      fpsi0(43) = -5.45E-01
      fpsi0(44) = -5.44E-01
      fpsi0(45) = -5.44E-01
      fpsi0(46) = -5.43E-01
      fpsi0(47) = -5.44E-01
      fpsi0(48) = -5.47E-01
      fpsi0(49) = -5.59E-01
      fpsi0(50) = -5.55E-01
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
c		118890 @ 1560ms	 L-mode
		rmajor = 1.7166
		aminor = 0.588
		elong = 1.8623
		triang = 0.363
		plasmacur = 1.39
   		B = 2.00
		bphi = -2.00
		q95 = 3.622
		pbeam = 4.88
		Rx = 1.48
		zx = -1.2401
		Rsep1 = 1.21
		rsep2 = 1.57
		zsep1 = -1.366
		zsep2 = -1.366
		ssi95 = 3.986
		pohmin = 0.2697
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
		xnpedex = 2.58e19
		xnsepex = 7.90e18
		tpedexi = 298.
		tsepexi = 221.
		tpedexe = 288.
		tsepexe = 36.0
		widthnx = .05
		widthtex = .06
		widthtix = .05
		gradnbar(3) = 0.0471
		gradnbar(4) = 0.0471
		gradTbar(3) = 0.1685
 		gradTbar(4) = 0.1685
		gradTebar(3) = 0.03857
		gradTebar(4) = 0.03857
		aped = 0.580
		xnctrped = 1.78
		tctrped = 6.8601
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
	
	