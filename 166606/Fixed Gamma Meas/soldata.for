
      SUBROUTINE SOLDATA(nbdep1,nbdep2,nbdep3,erextot,fpsi0)

c*****************************************************************
c     Reads data in from text files and defines edge profiles from 
c     the DIII-D database
c*****************************************************************

	INCLUDE 'SOLDIV.FI'

	real ibal,imod,ioptsep,vplas,ioptgeom,dchirat,changeouter,tw,
	1     gammid,xlmid,czd(25),cmid,delsol,rwall,ioptdel,ioptpump,
	2     hxpt,riol,bpiol,btiol,psireal,vpold,vtord,exlvpold(25),
     3	 exlvtord(25)
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

C	################################################
C    #
C	# Balance Values and Experimental Data
C	# Let the open statement pick up the values
C    #
C	################################################

c
c	 taue=
c	 lav=
c	 central ne=
c	 central te=
c	 teped=
c	 hconf=2.17


c	 hrat=0.765
c	 alphan=3.2
c	 alphat2=1.75
c	 cballoon=1.45
c
	
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
c	ioptCIRC = 1
	
	RLOSSIOL = 0.5
	NBIspin = 1.0
	NBIeloss = 0.0
c******************************************************
c    Begin shot specific input data
c******************************************************
c	 shot:166606 @ 1950, type: H-mode,  Date: 5/19/2016
c    Good but no NBI/fion data

c     Electron density [#/m^3]- drvr_pedxax.pro (after profile fitting) 

      exne(1)  =  5.904062436e+19
      exne(2)  =  5.913470587e+19
      exne(3)  =  5.941552043e+19
      exne(4)  =  5.983128961e+19
      exne(5)  =  6.033023497e+19
      exne(6)  =  6.086057809e+19
      exne(7)  =  6.137054053e+19
      exne(8)  =  6.180834386e+19
      exne(9)  =  6.212220965e+19
      exne(10) =  6.226035945e+19
      exne(11) =  6.217101484e+19
      exne(12) =  6.180239739e+19
      exne(13) =  6.110272866e+19
      exne(14) =  6.002023023e+19
      exne(15) =  5.850312364e+19
      exne(16) =  5.649963048e+19
      exne(17) =  5.395797231e+19
      exne(18) =  5.08263707e+19
      exne(19) =  4.705304722e+19
      exne(20) =  4.258622342e+19
      exne(21) =  3.737412088e+19
      exne(22) =  3.136496117e+19
      exne(23) =  2.450696585e+19
      exne(24) =  1.674835649e+19
      exne(25) =  0.803735466e+19
c     Electron density gradient scale length [m]-
c     drvr_pedxax.pro (after profile fitting)
c     xlne = [(1/n)dn/dr]^-1   

      xlne(1) =  -21.4115748
      xlne(2) =  1.809528086
      xlne(3) =  0.998799497
      xlne(4) =  0.770381424
      xlne(5) =  0.691762676
      xlne(6) =  0.690574018
      xlne(7) =  0.763137016
      xlne(8) =  0.964589088
      xlne(9) =  1.588549171
      xlne(10) = 11.30898334
      xlne(11) = -1.692870002
      xlne(12) = -0.705623147
      xlne(13) = -0.415451045
      xlne(14) = -0.278910297
      xlne(15) = -0.200390893
      xlne(16) = -0.149737579
      xlne(17) = -0.114479426
      xlne(18) = -0.088551983
      xlne(19) = -0.068666456
      xlne(20) = -0.052893014
      xlne(21) = -0.040029395
      xlne(22) = -0.029290474
      xlne(23) = -0.020143615
      xlne(24) = -0.0122159
      xlne(25) = -0.005239215
c     Toroidal Velocity gradient scale length [m]- 
c     drvr_pedxax.pro (after profile fitting)
c     exlv = [(1/v)dv/dr]^-1	
      exlv(1)  = 0.207080906
      exlv(2)  = 0.204428562
      exlv(3)  = 0.201922263
      exlv(4)  = 0.199568808
      exlv(5)  = 0.197376116
      exlv(6)  = 0.195353386
      exlv(7)  = 0.193511289
      exlv(8)  = 0.191862208
      exlv(9)  = 0.190420522
      exlv(10) = 0.189202954
      exlv(11) = 0.188229003
      exlv(12) = 0.187521474
      exlv(13) = 0.187107143
      exlv(14) = 0.187017588
      exlv(15) = 0.187290246
      exlv(16) = 0.187969768
      exlv(17) = 0.189109766
      exlv(18) = 0.190775107
      exlv(19) = 0.193044947
      exlv(20) = 0.19601681
      exlv(21) = 0.199812155
      exlv(22) = 0.204584106
      exlv(23) = 0.210528385
      exlv(24) = 0.21789912
	  exlv(25) = 0.227032268 
c     Electron temperature gradient scale length [m] - 
c     drvr_pedxax.pro (after profile fitting)
c     exlte = [(1/Te)dTe/dr]^-1

      exlte(1)  =  0.177610591
      exlte(2)  =  0.195748018
      exlte(3)  =  0.215807083
      exlte(4)  =  0.236992873
      exlte(5)  =  0.25782584
      exlte(6)  =  0.276060302
      exlte(7)  =  0.288904062
      exlte(8)  =  0.293675554
      exlte(9)  =  0.28875279
      exlte(10) =  0.274299549
      exlte(11) =  0.252243093
      exlte(12) =  0.22550139
      exlte(13) =  0.196995801
      exlte(14) =  0.165653412
      exlte(15) =  0.133408682
      exlte(16) =  0.104652468
      exlte(17) =  0.081632581
      exlte(18) =  0.063771692
      exlte(19) =  0.049413224
      exlte(20) =  0.038414446
      exlte(21) =  0.02980617
      exlte(22) =  0.022057343
      exlte(23) =  0.014880846
      exlte(24) =  0.009333474
      exlte(25) =  0.007073325
c     Ion temperature [eV]- drvr_pedxax.pro (after profile fitting)	 

      xti(1)  =  1.044109388e+3
      xti(2)  =  1.026594464e+3
      xti(3)  =  1.008669344e+3
      xti(4)  =  0.990250836e+3
      xti(5)  =  0.971255748e+3
      xti(6)  =  0.951600889e+3
      xti(7)  =  0.931203069e+3
      xti(8)  =  0.909979094e+3
      xti(9)  =  0.887845773e+3
      xti(10) =  0.864719916e+3
      xti(11) =  0.840518331e+3
      xti(12) =  0.815157826e+3
      xti(13) =  0.788555209e+3
      xti(14) =  0.76062729e+3
      xti(15) =  0.731290877e+3
      xti(16) =  0.700462778e+3
      xti(17) =  0.668059801e+3
      xti(18) =  0.633998756e+3
      xti(19) =  0.598196451e+3
      xti(20) =  0.560569694e+3
      xti(21) =  0.521035295e+3
      xti(22) =  0.47951006e+3
      xti(23) =  0.4359108e+3
      xti(24) =  0.390154322e+3
      xti(25) =  0.342157435e+3
c     Ion temperature gradient scale length [m]- 
c     drvr_pedxax.pro (after profile fitting)
c     exlti = [(1/Ti)dTi/dr]^-1

      exlti(1) =  0.297792188
      exlti(2) =  0.282221392
      exlti(3) =  0.267238929
      exlti(4) =  0.252820797
      exlti(5) =  0.238930857
      exlti(6) =  0.225523966
      exlti(7) =  0.21254911
      exlti(8) =  0.199952322
      exlti(9) =  0.187679177
      exlti(10) = 0.175676788
      exlti(11) = 0.16389529
      exlti(12) = 0.152288835
      exlti(13) = 0.140816168
      exlti(14) = 0.129440861
      exlti(15) = 0.118131295
      exlti(16) = 0.106860447
      exlti(17) = 0.095604167
      exlti(18) = 0.084270244
      exlti(19) = 0.072681794
      exlti(20) = 0.060689226
      exlti(21) = 0.048174873
      exlti(22) = 0.035069106
      exlti(23) = 0.022462274
      exlti(24) = 0.012905681
      exlti(25) = 0.008604923
c     Electron temperature [eV]- drvr_pedxax.pro (after profile fitting)

      xte(1)  = 0.86049812e+3
      xte(2)  = 0.832842698e+3
      xte(3)  = 0.808751806e+3
      xte(4)  = 0.787660355e+3
      xte(5)  = 0.769003261e+3
      xte(6)  = 0.752215436e+3
      xte(7)  = 0.736731797e+3
      xte(8)  = 0.721987255e+3
      xte(9)  = 0.707416726e+3
      xte(10) = 0.692455123e+3
      xte(11) = 0.676537361e+3
      xte(12) = 0.659098352e+3
      xte(13) = 0.639573012e+3
      xte(14) = 0.617396255e+3
      xte(15) = 0.592002994e+3
      xte(16) = 0.562828143e+3
      xte(17) = 0.529298852e+3
      xte(18) = 0.490422059e+3
      xte(19) = 0.444578369e+3
      xte(20) = 0.390097324e+3
      xte(21) = 0.32530847e+3
      xte(22) = 0.248688955e+3
      xte(23) = 0.167209179e+3
      xte(24) = 0.100799426e+3
      xte(25) = 0.070481231e+3
c     safety factor [1]- EFITtools --> profiles_1_mse
c     need to manually input for GTEDGE rho vector

	qedge(1)  = 3.080537901
	qedge(2)  = 3.133282087
	qedge(3)  = 3.187807085
	qedge(4)  = 3.244145692
	qedge(5)  = 3.302330705
	qedge(6)  = 3.362394922
	qedge(7)  = 3.424371137
	qedge(8)  = 3.488292149
	qedge(9)  = 3.554190755
	qedge(10) = 3.62209975
	qedge(11) = 3.692051932
	qedge(12) = 3.764080097
	qedge(13) = 3.838217042
	qedge(14) = 3.914495565
	qedge(15) = 3.992948461
	qedge(16) = 4.073608528
	qedge(17) = 4.15656614
	qedge(18) = 4.245027617
	qedge(19) = 4.346843685
	qedge(20) = 4.47024369
	qedge(21) = 4.623456978
	qedge(22) = 4.814627167
	qedge(23) = 5.046965043
	qedge(24) = 5.316154363
	qedge(25) = 5.617245093
c     Radial Electric Field [V/m]- drvr_pedxax.pro (after profile fitting).

      erex(1)  = 34.46387387e3
      erex(2)  = 33.93774082e3
      erex(3)  = 33.15524378e3
      erex(4)  = 32.1367509e3
      erex(5)  = 30.90263032e3
      erex(6)  = 29.47325018e3
      erex(7)  = 27.86897862e3
      erex(8)  = 26.11018377e3
      erex(9)  = 24.21723379e3
      erex(10) = 22.21049679e3
      erex(11) = 20.11034094e3
      erex(12) = 17.93713436e3
      erex(13) = 15.7112452e3
      erex(14) = 13.45304159e3
      erex(15) = 11.18289167e3
      erex(16) = 8.921163596e3
      erex(17) = 6.683945371e3
      erex(18) = 4.255697235e3
      erex(19) = 1.075631364e3
      erex(20) = -3.445185082e3
      erex(21) = -9.700364421e3
      erex(22) = -17.30041956e3
      erex(23) = -24.3940652e3
      erex(24) = -27.18215426e3
      erex(25) = -21.70152582e3
c     Carbon Poloidal Velocity [m/s]- drvr_pedxax.pro (after profile fitting)
c	rh sign convention + is down at outer midplane


      vthexp(1)  = -3.090737583e3
      vthexp(2)  = -2.823026325e3
      vthexp(3)  = -2.700242572e3
      vthexp(4)  = -2.560150583e3
      vthexp(5)  = -2.415405039e3
      vthexp(6)  = -2.341734787e3
      vthexp(7)  = -2.315716219e3
      vthexp(8)  = -2.372376429e3
      vthexp(9)  = -2.544178286e3
      vthexp(10) = -2.758444286e3
      vthexp(11) = -2.684414667e3
      vthexp(12) = -2.663214952e3
      vthexp(13) = -2.737244571e3
      vthexp(14) = -2.828232254e3
      vthexp(15) = -2.926938413e3
      vthexp(16) = -3.025644571e3
      vthexp(17) = -3.296104381e3
      vthexp(18) = -3.888341333e3
      vthexp(19) = -4.380133714e3
      vthexp(20) = -4.824311429e3
      vthexp(21) = -5.268489143e3
      vthexp(22) = -5.896815238e3
      vthexp(23) = -6.883876825e3
      vthexp(24) = -3.789643714e3
      vthexp(25) = 1.3184e3
c     Carbon Toroidal Velocity [m/s]- drvr_pedxax.pro (after profile fitting)


      torv(1)  =  33.66105666e+3
      torv(2)  =  32.69372209e+3
      torv(3)  =  31.74237667e+3
      torv(4)  =  30.80765382e+3
      torv(5)  =  29.89018578e+3
      torv(6)  =  28.99060362e+3
      torv(7)  =  28.1095372e+3
      torv(8)  =  27.24761519e+3
      torv(9)  =  26.40546511e+3
      torv(10) =  25.58371325e+3
      torv(11) =  24.78298474e+3
      torv(12) =  24.00390351e+3
      torv(13) =  23.24709232e+3
      torv(14) =  22.51317272e+3
      torv(15) =  21.8027651e+3
      torv(16) =  21.11648865e+3
      torv(17) =  20.45496136e+3
      torv(18) =  19.81880007e+3
      torv(19) =  19.20862039e+3
      torv(20) =  18.62503677e+3
      torv(21) =  18.06866247e+3
      torv(22) =  17.54010957e+3
      torv(23) =  17.03998894e+3
      torv(24) =  16.56891029e+3
      torv(25) =  16.12748213e+3
c     Density time derivative correction [#/m^3-s]- drvr_pedtimed.pro (after profile fitting)
	dlnn_dt(1)  = 0
	dlnn_dt(2)  = 0
	dlnn_dt(3)  = 0
	dlnn_dt(4)  = 0
	dlnn_dt(5)  = 0
	dlnn_dt(6)  = 0
	dlnn_dt(7)  = 0
	dlnn_dt(8)  = 0
	dlnn_dt(9)  = 0
	dlnn_dt(10) = 0
	dlnn_dt(11) = 0
	dlnn_dt(12) = 0
	dlnn_dt(13) = 0
	dlnn_dt(14) = 0
	dlnn_dt(15) = 0
	dlnn_dt(16) = 0
	dlnn_dt(17) = 0
	dlnn_dt(18) = 0
	dlnn_dt(19) = 0
	dlnn_dt(20) = 0
	dlnn_dt(21) = 0
	dlnn_dt(22) = 0
	dlnn_dt(23) = 0
	dlnn_dt(24) = 0
	dlnn_dt(25) = 0
c     Electron temperature time derivative correction [eV/s]- drvr_pedtimed.pro (after profile fitting)
	dlnwe_dt(1)  = 0
	dlnwe_dt(2)  = 0
	dlnwe_dt(3)  = 0
	dlnwe_dt(4)  = 0
	dlnwe_dt(5)  = 0
	dlnwe_dt(6)  = 0
	dlnwe_dt(7)  = 0
	dlnwe_dt(8)  = 0
	dlnwe_dt(9)  = 0
	dlnwe_dt(10) = 0
	dlnwe_dt(11) = 0
	dlnwe_dt(12) = 0
	dlnwe_dt(13) = 0
	dlnwe_dt(14) = 0
	dlnwe_dt(15) = 0
	dlnwe_dt(16) = 0
	dlnwe_dt(17) = 0
	dlnwe_dt(18) = 0
	dlnwe_dt(19) = 0
	dlnwe_dt(20) = 0
	dlnwe_dt(21) = 0
	dlnwe_dt(22) = 0
	dlnwe_dt(23) = 0
	dlnwe_dt(24) = 0
	dlnwe_dt(25) = 0
c     Ion temperature time derivative correction [eV/s]- drvr_pedtimed.pro (after profile fitting)
	dlnwi_dt(1)  = 0
	dlnwi_dt(2)  = 0
	dlnwi_dt(3)  = 0
	dlnwi_dt(4)  = 0
	dlnwi_dt(5)  = 0
	dlnwi_dt(6)  = 0
	dlnwi_dt(7)  = 0
	dlnwi_dt(8)  = 0
	dlnwi_dt(9)  = 0
	dlnwi_dt(10) = 0
	dlnwi_dt(11) = 0
	dlnwi_dt(12) = 0
	dlnwi_dt(13) = 0
	dlnwi_dt(14) = 0
	dlnwi_dt(15) = 0
	dlnwi_dt(16) = 0
	dlnwi_dt(17) = 0
	dlnwi_dt(18) = 0
	dlnwi_dt(19) = 0
	dlnwi_dt(20) = 0
	dlnwi_dt(21) = 0
	dlnwi_dt(22) = 0
	dlnwi_dt(23) = 0
	dlnwi_dt(24) = 0
	dlnwi_dt(25) = 0

	if(ioptDdata.eq.1) then
c     Deuterium Toroidal Velocity [m/s]- drvr_pedxax.pro OR from personal contact (after profile fitting)


      vtord(1)  =  36.10370688e+3
      vtord(2)  =  35.76187605e+3
      vtord(3)  =  35.30193447e+3
      vtord(4)  =  34.74067729e+3
      vtord(5)  =  34.09542225e+3
      vtord(6)  =  33.38400967e+3
      vtord(7)  =  32.62480241e+3
      vtord(8)  =  31.83668593e+3
      vtord(9)  =  31.03906825e+3
      vtord(10) =  30.25187994e+3
      vtord(11) =  29.49557418e+3
      vtord(12) =  28.79112669e+3
      vtord(13) =  28.16003576e+3
      vtord(14) =  27.62432227e+3
      vtord(15) =  27.20652966e+3
      vtord(16) =  26.92972393e+3
      vtord(17) =  26.81791807e+3
      vtord(18) =  26.91876944e+3
      vtord(19) =  27.31570981e+3
      vtord(20) =  28.09698686e+3
      vtord(21) =  29.26347635e+3
      vtord(22) =  30.46542999e+3
      vtord(23) =  31.25854331e+3
      vtord(24) =  31.1877279e+3
      vtord(25) =  29.78711143e+3
c     Deuterium Poloidal Velocity [m/s]- drvr_pedxax.pro OR from personal contact (after profile fitting)
      vpold(1)  = -14239.81
      vpold(2)  = -14351.58
      vpold(3)  = -14276.21
      vpold(4)  = -14026.03
      vpold(5)  = -13614.66
      vpold(6)  = -13056.51
      vpold(7)  = -12366.38
      vpold(8)  = -11559.03
      vpold(9)  = -10648.98
      vpold(10) = -9650.331
      vpold(11) = -8576.611
      vpold(12) = -7440.738
      vpold(13) = -6254.953
      vpold(14) = -5030.761
      vpold(15) = -3778.849
      vpold(16) = -2508.947
      vpold(17) = -1227.467
      vpold(18) = 173.9702
      vpold(19) = 1972.389
      vpold(20) = 4453.774
      vpold(21) = 7788.24
      vpold(22) = 11678.62
      vpold(23) = 14991.61
      vpold(24) = 16142.38
      vpold(25) = 21515.64

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
      erextot(1)  = 3e30
      erextot(2)  = 3e30
      erextot(3)  = 3e30
      erextot(4)  = 3e30
      erextot(5)  = 3e30
      erextot(6)  = 3e30
      erextot(7)  = 3e30
      erextot(8)  = 3e30
      erextot(9)  = 3e30
      erextot(10) = 3e30
      erextot(11) = 3e30
      erextot(12) = 3e30
      erextot(13) = 3e30
      erextot(14) = 3e30
      erextot(15) = 3e30
      erextot(16) = 3e30
      erextot(17) = 3e30
      erextot(18) = 3e30
      erextot(19) = 3e30
      erextot(20) = 3e30
      erextot(21) = 3e30
      erextot(22) = 3e30
      erextot(23) = 3e30
      erextot(24) = 3e30
      erextot(25) = 3e30
      erextot(26) = 3e30
      erextot(27) = 3e30
      erextot(28) = 3e30
      erextot(29) = 3e30
      erextot(30) = 3e30
      erextot(31) = 3e30
      erextot(32) = 3e30
      erextot(33) = 3e30
      erextot(34) = 3e30
      erextot(35) = 3e30
      erextot(36) = 3e30
      erextot(37) = 3e30
      erextot(38) = 3e30
      erextot(39) = 3e30
      erextot(40) = 3e30
      erextot(41) = 3e30
      erextot(42) = 3e30
      erextot(43) = 3e30
      erextot(44) = 3e30
      erextot(45) = 3e30
      erextot(46) = 3e30
      erextot(47) = 3e30
      erextot(48) = 3e30
      erextot(49) = 3e30
      erextot(50) = 3e30
      erextot(51) = 3e30

c     Neutral beam deposition profile (hofr1) from NBeams (Dr. John Mandrekas)
c     units [#/s] - need to convert to GTEDGE rho coordinates using MATLAB script
c     nbeams2gtedge.m	from T.M.Wilks
      NBdep1(1)  =  3e30
      NBdep1(2)  =  3e30
      NBdep1(3)  =  3e30
      NBdep1(4)  =  3e30
      NBdep1(5)  =  3e30
      NBdep1(6)  =  3e30
      NBdep1(7)  =  3e30
      NBdep1(8)  =  3e30
      NBdep1(9)  =  3e30
      NBdep1(10) =  3e30
      NBdep1(11) =  3e30
      NBdep1(12) =  3e30
      NBdep1(13) =  3e30
      NBdep1(14) =  3e30
      NBdep1(15) =  3e30
      NBdep1(16) =  3e30
      NBdep1(17) =  3e30
      NBdep1(18) =  3e30
      NBdep1(19) =  3e30
      NBdep1(20) =  3e30
      NBdep1(21) =  3e30
      NBdep1(22) =  3e30
      NBdep1(23) =  3e30
      NBdep1(24) =  3e30
      NBdep1(25) =  3e30
      NBdep1(26) =  3e30
      NBdep1(27) =  3e30
      NBdep1(28) =  3e30
      NBdep1(29) =  3e30
      NBdep1(30) =  3e30
      NBdep1(31) =  3e30
      NBdep1(32) =  3e30
      NBdep1(33) =  3e30
      NBdep1(34) =  3e30
      NBdep1(35) =  3e30
      NBdep1(36) =  3e30
      NBdep1(37) =  3e30
      NBdep1(38) =  3e30
      NBdep1(39) =  3e30
      NBdep1(40) =  3e30
      NBdep1(41) =  3e30
      NBdep1(42) =  3e30
      NBdep1(43) =  3e30
      NBdep1(44) =  3e30
      NBdep1(45) =  3e30
      NBdep1(46) =  3e30
      NBdep1(47) =  3e30
      NBdep1(48) =  3e30
      NBdep1(49) =  3e30
      NBdep1(50) =  3e30
      NBdep1(51) =  3e30

      NBdep2(1)  = 3e30
      NBdep2(2)  = 3e30
      NBdep2(3)  = 3e30
      NBdep2(4)  = 3e30
      NBdep2(5)  = 3e30
      NBdep2(6)  = 3e30
      NBdep2(7)  = 3e30
      NBdep2(8)  = 3e30
      NBdep2(9)  = 3e30
      NBdep2(10) = 3e30
      NBdep2(11) = 3e30
      NBdep2(12) = 3e30
      NBdep2(13) = 3e30
      NBdep2(14) = 3e30
      NBdep2(15) = 3e30
      NBdep2(16) = 3e30
      NBdep2(17) = 3e30
      NBdep2(18) = 3e30
      NBdep2(19) = 3e30
      NBdep2(20) = 3e30
      NBdep2(21) = 3e30
      NBdep2(22) = 3e30
      NBdep2(23) = 3e30
      NBdep2(24) = 3e30
      NBdep2(25) = 3e30
      NBdep2(26) = 3e30
      NBdep2(27) = 3e30
      NBdep2(28) = 3e30
      NBdep2(29) = 3e30
      NBdep2(30) = 3e30
      NBdep2(31) = 3e30
      NBdep2(32) = 3e30
      NBdep2(33) = 3e30
      NBdep2(34) = 3e30
      NBdep2(35) = 3e30
      NBdep2(36) = 3e30
      NBdep2(37) = 3e30
      NBdep2(38) = 3e30
      NBdep2(39) = 3e30
      NBdep2(40) = 3e30
      NBdep2(41) = 3e30
      NBdep2(42) = 3e30
      NBdep2(43) = 3e30
      NBdep2(44) = 3e30
      NBdep2(45) = 3e30
      NBdep2(46) = 3e30
      NBdep2(47) = 3e30
      NBdep2(48) = 3e30
      NBdep2(49) = 3e30
      NBdep2(50) = 3e30
      NBdep2(51) = 3e30

      NBdep3(1)  =  3e30
      NBdep3(2)  =  3e30
      NBdep3(3)  =  3e30
      NBdep3(4)  =  3e30
      NBdep3(5)  =  3e30
      NBdep3(6)  =  3e30
      NBdep3(7)  =  3e30
      NBdep3(8)  =  3e30
      NBdep3(9)  =  3e30
      NBdep3(10) =  3e30
      NBdep3(11) =  3e30
      NBdep3(12) =  3e30
      NBdep3(13) =  3e30
      NBdep3(14) =  3e30
      NBdep3(15) =  3e30
      NBdep3(16) =  3e30
      NBdep3(17) =  3e30
      NBdep3(18) =  3e30
      NBdep3(19) =  3e30
      NBdep3(20) =  3e30
      NBdep3(21) =  3e30
      NBdep3(22) =  3e30
      NBdep3(23) =  3e30
      NBdep3(24) =  3e30
      NBdep3(25) =  3e30
      NBdep3(26) = 3e30
      NBdep3(27) = 3e30
      NBdep3(28) = 3e30
      NBdep3(29) = 3e30
      NBdep3(30) = 3e30
      NBdep3(31) = 3e30
      NBdep3(32) = 3e30
      NBdep3(33) = 3e30
      NBdep3(34) = 3e30
      NBdep3(35) = 3e30
      NBdep3(36) = 3e30
      NBdep3(37) = 3e30
      NBdep3(38) = 3e30
      NBdep3(39) = 3e30
      NBdep3(40) = 3e30
      NBdep3(41) = 3e30
      NBdep3(42) = 3e30
      NBdep3(43) = 3e30
      NBdep3(44) = 3e30
      NBdep3(45) = 3e30
      NBdep3(46) = 3e30
      NBdep3(47) = 3e30
      NBdep3(48) = 3e30
      NBdep3(49) = 3e30
      NBdep3(50) = 3e30
      NBdep3(51) = 3e30

c      cosine of angle between injected NB particle and Bphi (NBeams output)
      fpsi0(1)  = 3e30
      fpsi0(2)  = 3e30
      fpsi0(3)  = 3e30
      fpsi0(4)  = 3e30
      fpsi0(5)  = 3e30
      fpsi0(6)  = 3e30
      fpsi0(7)  = 3e30
      fpsi0(8)  = 3e30
      fpsi0(9)  = 3e30
      fpsi0(10) = 3e30
      fpsi0(11) = 3e30
      fpsi0(12) = 3e30
      fpsi0(13) = 3e30
      fpsi0(14) = 3e30
      fpsi0(15) = 3e30
      fpsi0(16) = 3e30
      fpsi0(17) = 3e30
      fpsi0(18) = 3e30
      fpsi0(19) = 3e30
      fpsi0(20) = 3e30
      fpsi0(21) = 3e30
      fpsi0(22) = 3e30
      fpsi0(23) = 3e30
      fpsi0(24) = 3e30
      fpsi0(25) = 3e30
      fpsi0(26) = 3e30
      fpsi0(27) = 3e30
      fpsi0(28) = 3e30
      fpsi0(29) = 3e30
      fpsi0(30) = 3e30
      fpsi0(31) = 3e30
      fpsi0(32) = 3e30
      fpsi0(33) = 3e30
      fpsi0(34) = 3e30
      fpsi0(35) = 3e30
      fpsi0(36) = 3e30
      fpsi0(37) = 3e30
      fpsi0(38) = 3e30
      fpsi0(39) = 3e30
      fpsi0(40) = 3e30
      fpsi0(41) = 3e30
      fpsi0(42) = 3e30
      fpsi0(43) = 3e30
      fpsi0(44) = 3e30
      fpsi0(45) = 3e30
      fpsi0(46) = 3e30
      fpsi0(47) = 3e30
      fpsi0(48) = 3e30
      fpsi0(49) = 3e30
      fpsi0(50) = 3e30
      fpsi0(51) = 3e30
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

	rmajor = 1.76
	aminor = 0.573
	elong = 1.799
	triang = 0.42
	plasmacur = 1.275
   	B = 2.008
	bphi = -2.008
	q95 = 3.884
	pbeam = 3.
	Rx = 1.375
	zx = -1.075
	Rsep1 = 1.057
	rsep2 = 1.486
	zsep1 = -1.26
	zsep2 = -1.25
	ssi95 = 4.167
	pohmin = .429
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
      xnpedex = 0.59859e+20
	xnsepex = 0.3146e+19
	tpedexi = 1256.
	tsepexi = 525.
	tpedexe = 527.
	tsepexe = 26.7
	widthnx = .12
	widthtex = .073
	widthtix = .019
	gradnbar(3) = .067
	gradnbar(4) = .067
	gradTbar (3) = .24
 	gradTbar (4) = .24
	gradTebar(3) = .04
	gradTebar(4) = .04
	aped = 0.883
	xnctrped = 1.196
	tctrped	 = 3419.


c pedestal sensitivity

c      xnpedex = 0.7e19
c	xnsepex = 0.2e19
c	tpedexi = 600.
c	tsepexi = 300.
c	tpedexe = 250.
c	tsepexe = 30.
c	widthnx = .21
c	widthtex = .2
c	widthtix = .2
c	gradnbar(3) = .167
c	gradnbar(4) = .167
c	gradTbar (3) = .24
c	gradTbar (4) = .24
c	gradTebar(3) = .168
c	gradTebar(4) = .168
c	aped = 0.55
c	xnctrped = 2.16
c	tctrped	 = 9.6

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
	
	
