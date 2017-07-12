      
	  SUBROUTINE SOLDATA(nbdep1,nbdep2,nbdep3,erextot,fpsi0,fb)

c*****************************************************************
c     Reads data in from text files and defines edge profiles from 
c     the DIII-D database
c*****************************************************************

	INCLUDE 'SOLDIV.FI'

	real ibal,imod,ioptsep,vplas,ioptgeom,dchirat,changeouter,tw,
	1     gammid,xlmid,czd(25),cmid,delsol,rwall,ioptdel,ioptpump,
	2     hxpt,riol,bpiol,btiol,psireal,vpold,vtord,exlvpold(2),
	3	  exlvtord(25)
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
c	ioptXBTH - Experimental interpolated btheta values	
c	ioptbal - GTEDGE balance run															
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
c	ioptCIRC = 1
c	ioptXBTH = 0
c	ioptbal=0
	
	RLOSSIOL = 0.5
	NBIspin = 1.0
	NBIeloss = 0.0
c******************************************************
c    Begin shot specific input data
c******************************************************
c	 shot:144977 @ 3000, type: H-mode,  Date: 3/21/2017

c     Electron density [#/m^3]- drvr_pedxax.pro (after profile fitting) 
	  EXNE(1) =  0.55687864E+20
	  EXNE(2) =  0.55648638E+20
	  EXNE(3) =  0.55590002E+20
	  EXNE(4) =  0.55510769E+20
	  EXNE(5) =  0.55449333E+20
	  EXNE(6) =  0.55383019E+20
	  EXNE(7) =  0.55284267E+20
	  EXNE(8) =  0.55141487E+20
	  EXNE(9) =  0.54943092E+20
	  EXNE(10) = 0.54686436E+20
	  EXNE(11) = 0.54417069E+20
	  EXNE(12) = 0.54029594E+20
	  EXNE(13) = 0.53500835E+20
	  EXNE(14) = 0.52772283E+20
	  EXNE(15) = 0.51747793E+20
	  EXNE(16) = 0.50267879E+20
	  EXNE(17) = 0.48152580E+20
	  EXNE(18) = 0.45184665E+20 
	  EXNE(19) = 0.41180386E+20
	  EXNE(20) = 0.36137291E+20
	  EXNE(21) = 0.30330713E+20
	  EXNE(22) = 0.24330823E+20
	  EXNE(23) = 0.18800995E+20
	  EXNE(24) = 0.14224530E+20
	  EXNE(25) = 0.10760898E+20
c     Electron density gradient scale length [m]-
c     drvr_pedxax.pro (after profile fitting)
c     xlne = [(1/n)dn/dr]^-1  
	  XLNE(1) =     9.9965666 
	  XLNE(2) =     4.5723799 
	  XLNE(3) =     2.8438846 
	  XLNE(4) =     1.9952446 
	  XLNE(5) =     1.4917716 
	  XLNE(6) =     1.1582240 
	  XLNE(7) =    0.90700236 
	  XLNE(8) =    0.71343374 
	  XLNE(9) =    0.55956803 			 
	  XLNE(10) =   0.43178362 
	  XLNE(11) =   0.32517364 
	  XLNE(12) =   0.23981450 
	  XLNE(13) =   0.17035325 
	  XLNE(14) =   0.11771825 
	  XLNE(15) =  0.079595766 
	  XLNE(16) =  0.052959503 
	  XLNE(17) =  0.035081322 
	  XLNE(18) =  0.023552569 
	  XLNE(19) =  0.016280064 
	  XLNE(20) =  0.011578996 
	  XLNE(21) = 0.0085432690 
	  XLNE(22) = 0.0067080995 
	  XLNE(23) = 0.0056018137 
	  XLNE(24) = 0.0050678377 
	  XLNE(25) = 0.0050383094 
c     Toroidal Velocity gradient scale length [m]- 
c     drvr_pedxax.pro (after profile fitting)
c     exlv = [(1/v)dv/dr]^-1	
	  exlv(1) =  0.17072141
	  exlv(2) =  0.15261647
	  exlv(3) =  0.13402982
	  exlv(4) =  0.11698101
	  exlv(5) =  0.10291042
	  exlv(6) = 0.088905320
	  exlv(7) = 0.077629134
	  exlv(8) = 0.067498937
	  exlv(9) = 0.059805382
	  exlv(10) =0.053994283
	  exlv(11) =0.049548291
	  exlv(12) =0.045987923
	  exlv(13) =0.043271292
	  exlv(14) =0.041339435
	  exlv(15) =0.039868213
	  exlv(16) =0.039152659
	  exlv(17) =0.039282035
	  exlv(18) =0.039105825
	  exlv(19) =0.037708603
	  exlv(20) =0.035194296
	  exlv(21) =0.030929955
	  exlv(22) =0.026153525
	  exlv(23) =0.021311691
	  exlv(24) =0.016735684
	  exlv(25) =0.013927102 
c     Electron temperature gradient scale length [m] - 
c     drvr_pedxax.pro (after profile fitting)
c     exlte = [(1/Te)dTe/dr]^-1
	  EXLTE(1) =    0.18011623
	  EXLTE(2) =    0.17928343
	  EXLTE(3) =    0.17939225
	  EXLTE(4) =    0.17895137
	  EXLTE(5) =    0.17877984
	  EXLTE(6) =    0.17956233
	  EXLTE(7) =    0.17821562
	  EXLTE(8) =    0.17626455
	  EXLTE(9) =    0.17299857
	  EXLTE(10) =   0.16681242
	  EXLTE(11) =   0.15619546
	  EXLTE(12) =   0.14123997
	  EXLTE(13) =   0.11964992
	  EXLTE(14) =  0.094610032
	  EXLTE(15) =  0.069755750
	  EXLTE(16) =  0.048396304
	  EXLTE(17) =  0.032336613
	  EXLTE(18) =  0.021487360
	  EXLTE(19) =  0.014617266
	  EXLTE(20) =  0.010281877
	  EXLTE(21) = 0.0075912163
	  EXLTE(22) = 0.0060485716
	  EXLTE(23) = 0.0051862280
	  EXLTE(24) = 0.0048468134
	  EXLTE(25) = 0.0049662349
c     Ion temperature [eV]- drvr_pedxax.pro (after profile fitting)	 
	  XTI(1) =  0.74094872E+3 
	  XTI(2) =  0.73023354E+3 
	  XTI(3) =  0.71944092E+3 
	  XTI(4) =  0.70858054E+3 
	  XTI(5) =  0.69763809E+3 
	  XTI(6) =  0.68662822E+3 
	  XTI(7) =  0.67554466E+3 
	  XTI(8) =  0.66438472E+3 
	  XTI(9) =  0.65315682E+3 						  
	  XTI(10) = 0.64185385E+3 
	  XTI(11) = 0.63047836E+3 
	  XTI(12) = 0.61903298E+3 
	  XTI(13) = 0.60751790E+3 
	  XTI(14) = 0.59592837E+3 
	  XTI(15) = 0.58426877E+3 
	  XTI(16) = 0.57254035E+3 
	  XTI(17) = 0.56074358E+3 
	  XTI(18) = 0.54887615E+3 
	  XTI(19) = 0.53693721E+3 
	  XTI(20) = 0.52493084E+3 
	  XTI(21) = 0.51285603E+3 
	  XTI(22) = 0.50071288E+3 
	  XTI(23) = 0.48849962E+3 
	  XTI(24) = 0.47622110E+3 
	  XTI(25) = 0.46387507E+3 
c     Ion temperature gradient scale length [m]- 
c     drvr_pedxax.pro (after profile fitting)
c     exlti = [(1/Ti)dTi/dr]^-1
	  EXLTI(1) =   0.15323174
	  EXLTI(2) =   0.14814571
	  EXLTI(3) =   0.14381362
	  EXLTI(4) =   0.13905782
	  EXLTI(5) =   0.13460220
	  EXLTI(6) =   0.13110378
	  EXLTI(7) =   0.12642072
	  EXLTI(8) =   0.12202327
	  EXLTI(9) =   0.11792668
	  EXLTI(10) =  0.11376067
	  EXLTI(11) =  0.10942961
	  EXLTI(12) =  0.10595480
	  EXLTI(13) =  0.10174920
	  EXLTI(14) = 0.097731292
	  EXLTI(15) = 0.093968555
	  EXLTI(16) = 0.090213031
	  EXLTI(17) = 0.086528115
	  EXLTI(18) = 0.083305307
	  EXLTI(19) = 0.080529720
	  EXLTI(20) = 0.076812118
	  EXLTI(21) = 0.071763687
	  EXLTI(22) = 0.066531524
	  EXLTI(23) = 0.060707476
	  EXLTI(24) = 0.055403501
	  EXLTI(25) = 0.055106960
c     Electron temperature [eV]- drvr_pedxax.pro (after profile fitting)					
	  XTE(1) = 0.68198240E+3
	  XTE(2) = 0.67370585E+3
	  XTE(3) = 0.66558909E+3
	  XTE(4) = 0.65764498E+3
	  XTE(5) = 0.64986821E+3
	  XTE(6) = 0.64224877E+3
	  XTE(7) = 0.63477986E+3
	  XTE(8) = 0.62743337E+3
	  XTE(9) = 0.62012786E+3							 
	  XTE(10) =0.61285453E+3
	  XTE(11) =0.60530463E+3
	  XTE(12) =0.59743211E+3
	  XTE(13) =0.58854658E+3
	  XTE(14) =0.57800730E+3
	  XTE(15) =0.56472433E+3
	  XTE(16) =0.54690233E+3
	  XTE(17) =0.52192057E+3
	  XTE(18) =0.48690598E+3
	  XTE(19) =0.43947376E+3
	  XTE(20) =0.37959103E+3
	  XTE(21) =0.31158226E+3
	  XTE(22) =0.24351319E+3
	  XTE(23) =0.18360662E+3
	  XTE(24) =0.13649659E+3
	  XTE(25) =0.10241394E+3
c     safety factor [1]- EFITtools --> profiles_1_mse
c     need to manually input for GTEDGE rho vector
	  qedge(1) =  4.03
	  qedge(2) =  4.1
	  qedge(3) =  4.16
	  qedge(4) =  4.23
	  qedge(5) =  4.3
	  qedge(6) =  4.37
	  qedge(7) =  4.44
	  qedge(8) =  4.52
	  qedge(9) =  4.6
	  qedge(10) = 4.66
	  qedge(11) = 4.72
	  qedge(12) = 4.81
	  qedge(13) = 4.89
	  qedge(14) = 5
	  qedge(15) = 5.08
	  qedge(16) = 5.19
	  qedge(17) = 5.28
	  qedge(18) = 5.39
	  qedge(19) = 5.51
	  qedge(20) = 5.64
	  qedge(21) = 5.79
	  qedge(22) = 5.98
	  qedge(23) = 6.2
	  qedge(24) = 6.38
	  qedge(25) = 6.58
c     Radial Electric Field [V/m]- drvr_pedxax.pro (after profile fitting)
	  erex(1) =    20.174169E+3
	  erex(2) =    19.653452E+3
	  erex(3) =    19.081243E+3
	  erex(4) =    18.471576E+3
	  erex(5) =    17.794390E+3
	  erex(6) =    17.024023E+3
	  erex(7) =    16.128698E+3
	  erex(8) =    15.086741E+3
	  erex(9) =    13.872391E+3
	  erex(10) =   12.459514E+3
	  erex(11) =   10.764598E+3
	  erex(12) =   8.6785521E+3
	  erex(13) =   6.1083516E+3
	  erex(14) =   2.9996496E+3
	  erex(15) = -0.62748525E+3
	  erex(16) =  -4.5480835E+3
	  erex(17) =  -8.7162776E+3
	  erex(18) =  -12.732815E+3
	  erex(19) =  -15.744406E+3
	  erex(20) =  -18.084364E+3
	  erex(21) =  -19.290682E+3
	  erex(22) =  -19.398581E+3
	  erex(23) =  -17.503655E+3
	  erex(24) =  -13.542227E+3
	  erex(25) =  -6.8068230E+3
c     Carbon Poloidal Velocity [m/s]- drvr_pedxax.pro (after profile fitting)
c	rh sign convention + is down at outer midplane
	  vthexp(1) =   -3.3623267E+3
	  vthexp(2) =   -3.4073876E+3
	  vthexp(3) =   -3.4499055E+3
	  vthexp(4) =   -3.4899739E+3
	  vthexp(5) =   -3.5270985E+3
	  vthexp(6) =   -3.5583610E+3
	  vthexp(7) =   -3.5899476E+3
	  vthexp(8) =   -3.6280750E+3
	  vthexp(9) =   -3.6782590E+3
	  vthexp(10) =  -3.7560379E+3
	  vthexp(11) =  -3.8732421E+3
	  vthexp(12) =  -4.0411899E+3
	  vthexp(13) =  -4.2725385E+3
	  vthexp(14) =  -4.5786074E+3
	  vthexp(15) =  -4.9714471E+3
	  vthexp(16) =  -5.4631625E+3
	  vthexp(17) =  -6.0654055E+3
	  vthexp(18) =  -6.7903106E+3
	  vthexp(19) =  -7.6482443E+3
	  vthexp(20) =  -8.5230310E+3
	  vthexp(21) =  -9.0868254E+3
	  vthexp(22) =  -8.9437296E+3
	  vthexp(23) =  -7.7619072E+3
	  vthexp(24) =  -5.1713039E+3
	  vthexp(25) = -0.80954235E+3
c     Carbon Toroidal Velocity [m/s]- drvr_pedxax.pro (after profile fitting)
	  torv(1) =  87.566823E+3
	  torv(2) =  86.382864E+3
	  torv(3) =  85.085100E+3
	  torv(4) =  83.640642E+3
	  torv(5) =  82.032134E+3
	  torv(6) =  80.244396E+3
	  torv(7) =  78.261733E+3
	  torv(8) =  76.065021E+3
	  torv(9) =  73.660709E+3
	  torv(10) = 71.090530E+3
	  torv(11) = 68.400100E+3
	  torv(12) = 65.633043E+3
	  torv(13) = 62.832657E+3
	  torv(14) = 60.043457E+3
	  torv(15) = 57.309609E+3 
	  torv(16) = 54.673603E+3 
	  torv(17) = 52.181135E+3 
	  torv(18) = 49.833351E+3
	  torv(19) = 47.568117E+3
	  torv(20) = 45.319561E+3
	  torv(21) = 43.021218E+3
	  torv(22) = 40.606535E+3
	  torv(23) = 38.007715E+3
	  torv(24) = 35.160702E+3
	  torv(25) = 31.997304E+3
c     Density time derivative correction [#/m^3-s]- drvr_pedtimed.pro (after profile fitting)	  
	  dlnn_dt(1) =  0
	  dlnn_dt(2) =  0
	  dlnn_dt(3) =  0
	  dlnn_dt(4) =  0
	  dlnn_dt(5) =  0
	  dlnn_dt(6) =  0
	  dlnn_dt(7) =  0
	  dlnn_dt(8) =  0
	  dlnn_dt(9) =  0
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
	  dlnwe_dt(1) = 0
	  dlnwe_dt(2) = 0
	  dlnwe_dt(3) = 0
	  dlnwe_dt(4) = 0
	  dlnwe_dt(5) = 0
	  dlnwe_dt(6) = 0
	  dlnwe_dt(7) = 0
	  dlnwe_dt(8) = 0
	  dlnwe_dt(9) = 0
	  dlnwe_dt(10) =0
	  dlnwe_dt(11) =0
	  dlnwe_dt(12) =0
	  dlnwe_dt(13) =0
	  dlnwe_dt(14) =0
	  dlnwe_dt(15) =0
	  dlnwe_dt(16) =0
	  dlnwe_dt(17) =0
	  dlnwe_dt(18) =0
	  dlnwe_dt(19) =0
	  dlnwe_dt(20) =0
	  dlnwe_dt(21) =0
	  dlnwe_dt(22) =0
	  dlnwe_dt(23) =0
	  dlnwe_dt(24) =0
	  dlnwe_dt(25) =0
c     Ion temperature time derivative correction [eV/s]- drvr_pedtimed.pro (after profile fitting)
	  dlnwi_dt(1) =  0
	  dlnwi_dt(2) =  0
	  dlnwi_dt(3) =  0
	  dlnwi_dt(4) =  0
	  dlnwi_dt(5) =  0
	  dlnwi_dt(6) =  0
	  dlnwi_dt(7) =  0
	  dlnwi_dt(8) =  0
	  dlnwi_dt(9) =  0
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
      vtord(1) =  0
      vtord(2) =  0
      vtord(3) =  0
      vtord(4) =  0
      vtord(5) =  0
      vtord(6) =  0
      vtord(7) =  0
      vtord(8) =  0
      vtord(9) =  0
      vtord(10) = 0
      vtord(11) = 0
      vtord(12) = 0
      vtord(13) = 0
      vtord(14) = 0
      vtord(15) = 0
      vtord(16) = 0
      vtord(17) = 0
      vtord(18) = 0
      vtord(19) = 0
      vtord(20) = 0
      vtord(21) = 0
      vtord(22) = 0
      vtord(23) = 0
      vtord(24) = 0
      vtord(25) = 0
c     Deuterium Poloidal Velocity [m/s]- drvr_pedxax.pro OR from personal contact (after profile fitting)
      vpold(1) =  0
      vpold(2) =  0
      vpold(3) =  0
      vpold(4) =  0
      vpold(5) =  0
      vpold(6) =  0
      vpold(7) =  0
      vpold(8) =  0
      vpold(9) =  0
      vpold(10) = 0
      vpold(11) = 0
      vpold(12) = 0
      vpold(13) = 0
      vpold(14) = 0
      vpold(15) = 0
      vpold(16) = 0
      vpold(17) = 0
      vpold(18) = 0
      vpold(19) = 0
      vpold(20) = 0
      vpold(21) = 0
      vpold(22) = 0
      vpold(23) = 0
      vpold(24) = 0
      vpold(25) = 0
	endif
	
	
c	if (ioptXBTH.eq.0) goto 400
c
c	xbth(1)  =  0
c	xbth(2)  =  0
c	xbth(3)  =  0
c	xbth(4)  =  0
c	xbth(5)  =  0
c	xbth(6)  =  0
c	xbth(7)  =  0
c	xbth(8)  =  0
c	xbth(9)  =  0
c	xbth(10) =  0
c	xbth(11) =  0
c	xbth(12) =  0
c	xbth(13) =  0
c	xbth(14) =  0
c	xbth(15) =  0
c	xbth(16) =  0
c	xbth(17) =  0
c	xbth(18) =  0
c	xbth(19) =  0
c	xbth(20) =  0
c	xbth(21) =  0
c	xbth(22) =  0
c	xbth(23) =  0
c	xbth(24) =  0
c	xbth(25) =  0

400   continue
	if (ioptFIOL.eq.0) goto	500
c*************************************************
c     Input for neutral beam fast IOL calculation
c     Requires full profiles for rho [0,1]
c     Requires output data from NBeams 
c**************************************************
c	 shot:144977 @ 3000,  type:H-mode,  date: 3/21/2017

c     Radial electric field [V/m] for rho [0,1], deltarho = 0.02
c     drvr_pedxax.pro (after profile fitting)
      erextot(1) =     4.2308228E+3
      erextot(2) =     5.6098144E+3
      erextot(3) =     7.5157929E+3
      erextot(4) =     9.6503447E+3
      erextot(5) =     11.974697E+3
      erextot(6) =     14.674421E+3
      erextot(7) =     17.874477E+3
      erextot(8) =     20.474083E+3
      erextot(9) =     22.944612E+3
      erextot(10) =    25.290900E+3
      erextot(11) =    27.427178E+3
      erextot(12) =    29.383442E+3
      erextot(13) =    31.140685E+3
      erextot(14) =    32.701479E+3
      erextot(15) =    34.061142E+3
      erextot(16) =    35.217366E+3
      erextot(17) =    36.174377E+3
      erextot(18) =    36.840332E+3
      erextot(19) =    37.270558E+3
      erextot(20) =    37.530476E+3
      erextot(21) =    37.556814E+3
      erextot(22) =    37.370142E+3
      erextot(23) =    37.014605E+3
      erextot(24) =    36.515131E+3
      erextot(25) =    35.936835E+3
      erextot(26) =    35.339893E+3
      erextot(27) =    34.670195E+3
      erextot(28) =    33.984388E+3
      erextot(29) =    33.313878E+3
      erextot(30) =    32.630300E+3
      erextot(31) =    31.946779E+3
      erextot(32) =    31.282813E+3
      erextot(33) =    30.635985E+3
      erextot(34) =    29.978743E+3
      erextot(35) =    29.272765E+3
      erextot(36) =    28.572052E+3
      erextot(37) =    27.853829E+3
      erextot(38) =    26.980750E+3
      erextot(39) =    26.052385E+3
      erextot(40) =    25.061744E+3
      erextot(41) =    23.979270E+3
      erextot(42) =    22.777540E+3
      erextot(43) =    21.406530E+3
      erextot(44) =    19.805615E+3
      erextot(45) =    17.755379E+3
      erextot(46) =    14.645273E+3
      erextot(47) =    9.3512280E+3
      erextot(48) =  -0.72295354E+3
      erextot(49) =   -13.940074E+3
      erextot(50) =   -19.633746E+3
      erextot(51) =   -6.8068230E+3

c     Neutral beam deposition profile (hofr1) from NBeams (Dr. John Mandrekas)
c     units [#/s] - need to convert to GTEDGE rho coordinates using MATLAB script
c     nbeams2gtedge.m	from T.M.Wilks
      NBdep1(1) =  .33884E+01
      NBdep1(2) =  .69976E+01
      NBdep1(3) =  .46200E+01
      NBdep1(4) =  .40910E+01
      NBdep1(5) =  .38046E+01
      NBdep1(6) =  .35969E+01
      NBdep1(7) =  .34063E+01
      NBdep1(8) =  .32280E+01
      NBdep1(9) =  .30513E+01
      NBdep1(10) = .28753E+01
      NBdep1(11) = .27034E+01
      NBdep1(12) = .25336E+01
      NBdep1(13) = .23686E+01
      NBdep1(14) = .22099E+01
      NBdep1(15) = .20599E+01
      NBdep1(16) = .19195E+01
      NBdep1(17) = .16895E+01
      NBdep1(18) = .15316E+01
      NBdep1(19) = .14151E+01
      NBdep1(20) = .13214E+01
      NBdep1(21) = .12432E+01
      NBdep1(22) = .11768E+01
      NBdep1(23) = .11192E+01
      NBdep1(24) = .10699E+01
      NBdep1(25) = .10253E+01
      NBdep1(26) = .98532E+00
      NBdep1(27) = .95078E+00
      NBdep1(28) = .92115E+00
      NBdep1(29) = .89538E+00
      NBdep1(30) = .87380E+00
      NBdep1(31) = .85755E+00
      NBdep1(32) = .84251E+00
      NBdep1(33) = .83036E+00
      NBdep1(34) = .82131E+00
      NBdep1(35) = .81511E+00
      NBdep1(36) = .81192E+00
      NBdep1(37) = .81005E+00
      NBdep1(38) = .81127E+00
      NBdep1(39) = .81006E+00
      NBdep1(40) = .80902E+00
      NBdep1(41) = .80842E+00
      NBdep1(42) = .80700E+00
      NBdep1(43) = .80216E+00
      NBdep1(44) = .79338E+00
      NBdep1(45) = .78387E+00
      NBdep1(46) = .76855E+00
      NBdep1(47) = .74234E+00
      NBdep1(48) = .68778E+00
      NBdep1(49) = .56207E+00
      NBdep1(50) = .32558E+00
      NBdep1(51) = .00000E+00

      NBdep2(1) =  .18542E+01
      NBdep2(2) =  .38362E+01
      NBdep2(3) =  .25469E+01
      NBdep2(4) =  .22759E+01
      NBdep2(5) =  .21428E+01
      NBdep2(6) =  .20593E+01
      NBdep2(7) =  .19854E+01
      NBdep2(8) =  .19203E+01
      NBdep2(9) =  .18559E+01
      NBdep2(10) = .17903E+01
      NBdep2(11) = .17255E+01
      NBdep2(12) = .16589E+01
      NBdep2(13) = .15915E+01
      NBdep2(14) = .15242E+01
      NBdep2(15) = .14584E+01
      NBdep2(16) = .13944E+01
      NBdep2(17) = .12692E+01
      NBdep2(18) = .11844E+01
      NBdep2(19) = .11222E+01
      NBdep2(20) = .10724E+01
      NBdep2(21) = .10307E+01
      NBdep2(22) = .99561E+00
      NBdep2(23) = .96559E+00
      NBdep2(24) = .94149E+00
      NBdep2(25) = .91996E+00
      NBdep2(26) = .90200E+00
      NBdep2(27) = .88830E+00
      NBdep2(28) = .87806E+00
      NBdep2(29) = .87061E+00
      NBdep2(30) = .86652E+00
      NBdep2(31) = .86774E+00
      NBdep2(32) = .86883E+00
      NBdep2(33) = .87220E+00
      NBdep2(34) = .87840E+00
      NBdep2(35) = .88645E+00
      NBdep2(36) = .89810E+00
      NBdep2(37) = .91109E+00
      NBdep2(38) = .92956E+00
      NBdep2(39) = .94387E+00
      NBdep2(40) = .95886E+00
      NBdep2(41) = .97533E+00
      NBdep2(42) = .99236E+00
      NBdep2(43) = .10059E+01
      NBdep2(44) = .10157E+01
      NBdep2(45) = .10276E+01
      NBdep2(46) = .10315E+01
      NBdep2(47) = .10253E+01
      NBdep2(48) = .98685E+00
      NBdep2(49) = .84092E+00
      NBdep2(50) = .49948E+00
      NBdep2(51) = .00000E+00

      NBdep3(1) =  .10977E+01
      NBdep3(2) =  .22749E+01
      NBdep3(3) =  .15182E+01
      NBdep3(4) =  .13682E+01
      NBdep3(5) =  .13030E+01
      NBdep3(6) =  .12711E+01
      NBdep3(7) =  .12457E+01
      NBdep3(8) =  .12275E+01
      NBdep3(9) =  .12102E+01
      NBdep3(10) = .11921E+01
      NBdep3(11) = .11744E+01
      NBdep3(12) = .11546E+01
      NBdep3(13) = .11328E+01
      NBdep3(14) = .11094E+01
      NBdep3(15) = .10852E+01
      NBdep3(16) = .10602E+01
      NBdep3(17) = .99199E+00
      NBdep3(18) = .94741E+00
      NBdep3(19) = .91558E+00
      NBdep3(20) = .89057E+00
      NBdep3(21) = .86986E+00
      NBdep3(22) = .85280E+00
      NBdep3(23) = .83890E+00
      NBdep3(24) = .82934E+00
      NBdep3(25) = .82143E+00
      NBdep3(26) = .81669E+00
      NBdep3(27) = .81565E+00
      NBdep3(28) = .81744E+00
      NBdep3(29) = .82164E+00
      NBdep3(30) = .82891E+00
      NBdep3(31) = .84163E+00
      NBdep3(32) = .85370E+00
      NBdep3(33) = .86795E+00
      NBdep3(34) = .88514E+00
      NBdep3(35) = .90362E+00
      NBdep3(36) = .92656E+00
      NBdep3(37) = .95105E+00
      NBdep3(38) = .98309E+00
      NBdep3(39) = .10100E+01
      NBdep3(40) = .10384E+01
      NBdep3(41) = .10695E+01
      NBdep3(42) = .11027E+01
      NBdep3(43) = .11331E+01
      NBdep3(44) = .11606E+01
      NBdep3(45) = .11937E+01
      NBdep3(46) = .12181E+01
      NBdep3(47) = .12348E+01
      NBdep3(48) = .12186E+01
      NBdep3(49) = .10663E+01
      NBdep3(50) = .64340E+00
      NBdep3(51) = .00000E+00

c      cosine of angle between injected NB particle and Bphi
      fpsi0(1) =  0.63640
      fpsi0(2) =  0.63626
      fpsi0(3) =  0.63585
      fpsi0(4) =  0.63516
      fpsi0(5) =  0.63421
      fpsi0(6) =  0.63298
      fpsi0(7) =  0.63153
      fpsi0(8) =  0.62984
      fpsi0(9) =  0.62794
      fpsi0(10) = 0.62580
      fpsi0(11) = 0.62347
      fpsi0(12) = 0.62092
      fpsi0(13) = 0.61816
      fpsi0(14) = 0.61519
      fpsi0(15) = 0.61204
      fpsi0(16) = 0.60871
      fpsi0(17) = 0.60342
      fpsi0(18) = 0.59872
      fpsi0(19) = 0.59445
      fpsi0(20) = 0.59035
      fpsi0(21) = 0.58642
      fpsi0(22) = 0.58260
      fpsi0(23) = 0.57889
      fpsi0(24) = 0.57521
      fpsi0(25) = 0.57166
      fpsi0(26) = 0.56817
      fpsi0(27) = 0.56477
      fpsi0(28) = 0.56141
      fpsi0(29) = 0.55813
      fpsi0(30) = 0.55490
      fpsi0(31) = 0.55170
      fpsi0(32) = 0.54856
      fpsi0(33) = 0.54549
      fpsi0(34) = 0.54248
      fpsi0(35) = 0.53948
      fpsi0(36) = 0.53656
      fpsi0(37) = 0.53369
      fpsi0(38) = 0.53083
      fpsi0(39) = 0.52804
      fpsi0(40) = 0.52532
      fpsi0(41) = 0.52264
      fpsi0(42) = 0.52001
      fpsi0(43) = 0.51745
      fpsi0(44) = 0.51497
      fpsi0(45) = 0.51253
      fpsi0(46) = 0.51025
      fpsi0(47) = 0.50823
      fpsi0(48) = 0.50639
      fpsi0(49) = 0.50440
      fpsi0(50) = 0.50181
      fpsi0(51) = 0.50181
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
c		144977 @ 3000ms	 H-mode
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
		pohmin = 0.429
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
	xnsepex = 0.374e+19
	tpedexi = 0.596E+3
	tsepexi = 0.461E+3
	tpedexe = 0.579E+3
	tsepexe = 0.065E+3
	widthnx = .117
	widthtex = .077
	widthtix = .066
	gradnbar(3) = .067
	gradnbar(4) = .067
	gradTbar (3) = .258
 	gradTbar (4) = .258
	gradTebar(3) = .048
	gradTebar(4) = .048
	aped = 0.939
	xnctrped = 1.82
	tctrped	 = 3.654

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
	
	