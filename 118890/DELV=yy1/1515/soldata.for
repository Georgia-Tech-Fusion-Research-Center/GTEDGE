      
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
      ioptDdata = 0
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
c	 shot:118890 @ 1515, type: L-mode,  Date: 06/16/2004

c     Electron density [#/m^3]- drvr_pedxax.pro (after profile fitting) 
	  EXNE(1) = 0.20695089E20
	  EXNE(2) = 0.20319742E20
	  EXNE(3) = 0.19928489E20
	  EXNE(4) = 0.19520953E20
	  EXNE(5) = 0.19097041E20
	  EXNE(6) = 0.18655904E20
	  EXNE(7) = 0.18196639E20
	  EXNE(8) = 0.17718566E20
	  EXNE(9) = 0.17220925E20
	  EXNE(10) = 0.16703068E20
	  EXNE(11) = 0.16163068E20
	  EXNE(12) = 0.15598707E20
	  EXNE(13) = 0.15006853E20
	  EXNE(14) = 0.14381589E20
	  EXNE(15) = 0.13713438E20
	  EXNE(16) = 0.12985441E20
	  EXNE(17) = 0.12169782E20
	  EXNE(18) = 0.11226581E20  
	  EXNE(19) = 0.10105621E20
	  EXNE(20) = 0.087743021E20
	  EXNE(21) = 0.072810664E20
	  EXNE(22) = 0.058041636E20
	  EXNE(23) = 0.045820554E20
	  EXNE(24) = 0.037429973E20
	  EXNE(25) = 0.032463599E20
c     Electron dnsity gradient scale length [m]-
c     drvr_pedxax.pro (after profile fitting)
c     xlne = [(1/n)dn/dr]^-1  
	  XLNE(1) = 0.14239778
	  XLNE(2) = 0.13303775
	  XLNE(3) = 0.12409494
	  XLNE(4) = 0.11513081
	  XLNE(5) = 0.10749467
	  XLNE(6) = 0.099858960
	  XLNE(7) = 0.092035725
	  XLNE(8) = 0.085361138
	  XLNE(9) = 0.078274584
	  XLNE(10) = 0.072492991
	  XLNE(11) = 0.066185220
	  XLNE(12) = 0.060343719
	  XLNE(13) = 0.054043333
	  XLNE(14) = 0.048126073
	  XLNE(15) = 0.041566698
	  XLNE(16) = 0.035211737
	  XLNE(17) = 0.028899376
	  XLNE(18) = 0.022407799
	  XLNE(19) = 0.016443163
	  XLNE(20) = 0.011719672
	  XLNE(21) = 0.0086640326
	  XLNE(22) = 0.0071305427
	  XLNE(23) = 0.0070960175
	  XLNE(24) = 0.0086328371
	  XLNE(25) = 0.012608786
c     Toroidal Velocity gradient scale length [m]- 
c     drvr_pedxax.pro (after profile fitting)
c     exlv = [(1/v)dv/dr]^-1	
	  exlv(1) = 0.34997505
	  exlv(2) = 0.72968745
	  exlv(3) = 43.268032
	  exlv(4) = -0.92147166
	  exlv(5) = -0.49795339
	  exlv(6) = -0.36754569
	  exlv(7) = -0.30871674
	  exlv(8) = -0.28461486
	  exlv(9) = -0.27827954
	  exlv(10) = -0.29317817
	  exlv(11) = -0.32919604
	  exlv(12) = -0.41229668
	  exlv(13) = -0.62572914
	  exlv(14) = -1.9826676
	  exlv(15) = 1.1617957
	  exlv(16) = 0.39706090
	  exlv(17) = 0.22446537
	  exlv(18) = 0.14785956
	  exlv(19) = 0.10407945
	  exlv(20) = 0.075442985
	  exlv(21) = 0.055781230
	  exlv(22) = 0.041491587
	  exlv(23) = 0.031369884
	  exlv(24) = 0.023637181
	  exlv(25) = 0.018295772
c     Electron temperature gradient scale length [m] - 
c     drvr_pedxax.pro (after profile fitting)
c     exlte = [(1/Te)dTe/dr]^-1
	  EXLTE(1) = 0.064998166
	  EXLTE(2) = 0.061796797
	  EXLTE(3) = 0.058578462
	  EXLTE(4) = 0.055135970
	  EXLTE(5) = 0.052135236
	  EXLTE(6) = 0.048959834
	  EXLTE(7) = 0.045518272
	  EXLTE(8) = 0.042495923
	  EXLTE(9) = 0.039137083
	  EXLTE(10) = 0.036321192
	  EXLTE(11) = 0.033158334
	  EXLTE(12) = 0.030177518
	  EXLTE(13) = 0.026959426
	  EXLTE(14) = 0.023981375
	  EXLTE(15) = 0.020792414
	  EXLTE(16) = 0.017872569
	  EXLTE(17) = 0.015167088
	  EXLTE(18) = 0.012509450
	  EXLTE(19) = 0.010160030
	  EXLTE(20) = 0.0084901189
	  EXLTE(21) = 0.0080707009
	  EXLTE(22) = 0.0098147799
	  EXLTE(23) = 0.016582574
	  EXLTE(24) = 0.032642401
	  EXLTE(25) = 0.044378908
c     Ion temperature [eV]- drvr_pedxax.pro (after profile fitting)	 
	  XTI(1) = 281.41074
	  XTI(2) = 277.91693
	  XTI(3) = 274.36657
	  XTI(4) = 270.73710
	  XTI(5) = 267.01518
	  XTI(6) = 263.16201
	  XTI(7) = 259.18703
	  XTI(8) = 255.04074
	  XTI(9) = 250.70941
	  XTI(10) = 246.18753
	  XTI(11) = 241.43732
	  XTI(12) = 236.44032
	  XTI(13) = 231.17777
	  XTI(14) = 225.63049
	  XTI(15) = 219.77871
	  XTI(16) = 213.60123
	  XTI(17) = 207.07525
	  XTI(18) = 200.17719
	  XTI(19) = 192.89046
	  XTI(20) = 185.19168
	  XTI(21) = 177.06442
	  XTI(22) = 168.48222
	  XTI(23) = 159.42711
	  XTI(24) = 149.87613
	  XTI(25) = 139.81006
c     Ion temperature gradient scale length [m]- 
c     drvr_pedxax.pro (after profile fitting)
c     exlti = [(1/Ti)dTi/dr]^-1
	  EXLTI(1) = 0.20524389
	  EXLTI(2) = 0.19809227
	  EXLTI(3) = 0.19009943
	  EXLTI(4) = 0.18056574
	  EXLTI(5) = 0.17189693
	  EXLTI(6) = 0.16211376
	  EXLTI(7) = 0.15120490
	  EXLTI(8) = 0.14154387
	  EXLTI(9) = 0.13076483
	  EXLTI(10) = 0.12186172
	  EXLTI(11) = 0.11200948
	  EXLTI(12) = 0.10305309
	  EXLTI(13) = 0.093644448
	  EXLTI(14) = 0.085472859
	  EXLTI(15) = 0.076967470
	  EXLTI(16) = 0.069873817
	  EXLTI(17) = 0.063852943
	  EXLTI(18) = 0.057716891
	  EXLTI(19) = 0.051344957
	  EXLTI(20) = 0.044736486
	  EXLTI(21) = 0.038468596
	  EXLTI(22) = 0.032525647
	  EXLTI(23) = 0.027513498
	  EXLTI(24) = 0.022942856
	  EXLTI(25) = 0.019521238
c     Electron temperature [eV]- drvr_pedxax.pro (after profile fitting)					
	  XTE(1) = 197.64032
	  XTE(2) = 189.93658
	  XTE(3) = 182.21711
	  XTE(4) = 174.47586
	  XTE(5) = 166.70741
	  XTE(6) = 158.90565
	  XTE(7) = 151.06380
	  XTE(8) = 143.17551
	  XTE(9) = 135.23314
	  XTE(10) = 127.22783
	  XTE(11) = 119.14962
	  XTE(12) = 110.98462
	  XTE(13) = 102.71335
	  XTE(14) = 94.308467
	  XTE(15) = 85.729880
	  XTE(16) = 76.923966
	  XTE(17) = 67.830374
	  XTE(18) = 58.423097
	  XTE(19) = 48.823340
	  XTE(20) = 39.504759
	  XTE(21) = 31.428167
	  XTE(22) = 25.647773
	  XTE(23) = 22.412440
	  XTE(24) = 20.939735
	  XTE(25) = 20.189745
c     safety factor [1]- EFITtools --> profiles_1_mse
c     need to manually input for GTEDGE rho vector
	  qedge(1) = 2.88
	  qedge(2) = 2.94
	  qedge(3) = 2.98
	  qedge(4) = 3.03
	  qedge(5) = 3.09
	  qedge(6) = 3.15
	  qedge(7) = 3.21
	  qedge(8) = 3.27
	  qedge(9) = 3.33
	  qedge(10) = 3.41
	  qedge(11) = 3.47
	  qedge(12) = 3.53
	  qedge(13) = 3.60
	  qedge(14) = 3.69
	  qedge(15) = 3.77
	  qedge(16) = 3.87
	  qedge(17) = 3.95
	  qedge(18) = 4.04
	  qedge(19) = 4.14
	  qedge(20) = 4.25
	  qedge(21) = 4.43
	  qedge(22) = 4.64
	  qedge(23) = 4.87
	  qedge(24) = 5.06
	  qedge(25) = 5.23
c     Radial Electric Field [V/m]- drvr_pedxax.pro (after profile fitting)
	  erex(1) = 5.0548771e3
	  erex(2) = 4.9678280e3
	  erex(3) = 4.8598506e3
	  erex(4) = 4.7282480e3
	  erex(5) = 4.5901591e3
	  erex(6) = 4.4717639e3
	  erex(7) = 4.3386633e3
	  erex(8) = 4.1920349e3
	  erex(9) = 4.0296394e3
	  erex(10) = 3.8606370e3
	  erex(11) = 3.6872791e3
	  erex(12) = 3.5079235e3
	  erex(13) = 3.3201876e3
	  erex(14) = 3.1209867e3
	  erex(15) = 2.9097069e3
	  erex(16) = 2.6519277e3
	  erex(17) = 2.3259155e3
	  erex(18) = 1.9021621e3
	  erex(19) = 1.2879184e3
	  erex(20) = 0.44629242e3
	  erex(21) = -0.51520935e3
	  erex(22) = -1.2267265e3
	  erex(23) = -1.2583559e3
	  erex(24) = -0.61705988e3
	  erex(25) = 0.21080849e3
c     Carbon Poloidal Velocity [m/s]- drvr_pedxax.pro (after profile fitting)
c	rh sign convention + is down at outer midplane
	  vthexp(1) = 0.83440220e3
	  vthexp(2) = 0.81189155e3
	  vthexp(3) = 0.77540049e3
	  vthexp(4) = 0.72667589e3
	  vthexp(5) = 0.66741877e3
	  vthexp(6) = 0.59937491e3
	  vthexp(7) = 0.52430394e3
	  vthexp(8) = 0.44396424e3
	  vthexp(9) = 0.36011914e3
	  vthexp(10) = 0.27452583e3
	  vthexp(11) = 0.18894800e3
	  vthexp(12) = 0.10514434e3
	  vthexp(13) = 0.024875613e3
	  vthexp(14) = -0.050098153e3
	  vthexp(15) = -0.11801469e3
	  vthexp(16) = -0.17711403e3
	  vthexp(17) = -0.22563583e3
	  vthexp(18) = -0.26184017e3
	  vthexp(19) = -0.28404080e3
	  vthexp(20) = -0.29036405e3
	  vthexp(21) = -0.27876445e3
	  vthexp(22) = -0.24794573e3
	  vthexp(23) = -0.19602309e3
	  vthexp(24) = -0.12119632e3
	  vthexp(25) = -0.021706135e3
c     Carbon Toroidal Velocity [m/s]- drvr_pedxax.pro (after profile fitting)
	  torv(1) = 11.175998e3
	  torv(2) = 11.117617e3
	  torv(3) = 11.101750e3
	  torv(4) = 11.120994e3
	  torv(5) = 11.164253e3
	  torv(6) = 11.227710e3
	  torv(7) = 11.313180e3
	  torv(8) = 11.402522e3 
	  torv(9) = 11.495901e3
	  torv(10) = 11.591722e3
	  torv(11) = 11.682138e3
	  torv(12) = 11.759306e3
	  torv(13) = 11.815380e3
	  torv(14) = 11.842515e3
	  torv(15) = 11.832865e3 
	  torv(16) = 11.787569e3 
	  torv(17) = 11.704838e3 
	  torv(18) = 11.570438e3
	  torv(19) = 11.380222e3
	  torv(20) = 11.130801e3
	  torv(21) = 10.815161e3
	  torv(22) = 10.426058e3
	  torv(23) = 9.9577322e3
	  torv(24) = 9.4049229e3
	  torv(25) = 8.7611757e3
c     Density time derivative correction [#/m^3-s]- drvr_pedtimed.pro (after profile fitting)	  
	  dlnn_dt(1) = 36.099819
	  dlnn_dt(2) = 37.716988
	  dlnn_dt(3) = 39.469837
	  dlnn_dt(4) = 41.372719
	  dlnn_dt(5) = 43.424480
	  dlnn_dt(6) = 45.644363
	  dlnn_dt(7) = 48.077682
	  dlnn_dt(8) = 50.737465
	  dlnn_dt(9) = 53.647240
	  dlnn_dt(10) = 56.860733
	  dlnn_dt(11) = 60.429955
	  dlnn_dt(12) = 64.412201
	  dlnn_dt(13) = 68.931190
	  dlnn_dt(14) = 74.120247
	  dlnn_dt(15) = 80.231422
	  dlnn_dt(16) = 87.699394
	  dlnn_dt(17) = 97.283890
	  dlnn_dt(18) = 110.39427
	  dlnn_dt(19) = 129.48560
	  dlnn_dt(20) = 158.60201
	  dlnn_dt(21) = 202.38933
	  dlnn_dt(22) = 256.63586
	  dlnn_dt(23) = 275.89993
	  dlnn_dt(24) = 200.19069
	  dlnn_dt(25) = 125.76175
c     Electron temperature time derivative correction [eV/s]- drvr_pedtimed.pro (after profile fitting)
	  dlnwe_dt(1) = 116.50312
	  dlnwe_dt(2) = 119.69071
	  dlnwe_dt(3) = 123.12923
	  dlnwe_dt(4) = 126.85667
	  dlnwe_dt(5) = 130.91751
	  dlnwe_dt(6) = 135.36548
	  dlnwe_dt(7) = 140.26454
	  dlnwe_dt(8) = 145.68950
	  dlnwe_dt(9) = 151.72961
	  dlnwe_dt(10) = 158.49202
	  dlnwe_dt(11) = 166.09386
	  dlnwe_dt(12) = 174.65622
	  dlnwe_dt(13) = 184.32959
	  dlnwe_dt(14) = 195.20654
	  dlnwe_dt(15) = 207.35373
	  dlnwe_dt(16) = 220.71024
	  dlnwe_dt(17) = 235.02373
	  dlnwe_dt(18) = 249.67436
	  dlnwe_dt(19) = 263.25528
	  dlnwe_dt(20) = 272.17896
	  dlnwe_dt(21) = 267.63632
	  dlnwe_dt(22) = 236.71788
	  dlnwe_dt(23) = 179.13327
	  dlnwe_dt(24) = 116.56518
	  dlnwe_dt(25) = 68.713142
c     Ion temperature time derivative correction [eV/s]- drvr_pedtimed.pro (after profile fitting)
	  dlnwi_dt(1) = 53.672802
	  dlnwi_dt(2) = 52.319241
	  dlnwi_dt(3) = 50.947304
	  dlnwi_dt(4) = 49.568813
	  dlnwi_dt(5) = 48.191929
	  dlnwi_dt(6) = 46.837856
	  dlnwi_dt(7) = 45.503391
	  dlnwi_dt(8) = 44.216042
	  dlnwi_dt(9) = 42.985481
	  dlnwi_dt(10) = 41.817703
	  dlnwi_dt(11) = 40.736637
	  dlnwi_dt(12) = 39.757286
	  dlnwi_dt(13) = 38.896519
	  dlnwi_dt(14) = 38.173004
	  dlnwi_dt(15) = 37.609329
	  dlnwi_dt(16) = 37.232002
	  dlnwi_dt(17) = 37.073143 
	  dlnwi_dt(18) = 37.171440 
	  dlnwi_dt(19) = 37.568928
	  dlnwi_dt(20) = 38.324268
	  dlnwi_dt(21) = 39.504787
	  dlnwi_dt(22) = 41.204704
	  dlnwi_dt(23) = 43.543545
	  dlnwi_dt(24) = 46.682388
	  dlnwi_dt(25) = 50.844837
	  
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
c	 shot:118890 @ 1515,  type:L-mode,  date:06/16/2004

c     Radial electric field [V/m] for rho [0,1], deltarho = 0.02
c     drvr_pedxax.pro (after profile fitting)
      erextot(1) =  12.0214E3
      erextot(2) =   9.6798E3
      erextot(3) =  12.4080E3
      erextot(4) =  16.2712E3
      erextot(5) =  20.4616E3
      erextot(6) =  25.4903E3
      erextot(7) =  31.4465E3
      erextot(8) =  36.2645E3
      erextot(9) =  40.8774E3
      erextot(10) =  45.2762E3
      erextot(11) =  49.2980E3
      erextot(12) =  52.9664E3
      erextot(13) =  56.2796E3
      erextot(14) =  59.2020E3
      erextot(15) =  61.7277E3
      erextot(16) =  63.8663E3
      erextot(17) =  65.6122E3
      erextot(18) =  66.9733E3
      erextot(19) =  67.9598E3
      erextot(20) =  68.5916E3
      erextot(21) =  68.8917E3
      erextot(22) =  68.8837E3
      erextot(23) =  68.5997E3
      erextot(24) =  68.0707E3
      erextot(25) =  67.3394E3
      erextot(26) =  66.4367E3
      erextot(27) =  65.4149E3
      erextot(28) =  64.3005E3
      erextot(29) =  63.1456E3
      erextot(30) =  61.9937E3
      erextot(31) =  60.8426E3
      erextot(32) =  59.7278E3
      erextot(33) =  58.6216E3
      erextot(34) =  57.5529E3
      erextot(35) =  56.4092E3
      erextot(36) =  55.1953E3
      erextot(37) =  54.0807E3
      erextot(38) =  52.7824E3
      erextot(39) =  51.1412E3
      erextot(40) =  48.9597E3
      erextot(41) =  46.1144E3
      erextot(42) =  42.1548E3
      erextot(43) =  36.7260E3
      erextot(44) =  29.5493E3
      erextot(45) =  20.5774E3
      erextot(46) =  10.6846E3
      erextot(47) =  -0.3236E3
      erextot(48) = -16.3703E3
      erextot(49) = -35.9385E3
      erextot(50) = -21.8190E3
      erextot(51) = -12.8250E3

c     Neutral beam deposition profile (hofr1) from NBeams (Dr. John Mandrekas)
c     units [#/s] - need to convert to GTEDGE rho coordinates using MATLAB script
c     nbeams2gtedge.m	from T.M.Wilks
      NBdep1(1) =   2.5036
      NBdep1(2) =   5.2510
      NBdep1(3) =   3.5322
      NBdep1(4) =   3.2218
      NBdep1(5) =   3.1231
      NBdep1(6) =   3.0972
      NBdep1(7) =   3.1113
      NBdep1(8) =   3.1459
      NBdep1(9) =   3.1812
      NBdep1(10) =   3.2180
      NBdep1(11) =   3.2423
      NBdep1(12) =   3.2497
      NBdep1(13) =   3.2190
      NBdep1(14) =   3.1298
      NBdep1(15) =   2.9848
      NBdep1(16) =   2.9117
      NBdep1(17) =   2.8461
      NBdep1(18) =   2.7851
      NBdep1(19) =   2.7199
      NBdep1(20) =   2.6384
      NBdep1(21) =   2.5491
      NBdep1(22) =   2.4482
      NBdep1(23) =   2.3399
      NBdep1(24) =   2.2152
      NBdep1(25) =   2.0893
      NBdep1(26) =   1.9532
      NBdep1(27) =   1.8100
      NBdep1(28) =   1.6648
      NBdep1(29) =   1.5179
      NBdep1(30) =   1.3717
      NBdep1(31) =   1.2294
      NBdep1(32) =   1.0922
      NBdep1(33) =   0.9617
      NBdep1(34) =   0.8374
      NBdep1(35) =   0.7229
      NBdep1(36) =   0.6180
      NBdep1(37) =   0.5233
      NBdep1(38) =   0.4390
      NBdep1(39) =   0.3652
      NBdep1(40) =   0.3012
      NBdep1(41) =   0.2471
      NBdep1(42) =   0.2020
      NBdep1(43) =   0.1652
      NBdep1(44) =   0.1360
      NBdep1(45) =   0.1134
      NBdep1(46) =   0.0964
      NBdep1(47) =   0.0843
      NBdep1(48) =   0.0760
      NBdep1(49) =   0.0706
      NBdep1(50) =   0.0670
      NBdep1(51) =   0.0000

      NBdep2(1) =   1.6724
      NBdep2(2) =   3.5164
      NBdep2(3) =   2.3811
      NBdep2(4) =   2.1944
      NBdep2(5) =   2.1574
      NBdep2(6) =   2.1760
      NBdep2(7) =   2.2290
      NBdep2(8) =   2.3033
      NBdep2(9) =   2.3827
      NBdep2(10) =   2.4689
      NBdep2(11) =   2.5502
      NBdep2(12) =   2.6224
      NBdep2(13) =   2.6661
      NBdep2(14) =   2.6664
      NBdep2(15) =   2.6187
      NBdep2(16) =   2.6180
      NBdep2(17) =   2.6163
      NBdep2(18) =   2.6141
      NBdep2(19) =   2.6038
      NBdep2(20) =   2.5718
      NBdep2(21) =   2.5279
      NBdep2(22) =   2.4677
      NBdep2(23) =   2.3957
      NBdep2(24) =   2.3005
      NBdep2(25) =   2.2005
      NBdep2(26) =   2.0844
      NBdep2(27) =   1.9552
      NBdep2(28) =   1.8193
      NBdep2(29) =   1.6769
      NBdep2(30) =   1.5307
      NBdep2(31) =   1.3851
      NBdep2(32) =   1.2417
      NBdep2(33) =   1.1026
      NBdep2(34) =   0.9674
      NBdep2(35) =   0.8411
      NBdep2(36) =   0.7240
      NBdep2(37) =   0.6168
      NBdep2(38) =   0.5202
      NBdep2(39) =   0.4350
      NBdep2(40) =   0.3603
      NBdep2(41) =   0.2968
      NBdep2(42) =   0.2435
      NBdep2(43) =   0.1997
      NBdep2(44) =   0.1648
      NBdep2(45) =   0.1376
      NBdep2(46) =   0.1173
      NBdep2(47) =   0.1027
      NBdep2(48) =   0.0926
      NBdep2(49) =   0.0861
      NBdep2(50) =   0.0818
      NBdep2(51) =   0.0000

      NBdep3(1) =   1.3809
      NBdep3(2) =   2.9071
      NBdep3(3) =   1.9748
      NBdep3(4) =   1.8292
      NBdep3(5) =   1.8106
      NBdep3(6) =   1.8410
      NBdep3(7) =   1.9033
      NBdep3(8) =   1.9869
      NBdep3(9) =   2.0770
      NBdep3(10) =   2.1761
      NBdep3(11) =   2.2733
      NBdep3(12) =   2.3649
      NBdep3(13) =   2.4324
      NBdep3(14) =   2.4634
      NBdep3(15) =   2.4509
      NBdep3(16) =   2.4770
      NBdep3(17) =   2.4999
      NBdep3(18) =   2.5211
      NBdep3(19) =   2.5335
      NBdep3(20) =   2.5229
      NBdep3(21) =   2.4992
      NBdep3(22) =   2.4577
      NBdep3(23) =   2.4031
      NBdep3(24) =   2.3227
      NBdep3(25) =   2.2363
      NBdep3(26) =   2.1311
      NBdep3(27) =   2.0104
      NBdep3(28) =   1.8808
      NBdep3(29) =   1.7424
      NBdep3(30) =   1.5980
      NBdep3(31) =   1.4526
      NBdep3(32) =   1.3077
      NBdep3(33) =   1.1659
      NBdep3(34) =   1.0266
      NBdep3(35) =   0.8956
      NBdep3(36) =   0.7732
      NBdep3(37) =   0.6606
      NBdep3(38) =   0.5585
      NBdep3(39) =   0.4680
      NBdep3(40) =   0.3884
      NBdep3(41) =   0.3204
      NBdep3(42) =   0.2632
      NBdep3(43) =   0.2160
      NBdep3(44) =   0.1783
      NBdep3(45) =   0.1490
      NBdep3(46) =   0.1270
      NBdep3(47) =   0.1111
      NBdep3(48) =   0.1003
      NBdep3(49) =   0.0932
      NBdep3(50) =   0.0886
      NBdep3(51) =   0.0000

c      cosine of angle between injected NB particle and Bphi
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
c		118890 @ 1515ms	 L-mode
		rmajor = 1.7089
		aminor = 0.5886
		elong = 1.7089
		triang = 0.36
		plasmacur = 1.38
   		B = 2.00
		bphi = -2.00
		q95 = 3.643
		pbeam = 3.98
		Rx = 1.48
		zx = -1.2344
		Rsep1 = 1.57
		rsep2 = 1.21
		zsep1 = -1.366
		zsep2 = -1.366
		ssi95 = 3.983
		pohmin = 1.25
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
		xnpedex = 2.35e19
		xnsepex = 3.24e18
		tpedexi = 315.
		tsepexi = 140.
		tpedexe = 270.
		tsepexe = 20.0
		widthnx = .2
		widthtex = .2
		widthtix = .2
		gradnbar(3) = .132
		gradnbar(4) = .132
		gradTbar(3) = .26
 		gradTbar(4) = .26
		gradTebar(3) = .116
		gradTebar(4) = .116
		aped = 0.620
		xnctrped = 2.17
		tctrped = 5.4547
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
	
	