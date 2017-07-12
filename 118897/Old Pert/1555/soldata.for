      
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
c	 shot:118897 @ 1555, type: H-mode,  Date: 06/16/2004

c     Electron density [#/m^3]- drvr_pedxax.pro (after profile fitting) 
	  EXNE(1) = 0.25432339E20 
	  EXNE(2) = 0.25367708E20
	  EXNE(3) = 0.25287938E20
	  EXNE(4) = 0.25205516E20
	  EXNE(5) = 0.25120190E20
	  EXNE(6) = 0.25031709E20
	  EXNE(7) = 0.24955383E20
	  EXNE(8) = 0.24863676E20
	  EXNE(9) = 0.24770204E20
	  EXNE(10) = 0.24672747E20
	  EXNE(11) = 0.24570533E20
	  EXNE(12) = 0.24481169E20
	  EXNE(13) = 0.24368241E20
	  EXNE(14) = 0.24268274E20
	  EXNE(15) = 0.24158201E20
	  EXNE(16) = 0.24026628E20
	  EXNE(17) = 0.23866197E20
	  EXNE(18) = 0.23738392E20
	  EXNE(19) = 0.23486348E20
	  EXNE(20) = 0.23089641E20
	  EXNE(21) = 0.22312215E20
	  EXNE(22) = 0.20734289E20
	  EXNE(23) = 0.18406933E20
	  EXNE(24) = 0.14318536E20
	  EXNE(25) = 0.099052042E20
c     Electron density gradient scale length [m]-
c     drvr_pedxax.pro (after profile fitting)
c     xlne = [(1/n)dn/dr]^-1  
	  XLNE(1) =   0.81879968
	  XLNE(2) =   0.79458027
	  XLNE(3) =   0.75844061
	  XLNE(4) =   0.72709420
	  XLNE(5) =   0.69336077
	  XLNE(6) =   0.66170721
	  XLNE(7) =   0.63725788
	  XLNE(8) =   0.60595954
	  XLNE(9) =   0.57782118
	  XLNE(10) =  0.54970466
	  XLNE(11) =  0.52013536
	  XLNE(12) =  0.49563963
	  XLNE(13) =  0.46671346
	  XLNE(14) =  0.43455667
	  XLNE(15) =  0.40357520
	  XLNE(16) =  0.36434404
	  XLNE(17) =  0.30851963
	  XLNE(18) =  0.24301318
	  XLNE(19) =  0.15244485
	  XLNE(20) = 0.078866096
	  XLNE(21) = 0.035771507
	  XLNE(22) = 0.015725504
	  XLNE(23) =0.0083171931
	  XLNE(24) =0.0046261422
	  XLNE(25) =0.0034680625
c     Toroidal Velocity gradient scale length [m]- 
c     drvr_pedxax.pro (after profile fitting)
c     exlv = [(1/v)dv/dr]^-1	
	  exlv(1) =   0.085319303
	  exlv(2) =   0.083048366
	  exlv(3) =   0.079826139
	  exlv(4) =   0.077051483
	  exlv(5) =   0.075011373
	  exlv(6) =   0.074382350
	  exlv(7) =   0.075032569
	  exlv(8) =   0.077148281
	  exlv(9) =   0.081901491
	  exlv(10) =  0.090039909
	  exlv(11) =   0.10392207
	  exlv(12) =   0.12435929
	  exlv(13) =  0.096150324
	  exlv(14) =  0.035071872
	  exlv(15) =  0.015810037
	  exlv(16) =  0.010048374
	  exlv(17) = 0.0073221480
	  exlv(18) = 0.0059825485
	  exlv(19) = 0.0053042425
	  exlv(20) = 0.0078496598
	  exlv(21) = -0.011704930
	  exlv(22) =-0.0034706539
	  exlv(23) =-0.0028489996
	  exlv(24) =-0.0028381769
	  exlv(25) =-0.0030689016
c     Electron temperature gradient scale length [m] - 
c     drvr_pedxax.pro (after profile fitting)
c     exlte = [(1/Te)dTe/dr]^-1
	  EXLTE(1) = 0.11485391
	  EXLTE(2) = 0.11184671
	  EXLTE(3) = 0.10745275
	  EXLTE(4) = 0.10336992
	  EXLTE(5) = 0.098849869
	  EXLTE(6) = 0.094311099
	  EXLTE(7) = 0.090444429
	  EXLTE(8) = 0.085182370
	  EXLTE(9) = 0.079777183
	  EXLTE(10) = 0.073597494
	  EXLTE(11) = 0.066444277
	  EXLTE(12) = 0.059896465
	  EXLTE(13) = 0.051529388
	  EXLTE(14) = 0.042598531
	  EXLTE(15) = 0.034349823
	  EXLTE(16) = 0.026853022
	  EXLTE(17) = 0.020499011
	  EXLTE(18) = 0.016072921
	  EXLTE(19) = 0.011800297
	  EXLTE(20) = 0.0087532736
	  EXLTE(21) = 0.0066861363
	  EXLTE(22) = 0.0053493911
	  EXLTE(23) = 0.0045987035
	  EXLTE(24) = 0.0040872764
	  EXLTE(25) = 0.0039630884
c     Ion temperature [eV]- drvr_pedxax.pro (after profile fitting)	 
	  XTI(1) = 409.67030
	  XTI(2) = 403.71987
	  XTI(3) = 396.74350
	  XTI(4) = 389.92609
	  XTI(5) = 383.27921
	  XTI(6) = 376.81040
	  XTI(7) = 371.54108
	  XTI(8) = 365.33858
	  XTI(9) = 359.22526
	  XTI(10) = 353.14286
	  XTI(11) = 347.04605
	  XTI(12) = 341.91632
	  XTI(13) = 335.66754
	  XTI(14) = 329.26125
	  XTI(15) = 322.64837
	  XTI(16) = 315.78080
	  XTI(17) = 308.61113
	  XTI(18) = 302.37153
	  XTI(19) = 294.51771
	  XTI(20) = 286.22738
	  XTI(21) = 277.44620
	  XTI(22) = 268.12016
	  XTI(23) = 259.89543
	  XTI(24) = 249.45160
	  XTI(25) = 238.31833
c     Ion temperature gradient scale length [m]- 
c     drvr_pedxax.pro (after profile fitting)
c     exlti = [(1/Ti)dTi/dr]^-1
	  EXLTI(1) = 0.14207302
	  EXLTI(2) = 0.14189875
	  EXLTI(3) = 0.14076781
	  EXLTI(4) = 0.14024965
	  EXLTI(5) = 0.13940260
	  EXLTI(6) = 0.13892530
	  EXLTI(7) = 0.13842601
	  EXLTI(8) = 0.13643695
	  EXLTI(9) = 0.13387634
	  EXLTI(10) = 0.13003375
	  EXLTI(11) = 0.12477975
	  EXLTI(12) = 0.11962277
	  EXLTI(13) = 0.11295200
	  EXLTI(14) = 0.10520311
	  EXLTI(15) = 0.098315336
	  EXLTI(16) = 0.091546662
	  EXLTI(17) = 0.084888041
	  EXLTI(18) = 0.078695014
	  EXLTI(19) = 0.070109099
	  EXLTI(20) = 0.061505120
	  EXLTI(21) = 0.053306509
	  EXLTI(22) = 0.045856327
	  EXLTI(23) = 0.039933935
	  EXLTI(24) = 0.033909347
	  EXLTI(25) = 0.029135372
c     Electron temperature [eV]- drvr_pedxax.pro (after profile fitting)					
	  XTE(1) = 410.34546
	  XTE(2) = 402.89860
	  XTE(3) = 393.94239
	  XTE(4) = 384.95200
	  XTE(5) = 375.91171
	  XTE(6) = 366.79863
	  XTE(7) = 359.12605
	  XTE(8) = 349.78530
	  XTE(9) = 340.23646
	  XTE(10) = 330.37959
	  XTE(11) = 320.06651
	  XTE(12) = 310.97243
	  XTE(13) = 299.21695
	  XTE(14) = 286.11081
	  XTE(15) = 271.10217
	  XTE(16) = 253.47192
	  XTE(17) = 232.48973
	  XTE(18) = 212.02475
	  XTE(19) = 183.86329
	  XTE(20) = 152.73081
	  XTE(21) = 120.78401
	  XTE(22) = 90.896415
	  XTE(23) = 69.407082
	  XTE(24) = 48.901463
	  XTE(25) = 34.165660 
c     safety factor [1]- EFITtools --> profiles_1_mse
c     need to manually input for GTEDGE rho vector
	  qedge(1) = 2.88
	  qedge(2) = 2.93
	  qedge(3) = 2.98
	  qedge(4) = 3.04
	  qedge(5) = 3.09
	  qedge(6) = 3.15
	  qedge(7) = 3.21
	  qedge(8) = 3.27
	  qedge(9) = 3.34
	  qedge(10) = 3.40
	  qedge(11) = 3.47
	  qedge(12) = 3.54
	  qedge(13) = 3.61
	  qedge(14) = 3.69
	  qedge(15) = 3.77
	  qedge(16) = 3.86
	  qedge(17) = 3.94
	  qedge(18) = 4.02
	  qedge(19) = 4.12
	  qedge(20) = 4.23
	  qedge(21) = 4.43
	  qedge(22) = 4.65
	  qedge(23) = 4.89
	  qedge(24) = 5.07
	  qedge(25) = 5.22
c     Radial Electric Field [V/m]- drvr_pedxax.pro (after profile fitting)
	  erex(1) = 8.1889891e3
	  erex(2) = 7.8689327e3
	  erex(3) = 7.4802114e3
	  erex(4) = 7.0880421e3
	  erex(5) = 6.6944421e3
	  erex(6) = 6.3036138e3
	  erex(7) = 5.9849947e3
	  erex(8) = 5.6148535e3
	  erex(9) = 5.2645549e3
	  erex(10) = 4.9406790e3
	  erex(11) = 4.6465212e3
	  erex(12) = 4.4312380e3
	  erex(13) = 4.0852674e3
	  erex(14) = 3.3770136e3
	  erex(15) = 2.1438228e3
	  erex(16) = 0.68151839e3
	  erex(17) = -0.85847905e3
	  erex(18) = -2.1109947e3
	  erex(19) = -3.4672569e3
	  erex(20) = -5.3253188e3
	  erex(21) = -8.6044089e3
	  erex(22) = -13.541557e3
	  erex(23) = -17.103660e3
	  erex(24) = -15.903934e3
	  erex(25) = -10.207133e3
c     Carbon Poloidal Velocity [m/s]- drvr_pedxax.pro (after profile fitting)
c	rh sign convention + is down at outer midplane
	  vthexp(1) = 0.17694490e3
	  vthexp(2) = 0.11165820e3
	  vthexp(3) = 0.029123275e3
	  vthexp(4) = -0.058082725e3
	  vthexp(5) = -0.14991384e3
	  vthexp(6) = -0.24506865e3
	  vthexp(7) = -0.32550689e3
	  vthexp(8) = -0.42172459e3
	  vthexp(9) = -0.51577014e3
	  vthexp(10) = -0.60570102e3
	  vthexp(11) = -0.68958095e3
	  vthexp(12) = -0.75357691e3
	  vthexp(13) = -0.82543954e3
	  vthexp(14) = -0.89803483e3
	  vthexp(15) = -0.97795207e3
	  vthexp(16) = -1.0717820e3
	  vthexp(17) = -1.1860866e3
	  vthexp(18) = -1.3018027e3
	  vthexp(19) = -1.4708310e3
	  vthexp(20) = -1.6790319e3
	  vthexp(21) = -1.9329893e3
	  vthexp(22) = -2.2465552e3
	  vthexp(23) = -2.5825951e3
	  vthexp(24) = -3.1163406e3
	  vthexp(25) = -3.8369947e3
c     Carbon Toroidal Velocity [m/s]- drvr_pedxax.pro (after profile fitting)
	  torv(1) = 22.702894e3
	  torv(2) = 22.149902e3
	  torv(3) = 21.489340e3
	  torv(4) = 20.832342e3
	  torv(5) = 20.183188e3
	  torv(6) = 19.551862e3
	  torv(7) = 19.046636e3
	  torv(8) = 18.474930e3
	  torv(9) = 17.949709e3
	  torv(10) = 17.482425e3
	  torv(11) = 17.082690e3
	  torv(12) = 16.831737e3
	  torv(13) = 16.519788e3
	  torv(14) = 15.908374e3
	  torv(15) = 14.493644e3 
	  torv(16) = 12.276146e3 
	  torv(17) = 9.6677161e3 
	  torv(18) = 7.4980719e3
	  torv(19) = 5.2635728e3
	  torv(20) = 3.8290012e3
	  torv(21) = 3.6128811e3
	  torv(22) = 5.0424685e3
	  torv(23) = 7.7892570e3
	  torv(24) = 13.318420e3
	  torv(25) = 21.680810e3
c     Density time derivative correction [#/m^3-s]- drvr_pedtimed.pro (after profile fitting)	  
	  dlnn_dt(1) = 7.5881257
	  dlnn_dt(2) = 7.6453223
	  dlnn_dt(3) = 7.7141056
	  dlnn_dt(4) = 7.7837009
	  dlnn_dt(5) = 7.8539338
	  dlnn_dt(6) = 7.9258332
	  dlnn_dt(7) = 7.9872284
	  dlnn_dt(8) = 8.0609913
	  dlnn_dt(9) = 8.1365871
	  dlnn_dt(10) = 8.2130880
	  dlnn_dt(11) = 8.2918243
	  dlnn_dt(12) = 8.3583059
	  dlnn_dt(13) = 8.4387016
	  dlnn_dt(14) = 8.5179424
	  dlnn_dt(15) = 8.5945015
	  dlnn_dt(16) = 8.6685028
	  dlnn_dt(17) = 8.7249947
	  dlnn_dt(18) = 8.7310362
	  dlnn_dt(19) = 8.5843315
	  dlnn_dt(20) = 7.7994270
	  dlnn_dt(21) = 5.5974503
	  dlnn_dt(22) = 2.6044757
	  dlnn_dt(23) = 0.49553064
	  dlnn_dt(24) = -1.3400524
	  dlnn_dt(25) = -2.7474620
c     Electron temperature time derivative correction [eV/s]- drvr_pedtimed.pro (after profile fitting)
	  dlnwe_dt(1) = 7.3724103
	  dlnwe_dt(2) = 7.4673777
	  dlnwe_dt(3) = 7.5869608
	  dlnwe_dt(4) = 7.7127519
	  dlnwe_dt(5) = 7.8444524
	  dlnwe_dt(6) = 7.9815784
	  dlnwe_dt(7) = 8.0990295
	  dlnwe_dt(8) = 8.2423592
	  dlnwe_dt(9) = 8.3855124
	  dlnwe_dt(10) = 8.5241365
	  dlnwe_dt(11) = 8.6513910
	  dlnwe_dt(12) = 8.7419920
	  dlnwe_dt(13) = 8.8207779
	  dlnwe_dt(14) = 8.8496885
	  dlnwe_dt(15) = 8.8049231
	  dlnwe_dt(16) = 8.6563768
	  dlnwe_dt(17) = 8.3661098
	  dlnwe_dt(18) = 7.9827008
	  dlnwe_dt(19) = 7.3065982
	  dlnwe_dt(20) = 6.3326244
	  dlnwe_dt(21) = 4.9814930
	  dlnwe_dt(22) = 3.1652155
	  dlnwe_dt(23) = 1.2443478
	  dlnwe_dt(24) = -1.5521599
	  dlnwe_dt(25) = -4.7576642
c     Ion temperature time derivative correction [eV/s]- drvr_pedtimed.pro (after profile fitting)
	  dlnwi_dt(1) = 6.2023787
	  dlnwi_dt(2) = 6.2181830
	  dlnwi_dt(3) = 6.2309189
	  dlnwi_dt(4) = 6.2354937
	  dlnwi_dt(5) = 6.2305226
	  dlnwi_dt(6) = 6.2143350
	  dlnwi_dt(7) = 6.1910124
	  dlnwi_dt(8) = 6.1498194
	  dlnwi_dt(9) = 6.0923796
	  dlnwi_dt(10) = 6.0165391
	  dlnwi_dt(11) = 5.9199405
	  dlnwi_dt(12) = 5.8216348
	  dlnwi_dt(13) = 5.6790805
	  dlnwi_dt(14) = 5.5063734
	  dlnwi_dt(15) = 5.2986126
	  dlnwi_dt(16) = 5.0502973
	  dlnwi_dt(17) = 4.7546821
	  dlnwi_dt(18) = 4.4663138
	  dlnwi_dt(19) = 4.0598578
	  dlnwi_dt(20) = 3.5749056
	  dlnwi_dt(21) = 2.9946334
	  dlnwi_dt(22) = 2.2958889
	  dlnwi_dt(23) = 1.5988325
	  dlnwi_dt(24) = 0.59136623
	  dlnwi_dt(25) = -0.66293550
	  
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
c	 shot:118897 @ 1555,  type:H-mode,  date:06/16/2004

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
c		118897 @ 1555ms	 H-mode
		rmajor = 1.709
		aminor = 0.597
		elong = 1.8259
		triang = 0.366
		plasmacur = 1.39
   		B = 2.00
		bphi = -2.00
		q95 = 3.592
		pbeam = 4.42
		Rx = 1.48
		zx = -1.2404
		Rsep1 = 1.57
		rsep2 = 1.21
		zsep1 = -1.366
		zsep2 = -1.366
		ssi95 = 3.912
		pohmin = 0.5116
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
		xnpedex = 2.47e19
		xnsepex = 9.90e18
		tpedexi = 356.
		tsepexi = 238.
		tpedexe = 335.
		tsepexe = 35.
		widthnx = 0.09
		widthtex = 0.09
		widthtix = 0.09
		gradnbar(3) = 1.05e-1
		gradnbar(4) = 1.05e-1
		gradTbar(3) = 0.2265
 		gradTbar(4) = 0.2265
		gradTebar(3) = 0.0555
		gradTebar(4) = 0.0555
		aped = 6.00e-1
		xnctrped = 1.60
		tctrped = 5.641
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
	
	