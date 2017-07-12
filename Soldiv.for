	PROGRAM SOLDIV
C		SOL-DIVERTOR MODEL MAIN PROGRAM
	 USE MSIMSL 
	INCLUDE 'SOLDIV.FI' 
													
 	parameter (nit=9,npsi0=22,nsol=51,nn=30)
	dimension soln(nit),solv(nit),sole(nit),
	1	gam25con(nn),gamQ25con(nn),gamorbl(nn),gamQorbl(nn),
	2	gamsum(nn), gamQsum(nn), gamiller(nn),gamQmiller(nn),
	3	fheat(nsol),fpart(nsol),gamvorbl(nn),emin(npsi0,nit,nn,2),
     4	floss(npsi0,nit,nn),vloss(npsi0,nit,nn),
     5	eloss(npsi0,nit,nn),fsol(24,15),msol(24,15),
     6	esol(24,15),LO(npsi0,nit,nn),gpart(nsol),qheat(nsol),
     7	bigH(nsol),SoleAll(npsi0,nit),eminAll(22,8,24,8,2),
	8	xbigX(52),Tsolf(8,24),Tsolm(8,24),Tsole(8,24),
     9	eminAvgA(8,8,24,2),delf(8,8,22,24,3,2),minEtta(22,24,2),
     1	therms(24,3),xline(npsi0),thermsC(24,3)

      real nbdep1(51),nbdep2(51),nbdep3(51),ephi,forbl,xmorbl,eorbl,
	1	ffrac1(51),retcur(51),NBItot(51),Snbi,ffrac2(51),ffrac3(51),   
     2	ephi0(51),erextot(51),fpsi0(51),nbdepkept1(50),xm(2),
	3	nbdepkept2(50),nbdepkept3(50),nbdeplost1(50),nbdeplost2(50),
     4	nbdeplost3(50),NBIlost(51),radthet(9),bpthet(9),btthet(9),
     5	NBIreturn,dFdr,Edr,fthermal(2),mthermal(2),ethermal(2),
	6    Icur,vintrin,forblC(24),xmorblC(24),eorblC(24),
	7    eminall,xbigx,Tsolf,Tsolm,Tsole,eminAvgA,delf,nbrho,
	8     minEtta,emin,fast1,fast2,fast3,therms,Elost,BCnbi,Mlost(51),
	9	 fforb1,fforb2,fforb3,Fretcur,xline,radius,step,LowestE,
	1	 thermsC,vintrinC(24),coreflux(50),totflux(50),dV,fb(3)

	integer rholength,psinum,n0,solovev,lo,ioptFast,ioptFIOL

c     Initialize input data			
	CALL SOLDATA(nbdep1,nbdep2,nbdep3,erextot,fpsi0,fb)	  		
      fuelplout = fuelplin
	thetain = theta
	heatfrace = fheate
	N1 = 0
	DELN = 0.02
	CALL GEOMETRY(N1)
	AP = 39.44*RMAJOR*AMINOR*SQRT((1.+ELONG**2)/2.)
	VP = 19.72*RMAJOR*(AMINOR**2)*0.5*(1+ELONG**2)
c	VP = 19.72*RMAJOR*(AMINOR**2)*ELONG
	ITERTDL = 0
	yyt = fuelplout
	iyyt=ioptq95 
	IMARFE = 0
	TISOL = TSOL
	x = yltebarx
	x = fheate
	x = xlpm(20)
	TESOL = TSOL
	TIPED = TPED
	TEPED = TPED
	XNBAR = XNPED
	TIBAR = TPED
	TEBAR = TPED
	XLNBAR = 0.05
	XLTIBAR = 0.06
	XLTEBAR = 0.05
	VSEP = 1.E6
	TSPL = TSEP
	TPL = TSEP
	TPF = TSEP
	TDIV = TSEP
	TEMPSEP = TSEP
	SVIOND = 1.E-14
	SVTOTD = 1.E-14
	DEL = 1.E-2
	DELN = 2.*DEL
	DELNV = DELN
	DELEA = DELN/7.
	DQIN = FLUXHEAT*XLPERP/DELEA
	DELNIN = FLUXPART*XLPERP/DELN
	GAMOUTSPL = 1E20
	GAMOUTPF  = 1E20
	GAMOUTPL  = 1E20
	GAMZERO = 1E20
	XNAV = 1.E20
	XNEL = XNAV
 	CSD = SQRT(2.*XK*TD/XMASS)
	OPEN(9016,FILE='STPQ.TXT',STATUS='UNKNOWN')
	CALL DISTR
	AAA = xnsepex
  	xm(1) = 3.343e-27
	xm(2) = xm(1)*6.0
	xpi = 3.1416
	eq(1) = 1.6e-19
	eq(2) = eq(1)*6.0
      Icur = plasmacur
	Btor = Bphi
	radwall=   0.677
    	radminor = aminor*sqrt(0.5*(1+elong**2))
	rminorw = radwall*sqrt(0.5*(1+elong**2))
	dr = 0.1
	RM = rmajor
	xmu0 =1.257					 
      zmass = 3.34e-27
	delnaf = 0.02
	delT = 0.0204
	nbi = 75000

c***************DEBUGGING******************************************************
5077	format(F35.15,F35.15,F35.15)
5078	format(F35.15)
5079  format(I10)
5080	format(E35.15)
	OPEN(177,FILE='DEBOB.TXT',STATUS='UNKNOWN')
	OPEN(178,FILE='DEBOX.TXT',STATUS='UNKNOWN')
	OPEN(179,FILE='DEBOZ.TXT',STATUS='UNKNOWN')
	OPEN(180,FILE='DEBOY.TXT',STATUS='UNKNOWN')
	OPEN(181,FILE='DEBOA.TXT',STATUS='UNKNOWN')
	OPEN(182,FILE='DEBOC.TXT',STATUS='UNKNOWN')
	OPEN(183,FILE='DEBOD.TXT',STATUS='UNKNOWN')
	OPEN(184,FILE='DEBOE.TXT',STATUS='UNKNOWN')
	OPEN(185,FILE='DEBOF.TXT',STATUS='UNKNOWN')
	OPEN(186,FILE='DEBOG.TXT',STATUS='UNKNOWN')
	OPEN(187,FILE='DEBOH.TXT',STATUS='UNKNOWN')
c**************End of Debugging Section****************************************
c**************OUTPUTS for PLOTTING********************************************
	OPEN(9000,FILE='STPA.TXT',STATUS='UNKNOWN')
	OPEN(9001,FILE='STPB.TXT',STATUS='UNKNOWN')
	OPEN(9002,FILE='STPC.TXT',STATUS='UNKNOWN')
	OPEN(9003,FILE='STPD.TXT',STATUS='UNKNOWN')
	OPEN(9004,FILE='STPE.TXT',STATUS='UNKNOWN')
	OPEN(9005,FILE='STPF.TXT',STATUS='UNKNOWN')
	OPEN(9006,FILE='STPG.TXT',STATUS='UNKNOWN')
	OPEN(9007,FILE='STPH.TXT',STATUS='UNKNOWN')
	OPEN(9008,FILE='STPI.TXT',STATUS='UNKNOWN')
	OPEN(9009,FILE='STPJ.TXT',STATUS='UNKNOWN')
	OPEN(9010,FILE='STPK.TXT',STATUS='UNKNOWN')
	OPEN(9011,FILE='STPL.TXT',STATUS='UNKNOWN')
	OPEN(9012,FILE='STPM.TXT',STATUS='UNKNOWN')

	OPEN(9013,FILE='STPN.TXT',STATUS='UNKNOWN')
	OPEN(9014,FILE='STPO.TXT',STATUS='UNKNOWN')
	OPEN(9015,FILE='STPP.TXT',STATUS='UNKNOWN')
	OPEN(9017,FILE='STPR.TXT',STATUS='UNKNOWN')

	open(500,file='bugging2.txt',status='old')
 	open(600,file='rlossiol.txt',status='old')
	open(700,file='psiSOL.txt',status='old')
55	OPEN(unit=121,FILE='orbloss.TXT',STATUS='UNKNOWN')

c**************End of OUTPUTS for PLOTTING*************************************

c     fuel.for calculates NBI source into plasma
c     if ioptFIOL=1, then the fast ion calculation rewrites this source
      call fuel
	do 506 n = 1,25
	   fforb1(n) = 0.0
	   fforb2(n) = 0.0
	   fforb3(n) = 0.0
	   Fretcur(n) = 0.0
506	continue
	do 509 n0=1,50
	   nbdepkept1(n0) = 0.624E25*pbeam/(nbi)*nbdep1(n0)
 	   nbdepkept2(n0) = 0.624E25*pbeam/(nbi/2)*nbdep2(n0)
	   nbdepkept3(n0) = 0.624E25*pbeam/(nbi/3)*nbdep3(n0)
	   nbdeplost1(n0) = 0
	   nbdeplost2(n0) = 0
	   nbdeplost3(n0) = 0
509	continue

	BCnbi = 0.0
	BCenbi = 0.0

	if(ioptFIOL.eq.0) goto 505
c*******************************************************************
c      FAST ION CALCULATION
c*******************************************************************
	rholength = 50
	delna0 = delnaf*aminor
c     calculate electrostatic potential for entire rho vector
c     use scrape off layer electron temperature as boundary condition
c      ephi0(rholength+1) = tesol
      ephi0(rholength+1) = XTE(25)
	ersum = erextot(rholength+1)
      do 10 n =1,rholength
	   m = rholength+1-n
	   ephi0(m) = ephi0(m+1) + radminor*delna0*0.5*(erextot(m)
	1              +erextot(m+1))
         ersum = ersum + erextot(m)
	   erav(m) = ersum/(n+1)
10	continue 
c     loop over flux surfaces
      do 5 n0 = 1,rholength
         psinum = 1
         SOL = 0
	   ioptFast = 1
         xdr =  (rholength+1-n0)
         rminor0	= radminor - xdr*delna0
         rminorD = radminor
         if(n0.lt.26)then
            radthet = riol(n0,1:9)
         endif
c      calculate minimum velocities requried for IOL
         call e0min(xm,ephi0(n0),ephi0(51),rminor0,rminorD,psinum,
	1        radthet,SOL,fpsi0(n0),RM,BTOR,radminor,emin,LO,n0,
     2		eminAll,eminS,eminAvgA)
		do 5554 i = 1,2
         do 5555 np0i = 1,22
	     minEtta(np0i,n0,i) = 0.0	
5555	 continue
5554	continue
c	calculate loss fractions from minimum velocities
	   call lossfrac(ioptFast,psinum,1.0,n0,emin,eminAll,delf,fthermal
	1  	,mthermal,ethermal,fast1,fast2,fast3,minEtta)
	
	ffrac1(n0) = fast1
	ffrac2(n0) = fast2
	ffrac3(n0) = fast3
5     continue
 	  
c	Reduce NBI source rate by fast ion loss [ions/s]
c	If no NBI IOL, then just scaled deposition profiles
      source = 0.0
	  nbrho = 0.0
	do 502 n0=1,50  
	   if (n0.EQ.1) then
	      dv = 19.72*RMAJOR*0.5*(1+ELONG**2)*
	1      (((nbrho+0.02)*AMINOR)**2)
	   else
	      dv = 19.72*RMAJOR*0.5*(1+ELONG**2)*(AMINOR**2)*
	1     (((nbrho+0.02)**2)-nbrho**2)
	   endif
	   nbdepkept1(n0) = 0.624E25*pbeam/(vp*nbi)*(1-ffrac1(n0))
	1   *fb(1)*nbdep1(n0)
	   nbdeplost1(n0) = 0.624E25*pbeam/(vp*nbi)*ffrac1(n0)
	1   *fb(1)*nbdep1(n0)
	   nbdepkept2(n0) = 0.624E25*pbeam/(vp*nbi/2)*(1-ffrac2(n0))
	1   *fb(2)*nbdep2(n0)
	   nbdeplost2(n0) = 0.624E25*pbeam/(vp*nbi/2)*ffrac2(n0)
	1   *fb(2)*nbdep2(n0)
	   nbdepkept3(n0) = 0.624E25*pbeam/(vp*nbi/3)*(1-ffrac3(n0))
	1   *fb(3)*nbdep3(n0)
	   nbdeplost3(n0) = 0.624E25*pbeam/(vp*nbi/3)*ffrac3(n0)
	1   *fb(3)*nbdep3(n0)
c     NBIlost is particles [#/m^3-s] being lost over all energy components
	   NBIlost(n0) = nbdeplost1(n0)+nbdeplost2(n0)+nbdeplost3(n0)
	   Elost(n0) = (nbi*nbdeplost1(n0)+(nbi/2)*nbdeplost2(n0)
	1         +(nbi/3)*nbdeplost3(n0))*1.602e-19
	   NBItot(n0) = 0.624E25*pbeam/(vp*nbi)*(fb(1)*nbdep1(n0)+2*
	1   fb(2)*nbdep2(n0)+fb(3)*3*nbdep3(n0))
c	   NBItot(n0) = 0.624E25*6./(vp*nbi)*(nbdep1(n0)+2*
c	1   nbdep2(n0)+3*nbdep3(n0))
	   Mlost(n0) = (sqrt(6.)*ffrac1(n0)+sqrt(3.)*ffrac2(n0)
	1   +sqrt(2.)*ffrac3(n0))/(sqrt(6.)+sqrt(3.)+sqrt(2.))
c      Calculate radial particle flux based on NBI, with (coreflux) and without (totflux) fast ion losses.
c      This is done solving the continuity equation.
c	   if (n0.EQ.1) then
c	      totflux(n0) = NBItot(n0)*0.02*radminor
c	      coreflux(n0) = (NBItot(n0) - 2*NBIlost(n0))*0.02*radminor
c       else
c	      totflux(n0) = totflux(n0-1)*(n0-1)/n0 +
c	1	  NBItot(n0)*0.02*radminor
c		  coreflux(n0) = coreflux(n0-1)*(n0-1)/n0 +
c	1      (NBItot(n0)-2*NBIlost(n0))*0.02*radminor
c	   endif
c      More accurately, radial particle flux is calculated by summing sources and dividing by the plasma area at rho.
       if (n0.EQ.1) then
	      totflux(n0) = (NBItot(n0)*dv)/
	1           (39.44*RMAJOR*radminor*(nbrho+0.02))
	      coreflux(n0) = ((NBItot(n0) - 2*NBIlost(n0))*dv)/
	1           (39.44*RMAJOR*radminor*(nbrho+0.02))
       else
	      totflux(n0) = (totflux(n0-1)*(39.44*RMAJOR*radminor*nbrho)
	1	       + NBItot(n0)*dv)/(39.44*RMAJOR*radminor*(nbrho+0.02))
		  coreflux(n0) = (coreflux(n0-1)*(39.44*RMAJOR*radminor*nbrho)
	1	       + (NBItot(n0)-2*NBIlost(n0))*dv)/
     1           (39.44*RMAJOR*radminor*(nbrho+0.02))
	   endif
c	 grad dot j must be conserved, defining a return current from lost ions
	   retcur(n0) =  xk*NBIlost(n0)
c    Calculate the source rate into the plasma.
        source = source+NBItot(n0)*dv
c    Set the core source rate.
		if (n0.EQ.43) then
		   spellet = source
		endif
	   nbrho = nbrho + 0.02
502	continue
c      Set the edge source rate.
      fuelmp = source - spellet
      write(*,*) 'calc source rate =', source, 'ions/sec'
	  write(*,*) 'totflux(43) =', totflux(43)
	  write(*,*) 'totflux(50) =', totflux(50)
	  write(*,*) 'coreflux(43) =', coreflux(43)
	  write(*,*) 'coreflux(50) =', coreflux(50)
	  totflux(50) = totflux(50)*((6.24E24*pbeam/(nbi)*
	1    (fb(1)+2*fb(2)+fb(3)*3))/source)
	  coreflux(43) = coreflux(43)*((6.24E24*pbeam/(nbi)*
	1    (fb(1)+2*fb(2)+fb(3)*3))/source)
	  write(*,*) 'corrected totflux(50) =', totflux(50)
	  write(*,*) 'corrected coreflux(43) =', coreflux(43)
c     Set the core and edge source rates to the actual source rates.
      spellet = spellet*((6.24E24*pbeam/(nbi)*(fb(1)+2*fb(2)+fb(3)*3))
	1    /source)
	fuelmp = fuelmp*((6.24E24*pbeam/(nbi)*(fb(1)+2*fb(2)+fb(3)*3))
	1    /source)
	  write(*,*) 'Spellet =', spellet
	  write(*,*) 'Fuelmp =', fuelmp
c     Define radial particle flux source for neutrals.
	  fluxpart = (spellet+fuelmp)/AP
c     Define reduced NBI source from the core to the edge
	  BCnbi = coreflux(43)
      N = 43
c	spellet = spelletreal
c	fuelmp = fuelmpreal
	nbrho = 0.0
c	spellet is source rate [/s] into core and fuelmp is into edge
c     BCnbi is the lost NB particle correction to gammahat BC
      do 503 n0=1,N
c	  spellet = spellet+0.02*aminor*4*(xpi**2)*(rmajor+(nbrho+0.01)*
c     1	   aminor)*elong*(nbrho+0.01)*aminor*(nbdepkept1(n0)
c     2             +nbdepkept2(n0)+nbdepkept3(n0))
c         fuelmp = fuelmp+0.02*aminor*4*(xpi**2)*(rmajor+(nbrho+0.01)*
c     1	   aminor)*elong*(nbrho+0.01)*aminor*(nbdeplost1(n0)+
c     2       nbdeplost2(n0)+nbdeplost3(n0))
c      BCnbi is missing a ratio of rho(n-1)/rho(n) as the flux propogates outward, and a factor of 2 to account for return current.
c	   BCnbi = BCnbi+NBIlost(n0)*0.02*aminor       
	   BCenbi = BCenbi+NBIeloss*Elost(n0)*0.02*radminor
	   nbrho = nbrho + 0.02
503	continue
c	  spellet = source
c	 Project lost particles from NBI [#/m^3s] onto shorter rho vector
c      Note this loss continues into the plasma farther than rho=0.864
	do 504 n0 = 1,25
	   if(n0.le.3)then
	      NBIreturn(n0) = NBIlost(44)
	      Snbi(n0) = NBItot(44)-2*NBIlost(44)
	      fforb1(n0) = ffrac1(44)
		  fforb2(n0) = ffrac2(44)
		  fforb3(n0) = ffrac3(44)
	      Fretcur(n0) = retcur(44)
	   endif
	   if(n0.le.7.and.n0.gt.3)then
	      NBIreturn(n0) = NBIlost(45)
	      Snbi(n0) = NBItot(45)-2*NBIlost(45)
	      fforb1(n0) = ffrac1(45)
		  fforb2(n0) = ffrac2(45)
		  fforb3(n0) = ffrac3(45)
	      Fretcur(n0) = retcur(45)
	   endif
 	   if(n0.le.10.and.n0.gt.7)then
	      NBIreturn(n0) = NBIlost(46)
	      Snbi(n0) = NBItot(46)-2*NBIlost(46)
	      fforb1(n0) = ffrac1(46)
		  fforb2(n0) = ffrac2(46)
		  fforb3(n0) = ffrac3(46)
	      Fretcur(n0) = retcur(46)
	   endif
	   if(n0.le.14.and.n0.gt.10)then
	      NBIreturn(n0) = NBIlost(47)
	      Snbi(n0) = NBItot(47)-2*NBIlost(47)
	      fforb1(n0) = ffrac1(47)
		  fforb2(n0) = ffrac2(47)
		  fforb3(n0) = ffrac3(47)
	      Fretcur(n0) = retcur(47)
	   endif
	   if(n0.le.17.and.n0.gt.14)then
	      NBIreturn(n0) = NBIlost(48)
	      Snbi(n0) = NBItot(48)-2*NBIlost(48)
	      fforb1(n0) = ffrac1(48)
		  fforb2(n0) = ffrac2(48)
		  fforb3(n0) = ffrac3(48)
	      Fretcur(n0) = retcur(49)
	   endif
	   if(n0.le.21.and.n0.gt.17)then
	      NBIreturn(n0) = NBIlost(49)
	      Snbi(n0) = NBItot(49)-2*NBIlost(49)
	      fforb1(n0) = ffrac1(49)
		  fforb2(n0) = ffrac2(49)
		  fforb3(n0) = ffrac3(49)
	      Fretcur(n0) = retcur(49)
	   endif
	   if(n0.gt.21)then
	      NBIreturn(n0) = NBIlost(50)
	      Snbi(n0) = NBItot(50)-2*NBIlost(50)
	      fforb1(n0) = ffrac1(50)
		  fforb2(n0) = ffrac2(50)
		  fforb3(n0) = ffrac3(50)
	      Fretcur(n0) = retcur(50)
	   endif
504	continue 
505   continue
c*******************************************************************
c      THERMAL ION CALCULATION
c*******************************************************************
      rholength = 24
c	ephi(rholength+1) = tesol
      ephi(rholength+1) = XTE(25)
	ersum = erex(rholength+1)
	delna0 = delna
c     calculate electrostatic potential in the edge 
	do 20 n =1,rholength
	   m = rholength+1-n
	   ephi(m) = ephi(m+1) + radminor*delna0*0.5*(erex(m)
	1                 +erex(m+1))
         ersum = ersum + erex(m)
	   erav(m) = ersum/(n+1)
20	continue
c     loop over flux surfaces	
	do 15 n0 = 1,rholength
	   Tion = xti(n0)
	   psinum = 22
	   xdr =  (rholength+1-n0)
	   rminor0	= radminor - xdr*delna0
	   rminorD = radminor
	   r=rminor0/sqrt(0.5*(1.0+elong**2))
	   SOL = 0
	   ioptFast = 0
7000     format(1x,"Psi0 --> Psi_sep  n0 =",I3)
         write(700,7000) n0
         radthet = riol(n0,1:9)

c     calculate minimim velocities for IOL of deuterium and carbon
	   call e0min(xm,ephi(n0),ephi(25),rminor0,rminorD,psinum,
	1        radthet,SOL,fpsi0(n0),RM,BTOR,radminor,emin,LO,n0,
     2		eminAll,eminS,eminAvgA)
		do 6667 i = 1,2
			do 6666 np0i = 1,22
				minEtta(np0i,n0,i) = 0.0	
6666		continue
6667	continue
c	calculate loss fractions for deuterium and carbon
	    call lossfrac(ioptFast,psinum,Tion,n0,emin,eminAll,delf,
	1	fthermal,mthermal,ethermal,fast1,fast2,fast3,minEtta)
	
c     sum over differential loss fractions
		if(n0.eq.24)then
			redySet = 0.0
			do 5779 i = 1,2
				do 5783 roh = 1,24
					do 5782 rnpsi = 1,22
						do 5781 rn = 1,8
							do 5780 rm = 1,8
								redySet = redySet + 
     1							delf(rn,rm,rnpsi,roh,1,i)
5780						continue
5781					continue
5782				continue
5783			continue
5779		continue
		endif
c     define particle, momentum, and energy loss fractions for edgecalc.for

c     Option to have no IOL
c      fthermal = 0.0
c	  mthermal = 0.0
c	  ethermal = 0.0
c      Option to make vintrinC = VintrinD/2.5 
c      mthermal(2) = mthermal(1)
       
		if(n0.lt.2)then
			forbl(n0) = RLOSSIOL*fthermal(1)
			forblC(n0) = RLOSSIOL*fthermal(2)
			xmorbl(n0) = RLOSSIOL*mthermal(1)
			xmorblC(n0) = RLOSSIOL*mthermal(2)
			vintrin(n0) = 2/sqrt(xpi)*xmorbl(n0)*sqrt(2*xk*Tion/xm(1))
			vintrinC(n0) = 2/sqrt(xpi)*xmorblC(n0)*sqrt(2*xk*Tion/xm(2))
			eorbl(n0) = RLOSSIOL*ethermal(1)
			eorblC(n0) = RLOSSIOL*ethermal(2)
			therms(n0,1) = fthermal(1)
			therms(n0,2) = mthermal(1)
			therms(n0,3) = ethermal(1)
			thermsC(n0,1) = fthermal(2)
			thermsC(n0,2) = mthermal(2)
			thermsC(n0,3) = ethermal(2)
		else
			therms(n0,1) = fthermal(1)
			therms(n0,2) = mthermal(1)
			therms(n0,3) = ethermal(1)
			thermsC(n0,1) = fthermal(2)
			thermsC(n0,2) = mthermal(2)
			thermsC(n0,3) = ethermal(2)
			forbl(n0) = forbl(n0-1) + RLOSSIOL*therms(n0,1)
			forblC(n0) = forblC(n0-1) + RLOSSIOL*thermsC(n0,1)
			xmorbl(n0) = xmorbl(n0-1) + RLOSSIOL*therms(n0,2)
			xmorblC(n0) = xmorblC(n0-1) + RLOSSIOL*thermsC(n0,2)
			vintrin(n0) = 2/sqrt(xpi)*xmorbl(n0)*sqrt(2*xk*Tion/xm(1))
			vintrinC(n0) = 2/sqrt(xpi)*xmorblC(n0)*sqrt(2*xk*Tion/xm(2))
			eorbl(n0) = eorbl(n0-1) + RLOSSIOL*therms(n0,3)
			eorblC(n0) = eorblC(n0-1) + RLOSSIOL*thermsC(n0,3)
		endif
c**************************************************************
c     Scrape Off Layer Calculation - Currently non-operational
c************************************************************** 
c     calculates e0min for every value of rho to every
c     SOL flux surface
         if(ioptsol.eq.1)then
	      ns = 15
7100        format(1x,"Psi0 --> Psi_sol  n0,m =",2I2)
6000        format(1x,f10.3,1x,15f10.3)
	      do 200 m=1,ns
               rSOL = radminor + m*delnaf*aminor
	         phiSOL = tesol*exp(-(rSOL-radminor)/delT)
	         SOL = 1
			 ioptFast = 0			   
	         psinum = 22
               write(700,7100) n0,m
	         call e0min(xm,ephi(n0),phiSOL,rminor0,rminorD,psinum,
	1        radthet,SOL,fpsi0(n0),RM,BTOR,radminor,emin,LO,n0,
     2		eminAll,eminS,eminAvgA)
		do 5655 i = 1,2
			do 5656 np0i = 1,22
				minEtta(np0i,n0,i) = 0.0	
5656		continue
5655	continue

			call lossfrac(ioptFast,psinum,Tion,n0,emin,eminAll,delf,
	1		fthermal,mthermal,ethermal,fast1,fast2,fast3,minEtta)
			if(n0.lt.2)then
	         fsol(n0,m) = fthermal(1)
	         msol(n0,m) = mthermal(1)
	         esol(n0,m) = ethermal(1)
			else
				fsol(n0,m) = fsol(n0-1,m) + fthermal(1)
				msol(n0,m) = msol(n0-1,m) + mthermal(1)
				esol(n0,m) = esol(n0-1,m) + ethermal(1)
			endif
200	      continue   
            write(600,6000) forbl(n0),(fsol(n0,m),m=1,15)
	   endif
15	continue


C	WRITING TO PLOTTING OUPUTS
	write(9000,'(1x,35A)')'eminAll(22,8,24,8),[npsi,n,roh,m]'
	write(9000,5078) eminAll
c	STPB	
c	write(9001,'(1x,35A)')'22,8,24,8'
c	write(9001,5078)eminAll	

c	 Set Direction Cosines
      xline(1)=21.0/22.0
 	xline(2)=19.0/22.0
	xline(3)=17.0/22.0
	xline(4)=15.0/22.0
	xline(5)=13.0/22.0
	xline(6)=11.0/22.0
	xline(7)=9.0/22.0
	xline(8)=7.0/22.0
	xline(9)=5.0/22.0
	xline(10)=3.0/22.0
	xline(11)=1.0/22.0
	xline(12)=-1.0/22.0
	xline(13)=-3.0/22.0
	xline(14)=-5.0/22.0
	xline(15)=-7.0/22.0
	xline(16)=-9.0/22.0
	xline(17)=-11.0/22.0
	xline(18)=-13.0/22.0
	xline(19)=-15.0/22.0
	xline(20)=-17.0/22.0
	xline(21)=-19.0/22.0
	xline(22)=-21.0/22.0	
c	Set Direction Cosines to 22nd Legendre Roots
c	xline(1) = 0.994294585
c	xline(2) = 0.970060498
c	xline(3) = 0.926956772
c	xline(4) = 0.865812578
c	xline(5) = 0.787816806
c	xline(6) = 0.694487263
c	xline(7) = 0.587640404
c	xline(8) = 0.469355838
c	xline(9) = 0.341935820
c	xline(10)= 0.207860427
c	xline(11)= 0.069739273
c	xline(12)=-0.069739273
c	xline(13)=-0.207860427
c	xline(14)=-0.341935820
c	xline(15)=-0.469355838
c	xline(16)=-0.587640404
c	xline(17)=-0.694487263
c	xline(18)=-0.787816806
c	xline(19)=-0.865812578
c	xline(20)=-0.926956772
c	xline(21)=-0.970060498
c	xline(22)=-0.994294585
c     Set stepper variable
	step = (1 - 0.864)/24
c     *********************************   
c     * ITERATE OVER DIRECTION COSINE *
c     *********************************
     	do 1234 i=1,22
c		Set initial radius
		radius = 0.864	
c		********************************
c		* ITERATE OVER RADIAL DISTANCE *		
c		********************************
		do 1235 k=1,24
c			********************************
c			* ITERATE INTERIOR RING POINTS *		
c			********************************
			do 1236 j=1,8
c				********************************
c				* ITERATE OVER EXTERIOR POITNS *		
c				********************************
				do 1237 l= 1,8
c	            Initialize lowest energy temp variable
	            if (l.eq.1 .AND. j.eq.1) then
					LowestE = eminAll(i,j,k,l,1)
				end if
c				Check for extraneous solution (==0) and
c					test if lowest value 
				if(j.ne.1 .AND. l.ne.1 
	1				.AND. eminAll(i,j,k,l,1).gt.0
	2				.AND. eminAll(i,j,k,l,1).lt.LowestE) then
c					Record the lowest l(L) value: 
		 			LowestE = eminAll(i,j,k,l,1)
				end if
1237				continue			   
1236			continue
c		Write output to files
		write(9001,5077)xline(i),radius,LowestE
c		Step radius
		radius = radius + step
1235		continue
1234	continue	
c	STPC
	write(9002,'(1x,35A)')'Tion(24)'
	write(9002,5078)xti	
c	STPD 
  	write(9003,'(1x,35A)')'delf(8,8,22,24,3,2),
	1		[n,m,npsi,roh,k,i],k{Ions,Mom,Eng},i{species}'
	write(9003,5078)delf
c	STPE
   	write(9004,'(1x,35A)')'forbl(24)'
	write(9004,5078)forbl
	
c	STPF
	write(9005,'(1x,35A)')'xmorbl(24)'
	write(9005,5078)xmorbl
c	STPG
	write(9006,'(1x,35A)')'vintrin(24)'
	write(9006,5078)vintrin
c	STPH
	write(9007,'(1x,35A)')'eorbl(24)'
	write(9007,5078)eorbl
c	STPI
	write(9008,'(1x,35A)')'minEtta(22,24)'
	write(9008,5078)minEtta
c   STPJ
	write(9009,'(1x,35A)')'ephi(??)'
	write(9009,5078)ephi
c   STPK
	write(9010,'(1x,35A)')'forblC(24)'
	write(9010,5078)forblc
c   STPL
	write(9011,'(1x,35A)')'xmorblc(24)'
	write(9011,5078)xmorblc
c   STPN
	write(9013,'(1x,35A)')'eorblc(24)'
	write(9013,5078)eorblc
c   STPO
	write(9014,'(1x,35A)')'vintrinC(24)'
	write(9014,5078)vintrinC
	
C	END OF WRITING TO PLOTTING OUTPUTS

c     Calculate gradient of thermal IOL for continuity equation in edgecalc.for
      do 501 n0=1,23
	   dFdr(n0) = (forbl(n0+1)-forbl(n0))/(rhor(n0+1)-rhor(n0))
	   Edr(n0) = (eorbl(n0+1)-eorbl(n0))/(rhor(n0+1)-rhor(n0))
501	continue

	 
c *******************************************************************
c      X-TRANSPORT INPUT - to be updated 
c*******************************************************************
      thetax = 4.712
 	deltathetx = 0.15			    
  	deltarx = radminor*deltathetx
	rminorx = radminor - deltarx
	xy = xmu0*Icur/6.283
    
c	 --------------------------------------------------------------------
c     Write loss fractions to orbloss.txt
1043	format (I3, 6f10.3)
1044	format (I3, 5e10.3)
1046  format(I2,2x,f10.2,2x,f10.2,2x,2f10.2,2x,2e12.3)
	write(121,'(1x,20A)') '  n0    forbl   xmorbl  eorbl' 						 
	do 2223 n = 1,24
	xmv = 1.13*sqrt(2.*xk*xti(n)/xm(1))*yy1(n)
	write(121,1043) n, forbl(n), xmorbl(n), eorbl(n)
2223	continue
      write(121,'(1x,20A)') '  n0    forblC   xmorblC  eorblC'
      do 2226 n = 1,24
	write(121,1043) n, forblC(n), xmorblC(n), eorblC(n)
2226    continue
	write (121,'(1x20A)')' n0   intrinD   intrinC  NBIreturn   Snbi
	1   dFdr'
c **********************************************
c     Intrinsic Rotation
c***********************************************
	do 2224 n = 1, 24
	yy1(n)=  1.13*sqrt(2.*xk*xti(n)/xm(1))*xmorbl(n)
 	yy2(n)=  1.13*sqrt(2.*xk*xti(n)/xm(2))*xmorblC(n)
	write(121,1044) n, yy1(n), yy2(n),NBIreturn(n),Snbi(n),dFdr(n)
2224	continue 
c	this ends loop on n0 for calculating forbl(n0), morbl(n0),eorbl(n0)
      write (121,'(1x20A)')'n0         ffrac1    ffrac2    ffrac3
	1  nbdep1    nbilost   nbitot'
	do 2225 n = 1,50
	write(121,1046) n,ffrac1(n),ffrac2(n),ffrac3(n),
 	1               nbdep1(n),nbilost(n),nbitot(n)
2225	 continue  
	   
c**************************************************************
c            RUN GTEDGE MAIN SUBROUTINES
c**************************************************************
C	SOL/DIVERTOR PLASMA + PARTICLE/HEAT FLUX + CORE	SOLUTION 
25    CALL DIVERT3

C	DIVERTOR/SOL PLASMA-NEUTRAL & CORE PART/HEAT FLUX INTO SOL
C     CONVERGED.

C	EVALUATE THE MARFE DENSITY LIMIT																		
C	EVALUATE RADIAL INSTABILITY GROWTH RATES IN TRANSPORT BARRIER
50	CALL DENLIM 
75	IF(JIT.EQ.1) GOTO 100
      CALL DISRUPT
100	CALL EDITDIV

C	EVALUATE THERMAL STABILITY OF DIVERTOR
	CALL DIVSTAB
c     evaluate edge pedestal moment equations
 	IF(IOPTEDGE.EQ.1) CALL EDGECALC 


C**************************************
c     CALCULATE IOL FLUXES INTO SOL
c**************************************
c	integrate the loss to SOL octants
	do 228 n = 1,8
		solv(n) = 0.0 
		soln(n) = 0.0
		sole(n) = 0.0 
228   continue   
      do 231 n0 = 1,rholength
		xgamv = -1.0*(2.0/(3.141592654**(3.0/2.0)))*(xmorbl(n0))
     1			*sqrt(2.0*xti(n0)/zmass)
		xgamn = gamion(n0,1)*(forbl(n0))
		xgame = gamheati(n0)*(eorbl(n0))
		areafactor = (rhor(n0)/rhor(25))*1.0
		do 229 m = 1,8
			Tsolf(m,n0) = 0.0
			Tsolm(m,n0) = 0.0 
			Tsole(m,n0)	= 0.0
229		continue
231	continue

c	 ***********Miller Eq & 1D divertor solution*********************************
c     DIVSOL(orbfluxn,orbfluxv,orbfluxe,bigH,qheat,gpart)


c	CALL DIVSOL(soln,solv,sole,bigH,qheat,gpart)	

	do 233 n = 1,30
	
      gam25con(n) = fpart(n+10)*(1.- Forbl(24))
	gamQ25con(n) = fheat(n+10)*(1.- Eorbl(24))
	if(n.ge.22.and.n.le.25) gamorbl(n) =  7.5*gamion(21,1)*Forbl(21)
	if(n.ge.22.and.n.le.25) gamQorbl(n) =  7.5*gamheati(19)*Eorbl(19) 	
	if(n.ge.22.and.n.le.25) gamVorbl(n) =  7.5*gamion(21,1)*xMorbl(21)
     2	*xmas(1)

      if(n.ge.6.and.n.le.9) 
     1gamorbl(n) =  7.5*(fpart(n+10)*Forbl(24) -
     2	gamion(21,1)*Forbl(21))
	if(n.ge.6.and.n.le.9) 
     1gamQorbl(n) =  7.5*(fheat(n+10)*Eorbl(24) 
     2	- gamheati(19)*Eorbl(19))
	  
	if(n.ge.6.and.n.le.9) 
	1 gamVorbl(n) =  7.5*(fpart(n+10)*xMorbl(24) -
	2	gamion(21,1)*xMorbl(21))*xmas(1)
    	gamsum(n) = gam25con(n) + gamorbl(n)
	gamQsum(n) = gamQ25con(n) + gamQorbl(n)
		gamiller(n) = fpart(n+10)
	  gamQmiller(n) = fheat(n+10) 
233   continue
c***********distribution of fluxes into SOL**************
	OPEN(unit=132,FILE='distrsol.TXT',STATUS='UNKNOWN') 
c	********ion orbit loss fluxes***********************
	write (132,1061) 
	write (132,1007) (soln(n),n=1,8)
	write (132,1008) (solv(n),n=1,8)
	write (132,1009) (sole(n),n=1,8)
	x=areafactor
	write (132,'(1x,35A)')'mesh  gamcon   gamorb  gamsum    gamiller '
	do 234 n = 1, 30 
	write (132,10110) n,gam25con(n),gamorbl(n),gamsum(n),gamiller(n)
234	continue 
	write (132,'(1x,35A)')'mesh  gamQcon  gamQorb  gamQsum gamQmiller'
 	do 235 n = 1, 30 
	write (132,10110) n,gamQ25con(n),gamQorbl(n),gamQsum(n),
     1	gamQmiller(n)   
235	continue  
	write (132,'(1x,35A)')'	mesh  par_mom '
	do 236 n=1,30
	write (132,10110) n, gamVorbl(n)
236   continue

c     ----------Formatting Options----------------
1000	format(1x,"theta0 =",f10.3) 
1001  format(1x,"theta0=",9f9.3)
1002  format(1x,10e9.3) 
1003  format(6x, "floss",6x, "eloss")
1004	format(1x,2f10.3) 
1005	format(1x,"Flos =", f6.3,3x,"Vlos =",f6.3,3x,"Elos =",f6.3) 
1006	format(1x,"Flos2 =", f6.3,3x,"Vlos2 =",f6.3,3x,"Elos2 =",f6.3) 
1007	format(1x,"soln =", 8e9.3)
1008	format(1x,"solv =", 8e9.3)
1009	format(1x,"sole =", 8e9.3)
1010  format (9e9.3)
10110 format (i4,6e9.3) 
1061  format(1x, "ion orbit loss particle, momentum and energy")
1015	format(1x,"psi0 =", f6.3,3x,"Wtrap =",f9.2,3x,"Tion =",f8.3,
	1		3x,"r0 =",f6.3,3x,"rho =",f6.3)
1016	format(1x,"emin=",8f10.2)
1020	format(1x,"flos =", 8f6.3,1x,"av=",f6.3)
1021	format(1x,"vlos =", 8f6.3,1x,"av=",f6.3)


1022	format(1x,"elos =", 8f6.3,1x,"av=",f6.3)
1023	format(1x,"option2 flos=",f6.3,1x,"vlos=",f6.3,"elos=",f6.3)
1030	format(1x,"floss=", 8f6.3,1x,"sum=",f6.3)
1031	format(1x,"eloss=", 8f6.3,1x,"sum=",f6.3)
1040 	format(1x,"LO(npsi=1,22, m= 1,8)",23I3)
1041	format(f4.2,8i4)
1042	format (i4) 
	
	CLOSE(121,STATUS='UNKNOWN')
	
	CLOSE(132,STATUS='UNKNOWN')
 
1051	CONTINUE

C		SOLVE EDGE NEUTRAL & ION DISTRIBUTIONS
	yy=delx
	yz=delxreal 	 
	GOTO 400
	a1=f(1)
         
C		TERMINATION EDIT
300    WRITE (6,'(1x,50A)')'TEMPERATURE BELOW 1 EV OR COOLFRAC GT UNITY.
     2  TERMINATED'
305   RECYCLE =  CURSEP*(1.-ALPHASEP)/FLUXPART
      FUELPL = FUELPLIN + FUELPLOUT 
      WRITE(6,118) FUELPF,FUELPL,FUELMP,SPELLET
      WRITE (6,105) NZIMP1,FZ1,NZIMP2,FZ2,ZEFFC
      WRITE (6,113) IZINTRIN,FZINTRIN,IZINJECT,FZINJECT,ZEFF
118   FORMAT(1X,'PFFUEL=',E8.3,1X,'PLFUEL=',E8.3,1X,'SOLFUEL=',E8.3,1X,
     2       'PELFUEL=',E8.3)
105	  FORMAT (1X,'CORE IMP#1 Z & CONC=',I3.0,F6.4,2X,'IMP#2 Z & CONC=',
     2          I3.0,F6.4,2X,'CORE ZEFF=',F6.4)
113   FORMAT (1X,' DIV IMP#1 Z & CONC=',I3.0,F6.4,2X,'IMP#2 Z & CONC='
     2       ,I3.0,F6.4,2X,'DIV  ZEFF=',F6.4)
120	  FORMAT (1x,'POWFRACS: CORERAD=',F5.3,1x,'DIVRAD=',F5.3,1x,'ATWALL=
     2  ', F5.3,1x,'DPLAS=',F5.3,1x,'TOTAL=',F5.3) 
      WRITE(6,120) FRACRAD,FRACRADIV,FRACATOM,FRACPLASDP,FRACTOT
117   FORMAT(1X,'HEATFLUX=',E9.3,1X,'PARTFLUX=',E9.3,1X,'FRACHEATSOL=',
     2   E9.3,1X,'RECYCLE=',F8.3)
      WRITE(6,117) FLUXHEAT,FLUXPART,FRACSOL,RECYCLE
103   FORMAT (1X,'CENTRAL NE=',E10.3,1X,'EDGE NE=',E10.3,1X,'AVGNE=',
     2        E10.3,1X,'ALPHAN=',F4.2)
104	  FORMAT (1X,'CENTRAL TE=',E10.3,1X,'EDGE TE=',E10.3,1X,'AVGTE=',
     2        E10.3,1X,'ALPHAT=',F4.2)
      WRITE (6,103) XN0,XNPED,XNAV,ALPHAN
      WRITE (6,104) T0,TPED,TAV,ALPHAT
      WRITE(6,'(1X,14A)') 'PLASMA DENSITY'  
      WRITE(6,1011) XND,XNDIV,XNSEP,XNBAR,XNPED
      WRITE(6,'(1X,20A)') 'PLASMA TEMPERATURE'
      WRITE(6,1011) TD, TDIV,TSEP,TBAR,TPED 
      WRITE(6,'(1X,15A)') 'NEUTRAL DENSITY'
      WRITE(6,1011) XNOD,XNODIV,XNOSOL,XNOBAR,XNOPED
      WRITE (6,121) RPARTT,RPARTN,RPARTP,RPARTTC,RPARTC,itern
      WRITE (6,123) FLUXNEUTIN,FLUXIONIN,FLUXPART
121	  FORMAT (1X,'PARTICLE CONVERGE: TOTS0LDIV=',E10.5,1X,'ATOMSOLDIV=
     2 ', E10.5,1X,'IONSOLDIV=',E10.5,1X,'TOTCHAM=',E10.5,1X,'IONCORE=
     3  ', E10.5,'#ITER=',I4) 
123	  FORMAT (1X,'FLUXNEUTIN=',E9.4,1X,'FLUXIONIN=',E9.4,1X,
     2      'FLUXPART=',E9.4)
1011  FORMAT(7E10.3)
      GOTO 400
350	  WRITE (6,'(1X,27A)') 'ITERATION TERMINATED AT 100'  
      GOTO 100
375	  WRITE (6,'(1X,32A)') 'ITERATION STOPPED; X > 1 5 TIMES'	
400	  STOP
	  END
