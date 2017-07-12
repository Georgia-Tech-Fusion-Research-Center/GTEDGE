	subroutine e0min(xm,ephi0,ephiD,rminor0,rminorD,psinum,
	1 radthet,SOL,fpsi0,RM,BTOR,radminor,emin,LO,nnn0,eminAll,
     2 eminS,eminAvgA)
c     Calculates the minimum loss energy for fast or thermal ions given a particle mass, initial flux surface,
c     a final flux surface, electrostatic potential, and pitch angle
	 use msimsl
	include 'soldiv.fi'

	parameter (nit=9,npsi0=22,nn=30,nit0=8,nn0n=24,k0=2)
	dimension v0m(nit,nit,k0),v0p(nit,nit,k0),xke0m(nit,nit,k0),
	1	      xke0p(nit,nit,k0),angle0(nit),angleD(nit),
     2          xkeDM(nit,nit,k0),xkeDp(nit,nit,k0),
     3           emin(npsi0,nit,nn,k0),radthet(nit),
     4          delpsi(npsi0,nit),psidiff(nit),LO(npsi0,nit,nn,k0),
     5		  eminall(npsi0,nit0,nn0n,nit0,k0),
     6	      eminSort(npsi0,nit0,nn0n,nit0,k0),eminAvg(nit0,nn0n,k0),
     7          eminAvgS(nn0n,k0),eminS(nn0n,k0),xm(k0),
     8		  xline(npsi0),eminNpsi(nit0,nn0n,k0),
     9          eminAvgA(nit0,nit0,nn0n,k0) 
      real xm,ephi0,ephiD,rminor0,rminorD,fpsi0,xmu0,betap,li,FS0,
	1     FSD,lamb,radminor,xxI,plasmacur,delpsi,triang,elong,
     2     RM,eminAll,eminSort,xSrt,eminAvg,eminAvgS,eminS,xline,
     3	 eminNpsi,eminAvgA 
	integer psinum,npsi,shavshift,LO

	open(500,file='bugging2.txt',status='old')
	open(700,file='psiSOL.txt',status='old')

      shavshift = 0
	psireal = 1
      xmu0 = 1.257
 	xpi = 3.14159
	betap = 0.834
	li = 0.82
	lamb = betap+li/2-1
	Rs0 = RM
	RsD = RM
	if(shavshift.eq.1)then
	   Rs0 = RM-0.5*(li/2+betap)*rminor0**2
	   RsD = RM-0.5*(li/2+betap)*rminorD**2
	endif

c	calc loss for different direction cosines thru 200, for launch surfance n0
c     only do one calculation for fast ions - all oriented at the same psi0
c      22 different psi0 locations for deuterium and carbon
c      psi0 = -0.99
	
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
	do 400 k = 1,2
		do 200 npsi = 1,psinum
			psi0 = xline(npsi)
			if(psinum.eq.1)then
				psi0 = fpsi0
			endif
			theta0 = 0.0
			thetaD = 0.0
	
			do 99 n = 1,9 
				emin(npsi,n,nnn0,k) = 0.0
				do  90 m = 1,9
					xxI = xmu0*plasmacur/(2.0*xpi*(radminor**2))
					fphiD = 1./sqrt(1. + (xxI**2)*((radminor**4)/
	1				(rminorD**2))/(Btor**2))
					fphi0 = 1./sqrt(1. + (xxI**2)*((radminor**4)/
 	1				(rminor0**2))/(Btor**2))
					bthetaD = xmu0*plasmacur*2*(log(8*RsD/radminor)-2)
     1				/(2*xpi*rminorD)
					btheta0 = xmu0*plasmacur*2*(log(8*Rs0/radminor)-2)
     1				/(2*xpi*rminor0)
					h0 = 1. + (rminor0/Rs0)*cos(theta0)
					hd = 1. + (rminorD/RsD)*cos(thetaD)
					delphi = ephi0 - ephiD
					xpot = 2.*(eq(k)/xm(k))*delphi
					xflux= xxI*(eq(k)/(1.*xm(k)*hd*fphiD))*(rminor0**2
     1				-rminorD**2)
c****charge and mass enter as e/m in xpot and xflux***************
c	- WRONG - 
s	         		a =(h0/hd)*(1.-(psi0**2))-(((h0/hd)*(fphi0/fphid)*psi0)
s	1				**2)- 1.
c	- CORRECT - 
					a =(h0/hd)*(1.-(psi0**2))+(((h0/hd)*(fphi0/fphid)*
	1				psi0)**2)- 1.

					bterm = 2.*xflux*(h0*fphi0)*psi0/(2.0*hD)
c			  - incorrect - (below)
c	         		bterm = 2.*xflux*(h0*fphi0)*psi0

					c = ((xflux**2)/4.0) - xpot
c			  - incorrect - (below)
c	         c = (xflux**2) - xpot
					abc = 4.*a*c/(bterm**2)
					if(abc.gt.1.0) goto 80
					v0p(n,m,k) = -1.0*(bterm/(2.*a))*(1.0+sqrt(1.-4.*a
	1			    *c/(bterm**2)))
					v0m(n,m,k) = -1.0*(bterm/(2.*a))*(1.0-sqrt(1.-4.*a
	1			    *c/(bterm**2)))
					goto 85
80					v0p(n,m,k) = 0.0
					v0m(n,m,k) = 0.0
c***  min reduced energy depends on mass**********************
85	         		xke0m(n,m,k) = 0.5*xm(k)*(v0m(n,m,k)**2)/xk
					xke0p(n,m,k) = 0.5*xm(k)*(v0p(n,m,k)**2)/xk
					xkeDm(n,m,k) = xke0m(n,m,k) + (ephi0-ephiD)
					xkeDp(n,m,k) = xke0p(n,m,k) + (ephi0-ephiD)
					thetaD = thetaD + xpi/4.
90	      		continue
				thetaD = 0.0
				theta0 = theta0 + xpi/4.
99	   		continue
			angle0(1) = 0.0
			angleD(1) = 0.0
			do 150 n=2,9
				angle0(n) = angle0(n-1) + xpi/4.
				angled(n) = angled(n-1) + xpi/4.
150			continue 
c	Minimum Loss Energy for each Theta0
c	Assumes that for each launch theta0 and npsi, the thetaD with the smallest  
c	minimum Loss Energy defines the lower energy of the loss cone at that theta0
c	emin(npsi,theta0)
			Do 155 n=1,8
				emin(npsi,n,nnn0,k) = 1.0e9
				LO(npsi,n,nnn0,k) = 0.0 
c		  Choose smallest magnitude Positive velocity	
				Do 157 m=1,8
					eminAll(npsi,n,nnn0,m,k) = 0.0
c				Both Negative - Drop - 
					if((v0m(n,m,k).lt.0.0.and.v0p(n,m,k).lt.0.0).or.
	1				(v0m(n,m,k).eq.0.0.and.v0p(n,m,k).eq.0.0))goto 156 
c			v0m +p and v0p -n -> choose v0m 
					if(v0m(n,m,k).gt.0.0.and.v0p(n,m,k).lt.0.0)then
						if(emin(npsi,n,nnn0,k).gt.xke0m(n,m,k))then
							emin(npsi,n,nnn0,k) = xke0m(n,m,k)
							LO(npsi,n,nnn0,k) = m
							eminAll(npsi,n,nnn0,m,k)=xke0m(n,m,k)
						else
							eminAll(npsi,n,nnn0,m,k)=xke0m(n,m,k)
						endif
					endif
c			 	v0m -n and v0p +p -> choose v0p
					if(v0m(n,m,k).lt.0.0.and.v0p(n,m,k).gt.0.0)then
						if(emin(npsi,n,nnn0,k).gt.xke0p(n,m,k))then
							emin(npsi,n,nnn0,k) = xke0p(n,m,k)
							LO(npsi,n,nnn0,k) = m
							eminAll(npsi,n,nnn0,m,k)=xke0p(n,m,k)
						else
							eminAll(npsi,n,nnn0,m,k)=xke0p(n,m,k)
						endif
					endif
c			 Both Positive - Continue -
					if(v0m(n,m,k).gt.0.0.and.v0p(n,m,k).gt.0.0)then
c						v0m < v0p -> choose v0m
						if(v0m(n,m,k).lt.v0p(n,m,k))then
							if(emin(npsi,n,nnn0,k).gt.xke0m(n,m,k))then
								emin(npsi,n,nnn0,k) = xke0m(n,m,k)
								LO(npsi,n,nnn0,k) = m
								eminAll(npsi,n,nnn0,m,k)=xke0m(n,m,k)
							else
								eminAll(npsi,n,nnn0,m,k)=xke0m(n,m,k)
							endif
						endif
c				v0m > v0p -> choose v0p
						if(v0m(n,m,k).gt.v0p(n,m,k))then
							if(emin(npsi,n,nnn0,k).gt.xke0p(n,m,k))then
								emin(npsi,n,nnn0,k) = xke0p(n,m,k)
								LO(npsi,n,nnn0,k) = m
								eminAll(npsi,n,nnn0,m,k)=xke0p(n,m,k)
							else
								eminAll(npsi,n,nnn0,m,k)=xke0p(n,m,k)
							endif
						endif
					endif	  
156				continue
157	      		continue
155	   		continue	       
200		continue

c	Trapezoidal Integration and Averaging
		do 301 n = 1,8
			do 4000 m = 1,8
				eminAvgA(n,m,nnn0,k) = 0.0
				do 300 npsi = 1,21
					eminAvgA(n,m,nnn0,k) = eminAvgA(n,m,nnn0,k)+
	1						((xline(npsi+1)-xline(npsi))*
	2						(eminAll(npsi+1,n,nnn0,m,k)+
     3						eminAll(npsi,n,nnn0,m,k)))/2.0/2.0
300				continue
4000		continue
301		continue


c	Trapezoidal Integration and Averaging
		do 4002 n = 1,8
			eminAvg(n,nnn0,k) = 0.0
			do 4001 npsi = 1,21
				eminAvg(n,nnn0,k) = eminAvg(n,nnn0,k)+
	1						  ((xline(npsi+1)-xline(npsi))*
	2							(emin(npsi+1,n,nnn0,k)+
     3						emin(npsi,n,nnn0,k)))/2.0/2.0
4001		continue
4002	continue

c	Finds the minimum energy for each flux surface from eminAvg  	
		eminAvgS(nnn0,k) = min(eminAvg(1,nnn0,k),eminAvg(2,nnn0,k),
	1					eminAvg(3,nnn0,k),eminAvg(4,nnn0,k),
     2					eminAvg(5,nnn0,k),eminAvg(6,nnn0,k),
     3					eminAvg(7,nnn0,k),eminAvg(8,nnn0,k))
c	Finds the minimum energy for each flux surface from emin
		do 351 n = 1,8
			eminNpsi(n,nnn0,k) = emin(1,n,nnn0,k)
			do 350 npsi = 2,22
				eminNpsi(n,nnn0,k) = min(eminNpsi(n,nnn0,k),
	1								emin(npsi,n,nnn0,k))
350			continue
351		continue
		eminS(nnn0,k) = min(eminNpsi(1,nnn0,k),eminNpsi(2,nnn0,k),
 	1					eminNpsi(3,nnn0,k),eminNpsi(4,nnn0,k),
     2					eminNpsi(5,nnn0,k),eminNpsi(6,nnn0,k),
     3					eminNpsi(7,nnn0,k),eminNpsi(8,nnn0,k))
c	End of Averaging and minimum finding

c	formatting options
710		format("zeta =",f10.3,1x,"delpsi =",8e9.3)
720		format(16x,"psidiff = ",8e9.3)
c     STPM
	write (9012,*) eminS(nnn0,k)
400	continue
	return
	end