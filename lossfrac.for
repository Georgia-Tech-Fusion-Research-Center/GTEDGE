	subroutine lossfrac(ioptFast,psinum,Tion,n0,emin,eminAll,delf,
	1		fthermal,mthermal,ethermal,fast1,fast2,fast3,minEtta)
c       Calculates total and differential loss fraction for thermal 
c		or total LF for fast ions for a given emin matrix	  
	 use msimsl
	include 'soldiv.fi'

	parameter (nit=8,np0=22,nn=24,kAk=3)
	dimension emin(22,8,30,2),eminAll(np0,nit,nn,nit,2),
	1	delf(nit,nit,np0,nn,kAk,2),Aeh(kAk),bigG(kAk),
     2    minEtta(np0,nn,2),mb(8),cw(8),ccw(8),
     3    Etta(np0,nit,nit,2),flrone(np0,nit,nit,kAk,2),
     4    orderf(nit,2),sortf(nit,2),flr1mst(nit,nit,np0,2),
     5    order1mst(nit,nit,np0,2),order1nst(nit,np0,2),
     6    rohitize(nit*nit,2),orderR(nit*nit,2),sortR(nit*nit,2),
     7    rohit1st(nit*nit,np0,2),actord1st(nit*nit,np0,2),
     8	octet(nit*nit,2),chopM(nit,2),sortM(nit,2),
     9    octetNcwORccw(nit*nit,2),xline(np0)

      real Tion,emin,eminAll,delf,fthermal(2),mthermal(2),ethermal(2),
	1	fast2,fast3,Aeh,bigG,minEtta,Etta,flrone,sortf,xst,flr1mst,
     2	rohitize,sortR,rohit1st,octet,chopM,sortM,topedge,fast1
     3	octetNcwORccw,slice,sumdelf,multFix,fthermnew(2),mthermnew(2),
     4	ethermnew(2),lost1,lost2,lost3,
	5	tally,ftpo,ftpn,xline

	integer ioptFast,psinum,n0,roh,mb,cw,ccw,
	1	orderf,jst,order1mst,order1nst,orderR,nm,actord1st,
     2	stayloop
	if(ioptFast.eq.1) goto 511

c	New Section using enhanced methods [MTS: 12/15/2014]
c      delf = 0.0
	roh = n0
	Aeh(1) =  1.5
	Aeh(2) =  2.0
	Aeh(3) =  2.5
	bigG(1)	= GAMMA(Aeh(1))
	bigG(2)	= GAMMA(Aeh(2))
	bigG(3)	= GAMMA(Aeh(3))
	do 5555 np0i = 1,22
		minEtta(np0i,roh,1) = 21.0
		minEtta(np0i,roh,2) = 21.0
5555  continue

	mb(1) = 8
	mb(2) = 7
	mb(3) = 6
	mb(4) = 5
	mb(5) = 4
	mb(6) = 3
	mb(7) = 2
	mb(8) = 1
		
      cw(1) = 8
	cw(2) = 1
	cw(3) = 2
	cw(4) = 3
	cw(5) = 4
	cw(6) = 5
	cw(7) = 6
	cw(8) = 7
	
	ccw(1) = 2
	ccw(2) = 3
	ccw(3) = 4
	ccw(4) = 5
	ccw(5) = 6
	ccw(6) = 7
	ccw(7) = 8
	ccw(8) = 1 

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
             

c	Generate loss fraction matrix for ions, energy and momentum for the entire flux surface (roh)
	do 6000 i = 1,2
	do 1004 npsi = 1,22
		do 1003 n = 1,8
			do 1002 m = 1,8
c				Generate the Etta values we plug into the gamma functions 
				Etta(npsi,n,m,i) = eminAll(npsi,n,roh,m,i)/Tion
				if(Etta(npsi,n,m,i).eq.0.0)then
					Etta(npsi,n,m,i) = 21.0
				endif

				do 1001 k = 1,3
					if(Etta(npsi,n,m,i).gt.0.0)then
						if(Etta(npsi,n,m,i).lt.
     1					minEtta(npsi,roh,i))then		 
							minEtta(npsi,roh,i) = Etta(npsi,n,m,i)
						endif
						if(roh<2)then
							if(Etta(npsi,n,m,i).lt.20.0)then
								flrone(npsi,n,m,k,i) = ((bigG(k)
	1							-GAMI(Aeh(k),Etta(npsi,n,m,i)))
     2							/bigG(k))/22.0
								if(flrone(npsi,n,m,k,i).lt.0.0.or.
	1								flrone(npsi,n,m,k,i).gt.1.0)then
									flrone(npsi,n,m,k,i) = 0.0
								endif
							else
								flrone(npsi,n,m,k,i) = 0.0
							endif
						else
						if(Etta(npsi,n,m,i).lt.
     1					minEtta(npsi,roh-1,i))then
								flrone(npsi,n,m,k,i) = 
	1							((GAMI(Aeh(k),minEtta(npsi,roh-1,i)) 
     2							-GAMI(Aeh(k),Etta(npsi,n,m,i)))
     3							/bigG(k))/22.0
								if(flrone(npsi,n,m,k,i).lt.0.0.or.
	1								flrone(npsi,n,m,k,i).gt.1.0)then
									flrone(npsi,n,m,k,i) = 0.0
								endif
							else
								flrone(npsi,n,m,k,i) = 0.0
							endif
						endif
					else
						flrone(npsi,n,m,k,i) = 0.0
					endif
1001				continue
1002			continue
1003		continue
1004	continue
c		Sorting section (yes an insertion sort is slow but easy to program 
c		and as algarithms go it's not too bad over short sets like these)
	do 4000 k = 1,3
		do 1005 npsi = 1,22
c			Sort fractions according to Theta_S (m), the poloidal exit location
			do 1013 n = 1,8
				orderf(1,i) = 1
				sortf(1,i) = flrone(npsi,n,1,k,i)
				do 1015 m = 2,8
					orderf(m,i) = 0
					sortf(m,i) = 0.0
					xst = flrone(npsi,n,m,k,i)
					jst = m
					do while(jst.gt.1.and.sortf(jst-1,i).gt.xst)
						sortf(jst,i) = sortf(jst-1,i)
						orderf(jst,i) = orderf(jst-1,i)
						jst = jst - 1
					enddo
					sortf(jst,i) = xst
					orderf(jst,i) = m
1015				continue
				do 1014 m = 1,8
					flr1mst(n,m,npsi,i) = flrone(npsi,n,
     1				orderf(m,i),k,i)
					order1mst(n,m,npsi,i) = orderf(m,i)
1014				continue
1013			continue
		
c			Sort Theta_0 (poloidal launch position) fractions flr1mst by largest sorted Theta_S
			orderf(1,i) = 1
			sortf(1,i) = flr1mst(1,8,npsi,i)
			do 1012 n = 2,8
				orderf(n,i) = 0
				sortf(n,i) = 0.0
				xst = flr1mst(n,8,npsi,i)
				jst = n
				do while(jst.gt.1.and.sortf(jst-1,i).gt.xst)
					sortf(jst,i) = sortf(jst-1,i)
					orderf(jst,i) = orderf(jst-1,i)
					jst = jst - 1
				enddo
				sortf(jst,i) = xst
				orderf(jst,i) = n
1012			continue
			do 1011 n = 1,8
				order1nst(n,npsi,i) = orderf(n,i)
1011			continue

c			Sort fractions by Theta_0 and Theta_S
			do 1009 n = 1,8
				do 1010 m = 1,8
                    rohitize(n+(m-1)*8,i) = flrone(npsi,n,m,k,i)
1010				continue
1009			continue
			orderR(1,i) = 1
			sortR(1,i) = rohitize(1,i)
			do 1007 nm = 2,64
				orderR(nm,i) = 0
				sortR(nm,i) = 0.0
				xst = rohitize(nm,i)
				jst = nm
				do while(jst.gt.1.and.sortR(jst-1,i).gt.xst)
					sortR(jst,i) = sortR(jst-1,i)
					orderR(jst,i) = orderR(jst-1,i)
					jst = jst -1
c1008				
				enddo
				sortR(jst,i) = xst
				orderR(jst,i) = nm
1007			continue
			do1006 nm = 1,64
				rohit1st(nm,npsi,i) = rohitize(orderR(nm,i),i)
				actord1st(orderR(nm,i),npsi,i) = nm
1006			continue	
1005		continue
c		End of Sorting Section

c		Allocations & Ajustment Section	
		do 2000 npsi = 1,22
			octet(1:64,i) = rohit1st(1:64,npsi,i)/8.0
			do 2001 itit = 1,8
				n = order1nst(itit,npsi,i)
				do 2013 m = 8,2,-1
					chopM(m,i) = (octet(actord1st((n
	1				+(order1mst(n,m,npsi,i)-1)*8),npsi,i),i)
     2				-octet(actord1st((n+(order1mst(n,m-1,npsi,i)-1)
     3				*8),npsi,i),i))/(mb(m)*1.0)
2013				continue
				chopM(1,i) = octet(actord1st((n
	1			+(order1mst(n,1,npsi,i)-1)*8),npsi,i),i)/8.0
				do 2011 m = 1,8
					sortM(m,i) = 0.0
					mqm = m
					do 2012 mrm = 1,mqm
						sortM(m,i) = sortM(m,i) + chopM(mrm,i)
2012					continue
2011				continue
				do 2010 m = 1,8
					delf(n,order1mst(n,m,npsi,i),npsi,roh,k,i) = 
	1				delf(n,order1mst(n,m,npsi,i),npsi,roh,k,i)+
     2                sortM(m,i)
2010			continue
                
c				Now we need to check if the remaining could be lost at other Theta_0's (n)

				topedge = actord1st((n+(order1mst(n,8,npsi,i)
	1											-1)*8),npsi,i)
				if(topedge.lt.64)then
c2009					continue
					do 2008 nm = (topedge+1),64
c						The restwill be sent CW or CCW 
						octetNcwORccw(nm,i) = (octet(nm,i) 
	1								- octet(topedge,i))
2008					continue
					
					                    
					if(npsi.gt.11)then
c                     Clockwise loop
						nsn = cw(n)
					elseif(npsi.lt.12)then
c					Counter Clockwise loop
						nsn = ccw(n)
					endif
					stayloop = 1
					do while (stayloop.gt.0)
						do 2007 m = 8,2,-1
							chopM(m,i) = (octetNcwORccw(actord1st((nsn
	1							+(order1mst(nsn,m,npsi,i)-1)*8),
     2                            npsi,i),i)
     3							-octetNcwORccw(actord1st((nsn
     4							+(order1mst(nsn,m-1,npsi,i)
     5							-1)*8),npsi,i),i))/(mb(m)*1.0)
2007						continue
						chopM(1,i) = octetNcwORccw(actord1st((nsn
	1						+(order1mst(nsn,1,npsi,i)-1)*8),npsi,i),i)
     2                        /8.0

						do 2005 m = 1,8
							sortM(m,i) = 0.0
							mqm = m
							do 2006 mrm = 1,mqm
								sortM(m,i) = sortM(m,i) + chopM(mrm,i)
2006							continue
2005						continue
						do 2004 m = 1,8
							delf(nsn,order1mst(nsn,m,npsi,i),
	1						npsi,roh,k,i) = delf(nsn,order1mst(nsn,
     2						m,npsi,i),npsi,roh,k,i)+sortM(m,i)
2004						continue
						if(nsn.eq.order1nst(8,npsi,i))then
							stayloop = 0
						else                
							topedge = actord1st((nsn
	1						+(order1mst(nsn,8,npsi,i)-1)*8),npsi,i)
							if(topedge.lt.64)then
								slice = octetNcwORccw(topedge,i)
								do 2003 nm = 1,topedge
									octetNcwORccw(nm,i) = 0
2003								continue
								do 2002 nm = (topedge+1),64
									octetNcwORccw(nm,i) = 
	1								(octetNcwORccw(nm,i) 
	2								- slice)
2002								continue
							endif
							if(npsi.gt.11)then
								nsn = cw(nsn)
							elseif(npsi.lt.12)then
								nsn = ccw(nsn)
							endif
						endif
					enddo 
c					End of CW or CCW loop
                  endif  
2001			continue
c			End of n loop			 
2000		continue
c		End of npsi loop
c		End of Allocations Section

s		sumdelf = 0.0
s		do 3003 n = 1,8
s			do 3004 m = 1,8
s				do 3005 npsi = 1,22
s					sumdelf = sumdelf + delf(n,m,npsi,roh,k,i)
s3005				continue
s3004			continue
s3003		continue
s
s		if(sumdelf.gt.0.0)then
s			if(roh.lt.2)then
s				ftpo = GAMI(Aeh(k),minEtta(npsi,roh,i))
sc				This generates a correction factor
s				if(ftpo.lt.0.0.or.ftpo.gt.bigG(k))then
s					multFixix = 1.0
s				else
s					multFix = ((bigG(k)-ftpo)
s	1						/bigG(k))/sumdelf
s				endif
s			else
s				ftpo = GAMI(Aeh(k),minEtta(npsi,roh,i))
s				ftpn = GAMI(Aeh(k),minEtta(npsi,roh-1,i))
s				if(ftpo.lt.0.0.or.ftpo.gt.1.0.or.
s	1				ftpn.lt.0.0.or.ftpn.gt.1.0.or.
s     2				ftpo.gt.ftpn)then
s					multFix = 1.0
s				else
s					multFix = ((ftpn-ftpo)/bigG(k))
s     1						/sumdelf
s				endif
s			endif
s			do 3000 n = 1,8
s				do 3001 m = 1,8
s					do 3002 npsi = 1,22
s						delf(n,m,npsi,roh,k,i) = 
s	1						delf(n,m,npsi,roh,k,i)*multFix
s3002					continue
s3001				continue
s3000			continue
s		else
s			sumdelf = 1.0
s		endif

4000	continue
c	End of k loop (1=ions, 2=momentum, and 3=energy)

c	Quick summation for some terms we need to use back in SolDiv
	fthermnew(i) = 0.0
	mthermnew(i) = 0.0
	ethermnew(i) = 0.0
	do 5000 npsi = 1,22
		do 5001 n = 1,8
			do 5002 m = 1,8
				fthermnew(i) = fthermnew(i) + delf(n,m,npsi,roh,1,i)
				mthermnew(i) = mthermnew(i) + xline(npsi)*
	1									delf(n,m,npsi,roh,2,i)
				ethermnew(i) = ethermnew(i) + delf(n,m,npsi,roh,3,i)
5002		continue
5001	continue
5000  continue
c	END OF New Section using enhanced methods [MTS: 12/15/2014]


c     Thermal loss fractions
	fthermal(i) = fthermnew(i) 
	mthermal(i) = mthermnew(i) 
	ethermal(i) = ethermnew(i)
6000  continue  
      goto 510

511	continue
c     Fast ions
	lost1 = 0.0
	lost2 = 0.0
	lost3 = 0.0
	tally = 0.0
      do 105 npsi = 1,psinum
	   do 106 n = 1,8
		  if(emin(npsi,n,n0,1)<nbi)then
	         lost1 = 1.0+lost1
	      endif
	      if(emin(npsi,n,n0,1)<nbi/2)then
	        lost2 = 1.0+lost2
	      endif
	      if(emin(npsi,n,n0,1)<nbi/3)then
	        lost3 = 1.0+lost3
	      endif
	      tally = 1.0+tally
106      continue
105   continue

      fast1 = lost1/tally
      fast2 = lost2/tally
      fast3 = lost3/tally	
	
c     Thermal loss fractions
	fthermal = 0.0 
	mthermal = 0.0 
	ethermal = 0.0 
	delf = 0.0
	minetta = 0.0    	

510   continue
      return
      end

