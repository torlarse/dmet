!   --------------------------------------------------------------------
!   DMET DELUX 2000	Subrutine	statistikk
!   --------------------------------------------------------------------
!   HENSIKT :
!   Beregner bølgestatistikk fra oppgitt JONSWAP-spekter
!
!   METODE :
!   Tar inn frekvensvektor,spekterverdier og spekterparametere fra 
!   tidligere subrutiner. Beregner spektermomenter, midlere periode
!   og maksimal bølgehøyde for en femtimers periode.
!   
!   KALLESEKVENS:
!   CALL statistikk(a,b,n,omega,Sn,Hs,pi)
!
!   PARAMETERE:
!   Navn	I/O	Type			Innhold/Beskrivelse
!   ....................................................................
!   omega	I	flyttals vektor		bølgefrekvens			
!   n		I	heltall			antall frekvensintervaller
!   k		I	flyttallsvektor		bølgetall
!   zeta_an	I	flyttallsvektor		bølgeamplitude
!   hs		I	heltall	  		signifikant bølgehøyde
!   tp		I	heltall			midlere periode
!   gamma	I	heltall			toppethetsfaktor
!   pi		I	flyttall		pi
!   g		I	flyttall		tyngdelfeltsakselerasjon
!   sn		I	flyttallsvektor		spekterverdi
!   a		I	heltall			nedre frekvensverdi
!   b		I	heltall			øvre frekvensverdi
!   pi		I	flyttall		pi
!   rho		I	flyttal			tetthet sjøvann
!   g		I	flyttall		tyngdeakselerasjonen
!   Tm02	O	flyttall		nullkrysningsperiode
!   Tm0e	O	flyttall		middelperiode
!   Tm24	O	flyttall		middelperiode mellom topper
!    Hmax	O	flyttal			største bølgehøyde fem timer
!
!   INTERNE VARIABLE:
!   Navn			Funksjon	
!   .....................................................................
!   i,n				tellevariabler
!   deltaomega			frekvensintervall
!   m0,m1,m2,m4			spektermomenter
!   mndelsum			mellomregningsverdier	   
!   
!   SUBRUTINER:
!   Navn			Funksjonsbeskrivelse
!   .....................................................................
!   konstanter			beregner metasenter og andre arealmomenter
!   akalk			beregner tilleggmasse i hiv
!   bckalk			beregner demping og fjærkoeffisienter
!   h3kalk			beregner transferfunksjoner hiv
!   h5kalk			beregner transferfunksjoner stamp
!   fasekalk			beregner fasevinkel for bevegelser
!
!   LESEFILER:
!   Navn			Eventuell beskrivelse
!   .....................................................................
!   konstantinput.txt		inneholder lektergeometri
!
!   
!   SKRIVEFILER:
!   Navn			Eventuell beskrivelse
!   .....................................................................
!   statistikk.txt		resultater for bølgestatistikk
!
!   --------------------------------------------------------------------
!  
!   Programmert av:	     Tor Erik Larsen
!   Dato/Versjon  : 	     27.04.15 / 1.0
!
!   --------------------------------------------------------------------

SUBROUTINE statistikk(a,b,n,omega,Sn,Hs,pi)
IMPLICIT NONE
INTEGER i,n
REAL::m0delsum, m1delsum,m2delsum,m4delsum
REAL::m0,m1,m2,m4
REAL::Tz,Tm02,Tmo2,Tm24,Hmax,NN,Tmoe
REAL::pi,Hs,a,b,deltaomega
DOUBLE PRECISION,DIMENSION(1:n)::omega,sn

!Beregner deltaomega
deltaomega = (b-a)/n

!Beregner spektermomentet m0
m0delsum = 0
DO i = 1,n
   m0delsum = m0delsum +  sn(i)
END DO
m0 = deltaomega * m0delsum

!Beregner spektermomentet m1
m1delsum = 0
DO i = 1,n
   m1delsum = m1delsum + sn(i)*omega(i)
END DO
m1 = deltaomega*m1delsum

!Beregner m2 og tm02
m2delsum = 0
DO i=1,n
   m2delsum = m2delsum + omega(i)**2 * sn(i)
END DO
m2 = deltaomega*m2delsum

!Beregner spektermomentet m4
m4delsum = 0
DO i=1,n
   m4delsum = m4delsum + omega(i)**4 * sn(i)
END DO
m4 = deltaomega*m4delsum

!Beregner nullkrysningsperiode Tz
Tz = pi*Hs / (2.0*SQRT(m2))
Tm02= 2.0*pi*SQRT(m0/m2)
Tmoe = 2.0*pi*(m0/m1)
Tm24 = 2.0*pi*SQRT(m2/m4)

!Beregner antall bølger N for en varighet på 5 timer eller 18000 sekunder
NN = 18000.0 / Tz
Hmax = Hs*SQRT(LOG(NN)/2.0)

!Skriver resultater til fil
OPEN(252,FILE="statistikk.txt",ACTION="WRITE",STATUS="UNKNOWN")
WRITE(*,*) ''
WRITE(252,*) '---------------------------------------------------------'
WRITE(252,"(A,F10.1,A)") 'Nullkrysningsperioden Tm0z er: ', Tm02, '[s]'
WRITE(252,"(A,F10.1,A)") 'Midlere periode Tm0e er:       ', Tmoe, '[s]'
WRITE(252,"(A,F10.1,A)") 'Midlere periode mellom bølgetoppene er: ',Tm24, '[s]'
WRITE(252,"(A,F10.1,A)") 'Høyeste bølge i en 5-timers periode er: ',Hmax, '[m]'
WRITE(252,*) '--------------------------------------------------------'
WRITE(*,*) ''
CLOSE(252)

RETURN
END SUBROUTINE
