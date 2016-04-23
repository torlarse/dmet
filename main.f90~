!   --------------------------------------------------------------------
!   DMET DELUX 2000	Program		main	               No.:
!   --------------------------------------------------------------------
!   Hensikt :
!   Beregne bølgespekter, bølgestatistikk og transfer-funksjoner
!
!   Metode :
!   Definerer parametere som programmet trenger, og kaller på subrutiner suksessivt.
!   
!   Kallesekvens:
!   "dmet.sh" i terminal/ shell/ kommandolinje
!
!   Parametre:
!   Navn	I/O	Type			Innhold/Beskrivelse
!   .......................................................................
!   omega	I/O	double			bølgefrekvens			
!   a		I/O	flyttall		nederste frekvensverdi
!   b		I/O	flyttall		øverste frekvensverdi
!   n		I/O	heltall			antall frekvensintervaller
!   nl		I/O	heltall			antall lektere
!   Hs		I/O	heltall			signifikant bølgehøyde
!   k		I/O	flyttall		bølgetall
!   zeta_an	I/O	flyttalls matrise	bølgeamplitude
!   sn		I/O	flyttalls matrise	spekterverdi
!   H3		I/O	flyttalls matrise	transferfunksjon hiv
!   H5		I/O	flyttalls matrise	transferfunksjon stamp
!   delta3	I/O	flyttalls matrise	fasevinkel hiv
!   delta5	I/O	flyttalls matrise	fasevinkel stamp
!   A33		I/O	flyttalls matrise	tilleggsmasse hiv
!   A55		I/O 	flyttalls matrise	tilleggsmasse stamp
!   B33		I/O	flyttalls matrise	demping hiv
!   B55		I/O	flyttalls matrise	demping stamp
!   L		I/O	heltalls vektor		lengde på lekter
!   B		I/O 	heltalls vektor		bredde på lekter
!   D		I/O	flyttalls vektor	dypgang på lekter
!   H		I/O	flyttalls vektor	dybde i riss for lekter
!   M		I/O	flyttalls vektor	masse lekter
!   GML		I/O	flyttalls vektor	langskips metasenter
!   I33		I/O	flyttall vektor		andre arealmoment tverrskips
!   I55		I/O	flyttalls vektor	andre arealmoment langskips
!   pi		I/O	flyttall		pi
!   rho		I/O	flyttal			tetthet sjøvann
!   g		I/O	flyttall		tyngdeakselerasjonen
!     
!     INTERNE VARIABLE:
!     Ingen interne variable i hovedprogrammet
!
!     SUBRUTINER:
!     spekter		Beregner et JONSWAP-spekter
!     statistikk	Beregner bølgestatistikk som påkrevd
!     transfer		Beregner transferfunksjoner i hiv og stamp
!     utskrift		Samle resultater og skrive til fil
!     
!   Programmert av:	     Tor Erik Larsen
!   Dato/Versjon  : 	     27.04.15 / 1.0
!
! ----------------------------------------------------------------------

PROGRAM main
IMPLICIT NONE

!Erklærer variable og parametere brukt i programmet
INTEGER,PARAMETER::n=1000, nl=2
REAL::a,b,Hs
REAL,PARAMETER::pi=3.1415,g=9.81,rho=1025
REAL,DIMENSION(1:nl)::L,Br,D,H,M,GML,I33,I55
REAL,DIMENSION(1:n,1:nl)::A33,A55,B33,B55
DOUBLE PRECISION,DIMENSION(1:n)::omega,sn,zeta_an,k
DOUBLE PRECISION,DIMENSION(1:n,1:nl)::H3,H5,delta3,delta5

!Kaller opp alle subrutiner
CALL spekter(a,b,n,pi,omega,sn,zeta_an,k,g,Hs)
CALL statistikk(a,b,n,omega,Sn,Hs,pi)
CALL transfer(a,b,omega,pi,rho,g,n,nl,L,Br,D,H,M,GML,I33,I55, &
     A33,A55,B33,B55,k,H3,H5,delta3,delta5,zeta_an)
CALL utskrift(n,nl,omega,zeta_an,k,H3,H5,delta3,delta5)

END PROGRAM
