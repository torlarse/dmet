!   --------------------------------------------------------------------
!   DMET DELUX 2000	Subrutine	transfer             No.:
!   --------------------------------------------------------------------
!   HENSIKT :
!   Samordne alle subrutiner som trengs for å beregne transferfunksjoner 
!   for lekterne
!
!   METODE :
!   Tar inn parametere og konstanter tidligere definert i hovedprogram. 
!   Kaller opp subrutiner suksessivt for å beregne transferfunksjoner 
!   og fasevinkler for begge lektere samtidig. Teorien bak beregningene
!   vises i rapporten. Grunnen til at alle parametere er erklært i denne
!   subrutinen er at alle resultater sendes som vektorer inn og ut.
!   
!   KALLESEKVENS:
!   CALL transfer(a,b,omega,sn,pi,rho,g,n,nl,L,Br,D,H,M,GML,I33,I55, &
!   	 A33,A55,B33,B55,k,H3,H5,delta3,delta5)
!
!   PARAMETERE:
!   Navn	I/O	Type			Innhold/Beskrivelse
!   ....................................................................
!   omega	I	flyttals vektor		bølgefrekvens			
!   n		I	heltall			antall frekvensintervaller
!   nl		I/O	heltall			antall lektere
!   k		I	flyttalls vektor	bølgetall
!   zeta_an	I	flyttalls vektor	bølgeamplitude
!   hs		I	heltall	  		signifikant bølgehøyde
!   tp		I	heltall			midlere periode
!   gamma	I	heltall			toppethetsfaktor
!   pi		I	flyttall		pi
!   g		I/O	flyttall		tyngdelfeltsakselerasjon
!   sn		O	flyttall		spekterverdi
!   a		I/O	heltall			nedre frekvensverdi
!   b		I/O	heltall			øvre frekvensverdi
!   H3		I/O	flyttalls matrise	transferfunksjon hiv
!   H5		I/O	flyttalls matrise	transferfunksjon stamp
!   delta3	I/O	flyttalls matrise	fasevinkel hiv
!   delta5	I/O	flyttalls matrise	fasevinkel stamp
!   A33		I/O	flyttalls matrise	tilleggsmasse hiv
!   A55		I/O 	flyttalls matrise	tilleggsmasse stamp
!   B33		I/O	flyttalls matrise	demping hiv
!   B55		I/O	flyttalls matrise	demping stamp
!   C33		I/O	flyttalls vektor	fjørkoeffisient hiv
!   C55		I/O	flyttalls vektor	fjærkoeffisient stamp
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
!
!   INTERNE VARIABLE:
!   Navn			Funksjon	
!   ...................................................................
!   i,m,n			tellevariabler
!   omegap			mellomregningsvariabel
!   konstant,v1,v2,v3		mellomregningsvariabler
!   deltaomega			mellomregningsvariabel
!   sigma			frekvensparameter
!   omegap			frekvensparameter
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
!   --------------------------------------------------------------------
!  
!   Programmert av:	     Tor Erik Larsen
!   Dato/Versjon  : 	     27.04.15 / 1.0
!
!   --------------------------------------------------------------------

SUBROUTINE transfer(a,b,omega,pi,rho,g,n,nl,L,Br,D,H,M,GML,I33,I55, &
     A33,A55,B33,B55,k,H3,H5,delta3,delta5,zeta_an)
IMPLICIT NONE

!Erklærer variable og parametere
INTEGER::n,nl
REAL::hs,tp,gamma,a,b,pi,g,rho
REAL,DIMENSION(1:n)::L,Br,D,H,M,GML,I33,I55
DOUBLE PRECISION,DIMENSION(1:n)::omega,k,zeta_an
REAL,DIMENSION(1:nl)::C33,C55
REAL,DIMENSION(1:n,1:nl)::A33,A55,B33,B55
DOUBLE PRECISION,DIMENSION(1:n,1:nl)::H3,H5,delta3,delta5

!Kaller opp subrutinene
CALL konstanter(L,Br,D,H,M,GML,I33,I55,rho,g,nl)
CALL akalk(n,nl,rho,g,omega,L,Br,D,A33,A55)
CALL bckalk(n,nl,rho,g,omega,L,Br,D,GML,B33,B55,C33,C55)
CALL h3kalk(k,omega,n,nl,rho,g,A33,B33,C33,L,Br,D,M,H3,zeta_an)
CALL h5kalk(n,nl,L,Br,D,M,A33,B33,A55,B55,C55,I55,k,omega,rho,g,H5)
CALL fasekalk(n,nl,omega,M,A33,B33,C33,A55,B55,C55,I55,delta3,delta5)

RETURN
END SUBROUTINE

