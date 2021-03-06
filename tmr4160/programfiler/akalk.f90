!   --------------------------------------------------------------------
!   DMET DELUX 2000	Subrutine	akalk
!   --------------------------------------------------------------------
!   HENSIKT :
!   Beregner hydrodynamiske tilleggskoeffisienter for hiv
!
!   METODE :
!   Tar inn konstanter fra lektergeometri og frekvensvektor. Regner om
!   frekvens tilpasset avleste kurver i lærebok. Bruker innlagte polynomer
!   til å beregne tilleggsmasse for todimensjonale striper av lekter. 
!   Sender resultatmatriser for begge lektere videre til beregning av 
!   transferfunksjoner.Skriver resultatfil som kan plottes ved kjøring 
!   for visuell kontroll. 
!   
!   KALLESEKVENS:
!   CALL akalk(n,nl,rho,g,omega,L,Br,D,A33,A55)
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
!   A33		I/O	flyttalls matrise	tilleggsmasse hiv
!   A55		I/O 	flyttalls matrise	tilleggsmasse stamp
!   B33		I/O	flyttalls matrise	demping hiv
!   B55		I/O	flyttalls matrise	demping stamp
!   C33		I/O	flyttalls vektor	fjørkoeffisient hiv
!   C55		I/O	flyttalls vektor	fjærkoeffisient stamp
!   L		I/O	heltalls vektor		lengde på lekter
!   Br		I/O 	heltalls vektor		bredde på lekter
!   D		I/O	flyttalls vektor	dypgang på lekter
!   H		I/O	flyttalls vektor	dybde i riss for lekter
!   M		I/O	flyttalls vektor	masse lekter
!   GML		I/O	flyttalls vektor	langskips metasenter
!   pi		I/O	flyttall		pi
!   rho		I/O	flyttal			tetthet sjøvann
!   g		I/O	flyttall		tyngdeakselerasjonen
!   	
!
!   INTERNE VARIABLE:
!   Navn			Funksjon	
!   .....................................................................
!   i				tellevariabler
!   oomegaA,oomegaB		omregning av frekvens
!   A33plot			plotteverdi avlest tilleggsmasse
!
!   SKRIVEFILER:
!   Navn			Eventuell beskrivelse
!   .....................................................................
!   A33plot.txt			skriver resultat for visuell kontroll
!   
!
!   --------------------------------------------------------------------
!  
!   Programmert av:	     Tor Erik Larsen
!   Dato/Versjon  : 	     27.04.15 / 1.0
!
!   --------------------------------------------------------------------

!Starter subrutine og erklærer alle variable og vektorer
SUBROUTINE akalk(n,nl,rho,g,omega,L,Br,D,A33,A55)
IMPLICIT NONE
INTEGER::i,n,nl
REAL::rho,g
DOUBLE PRECISION,DIMENSION(1:n)::omega,oomegaA,oomegaB
REAL,DIMENSION(1:nl)::L,Br,D,M,GML
REAL,DIMENSION(1:n,1:nl)::A33,A55,A33plot

!Beregner x-aksen for koeffisientene
DO i=1,n
   oomegaA(i)=omega(i)*SQRT(Br(1)/(2.0*g))
   oomegaB(i)=omega(i)*SQRT(Br(2)/(2.0*g))
END DO

!Beregner A33_2D for lekter A, sjettegradspolynom
DO i = 1,n
   IF (oomegaA(i) < 2.0) THEN
   A33plot(i,1) = ( &
        -1.8169*oomegaA(i)**5 + 11.493*oomegaA(i)**4 - 28.060*oomegaA(i)**3 &
        + 32.823*oomegaA(i)**2 - 17.894*oomegaA(i)**1 + 4.3851 &
        )
ELSE
   A33plot(i,1) = 1.15
END IF
END DO

!Beregner A33_2D for lekter B, femtegradspolynom
DO  i = 1,n
   IF (oomegaB(i) <2.0) THEN
   A33plot(i,2) = (&
        1.0534*oomegaB(i)**6 - 8.207*oomegaB(i)**5 &
        + 26.398*oomegaB(i)**4 - 45.717*oomegaB(i)**3 &
        + 46.022*oomegaB(i)**2 -25.507*oomegaB(i)**1  &
        + 7.4189 &
        )
ELSE
   A33plot(i,2) = 1.9
END IF
END DO

!Beregner fullengde A33 ved å gange A33_2D med lengde
DO i=1,n
   A33(i,1) = L(1) * A33plot(i,1)*(rho*Br(1)*D(1))
   A33(i,2) = L(2) * A33plot(i,2)*(rho*Br(2)*D(2))
END DO

!Beregner A55 for begge lektere
DO i=1,n
   A55(i,1) = (L(1)**2 / 12) * A33(i,1)
   A55(i,2) = (L(2)**2 / 12) * A33(i,2)
END DO

!Bygger plottefil for å sjekke om koeffisientene er regnet riktig
OPEN(13,FILE="A33plot.txt",ACTION="WRITE",STATUS="UNKNOWN")
DO i=1,n
   WRITE(13,*), oomegaA(i),A33plot(i,1),oomegaB(i), A33plot(i,2)
END DO
CLOSE(13)

!Gir tilbake kontrollen til hovedprogram og avslutter subrutine
RETURN
END SUBROUTINE
