!   --------------------------------------------------------------------
!   DMET DELUX 2000	Subrutine	bckalk
!   --------------------------------------------------------------------
!   HENSIKT :
!   Beregner dempingskoeffisienter og fjærkoeffisienter
!
!   METODE :
!   Tar inn frekvensvektor og konstanter fra lektergeometri. Beregner
!   og sender videre dempings- og fjærkoeffisienter for hiv og stamp.
!   
!   KALLESEKVENS:
!   CALL bckalk(n,nl,rho,g,omega,L,Br,D,GML,B33,B55,C33,C55)
!
!   PARAMETERE:
!   Navn	I/O	Type			Innhold/Beskrivelse
!   ....................................................................
!   omega	I	flyttals vektor		bølgefrekvens			
!   n		I	heltall			antall frekvensintervaller
!   nl		I/O	heltall			antall lektere
!   k		I	flyttallsvektor		bølgetall
!   zeta_an	I	flyttallsvektor		bølgeamplitude
!   hs		I	heltall	  		signifikant bølgehøyde
!   tp		I	heltall			midlere periode
!   gamma	I	heltall			toppethetsfaktor
!   pi		I	flyttall		pi
!   g		I/O	flyttall		tyngdelfeltsakselerasjon
!   sn		O	flyttallsvektor		spekterverdi
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
!   Br		I/O 	heltalls vektor		bredde på lekter
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
!   .....................................................................
!   i,j				tellevariabler
!   oomegaA,oomegaB		omregning av frekvens
!   B33plot			plotteverdi avlest tilleggsmasse
!   
!   SKRIVEFILER:
!   Navn			Eventuell beskrivelse
!   .....................................................................
!   B33plot.txt			skriver resultat for visuell kontroll
!   
!
!   --------------------------------------------------------------------
!  
!   Programmert av:	     Tor Erik Larsen
!   Dato/Versjon  : 	     27.04.15 / 1.0
!
!   --------------------------------------------------------------------

!Starter subrutine og erklærer alle variable og vektorer
SUBROUTINE bckalk(n,nl,rho,g,omega,L,Br,D,GML,B33,B55,C33,C55)
IMPLICIT NONE
INTEGER::i,j,n,nl
REAL::rho,g
REAL,DIMENSION(1:nl)::L,Br,D,GML,C33,C55
REAL,DIMENSION(1:n,1:nl)::B33,B55,B33plot
DOUBLE PRECISION,DIMENSION(1:n)::omega,oomegaA,oomegaB

!Beregner x-aksen for koeffisientene
DO i=1,n
   oomegaA(i) = omega(i)*SQRT(Br(1)/(2.0*g))
   oomegaB(i) = omega(i)*SQRT(Br(2)/(2.0*g))
END DO

!Beregner B33_2D plotteverdi for lekter A, sjetteegradspolynom
DO i=1,n
   IF (oomegaA(i) < 1.47) THEN
      B33plot(i,1) = (  &
           0.985*oomegaA(i)**6 &
           - 5.368*oomegaA(i)**5 + 10.474*oomegaA(i)**4 &
           - 7.778*oomegaA(i)**3 + 0.077*oomegaA(i)**2 &
           + 1.727*oomegaA(i)**1 + 0.004 &
           )
   ELSE
      B33plot(i,1) = 0.001
END IF
END DO

!Beregner B33_2D plotteverdi for lekter B, femtegradspolynom
DO i=1,n
   IF (oomegaB(i) < 2.03) THEN
   B33plot(i,2) = ( &
        - 0.220*oomegaB(i)**5 &
        + 0.547*oomegaB(i)**4 + 1.431*oomegaB(i)**3 &
        - 5.396*oomegaB(i)**2 + 4.212*oomegaB(i)**1 &
        + 0.017 &
        )
ELSE
   B33plot(i,2) = 0.001
END IF
END DO 

!Beregner fullengde B33, ved å gange A33_2D med lengde L
DO i=1,n
   B33(i,1) = L(1) * B33plot(i,1) * (rho*Br(1)*D(1) * &
        SQRT((2.0*g) / Br(1)) )
   B33(i,2) = L(2) * B33plot(i,2) * (rho*Br(2)*D(2) * &
        SQRT( (2.0*g) / Br(2) ) )
END DO

!Beregner koeffisientene i stamp, A55 og B55, UT FRA A33_2D
DO i=1,n
   B55(i,1) = (L(1)**2 / 12) * B33(i,1)
   B55(i,2) = (L(2)**2 / 12) * B33(i,2)
END DO

!Beregner fjærkoeffisientene C33 og C55
DO j=1,nl
   C33(j) = rho*g*L(j)*Br(j)
   C55(j) = rho*g*L(j)*Br(j)*D(j)*GML(j)
END DO

!Bygger plottefil for å sjekke at koeffisientene er regnet riktig
OPEN(44,FILE="B33plot.txt",ACTION="WRITE",STATUS="UNKNOWN")
DO i=1,n
   WRITE(44,*) oomegaA(i), B33plot(i,1), oomegaB(i), B33plot(i,2)
END DO
CLOSE(44)

!Gir tilbake kontrollen til hovedprogram og avslutter subrutine
RETURN
END SUBROUTINE
