!   --------------------------------------------------------------------
!   DMET DELUX 2000	Subrutine	h3kalk
!   --------------------------------------------------------------------
!   HENSIKT :
!   Beregner transferfunksjoner i hiv for begge lektere
!
!   METODE :
!   Tar inn frekvensvektor, konstantverdier fra lektergeometri og 
!   alle beregnede koeffisienter for hiv og stamp. Benytter 
!   matematiske modeller for svingesystemer og kalkulererer 
!   det dimensjonsløse utslaget for hivbevegelsen til begge lektere. 
!   Teorien er presentert i rapporten. På grunn av komplekse formler
!   er uttrykkene faktorisert i stor grad. 
!   
!   KALLESEKVENS:
!   CALL h3kalk(k,omega,n,nl,rho,g,A33,B33,C33,L,Br,D,M,H3)
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
!   i				tellevariabler
!   F3A,F3A1,F3A2		faktorer i uttrykk
!   eta3nevner			faktor i uttrykk
!   eta3A			utslag for lekter i meter	
!
!   
!   SKRIVEFILER:
!   Navn			Eventuell beskrivelse
!   .....................................................................
!   H3plot.txt			skriver resultat for visuell kontroll
!   
!
!   --------------------------------------------------------------------
!  
!   Programmert av:	     Tor Erik Larsen
!   Dato/Versjon  : 	     27.04.15 / 1.0
!
!   --------------------------------------------------------------------

SUBROUTINE h3kalk(k,omega,n,nl,rho,g,A33,B33,C33,L,Br,D,M,H3,zeta_an)
IMPLICIT NONE
!Erklærer variable og vektorer
INTEGER::n,nl,i,j
DOUBLE PRECISION,DIMENSION(1:n)::omega,k,zeta_an
REAL,DIMENSION(1:n,1:nl)::A33,B33
DOUBLE PRECISION,DIMENSION(1:n,1:nl)::eta3nevner,F3A1,F3A2,F3A
DOUBLE PRECISION,DIMENSION(1:n,1:nl)::eta3A,H3
REAL,DIMENSION(1:nl)::L,Br,D,M,C33
REAL rho,g,a,b

!Beregner integralene til kreftene i hiv, skriver kontroll til skjerm
DO i=1,n
   DO j=1,nl
      F3A1(i,j) = (2.0/k(i)) * SIN((k(i) * L(j)) / 2.0 )

      F3A2(i,j) = rho*g*Br(j)* EXP(- k(i)*D(j) )&
           - omega(i)**2 * ( A33(i,j)/L(j) ) * &
           EXP(- k(i)*D(j) / 2.0)

      eta3nevner(i,j)= SQRT((C33(j) - (M(j) + A33(i,j))*omega(i)**2 )**2 &
           + (B33(i,j) * omega(i))**2 )

      F3A (i,j) = F3A1(i,j) * F3A2(i,j)
   END DO
END DO

!Beregner eta 3
DO i=1,n
   DO j=1,nl
      eta3A(i,j)=   F3A(i,j) / eta3nevner(i,j)
   END DO
END DO

!Beregner transfer-funksjoner
DO i=1,n
   DO j=1,nl
      H3(i,j)=ABS(eta3A(i,j))
   END DO
END DO

OPEN(49,FILE="H3plot.txt",ACTION="WRITE",STATUS="UNKNOWN")
DO i=1,n
   WRITE(49,*) omega(i),H3(i,1),H3(i,2)
END DO
CLOSE(49)

RETURN
END SUBROUTINE
