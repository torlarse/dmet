!   --------------------------------------------------------------------
!   DMET DELUX 2000	Subrutine	h5kalk
!   --------------------------------------------------------------------
!   HENSIKT :
!   Beregner transferfunksjoner i stamp for begge lektere
!
!   METODE :
!   Tar inn frekvensvektor, konstantverdier fra lektergeometri og 
!   alle beregnede koeffisienter for stamp. Benytter 
!   matematiske modeller for svingesystemer og kalkulererer 
!   det dimensjonsløse utslaget for stampebevegelsen til begge lektere. 
!   Teorien er presentert i rapporten. På grunn av komplekse formler
!   er uttrykkene faktorisert i stor grad. 
!   
!   KALLESEKVENS:
!   CALL h5kalk(n,nl,L,Br,D,M,A33,B33,A55,B55,C55,I55,k,omega,rho,g,H5)
!
!   PARAMETERE:
!   Navn	I/O	Type			Innhold/Beskrivelse
!   ....................................................................
!   omega	I	flyttals vektor		bølgefrekvens			
!   n		I	heltall			antall frekvensintervaller
!   nl		I/O	heltall			antall lektere
!   k		I	flyttallsvektor		bølgetall
!   pi		I	flyttall		pi
!   g		I/O	flyttall		tyngdelfeltsakselerasjon
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
!   I55		I/O	flyttalls vektor	andre arealmoment langskips
!   pi		I/O	flyttall		pi
!   rho		I/O	flyttall		tetthet sjøvann
!   g		I/O	flyttall		tyngdeakselerasjonen
!   	
!
!   INTERNE VARIABLE:
!   Navn			Funksjon	
!   .....................................................................
!   i,j				tellevariabler
!   F5A,F5A1,F5A2		faktorer i uttrykk
!   eta5nevner			faktor i uttrykk
!   eta5A			utslag for lekter i meter	
!   
!
!   SKRIVEFILER:
!   Navn			Eventuell beskrivelse
!   .....................................................................
!   H5plot22.txt		skriver resultat for visuell kontroll
!   
!
!   --------------------------------------------------------------------
!  
!   Programmert av:	     Tor Erik Larsen
!   Dato/Versjon  : 	     27.04.15 / 1.0
!
!   --------------------------------------------------------------------

SUBROUTINE h5kalk(n,nl,L,Br,D,M,A33,B33,A55,B55 &
     ,C55,I55,k,omega,rho,g,H5)
  IMPLICIT NONE
  !Erklærer variable
  INTEGER::n,nl,i,j
  REAL rho,g
  REAL,DIMENSION(1:nl)::L,Br,D,M,I55,C55
  REAL,DIMENSION(1:n,1:nl)::A33,B33,A55,B55
  DOUBLE PRECISION,DIMENSION(1:n)::k,omega
  DOUBLE PRECISION,DIMENSION(1:n,1:nl)::F5A1,F5A2,eta5nevner,F5A
  DOUBLE PRECISION,DIMENSION(1:n,1:nl)::eta5A,H5

  !Beregner eksitasjonskraft F5A
  DO i=1,n
     DO j=1,nl
        F5A1(i,j) = (rho*g*Br(j)*EXP(-k(i)*D(j)) &
             - omega(i)**2 * (A33(i,j)/L(j))* EXP(-k(i)*D(j)/2.0 ) )

        F5A2(i,j) =  L(j)*COS(k(i)*L(j) / 2.0) &
             - (2.0 / k(i) )*SIN( k(i)*L(j) / 2.0 )

        eta5nevner(i,j) = (k(i)**2) &
             * SQRT(  (C55(j) - (I55(j) + A55(i,j))*omega(i)**2 )**2 &
             + (B55(i,j)*omega(i))**2   )
     END DO
  END DO

  !Multipliserer faktorene i nevner
  DO i=1,n
     DO j=1,nl
        F5A (i,j) = F5A1(i,j) * F5A2(i,j)
     END DO
  END DO

  !Beregner eta5A
  DO i=1,n
     DO j=1,nl
        eta5A(i,j) = F5A(i,j) / eta5nevner(i,j)
     END DO
  END DO

  !Beregner transferfunksjonen H5
  DO i=1,n
     DO j=1,nl
        H5(i,j) = ABS(eta5A(i,j))
     END DO
  END DO

  !Skriver H5 til plottefil for visuell sjekk
  OPEN(488,FILE="H5plot22.txt",ACTION="WRITE",STATUS="UNKNOWN")
  DO i=1,n
     WRITE(488,*) omega(i), H5(i,1), H5(i,2)
  END DO
  CLOSE(488)

  RETURN
END SUBROUTINE h5kalk
