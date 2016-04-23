!   --------------------------------------------------------------------
!   DMET DELUX 2000	Subrutine	utskrift             No.:
!   --------------------------------------------------------------------
!   Hensikt :
!   Skrive resultatfil som C-programmet skal lese inn
!
!   Metode :
!   Tar inn alle parametere som skal sendes videre, åpner en tekstfil og 
!   skriver resultatet som en matrise i tekstfilen.
!   
!   Kallesekvens fra hovedprogram:
!   CALL utskrift(n,nl,omega,zeta_an,k,H3,H5,delta3,delta5)
!
!   Parametere:
!   Navn	I/O	Type			Innhold/Beskrivelse
!   .......................................................................
!   omega	I/O	flyttal			bølgefrekvens			
!   n		I/O	heltall			antall frekvensintervaller
!   nl		I/O	heltall			antall lektere
!   k		I/O	flyttall		bølgetall
!   zeta_an	I/O	flyttalls matrise	bølgeamplitude
!   H3		I/O	flyttalls matrise	transferfunksjon hiv
!   H5		I/O	flyttalls matrise	transferfunksjon stamp
!   delta3	I/O	flyttalls matrise	fasevinkel hiv
!   delta5	I/O	flyttalls matrise	fasevinkel stamp
!
!   
!   Interne variable:
!   Navn		Funksjon	
!   .....................................................................
!   i,n			Tellevariabler
!   
!   SKRIVEFILER:
!   Navn		
!   .....................................................................
!   fortranresultat.txt 
!   
!   --------------------------------------------------------------------
!  
!   Programmert av:	     Tor Erik Larsen
!   Dato/Versjon  : 	     27.04.15 / 1.0
!
!   --------------------------------------------------------------------

SUBROUTINE utskrift(n,nl,omega,zeta_an,k,H3,H5,delta3,delta5)
IMPLICIT NONE

!Erklærer variable brukt i subrutine
INTEGER::i,n,nl
DOUBLE PRECISION,DIMENSION(1:n)::omega,zeta_an,k
DOUBLE PRECISION,DIMENSION(1:n,1:nl)::H3,H5,delta3,delta5

!Bygger tekstfil som skal leses av C-programmet for animasjon
OPEN(254,FILE="fortranresultat.txt",ACTION="WRITE",STATUS="UNKNOWN")
DO i = 1,n
   WRITE(254,*) omega(i),k(i),zeta_an(i),H3(i,1),H3(i,2),H5(i,1),H5(i,2), &
        delta3(i,1),delta3(i,2),delta5(i,1),delta5(i,2)
END DO
CLOSE(254)

RETURN
END SUBROUTINE utskrift