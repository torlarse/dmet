!   --------------------------------------------------------------------
!   DMET DELUX 2000	Subrutine	konstanter
!   --------------------------------------------------------------------
!   HENSIKT :
!   Beregner konstantverdier for begge lektere, slik som andre
!   arealmoment og metasenter
!
!   METODE :
!   Leser lektergeometri fra inputfil, og beregner andre arealmoment,
!   og begge metasentre for begge lektere. Sender verdiene tilbake til
!   transfer-subrutine
!   
!   KALLESEKVENS:
!   CALL konstanter(L,Br,D,H,M,GML,I33,I55,rho,g,nl)
!
!   PARAMETERE:
!   Navn	I/O	Type			Innhold/Beskrivelse
!   ....................................................................
!   n		I	heltall			antall frekvensintervaller
!   nl		I/O	heltall			antall lektere
!   g		I/O	flyttall		tyngdelfeltsakselerasjon
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
!   g		I/O	flyttall		tyngdeakselerasjonen!   	
!
!   INTERNE VARIABLE:
!   Navn			Funksjon	
!   .....................................................................
!   i,j				tellevariabler
!   KG			    	tyngdepunkt lekter
!   KB				volumsenter lekter
!   V				volumdeplasement
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
!   --------------------------------------------------------------------
!  
!   Programmert av:	     Tor Erik Larsen
!   Dato/Versjon  : 	     27.04.15 / 1.0
!
!   --------------------------------------------------------------------

!Starter subrutiner og erklærer alle variabler og konstanter
SUBROUTINE konstanter(L,Br,D,H,M,GML,I33,I55,rho,g,nl)
IMPLICIT NONE

!Erklærer variable og parametere, skriver kontroll til skjerm
INTEGER i,j,nl
REAL,DIMENSION(1:nl)::L,Br,D,H,GML,I33,I55,M,V
REAL,DIMENSION(1:nl)::KG,KB,BML
REAL::rho,g

!Leser inn lektergeometri fra input-fil, skriver ut kontroll til skjerm
OPEN(20,FILE="konstantinput.txt",ACTION="READ",STATUS="UNKNOWN")
DO j = 1,nl
   READ(20,*) L(j),Br(j),D(j),H(j)
END DO
CLOSE(20)

!Beregner andre arealmoment for begge lektere, skriver ut kontroll
!Setter lekterfribord til dobbel dypgang
DO j=1,nl
   V(j)   = L(j)*Br(j)*D(j)
   M(j)   = rho*V(j)
   I33(j) = L(j) * Br(j)**3 / 12.0
   I55(j) = M(j) * ( (L(j)**2 / 12.0) + H(j)**2)
   KG(j)  = D(j)
   KB(j)  = D(j) / 2.0
   BML(j) = L(j)**2 / (12.0*D(j))
   GML(j) = KB(j) + BML(j) - KG(j)
END DO

!Gir tilbake kontroll og avslutter subrutine
RETURN
END SUBROUTINE
