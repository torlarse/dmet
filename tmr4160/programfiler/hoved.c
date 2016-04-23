//   --------------------------------------------------------------------
//   DMET DELUX 2000	Program		hoved
//   --------------------------------------------------------------------
//   HENSIKT :
//   Definere vektorer, regne fasevinkel og kalle alle subrutiner som	
//   behøves for å skrive visualiseringsfilen dmet.vtf
//
//   METODE :
//   Inkluderer header-fil og definerer samtlige konstanter og parametere
//   som programmet trenger. Kaller opp subrutinene suksessivt.
//   
//   KALLESEKVENS:
//   Kjører ved å skrive "dmet.sh" i terminal
//
//   PARAMETERE:
//   Navn	I/O	Type			Innhold/Beskrivelse
//   ....................................................................
//   omega	I	flyttals vektor		bølgefrekvens			
//   n		I	heltall			antall frekvensintervaller
//   nl		I/O	heltall			antall lektere
//   k		I	flyttallsvektor		bølgetall
//   A		I	flyttallsvektor		bølgeamplitude
//   pi		I	flyttall		pi
//   g		I/O	flyttall		tyngdelfeltsakselerasjon
//   a		I/O	flyttall		2pi
//   H3		I/O	flyttalls matrise	transferfunksjon hiv
//   H5		I/O	flyttalls matrise	transferfunksjon stamp
//   delta3	I/O	flyttalls matrise	fasevinkel hiv
//   delta5	I/O	flyttalls matrise	fasevinkel stamp
//   L		I/O	heltalls vektor		lengde på lekter
//   B		I/O 	heltalls vektor		bredde på lekter
//   D		I/O	flyttalls vektor	dypgang på lekter
//   H		I/O	flyttalls vektor	dybde i riss for lekter
//   pi		I/O	flyttall		pi
//   rho	I/O	flyttal			tetthet sjøvann
//   g		I/O	flyttall		tyngdeakselerasjonen
//   epsilon	I/O  	flyttal			fasevinkel respons
//
//   INTERNE VARIABLE:
//   Navn			Funksjon	
//   .....................................................................
//   i,j	       		tellevariabler
//   
//   SUBRUTINER:
//   Navn			Funksjonsbeskrivelse
//   .....................................................................
//   lesInn			leser inn resultatfil fra Fortran-program
//   glModell			bygger første del av VTF-fil med geometri
//   havFlate			skriver overflateheving til havteppe
//   respons			skriver respons for lektere til VTF-fil
//   kontroll			sammenstiller resultater i tidssteg
//
//   --------------------------------------------------------------------
//  
//   Programmert av:	     Tor Erik Larsen
//   Dato/Versjon  : 	     27.04.15 / 1.0
//
//   --------------------------------------------------------------------

#include "header.h"

//Definerer hovedfil
int main(){
  int i,j;
  int L[NL];
  int B[NL];
  float D[NL],H[NL];
  double omega[N],k[N],A[N];
  double H3[N][NL],H5[N][NL],delta3[N][NL],delta5[N][NL];
  double epsilon[N],a=2.0*pi;
  //srand(time(NULL));

  //Beregner fasevinkel for å sende til hav.c og respons
  for (j=0;j<N;j++){
    epsilon[j] = ((double)rand()/(double)(RAND_MAX))*a;
  }
  //Kaller opp alle subrutiner
  lesInn(L,B,D,H,omega,k,A,H3,H5,delta3,delta5);
  glModell(L,B,D,H);
  havFlate(A,omega,k,epsilon);
  respons(L,B,D,H,omega,k,A,H3,H5,delta3,delta5,epsilon);
  kontroll();

  return 0;
}
