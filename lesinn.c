//   --------------------------------------------------------------------
//   DMET DELUX 2000	Subrutine	lesinn
//   --------------------------------------------------------------------
//   HENSIKT :
//   Lese inn resultater fra Fortran-programmet og bygge vektorer for 
//   bruk videre i programmet. 
//
//   METODE :
//   Åpe tekstfilen fra Fortran-programmet, og benytte fscanf til å lese
//   tekstfil linje for linje. For hver linje legges verdiene inn i
//   forhåndsdefinerte vektorer. I tillegg leses geometrifil for lektere.
//      
//   KALLESEKVENS:
//   lesInn(L,B,D,H,omega,k,A,H3,H5,delta3,delta5);
//
//   PARAMETERE:
//   Navn	I/O	Type			Innhold/Beskrivelse
//   ....................................................................
//   omega	I/O	flyttals vektor		bølgefrekvens			
//   n		I	heltall			antall frekvensintervaller
//   nl		I	heltall			antall lektere
//   k		I/O	flyttallsvektor		bølgetall
//   A		I/O	flyttallsvektor		bølgeamplitude
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
//
//   INTERNE VARIABLE:
//   Navn			Funksjon	
//   .....................................................................
//   i,j			tellevariabler
//
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
//   LESEFILER:
//   Navn			Eventuell beskrivelse
//   .....................................................................
//   konstantinput.txt		Inneholder lektergeometri
//   fortranresultat.txt	Inneholder resultater fra Fortran-prog
//
//   --------------------------------------------------------------------
//  
//   Programmert av:	     Tor Erik Larsen
//   Dato/Versjon  : 	     27.04.15 / 1.0
//
//   --------------------------------------------------------------------

#include "header.h"

//Definerer subrutine som kjøres i denne filen

void lesInn(int L[NL],int B[NL],float D[NL],float H[NL],	\
	    double omega[N],double k[N],double A[N], \
	    double H3[N][NL],double H5[N][NL],		\
	    double delta3[N][NL],double delta5[N][NL]){

  //Definerer filnavn og erklærer telletall
  FILE * filpeker;
  int i,j;

  //Laster inn konstantfil og fyller lektergeometri i vektorene
  filpeker = fopen("konstantinput.txt", "r");
  for(i=0;i<NL;i++){
    fscanf(filpeker,"%d %d %f %f",&L[i],&B[i],&D[i],&H[i]);
  }
  fclose(filpeker);

  //Laster inn resultatfil og fyller vektorer
  filpeker = fopen("fortranresultat.txt", "r");
  for(i=0;i<N;i++){
    fscanf(filpeker,"%lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf",	\
	   &omega[i],&k[i],&A[i],&H3[i][0],&H3[i][1],&H5[i][0],&H5[i][1], \
	   &delta3[i][0],&delta3[i][1],&delta5[i][0],&delta5[i][1]);
  }
  fclose(filpeker);

  //Gir tilbake kontroll og avslutter subrutine
  return;  
}



