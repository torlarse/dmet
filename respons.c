//   --------------------------------------------------------------------
//   DMET DELUX 2000	Subrutine	respons
//   --------------------------------------------------------------------
//   HENSIKT :
//   Skrive utslag for lekternoder som følge av bølger til VTF-fil
//
//   METODE :
//   Ta inn transferfunksjoner fra Fortran-program, beregne respons fra
//   responsligning, beregne utslag i hver enkelt lekternode og skrive
//   fysisk utslag til VTF-fil
//
//   KALLESEKVENS:
//   respons(L,B,D,H,omega,k,A,H3,H5,delta3,delta5,epsilon);
//
//   PARAMETERE:
//   Navn	I/O	Type			Innhold/Beskrivelse
//   ....................................................................
//   omega	I	flyttals vektor		bølgefrekvens			
//   n		I	heltall			antall frekvensintervaller
//   nl		I	heltall			antall lektere
//   k		I	flyttallsvektor		bølgetall
//   A		I	flyttallsvektor		bølgeamplitude
//   pi		I	flyttall		pi
//   g		I	flyttall		tyngdelfeltsakselerasjon
//   a		I	flyttall		2pi
//   H3		I	flyttalls matrise	transferfunksjon hiv
//   H5		I	flyttalls matrise	transferfunksjon stamp
//   delta3	I	flyttalls matrise	fasevinkel hiv
//   delta5	I	flyttalls matrise	fasevinkel stamp
//   L		I	heltalls vektor		lengde på lekter
//   B		I 	heltalls vektor		bredde på lekter
//   D		I	flyttalls vektor	dypgang på lekter
//   H		I	flyttalls vektor	dybde i riss for lekter
//   pi		I	flyttall		pi
//   rho	I	flyttal			tetthet sjøvann
//   g		I	flyttall		tyngdeakselerasjonen
//   epsilon	I  	flyttal			fasevinkel respons
//   fps	I	heltall			steg per sekund
//   sx1	O	flyttall		utslag i x-retning nodepar 1
//   sx2	O	flyttall		utslag i x-retning nodepar 2
//   sx5	O	flyttall		utslag i x-retning nodepar 5
//   sx6	O	flyttall		utslag i x-retning nodepar 6
//   sz1	O	flyttall		utslag i z-retning nodepar 1
//   sz2	O	flyttall		utslag i z-retning nodepar 2
//   sz5	O	flyttall		utslag i z-retning nodepar 5
//   sz6	O	flyttall		utslag i z-retning nodepar 6
//
//   INTERNE VARIABLE:
//   Navn			Funksjon	
//   .....................................................................
//   i,j			tellevariabler
//   lekter,teller		tellevariabler
//   xkoordinat			koordinatvariabel
//   Zbunn,Zdekk		koordinater z-akse
//   xBaug,xDekk		xkoordinater for noder
//   s1,s2,5,s6			utslagsvariabler
//   xnull,znull		nullpunkter lektere
//   t				tid
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
//   SKRIVEFILER:
//   Navn			Eventuell beskrivelse
//   .....................................................................
//
//
//   --------------------------------------------------------------------
//  
//   Programmert av:	     Tor Erik Larsen
//   Dato/Versjon  : 	     27.04.15 / 1.0
//
//   --------------------------------------------------------------------

#include "header.h"

void respons(int L[NL],int B[NL],float D[NL],float H[NL],	\
	     double omega[N],double k[N],double A[N],		\
	    double H3[N][NL],double H5[N][NL],		\
	     double delta3[N][NL],double delta5[N][NL],\
	     double epsilon[N]){

  //Definerer filnavn og erklærer telletall
  FILE * filpeker;
  FILE * filpeker2;
  int i,j,m,n,lekter,teller;
  double xkoordinat,t;
  double zBunn,zDekk,xBaug,xHekk,xnull,znull;
  double s1,s2,s5,s6,eta3,eta5,eta3sjekk,eta5sjekk;
  double sx1,sx2,sx5,sx6;
  double sz1,sz2,sz5,sz6;
  //srand(time(NULL));

  //Åpner VTF-fil og starter skriving
  filpeker = fopen("dmet.vtf", "a");
  filpeker2 = fopen("responsplott.dat","w");
  //Beregner respons for hiv, som skal gjelde alle noder
  for(m=0;m<NL;m++){
    for(i=0;i<steg;i++){
      lekter = m+1;
      teller = steg*lekter+i;
      fprintf(filpeker,"*RESULTS %d\n",teller);
      fprintf(filpeker,"%s","%DIMENSION 3\n");
      fprintf(filpeker,"%s%d\n","%PER_NODE #",lekter+1);
      t=i*1.0/fps;
      eta3 = 0;
      eta5 = 0;
      eta3sjekk = 0;
      eta5sjekk = 0;
      for(j=0;j<N;j+=intervall){
	eta3 = eta3 + A[j]*H3[j][m]*sin(omega[j]*t + delta3[j][m] + epsilon[j]);
 	eta5 = eta5 + A[j]*H5[j][m]*k[j]*cos(omega[j]*t + delta5[j][m] + epsilon[j]); 
      }
      for(j=133;j<134;j++){
	eta3sjekk = eta3sjekk + A[j]*H3[j][m]*sin(omega[j]*t + delta3[j][m]\
						  + epsilon[j]);
 	eta5sjekk = eta5sjekk + A[j]*H5[j][m]*k[j]*cos(omega[j]*t + delta5[j][m] \
						  + epsilon[j]); 
      }
      fprintf(filpeker2,"%lf %lf\n",eta3sjekk,eta5sjekk);
      //Beregner respons fra bevegelsesligning
      //printf("Lengde %d, bredde %d, høgde%f\n",L[m],B[m],H[m]);
      zBunn = -H[m]/2.0;
      zDekk = H[m]/2.0;
      xBaug = -L[m]/2.0;
      xHekk = L[m]/2.0;
      sx1 = zBunn*eta5;
      sz1 = eta3 -xBaug*eta5;
      sx2 = zBunn*eta5;
      sz2 = eta3 -xHekk*eta5;
      sx5 = zDekk*eta5;
      sz5 = eta3 -xBaug*eta5;
      sx6 = zDekk*eta5;
      sz6 = eta3 -xHekk*eta5;
      fprintf(filpeker,"%f %d %f\n", sx1, 0, sz1);
      fprintf(filpeker,"%f %d %f\n", sx2, 0, sz2);
      fprintf(filpeker,"%f %d %f\n", sx2, 0, sz2);
      fprintf(filpeker,"%f %d %f\n", sx1, 0, sz1);
      fprintf(filpeker,"%f %d %f\n", sx5, 0, sz5);
      fprintf(filpeker,"%f %d %f\n", sx6, 0, sz6);
      fprintf(filpeker,"%f %d %f\n", sx6, 0, sz6);
      fprintf(filpeker,"%f %d %f\n", sx5, 0, sz5);
      }
  }

  fclose(filpeker);
  fclose(filpeker2);
  return;
}
