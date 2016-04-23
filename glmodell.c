//   --------------------------------------------------------------------
//   DMET DELUX 2000	Subrutine	glmodell
//   --------------------------------------------------------------------
//   HENSIKT :
//   Starte skriving til VTF-fil for visualisering i GLView. Definerer
//   alle noder og elementer for både havteppe og lektere
//
//   METODE :
//   Ta inn havteppegeometri fra header-fil, og ta inn lektergeometri
//   fra tidligere subrutine. Skrive filhode og starte noder og elementer.
//      
//   KALLESEKVENS:
//   glModell(L,B,D,H);
//
//   PARAMETERE:
//   Navn	I/O	Type			Innhold/Beskrivelse
//   ....................................................................
//   n		I	heltall			antall frekvensintervaller
//   nl		I	heltall			antall lektere
//   L		I	heltalls vektor		lengde på lekter
//   B		I 	heltalls vektor		bredde på lekter
//   D		I	flyttalls vektor	dypgang på lekter
//   H		I	flyttalls vektor	dybde i riss for lekter
//
//   INTERNE VARIABLE:
//   Navn			Funksjon	
//   .....................................................................
//   i,j			tellevariabler
//   nodegruppe,x,xkoordinat	nodevariabler
//   xsenter,ysenter		definerer senter i havteppe
//   senterlinje		definerer senterlinjer på lektere
// 
//  
//   SKRIVEFILER:
//   Navn			Eventuell beskrivelse
//   .....................................................................
//   dmet.vtf                   Visualiseringsfil for GLView
//
//   --------------------------------------------------------------------
//  
//   Programmert av:	     Tor Erik Larsen
//   Dato/Versjon  : 	     27.04.15 / 1.0
//
//   --------------------------------------------------------------------

#include "header.h"

void glModell(int L[NL],int B[NL],float D[NL],float H[NL]){
  int i,j,nodegruppe,n,xstart;
  double senterlinje[2];
  double xkoordinat,xsenter,ysenter,ykoordinat,x;
  FILE * filpeker;

  //Åpner VTF-fil og starter skriving
  filpeker = fopen("dmet.vtf", "w");
  fprintf(filpeker,"%s\n","*VTF-1.00");
  fprintf(filpeker,"%s \n","*NODES 1");
  fprintf(filpeker,"%s \n","%WITH_ID");
  n=1;
  xstart = -havlengde/2;
  ykoordinat = havbredde/2.0;
  for(i=xstart;i<havlengde/2;i++){
    x = i*1.0;
    xkoordinat = i+0.5;
    fprintf(filpeker,"%d \t %.1f %.1f %.1f\n",n,x,-ykoordinat,0.0);
    fprintf(filpeker,"%d \t %.1f %.1f %.1f\n",n+1,xkoordinat,-ykoordinat,0.0);
    fprintf(filpeker,"%d \t %.1f %.1f %.1f\n",n+2,xkoordinat,ykoordinat,0.0);
    fprintf(filpeker,"%d \t %.1f %.1f %.1f\n",n+3,x,ykoordinat,0.0);
    n = n+4;
  }

  //Definerer elementer for havflaten ut fra nodene ovenfor
  fprintf(filpeker,"%s\n","*ELEMENTS 1");
  fprintf(filpeker,"%s\n","%COLORS 0,0,1");
  fprintf(filpeker,"%s\n","%NODES #1");
  fprintf(filpeker,"%s\n","%QUADS");
  
  n=1;
  for(x=0;x<havlengde-1;x++){
    fprintf(filpeker,"%d %d %d %d\n",n,n + 1,n + 2,n + 3);
    fprintf(filpeker,"%d %d %d %d\n",n + 1,n + 4,n + 7,n + 2);
    n = n +4;
  }
  n=0;
  //Bygger lekternoder
  senterlinje[0] = 1.0 + B[0]/2.0;
  senterlinje[1] = - 1.0 - B[1]/2.0;
  for(j=0;j<NL;j++){
    nodegruppe = j+2;
    n= nodegruppe*1000;
    fprintf(filpeker,"%s %d \n","*NODES ",nodegruppe);
    fprintf(filpeker,"%s \n","%WITH_ID");
    fprintf(filpeker,"%d %.1f %.1f %f\n",n+1,-L[j]/2.0,senterlinje[j] \
	    -B[j]/2.0,-H[j]/2.0);
    fprintf(filpeker,"%d %.1f %.1f %f\n",n+2,L[j]/2.0,senterlinje[j] \
	    -B[j]/2.0,-H[j]/2.0);
    fprintf(filpeker,"%d %.1f %.1f %f\n",n+3,L[j]/2.0,senterlinje[j]+\
	    B[j]/2.0,-H[j]/2.0);
    fprintf(filpeker,"%d %.1f %.1f %f\n",n+4,-L[j]/2.0,senterlinje[j]+\
	    B[j]/2.0,-H[j]/2.0);
    fprintf(filpeker,"%d %.1f %.1f %f\n",n+5,-L[j]/2.0,senterlinje[j]-\
	    B[j]/2.0,H[j]/2.0);
    fprintf(filpeker,"%d %.1f %.1f %f\n",n+6,L[j]/2.0,senterlinje[j]-\
	    B[j]/2.0,H[j]/2.0);
    fprintf(filpeker,"%d %.1f %.1f %f\n",n+7,L[j]/2.0,senterlinje[j]+\
	    B[j]/2.0,H[j]/2.0);
    fprintf(filpeker,"%d %.1f %.1f %f\n",n+8,-L[j]/2.0,senterlinje[j]+\
	    B[j]/2.0,H[j]/2.0);
    fprintf(filpeker,"%s%d\n","*ELEMENTS ",nodegruppe);
    fprintf(filpeker,"%s%d\n","%NODES #",nodegruppe);
    fprintf(filpeker,"%s\n","%HEXAHEDRONS");
    fprintf(filpeker,"%d %d %d %d %d %d %d %d\n",n+1,n+2,n+3,n+4,n+5,n+6,n+7,n+8);
  }
  fclose(filpeker);
  return;
}
