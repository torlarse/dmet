//   --------------------------------------------------------------------
//   DMET DELUX 2000	Subrutine	kontroll
//   --------------------------------------------------------------------
//   HENSIKT :
//   Sammenstille resultater i VTF-fil, samordne tidssteg
//
//   METODE :
//   Åpne VTF-fil, og skrive en såkalt GLVIEWVECTOR til fil. Denne 
//   samordner hvert enkelt resultat for hvert enkelt tidssteg, slik
//   at visualiseringen henger sammen
//
//   KALLESEKVENS:
//   kontroll();
//
//   PARAMETERE:
//   Navn	I/O	Type			Innhold/Beskrivelse
//   ....................................................................
//   tid	O	flyttall		tid for hvert steg
//   fps	I	flyttall		tidsintervall
//   i		O	heltall			resultatteller
//
//
//   INTERNE VARIABLE:
//   Navn			Funksjon	
//   .....................................................................
//   
//   
//   SKRIVEFILER:
//   Navn			Eventuell beskrivelse
//   .....................................................................
//   dmet.vtf			visualiseringsfil
//
//   --------------------------------------------------------------------
//  
//   Programmert av:	     Tor Erik Larsen
//   Dato/Versjon  : 	     27.04.15 / 1.0
//
//   --------------------------------------------------------------------

#include "header.h"

void kontroll(){
  int i;
  double tid;

  FILE * filpeker;

  //Åpner VTF-fil og starter skriving
  filpeker = fopen("dmet.vtf", "a");

  //Definerer GLVIEW-geometri
  fprintf(filpeker,"*GLVIEWGEOMETRY 1\n");
  fprintf(filpeker,"%s","%ELEMENTS\n");
  fprintf(filpeker,"1,2,3\n");

  //Definerer GLVIEW-vektor, som samordner resultater
  fprintf(filpeker,"*GLVIEWVECTOR 1\n");
  fprintf(filpeker,"%s\n","%NAME \"Displacements\"");
  for(i=1;i<steg;i++){
    tid = i*1.0/fps;
    fprintf(filpeker,"%s %d\n","%STEP", i);
    fprintf(filpeker,"%s %.1f%s\n","%STEPNAME \"Tid: ", tid,"s\"");
    fprintf(filpeker,"%d,%d,%d\n",i,i+steg,i+steg*2);
  }

  fclose(filpeker);
return;
}
