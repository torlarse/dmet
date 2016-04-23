#include "header.h"

void havModell(){
  int i,j,x,n;
  double Zeta[2];
  double xkoordinat, a=2.0*pi,halvx;
  FILE * filpeker;

  //Åpner VTF-fil og starter skriving
  filpeker = fopen("dmet.vtf", "w");
  fprintf(filpeker,"%s\n","*VTF-1.00");
  fprintf(filpeker,"%s \n","*NODES 1");
  fprintf(filpeker,"%s \n","%WITH_ID");
  n=1;
  for(x=0;x<havlengde;x++){
    xkoordinat = x+0.5;
    fprintf(filpeker,"%d \t %.1f %.1f %.1f\n",n,xkoordinat,0.0,0.0);
    fprintf(filpeker,"%d \t %.1f %.1f %.1f\n",n+1,xkoordinat+0.5,0.0,0.0);
    fprintf(filpeker,"%d \t %.1f %.1f %.1f\n",n+2,xkoordinat+0.5,havbredde,0.0);
    fprintf(filpeker,"%d \t %.1f %.1f %.1f\n",n+3,xkoordinat,havbredde,0.0);
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
  fclose(filpeker);

  return;
}
