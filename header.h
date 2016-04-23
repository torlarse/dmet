//   --------------------------------------------------------------------
//   DMET DELUX 2000	header
//   --------------------------------------------------------------------
//   HENSIKT :
//   Inkludere innebygde C-biblioteker, definerere konstanter og erklære
//   subrutiner i programmet
//
//   METODE :
//
//   KALLESEKVENS:
//   #include "header.h"
//
//   PARAMETERE:
//   Navn	I/O	Type			Innhold/Beskrivelse
//   ....................................................................
//   pi		O	flyttall		pi
//   intervall	O	heltall			steg mellom bølger
//   N		O	heltall			antall bølger
//   NL		O	heltall			antall lektere
//   fps	O	heltall			tidsintervall persekund
//   steg	O	heltall			antall resultater
//   havlengde	O	heltall			lengde havteppe
//   havbredde 	O	heltall			bredde havteppe
//   stripebredde O	heltall			bredde havstripe
//   nstriper	  O	heltall			antall havstriper totalt	
//   
//   SUBRUTINER
//   Navn			Eventuell beskrivelse
//   .....................................................................
//   lesInn			Se beskrivelse i hoved.c
//   glModell
//   havFlate
//   respons
//   kontroll
//
//   --------------------------------------------------------------------
//  
//   Programmert av:	     Tor Erik Larsen
//   Dato/Versjon  : 	     27.04.15 / 1.0
//
//   --------------------------------------------------------------------

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <time.h>

#define pi 3.1415

//Definerer antall bølgekomponenter og antall lektere
#define intervall 10
#define N 1000
#define NL 2

//Definerer "lukkertid" og totalt antall steg
#define fps 4
#define steg 600

//Definerer havgeometri og inndeling for visualisering
#define havlengde 250
#define havbredde 100.0
#define stripebredde 0.5
#define nstriper 300

//Erklærer subrutiner som brukers i programmet
void lesInn(int L[NL],int B[NL],float D[NL],float H[NL],	\
	    double omega[N],double k[N],double A[N], \
	    double H3[N][NL],double H5[N][NL],		\
	    double delta3[N][NL],double delta5[N][NL]);

void glModell(int L[NL],int B[NL],float D[NL],float H[NL]);

void havFlate(double A[N], double omega[N], double k[N],\
	      double epsilon[N]);

void respons(int L[NL],int B[NL],float D[NL],float H[NL],	\
	     double omega[N],double k[N],double A[N],		\
	    double H3[N][NL],double H5[N][NL],	   \
	     double delta3[N][NL],double delta5[N][NL],\
	     double epsilon[N]);

void kontroll();


