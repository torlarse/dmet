OBJECTS1 = main.o spekter.o transfer.o konstanter.o akalk.o bckalk.o h3.o h5.o print.o fase.o statistikk.o

OBJECTS2 = hoved.o lesinn.o hav.o respons.o glmodell.o kontroll.o

FC = gfortran
FCFLAGS = -g -fcheck=all

CC = gcc
CFLAGS = -g

.SUFFIXES: .f90 .o
.f90.o:
	$(FC) $(FCFLAGS) -c $<		

#all: fprogram cprogram

f: ${OBJECTS1}
	$(FC) ${OBJECTS1} -o fprogram
c: ${OBJECTS2}
	$(CC) ${OBJECTS2} -o cprogram -lm

clean: 
	rm fprogram ${OBJECTS1} 
	rm cprogram ${OBJECTS2}
