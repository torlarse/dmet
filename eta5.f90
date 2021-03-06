SUBROUTINE h5kalk(n,nl,L,Br,D,M,A33,B33,A55,B55,C55,I55,k,omega,rho,g,H5)
  IMPLICIT NONE
  !Erklærer variable
  INTEGER::n,nl,i,j
  REAL,DIMENSION(1:nl)::L,Br,D,M,C33,I55,C55
  DOUBLE PRECISION,DIMENSION(1:n)::k,omega
  REAL,DIMENSION(1:n,1:nl)::A33,B33,A55,B55
DOUBLE PRECISION,DIMENSION(1:n,1:nl)::F5A1,F5A2,eta5nevner,F5A
DOUBLE PRECISION,DIMENSION(1:n,1:nl)::eta5A,H5
REAL rho,g

!Beregner eksitasjonskraft F5A
DO i=1,n
   DO j=1,nl
      F5A1(i,j)= (rho*g*EXP(k(i)*(-D(j)))*Br(j) &
           + omega(i)**2 * (A33(i,j)/L(i,j))*exp(k(i)*(-D(j)/2.0)) )

      F5A2(i,j)= (2.0/k(i)) * SIN(k(i)*L(j) / 2.0) - L(j)*COS(k(i)*L(j)/2.0))

      eta5nevner(i,j) = k(i)* SQRT( (C33(j) - (I55(j) &
           + A55(i,j)) * omega(i)**2 )**2 + ( B55(i,j)*omega(i) )**2)
      F5A (i,j) = F5A1 (i,j) * F5A2 (i,j)
   END DO
END DO

!Beregner eta5A
DO i=1,ny
   DO j=1,nl
      eta5A(i,j) = (F5A (i,j)) / eta5nevner(i,j)
   END DO
END DO

!Beregner transferfunksjonen, som er eta5A absolutt
DO i=1,n
   DO j=1,nl
      H5(i,j) = ABS(eta5A(i,j) / k(i) )
   END DO
END DO


!Bygger fil for å sjekke faktorene i eta5-uttrykket
OPEN(52,FILE="eta5sjekk.txt", ACTION="WRITE", STATUS="UNKNOWN")
DO i=1,n
   WRITE(52,*), F5A(i,1), eta5nevner(i,1)
END DO
CLOSE(52)

!Skriver resultat til fil for sjekk/ plott
OPEN(42,FILE="eta5plot.txt",ACTION="WRITE",STATUS="UNKNOWN")
DO i=1,n
   WRITE(42,*) omega(i),eta5A(i,1),eta5A(i,2)
END DO
CLOSE(42)

RETURN
END SUBROUTINE
