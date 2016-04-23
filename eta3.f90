SUBROUTINE h3kalk(k,omega,n,nl,rho,g,A33,B33,C33,L,Br,D,M,H3)
IMPLICIT NONE
!Erklærer variable og vektorer
INTEGER::n,nl,i,j
DOUBLE PRECISION,DIMENSION(1:n)::omega,k
REAL,DIMENSION(1:n,1:nl)::A33,B33
DOUBLE PRECISION,DIMENSION(1:n,1:nl)::eta3nevner,F3A1,F3A2,F3A
DOUBLE PRECISION,DIMENSION(1:n,1:nl)::eta3A,H3
REAL,DIMENSION(1:nl)::L,Br,D,M,C33
REAL rho,g,a,b

!Beregner integralene til kreftene i hiv, skriver kontroll til skjerm
DO i=1,n
   DO j=1,nl
      F3A1(i,j) = (2.0/k(i)) * SIN((k(i) * L(j)) / 2.0 )

      F3A2(i,j) = rho*g*Br(j)* EXP(- k(i)*D(j)) - (omega(i)**2) * (A33(i,j)/L(i,j)) * &
           EXP(- k(i)*D(j)/2.0)

      eta3nevner(i,j)= SQRT(  (C33(j) - (M(j)+A33(i,jy)) * omega(i)**2 )**2 + &
           (B33(i,j) * omega(i))**2  )

      F3A(i,j) = F3A1(i,j) * F3A2(i,j)
   END DO
END DO

!Beregner eta 3
DO i=1,n
   DO j=1,nl
      eta3A(i,j)=   F3A(i,j) / eta3nevner(i,j)
   END DO
END DO

!Beregner transfer-funksjoner
DO i=1,n
   DO j=1,nl
      H3(i,j)=ABS(eta3A(i,j))
   END DO
END DO


!Skriver resultatet til fil for sjekk/ plotting
OPEN(41,FILE="eta3plot.txt",ACTION="WRITE",STATUS="UNKNOWN")
DO i=1,n
   WRITE(41,*) omega(i),eta3A(i,1),eta3A(i,2)
END DO
CLOSE(41)

RETURN
END SUBROUTINE
