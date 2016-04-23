!---------------------------------------------------------------
! Subrutine: bevegelse
!---------------------------------------------------------------
!Hensikt:
!Beregne responsvariablene eta3 og eta 5
!
!Metode
!Ta inn alt fra koeffisienter og geometri, løse diff.ligning
!
!Parametere:
!
!Programmert av: Tor Erik Larsen
!Dato: i dag
!---------------------------------------------------------------
SUBROUTINE bevegelse(k,omega,a,b,n,nl,rho,g,Sn,A33,A55 & 
     ,B33,B55,C33,C55,L,Br,D,M,GML,I33,I55,eta3A,eta5A)
IMPLICIT NONE
!Erklærer variable og vektorer
INTEGER::n,nl,i,j
REAL,DIMENSION(1:n)::omega,Sn,k
REAL,DIMENSION(1:n,1:nl)::A33,A55,B33,B55
DOUBLE PRECISION,DIMENSION(1:n,1:nl)::F3e,e3n,F3int,F5e,F51,F52,eta5Anevner
DOUBLE PRECISION,DIMENSION(1:n,1:nl)::eta3A,eta5A
DOUBLE PRECISION,DIMENSION(1:n,1:nl)::H3,H5
REAL,DIMENSION(1:nl)::L,Br,D,M,GML,I33,I55,C33,C55

!Regner ut vektor for bølgetallet k, skriver kontroll til skjerm
!DO i=1,n
!   k(i)=omega(i)**2 / g
!END DO
WRITE(*,*)' '
WRITE(*,*)'------------------START UTSKRIFT SUBRUTINE BEVEGELSE.F90------'
WRITE(*,*)' '
!WRITE(*,*)'k(250) er : ', k(250)

!Beregner integralene til kreftene i hiv, skriver kontroll til skjerm
!DO i=1,n
!   DO j=1,nl
!      F3int(i,j)=(2.0/k(i))*SIN((k(i)*L(j))/2.0)
!   END DO
!END DO

!WRITE(*,*)'F3int(250,2) er: ',F3int(250,2)

!Beregner kreftene i hiv F3e, skriver kontroll til skjerm
!DO i=1,n
!   DO j=1,nl
!      F3e(i,j) = (rho*g*EXP(k(i)*D(j))*Br(j) - omega(i)**2*A33(i,j)*EXP(k(i)*D(j)/2.0))*F3int(i,j)
!   END DO
!END DO
!WRITE(*,*)'F3e(250,1) er :',F3e(250,1)

!Beregner nevner i diffligning
!DO i=1,n
!   DO j=1,nl
!      e3n(i,j)= SQRT(  (C33(j)-M(j)*omega(i)**2)**2 + B33(i,j)**2 * omega(i)**2    )
!   END DO
!END DO
!WRITE(*,*)'eta3nevner(250,1) er: ',e3n(250,1)

!Beregner eta 3
!DO i=1,n
!   DO j=1,nl
!      eta3A(i,j)=F3e(i,j) / e3n(i,j)
!   END DO
!END DO
!WRITE(*,*)'eta3A(250,1) er: ',eta3A(250,1)
!WRITE(*,*)'------'

!Beregner integralet til F5e
!DO i=1,n
!   DO j=1,nl
!      F51(i,j)= (rho*g*exp(k(i)*(-D(j)))*Br(j) &
!           + omega(i)**2 * A33(i,j)*exp(k(i)*(-D(j)/2.0)) )
!   END DO
!END DO
!WRITE(*,*)'F51(250,1) fra subrutine er:',F51(250,1)

!Beregner F5
!DO i=1,n
!   DO j=1,nl
!      F52(i,j)= 2.0*SIN((k(i)*L(j)/2.0)) - k(i)*L(j)*COS((k(i)*L(j)/2.0))
!   END DO
!END DO
!WRITE(*,*)'F52(250,1) fra subrutine er:',F52(250,1)

!Beregner nevner i eta5-uttrykket
!DO i=1,n
!   DO j=1,nl
!      eta5Anevner(i,j)=SQRT((C33(j)-(I55(j)+A55(i,j))*omega(i)**2)**2 + B33(i,j)**2 * omega(i)**2)
!   END DO
!END DO
!WRITE(*,*)'Eta5nevner fra subrutine er:',eta5Anevner(250,1)

!Beregner eta5
!DO i=1,n
!   DO j=1,nl
!      eta5A(i,j)=(F51(i,j) * F52(i,j)) / eta5Anevner(i,j)
!   END DO
!END DO
!WRITE(*,*)'eta5A(250,1) er: ',eta5A(250,1)
WRITE(*,*)' '
WRITE(*,*)'-------------------------STOPP UTSKRIFT SUBRUTINE BEVEGELSEF90---------------------'
WRITE(*,*)' '

RETURN
END SUBROUTINE