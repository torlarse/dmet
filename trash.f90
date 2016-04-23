subroutine sncalc(sn,pi,hs,tp,omegap,omega,gamma)
implicit none
real, intent(in) :: hs,tp,gamma,pi,a,b
!real,dimension(0:),intent(in)::omega, sn
real,dimension(:),intent(out)::sn
real omegap,sigma,konstant,v1,v2,v3
integer, intent(in) ::n
integer i
!Deler opp Sn i faktorer og multipliserer sammen til slutt
konstant = (5.0/(32.0*pi))*(hs**2)*tp*(1.0-0.287*log(gamma))
v1 = (omegap/omega(i))**5
v2 = exp(-(5.0/4.0)*((omegap/omega(i))**4))
v3 = gamma**(exp(-(((omega(i)/omegap)-1.0)**2))/2.0*(sigma**2))
sn(i)=konstant*v1*v2*v3
end do
return
end subroutine
