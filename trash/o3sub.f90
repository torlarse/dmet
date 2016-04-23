subroutine fcal(omegap,k,a,b,n,f)
implicit none
  real o, meg, expo,gamma, gam1, gam, sn,fn,f,sigma, omega,test
  real, intent(in) :: omegap,k,a,b
  integer i
  integer, intent(in) :: n
  open(10,file='spekterplot.dat',action = "write", status ="replace")
  do i = 1,n
   omega = a + ((b-a)*(i-0.5))/n
   write(*,*) 'omega fra subrutine er', omega
   if (omegap < omega) then
      sigma = 0.09
   else
      sigma = 0.07
   end if
   write(*,*) 'sigma fra subrutine er:',sigma
   o = omegap/omega
   write(*,*) 'o fra subrutine er ',o
   meg = (o)**5
   write(*,*) 'o fra subrutine er ',o
   expo = exp(-(5./4.) * (o**4))
!   write(*,*) 'expo fra subrutine er:', expo
   test = omega/omegap
!   write(*,*) 'omega/omegap fra subrutine er:',test
   gam1 = exp(-(((omega/omegap)-1.)**2) /(2.*(sigma**2)))
!   write(*,*) 'gam1 fra subrutine er:', gam1
   gam = gamma**gam1
!   write(*,*) 'gam fra subrutine er:', gam
   sn = meg*expo*gam*k
   fn = (omega**2) * sn
   f = f + fn
   write(10,*) omega,sn
   write(*,*) 'f fra subrutine er:',f
end do
close(10)
return
end subroutine fcal
